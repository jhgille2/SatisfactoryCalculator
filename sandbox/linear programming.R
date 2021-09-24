

startingResources = c("Desc_OreCopper_C" = -1200, 
                      "Desc_OreIron_C"   = -2640, 
                      "Desc_Coal_C"      = -1000, 
                      "Desc_LiquidOil_C" = -1080, 
                      "Desc_Stone_C"     = -1920)

const_dir <- rep(">=", nrow(recipeMatrix))
rhs       <- rep(0, nrow(recipeMatrix))

for(i in 1:length(startingResources)){
  
  currentRes      <- which(rownames(recipeMatrix) ==  names(startingResources)[[i]])
  rhs[currentRes] <- startingResources[[i]]
  
}

products <- c("Desc_ModularFrameHeavy_C" = 3, 
              "Desc_SteelPlateReinforced_C" = 5, 
              "Desc_Motor_C" = 1, 
              "Desc_Plastic_C" = 1, 
              "Desc_Rubber_C" = 1, 
              "Desc_Computer_C" = 3)

for(i in 1:length(products)){
  
  currentRes      <- which(rownames(recipeMatrix) ==  names(products)[[i]])
  rhs[currentRes] <- products[[i]]
  
}

# A strategy to fit many linear models with different objective functions
# to weigh products differently
objectiveGrid <- expand_grid("Heavy Modular Frame"     = seq(0.1, 1, 0.2), 
                             "Encased Industrial Beam" = seq(0.1, 1, 0.2), 
                             "Motor"                   = seq(0.1, 1, 0.2), 
                             "Plastic"                 = seq(0.1, 1, 0.2), 
                             "Rubber"                  = seq(0.1, 1, 0.2), 
                             "Computer"                = seq(0.1, 1, 0.2)) %>% 
  t()

objective_indices <- match(rownames(objectiveGrid), colnames(recipeMatrix))
objective_list    <- vector("list", length = ncol(objectiveGrid))
obj_size          <- ncol(recipeMatrix)
for(i in 1:length(objective_list)){
  
  currentVec <- rep(0, obj_size)
  currentVec[objective_indices] <- objectiveGrid[, i]
  
  objective_list[[i]] <- currentVec
}

all_lps <- pblapply(objective_list, function(x) lp(direction = "max", 
                                           objective.in = x, 
                                           const.mat = recipeMatrix, 
                                           const.dir = const_dir, 
                                           const.rhs = rhs))

# functions to get just the objectives set for the desired components
clean_objective <- function(x, recipeMatrix, outputobjectives){
  pluck(x, "objective") %>% 
    set_names(colnames(recipeMatrix)) %>% 
    .[. > 0]
}

clean_solutions <- function(x, recipeMatrix){
  pluck(x, "solution") %>% 
    set_names(colnames(recipeMatrix))
}

# A function to calculate the production rates from the solution
calc_rates <- function(lp, recipeMatrix){
  recipeMatrix %*% lp$solution %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    set_names(c("ingredient", "rate"))
}

# A function to calculate the time to reach a given milestone from some production
# rates
example_milestone <- tibble(ingredient = c("Desc_Computer_C", 
                                           "Desc_Plastic_C", 
                                           "Desc_ModularFrameHeavy_C", 
                                           "Desc_SteelPlateReinforced_C", 
                                           "Desc_Motor_C"), 
                            qty = c(150, 1000, 125, 300, 100))

time_to_milestone <- function(production_rate_df, milestone_ingredients = example_milestone){
  
  production_rate_df %>% 
    right_join(milestone_ingredients, by = "ingredient") %>% 
    mutate(rate = round(rate, digits = 5)) %>%
    mutate(time = qty/rate) %>% 
    summarise(var_time = var(time), 
              max_time = max(time))
  
}

get_standard_rate <- function(recipeMatrix){
  
  allRates <- apply(recipeMatrix, 1, function(x) x[which(x > 0)])
  
  get_first_rate <- function(rateList){
    if(length(rateList)){
      return(rateList[[1]])
    }else{
      return(NA)
    }
  }
  
  standardRate <- map_dbl(allRates, get_first_rate)
  return(standardRate)
}


currentStandardRate <- get_standard_rate(recipeMatrix)


calc_efficiency <- function(lp, recipeMatrix, standardRates){
  
  efficientProduction  <- (recipeMatrix %*% lp$solution) %>% ceiling()
  productionDifference <- efficientProduction - (recipeMatrix %*% lp$solution)
  
  sum(productionDifference/standardRates, na.rm = TRUE)
}

justRates <- pblapply(all_lps, calc_rates, recipeMatrix)
justTimes  <- pblapply(justRates, time_to_milestone, example_milestone)
validRates <- pblapply(justTimes, function(x) !is.nan(unlist(x)))

lp_data <- tibble(lp_object = all_lps) %>% 
  mutate(objective        = map(lp_object, clean_objective, recipeMatrix, rownames(objectiveGrid)), 
         solution         = map(lp_object, clean_solutions, recipeMatrix), 
         production_rates = map(lp_object, calc_rates, recipeMatrix), 
         efficiency       = map_dbl(lp_object, calc_efficiency, recipeMatrix, currentStandardRate)) %>% 
  arrange(efficiency)

lp_data_long <- lp_data %>% 
  unnest(production_rates) %>% 
  unnest_longer(objective)








# objectiveFn <- function(x, desiredProducts = prod, resources = startingResources, recipematrix = recipeMatrix){
#   
#   current_solution <- recipematrix %*% x
#   
#   resource_consumption <- current_solution[names(resources),] < resources
#   
#   if(any(resource_consumption)){
#     return(NaN)
#   }
#   
#   other_items <- current_solution[!(rownames(current_solution) %in% names(resources)),] < 0
#   
#   if(any(other_items)){
#     return(NaN)
#   }
#   
#   objectiveitems <- current_solution[desiredProducts]
#   
#   return(objectiveitems)
# }
# 
# 
# milestone_objectives <- c("Automated Wiring" = 2.5100, 
#                           "Smart Plating" = 2/500, 
#                           "Versatile Framework" = 5/500)

# obj <- rep(0, ncol(recipeMatrix))
# 
# for(i in 1:length(milestone_objectives)){
#   Current_index <- which(colnames(recipeMatrix) == names(milestone_objectives)[[i]])
#   obj[Current_index] <- milestone_objectives[[i]]
# }

# prod <- c("Desc_ModularFrame_C", 
#           "Desc_Motor_C", 
#           "Desc_IronPlateReinforced_C")

# lp(direction = "max", 
#    objective.in = obj, 
#    const.mat = recipeMatrix, 
#    const.dir = const_dir, 
#    const.rhs = rhs, 
#    int.vec = 1:nrow(recipeMatrix)) -> soln
# 
# Rsymphony_solve_LP(obj = obj, 
#                    mat = recipeMatrix, 
#                    dir = const_dir, 
#                    rhs = rhs, 
#                    max = TRUE)