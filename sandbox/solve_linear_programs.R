# A function that takes a recipe matrix, and vectors of starting resources, 
# and the desired output rates for a vector of products and then solves
# many linear programs with different product weights
# TODO: use a different solevr to explicitly perform multi criteria optimization
# e.g. with product outputs/efficiencies
#
# recipeMatrix is the matrix output from the recipe_matrix_function, 
# startingResources and product are named vactors where startingResources
# has negative values for the resource available and product has the 
# desired output rate for each component (in minutes)


tar_load(RecipeData)
tar_load(RecipeGraphs)


recipe_lp_solutions <- function(startingResources, products, product_longnames, recipeData = RecipeData, recipeGraph = RecipeGraphs, integerFactories = FALSE){
  
  recipeMatrix <- make_recipeMatrix(product_longnames, recipeGraph, recipeData)
  
  # COnstraint direction and right hand side for the linear program
  const_dir <- rep(">=", nrow(recipeMatrix))
  rhs       <- rep(0, nrow(recipeMatrix))
  
  # Set bounds for raw resource consumption
  for(i in 1:length(startingResources)){
    
    currentRes      <- which(rownames(recipeMatrix) ==  names(startingResources)[[i]])
    rhs[currentRes] <- startingResources[[i]]
    
  }
  
  # Set minimum production rates for products
  for(i in 1:length(products)){
    
    currentRes            <- which(rownames(recipeMatrix) ==  names(products)[[i]])
    const_dir[currentRes] <- ">"
    rhs[currentRes]       <- products[[i]]
    
  }
  
  rhs <- as.numeric(rhs)
  
  # This next part is pretty slow if there are 
  # lots of objectives but for the time being it kinda works
  
  # A strategy to fit many linear models with different objective functions
  # to weigh products differently
  objective_vars <- vector("list", length = length(product_longnames))
  for(i in 1:length(objective_vars)){
    objective_vars[[i]] <- seq(0.1, 1, 0.1)
  }
  
  names(objective_vars) <- product_longnames
  
  objectiveGrid <- cross_df(objective_vars) %>% 
    t()
  
  objective_indices <- match(rownames(objectiveGrid), colnames(recipeMatrix))
  objective_list    <- vector("list", length = ncol(objectiveGrid))
  obj_size          <- ncol(recipeMatrix)
  for(i in 1:length(objective_list)){
    
    currentVec <- rep(0, obj_size)
    currentVec[objective_indices] <- objectiveGrid[, i]
    
    objective_list[[i]] <- currentVec
  }
  
  all_lps <- pblapply(objective_list, function(x) lp(direction    = "max", 
                                                     objective.in = x, 
                                                     const.mat    = recipeMatrix, 
                                                     const.dir    = const_dir, 
                                                     const.rhs    = rhs, 
                                                     all.int      = integerFactories))
  
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
  
  # # A function to calculate the production rates from the solution
  # calc_rates <- function(lp, recipeMatrix){
  #   recipeMatrix %*% lp$solution %>% 
  #     as.data.frame() %>% 
  #     rownames_to_column() %>% 
  #     set_names(c("ingredient", "rate"))
  # }
  # 
  # # A function to calculate the time to reach a given milestone from some production
  # # rates
  # example_milestone <- tibble(ingredient = c("Desc_Computer_C", 
  #                                            "Desc_Plastic_C", 
  #                                            "Desc_ModularFrameHeavy_C", 
  #                                            "Desc_SteelPlateReinforced_C", 
  #                                            "Desc_Motor_C"), 
  #                             qty = c(150, 1000, 125, 300, 100))
  # 
  # time_to_milestone <- function(production_rate_df, milestone_ingredients = example_milestone){
  #   
  #   production_rate_df %>% 
  #     right_join(milestone_ingredients, by = "ingredient") %>% 
  #     mutate(rate = round(rate, digits = 5)) %>%
  #     mutate(time = qty/rate) %>% 
  #     summarise(var_time = var(time), 
  #               max_time = max(time))
  #   
  # }
  
  # A (very lazy) function to get the production rate for each item in the recipeMatris.
  # A lookup table would be better
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
  
  # A function to calculate the production rates from the solution
  calc_rates <- function(lp, recipeMatrix){
    recipeMatrix %*% lp$solution %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      set_names(c("ingredient", "rate"))
  }
  
  # The standard production rate for each item in the matrix
  currentStandardRate <- get_standard_rate(recipeMatrix)
  
  # A simple efficiency calculation (lower score is better)
  calc_efficiency <- function(lp, recipeMatrix, standardRates){
    
    efficientProduction  <- (recipeMatrix %*% lp$solution) %>% ceiling()
    productionDifference <- efficientProduction - (recipeMatrix %*% lp$solution)
    
    sum(productionDifference/standardRates, na.rm = TRUE)
  }
  
  # Organize the final data into a tible and return
  lp_data <- tibble(lp_object = all_lps) %>% 
    mutate(objective        = map(lp_object, clean_objective, recipeMatrix, rownames(objectiveGrid)), 
           solution         = map(lp_object, clean_solutions, recipeMatrix), 
           production_rates = map(lp_object, calc_rates, recipeMatrix), 
           efficiency       = map_dbl(lp_object, calc_efficiency, recipeMatrix, currentStandardRate)) %>% 
    arrange(efficiency) %>% 
    distinct(solution, .keep_all = TRUE)
  
  return(lp_data)
  
}
