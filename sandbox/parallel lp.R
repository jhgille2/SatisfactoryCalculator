#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param startingResources
#' @param products
#' @param product_longnames
#' @param recipeData
#' @param recipeGraph
#' @param integerFactories
recipe_lp_solutions_parallel <- function(startingResources = available_resources,
                                products = Opt_products, product_longnames =
                                  Opt_recipes, recipeData =
                                  RecipeData$NoAlternates, recipeGraph =
                                  RecipeGraphs$NoAlternates, integerFactories =
                                  TRUE, 
                                gridsize = 10, 
                                reqAmt = NULL) {
  
  recipeMatrix <- make_recipeMatrix(product_longnames, recipeGraph, recipeData)
  
  # Constraint direction and right hand side for the linear program
  const_dir <- rep(">=", nrow(recipeMatrix))
  rhs       <- rep(0, nrow(recipeMatrix))
  
  # Set bounds for raw resource consumption
  for(i in 1:length(startingResources)){
    
    currentRes      <- which(rownames(recipeMatrix) ==  names(startingResources)[[i]])
    rhs[currentRes] <- startingResources[[i]]
    
  }
  
  if("Desc_HeavyOilResidue_C" %in% rownames(recipeMatrix)){
    const_dir[which(rownames(recipeMatrix) == "Desc_HeavyOilResidue_C")] <- "="
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
  #
  # One easy improvement here is to remove any duplicated ratios between the
  # objective weights or use an otherwise more intellegent grid of objectives
  # Maybe look into the design functions that the dials package uses,
  # maybe the lhs package too?
  #
  # ex. optimimLHS(length(product_longnames), nObjectives)
  # where nObjectives is just some set number of objective function weights to try
  
  
  # A strategy to fit many linear models with different objective functions
  # to weigh products differently
  # objective_vars <- vector("list", length = length(product_longnames))
  # for(i in 1:length(objective_vars)){
  #   objective_vars[[i]] <- seq(0.1, 1, 0.1)
  # }
  # 
  # names(objective_vars) <- product_longnames
  # 
  # objectiveGrid <- cross_df(objective_vars) %>%
  #   t()
  
  # Latin hypercube sampling for objective (number of factory) weights
  objectiveGrid           <- lhs::optimumLHS(n = length(product_longnames), gridsize)
  rownames(objectiveGrid) <- product_longnames 
  
  objective_indices <- match(rownames(objectiveGrid), colnames(recipeMatrix))
  objective_list    <- vector("list", length = ncol(objectiveGrid))
  obj_size          <- ncol(recipeMatrix)
  for(i in 1:length(objective_list)){
    
    currentVec <- rep(0, obj_size)
    currentVec[objective_indices] <- objectiveGrid[, i]
    
    objective_list[[i]] <- currentVec
  }
  
  nWorkers <- ceiling(parallel::detectCores() *0.5)
  
  plan(multisession, workers = nWorkers, gc = TRUE)
  
  all_lps <- future_map(objective_list, function(x) lp(direction    = "max", 
                                                     objective.in = x, 
                                                     const.mat    = recipeMatrix, 
                                                     const.dir    = const_dir, 
                                                     const.rhs    = rhs, 
                                                     all.int      = integerFactories), 
                        .progress = TRUE)
  
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
  
  # A function to calculate the total time to produce some given amount of the objective items
  objective_time <- function(production_rate, product_names, objective_amount){
    
    # Get the production rates from the production_rates dataframe
    current_rates <- production_rate$rate[match(names(product_names), production_rate$ingredient)]
    
    total_time <- (objective_amount/current_rates) %>% sum()
    
    return(total_time)
  }
  
  # Organize the final data into a tibble and return
  lp_data <- tibble(lp_object = all_lps) %>% 
    mutate(objective        = map(lp_object, clean_objective, recipeMatrix, rownames(objectiveGrid)), 
           solution         = map(lp_object, clean_solutions, recipeMatrix), 
           production_rates = map(lp_object, calc_rates, recipeMatrix), 
           efficiency       = map_dbl(lp_object, calc_efficiency, recipeMatrix, currentStandardRate)) %>% 
    arrange(efficiency) %>% 
    distinct(solution, .keep_all = TRUE)
  
  if(!is.null(reqAmt)){
    lp_data %<>% 
      mutate(time_to_objective = map_dbl(production_rates, objective_time, products, reqAmt)) %>% 
      arrange(time_to_objective)
  }
  
  return(lp_data)
}
