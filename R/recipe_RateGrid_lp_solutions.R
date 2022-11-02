recipe_lp_base <- function(startingResources = available_resources,
                           products = Opt_products, recipeData =
                             RecipeData$NoAlternates, recipeGraph =
                             RecipeGraphs$NoAlternates, integerFactories =
                             TRUE) {
  
  # Make a matrix of consumption/production rates. Recipes as columns, 
  # components in rows
  recipeMatrix <- make_recipeMatrix(names(products), recipeGraph, recipeData, itemMode = "component")
  
  # Constraint direction and right hand side for the linear program
  # Basically, make sure that there is no negative net flow in a solution
  const_dir <- rep(">", nrow(recipeMatrix))
  rhs       <- rep(0, nrow(recipeMatrix))
  
  # Set bounds for raw resource consumption
  for(i in 1:length(startingResources)){
    
    currentRes      <- which(rownames(recipeMatrix) ==  names(startingResources)[[i]])
    rhs[currentRes] <- startingResources[[i]]
  }
  
  # Insist that net flow of heavy oil residue must equal zero
  # (it's practically useless (?))
  if("Desc_HeavyOilResidue_C" %in% rownames(recipeMatrix)){
    const_dir[which(rownames(recipeMatrix) == "Desc_HeavyOilResidue_C")] <- "="
  }
  
  # Set minimum production rates for products
  const_dir[match(names(products), rownames(recipeMatrix))] <- ">"
  rhs[match(names(products), rownames(recipeMatrix))] <- products
  rhs <- as.numeric(rhs)
  
  # The indices of the objective products in the column names of the matrix
  if(length(products) == 1){
    product_longnames <- colnames(recipeMatrix)[which(recipeMatrix[names(products),] > 0, arr.ind = TRUE)]
  }else{
    product_longnames <- colnames(recipeMatrix)[which(recipeMatrix[names(products),] > 0, arr.ind = TRUE)[, 2]]
  }
  
  objective_indices <- match(product_longnames, colnames(recipeMatrix))

  ##### THIS NEEDS A LOT OF WORK #####
  # A (dumb) objective function with coefficients of 1 for each 
  # desired product and 0 for everything else. 
  objectiveVec <- rep(0, ncol(recipeMatrix))
  objectiveVec[objective_indices] <- 1
  
  lp_soln <- lp(direction    = "max", 
                objective.in = objectiveVec, 
                const.mat    = recipeMatrix, 
                const.dir    = const_dir, 
                const.rhs    = rhs, 
                all.int      = integerFactories)
  
  return(lp_soln)
}

# This function solves a grid of linear programs where the grid is defined by
# different combinations of minimum constraints for the desired product
# components
recipe_lp_rate_grid <- function(startingResources = available_resources,
                                products = Opt_products, recipeData =
                                  RecipeData$NoAlternates, recipeGraph =
                                  RecipeGraphs$NoAlternates, integerFactories =
                                  TRUE, 
                                  reqAmt = NULL, 
                                gridsize = 25){
  
  on.exit(plan(sequential))
  


  # Make a crossed grid of minimal production rates. 
  # Also, maybe look into other options for different
  # types of grids e.g. space filling, latin hypercube
  ratevec <- vector("list", length  = length(products))
  names(ratevec) <- names(products)
  for(i in 1:length(ratevec)){
    ratevec[[i]] <- seq(0.1, products[[i]], length.out = gridsize)
  }
  
  rates_crossed <- cross_df(ratevec) %>% t()
  
  rate_args <- vector("list", length = ncol(rates_crossed))
  for(i in 1:length(rate_args)){
    rate_args[[i]] <- rates_crossed[, i]
  }
  
  
  nWorkers <- ceiling(parallel::detectCores() *0.5)
  plan(multisession, workers = nWorkers, gc = TRUE)
  all_lps <- future_map(rate_args, function(x) recipe_lp_base(products          = x, 
                                                              startingResources = startingResources,
                                                              recipeData        = recipeData, 
                                                              recipeGraph       = recipeGraph, 
                                                              integerFactories  = integerFactories), 
                        .progress = TRUE)
  
  plan(sequential)
  
  recipeMatrix <- make_recipeMatrix(names(products), recipeGraph, recipeData, itemMode = "component")
  
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
      set_names(c("ingredient", "rate")) %>% 
      mutate(rate = round(rate, 3))
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
    
    total_time <- (objective_amount/current_rates) %>% max()
    
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
  
  return(all_lps)
}
