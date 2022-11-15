##################################################
## Project: Satisfactory calculator
## Script purpose: Development of a binary search function that works in continuous space
## Date: 2022-11-15
## Author: Jay Gillenwater
##################################################

factory_binary_search_continuous <- function(Opt_products, current_recipes,
                                             available_resources, req_amt = c(2500, 500), max_rate = 50){
  
  # If only one product is needed, just maximize the production of that item
  if(length(Opt_products) == 1){
    res <- recipe_lp_base(available_resources, 
                          Opt_products, 
                          recipeData = current_recipes$data_frame, 
                          recipeGraph = current_recipes$graph, 
                          integerFactories = FALSE)
    
    return(res)
  }
  
  # Get the recipe matrix for these products
  recipe_matrix <- make_recipeMatrix(names(Opt_products), 
                                     current_recipes$graph, 
                                     current_recipes$data_frame, 
                                     "component")
  
  # Define the objective function as the maximum time required to 
  # produce the required amount
  time_to_objective <- function(x){
    
    net_rate <- recipe_matrix %*% x
    
    max(net_rate[names(Opt_products),]/req_amt)
  }
  
  calc_solution_time <- function(lp_soln){
    soln <- lp_soln %>% purrr::pluck("solution")
    
    time_to_objective(soln)
  }
  
  
  # Initialize counting variables
  current_best_time     <- Inf
  upper_boundry_delta   <- Inf
  
  current_best_soln <- NA
  
  delta_lim <- 0.0001
  
  run <- 0
  
  # Ratios of required amounts relative to the maximum required amount
  amt_ratios <- req_amt/min(req_amt)
  
  upper_boundry <- max_rate
  lower_boundry <- 0
  failing_upper_boundry <- upper_boundry
  
  lower_product_rates <- (amt_ratios * lower_boundry) %>% 
    set_names(names(Opt_products))
  
  upper_poduct_rates <- (amt_ratios * upper_boundry) %>% 
    set_names(names(Opt_products))
  
  while(run < 150 & upper_boundry_delta > delta_lim){
    
    
    # Solve the minimum and maximum parameter combinations
    # Technically only have to do one of these after the first time but I'm lazy
    min_soln <- recipe_lp_base(products          = lower_product_rates, 
                               startingResources = available_resources,
                               recipeData        = current_recipes$data_frame, 
                               recipeGraph       = current_recipes$graph, 
                               integerFactories  = FALSE)
    
    max_soln <- recipe_lp_base(products          = upper_poduct_rates, 
                               startingResources = available_resources,
                               recipeData        = current_recipes$data_frame, 
                               recipeGraph       = current_recipes$graph, 
                               integerFactories  = FALSE)
    
    # Get the statuses of the max solution
    max_status <- max_soln$status
    
    # Two possibilities: 
    #
    # 1. Max failed
    # 2. Max success
    #
    # 1. Set new max to ceiling(max_index - (max_index - min_index/2))
    # 2. Set new min to old max, set 
    #        new max to ceiling(max_index + (max_index - min_index/2))
    
    # Check that you can actually find any solution
    if(run == 0){
      if(min_soln$status == 2){
        print("No solution possible")
        break
      }
    }
    
    # Update boundries 
    if(max_status == 2){       # If failure...
      
      # Decrease the upper boundry by half the range between the upper and lower boundaries
      failing_upper_boundry <- upper_boundry
      upper_boundry         <- upper_boundry - ((upper_boundry - lower_boundry)/2)
      
      upper_boundry_delta <- abs(failing_upper_boundry - upper_boundry)
      
    }else if(max_status == 0){ # If success...
      
      # Increase the upper boundry by half the range between the upper and lower boundaries
      # and set the lower boundry to the current upper boundry
      old_upper_boundry <- upper_boundry
      upper_boundry     <- upper_boundry + ((failing_upper_boundry - upper_boundry)/2)
      lower_boundry     <- old_upper_boundry
      
      upper_boundry_delta <- abs(upper_boundry - old_upper_boundry)
      
    }
    
    # Increment the run counter
    run <- run + 1
    
    # Update rates from new boundaries
    lower_product_rates <- (amt_ratios * lower_boundry) %>% 
      set_names(names(Opt_products))
    
    upper_poduct_rates <- (amt_ratios * upper_boundry) %>% 
      set_names(names(Opt_products))
    
  }
  
  # Set names of the solution to the appropriate recipe names
  min_soln$solution <- min_soln$solution %>% 
    set_names(colnames(recipe_matrix))
  
  # Return the max solution
  min_soln
  
}
