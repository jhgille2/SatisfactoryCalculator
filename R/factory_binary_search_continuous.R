##################################################
## Project: Satisfactory calculator
## Script purpose: Development of a binary search function that works in continuous space
## Date: 2022-11-15
## Author: Jay Gillenwater
##################################################

factory_binary_search_continuous <- function(Opt_products, current_recipes,
                                             available_resources, req_amt = c(2500, 500), whole_number_factories = FALSE, slack = FALSE){
  
  # If only one product is needed, just maximize the production of that item
  if(length(Opt_products) == 1){
    res <- recipe_lp_base(available_resources, 
                          Opt_products, 
                          recipeData = current_recipes$data_frame, 
                          recipeGraph = current_recipes$graph, 
                          integerFactories = FALSE,
                          slack)
    
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
  
  delta_lim <- 0.00000001
  
  max_rate <- 1
  
  run <- 0
  belowFailing <- TRUE
  
  # Ratios of required amounts relative to the minimum required amount
  amt_ratios <- req_amt/min(req_amt)
  
  upper_boundry <- max_rate
  lower_boundry <- 0
  failing_upper_boundry <- upper_boundry
  
  lower_product_rates <- (amt_ratios * lower_boundry) %>% 
    purrr::set_names(names(Opt_products))
  
  lower_product_rate_list <- vector("list", 1000)
  
  upper_poduct_rates <- (amt_ratios * upper_boundry) %>% 
    purrr::set_names(names(Opt_products))
  
  upper_product_rate_list <- vector("list", 1000)
  
  while(run < 550 & upper_boundry_delta > delta_lim){
    
    if(run == 0){
      # Solve the minimum and maximum parameter combinations
      min_soln <- recipe_lp_base(products          = lower_product_rates, 
                                 startingResources = available_resources,
                                 recipeData        = current_recipes$data_frame, 
                                 recipeGraph       = current_recipes$graph, 
                                 integerFactories  = whole_number_factories,
                                 slack             = slack)
      
      max_soln <- recipe_lp_base(products          = upper_poduct_rates, 
                                 startingResources = available_resources,
                                 recipeData        = current_recipes$data_frame, 
                                 recipeGraph       = current_recipes$graph, 
                                 integerFactories  = whole_number_factories,
                                 slack             = slack)
    }else{
      if(max_status == 2){
        max_soln <- recipe_lp_base(products          = upper_poduct_rates, 
                                   startingResources = available_resources,
                                   recipeData        = current_recipes$data_frame, 
                                   recipeGraph       = current_recipes$graph, 
                                   integerFactories  = whole_number_factories,
                                   slack             = slack)
      }else{
        min_soln <- max_soln
        
        max_soln <- recipe_lp_base(products          = upper_poduct_rates, 
                                   startingResources = available_resources,
                                   recipeData        = current_recipes$data_frame, 
                                   recipeGraph       = current_recipes$graph, 
                                   integerFactories  = whole_number_factories,
                                   slack             = slack)
      }
    }

    
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
        return(NULL)
      }
    }
    
    # Update boundaries 
    if(max_status == 2){       # If failure...
      
      belowFailing <- FALSE
      
      # Decrease the upper boundary by half the range between the upper and lower boundaries
      failing_upper_boundry <- upper_boundry
      upper_boundry         <- upper_boundry - ((upper_boundry - lower_boundry)/2)
      
      upper_boundry_delta <- abs(failing_upper_boundry - upper_boundry)
      
    }else if(max_status == 0){ # If success...
      
      # Increase the upper boundary by half the range between the upper and lower boundaries
      # and set the lower boundary to the current upper boundry
      old_upper_boundry <- upper_boundry
      
      if(belowFailing){
        failing_upper_boundry <- failing_upper_boundry * 2
        upper_boundry <- upper_boundry + ((failing_upper_boundry - upper_boundry)/2)
      }else{
        upper_boundry <- upper_boundry + ((failing_upper_boundry - upper_boundry)/2)
      }
      
      lower_boundry       <- old_upper_boundry
      upper_boundry_delta <- abs(upper_boundry - old_upper_boundry)
      
    }
    
    
    # Increment the run counter
    run <- run + 1
    # print(paste("run", run))
    # print(paste("upper boundry", upper_boundry))
    # print(paste("lower boundry", lower_boundry))
    
    # Update rates from new boundaries
    lower_product_rates <- (amt_ratios * lower_boundry) %>% 
      purrr::set_names(names(Opt_products))
    
    lower_product_rate_list[[run]] <- lower_product_rates
    
    upper_poduct_rates <- (amt_ratios * upper_boundry) %>% 
      purrr::set_names(names(Opt_products))
    
    upper_product_rate_list[[run]] <- upper_poduct_rates
    
  }
  
  # Set names of the solution to the appropriate recipe names
  min_soln$solution <- min_soln$solution %>% 
    purrr::set_names(colnames(recipe_matrix))
  
  # Return the min solution
  # min_soln
  
  lower_rate_df <- lower_product_rate_list[!map_lgl(lower_product_rate_list, is.null)] %>%
    purrr::reduce(bind_rows) %>%
    mutate(boundry = "lower",
           iteration = 1:n())
  
  upper_rate_df <- upper_product_rate_list[!map_lgl(upper_product_rate_list, is.null)] %>%
    purrr::reduce(bind_rows) %>%
    mutate(boundry = "upper",
           iteration = 1:n())
  
  rate_df <- bind_rows(lower_rate_df, upper_rate_df)
  
  res <- list("soln" = min_soln,
              "rate_df" = rate_df)
  
  return(res)
}
