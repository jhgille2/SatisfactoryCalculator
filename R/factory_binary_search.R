#'
#' @title
#' @param Opt_products
#' @param current_recipes
#' @param available_resources
#' @param req_amt
#' @param max_rate
factory_binary_search <- function(Opt_products, current_recipes,
                                  available_resources, req_amt = c(2500, 500,
                                  100), max_rate = 50) {

  # Get the recipe matrix for these products
  recipe_matrix <- make_recipeMatrix(names(Opt_products), 
                                     current_recipes$graph, 
                                     current_recipes$data_frame, 
                                     "component")
  
  # A function to get the recipe names and column indices 
  # that make some provided set of products
  get_recipe_names <- function(recipe_matrix, product_names){
    
    recipe_indices <- which(recipe_matrix[product_names, ]>0, arr.ind = TRUE)[, 2]
    recipe_names   <- colnames(recipe_matrix)[recipe_indices]
    
    res <- as.numeric(recipe_indices) %>% 
      set_names(recipe_names)
    
    return(res)
  }
  
  # Define the objective function as the maximum time required to 
  # produce the required amount
  time_to_objective <- function(x){
    
    net_rate <- recipe_matrix %*% x
    
    max(net_rate[names(Opt_products),]/req_amt)
  }
  
  # Define boundaries for a new grid
  make_new_grid <- function(grid_result, starting_indices){
    
    all_statuses <- map_dbl(grid_result, function(x) purrr::pluck(x, "status"))
    
    # The maximum index of a successful solution
    success_index <- which(all_statuses == 0)
    failure_index <- which(all_statuses == 2)
    
    
  }
  
  
  # Initialize variables
  current_best_time <- Inf
  current_best_soln <- NA
  
  # Ratios of required amounts relative to the maximum required amount
  amt_ratios <- req_amt/min(req_amt)
  
  base_product_rates <- seq.int(0, max_rate, length.out = 500000)
  
  all_rates <- t(matrix(amt_ratios %x% base_product_rates, 
                        ncol = length(amt_ratios)))
  
  all_rates_list <- vector("list", length = ncol(all_rates))
  for(i in 1:length(all_rates_list)){
    all_rates_list[[i]] <- all_rates[, i] %>% 
      set_names(names(Opt_products))
  }
  
  # Initial parameters to test
  min_index <- 1
  max_index <- length(all_rates_list)
  
  index_range <- max_index - min_index
  run <- 0
  
  # Begin binary search
  while(index_range > 1){
    
    # Solve the minimum and maximum parameter combinations
    # Technically only have to do one of these after the first time but I'm lazy
    min_soln <- recipe_lp_base(products          = all_rates_list[[min_index]], 
                               startingResources = available_resources,
                               recipeData        = current_recipes$data_frame, 
                               recipeGraph       = current_recipes$graph, 
                               integerFactories  = FALSE)
    
    max_soln <- recipe_lp_base(products          = all_rates_list[[max_index]], 
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
    
    
    if(max_status == 2){       # If failure...
      failing_max_index <- max_index
      max_index   <- ceiling(max_index - ((max_index - min_index)/2))
      
    }else if(max_status == 0){ # If success...
      
      old_max_index <- max_index
      max_index     <- ceiling(max_index + ((failing_max_index - max_index)/2))
      min_index     <- old_max_index
      
    }
    
    # Update tracking variables
    index_range <- max_index - min_index
    run <- run + 1
    
  }
  
  # Set names of the solution to the appropriate recipe names
  min_soln$solution <- min_soln$solution %>% 
    set_names(colnames(recipe_matrix))
  
  # Return the max solution
  min_soln
}
