##################################################
## Project: Satisfactory Calculator
## Script purpose: Development of more efficient search 
## for optimal factory
## Date: 2022-10-27
## Author: Jay Gillenwater
##################################################


tar_load(c(current_recipes, 
           Opt_products, 
           available_resources))

# The amount required of each product for the objective
req_amt <- c(2500, 500, 100)

# Ratios of required amounts relative to the maximum required amount
amt_ratios <- req_amt/min(req_amt)

max_rate <- 50

base_product_rates <- seq.int(0, max_rate, length.out = 100000)

all_rates <- t(matrix(amt_ratios %x% base_product_rates, 
                    ncol = length(amt_ratios)))

all_rates_list <- vector("list", length = ncol(all_rates))
for(i in 1:length(all_rates_list)){
  all_rates_list[[i]] <- all_rates[, i] %>% 
    set_names(names(Opt_products))
}


# First five points
first_five_indices <- ceiling(seq.int(1, length(all_rates_list), length.out = 5))


first_five_products <- all_rates_list[first_five_indices]

test <- map(first_five_products, 
            function(x) recipe_lp_base(products          = x, 
                                       startingResources = available_resources,
                                       recipeData        = current_recipes$data_frame, 
                                       recipeGraph       = current_recipes$graph, 
                                       integerFactories  = FALSE))

# Define boundaries for a new grid
make_new_grid <- function(grid_result, starting_indices){
  
  all_statuses <- map_dbl(grid_result, function(x) purrr::pluck(x, "status"))
  
  # The maximum index of a successful solution
  max_success <- max(which(all_statuses == 0))
  
  new_range <- c(starting_indices[max_success], starting_indices[max_success + 1])
  
  return(ceiling(new_range))
}

next_range <- make_new_grid(test, first_five_indices)
next_rates <- seq.int(1, 500, length.out = 100) %>% ceiling()
next_five_products <- all_rates_list[next_rates]

test2 <- map(next_five_products, 
             function(x) recipe_lp_base(products          = x, 
                                        startingResources = available_resources,
                                        recipeData        = current_recipes$data_frame, 
                                        recipeGraph       = current_recipes$graph, 
                                        integerFactories  = FALSE)) 


# General sequence
# 1. Make a big grid of potential solutions with the appropriate ratios
# 2. Choose 5(?) evenly spaced integers in the range of the number of potential solutions
# 3. Attempt to find a solution for each of the parameter choices 
# 4. Identify the largest index that had a successful solution
# 5. Recorsd the time to the objective of the successful index
# 5a. If the time is less than the current best time, replace the best solution with the current solution
# 6. Go back to 2, but update the range to be between the largest index, and the next closest unsuccessful index. 
# 7. Continue until the index range is less than 5 

sparse_grid_search <- function(Opt_products, 
                               current_recipes, 
                               available_resources, 
                               req_amt = c(2500, 500, 100), 
                               max_rate = 50){
  
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
    
    max(net_rate[names(product_names),]/req_amt)
  }
  
  # Define boundaries for a new grid
  make_new_grid <- function(grid_result, starting_indices){
    
    all_statuses <- map_dbl(grid_result, function(x) purrr::pluck(x, "status"))
    
    # The maximum index of a successful solution
    max_success <- max(which(all_statuses == 0))
    
    # Get the LP solution with the best time
    successful_solutions <- which(all_statuses == 0)
    successful_lps <- grid_result[successful_solutions]
    
    times_to_objectives <- map_dbl(successful_lps, function(x) time_to_objective(pluck(x, "solution")))
    fastest_soln <- which(times_to_objectives == min(times_to_objectives))[[1]]
    
    fastest_index <- successful_solutions[[fastest_soln]]
    
    new_range <- c(fastest_index, starting_indices[max_success + 1])
    
    return(ceiling(new_range))
  }
  
  
  # Initialize variables
  current_best_time <- Inf
  current_best_soln <- NA

  # Ratios of required amounts relative to the maximum required amount
  amt_ratios <- req_amt/min(req_amt)

  base_product_rates <- seq.int(0, max_rate, length.out = 100000)
  
  all_rates <- t(matrix(amt_ratios %x% base_product_rates, 
                        ncol = length(amt_ratios)))
  
  all_rates_list <- vector("list", length = ncol(all_rates))
  for(i in 1:length(all_rates_list)){
    all_rates_list[[i]] <- all_rates[, i] %>% 
      set_names(names(Opt_products))
  }
  
  # Initial parameters to test
  search_indices <- ceiling(seq.int(1, length(all_rates_list), length.out = 5))
  
  index_range <- range(search_indices)[2] - range(search_indices)[1]
  
  while(index_range >=5){
    
    current_parameters <- all_rates_list[search_indices]
    
    current_solutions <- map(current_parameters, 
                function(x) recipe_lp_base(products          = x, 
                                           startingResources = available_resources,
                                           recipeData        = current_recipes$data_frame, 
                                           recipeGraph       = current_recipes$graph, 
                                           integerFactories  = FALSE))
    
    new_range <- make_new_grid(current_solutions, search_indices)
    
    search_indices <- seq.int(new_range[[1]], new_range[[2]], length.out = 5) %>% ceiling()
    index_range <- range(search_indices)[2] - range(search_indices)[1]
  }
  
}
