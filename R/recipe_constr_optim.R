#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param Opt_products
#' @param current_recipes
#' @param available_resources
#' @param req_amt
recipe_constr_optim <- function(Opt_products, current_recipes,
                                available_resources, req_amt = c(2500, 500,
                                100)) {

  
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
  
  
  # Get a set of solutions from a sparse grid search
  grid_search_result <-   recipe_lp_rate_grid(startingResources = available_resources,
                                              products          = Opt_products, 
                                              # product_longnames = Opt_recipes, 
                                              recipeData        = current_recipes$data_frame, 
                                              recipeGraph       = current_recipes$graph, 
                                              integerFactories  = FALSE,             
                                              reqAmt            = req_amt,      
                                              gridsize          = 20)
  
  # Find the resources that are required for the current recipes
  resource_names_matches <- match(names(available_resources), rownames(recipe_matrix))
  
  # Required resource indices
  required_resource_indices <- which(!is.na(resource_names_matches))
  
  # Initialize the rhs to all zeros
  ci  <- rep(0, nrow(recipe_matrix))
  
  # Now, for each required resource, replace the 0 with the amount of the available resource
  for(i in 1:length(required_resource_indices)){
    ci[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[required_resource_indices[[i]]]] 
  }
  
  ci <- ci - 0.000000001
  
  # Set required amounts
  product_names <- Opt_products
  
  # Define the objective function as the maximum time required to 
  # produce the required amount
  time_to_objective <- function(x){
    
    net_rate <- recipe_matrix %*% x
    
    if(any(recipe_matrix %*% x - ci < 0)){
      return(Inf)
    }
    
    max(net_rate[names(product_names),]/req_amt)
  }
  
  # Do the constrained optimizatiom
  constr_optim_soln <- constrOptim(theta = grid_search_result$solution[[1]],
                                   f = time_to_objective, 
                                   grad = NULL, 
                                   ui = recipe_matrix, 
                                   ci = ci)
  
  # Name the elements of the solution vector with the recipe names
  constr_optim_soln$par <- constr_optim_soln$par %>% 
    set_names(colnames(recipe_matrix))
  
  return(constr_optim_soln)

}
