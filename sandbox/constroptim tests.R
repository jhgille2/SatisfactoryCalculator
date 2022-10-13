
# Load in pc(roduct vector from targets file
tar_load(c(Opt_products, current_recipes, available_resources))

# Get the recipe matrix for these products
example_recipe_matrix <- make_recipeMatrix(names(Opt_products), 
                                           current_recipes$graph, 
                                           current_recipes$data_frame, 
                                           "component")


# A function to get the recipe names and column indices 
# that make some provided set of products
get_recipe_names <- function(recipe_matrix, product_names){
  
  recipe_indices <- which(recipe_matrix[product_names, ]>0, arr.ind = TRUE)[, 2]
  recipe_names   <- colnames(recipe_matrix)[recipe_indices]
  
  res <- c(recipe_names = recipe_indices)
  
  return(res)
}


# A function to get an initial valid lp solution 
#
# All produce rates > 0, not consuming more resources than available

solve_init_lp <- function(recipe_matrix, product_names, available_resources){
  
  
  # Find the resources that are required for the current recipes
  resource_names_matches <- match(names(available_resources), rownames(recipe_matrix))
  
  # Required resource indices
  required_resource_indices <- which(!is.na(resource_names_matches))
  
  # Initialize the rhs to all zeros
  rhs <- rep(0.01, nrow(recipe_matrix))
  ci <- rep(0.01, nrow(recipe_matrix))
  
  # Now, for each required resource, replace the 0 with the amount of the available resource
  for(i in 1:length(required_resource_indices)){
    rhs[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[i]] + 0.01
    ci[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[i]] 
  }
  
  # Constraint direction vector
  const_dir <- rep(">", nrow(recipe_matrix))
  
  # Get the recipe names to optimize
  recipe_optim_names <- get_recipe_names(recipe_matrix, names(product_names))
  
  # Objective vector
  obj_vector <- rep(0, ncol(recipe_matrix))
  obj_vector[recipe_optim_names] <- 1
  
  
  lp_soln <- lp(direction = "max", 
                objective.in = obj_vector, 
                const.mat = recipe_matrix, 
                const.dir = const_dir, 
                const.rhs = rhs)
  
  ci[ci == 0.01] <- 0
  
  res <- list("lp_soln" = lp_soln, 
              "ci" = ci)
  
  return(res)
}

req_amt <- c(50, 50)
product_names <- Opt_products

time_to_objective <- function(x){
  
  net_rate <- example_recipe_matrix %*% x
  
  max(net_rate[names(product_names),]/req_amt)
  
}

example_lp_init <- solve_init_lp(example_recipe_matrix, Opt_products, available_resources)

constr_optim_soln <- constrOptim(theta = example_lp_init$lp_soln$solution,
                                f = time_to_objective, 
                                grad = NULL, 
                                ui = example_recipe_matrix, 
                                ci = example_lp_init$ci, 
                                outer.iterations = 100000)
