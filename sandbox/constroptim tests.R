##################################################
## Project: SatisfactoryCalculator
## Script purpose: Testing out the linearly constrained optimization
## functrion constrOptim as an alternative for the grid search
## Date: 2022-10-13
## Author: Jay Gillenwater
##################################################


## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

# Load in pc(roduct vector from targets file
tar_load(c(Opt_products, current_recipes, available_resources))



# Constrained optimization of the factory layout
recipe_constr_optim <- function(Opt_products, current_recipes, available_resources, req_amt){
  
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
                                              gridsize          = 15)
  
  # Find the resources that are required for the current recipes
  resource_names_matches <- match(names(available_resources), rownames(recipe_matrix))
  
  # Required resource indices
  required_resource_indices <- which(!is.na(resource_names_matches))
  
  # Initialize the rhs to all zeros
  ci  <- rep(0, nrow(recipe_matrix))
  
  # Now, for each required resource, replace the 0 with the amount of the available resource
  for(i in 1:length(required_resource_indices)){
    ci[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[i]] 
  }
  
  ci <- ci - 0.000000001

  # # A function to get an initial valid lp solution 
  # # All produce rates > 0, not consuming more resources than available
  # solve_init_lp <- function(recipe_matrix, product_names, available_resources){
  #   
  #   
  #   # Find the resources that are required for the current recipes
  #   resource_names_matches <- match(names(available_resources), rownames(recipe_matrix))
  #   
  #   # Required resource indices
  #   required_resource_indices <- which(!is.na(resource_names_matches))
  #   
  #   # Initialize the rhs to all zeros
  #   rhs <- rep(0, nrow(recipe_matrix))
  #   ci <- rep(0, nrow(recipe_matrix))
  #   
  #   # Now, for each required resource, replace the 0 with the amount of the available resource
  #   for(i in 1:length(required_resource_indices)){
  #     rhs[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[i]]
  #     ci[[resource_names_matches[[required_resource_indices[[i]]]]]] <- available_resources[[i]] 
  #   }
  #   
  #   # Constraint direction vector
  #   const_dir <- rep(">", nrow(recipe_matrix))
  #   
  #   # Get the recipe names to optimize
  #   recipe_optim_names <- get_recipe_names(recipe_matrix, names(product_names))
  #   
  #   # Objective vector
  #   obj_vector <- rep(0, ncol(recipe_matrix))
  #   obj_vector[recipe_optim_names] <- 1
  #   
  #   # Insist that net flow of heavy oil residue must equal zero
  #   # (it's practically useless (?))
  #   if("Desc_HeavyOilResidue_C" %in% rownames(recipe_matrix)){
  #     const_dir[which(rownames(recipe_matrix) == "Desc_HeavyOilResidue_C")] <- "="
  #   }
  #   
  #   ci <- ci - 0.000000001
  #   
  #   lp_soln <- lp(direction = "max", 
  #                 objective.in = obj_vector, 
  #                 const.mat = recipe_matrix, 
  #                 const.dir = const_dir, 
  #                 const.rhs = rhs)
  #   
  #   res <- list("lp_soln" = lp_soln, 
  #               "ci" = ci)
  #   
  #   return(res)
  # }
  
  # Initial lp solution
  # lp_init <- solve_init_lp(recipe_matrix, Opt_products, available_resources)
  # 
  # Set required amounts
  product_names <- Opt_products
  
  # Define the objective function as the maximim time required to 
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
  
  constr_optim_soln$par <- constr_optim_soln$par %>% 
    set_names(colnames(recipe_matrix))
  
  return(constr_optim_soln)
}


recipe_constr_optim(Opt_products, 
                    current_recipes, 
                    available_resources, 
                    c(2500,500, 100))


recipe_lp_rate_grid(startingResources = tar_read(available_resources),
                    products          = tar_read(Opt_products), 
                    # product_longnames = Opt_recipes, 
                    recipeData        = tar_read(current_recipes)$data_frame, 
                    recipeGraph       = tar_read(current_recipes)$graph, 
                    integerFactories  = FALSE,             # Basically, do you want to underclock the last factory for an item or not ***This will run VERY slowly for complicated production chains if set to TRUE***
                    reqAmt            = c(2500, 500, 100),      # How many of each product are required for the objective
                    gridsize          = 10)


