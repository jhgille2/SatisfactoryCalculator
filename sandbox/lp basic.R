recipe_lp_base <- function(startingResources = available_resources,
                                     products = Opt_products, product_longnames =
                                     Opt_recipes, recipeData =
                                     RecipeData$NoAlternates, recipeGraph =
                                     RecipeGraphs$NoAlternates, integerFactories =
                                     TRUE) {

  recipeMatrix <- make_recipeMatrix(product_longnames, recipeGraph, recipeData)
  
  # Constraint direction and right hand side for the linear program
  const_dir <- rep(">", nrow(recipeMatrix))
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
  const_dir[match(names(products), rownames(recipeMatrix))] <- ">"
  rhs[match(names(products), rownames(recipeMatrix))] <- products
  # for(i in 1:length(products)){
  #   
  #   currentRes            <- which(rownames(recipeMatrix) ==  names(products)[[i]])
  #   const_dir[currentRes] <- ">"
  #   rhs[currentRes]       <- products[[i]]
  #   
  # }
  
  rhs <- as.numeric(rhs)

  objective_indices <- match(product_longnames, colnames(recipeMatrix))
  #obj_coefs <- recipeRates/reqAmt

  
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


