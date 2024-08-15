find_limiting_resource <- function(current_recipe_matrix,
                                   binary_LP_result,
                                   available_resources){
  
  product_rates      <- current_recipe_matrix %*% binary_LP_result$solution
  consumed_resources <- match(names(available_resources), rownames(product_rates))
  consumed_resources <- consumed_resources[!is.na(consumed_resources)]
  
  product_rates[consumed_resources, ]/available_resources[names(product_rates[consumed_resources, ])]
  
  
}