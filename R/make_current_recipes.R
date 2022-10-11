#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param RecipeData
#' @param available_alternate_recipes
make_current_recipes <- function(RecipeData, available_alternate_recipes) {

  # Make a dataframe and a graph to hold the currently available recipes. 
  # Do this by adding any alternate recipes to the previously created dataframe 
  # that ony has the non-alternate recipes and then making a graph of this dataframe
  if(!is.null(available_alternate_recipes)){
    alternate_recipes <- RecipeData$AllRecipes %>% 
      dplyr::filter(slug %in% available_alternate_recipes)
    
    final_recipe_data <- bind_rows(RecipeData$NoAlternates, alternate_recipes)
    
    final_recipe_graph <- igraph::graph_from_data_frame(final_recipe_data)
  }else{
    final_recipe_data  <- RecipeData$NoAlternates
    final_recipe_graph <- igraph::graph_from_data_frame(final_recipe_data)
  }
  
  res <- list("data_frame" = final_recipe_data, 
              "graph"      = final_recipe_graph)
return(res)
}
