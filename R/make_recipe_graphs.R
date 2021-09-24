#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param recipetables
make_recipe_graphs <- function(recipetables = RecipeData) {

  NoAlternate_graph <- igraph::graph_from_data_frame(recipetables$NoAlternates)
  
  
  res <- list("NoAlternates" = NoAlternate_graph)
  
  return(res)

}
