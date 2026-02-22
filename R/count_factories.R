#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param CytoscapeReady_binary
#' @return
#' @author Jay Gillenwater
#' @export
count_factories <- function(CytoscapeReady_binary) {

  # Get edge attributes
  all_vertex_attrs <- get.vertex.attribute(CytoscapeReady_binary) %>% 
    as_tibble()
  
  
  factory_counts <- all_vertex_attrs %>%
    group_by(producedIn) %>%
    summarise(factory_total = sum(nFactories))
  
  res <- list("count_by_recipe"   = all_vertex_attrs,
              "count_by_building" = factory_counts)
  
  return(res)
  

}
