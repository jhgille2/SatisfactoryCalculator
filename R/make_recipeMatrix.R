#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param product_longnames
#' @param recipeGraph
#' @param recipeData
make_recipeMatrix <- function(itemVector, recipeGraph, recipeData) {

  # Find the item names for the clean names in the item vector
  unique_products <- recipeData %>% 
    filter(slug %in% itemVector) %>% 
    select(product_item) %>% 
    unlist() %>% 
    as.character() %>% 
    unique()
  
  # Get the neighborhood graph for each item (the production chain for each item)
  all_ego_graphs <- make_ego_graph(recipeGraph, 
                                   order = diameter(recipeGraph), 
                                   nodes = unique_products, 
                                   mode  = "in")
  
  # Create a union of all the neighborhoods and then clean up the edge data columns
  ego_union <- do.call(igraph::union, all_ego_graphs)
  
  ego_df <- igraph::as_data_frame(ego_union)
  
  GetRepeatedCols <- function(pat, df){
    colnames(df)[grepl(pat, colnames(df))]
  }
  
  # Creating a union of all the graphs will repeat edge properties in the resulting
  # dataframe wherever edge attribute names were the same. I want to coalesce these
  # attributes down to a single column for each property for plotting later on
  RepeatedCols <- c("name", 
                    "slug",
                    "producedIn", 
                    "ingredient_amount", 
                    "product_amount", 
                    "time", 
                    "ingredient_per_minute", 
                    "product_per_minute")
  
  RepeatColNames <- map(RepeatedCols, GetRepeatedCols, ego_df) %>% 
    set_names(RepeatedCols)
  
  for(i in 1:length(RepeatColNames)){
    
    NewColName <- names(RepeatColNames)[[i]]
    
    ego_df %<>%
      mutate(!!NewColName := dplyr::coalesce(!!!rlang::syms(RepeatColNames[[i]])), .keep = "unused")
  }
  
  # Pivot the data into a wide format so that each recipe is a column
  # and each ingredient is a row and values in the matrix are negative
  # if a recipe consumes an ingredient and positive if the recipe produces it
  Ingredients <- ego_df %>% 
    filter(slug != "uranium-pellet") %>% # I'm not sure what's up with this recipe so I'm just removing it for now
    select(slug, from, ingredient_per_minute) %>% 
    mutate(ingredient_per_minute = -1*ingredient_per_minute) %>% 
    rename(component = from, 
           component_rate = ingredient_per_minute)
  
  Products <- ego_df %>% 
    filter(slug != "uranium-pellet") %>% # Removing uranium pellets for now cause the recipe is annoying and I wont be making them for a while
    select(slug, to, product_per_minute) %>% 
    rename(component = to, 
           component_rate = product_per_minute)
  
  recipeMatrix <- bind_rows(Ingredients, Products) %>% 
    distinct() %>% 
    pivot_wider(names_from = slug, values_from = component_rate, values_fill = 0) %>% 
    column_to_rownames("component") %>% 
    as.matrix()
  
  return(recipeMatrix)
}
