


## Section: looking at the residual plastic production tree to fix some bugs
##################################################

# Load in data
tar_load(RecipeData)
tar_load(RecipeGraphs)

plastic_items    <- c("Residual Plastic")

plastic_products <- c("Desc_Computer_C", 
                       "Desc_ModularFrameHeavy_C")


make_recipeMatrix_longnames <- function(itemVector, recipeGraph, recipeData){
  
  # Find the item names for the clean names in the item vector
  unique_products <- recipeData$NoAlternates %>% 
    filter(name %in% itemVector) %>% 
    select(product_item) %>% 
    unlist() %>% 
    as.character() %>% 
    unique()
  
  # Get the neighborhood graph for each item
  all_ego_graphs <- make_ego_graph(recipeGraph$NoAlternates, 
                                   order = diameter(recipeGraph$NoAlternates), 
                                   nodes = unique_products, 
                                   mode  = "in")
  
  # Create a union of all the neighbourhoods and then clean up the edge data columns
  ego_union <- do.call(igraph::union, all_ego_graphs)
  
  ego_df <- igraph::as_data_frame(ego_union)
  
  GetRepeatedCols <- function(pat, df){
    colnames(df)[grepl(pat, colnames(df))]
  }
  
  RepeatedCols <- c("name", 
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
  
  
  ego_df %<>% 
    mutate(recipe = paste(to, name, sep = "_"))
  
  
  product_df <- ego_df %>% 
    filter(!grepl("GenericBiomass", from)) %>%
    select(from, to, product_per_minute) %>% 
    distinct() %>% 
    rename(ingredient_per_minute = product_per_minute)
  
  recipeMatrix <- ego_df %>% 
    select(from, to, ingredient_per_minute) %>% 
    filter(!grepl("GenericBiomass", from)) %>%
    mutate(ingredient_per_minute = -1*ingredient_per_minute) %>%
    bind_rows(product_df) %>%
    rename(recipe = to) %>% 
    distinct() %>%
    pivot_wider(names_from = recipe, values_from = ingredient_per_minute, values_fill = 0) %>% 
    column_to_rownames("from") %>% 
    as.matrix()
  
  return(recipeMatrix)
}
plastic_matrix <- make_recipeMatrix_longnames(plastic_items, RecipeGraphs, RecipeData) 
