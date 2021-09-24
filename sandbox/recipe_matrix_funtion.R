tar_load(RecipeGraphs)
tar_load(RecipeData)

# Example of vector to pass to function
TierItems <- c("Computer")

make_recipeMatrix <- function(itemVector, recipeGraph, recipeData){
  
  # Find the item names for the clean names in the item vector
  unique_products <- recipeData %>% 
    filter(slug %in% itemVector) %>% 
    select(product_item) %>% 
    unlist() %>% 
    as.character() %>% 
    unique()
  
  # Get the neighborhood graph for each item
  all_ego_graphs <- make_ego_graph(recipeGraph, 
                                   order = diameter(recipeGraph), 
                                   nodes = unique_products, 
                                   mode  = "in")
  
  # Create a union of all the neighbourhoods and then clean up the edge data columns
  ego_union <- do.call(igraph::union, all_ego_graphs)
  
  ego_df <- igraph::as_data_frame(ego_union)
  
  GetRepeatedCols <- function(pat, df){
    colnames(df)[grepl(pat, colnames(df))]
  }
  
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
  
  
  Ingredients <- ego_df %>% 
    filter(slug != "uranium-pellet") %>%
    select(slug, from, ingredient_per_minute) %>% 
    mutate(ingredient_per_minute = -1*ingredient_per_minute) %>% 
    rename(component = from, 
           component_rate = ingredient_per_minute)
  
  Products <- ego_df %>% 
    filter(slug != "uranium-pellet") %>%
    select(slug, to, product_per_minute) %>% 
    rename(component = to, 
           component_rate = product_per_minute)
  
  recipeMatrix <- bind_rows(Ingredients, Products) %>% 
    distinct() %>% 
    pivot_wider(names_from = slug, values_from = component_rate, values_fill = 0) %>% 
    column_to_rownames("component") %>% 
    as.matrix()
  
  
  # Make a recipe matrix from this data
  # product_df <- ego_df %>% 
  #   filter(!grepl("GenericBiomass", from)) %>%
  #   select(name, to, product_per_minute) %>% 
  #   distinct() %>% 
  #   rename(ingredient_per_minute = product_per_minute, 
  #          from = to)
  # 
  # recipeMatrix <- ego_df %>% 
  #   select(from, name, ingredient_per_minute) %>% 
  #   filter(!grepl("GenericBiomass", from)) %>%
  #   mutate(ingredient_per_minute = -1*ingredient_per_minute) %>%
  #   bind_rows(product_df) %>%
  #   rename(recipe = name) %>% 
  #   distinct() %>%
  #   filter(!(from == "Desc_SulfuricAcid_C" & ingredient_per_minute == 20)) %>%
  #   pivot_wider(names_from = recipe, values_from = ingredient_per_minute, values_fill = 0) %>% 
  #   column_to_rownames("from") %>% 
  #   as.matrix()
  # 
  return(recipeMatrix)
}
