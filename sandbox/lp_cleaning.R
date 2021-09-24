
recipeCounts <- RecipeData$NoAlternates %>% 
  select(name, product_item) %>% 
  group_by(name) %>% 
  summarise(nRecipes = length(unique(product_item))) %>% 
  filter(nRecipes == 1)

onerecipeitems <- filter(RecipeData$NoAlternates, name %in% recipeCounts$name)

RecipeData$NoAlternates %>% 
  filter(product_item %in% onerecipeitems$product_item) %>% 
  select(product_item, name, product_per_minute, producedIn) %>% 
  unique() %>% 
  rename(longname = name) -> itemNameConversion

madeIn <- itemNameConversion %>% 
  select(longname, producedIn)

slug_conversion <- RecipeData$NoAlternates %>% 
  select(slug, ingredient_item, name) %>% 
  distinct()


itemConsumptionMatrix <- t(make_recipeMatrix(computer_items, recipeGraph = RecipeGraphs$NoAlternates, recipeData = RecipeData$NoAlternates)) * soln_computer_int$solution[[1]]

itemnetwork_df <- itemConsumptionMatrix %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  filter(value != 0) %>% 
  relocate(name) %>%
  rename(ingredient_item = name, 
         slug            = rowname)

nameConversion <- RecipeData$NoAlternates %>% 
  filter(product_item %in% test$name) %>% 
  select(product_item, name) %>%
  rename(longname = product_item, shortname = name) %>% 
  distinct()

itemnetwork_df <- inner_join(itemnetwork_df, slug_conversion) %>% 
  mutate(to = dplyr::coalesce(name, slug)) %>% 
  select(-name) %>% 
  left_join(itemNameConversion, by = c("ingredient_item" = "product_item")) %>% 
  mutate(from = dplyr::coalesce(longname, ingredient_item)) %>% 
  select(from, to, value, product_per_minute) %>% 
  mutate(value          = abs(value),
         nFactories     = ceiling(value/product_per_minute), 
         productionrate = ifelse(is.na(nFactories), value, paste0(value, " (", nFactories, " factories)"))) %>% 
  rename(flow = value)


# %>% 
#   left_join(itemNameConversion, by = c("name" = "product_item")) %>% 
#   mutate(from           = dplyr::coalesce(longname, name),
#          value          = abs(value),
#          nFactories     = ceiling(value/product_per_minute), 
#          productionrate = ifelse(is.na(nFactories), value, paste0(value, " (", nFactories, " factories)"))) %>% 
#   select(from, rowname, value, nFactories, productionrate) %>% 
#   inner_join(slug_conversion, by = c("rowname" = "slug")) %>%
#   select(from, name, value, nFactories, productionrate) %>%
#   rename(to   = name, 
#          flow = value) %>% 
#   filter(from != to)

vertexData <- tibble(longname = unlist(select(itemnetwork_df, from, to)) %>% unique()) %>% 
  left_join(madeIn)

itemnetwork_igraph <- graph_from_data_frame(itemnetwork_df, vertices = vertexData)

createNetworkFromIgraph(itemnetwork_igraph)


# A function to clean output from the linear programming function
clean_lp <- function(itemVec, recipeGraph, recipeData, lp_solution){
  
  # A table to convert cumbersome item names to a more readable format
  itemNameConversion <- recipeData %>% 
    filter(product_item %in% onerecipeitems$product_item) %>% 
    select(product_item, name) %>% 
    unique() %>% 
    rename(longname = name) 
  
  # Get the rate at which each item is consumed by other items
  itemConsumptionMatrix <- make_recipeMatrix(crystal_items, recipeGraph = RecipeGraphs, recipeData = RecipeData) %>% t() * soln_crystal_int$solution[[1]]
  
  # Merge the consumption matrix with the name conversion table to clean up names and then pivot to a dataframe thats ready 
  # for use in igraph
  itemnetwork_df <- itemConsumptionMatrix %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    pivot_longer(cols = 2:ncol(.)) %>% 
    filter(value != 0) %>% 
    relocate(name) %>% 
    left_join(itemNameConversion, by = c("name" = "product_item")) %>% 
    mutate(from           = dplyr::coalesce(longname, name),
           value          = abs(value),
           nFactories     = ceiling(value/product_per_minute), 
           productionrate = ifelse(is.na(nFactories), value, paste0(value, " (", nFactories, " factories)"))) %>% 
    select(from, rowname, value, nFactories, productionrate) %>% 
    rename(to   = rowname, 
           flow = value) %>% 
    filter(from != to)
  
  itemnetwork_igraph <- graph_from_data_frame(itemnetwork_df)
  
  return(itemnetwork_igraph)
}