
computer_matrix <- make_recipeMatrix(computer_items, recipeGraph = RecipeGraphs$NoAlternates, recipeData = RecipeData$NoAlternates)

recipeCounts <- RecipeData$NoAlternates %>% 
  select(name, product_item) %>% 
  group_by(name) %>% 
  summarise(nRecipes = length(unique(product_item))) %>% 
  filter(nRecipes == 1)

onerecipeitems <- filter(RecipeData$NoAlternates, name %in% recipeCounts$name)

itemNameConversion <- RecipeData$NoAlternates %>% 
  filter(product_item %in% onerecipeitems$product_item) %>% 
  select(product_item, slug, product_per_minute, producedIn) %>% 
  unique()

madeIn <- itemNameConversion %>% 
  select(product_item, producedIn)

itemConsumptionMatrix <- t(computer_matrix) * soln_computer_int$solution[[1]]

item_df <- itemConsumptionMatrix %>%
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  rename(recipe = rowname, item = name) %>% 
  filter(value != 0) %>%
  mutate(direction = ifelse(value < 0, "ingredient", "product")) 

ingredient_df <- item_df %>% filter(direction == "ingredient") %>% 
  rename(ingredient_item = item, 
         ingredient_amt = value) %>% 
  select(-direction)

product_df    <- item_df  %>% filter(direction == "product") %>% 
  rename(product_item = item, 
         product_amt  = value) %>% 
  select(- direction)

recipe_data <- RecipeData$NoAlternates %>% 
  select(ingredient_item, product_item, slug, product_per_minute) %>% 
  distinct()

all_ingredients <- left_join(ingredient_df, product_df, by = "recipe") %>% 
  rename(from = ingredient_item, to = product_item) %>% 
  select(from, to, recipe, ingredient_amt, product_amt) %>% 
  mutate(ingredient_amt = -1*ingredient_amt) %>% 
  left_join(recipe_data, by = c("recipe" = "slug", "from" = "ingredient_item", "to" = "product_item")) %>% 
  mutate(nFactories     = ceiling(product_amt/product_per_minute), 
         productionrate = ifelse(is.na(nFactories), ingredient_amt, paste0(ingredient_amt, " (", nFactories, " factories)"))) %>% 
  rename(flow = ingredient_amt)

vertexData <- tibble(product_item = unlist(select(all_ingredients, from, to)) %>% unique()) %>% 
  left_join(madeIn) %>% 
  distinct()

itemnetwork_igraph <- graph_from_data_frame(all_ingredients, vertices = vertexData)

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