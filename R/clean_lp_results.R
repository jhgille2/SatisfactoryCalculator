#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lp_table
clean_lp_results <- function(lp_table = LP_result, recipeData = RecipeData$NoAlternates, recipeGraph = RecipeGraphs$NoAlternates, products = Opt_recipes) {

  # Calculate how many items are being produces and consumed from the lp solution
  # Can extend this in the future to apply the function to each solution, realistically I think I should focus
  # on making a better objective function instead though
  itemConsumptionMatrix <- t(make_recipeMatrix(products, recipeGraph = recipeGraph, recipeData = recipeData)) * lp_table$solution[[1]]
  
  recipeCounts <- recipeData %>% 
    select(name, product_item) %>% 
    group_by(name) %>% 
    summarise(nRecipes = length(unique(product_item))) %>% 
    filter(nRecipes == 1)
  
  onerecipeitems <- filter(recipeData, name %in% recipeCounts$name)
  
  # A table to convert ingredients to longer (and prettier) names
  itemNameConversion <- recipeData %>% 
    filter(product_item %in% onerecipeitems$product_item) %>% 
    select(product_item, slug, product_per_minute, producedIn) %>% 
    unique()
  
  # Table to lookup in what building each item is mase
  madeIn <- itemNameConversion %>% 
    select(product_item, producedIn)
  
  # A table to convert slugs (recipes) tp long names
  slug_conversion <- recipeData %>% 
    select(slug, ingredient_item, name) %>% 
    distinct()
  
  # Clean up the consumption matrix into a tidy dataframe that can be 
  # read into igraph
  item_df <- itemConsumptionMatrix %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    pivot_longer(cols = 2:ncol(.)) %>% 
    rename(recipe = rowname, item = name) %>% 
    filter(value != 0) %>%
    mutate(direction = ifelse(value < 0, "ingredient", "product")) 
  
  # Split the dataframe into ingredient and product dataframes
  ingredient_df <- item_df %>% filter(direction == "ingredient") %>% 
    rename(ingredient_item = item, 
           ingredient_amt = value) %>% 
    select(-direction)
  
  product_df    <- item_df  %>% filter(direction == "product") %>% 
    rename(product_item = item, 
           product_amt  = value) %>% 
    select(-direction)
  
  recipe_data <- recipeData %>% 
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
  
  vertexData <- all_ingredients %>% 
    select(to, product_amt, product_per_minute) %>% 
    distinct()%>%
    mutate(nFactories = product_amt/product_per_minute) %>% 
    select(to, nFactories)
  
  all_components <- all_ingredients %>% 
    select(from, to) %>% 
    unlist() %>% 
    unique()
  
  excludedComponents <- tibble(to = all_components[!(all_components %in% vertexData$to)], 
                               nFactories = NA)
  
  vertexData %<>%
    bind_rows(excludedComponents) %>% 
    left_join(madeIn, by = c("to" = "product_item")) %>% 
    distinct() %>% 
    group_by(to) %>% 
    summarise(nFactories = sum(nFactories, na.rm = TRUE), 
              producedIn = toString(producedIn))
  
  # Metadata for the graph vertices
  # vertexData <- tibble(product_item = unlist(select(all_ingredients, from, to)) %>% unique()) %>% 
  #   left_join(madeIn) %>% 
  #   distinct()
  
  # Convert the dataframe to an igraph graph
  itemnetwork_igraph <- graph_from_data_frame(all_ingredients, vertices = vertexData)

  return(itemnetwork_igraph)
}
