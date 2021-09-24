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
    select(product_item, name, product_per_minute, producedIn) %>% 
    unique() %>% 
    rename(longname = name)
  
  # Table to lookup in what building each item is mase
  madeIn <- itemNameConversion %>% 
    select(longname, producedIn)
  
  # A table to convert slugs (recipes) tp long names
  slug_conversion <- recipeData %>% 
    select(slug, ingredient_item, name) %>% 
    distinct()
  
  # Clean up the consumption matrix into a tidy dataframe that can be 
  # read into igraph
  itemnetwork_df <- itemConsumptionMatrix %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    pivot_longer(cols = 2:ncol(.)) %>% 
    filter(value != 0) %>% 
    relocate(name) %>%
    rename(ingredient_item = name, 
           slug            = rowname) %>% 
    inner_join(slug_conversion) %>% 
    mutate(to = dplyr::coalesce(name, slug)) %>% 
    select(-name) %>% 
    left_join(itemNameConversion, by = c("ingredient_item" = "product_item")) %>% 
    mutate(from = dplyr::coalesce(longname, ingredient_item)) %>% 
    select(from, to, value, product_per_minute) %>% 
    mutate(value          = abs(value),
           nFactories     = ceiling(value/product_per_minute), 
           productionrate = ifelse(is.na(nFactories), value, paste0(value, " (", nFactories, " factories)"))) %>% 
    rename(flow = value)
  
  # Metadata for the graph vertices
  vertexData <- tibble(longname = unlist(select(itemnetwork_df, from, to)) %>% unique()) %>% 
    left_join(madeIn)
  
  # Convert the dataframe to an igraph graph
  itemnetwork_igraph <- graph_from_data_frame(itemnetwork_df, vertices = vertexData)

}
