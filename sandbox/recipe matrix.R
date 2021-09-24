tar_load(RecipeData)
tar_load(RecipeGraphs)

# TierItems <- c("Motor", 
#                "Encased Industrial Beam", 
#                "Steel Pipe", 
#                "Copper Sheet", 
#                "Plastic", 
#                "Rubber", 
#                "Cable", 
#                "Heavy Modular Frame", 
#                "Wire", 
#                "Fabric", 
#                "Computer", 
#                "Packaged Fuel", 
#                "Steel Beam")

TierItems <- c("Computer")

unique_products <- RecipeData$NoAlternates %>% 
  filter(name %in% TierItems) %>% 
  select(product_item) %>% 
  unlist() %>% 
  as.character() %>% 
  unique()

oil_graph <- make_ego_graph(RecipeGraphs$NoAlternates, 
                            order = diameter(RecipeGraphs$NoAlternates), 
                            nodes = "Desc_LiquidOil_C", 
                            mode = "out")

all_ego_graphs <- make_ego_graph(RecipeGraphs$NoAlternates, 
                                 order = diameter(RecipeGraphs$NoAlternates), 
                                 nodes = unique_products, 
                                 mode  = "in")

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

ego_graph <- graph_from_data_frame(ego_df)

createNetworkFromIgraph(ego_graph)

#ego_df <- igraph::as_data_frame(RecipeGraphs$NoAlternates)


product_df <- ego_df %>% 
  filter(!grepl("GenericBiomass", from)) %>%
  select(name, to, product_per_minute) %>% 
  distinct() %>% 
  rename(ingredient_per_minute = product_per_minute, 
         from = to)

# oreTibble <- tibble(name = c("Iron Ore"), 
#                     from = c("Desc_OreIron_C"), 
#                     ingredient_per_minute = c(720))

recipeMatrix <- ego_df %>% 
  select(from, name, ingredient_per_minute) %>% 
  filter(!grepl("GenericBiomass", from)) %>%
  mutate(ingredient_per_minute = -1*ingredient_per_minute) %>%
  bind_rows(product_df) %>%
  rename(recipe = name) %>% 
  distinct() %>%
  filter(!(from == "Desc_SulfuricAcid_C" & ingredient_per_minute == 20)) %>%
  pivot_wider(names_from = recipe, values_from = ingredient_per_minute, values_fill = 0) %>% 
  column_to_rownames("from") %>% 
  as.matrix()





