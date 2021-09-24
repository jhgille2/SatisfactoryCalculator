list.files("C:/Program Files/Epic Games/SatisfactoryEarlyAccess/CommunityResources/Docs", full.names = TRUE)

datasource_1 <- "https://raw.githubusercontent.com/factoriolab/factorio-lab/main/src/data/sfy/data.json"
datasource_2 <- "https://raw.githubusercontent.com/greeny/SatisfactoryTools/dev/data/data.json"

recipes <- fromJSON(datasource_2, flatten = TRUE)


clean_recipe <- function(recipe){
  
  emptyElements <- map_lgl(recipe, purrr::is_empty)
  
  recipe[emptyElements] <- as.character(NA)
  
  as_tibble(recipe)
  
}


AllRecipes <- map(recipes$recipes, clean_recipe) %>% 
  reduce(bind_rows)


Recipe_Edgelist <- AllRecipes %>% 
  select(ingredients, products, name, slug, producedIn, time)


Ingr     <- reduce(Recipe_Edgelist[1], tibble) %>% 
  rename(ingredient_item = item, 
         ingredient_amount = amount)

Products <- reduce(Recipe_Edgelist[2], tibble) %>% 
  rename(product_item = item, 
         product_amount = amount)

Recipe_Edgelist_Clean <- bind_cols(Ingr, 
                                   Products, 
                                   Recipe_Edgelist[3],
                                   Recipe_Edgelist[4], 
                                   Recipe_Edgelist[5], 
                                   Recipe_Edgelist[6]) %>% 
  select(ingredient_item, product_item, name, slug, producedIn, ingredient_amount, product_amount, time) %>% 
  mutate(ingredient_per_minute = ingredient_amount*(60/time), 
         product_per_minute = product_amount*(60/time))


Component_Edgelist <- Recipe_Edgelist_Clean %>% 
  filter(!is.na(producedIn))

Component_Edgelist_NoAlternate <- Component_Edgelist %>% 
  filter(!grepl("Alternate", name))

res <- list("AllRecipes"     = Recipe_Edgelist_Clean, 
            "ComponentsOnly" = Component_Edgelist, 
            "NoAlternates"   = Component_Edgelist_NoAlternate)


Component_Edgelist_NoAlternate %>% 
  select(slug, ingredient_item, product_item, ingredient_per_minute, product_per_minute) %>%
  pivot_longer(cols = c(ingredient_per_minute, product_per_minute))

Ingredients <- Component_Edgelist_NoAlternate %>% 
  filter(slug != "uranium-pellet") %>%
  select(slug, ingredient_item, ingredient_per_minute) %>% 
  mutate(ingredient_per_minute = -1*ingredient_per_minute) %>% 
  rename(component = ingredient_item, 
         component_rate = ingredient_per_minute)

Products <- Component_Edgelist_NoAlternate %>% 
  filter(slug != "uranium-pellet") %>%
  select(slug, product_item, product_per_minute) %>% 
  rename(component = product_item, 
         component_rate = product_per_minute)

recipes_long <- bind_rows(Ingredients, Products) %>% 
  distinct()

recipes_wide <- pivot_wider(recipes_long, names_from = slug, values_from = component_rate, values_fill = 0)
