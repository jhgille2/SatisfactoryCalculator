#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param url
get_recipe_data <- function(url = Recipe_url) {
  
  recipes <- fromJSON(url, flatten = TRUE)
  
  
  clean_recipe <- function(recipe){
    
    id_cols <- c("slug", "name", "className", 
                 "alternate", "time", "forBuilding", 
                 "inHand", "inMachine", 
                 "inWorkshop", 
                 "producedIn", "isVariablePower")
    
    ingr_cols <- c(id_cols, "ingredients")
    prod_cols <- c(id_cols, "products")
    
    emptyElements <- map_lgl(recipe, purrr::is_empty)
    
    recipe[emptyElements] <- as.character(NA)
    
    ingredient_tibble <- as_tibble(recipe[ingr_cols])
    product_tibble    <- as_tibble(recipe[prod_cols])
    
    res <- list("ingredient_table" = ingredient_tibble, 
                "product_table" = product_tibble)
    
    return(res)
  }
  
  
  
  AllRecipes <- map(recipes$recipes, clean_recipe)
  
  all_ingredients <- map(AllRecipes, pluck("ingredient_table") ) %>%
    reduce(bind_rows)
  
  ingredient_df <- map_dfc(all_ingredients$ingredients, tibble) %>% 
    set_names(c("ingredient_item", "ingredient_amount"))
  
  all_ingredients %<>%
    select(-ingredients) %>% 
    bind_cols(ingredient_df)
  
  
  
  all_products <- map(AllRecipes, pluck("product_table") ) %>%
    reduce(bind_rows)
  
  product_df <- map_dfc(all_products$products, tibble) %>% 
    set_names(c("product_item", "product_amount"))
  
  all_products %<>%
    select(-products) %>%
    bind_cols(product_df)
  
  Recipe_Edgelist_Clean <- full_join(all_ingredients, all_products, 
                                     by = c("slug", "name", "className", "alternate", 
                                            "time", "forBuilding", "inHand", 
                                            "inMachine", "inWorkshop", "producedIn", 
                                            "isVariablePower")) %>% 
    select(ingredient_item, product_item, name, slug, producedIn, ingredient_amount, product_amount, time) %>% 
    mutate(ingredient_per_minute = ingredient_amount*(60/time), 
           product_per_minute = product_amount*(60/time))
  
  problem_recipes <- Recipe_Edgelist_Clean %>% 
    filter(ingredient_item == product_item)
  
  problem_recipes <- Recipe_Edgelist_Clean %>% 
    filter(slug %in% problem_recipes$slug, 
           product_item %in% problem_recipes$product_item) %>% 
    mutate(ingredient_per_minute = ingredient_per_minute - product_per_minute, product_per_minute = NA)
  
  Recipe_Edgelist_Clean %<>% 
    filter(!(slug %in% problem_recipes$slug & product_item %in% problem_recipes$product_item)) %>%
    bind_rows(problem_recipes)
  
  # Recipe_Edgelist <- AllRecipes %>% 
  #   select(ingredients, products, name, slug, producedIn, time)
  # 
  # 
  # Ingr     <- reduce(Recipe_Edgelist[1], tibble) %>% 
  #   rename(ingredient_item = item, 
  #          ingredient_amount = amount)
  # 
  # Products <- reduce(Recipe_Edgelist[2], tibble) %>% 
  #   rename(product_item = item, 
  #          product_amount = amount)
  # 
  # Recipe_Edgelist_Clean <- bind_cols(Ingr, 
  #                                    Products, 
  #                                    Recipe_Edgelist[3],
  #                                    Recipe_Edgelist[4], 
  #                                    Recipe_Edgelist[5], 
  #                                    Recipe_Edgelist[6]) %>% 
  #   select(ingredient_item, product_item, name, slug, producedIn, ingredient_amount, product_amount, time) %>% 
  #   mutate(ingredient_per_minute = ingredient_amount*(60/time), 
  #          product_per_minute = product_amount*(60/time))
  
  
  Component_Edgelist <- Recipe_Edgelist_Clean %>% 
    filter(!is.na(producedIn))
  
  Component_Edgelist_NoAlternate <- Component_Edgelist %>% 
    filter(!grepl("Alternate", name))
  
  res <- list("AllRecipes"     = Recipe_Edgelist_Clean, 
              "ComponentsOnly" = Component_Edgelist, 
              "NoAlternates"   = Component_Edgelist_NoAlternate)
  
  return(res)
}
