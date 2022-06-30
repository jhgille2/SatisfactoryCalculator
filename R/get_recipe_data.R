#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param url
get_recipe_data <- function(url = Recipe_url) {
  
  # Download the recipe data json file and flatten it outr
  recipes <- fromJSON(url, flatten = TRUE)
  
  # A function to clean individual recipes
  clean_recipe <- function(recipe){
    
    # Vectors to identify identification, ingredient, and product
    # column names
    id_cols <- c("slug", "name", "className", 
                 "alternate", "time", "forBuilding", 
                 "inHand", "inMachine", 
                 "inWorkshop", 
                 "producedIn", "isVariablePower")
    
    ingr_cols <- c(id_cols, "ingredients")
    prod_cols <- c(id_cols, "products")
    
    # Find empty components of the recipe and replace empty elements with NA
    emptyElements <- map_lgl(recipe, purrr::is_empty)
    recipe[emptyElements] <- as.character(NA)
    
    # Convert the ingredient and product elements to tibbles
    ingredient_tibble <- as_tibble(recipe[ingr_cols])
    product_tibble    <- as_tibble(recipe[prod_cols])
    
    # Return a list of two tibbles, one for the ingredients and 
    # one for the products
    res <- list("ingredient_table" = ingredient_tibble, 
                "product_table" = product_tibble)
    
    return(res)
  }
  
  # Use this function to clean all the recipes
  AllRecipes <- map(recipes$recipes, clean_recipe)
  
  # Grab the ingredient tibbles and merge them all
  all_ingredients <- map(AllRecipes, pluck("ingredient_table") ) %>%
    purrr::reduce(dplyr::bind_rows)
  
  # Clean up the nested ingredient table and bind this back to the 
  # starting ingredient data
  ingredient_df <- map_dfc(all_ingredients$ingredients, tibble) %>% 
    set_names(c("ingredient_item", "ingredient_amount"))

  all_ingredients %<>%
    select(-ingredients) %>% 
    dplyr::bind_cols(ingredient_df)
  
  # And do the same for the products
  all_products <- map(AllRecipes, pluck("product_table") ) %>%
    reduce(bind_rows)
  
  product_df <- map_dfc(all_products$products, tibble) %>% 
    set_names(c("product_item", "product_amount"))
  
  all_products %<>%
    select(-products) %>%
    dplyr::bind_cols(product_df)
  
  
  # Merge the ingredient and product tibbles and add columns for 
  # item production and consumption rates
  Recipe_Edgelist_Clean <- full_join(all_ingredients, all_products, 
                                     by = c("slug", "name", "className", "alternate", 
                                            "time", "forBuilding", "inHand", 
                                            "inMachine", "inWorkshop", "producedIn", 
                                            "isVariablePower")) %>% 
    select(ingredient_item, product_item, name, slug, producedIn, ingredient_amount, product_amount, time) %>% 
    mutate(ingredient_per_minute = ingredient_amount*(60/time), 
           product_per_minute = product_amount*(60/time))
  
  # Identify recipes that produce an item they consume
  # These mess with the LP solver
  problem_recipes <- Recipe_Edgelist_Clean %>% 
    filter(ingredient_item == product_item)
  
  # Fix these recipes so that the item consumption rate is equal to the
  # net rate
  problem_recipes <- Recipe_Edgelist_Clean %>% 
    filter(slug %in% problem_recipes$slug, 
           product_item %in% problem_recipes$product_item) %>% 
    mutate(ingredient_per_minute = ingredient_per_minute - product_per_minute, 
           product_per_minute = NA)
  
  # And add these fixed recipes back to the full recipe list
  Recipe_Edgelist_Clean %<>% 
    filter(!(slug %in% problem_recipes$slug & product_item %in% problem_recipes$product_item)) %>%
    dplyr::bind_rows(problem_recipes)
  
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
  
  
  # Remove any recipes that aren't produced in a building
  Component_Edgelist <- Recipe_Edgelist_Clean %>% 
    filter(!is.na(producedIn))
  
  # Get just the non-alternate recipes
  Component_Edgelist_NoAlternate <- Component_Edgelist %>% 
    filter(!grepl("Alternate", name))
  
  # Return these three dataframes as a list
  res <- list("AllRecipes"     = Recipe_Edgelist_Clean, 
              "ComponentsOnly" = Component_Edgelist, 
              "NoAlternates"   = Component_Edgelist_NoAlternate)
  
  return(res)
}
