##################################################
## Project: Satisfactory calculator
## Script purpose: Tools to extract recipe data directly from Docs.json
## from the CommunityResources directory
## Date: 2024-09-08
## Author: Jay Gillenwater
##################################################

# Laod packages
source("./packages.R")

tar_load(RecipeData)


# Read in docs.json
docs_json <- jsonlite::fromJSON(readLines(here::here("data", "recipe_jsons", "Docs_utf8.json"),
                                               skipNul = T,
                                               encoding = "UTF-16LE"))

# Get the recipe data
recipe_df <- docs_json$Classes[[6]]


# Need a function to parse the ingredients/products content
recipes <- fromJSON(tar_read(Recipe_url), flatten = TRUE)


ingredient_df <- recipe_df %>% 
  select(ClassName, FullName, mDisplayName, mIngredients)


test_ingredients <- ingredient_df$mIngredients[[1]] %>% str_split(",")
test_ingredients <- test_ingredients[[1]]


# A function to remove any parentheses from a string
remove_all_parentheses <- function(string){
  string %>% 
    str_remove_all("\\(") %>%
    str_remove_all("\\)")
}

ingrediends_chr_vec <- map_chr(test_ingredients, remove_all_parentheses)

ingr_example_string <- ingrediends_chr_vec[[1]]
amt_example_string  <- ingrediends_chr_vec[[2]]

# To parse ingredient string, split by backslash, then by "."
parse_odd <- function(ingredient_string){
  
  ingredient_split <- ingredient_string %>%
    str_split(., "'") %>%
    unlist() %>%
    map_chr(., basename)
  
  ingredient_name <- ingredient_split[[2]] %>% 
    str_sub(1, str_length(.) - 1) %>% 
    str_split(., "\\.") %>% 
    unlist()
  
  return(ingredient_name[[2]])
}

parse_even <- function(ingredient_string){
  
  str_remove(ingredient_string, "Amount=") %>% 
    as.numeric()
  
}

# A function to combine the functions above to return a zipped dataframe of 
# ingredient names and amountsingredie
zip_ingredients <- function(ingredient_chr_vec){
  
  ingredient_chr_vec %<>%
    unlist() %>%
    str_split(",") %>%
    unlist() %>%
    map_chr(remove_all_parentheses)
  
  # Get the vectors for the ingredient names and amounts
  ingredient_name_vec <- ingredient_chr_vec[seq(1, length(ingredient_chr_vec), 2)]
  ingredient_amt_vec  <- ingredient_chr_vec[seq(2, length(ingredient_chr_vec), 2)]
  
  
  names_clean <- map_chr(ingredient_name_vec, parse_odd)
  amt_clean   <- map_dbl(ingredient_amt_vec, parse_even)
  
  ingredient_tbl <- tibble(ingredient_item   = names_clean,
                           ingredient_amount = amt_clean)
  
  return(ingredient_tbl)
}

# Same for the products, just apply the ingredient parsing function and then rename
zip_products <- function(product_chr_vec){
  
  zip_ingredients(product_chr_vec) %>%
    fill(ingredient_amount) %>%
    rename(product_amount = ingredient_amount,
           product_item = ingredient_item) %>% 
    distinct()
  
}

test3 <- recipe_df %>% filter(mDisplayName == "AI Limiter") %>% select(mProducedIn) %>% as.character()

parse_producedIn <- function(producedin_string){
  
  if(str_length(producedin_string) == 0){
    return(NA)
  }
  
  producedin_string %>% 
    sub("^[^.]*\\.", "",.) %>% 
    str_extract("_(.*?)_C") %>% 
    str_remove("^_")
  
}


recipe_df_clean <- recipe_df %>%
  mutate(ingredient_df = map(mIngredients, zip_ingredients),
         product_df    = map(mProduct, zip_products),
         producedIn = map_chr(mProducedIn, parse_producedIn),
         nProducts  = map_dbl(product_df, nrow)) %>% 
  select(ClassName, mDisplayName, mManufactoringDuration, ingredient_df, product_df, producedIn, nProducts)

ingredients_unnested <- recipe_df_clean %>% 
  select(-product_df) %>% 
  unnest(ingredient_df) %>% 
  mutate(ingredient_per_minute = ingredient_amount * (60/as.numeric(mManufactoringDuration)))

products_unnested <- recipe_df_clean %>%
  select(-ingredient_df) %>% 
  unnest(product_df) %>%
  mutate(product_per_minute = product_amount * (60/as.numeric(mManufactoringDuration))) %>% 
  select(ClassName, mDisplayName, product_amount, product_per_minute, product_item)


Recipe_Edgelist_Clean <- full_join(ingredients_unnested,
                            products_unnested,
                            by = c("ClassName", 
                                   "mDisplayName")) %>% 
  mutate(slug = tolower(mDisplayName) %>% str_replace_all(" ", "-")) %>% 
  rename(name = mDisplayName) %>% 
  relocate(slug, .after = name)

# Identify recipes that produce an item they consume
# These mess with the LP solver
# Replace the explicit rates with the net rate of each item
problem_recipes <- Recipe_Edgelist_Clean %>% 
  filter(ingredient_item == product_item)

fixed_recipes <-  Recipe_Edgelist_Clean %>% 
  filter(slug %in% problem_recipes$slug, !(product_item %in% problem_recipes$product_item))

new_rates <- problem_recipes %>% 
  mutate(ingredient_per_minute = ingredient_per_minute - product_per_minute) %>% 
  select(ingredient_item, slug, ingredient_per_minute) %>% 
  rename(new_ingredient_rate = ingredient_per_minute)

fixed_recipes <- left_join(fixed_recipes, new_rates, by = c("ingredient_item", "slug")) %>% 
  mutate(ingredient_per_minute = ifelse(!is.na(new_ingredient_rate), new_ingredient_rate, ingredient_per_minute)) %>%
  select(-new_ingredient_rate)

# And add these fixed recipes back to the full recipe list
Recipe_Edgelist_Clean %<>% 
  filter(!(slug %in% problem_recipes$slug)) %>%
  dplyr::bind_rows(fixed_recipes)
                            