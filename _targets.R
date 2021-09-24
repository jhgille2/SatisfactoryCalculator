## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  ## Section: Data cleaning
  ##################################################

  # The url to recipe data (someones github repo)
  tar_target(Recipe_url, 
             "https://raw.githubusercontent.com/greeny/SatisfactoryTools/dev/data/data.json", 
             format = "url"), 
  
  # Download the data from the url
  tar_target(RecipeData, 
             get_recipe_data(url = Recipe_url)),
  
  # Graph representation(s) of the recipe data
  tar_target(RecipeGraphs, 
             make_recipe_graphs(recipetables = RecipeData)), 
  
  ## Section: Factory optimization
  ##################################################
  
  # Set the items to produce (recipe names for the objective function)
  tar_target(Opt_recipes, 
             c("computer", 
               "heavy-modular-frame")), 
  
  # And ingredient names with (optional) minimum desired production rates, defaults to 0.
  tar_target(Opt_products, 
             c("Desc_Computer_C"          = 1, 
               "Desc_ModularFrameHeavy_C" = 1)), 
  
  # Provide available resources (negative values)
  tar_target(available_resources, 
             c("Desc_OreCopper_C" = -1500, 
               "Desc_OreIron_C"   = -2400, 
               "Desc_Coal_C"      = -1000, 
               "Desc_Stone_C"     = -700, 
               "Desc_RawQuartz_C" = -240, 
               "Desc_LiquidOil_C" = -720, 
               "Desc_Water_C"     = -720)),
  
  # Run the solver for the given items
  tar_target(LP_result, 
             recipe_lp_solutions(startingResources = available_resources, 
                                 products          = Opt_products, 
                                 product_longnames = Opt_recipes, 
                                 recipeData        = RecipeData$NoAlternates, 
                                 recipeGraph       = RecipeGraphs$NoAlternates, 
                                 integerFactories  = TRUE)), 
  
  # Clean up the output into a format that's ready for plotting in cytoscape
  #
  # TODO: Fix this function. The join isn't right. Basically, the names of items
  # need to match their role in a recipt. An example right now is that
  # plastic is getting renamed to residual plastic and because of this, 
  # connections are showing up in the final graph that aren't actually returned
  # by the lp solution. I think a solution could be to use some sort of more 
  # complicated join that makes use of both ingredient names and recipe id's
  # to properly identify and translate names.
  #
  tar_target(CytoscapeReady, 
             clean_lp_results(lp_table    = LP_result, 
                              recipeData  = RecipeData$NoAlternates, 
                              recipeGraph = RecipeGraphs$NoAlternates, 
                              products    = Opt_recipes))
)
