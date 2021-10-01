## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  ## Section: Data cleaning
  ##################################################

  # The url to the recipe data (someones github repo)
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
             c("modular-engine",
               "adaptive-control-unit", 
               "supercomputer")), 
  
  # And ingredient names with maximum desired production rates, used to make a crossed grid of 
  # minimum production rates for each products to feed into the lp solver.
  # Basically my (bad I feel) solution to optimize the factory relative to the time it
  # takes to complete an objective that uses the products.
  tar_target(Opt_products, 
             c("Desc_SpaceElevatorPart_4_C" = 50,
               "Desc_SpaceElevatorPart_5_C" = 50, 
               "Desc_ComputerSuper_C"       = 50)), 
  
  # Provide available resources (negative values)
  tar_target(available_resources, 
             c("Desc_OreCopper_C" = -3450, 
               "Desc_OreIron_C"   = -7354, 
               "Desc_Coal_C"      = -1560, 
               "Desc_Stone_C"     = -2640, 
               "Desc_RawQuartz_C" = -480, 
               "Desc_LiquidOil_C" = -1740, 
               "Desc_Water_C"     = -5000, 
               "Desc_OreGold_C"   = -720)),
  
  # Run the solver for the given items
  tar_target(LP_result, 
             recipe_lp_rate_grid(startingResources = available_resources,
                                 products          = Opt_products, 
                                 product_longnames = Opt_recipes, 
                                 recipeData        = RecipeData$NoAlternates, 
                                 recipeGraph       = RecipeGraphs$NoAlternates, 
                                 integerFactories  = FALSE,             # Basically, do you want to underclock the last factory for an item or not
                                 reqAmt            = c(500, 100, 100),  # How many of each product are required for the objective
                                 gridsize          = 50)),              # How many production rate minima to give to the solver...important to 
                                                                        # remember that the solver will run gridsize^length(Opt_products) times
                                                                        # so keep this number small if you have a lot of products.
  
  # Clean up the output into a format that's ready for plotting in cytoscape
  #
  # TODO: It would be nice to have a way of converting the rates of materials
  # required by a recipe into the number of factories, not really sure if I can do this,
  # or even if I should since the same material can be made by multiple recipes and 
  # it'll only get more confusing with alternate recipes in the mix. 
  # Maybe something with the nodes (individual recipes) instead?
  tar_target(CytoscapeReady, 
             clean_lp_results(lp_table    = LP_result, 
                              recipeData  = RecipeData$NoAlternates, 
                              recipeGraph = RecipeGraphs$NoAlternates, 
                              products    = Opt_recipes))
)
