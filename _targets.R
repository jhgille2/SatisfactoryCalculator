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
  
  
  # NEED TO UPDATE THIS PART TO USE UPDATE 5 DATA
  # Download the data from the url
   tar_target(RecipeData,
              get_recipe_data(url = Recipe_url)),

  # Graph representation(s) of the recipe data
   tar_target(RecipeGraphs,
              make_recipe_graphs(recipetables = RecipeData)),
  
  # tar_target(RecipeData, 
  #            load(here("data", "RecipeData.RData"))),
  # 
  ## Section: Factory optimization
  ##################################################
  
  # This just solves a grid of linear programs that is made from requiring
  # different minimum production rates for the products in the lp formulation. 
  # After that, I just calculate some summary stats for each viable solution and
  # choose the one that I like the most, right now this is normally the factory
  # that can archive some large milestone the fastest with a given set of 
  # input resources
  #
  # TODO: Look into global solvers that work well with linear constraints, I think
  # that'd be more appropriate (and efficient) for the kind of problem I'm trying to solve here. 
  # Basically, the problem right now is the optimization is "detached" from my actual 
  # objectives. What is being optimized is the net item production rates  after constraints
  # are put on different minimum net rates of some objective items. Really what I'm 
  # trying to optimize is the time to complete an objective which requires some set of 
  # objective items. As it stands right now, my strategy essentially performs an 
  # (unoptimized) grid search of the (mostly) feasible region of factories, and then I choose
  # the solution which happens to have the fastest time to complete the objective. 
  # It would be good to instead take an approach that optimizes this time
  # in a more efficient and directed manner. 
  
  
  # Set the items to produce (recipe names for the objective function).
  # This can also be improved in the long run by instead getting the names 
  # of recipes from the component names so that alternate recipes can be more
  # easily accomodated. 
  # I think this can be done relatively easily by adjustting the makea_recipematrix function
  # so that it builds the recipe matrix from the product names instead of the recipe names
  # and then the recipes which produce the products can be found with the sign
  # of the coefficient for the product row.
  tar_target(Opt_recipes, 
             c("modular-engine",
               "adaptive-control-unit", 
               "supercomputer")), 
  
  # And component names with maximum desired production rates, used to make a crossed grid of 
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
                                 integerFactories  = FALSE,             # Basically, do you want to underclock the last factory for an item or not ***This will run VERY slowly for complicated production chains if set to TRUE***
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
