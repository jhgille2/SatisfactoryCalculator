## Section: Setup (don't mess with this part)
##################################################

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)


## Section: INSTRUCTIONS
##################################################
# This script lets you optimize relative production rates of some set of components, 
# given some known availability of raw resources.
#
# I thought that optimization in particular could be useful because it would
# let you design a factory that would reach some milestone production goal
# the fastest.
# 
# I have set it up so that the optimization relatively automated but 
# it is not intuitive what inputs should go where in the script. 
# 
# There are 4 sections which need to be set by the user before running the script. 
#
# First, get the full table of recipes by running tar_make(RecipeData) in the console. 
# Then run tar_load(RecipeData) to load the table into the environment. 
# The full recipe table can then be inspected by running View(RecipeData$AllRecipes). This
# Will bring up a table of all the recipes which will be helpful for finding most of the other
# required inputs.
#
# Next, enter any alternative recipes you've unlocked by replacing c(NULL) in
# the available_alternative_recipes target with a comma seperated character
# vector of the recipe 'slug' names for the unlocked alternative recipes.
# You can find the 'slug' names from the full recipe table. 
#
# Example: if you've unlocked the steel rotor and recycled plastic alternate recipes, 
# you could replace c(NULL) with c("alternate-steel-rotor", "alternate-recycled-plastic")
#
#
# Now, go to the Opt_products target. Here, put in the items you want to produce. 
# These have the be product names, again as they appear in the recipes table. 
# Enter these in the form of a named vector like
# c("product name" = some number, "another product name" = some number). The numbers you set these
# product names to set the upper limit of the rate that the solver will try to find a solution for to produce each item at. 
# 
# Example: If you want to make rotors and reinforced iron plates, you could set this to 
# c("Desc_Rotor_C" = 50, "Desc_IronPlateReinforced_C" = 50) which means that you want the solver to search for factories
# that will produce rotors and reinforced iron plates at some rate between >0 and 50 per minute. 
#
# Next, provide your available resources in the same format as the desired products. 
# make sure each available resource is negative. 
#
# Example: If you have 120 iron ore and 60 copper available, this would be set to  
# c("Desc_OreCopper_C" = -60, "Desc_OreIron_C"   = -120)
#
# Finally, in the LP result target, you need to set three arguments in the recipe_lp_rate_grid
# function: 
# 
# 1. integerFactories: Do you want the solution to have only full number factories? In practice
# this will likely find a solution that produces some intermediate components in excess but could
# also be desirable for the early game where you could want to store intermediate components. 
#
# 2. reqAmt: I made this calculator to find the factory that would reach some milestone
# the fastest. That is equivalent to finding the factory that produces some set of items
# at a rate relative to one another that is proportional to the amounts of each item that
# is required for the objective. If you are using the script to complete the objective, 
# it makes sense to set this argument equal to the number of each product that is required for the objective, 
# enter these numbers in the same order the product names appear in the Opt_products step. 
#
# 3. gridsize: This controls the size of the grid that the solver will search
# for a factory. Keep relatively small for factories that will be producing lots of different products.



## Section: Targets plan - Begin control script
##################################################

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
  
  # The names of the alternative recipes you've unlocked
  # Set to c(NULL) if none have been unlocked
  tar_target(available_alternate_recipes, 
             c(NULL)),
  
  # Make a list holding the current recipe table and its associated graph
  # from the alternate recipes that have been unlocked
  tar_target(current_recipes, 
             make_current_recipes(RecipeData, available_alternate_recipes)),
  

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
  # objectives. What is being optimized is the net item production rates relative to one another after constraints
  # are put on different minimum net rates of some objective items. Really what I'm 
  # trying to optimize is the time to complete an objective which requires some set of 
  # objective items. As it stands right now, my strategy essentially performs an 
  # (unoptimized) grid search of the (mostly) feasible region of factories, and then I choose
  # the solution which happens to have the fastest time to complete the objective. 
  # It would be good to instead take an approach that optimizes this time
  # in a more efficient and directed manner. 
  
  
  # And component names with maximum desired production rates, used to make a crossed grid of 
  # minimum production rates for each products to feed into the lp solver.
  # Basically my (bad I feel) solution to optimize the factory relative to the time it
  # takes to complete an objective that uses the products.
  tar_target(Opt_products, 
             c("Desc_SpaceElevatorPart_2_C" = 10, 
               "Desc_SpaceElevatorPart_4_C" = 10,
               "Desc_SpaceElevatorPart_5_C" = 10)), 
  
  # Provide available resources (negative values)
  tar_target(available_resources, 
             c("Desc_OreCopper_C" = -1200, 
               "Desc_OreIron_C"   = -1200, 
               "Desc_Coal_C"      = -1200,
               "Desc_Stone_C"     = -1200,
               "Desc_LiquidOil_C" = -1200,
               "Desc_RawQuartz_C" = -1200,
               "Desc_Water_C"     = -9007199254740991)),
  
  # Run the solver for the given items
  # tar_target(LP_result, 
  #            recipe_lp_rate_grid(startingResources = available_resources,
  #                                products          = Opt_products, 
  #                                recipeData        = current_recipes$data_frame, 
  #                                recipeGraph       = current_recipes$graph, 
  #                                integerFactories  = FALSE,                  # Basically, do you want to underclock the last factory for an item or not ***This will run VERY slowly for complicated production chains if set to TRUE***
  #                                reqAmt            = c(2500, 500, 100),      # How many of each product are required for the objective
  #                                gridsize          = 10)),                   # How many production rate minima to give to the solver...important to 
  # remember that the solver will run gridsize^length(Opt_products) times
  # so keep this number small if you have a lot of products.
  
  # IN DEVELOPMENT(?)
  # Sparse grid search followed by a local constrained optimization of the best
  # result of the grid search. Seems to be a good improvement so far over
  # the pure grid search. Especially when factories get big
  #
  # So far, this seems like an improvement across the board. Very good improvement
  # on sparse grid searches and equivalent to very dense grid searches so far. 
  # I'm still gonna leave the grid search
  # target there for now but this does seem to be working
  tar_target(constrained_LP_result, 
             recipe_constr_optim(Opt_products, 
                                 current_recipes, 
                                 available_resources, 
                                 req_amt = c(2500, 500, 100))),
  
  # Clean up the output into a format that's ready for plotting in cytoscape
  #
  # TODO: It would be nice to have a way of converting the rates of materials
  # required by a recipe into the number of factories, not really sure if I can do this,
  # or even if I should since the same material can be made by multiple recipes and 
  # it'll only get more confusing with alternate recipes in the mix. 
  # Maybe something with the nodes (individual recipes) instead?
  # tar_target(CytoscapeReady, 
  #            clean_lp_results(lp_table    = LP_result, 
  #                             recipeData  = current_recipes$data_frame, 
  #                             recipeGraph = current_recipes$graph, 
  #                             products    = Opt_products)),
  # 
  # Same thing but with the constrained optimization result
  tar_target(CytoscapeReady_constrained, 
             clean_constrained_lp_results(lp_result   = constrained_LP_result, 
                                          recipeData  = current_recipes$data_frame, 
                                          recipeGraph = current_recipes$graph, 
                                          products    = Opt_products))
)
