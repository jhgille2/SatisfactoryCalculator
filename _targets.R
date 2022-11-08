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
# c("product name" = some number, "another product name" = some number). These can be set to any numbers now, 
# it's a leftover from the old solver that I still need to fix
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
# Finally, in the LP result target, you need to set two arguments in the factory_binary_search
# function: 
#
# 1. reqAmt: I made this calculator to find the factory that would reach some milestone
# the fastest. That is equivalent to finding the factory that produces some set of items
# at a rate relative to one another that is proportional to the amounts of each item that
# is required for the objective. If you are using the script to complete the objective, 
# it makes sense to set this argument equal to the number of each product that is required for the objective, 
# enter these numbers in the same order the product names appear in the Opt_products step. 
#
# 2. max_rate: This controls the maximum rate of production of the product with the SMALLEST REQUIRED AMOUNT that 
# the solver will try to find a solution for. Until very VERY late game you'll be fine setting this to 20 or less. 
# If you do end up with a solution that is maxing out the production without consuming all the resources you have, 
# just increase this


## Section: Targets plan - Begin control script
##################################################

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  
  ## Section: Data cleaning
  ##################################################

  # The url to the recipe data (Greenys github repo)
  tar_target(Recipe_url, 
             "https://raw.githubusercontent.com/greeny/SatisfactoryTools/dev/data/data.json", 
             format = "url"), 
  
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
  
  # And component names with maximum desired production rates, used to make a crossed grid of 
  # minimum production rates for each products to feed into the lp solver.
  # Basically my (bad I feel) solution to optimize the factory relative to the time it
  # takes to complete an objective that uses the products.
  tar_target(Opt_products, 
             c("Desc_SpaceElevatorPart_3_C" = 0, 
               "Desc_SteelPlateReinforced_C" = 0)), 
  
  # Provide available resources (negative values)
  tar_target(available_resources, 
             c("Desc_OreCopper_C" = -240, 
               "Desc_OreIron_C"   = -480, 
               "Desc_Coal_C"      = -240,
               "Desc_Stone_C"     = -480,
               "Desc_LiquidOil_C" = -2200,
               "Desc_RawQuartz_C" = -2200,
               "Desc_Water_C"     = -9007199254740991)),
  
  # Use binary search to try to find a factory that....
  # a) Produces the desired products at rates proportional to the 
  #    amount that are required
  # b) Uses all available resources
  tar_target(binary_LP_result, 
             factory_binary_search(Opt_products, 
                                   current_recipes, 
                                   available_resources, 
                                   req_amt = c(1, 1), 
                                   max_rate = 50)), # Bounds the search space for production rates. 
                                                    # Room for optimization here by just automatically finding the maximum possible production rates directly
  
  # Clean up the output into a format that's ready for plotting in cytoscape
  tar_target(CytoscapeReady_binary,
             clean_binary_lp_results(lp_result   = binary_LP_result, 
                                     recipeData  = current_recipes$data_frame, 
                                     recipeGraph = current_recipes$graph, 
                                     products    = Opt_products)),
  
  ## Section: Targets for testing stuff
  ##################################################
  tar_target(current_recipe_matrix, 
             make_recipeMatrix(names(Opt_products), 
                               current_recipes$graph, 
                               current_recipes$data_frame, 
                               "component"))
)
