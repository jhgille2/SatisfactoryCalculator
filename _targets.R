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
# I chose this as a goal because the production planners I knew of would let you design
# a factory that would produce components at some desired rate, but most of the time I don't really 
# have a good idea about what rate would be "best". What I wanted to do instead was to have a calculator
# that could look at a set of available resources, and an amount of components that I wanted to make,
# and then spit out a design for a factory that would use up all the resources and also make the components
# I wanted at rates that were proportional to how much of each I needed (e.g for some milestone). This
# to me seemed to be a satisfactory (ha) way of deciding on a "best" rate that (at the time of writing)
# wasn't implemented in existing calculators.
# 
# I have set it up so that the optimization is relatively automated but 
# it is still not necessarily intuitive what inputs should go where in the script. 
# 
# There are 4 sections which need to be set by the user before running the script. 
#
# First, get the full table of recipes by running tar_make(RecipeData) in the console. 
# Then run tar_load(RecipeData) to load the table into the environment. 
# The full recipe table can then be inspected by running View(RecipeData$AllRecipes). This
# will bring up a table of all the recipes which will be helpful for finding most of the other
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
# it's a leftover from the old solver that I still need to fix but I'm lazy.
# 
# Example: If you want to make rotors and reinforced iron plates, you could set this to 
# c("Desc_Rotor_C" = 50, "Desc_IronPlateReinforced_C" = 50) 
#
# Next, provide your available resources in the same format as the desired products. 
# MAKE SURE EACH AVAILABLE RESOURCE IS NEGATIVE
#
# Example: If you have 120 iron ore and 60 copper available, this would be set to  
# c("Desc_OreCopper_C" = -60, "Desc_OreIron_C"   = -120)
#
# Finally, in the LP result target, you need to set two arguments in the factory_binary_search
# function: 
#
# 1. reqAmt: I made this calculator to find the factory that produces some set of items
# at a rate relative to one another that is proportional to the amounts of each item that
# is required for the objective. If you are using the script to complete an objective, 
# it makes sense to set this argument equal to the number of each product that is required for the objective.
# Otherwise, you can enter numbers so that they reflect how much more of some component you want relative to the others.
# Enter these numbers in the same order the product names appear in the Opt_products step. 
#
# 2. max_rate: This controls the maximum rate of production of the product with the SMALLEST REQUIRED AMOUNT that 
# the solver will try to find a solution for. Until very VERY late game you'll be fine setting this to 20 or less. 
# If you do end up with a solution that is maxing out the production without consuming all the resources you have, 
# just increase this. You can also set it to some very large number and you should be fine.


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
  
  # Component names to produce (set each equal to 0), 
  tar_target(Opt_products, 
             c("Desc_SpaceElevatorPart_1_C" = 0,
               "Desc_SpaceElevatorPart_2_C" = 0,
               "Desc_SpaceElevatorPart_3_C" = 0)), 
  
  # Provide available resources (negative values)
  tar_target(available_resources, 
             c(#"Desc_OreBauxite_C" = -10000, 
               "Desc_Coal_C"       = -660,
               #"Desc_RawQuartz_C"  = -10000,
               "Desc_OreIron_C"    = -720, 
               "Desc_OreCopper_C"  = -210,
               #"Desc_OreGold_C"    = -10000,
               "Desc_Stone_C"      = -240,
               #"Desc_Sulfur_C"     = -10000,
               #"Desc_OreUranium_C" = -300,
               "Desc_Water_C"      = -9007199254740991)),
  
  # Use binary search to try to find a factory that....
  # a) Produces the desired products at rates proportional to the 
  #    amount that are required
  # b) Uses all available resources
  tar_target(binary_LP_result, 
             factory_binary_search_continuous(Opt_products,                     # DON'T CHANGE THIS
                                              current_recipes,                  # DON'T CHANGE THIS
                                              available_resources,              # DON'T CHANGE THIS
                                              req_amt = c(500, 500, 100),       # How much of each product is required. Same order as Opt_products list
                                              max_rate = 50000,                 # Starting upper bound for the the search space of production rates
                                              whole_number_factories = FALSE)), # Do you want a solution with integer factories (do you not want to worry about under clocking factories)
                                                                                # Likely, you will produce extra intermediate components if this is set to TRUE
  
  # Clean up the output into a format that's ready for plotting in cytoscape
  # To open in cytoscape: 
  # 1. Start cytoscape
  # 2. Test connection by running: 'RCy3::cytoscapePing()' in the console
  # 3. Run 'RCy3::createNetworkFromIgraph(tar_read(CytoscapeReady_binary))'
  #    in the console to open the network in cytoscape and customize.
  #
  # Network notes for future me: 
  # The yFiles orthogonal edge router layout
  # does a good job of placing/clustering nodes in a layout that makes sense
  # for organizing factories (minimizes edge overlaps).
  #
  # Use the "Curved" style. This should work right away but if not...
  # * edge labels from the "flow" column
  # * Node fill color from "producedIn", Mapping type: discrete, use whatever
  #   color assignment you like
  # * Node name from "name" col (duh)
  # * Node size from "nFactories" w/continuous mapping. Adjust till it looks nice
  # * Adding in a legend w/the extension doesn't hurt
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
