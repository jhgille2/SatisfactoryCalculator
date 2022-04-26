tar_load(RecipeData)
tar_load(RecipeGraphs)

crystal_items <- c("versatile-framework")

crystal_products <- c("Desc_SpaceElevatorPart_2_C" = 1)

startingResources_crystal = c("Desc_OreIron_C"   = -240, 
                              "Desc_Coal_C"      = -120)

soln_crystal <- recipe_lp_solutions(startingResources = startingResources_crystal, 
                            products          = crystal_products, 
                            product_longnames =  crystal_items, 
                            recipeData        =  RecipeData, 
                            recipeGraph       =  RecipeGraphs)

soln_crystal_int <- recipe_lp_solutions(startingResources = startingResources_crystal, 
                                        products          = crystal_products, 
                                        product_longnames =  crystal_items, 
                                        recipeData        =  RecipeData$NoAlternates, 
                                        recipeGraph       =  RecipeGraphs$NoAlternates, 
                                        integerFactories  = TRUE, 
                                        gridsize          = 50)



# Elevator tier 3 - removed versatile framework since I had leftover production from the previous space elevator milestone
tier3_items <- c("versatile-framework")

tier3_products <- c("Desc_SpaceElevatorPart_2_C" = 20)

startingResources_tier3 = c("Desc_OreIron_C"   = -360, 
                            "Desc_Coal_C"      = -180)


soln_tier3_rategrid <- recipe_lp_rate_grid(startingResources = startingResources_tier3,
                                           products          = tier3_products, 
                                           product_longnames = tier3_items, 
                                           recipeData        = RecipeData$NoAlternates, 
                                           recipeGraph       = RecipeGraphs$NoAlternates, 
                                           integerFactories  = FALSE, 
                                           reqAmt            = 500, 
                                           gridsize          = 100)

tier_3_cytoscape <- clean_lp_results(lp_table    = soln_tier3_rategrid, 
                                     recipeData  = RecipeData$NoAlternates, 
                                     recipeGraph = RecipeGraphs$NoAlternates, 
                                     products    = tier3_items)

createNetworkFromIgraph(tier_3_cytoscape)

soln# Use the rate grid function to "optimize" rate to objective
tier3_products_byrate <- c("Desc_SpaceElevatorPart_4_C" = 50,
                           "Desc_SpaceElevatorPart_5_C" = 50)

soln_tier3_rate_objective <- recipe_lp_rate_grid(startingResources  = startingResources_tier3, 
                                                  products          = tier3_products_byrate, 
                                                  product_longnames = tier3_items, 
                                                  recipeData        = RecipeData$NoAlternates, 
                                                  recipeGraph       = RecipeGraphs$NoAlternates, 
                                                  integerFactories  = FALSE, 
                                                  reqAmt            = c(500, 100))

tier_3_cleanedResult <- clean_lp_results(soln_tier3_rate_objective, products = tier3_items)

# Computer
computer_items    <- c("computer", 
                       "heavy-modular-frame")

computer_products <- c("Desc_Computer_C", 
                       "Desc_ModularFrameHeavy_C")

soln_computer_int <- recipe_lp_solutions(startingResources     = startingResources_tier3, 
                                         products              = computer_products, 
                                         product_longnames     = computer_items, 
                                         recipeData            = RecipeData$NoAlternates, 
                                         recipeGraph           = RecipeGraphs$NoAlternates, 
                                         integerFactories      = TRUE, 
                                         gridsize              = 20)

# Elevator phase 2
phase2_items <- c("smart-plating", 
                  "versatile-framework", 
                  "automated-wiring")

phase2_products <- c("Desc_SpaceElevatorPart_1_C" = 5, 
                     "Desc_SpaceElevatorPart_2_C" = 5,
                     "Desc_SpaceElevatorPart_3_C" = 1) 

startingResources_phase2 = c("Desc_OreCopper_C" = -1000, 
                            "Desc_OreIron_C"   = -1000, 
                            "Desc_Coal_C"      = -720, 
                            "Desc_Stone_C"     = -720, 
                            "Desc_RawQuartz_C" = -480, 
                            "Desc_LiquidOil_C" = -720, 
                            "Desc_Water_C"     = -2000)

# Integer factories
soln_phase2 <- recipe_lp_solutions(startingResources     = startingResources_phase2, 
                                         products              = phase2_products, 
                                         product_longnames     = phase2_items, 
                                         recipeData            = RecipeData$NoAlternates, 
                                         recipeGraph           = RecipeGraphs$NoAlternates, 
                                         integerFactories      = TRUE, 
                                         gridsize              = 1000, 
                                   reqAmt = c(500, 500, 100))

soln_phase2_parallel <- recipe_lp_solutions_parallel(startingResources     = startingResources_phase2, 
                                                     products              = phase2_products, 
                                                     product_longnames     = phase2_items, 
                                                     recipeData            = RecipeData$NoAlternates, 
                                                     recipeGraph           = RecipeGraphs$NoAlternates, 
                                                     integerFactories      = TRUE, 
                                                     gridsize              = 100000, 
                                                     reqAmt                = c(500, 500, 100))

recipe_lp_base(startingResources     = startingResources_phase2, 
                         products              = phase2_products, 
                         product_longnames     = phase2_items, 
                         recipeData            = RecipeData$NoAlternates, 
                         recipeGraph           = RecipeGraphs$NoAlternates, 
                         integerFactories      = TRUE)
