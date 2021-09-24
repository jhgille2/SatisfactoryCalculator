tar_load(RecipeData)
tar_load(RecipeGraphs)

crystal_items <- c("Crystal Oscillator", 
                   "Heavy Modular Frame")

crystal_products <- c("Desc_CrystalOscillator_C" = 1, 
                      "Desc_ModularFrameHeavy_C" = 1)

startingResources_crystal = c("Desc_OreCopper_C" = -240, 
                              "Desc_OreIron_C"   = -700, 
                              "Desc_Coal_C"      = -350, 
                              "Desc_Stone_C"     = -240, 
                              "Desc_RawQuartz_C" = -240)

soln_crystal <- recipe_lp_solutions(startingResources = startingResources_crystal, 
                            products          = crystal_products, 
                            product_longnames =  crystal_items, 
                            recipeData        =  RecipeData, 
                            recipeGraph       =  RecipeGraphs)

soln_crystal_int <- recipe_lp_solutions(startingResources = startingResources_crystal, 
                                        products          = crystal_products, 
                                        product_longnames =  crystal_items, 
                                        recipeData        =  RecipeData, 
                                        recipeGraph       =  RecipeGraphs, 
                                        integerFactories  = TRUE)



# Elevator tier 3 
tier3_items <- c("Versatile Framework", 
                 "Modular Engine", 
                 "Adaptive Control Unit")

tier3_products <- c("Desc_SpaceElevatorPart_2_C" = 1, 
                    "Desc_SpaceElevatorPart_4_C" = 1, 
                    "Desc_SpaceElevatorPart_5_C" = 1)

startingResources_tier3 = c("Desc_OreCopper_C"   = -1500, 
                              "Desc_OreIron_C"   = -2400, 
                              "Desc_Coal_C"      = -1000, 
                              "Desc_Stone_C"     = -700, 
                              "Desc_RawQuartz_C" = -240, 
                              "Desc_LiquidOil_C" = -720, 
                              "Desc_Water_C"     = -720)

soln_tier3 <- recipe_lp_solutions(startingResources = startingResources_tier3, 
                                    products          = tier3_products, 
                                    product_longnames =  tier3_items, 
                                    recipeData        =  RecipeData, 
                                    recipeGraph       =  RecipeGraphs)

# Integer factory constraint
soln_tier3_int <- recipe_lp_solutions(startingResources = startingResources_tier3, 
                                  products              = tier3_products, 
                                  product_longnames     =  tier3_items, 
                                  recipeData            =  RecipeData, 
                                  recipeGraph           =  RecipeGraphs, 
                                  integerFactories      = TRUE)

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
                                         integerFactories      = TRUE)
