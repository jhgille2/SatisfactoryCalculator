# Need a function to 

auto_wiring_matrix <- matrix(c(-15, 0, 0, 0, 0, 
                               -40, 0, -60, 30, 0, 
                               5, -2.5, 0, 0, 0, 
                               0, 2.5, 0, 0, 0, 
                               0, -50, 30, 0, 0, 
                               0, 0, 0, -15, 1, 
                               0, 0, 0, 0, -1),
                             byrow = TRUE, ncol = 5, nrow = 7)

colnames(auto_wiring_matrix) <- c("stator", 
                                  "automated wiring", 
                                  "cable", 
                                  "wire", 
                                  "copper ingot")

rownames(auto_wiring_matrix) <- c("steel pipe",  
                                  "wire", 
                                  "stator", 
                                  "automated wiring", 
                                  "cable", 
                                  "copper ingot", 
                                  "copper ore")

objective <- c(0, 1, 0, 0, 0)

constraints_rhs <- c(-80, 
                     0, 
                     0, 
                     0, 
                     0, 
                     0, 
                     -120)

const_direction <- c(">=", 
                     rep(">=", 5), 
                     ">=")

test <- lp(direction = "max", 
           objective.in = objective, 
           const.mat = auto_wiring_matrix, 
           const.dir = const_direction, 
           const.rhs = constraints_rhs)


oil_products_matrix <- matrix(c(-30, 0, 0, -30, 0, 0, -60, 0, 0, 
                                0, -60, 0, 0, -40, 0, 30, 0, 0, 
                                0, -20, 0, 0, -40, 0, 0, 0, -100, 
                                0, 0, -30, 20, 20, 60, 0, 0, 0,
                                0, 0, -30, 0, 0, -30, 40, 40, 100, 
                                20, 20, 60, 0, 0, -30, 0, 0, 0, 
                                10, 0, 0, 20, 0, 0, 0, -60, -50),
                              byrow = TRUE, nrow = 7, ncol = 9)

colnames(oil_products_matrix) <- c("Plastic", "Residual Plastic", "Recycled Plastic", "Rubber", 
                                   "Residual Rubber", "Recycled Rubber", 
                                   "Fuel", "Residual Fuel", "Diluted Fuel")

rownames(oil_products_matrix) <- c("Crude Oil", "Polymer Resin", "Water", "Rubber", 
                                   "Fuel", "Plastic", "Heavy Oil Residue")

const_rhs_oil <- c(-2100, 
               0, 
               -500000, 
               0, 
               0, 
               0, 
               0)

obj_oil <- c(1, 
         1, 
         1, 
         1, 
         1, 
         1, 
         1, 
         1, 
         1)

const_direction_oil <- rep(">=", 7)

oil_lp <- lp(direction = "max", 
             objective.in = obj_oil, 
             const.mat = oil_products_matrix, 
             const.dir = const_direction_oil, 
             const.rhs = const_rhs_oil)
