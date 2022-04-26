# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packaged
pacman::p_load(conflicted, 
               dotenv, 
               targets, 
               tarchetypes, 
               tidyverse, 
               jsonlite, 
               RCy3, 
               igraph, 
               magrittr, 
               pbapply, 
               lpSolve, 
               lhs, 
               furrr, 
               job,
               here)

conflict_prefer("filter", "dplyr")

c("Alternate: Recycled Plastic", 
  "Residual Plastic", 
  "Alternate: Recycled Rubber", 
  "Residual Rubber")