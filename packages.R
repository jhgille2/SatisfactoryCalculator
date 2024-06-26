# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages("pacman")
}

# Use pacman to load/install packaged
pacman::p_load(dplyr, 
               stringr,
               tidyr,
               conflicted, 
               dotenv, 
               targets, 
               tarchetypes, 
               jsonlite, 
               RCy3, 
               igraph, 
               magrittr, 
               pbapply, 
               lpSolve, 
               lhs, 
               furrr, 
               job,
               here, 
               purrr, 
               tibble, 
               visNetwork)

conflict_prefer("filter", "dplyr")
conflicts_prefer(purrr::set_names)

