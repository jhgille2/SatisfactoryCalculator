#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param CytoscapeReady_binary
#' @param current_recipes
#' @param Opt_products
#' @return
#' @author Jay Gillenwater
#' @export
tidy_factory_results <- function(CytoscapeReady_binary, current_recipes,
                                 Opt_products) {

  
  # Convert to dataframe
  solution_df <- igraph::as_data_frame(CytoscapeReady_binary)
  
  node_df <- igraph::as_data_frame(CytoscapeReady_binary,
                                   what = "vertices") %>% 
    select(name, producedIn)
  
  
  # The current recipes
  recipe_buildings <- current_recipes %>%
    purrr::pluck("data_frame") %>%
    select(product_item, slug, producedIn) %>%
    distinct()
  
  
  factory_df <- solution_df %>%
    select(-productionrate) %>% 
    uncount(nFactories) %>% 
    group_by(from, to, recipe) %>% 
    mutate(factory_id = 1:n(),
           last_factory = ifelse(factory_id == max(factory_id), T, F)) %>% 
    ungroup() %>%
    left_join(recipe_buildings, by = c("recipe" = "slug")) %>%
    select(from, to, recipe, factory_id, product_per_minute, product_amt, last_factory, producedIn, last_factory) %>% 
    group_by(recipe) %>% 
    distinct() %>%
    mutate(product_sum = cumsum(product_per_minute),
           lag_sum = dplyr::lag(product_sum, 1),
           underclock_pct = ifelse(last_factory, (product_amt - lag_sum)/product_per_minute, 1)) %>%
    select(from, to, recipe, factory_id, product_per_minute, product_amt, underclock_pct, producedIn, last_factory) %>% 
    ungroup() %>% 
    mutate(building_name = map2_chr(recipe, factory_id, function(x, y) paste(x, y, sep = "_")))
  
  # Need to allocate factories to intermediates.
  # Two main problems: 
  # 1. Multiple recipes can make the same intermediate. 
  # 2. The number of factories to make for an intermediate (according to
  #    how I like to build factories) is not dictated by the quantity
  #    produced by each recipe, but rather by how much of that intermediate
  #    is consumed by downstream recipes.
  #
  # For what I want to do, I'll need to "tag" the factories that produce
  # some intermediate with the recipe that will eventually consume that 
  # intermediate. 
  # One option could be to allocate the same distribution of factories to each
  # recipe 
  
  # A function to do the steps above
  make_factory_df <- function(solution_df, current_recipes){
    
    production_buildings <- current_recipes %>%
      purrr::pluck("data_frame") %>%
      select(slug, producedIn) %>%
      distinct()
    
    solution_df %<>%
      left_join(production_buildings, by = c("recipe" = "slug")) %>% 
      distinct()
    
    # List to hold factory dataframes for all intermediates
    all_factories <- vector("list", length = nrow(solution_df))
    
    for(i in 1:nrow(solution_df)){
      
      # Look at the current item that is consumed in the solution
      item_df <- solution_df[i, ]
      
      # What recipes in the solution make the item that is being consumed here?
      current_item <- item_df$from[[1]]
      
      
      # Handle ores separately
      if(str_detect(current_item, paste0(c("Ore", "LiquidOil", "Desc_Coal_C", "Desc_NitrogenGas_C", "Desc_Stone_C", "Desc_RawQuartz_C", 'Desc_Sulfur_C'), collapse = "|"))){
        item_recipe <- tibble(recipe = paste("Raw resource -", current_item),
                              product_per_minute = item_df$flow,
                              producedIn = "source_node") # This may cause issues if SAM ore is used to convert
      }else{
        item_recipe <- solution_df %>%
          dplyr::filter(to == current_item) %>% 
          select(recipe, product_per_minute, producedIn) %>% 
          distinct()
      }
      
      # A list to hold the results for each recipe
      recipe_factories <- vector("list", length = nrow(item_recipe))
      
      for(j in 1:nrow(item_recipe)){
        
        # The number of factories I need to make the item for this particular 
        # consumption route is equal to first_item$flow/first_item_recipe$product_per_minute
        # I can calculate underclocking for this setup like I did before
        # FUTURE: Maybe wrap this in a loop to account for alternate recipes?
        # Will need to include a step to "scale" factory numbers proportional
        # to how their recipes are used in the solution
        num_factories           <- item_df$flow/item_recipe$product_per_minute[[j]]
        total_factories         <- ceiling(num_factories)
        underclock_last_factory <- num_factories - floor(num_factories)
        
        first_factory_vec <- if(total_factories == 1){T}else{c(T, rep(F, total_factories-1))}
        last_factory_vec  <- if(total_factories == 1){T}else{c(rep(F, total_factories-1), T)} 
        
        # Vector to say how to underclock factories
        if(underclock_last_factory > 0){
          underclock_vec <- c(rep(1, floor(num_factories)), underclock_last_factory)
        }else{
          underclock_vec <- rep(1, num_factories)
        }
        
        
        # Make a dataframe for this set of factories
        item_factories <- tibble(factory_name = paste(item_recipe$recipe[[j]], "to", item_df$recipe, sep = "_"),
                                 nFactories   = total_factories,
                                 factory_type = item_recipe$producedIn[[j]]) %>% 
          uncount(nFactories) %>% 
          mutate(factory_name     = paste(factory_name, 1:n(), sep = "_"),
                 factory_recipe   = item_recipe$recipe[[j]],
                 factory_target   = item_df$recipe,
                 product_name     = item_df$from[[1]],
                 underclock_value = underclock_vec,
                 product_qty      = item_recipe$product_per_minute[[j]]*underclock_value,
                 splitter_node    = paste("splitter", factory_name, sep = "_"),
                 merger_node      = paste("merger", factory_name, sep = "_"),
                 factory_group    = paste(i, j, sep = "_"),
                 is_last_factory  = last_factory_vec,
                 is_first_factory = first_factory_vec)
        
        recipe_factories[[j]] <- item_factories
      }
      
      all_recipe_factories <- reduce(recipe_factories, bind_rows)
      
      all_factories[[i]] <- all_recipe_factories
      
    }
    
    # Handle products separately
    final_product_df <- solution_df %>% 
      dplyr::filter(to %in% names(Opt_products)) %>% 
      select(recipe, product_amt, product_per_minute, to, producedIn) %>%
      distinct() %>% 
      group_by(recipe) %>% 
      mutate(num_factories           = product_amt/product_per_minute,
             total_factories         = ceiling(num_factories),
             underclock_last_factory = num_factories - floor(num_factories),
             underclock_vec          = list(c(rep(1, floor(num_factories)), underclock_last_factory)),
             factory_recipe          = recipe,
             factory_target          = "final_product",
             is_first_factory = if(total_factories == 1){list(c(T))}else{list(c(T, rep(F, total_factories-1)))}, 
             is_last_factory = if(total_factories == 1){list(c(T))}else{list(c(rep(F, total_factories-1), T))}) %>% 
      unnest(c(underclock_vec, is_first_factory, is_last_factory)) %>% 
      mutate(factory_name = paste(recipe, 1:n(), sep = "_")) %>% 
      select(-recipe) %>%
      relocate(c("factory_name", "factory_recipe")) %>% 
      group_by(factory_recipe) %>%
      rename(product_name     = to,
             underclock_value = underclock_vec,
             factory_type     = producedIn) %>% 
      mutate(splitter_node    = paste("splitter", factory_name, sep = "_"),
             merger_node      = paste("merger", factory_name, sep = "_"),
             product_qty      = product_per_minute * underclock_value) %>%
      select(factory_name, 
             factory_type,
             factory_recipe, 
             factory_target, 
             product_name, 
             underclock_value, 
             product_qty, 
             splitter_node, 
             merger_node, 
             is_last_factory, 
             is_first_factory) %>%
      group_by(factory_recipe) %>% 
      mutate(factory_group = paste(cur_group_id(), "P", sep = "_"),
             splitter_lag  = dplyr::lag(splitter_node, 1),
             merger_lag    = dplyr::lag(merger_node, 1))
    
    # Bind rows of all dataframes
    all_factories %>%
      reduce(bind_rows) %>% 
      group_by(factory_target, factory_group) %>%
      mutate(splitter_lag = dplyr::lag(splitter_node, 1),
             merger_lag   = dplyr::lag(merger_node, 1)) %>% 
      ungroup() %>%
      bind_rows(final_product_df)
  }
  
  solution_factories <- make_factory_df(solution_df, current_recipes)
  
  factory_counts <- solution_factories %>%
    mutate(short_name = paste(factory_recipe, "to", factory_target, sep = "_")) %>% 
    select(short_name,
           factory_group,
           underclock_value) %>%
    group_by(factory_group) %>%
    summarise(factory_count = sum(underclock_value))
  
  just_factories <- solution_factories %>% 
    select(factory_name, factory_recipe, factory_group, is_last_factory, is_first_factory)
  
  just_recipes <- solution_factories %>% 
    select(factory_recipe, factory_target) %>% 
    distinct() %>% 
    rename(input = factory_recipe,
           product = factory_target)
  
  raw_resource_tbl <- tibble(input = "source_node",
                             product = just_recipes$input[grepl("Raw resource", just_recipes$input)],
                             factory_type = "environment")
  
  just_recipes <- bind_rows(just_recipes, raw_resource_tbl)

  # Need some dataframes
  # 1. From splitters to splitters
  # 2. From splitters to factories
  # 3. From factories to mergers
  # 4. From mergers to mergers
  # 5. From last merger to first splitter
  # 6. From raw resources to inputs
  
  # To connect factory groups to each other, I need to connect the last merger 
  # of each group to the first splitter of its target
  connect_factory_groups <- function(solution_factory_df, splitter_inputs){
    
    # Dataframe with the first factory of each recipe
    first_factory_df <- splitter_inputs %>%
      dplyr::filter(is_first_factory)
    
    # Dataframe with last factory of each recipe
    last_factory_df <- solution_factory_df %>%
      dplyr::filter(is_last_factory)
    
    connection_dfs <- vector("list", length = nrow(last_factory_df))
    
    for(i in 1:nrow(last_factory_df)){
      
      current_recipe <- last_factory_df[i, ]
      current_target <- current_recipe$factory_target[[1]]
      factory_merger <- current_recipe$merger_node[[1]]
      
      recipe_target <- first_factory_df %>% 
        dplyr::filter(factory_recipe == current_target,
                      input == current_recipe$factory_recipe[[1]])
      
      if(nrow(recipe_target) == 0){
        next
      }
      
      all_targets <- vector("list", length = nrow(recipe_target))
      
      for(j in 1:length(all_targets)){
        
        target_splitter <- recipe_target$splitter_name[[j]]
        
        all_targets[[j]] <- tibble(from = factory_merger,
                                   to = target_splitter,
                                   factory_target = paste(current_recipe$factory_recipe,
                                                          current_target, sep = "_"))
        
      }
      
      
      
      connection_dfs[[i]] <- reduce(all_targets,
                                    bind_rows)
      
      
    }
    
    reduce(connection_dfs, bind_rows)
  }
  
  
  
  # 1
  splitter_inputs <- left_join(just_factories, 
                               just_recipes, 
                               by = c("factory_recipe" = "product"),
                               relationship = "many-to-many") %>% 
    mutate(splitter_name = paste("splitter", input, factory_name, sep = "_")) %>% 
    group_by(factory_group, input) %>% 
    mutate(splitter_lag = dplyr::lag(splitter_name, 1)) %>% 
    ungroup()
  
  splitter_series <- splitter_inputs %>% 
    dplyr::filter(!is.na(splitter_lag)) %>%
    rename(to = splitter_name,
           from = splitter_lag)
  
  # 2
  splitter_to_factory <- splitter_inputs %>% 
    select(factory_name, splitter_name, factory_group) %>% 
    rename(from = splitter_name,
           to = factory_name)
  
  # 3
  factory_to_merger <- solution_factories %>%
    select(factory_name, merger_node, factory_group, is_last_factory) %>% 
    rename(from = factory_name,
           to = merger_node)
  
  # 4
  merger_series <- solution_factories %>%
    select(factory_recipe, factory_group, merger_node, merger_lag, is_last_factory, is_first_factory) %>%
    dplyr::filter(!is.na(merger_lag)) %>%
    rename(to = merger_node,
           from   = merger_lag)
  
  # 5
  factory_connections <- connect_factory_groups(solution_factories, splitter_inputs)
  
  
  production_buildings <- current_recipes$data_frame %>% 
    select(slug, producedIn) %>% 
    distinct()
  
  all_connections <- bind_rows(splitter_series,
                               splitter_to_factory,
                               factory_to_merger,
                               merger_series,
                               factory_connections) %>% 
    select(c("from", "to", "factory_name", "factory_group"))
  
  factory_info <- solution_factories %>%
    mutate(factory_label = paste(factory_recipe, paste0("(", round(underclock_value, 3), ")"))) %>%
    select(factory_name,
           factory_type,
           factory_label)
  
  factory_group_lookup_splitters <- solution_factories %>% 
    select(splitter_node, merger_node, factory_group) %>% 
    pivot_longer(cols = 1:2) %>% 
    select(factory_group, value) %>%
    rename(node_name = value)
  
  factory_group_lookup <- solution_factories %>% 
    select(factory_name, factory_group) %>% 
    rename(node_name = factory_name) %>%
    bind_rows(factory_group_lookup_splitters)
  
  
  node_df <- tibble(node_name = unique(c(all_connections$from, all_connections$to))) %>% 
    mutate(factory_type = ifelse(str_detect(node_name, "merger"), "merger",
                                 ifelse(str_detect(node_name, "splitter"), "splitter", NA))) %>% 
    left_join(factory_info, by = c("node_name" = "factory_name")) %>% 
    mutate(factory_type = coalesce(factory_type.x, factory_type.y)) %>%
    select(-c(factory_type.x, factory_type.y)) %>% 
    mutate(factory_size = ifelse(factory_type %in% c("splitter", "merger"), 1, 10),
           factory_label = ifelse(is.na(factory_label), factory_type, factory_label)) %>%
    left_join(factory_group_lookup, by = "node_name")
  
  # Finally, add in resource node names and connections
  resource_connection <- node_df %>% 
    dplyr::filter(factory_type == "source_node") %>% select(factory_label, node_name) %>% 
    rename(from = factory_label, to = node_name)
  
  resource_nodes <- node_df %>% 
    dplyr::filter(factory_type == "source_node") %>% 
    select(factory_label) %>% 
    rename(node_name = factory_label) %>%
    distinct()
  
  all_connections <- bind_rows(all_connections, resource_connection)
  node_df <- bind_rows(node_df, resource_nodes) %>% 
    distinct()
  
  # Full factory graph with mergers, splitters, and individual buildings
  factory_graph_full <- igraph::graph_from_data_frame(all_connections, vertices = node_df)
  
  # Second option for a cleaner looking graph with a single node for each factory group
  # What factory group does each merger belong to
  merger_groups <- factory_to_merger %>%
    select(to, factory_group) %>%
    rename(merger = to,
           merger_group = factory_group)
  
  # What factory group does each splitter belong to
  splitter_groups <- splitter_to_factory %>%
    select(from, factory_group) %>%
    rename(splitter = from,
           splitter_group = factory_group)
  
  group_connections <- factory_connections %>%
    left_join(merger_groups, by = c("from" = "merger")) %>%
    left_join(splitter_groups, by = c("to" = "splitter")) %>%
    select(merger_group, splitter_group) %>%
    rename(from = merger_group, to = splitter_group)
  
  factory_recipes <- solution_factories %>%
    select(factory_recipe, factory_group) %>%
    distinct()
  
  factory_types <- solution_factories %>% 
    select(factory_type,
           factory_group) %>%
    distinct()
  
  factory_counts <- solution_factories %>%
    mutate(short_name = paste(factory_recipe, "to", factory_target, sep = "_")) %>% 
    select(short_name,
           factory_group,
           underclock_value) %>%
    group_by(factory_group) %>%
    summarise(factory_count = sum(underclock_value)) %>% 
    left_join(factory_recipes, by = c("factory_group")) %>% 
    mutate(total_factory_count = ceiling(factory_count),
           factory_label = paste(factory_recipe, "-", total_factory_count ,paste0("(", round(factory_count, 3), ")"))) %>% 
    left_join(factory_types, by = "factory_group")

  group_graph <- igraph::graph_from_data_frame(group_connections, vertices = factory_counts)
  
  res <- list("full_graph" = factory_graph_full,
              "group_graph" = group_graph)
  #res <- list("group_graph" = group_graph)
  
  return(res)
}
