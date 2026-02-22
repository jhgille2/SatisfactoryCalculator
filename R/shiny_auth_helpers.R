##################################################
## Database and Authentication Helper Functions
## For Satisfactory Calculator Shiny App
##################################################

# Initialize the database if it doesn't exist
init_database <- function(db_path = "satisfactory_users.db") {
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  # Create users table if it doesn't exist
  if (!DBI::dbExistsTable(conn, "users")) {
    DBI::dbCreateTable(conn, "users",
      fields = list(
        user_id = "INTEGER PRIMARY KEY AUTOINCREMENT",
        username = "TEXT UNIQUE NOT NULL",
        password = "TEXT NOT NULL",
        created_date = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
      )
    )
  }
  
  # Create resources table if it doesn't exist
  if (!DBI::dbExistsTable(conn, "user_resources")) {
    DBI::dbCreateTable(conn, "user_resources",
      fields = list(
        resource_id = "INTEGER PRIMARY KEY AUTOINCREMENT",
        user_id = "INTEGER NOT NULL",
        resource_name = "TEXT NOT NULL",
        quantity = "REAL NOT NULL",
        last_updated = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
        "FOREIGN KEY(user_id)" = "REFERENCES users(user_id)"
      )
    )
  }
  
  # Create alternate recipes table if it doesn't exist
  if (!DBI::dbExistsTable(conn, "user_alternate_recipes")) {
    DBI::dbCreateTable(conn, "user_alternate_recipes",
      fields = list(
        recipe_id = "INTEGER PRIMARY KEY AUTOINCREMENT",
        user_id = "INTEGER NOT NULL",
        recipe_name = "TEXT NOT NULL",
        last_updated = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
        "FOREIGN KEY(user_id)" = "REFERENCES users(user_id)"
      )
    )
  }
  
  DBI::dbDisconnect(conn)
}

# Hash password using digest
hash_password <- function(password) {
  digest::digest(password, algo = "sha256")
}

# Verify password
verify_password <- function(password, hash) {
  digest::digest(password, algo = "sha256") == hash
}

# Create new user
create_user <- function(username, password, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Check if user already exists
    existing <- DBI::dbGetQuery(conn, 
      paste0("SELECT user_id FROM users WHERE username = '", username, "'"))
    
    if (nrow(existing) > 0) {
      DBI::dbDisconnect(conn)
      return(list(success = FALSE, message = "Username already exists"))
    }
    
    # Insert new user
    DBI::dbExecute(conn,
      "INSERT INTO users (username, password) VALUES (?, ?)",
      params = list(username, hash_password(password))
    )
    
    DBI::dbDisconnect(conn)
    return(list(success = TRUE, message = "User created successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Authenticate user
authenticate_user <- function(username, password, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    user_data <- DBI::dbGetQuery(conn,
      paste0("SELECT user_id, password FROM users WHERE username = '", username, "'")
    )
    
    DBI::dbDisconnect(conn)
    
    if (nrow(user_data) == 0) {
      return(list(success = FALSE, user_id = NULL, message = "Username not found"))
    }
    
    if (verify_password(password, user_data$password[1])) {
      return(list(success = TRUE, user_id = user_data$user_id[1], message = "Login successful"))
    } else {
      return(list(success = FALSE, user_id = NULL, message = "Incorrect password"))
    }
  }, error = function(e) {
    return(list(success = FALSE, user_id = NULL, message = paste("Error:", e$message)))
  })
}

# Save user resources
save_user_resources <- function(user_id, resources_df, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Delete existing resources for this user
    DBI::dbExecute(conn,
      "DELETE FROM user_resources WHERE user_id = ?",
      params = list(user_id)
    )
    
    # Insert new resources
    for (i in 1:nrow(resources_df)) {
      DBI::dbExecute(conn,
        "INSERT INTO user_resources (user_id, resource_name, quantity) VALUES (?, ?, ?)",
        params = list(user_id, resources_df[i, "item"], resources_df[i, "qty"])
      )
    }
    
    DBI::dbDisconnect(conn)
    return(list(success = TRUE, message = "Resources saved successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error saving resources:", e$message)))
  })
}

# Load user resources
load_user_resources <- function(user_id, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    user_resources <- DBI::dbGetQuery(conn,
      "SELECT resource_name, quantity FROM user_resources WHERE user_id = ? ORDER BY resource_name",
      params = list(user_id)
    )
    
    DBI::dbDisconnect(conn)
    
    if (nrow(user_resources) == 0) {
      return(NULL)
    }
    
    # Rename columns to match app format
    names(user_resources) <- c("item", "qty")
    return(user_resources)
  }, error = function(e) {
    return(NULL)
  })
}

# Get user ID from username
get_user_id <- function(username, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    result <- DBI::dbGetQuery(conn,
      paste0("SELECT user_id FROM users WHERE username = '", username, "'")
    )
    
    DBI::dbDisconnect(conn)
    
    if (nrow(result) > 0) {
      return(result$user_id[1])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# Save user alternate recipes
save_user_alternate_recipes <- function(user_id, recipes_vector, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Delete existing alternate recipes for this user
    DBI::dbExecute(conn,
      "DELETE FROM user_alternate_recipes WHERE user_id = ?",
      params = list(user_id)
    )
    
    # Insert new alternate recipes (skip empty values)
    recipes_vector <- recipes_vector[recipes_vector != ""]
    if (length(recipes_vector) > 0) {
      for (recipe in recipes_vector) {
        DBI::dbExecute(conn,
          "INSERT INTO user_alternate_recipes (user_id, recipe_name) VALUES (?, ?)",
          params = list(user_id, recipe)
        )
      }
    }
    
    DBI::dbDisconnect(conn)
    return(list(success = TRUE, message = "Alternate recipes saved successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error saving alternate recipes:", e$message)))
  })
}

# Load user alternate recipes
load_user_alternate_recipes <- function(user_id, db_path = "satisfactory_users.db") {
  tryCatch({
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    user_recipes <- DBI::dbGetQuery(conn,
      "SELECT recipe_name FROM user_alternate_recipes WHERE user_id = ? ORDER BY recipe_name",
      params = list(user_id)
    )
    
    DBI::dbDisconnect(conn)
    
    if (nrow(user_recipes) == 0) {
      return(character(0))
    }
    
    return(user_recipes$recipe_name)
  }, error = function(e) {
    return(character(0))
  })
}
