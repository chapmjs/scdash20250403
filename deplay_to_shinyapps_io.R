# Deploy Supply Chain Dashboard to shinyapps.io
# This script handles deployment to shinyapps.io

# Load necessary libraries
library(rsconnect)

# Setup function to prepare for deployment
prepare_for_deployment <- function() {
  # Ensure renv lock file is up to date
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::snapshot()
  }
  
  # Check for necessary files
  required_files <- c(
    "app.R", 
    "global.R", 
    "ui.R", 
    "server.R", 
    "data/Supply Chain Dashboard.xlsx",
    "www/custom.css",
    "www/script.js"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    cat("ERROR: The following required files are missing:\n")
    cat(paste(" -", missing_files), sep = "\n")
    cat("\nPlease ensure all files exist before deploying.\n")
    return(FALSE)
  }
  
  cat("All required files present. Ready for deployment!\n")
  return(TRUE)
}

# Function to deploy to shinyapps.io
deploy_to_shinyapps <- function() {
  # Check if accounts are set
  tryCatch({
    accounts <- rsconnect::accounts()
    if (length(accounts) == 0) {
      cat("ERROR: No shinyapps.io account credentials found.\n")
      cat("Please set up your shinyapps.io account first using:\n\n")
      cat('rsconnect::setAccountInfo(name="YOUR_ACCOUNT", token="TOKEN", secret="SECRET")\n\n')
      return(FALSE)
    }
  }, error = function(e) {
    cat("ERROR: No shinyapps.io account credentials found.\n")
    cat("Please set up your shinyapps.io account first using:\n\n")
    cat('rsconnect::setAccountInfo(name="YOUR_ACCOUNT", token="TOKEN", secret="SECRET")\n\n')
    return(FALSE)
  })
  
  # Prepare for deployment
  if (!prepare_for_deployment()) {
    return(FALSE)
  }
  
  # List of files to deploy
  deploy_files <- c(
    "app.R", 
    "global.R", 
    "ui.R", 
    "server.R", 
    "data/Supply Chain Dashboard.xlsx",
    "www/custom.css",
    "www/script.js"
  )
  
  # Deploy the application
  cat("Deploying to shinyapps.io...\n")
  tryCatch({
    rsconnect::deployApp(
      appFiles = deploy_files,
      appName = "SupplyChainDashboard",
      appTitle = "Supply Chain Dashboard",
      appId = NULL,  # Will create a new app if NULL
      launch.browser = TRUE
    )
    
    cat("Deployment successful!\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR during deployment:", e$message, "\n")
    return(FALSE)
  })
}

# Run the deployment
deploy_to_shinyapps()

# Run the deployment
deploy_to_shinyapps()