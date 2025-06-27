# .Rprofile - Ensure required packages are available

# Set CRAN repository
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Function to check and install packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "tidyverse",
  "DT",
  "randomForest",
  "glmnet",
  "MatchIt",
  "broom",
  "viridis"
)

# Check and install each package
invisible(lapply(required_packages, check_and_install))
