# install_libraries.R
#library(tidyverse)
library(tidyr)
# First line to make sure the basic utils are there
install.packages("utils", repos = "https://cran.r-project.org/")

# Check for devtools and install if not present
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos = "https://cran.r-project.org/")
}

# Install bartik.weight from GitHub
if (!requireNamespace("bartik.weight", quietly = TRUE)) {
  devtools::install_github("paulgp/bartik-weight/R-code/pkg")
}

# Get the list of library names from common_libraries.R
library_names <- readLines("scripts/common_libraries.R") %>%
  stringr::str_extract("(?<=library\\()(.*?)(?=\\))") %>% 
  na.omit()

# Remove bartik.weight since it's already installed
library_names <- library_names[library_names != "bartik.weight"]

# Function to install missing CRAN packages
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, repos = "https://cran.r-project.org/")
  }
}

# Install missing libraries
lapply(library_names, install_if_missing)

print("All required packages installed successfully")