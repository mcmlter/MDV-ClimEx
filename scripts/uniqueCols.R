# This script pulls the unique column names from the files nested in the met subfolders

# Load necessary libraries
library(dplyr)

# Define the main directory containing subfolders
main_dir <- "~/GitHub/MDV-ClimEx/data"

# Initialize a set to store unique column names
unique_columns <- character()

# Function to get unique column names from a single file
get_column_names <- function(file_path) {
  # Attempt to read the file (assumes CSV format, adjust as needed)
  tryCatch({
    data <- read.csv(file_path, nrows = 0) # Read only the header
    colnames(data) # Return the column names
  }, error = function(e) {
    message(paste("Error reading file:", file_path))
    return(NULL)
  })
}

# Iterate through all subfolders and files
files <- list.files(main_dir, recursive = TRUE, full.names = TRUE)
for (file in files) {
  # Get column names for the current file
  col_names <- get_column_names(file)
  if (!is.null(col_names)) {
    # Add the column names to the unique_columns set
    unique_columns <- unique(c(unique_columns, col_names))
  }
}

# Print the unique column names
print(unique_columns)
unique_columns

df <- data.frame(x = unique_columns)

df

