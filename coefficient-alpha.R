
folder <- "csvs"
data <- read.csv(paste0(folder,"/data_test2.csv"))

# Cronbach's alpha computation with different indicators
# Create a sample dataset (replace with your actual data)
sample_data <- data.frame(
  X1 = data$lb003a,
  X2 = data$lb003b,
  X3 = data$lb003c,
  X4 = data$lb003d,
  X5 = data$lb003e
)

# Function to compute Cronbach's alpha for all combinations of variables
compute_cronbach_alpha <- function(data) {
  # Get variable names
  var_names <- colnames(data)
  
  # Store results in a list
  results <- list()
  
  # Generate all combinations of 5 to 2 variables (inverse order)
  for (num_vars in length(var_names):2) {
    combinations <- combn(var_names, num_vars, simplify = FALSE)
    
    for (combo in combinations) {
      # Subset data based on the current combination
      subset_data <- data[, combo]
      
      # Compute Cronbach's alpha
      alpha <- cronbach.alpha(subset_data)$alpha
      
      # Store results in a list
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # Combination of indicators
        round(alpha, 3)                 # Cronbach's alpha (rounded to 3 decimal places)
      )))
    }
  }
  
  # Convert the list of results into a matrix
  results_matrix <- do.call(rbind, results)
  
  # Assign column names
  colnames(results_matrix) <- c("Combination", "Cronbach_Alpha")
  return(results_matrix)
}

# Compute Cronbach's alpha for all combinations and store in matrix
results_matrix <- compute_cronbach_alpha(sample_data)

# Save the matrix to a CSV file
write.csv(results_matrix, paste0(folder,"/coefficient_alphas.csv"), row.names = FALSE)

