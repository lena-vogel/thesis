library(dplyr) # used for bind_rows

# import functions to compute the p-values for the statistical test
source("stde.R") 

# load files
load_dir <- "csvs"

# select years to load
years <- c(2010,2012) 

# load the corresponding data
data <-c()
for (year in years) {
  data <- bind_rows(data, read.csv(paste0(load_dir,"/data_",year,".csv")))
}

# function to compute correlations with constructs
compute_results <- function(indicators, discriminant_construct, convergent_construct) {
  # get the indicators names
  var_names <- colnames(indicators)
  
  # store results in a list
  results <- list()
  
  # generate combinations of indicators (starting from 5 to 2 indicators)
  for (num_vars in ncol(indicators):2) {
    combinations <- combn(var_names, num_vars, simplify = FALSE)
    
    for (combo in combinations) {
      # get the subset of columns by their names
      subset_data <- indicators[, combo, drop = FALSE]
      
      # compute the total score
      total_score <- rowSums(subset_data, na.rm = TRUE)
      
      print(total_score)
      print(discriminant_construct)
      # compute correlation with discriminant criteria or construct
      discriminant_cor <- cor(total_score, discriminant_construct)

      # compute correlation with convergent criteria or construct
      convergent_cor <- cor(total_score, convergent_construct)

      # store results as a vector
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # Combination of indicator names
        format(mean(discriminant_cor), digits=2),
        format(mean(convergent_cor), digits=2)
      )))
    }
  }
  
  # convert the results list to a matrix and define column names
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- c("Combination", 
                                "Discriminant cor",
                                "Convergent cor")
  return(results_matrix)
}

# create a matrix of indicators
indicators <- cbind(
  X1 = data$lb003a,
  X2 = data$lb003b,
  X3 = data$lb003c,
  X4 = data$lb003d,
  X5 = data$lb003e
)

# select the convergent/discriminant construct/criteria and make additional format changes
depression <- data$d110
depression <- ifelse(depression == 1, 1, 0)
optimism   <- data$d115
optimism   <- ifelse(optimism == 1, 1, 0)
ls         <- data$b000
ls[ls %in% c(8, 9)] <- 3

# compute results for each combination
results_matrix <- compute_results(indicators, discriminant_construct = depression, convergent_construct = optimism)

# save the matrix to a .csv file with the chosen year(s) in the name
write.csv(results_matrix, paste0(load_dir,"/validity_",paste(years,collapse="_"),".csv"), row.names = FALSE)

