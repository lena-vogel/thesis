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
compute_results <- function(indicators, outcome, discriminant_construct, convergent_construct) {
  # get the indicators names
  var_names <- colnames(indicators)
  
  # store results in a list
  results <- list()
  
  # generate combinations of indicators (starting from 5 to 2 indicators)
  for (d in ncol(indicators):3) {
    combinations <- combn(var_names, d, simplify = FALSE)
    
    for (combo in combinations) {
      # get the subset of columns by their names
      set_items <- indicators[, combo, drop = FALSE]
      
      # compute p-value depending on the number of indicators
      lambdas <- if      (d == 3) stde_3_indicators(set_items,outcome)$lambdas
                 else if (d == 4) stde_4_indicators(set_items,outcome)$lambdas
                 else if (d == 5) stde_5_indicators(set_items,outcome)$lambdas
                 else list(stde=NA,lambdas=NA)
      
      # compute the total scores
      total_score <- rowSums(set_items * lambdas)

      # compute correlation with discriminant criteria or construct
      discriminant_cor <- cor(total_score, discriminant_construct)

      # compute correlation with convergent criteria or construct
      convergent_cor <- cor(total_score, convergent_construct)

      # store results as a vector
      results <- append(results, list(c(
        paste(combo, collapse = ", "),
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

# define the outcome
outcome <- data$is_dead

# select the convergent/discriminant construct/criteria and make additional format changes
depression <- data$d110
depression <- ifelse(depression == 1, 1, 0)
optimism   <- data$d115
optimism   <- ifelse(optimism == 1, 1, 0)
ls         <- data$b000
ls[ls %in% c(8, 9)] <- 3

# compute results for each combination
results_matrix <- compute_results(indicators, outcome, discriminant_construct = depression, convergent_construct = optimism)

# save the matrix to a .csv file with the chosen year(s) in the name
write.csv(results_matrix, paste0(load_dir,"/validity_",paste(years,collapse="_"),".csv"), row.names = FALSE)

