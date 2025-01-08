library(dplyr) # used for bind_rows
library(ltm)   # used for cronbach.alpha
library(psych) # used for omega


source("stde.R") # import functions to compute the p-values for the statistical test

# load files
load_dir <- "csvs"
years <- c(2010,2012) # select years to load
data <-c()
for (year in years) {
  data <- bind_rows(data, read.csv(paste0(load_dir,"/data_",year,".csv")))
}

# function to compute Cronbach's alpha and p-values for combinations of indicators
compute_results <- function(indicators,outcome) {
  # Get the indicators names
  var_names <- colnames(indicators)
  
  # store results in a list
  results <- list()
  
  # generate combinations of indicators (starting from 5 to 2 indicators)
  for (num_vars in ncol(indicators):2) {
    combinations <- combn(var_names, num_vars, simplify = FALSE)
    
    for (combo in combinations) {
      # get the subset of columns by their names
      subset_data <- indicators[, combo, drop = FALSE]
      
      # compute Cronbach's alpha
      alpha <- ltm::cronbach.alpha(subset_data)$alpha
      
      # compute McDonald's Omega, suppress message as we don't use Omega_h
      omega_tot <- suppressMessages(psych::omega(subset_data,nfactors = 1,fm = "minres")$omega.tot)
      
      # compute p-value depending on the number of indicators
      result <- if      (length(combo) == 3) stde_3_indicators(subset_data,outcome)
                else if (length(combo) == 4) stde_4_indicators(subset_data,outcome)
                else if (length(combo) == 5) stde_5_indicators(subset_data,outcome)
                else c(NA,NA)
      
      # store results as a vector
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # Combination of indicator names
        format(result[2], digits=2),                # Cronbach's alpha (rounded to 2 decimals)
        format(alpha, digits=2),                # Cronbach's alpha (rounded to 2 decimals)
        format(omega_tot, digits=2),
        if (!is.na(result[1])) format(result[1], digits=2) else NA  # P-value rounded to 3 decimals or NA
      )))
    }
  }
  
  # convert the results list to a matrix and define column names
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- c("Combination", 
                                "Difference",
                                "Alpha", 
                                "Omega", 
                                "P-value")
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

# compute results for each combination
results_matrix <- compute_results(indicators, outcome)

# save the matrix tomega_h# save the matrix to a .csv file with the chosen year(s) in the name
write.csv(results_matrix, paste0(load_dir,"/results_",paste(years,collapse="_"),".csv"), row.names = FALSE)

