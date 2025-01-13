library(dplyr) # used for bind_rows
library(ltm)   # used for cronbach.alpha
library(psych) # used for omega
library(lavaan)# used for anove

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

# function to compute Cronbach's alpha and p-values for combinations of indicators
compute_results <- function(indicators,outcome) {
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
      d <- length(combo)
      
      # compute Cronbach's alpha
      alpha <- ltm::cronbach.alpha(subset_data)$alpha
      
      # compute McDonald's Omega, suppress message as we don't use Omega_h
      omega_tot <- suppressMessages(psych::omega(subset_data,nfactors = 1,fm = "minres")$omega.tot)
      
      # analysis of tau-equivalence for combos of more than 2 items
      if (d>2){
        # create and fit the unconstrained model
        model_unconstrained <- paste0("LatentFactor =~ ", paste(combo, collapse = " + "))
        fit_unconstrained <- cfa(model_unconstrained, data = data.frame(subset_data))
  
        # create and fit constrained model (equal loading factors)
        model_constrained <- paste0("LatentFactor =~ ", paste(paste0("l * ", combo), collapse = " + "))
        fit_constrained <- cfa(model_constrained, data = data.frame(subset_data))
  
        # compare the models with ANOVA 
        comparison <- anova(fit_unconstrained, fit_constrained)
  
        # extract the p-value
        p_value_cfa <- comparison$`Pr(>Chisq)`[2]
        }
      else p_value_cfa <- NA

      # compute p-value depending on the number of indicators
      result <- if      (d == 3) stde_3_indicators(subset_data,outcome)
                else if (d == 4) stde_4_indicators(subset_data,outcome)
                else if (d == 5) stde_5_indicators(subset_data,outcome)
                else c(NA,NA)
      
      # store results as a vector
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # set of indicators name
        format(alpha, digits=2),        # Coefficient alpha
        format(omega_tot, digits=2),    # Coefficient omega
        format(result[2], digits=2),    # range of loading factors estimates
        if (!is.na(p_value_cfa)) format(p_value_cfa, digits=2) else NA,  # P-value for CFA
        if (!is.na(result[1])) format(result[1], digits=2) else NA  # P-value for STDE
      )))
    }
  }
  
  # convert the results list to a matrix and define column names
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- c("Combination", 
                                "Alpha", 
                                "Omega", 
                                "Range of loading factors",
                                "P-value tau-equivalence",
                                "P-value STDE")
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

# save the matrix to a .csv file with the chosen year(s) in the name
write.csv(results_matrix, paste0(load_dir,"/results_",paste(years,collapse="_"),".csv"), row.names = FALSE)

