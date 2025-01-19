library(dplyr)   # used for bind_rows
library(ltm)     # used for cronbach.alpha
library(psych)   # used for omega
library(lavaan)  # used for anova
library(paran)   # used for paran (parallel analysis)

# set working directory
setwd(this.path::here())

# import functions to compute the p-values for the statistical test
source("stde.R") 

# load files
load_dir <- "csvs"

# select years to load
years <- c(2010,2012) 

# select the n-tiles to compare levels of the latent factor
n_tiles <- 2

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
  print(paste0("Results will be saved in '",load_dir,"/results_",paste(years,collapse="_"),".csv'"))
  
  # generate combinations of indicators (starting from 5 to 2 indicators)
  for (d in ncol(indicators):2) {
    combinations <- combn(var_names, d, simplify = FALSE)
    print(paste0("Computing results for sets of ", d, " items..."))
    
    for (combo in combinations) {
      # get the subset of columns by their names
      set_items <- indicators[, combo, drop = FALSE]

      # run Horn's parallel analysis
      invisible(capture.output({
        n_factors <- paran(set_items,cfa=TRUE, centile = 99)$Retained
      }))

      # compute Coefficient alpha
      alpha <- ltm::cronbach.alpha(set_items)$alpha
      
      # compute McDonald's Omega, suppress message as we don't use Omega_h
      omega_tot <- suppressMessages(psych::omega(set_items,nfactors = 1,fm = "minres")$omega.tot)
      
      # analysis of tau-equivalence for combos of more than 2 items
      if (d>2){
        # create and fit the unconstrained model
        model_unconstrained <- paste0("LatentFactor =~ ", paste(combo, collapse = " + "))
        fit_unconstrained <- cfa(model_unconstrained, data = data.frame(set_items))
  
        # create and fit constrained model (equal loading factors)
        model_constrained <- paste0("LatentFactor =~ ", paste(paste0("l * ", combo), collapse = " + "))
        fit_constrained <- cfa(model_constrained, data = data.frame(set_items))
  
        # compare the models with ANOVA 
        comparison <- anova(fit_unconstrained, fit_constrained)
        
        # extract the p-value
        p_value_cfa <- comparison$`Pr(>Chisq)`[2]
        }
      else p_value_cfa <- NA

      # compute p-value depending on the number of indicators
      result <- if      (d == 3) stde_3_indicators(set_items,outcome)
                else if (d == 4) stde_4_indicators(set_items,outcome)
                else if (d == 5) stde_5_indicators(set_items,outcome)
                else list(stde=NA,lambdas=NA)

      # compute lambda range
      lambdas <- result$lambdas
      range_lambda <- (max(lambdas)-min(lambdas))/mean(lambdas)
      
      # compute the effect of total score on outcome, using 1st and 3rd tertiles
      total_score <- if (d>2) rowSums(indicators * lambdas)
                     else     rowSums(indicators)
      tertiles <- ntile(total_score, n_tiles)
      data_with_tertiles <- bind_cols(set_items, tertile = tertiles, outcome = outcome)
      f_tile_data <- filter(data_with_tertiles, tertile == 1)       # data in first tertile (lowest total scores)
      n_tile_data <- filter(data_with_tertiles, tertile == n_tiles) # data in n-th tertile (lowest total scores)
      effect <- mean(f_tile_data$outcome)/mean(n_tile_data$outcome)
      
      # store results as a vector
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # set of indicators name
        paste0(n_factors),
        format(range_lambda, digits=2),    # range of loading factors estimates
        if (!is.na(p_value_cfa)) format(p_value_cfa, digits=2) else NA,  # P-value for CFA
        format(alpha, digits=2),        # Coefficient alpha
        format(omega_tot, digits=2),    # Coefficient omega
        if (!is.na(result$stde)) format(result$stde, digits=2) else NA,  # P-value for STDE
        format(effect, digits=3)
      )))
    }
  }
  
  # convert the results list to a matrix and define column names
  results_matrix <- do.call(rbind, results)
  colnames(results_matrix) <- c("Combination", 
                                "Nb factors",
                                "Range of loading factors",
                                "P-value tau-equivalence",
                                "Alpha", 
                                "Omega", 
                                "P-value STDE",
                                "Effect")
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