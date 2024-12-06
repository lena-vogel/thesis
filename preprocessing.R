library(haven)
library(dplyr)
library(ltm)

file_dir <- "dta"
# load the data sets
raw_2006 <- read_dta(paste0(file_dir,"/h06f4a_STATA/h06f4a.dta"))
raw_2008 <- read_dta(paste0(file_dir,"/h08f3a_STATA/h08f3a.dta"))
raw_2010 <- read_dta(paste0(file_dir,"/hd10f6a_STATA/hd10f6a.dta"))
raw_2012 <- read_dta(paste0(file_dir,"/h12f3a_STATA/h12f3a.dta"))
raw_2014 <- read_dta(paste0(file_dir,"/h14f2b_STATA/h14f2b.dta"))
raw_2016 <- read_dta(paste0(file_dir,"/h16f2c_STATA/h16f2c.dta"))

# load the Exit/Post-Exit data sets
exit_2008 <- read_dta(paste0(file_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2008v1_STATA/randhrsexit2008v1.dta"))
exit_2010 <- read_dta(paste0(file_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2010v1_STATA/randhrsexit2010v1.dta"))
exit_2012 <- read_dta(paste0(file_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2012v1_STATA/randhrsexit2012v1.dta"))
exit_2014 <- read_dta(paste0(file_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2014v1_STATA/randhrsexit2014v1.dta"))
exit_2016 <- read_dta(paste0(file_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2016v1_STATA/randhrsexit2016v1.dta"))

# define the suffixes of the 5 questions of SWLS and the exercise level 
# we use only the suffixes, as the 1st character changes every year
swls_suffixes <- c("lb003a", "lb003b", "lb003c", "lb003d", "lb003e")
exercise_suffixes <- c("c223","c224")

############################################################
# function that filters the data according to complete data for the selected variables and displays number of deleted rows
filter_data_display <- function(data, year_letter, var_suffixes) {
  # create the full variable names according to the year
  columns_to_check <- paste0(year_letter, var_suffixes)
  
  # init a list to store counts of removed samples for each column
  missing_counts <- sapply(columns_to_check, function(col) sum(is.na(data[[col]])))
  
  # print number of rows removed for each column
  cat("Number of rows removed due to missing values for each variable:\n")
  for (i in seq_along(columns_to_check)) {
    cat(columns_to_check[i], ":", missing_counts[i], "rows removed\n")
  }
  
  # select only rows with complete answers for the specified columns
  filtered_data <- data[complete.cases(data %>% dplyr::select(all_of(columns_to_check))), ]
  
  # print total number of rows removed
  total_removed <- nrow(data) - nrow(filtered_data)
  cat("Total rows removed after filtering:", total_removed, "\n")
  return(filtered_data)
}

# function to only keep selected columns and only complete rows for these columns
filter_data <- function(data, year_letter, var_suffixes) {
  # Create a list of columns to keep
  columns_to_keep <- c("hhidpn", paste0(year_letter, var_suffixes))
  
  # Filter the data and select only the required columns with complete cases
  filtered_data <- data %>%
    dplyr::select(all_of(columns_to_keep)) %>%
    filter(complete.cases(.))
  
  # Rename the columns by removing the first letter
  renamed_data <- filtered_data %>%
    rename_with(~ sub(paste0("^", year_letter), "", .), starts_with(year_letter))
  
  return(renamed_data)
}

# function to add "is_dead" column based on presence in exit files
add_is_dead_column <- function(data, exit_files) {
  # combine hhidpn from all exit files
  deceased_hhidpn <- unique(unlist(lapply(exit_files, function(file) file$hhidpn)))
  
  # add a new column "is_dead" which is 1 if hhidpn is in deceased_hhidpn, otherwise 0
  data <- data %>%
    mutate(is_dead = ifelse(hhidpn %in% deceased_hhidpn, 1, 0))
  
  return(data)
}
############################################################ GENERAL PROCESSING
# keep only the SWLS columns and remove all rows with missing data
data_2006 <- filter_data(raw_2006, "k", swls_suffixes)
data_2008 <- filter_data(raw_2008, "l", swls_suffixes)
data_2010 <- filter_data(raw_2010, "m", swls_suffixes)
data_2012 <- filter_data(raw_2012, "n", swls_suffixes)

# add "is_dead" column to 2006->2010 (check against exit files for 2012 and 2014), same for 2008->2012
data_2006 <- add_is_dead_column(data_2006, list(exit_2008, exit_2010))
data_2008 <- add_is_dead_column(data_2008, list(exit_2010, exit_2012))
data_2010 <- add_is_dead_column(data_2010, list(exit_2012, exit_2014))
data_2012 <- add_is_dead_column(data_2012, list(exit_2014, exit_2016))

# filter data to keep only rows if dead or alive 4 years later
data_2006 <- data_2006 %>%filter(is_dead == 1 | hhidpn %in% raw_2010$hhidpn)
data_2008 <- data_2008 %>%filter(is_dead == 1 | hhidpn %in% raw_2012$hhidpn)
data_2010 <- data_2010 %>%filter(is_dead == 1 | hhidpn %in% raw_2014$hhidpn)
data_2012 <- data_2012 %>%filter(is_dead == 1 | hhidpn %in% raw_2016$hhidpn)

years <- c(2006, 2008, 2010, 2012)
datasets <- list(data_2006, data_2008, data_2010, data_2012)

# Use a for loop to calculate and print the total deceased for each year
for (i in seq_along(years)) {
  year <- years[i]
  dataset <- datasets[[i]]
  total_deceased <- sum(dataset$is_dead, na.rm = TRUE) # Include na.rm = TRUE to handle missing values
  cat("Total deceased in the 4 following years after", year, ":", total_deceased, "\n")
}

# write filtered csv files with SWLS for each year
write.csv(data_2006, "data_2006.csv", row.names = FALSE)
write.csv(data_2010, "data_2008.csv", row.names = FALSE)
write.csv(data_2010, "data_2010.csv", row.names = FALSE)
write.csv(data_2012, "data_2012.csv", row.names = FALSE)

############################################################ PROCESSING TEST 2
# filter each data set according to the exercise level for the baselines covariates for test 2
data_2006_exercise <- filter_data(raw_2006, "k", exercise_suffixes) # exercise level
data_2008_exercise <- filter_data(raw_2008, "l", exercise_suffixes)

# join and filter rows with identical hhidpn in both years
data_2006_2010 <- inner_join(data_2006_exercise, data_2010, by = "hhidpn") 
data_2008_2012 <- inner_join(data_2008_exercise, data_2012, by = "hhidpn")

# concatenate data of the 2 groups
data2 <- bind_rows(data_2006_2010, data_2008_2012)

# implement the exercise level (for part 3.3) -> 2 if >= 1x/week vigorous or moderate exercise, else 1
data2$exercise_level <- ifelse(data2$c223 %in% c(1, 2, 7) | data2$c224 %in% c(1, 2, 7), 1, 0)

write.csv(data2, "data_test2.csv", row.names = FALSE)
