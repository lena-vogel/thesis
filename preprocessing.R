library(dplyr)
library(haven) # used for read_dta
library(this.path)

# set working directory
setwd(this.path::here())

############################################################ DATA LOADING
# set directory of raw data
read_dir <- "dta"
if (!dir.exists(read_dir)) {dir.create(read_dir)}

# load the data sets
raw_2006 <- read_dta(paste0(read_dir,"/h06f4a_STATA/h06f4a.dta"))
raw_2008 <- read_dta(paste0(read_dir,"/h08f3a_STATA/h08f3a.dta"))
raw_2010 <- read_dta(paste0(read_dir,"/hd10f6a_STATA/hd10f6a.dta"))
raw_2012 <- read_dta(paste0(read_dir,"/h12f3a_STATA/h12f3a.dta"))
raw_2014 <- read_dta(paste0(read_dir,"/h14f2b_STATA/h14f2b.dta"))
raw_2016 <- read_dta(paste0(read_dir,"/h16f2c_STATA/h16f2c.dta"))

# load the Exit/Post-Exit data sets
exit_2008 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2008v1_STATA/randhrsexit2008v1.dta"))
exit_2010 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2010v1_STATA/randhrsexit2010v1.dta"))
exit_2012 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2012v1_STATA/randhrsexit2012v1.dta"))
exit_2014 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2014v1_STATA/randhrsexit2014v1.dta"))
exit_2016 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2016v1_STATA/randhrsexit2016v1.dta"))

# define the suffixes of the 5 questions of SWLS
# we use only the suffixes, as the 1st character changes every year
swls_suffixes <- c("lb003a", "lb003b", "lb003c", "lb003d", "lb003e")

# define additional criteria/constructs to assess validity
# to use it, simple uncomment the chosen variables or add other ones
criteria <- c(
  #"b000", # life satisfaction as a whole
  #"d115", # optimism
  #"d110"  # depression
)

############################################################ FUNCTIONS
# function to only keep selected columns var_suffixes for a year_letter corresponding to the corresponding
# year and only complete rows for these columns
filter_data <- function(data, year_letter, var_suffixes) {
  # create a list of columns to keep
  columns_to_keep <- c("hhidpn", paste0(year_letter, var_suffixes))
  
  # filter the data and select only the required columns with complete cases
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

############################################################ DATA PROCESSING
# keep only the SWLS columns and remove all rows with missing data
data_2006 <- filter_data(raw_2006, "k", c(swls_suffixes, criteria))
data_2008 <- filter_data(raw_2008, "l", c(swls_suffixes, criteria))
data_2010 <- filter_data(raw_2010, "m", c(swls_suffixes, criteria))
data_2012 <- filter_data(raw_2012, "n", c(swls_suffixes, criteria))

# add "is_dead" column to 2006->2010 (check against exit files for 2012 and 2014), same for 2008->2012
data_2006 <- add_is_dead_column(data_2006, list(exit_2008, exit_2010))
data_2008 <- add_is_dead_column(data_2008, list(exit_2010, exit_2012))
data_2010 <- add_is_dead_column(data_2010, list(exit_2012, exit_2014))
data_2012 <- add_is_dead_column(data_2012, list(exit_2014, exit_2016))

# filter data to keep only rows if dead or alive 4 years later
data_2006 <- data_2006 %>%filter(is_dead == 1 | hhidpn %in% raw_2010$hhidpn | hhidpn %in% raw_2012$hhidpn | hhidpn %in% raw_2014$hhidpn | hhidpn %in% raw_2016$hhidpn)
data_2008 <- data_2008 %>%filter(is_dead == 1 | hhidpn %in% raw_2012$hhidpn | hhidpn %in% raw_2014$hhidpn | hhidpn %in% raw_2016$hhidpn)
data_2010 <- data_2010 %>%filter(is_dead == 1 | hhidpn %in% raw_2014$hhidpn | hhidpn %in% raw_2016$hhidpn)
data_2012 <- data_2012 %>%filter(is_dead == 1 | hhidpn %in% raw_2016$hhidpn)

# write filtered csv files with SWLS for each year
write_dir <- "csvs"
if (!dir.exists(write_dir)) {dir.create(write_dir)}

write.csv(data_2006, paste0(write_dir, "/data_2006.csv"), row.names = FALSE)
write.csv(data_2010, paste0(write_dir, "/data_2008.csv"), row.names = FALSE)
write.csv(data_2010, paste0(write_dir, "/data_2010.csv"), row.names = FALSE)
write.csv(data_2012, paste0(write_dir, "/data_2012.csv"), row.names = FALSE)

############################################################# OPTIONAL DISPLAY FUNCTIONS
# function that filters the data according to complete data for the selected variables and displays number of deleted rows
display_filter_data <- function(data, year_letter, var_suffixes) {
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

# diplay the number of people deceased in the 4 next following years for each year
years <- c(2006, 2008, 2010, 2012)
datasets <- list(data_2006, data_2008, data_2010, data_2012)
for (i in seq_along(years)) {
  year <- years[i]
  dataset <- datasets[[i]]
  total_deceased <- sum(dataset$is_dead, na.rm = TRUE) # Include na.rm = TRUE to handle missing values
  cat("Total deceased in the 4 following years after", year, ":", total_deceased, "\n")
}
