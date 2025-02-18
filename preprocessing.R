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
raw_2018 <- read_dta(paste0(read_dir,"/h18f2b_STATA/h18f2b.dta"))
raw_2020 <- read_dta(paste0(read_dir,"/h20f1a_STATA/h20f1a.dta"))


# load the Exit/Post-Exit data sets
exit_2008 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2008v1_STATA/randhrsexit2008v1.dta"))
exit_2010 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2010v1_STATA/randhrsexit2010v1.dta"))
exit_2012 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2012v1_STATA/randhrsexit2012v1.dta"))
exit_2014 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2014v1_STATA/randhrsexit2014v1.dta"))
exit_2016 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2016v1_STATA/randhrsexit2016v1.dta"))
exit_2018 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2018v2_STATA/randhrsexit2018v2.dta"))
exit_2020 <- read_dta(paste0(read_dir,"/randhrsexit1994_2020v1_STATA/randhrsexit2020v1_STATA/randhrsexit2020v1.dta"))

# define the year-letter mapping
year_to_letter <- c("2008" = "l", 
                    "2010" = "m", 
                    "2012" = "n",
                    "2014" = "o",
                    "2016" = "p")

# define the suffixes of the 5 questions of SWLS
swls_standard <- c("a", "b", "c", "d", "e")

# define additional criteria/constructs to assess validity
# to use it, simple uncomment the chosen variables or add other ones
criteria <- c(
  "b000", # life satisfaction as a whole
  "d115", # optimism
  "d110"  # depression
)

############################################################ FUNCTIONS
# function to rename SWLS columns to standard format
rename_columns <- function(data, year_letter) {
  swls_cols  <- grep(paste0("^", year_letter, "lb00[23][a-e]"), names(data), value = TRUE)
  names(data)[names(data) %in% swls_cols] <- swls_standard
  
  ls         <- grep(paste0("^", year_letter, "b000"), names(data), value = TRUE)
  optimism   <- grep(paste0("^", year_letter, "d115"), names(data), value = TRUE)
  depression <- grep(paste0("^", year_letter, "d110"), names(data), value = TRUE)
  names(data)[names(data) %in% c(ls,optimism,depression)] <- criteria
  return(data)
}

# function to only keep selected columns var_suffixes for a year_letter corresponding to the corresponding
# year and only complete rows for these columns
filter_data <- function(data, year_letter, var_suffixes) {
  # create a list of columns to keep
  columns_to_keep <- c("hhidpn", paste0(year_letter, var_suffixes))
  # filter the data and select only the required columns with complete cases
  filtered_data <- data %>%dplyr::select(all_of(columns_to_keep)) %>%filter(complete.cases(.))
  # Rename the columns by removing the first letter
  renamed_data <- filtered_data %>%rename_with(~ sub(paste0("^", year_letter), "", .), starts_with(year_letter))
  return(renamed_data)
}

# function to add "is_dead" column based on presence in exit files
add_is_dead_column <- function(data, exit_files) {
  # combine hhidpn from all exit files
  deceased_hhidpn <- unique(unlist(lapply(exit_files, function(file) file$hhidpn)))

  # add a new column "is_dead" which is 1 if hhidpn is in deceased_hhidpn, otherwise 0
  data <- data %>%mutate(is_dead = ifelse(hhidpn %in% deceased_hhidpn, 1, 0))
  return(data)
}

############################################################ DATA PROCESSING
# keep only the SWLS columns and remove all rows with missing data
# loop over the years in dictionary to filter the data
for (year in names(year_to_letter)) {
  raw_data <- get(paste0("raw_", year))
  raw_data <- rename_columns(raw_data, year_to_letter[year])
  assign(paste0("data_", year), raw_data %>% dplyr::select(hhidpn
                                                           , all_of(swls_standard)
                                                           #, all_of(criteria)
                                                           ) 
         %>% filter(complete.cases(.)))
}

# add "is_dead" column to 2006->2010 (check against exit files for 2012 and 2014), same for 2008->2012
for (year in names(year_to_letter)) {
  assign(paste0("data_", year), add_is_dead_column(get(paste0("data_", year)), list(get(paste0("exit_", as.numeric(year)+2))
                                                                                    #, get(paste0("exit_", as.numeric(year)+4))
                                                                                    )))
}

# filter data to keep only rows if dead or alive 4 years later
for (year in names(year_to_letter)) {
  assign(paste0("data_", year), get(paste0("data_", year))%>%filter(is_dead == 1 | hhidpn %in% get(paste0("raw_", as.numeric(year)+2))$hhidpn))
}

# write filtered csv files with SWLS for each year
write_dir <- "csvs"
if (!dir.exists(write_dir)) {dir.create(write_dir)}

for (year in names(year_to_letter)) {
  write.csv(get(paste0("data_", year)), paste0(write_dir, "/data_", year, ".csv"), row.names = FALSE)
}

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
years <- c(2008, 2010, 2012)
datasets <- list(data_2008, data_2010, data_2012)
for (i in seq_along(years)) {
  year <- years[i]
  dataset <- datasets[[i]]
  total_deceased <- sum(dataset$is_dead, na.rm = TRUE) # Include na.rm = TRUE to handle missing values
  cat("Total deceased in the 4 following years after", year, ":", total_deceased, "\n")
}

