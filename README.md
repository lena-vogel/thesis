# Master's thesis code

Access to the data:
- In this study, we exclusively used `.dta` files, so make sure to download this type of files only.
- All the data are openly accessible on the Health and Retirement Study ([HRS](https://hrsdata.isr.umich.edu/))
- Core files (contains all answers to all questionnaires in 2-year intervals), see https://hrsdata.isr.umich.edu/data-products/public-survey-data
- Exit files (registers the people who die), see https://hrsdata.isr.umich.edu/data-products/rand-hrs-exitpost-exit-interview-and-finder-files-2020

Structure of the repository:

    .
    ├── main.R                  # main file which loads the data sets from csvs, previously processed and generates a table in csvs
    ├── (names_covariates.R)    # list of the names of the covariates. This list is not currently used in our code
    ├── preprocessing.R         # preprocesses the files from dtas and generates new files in csvs
    ├── stde.R                  # contains the functions necessary for testing the structural assumption of the univariate LFM, for 3, 4 or 5 items
    ├── validity_assessment.R   # similar to main.R but only computes the correlations with some chosen criteria or constructs
    ├── csvs                    # folder to store preprocessed data and results
    │   ├── h06f4a_STATA        # 2006 core file
    │   │   ├── h06f4a.dta      # 2006 core data
    │   ├── h08f3a_STATA        # 2008 core file
    │   │   ├── h08f3a.dta      # 2008 core data
    │   │   ...
    │   ├── randhrsexit1994_2020v1_STATA    # all exit files until 2020
    │   │   ├── randhrsexit2008v1_STATA     # 2008 exit folder
    │   │       └── randhrsexit2008v1.dta"  # 2008 exit file
    │   │   ├── randhrsexit2010v1_STATA     # 2010 exit folder
    │   │       └── randhrsexit2010v1.dta"  # 2010 exit file
    │   └── ...               
    └── dta                        # folder with processed data and results
        ├── results_2010_2012.csv  # results for 2010 and 2012
        ├── validity_2010_2012.csv # validity results for 2010 and 2012
        └── ...

    
