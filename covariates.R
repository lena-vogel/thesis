# define the the covariates (only 2010 and 2012)
cov_suffixes <- c("b082",   # religious service attendance
                  "a019",   # age
                  "b063",   # marital status
                  "x060_r", # sex
                  "j005m1", # employment status
                  "x026m",  # geographic region of 1st address
                  "n001",   # health insurance
                  
                  "c005",   # hypertension
                  "c010",   # diabetes
                  "c053",   # stroke
                  "c018",   # cancer (aside from skin)
                  "c036",   # heart disease
                  "c030",   # lung disease
                  "c070",   # arthritis
                  "c139",   # weight (not specifically obesity)
                  "c086",   # sleep well
                  "c223",   # vigorous physical activity
                  "c224",   # moderate physical activity
                  "c225",   #     mild physical activity
                  "c128",   # drink alcohol or not
                  
                  "d114",   # loneliness (removes 200 people)
                  "d110",   # depressive symptoms (removes 200 people)
                  "lb035g", # purpose in life
                  "lb019m", # hopelessness
                  "lb019g" # optimism
)