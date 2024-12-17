library(haven)
library(dplyr)
library(ltm)

# load the data sets
raw_2006 <- read_dta("h06f4a_STATA/h06f4a.dta")
raw_2008 <- read_dta("h08f3a_STATA/h08f3a.dta")
raw_2010 <- read_dta("hd10f6a_STATA/hd10f6a.dta")
raw_2012 <- read_dta("h12f3a_STATA/h12f3a.dta")
raw_2014 <- read_dta("h14f2b_STATA/h14f2b.dta")
raw_2016 <- read_dta("h16f2c_STATA/h16f2c.dta")

# load the Exit/Post-Exit data sets
exit_2012 <- read_dta("randhrsexit1994_2020v1_STATA/randhrsexit2012v1_STATA/randhrsexit2012v1.dta")
exit_2014 <- read_dta("randhrsexit1994_2020v1_STATA/randhrsexit2014v1_STATA/randhrsexit2014v1.dta")
exit_2016 <- read_dta("randhrsexit1994_2020v1_STATA/randhrsexit2016v1_STATA/randhrsexit2016v1.dta")

# define the 5 questions of SWLS (only the suffixes, the 1st letter is missing)
swls_suffixes <- c("lb003a", "lb003b", "lb003c", "lb003d", "lb003e")

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
  filtered_data <- data[complete.cases(data %>% select(all_of(columns_to_check))), ]
  
  # print total number of rows removed
  total_removed <- nrow(data) - nrow(filtered_data)
  cat("Total rows removed after filtering:", total_removed, "\n")
  return(filtered_data)
}

# function to only keep selected columns and only complete rows for these columns
filter_data <- function(data, year_letter, var_suffixes) {
  # Create a list of columns to keep
  columns_to_keep <- c("hhidpn", paste0(year_letter, var_suffixes))
  print(columns_to_keep)
  # Filter the data and select only the required columns with complete cases
  filtered_data <- data %>%
    select(all_of(columns_to_keep)) %>%
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
############################################################

# TODO put things on the same scale
# filter each data set according to the 5 SWLS questions only
data_2006 <- filter_data(raw_2006, "k", c(swls_suffixes, cov_suffixes, "lb037j", "lb002")) # childhood abuse, social integration (attendance to social clubs)
data_2008 <- filter_data(raw_2008, "l", c(swls_suffixes, cov_suffixes, "lb037n", "lb001f"))
data_2010 <- filter_data(raw_2010, "m", swls_suffixes)
data_2012 <- filter_data(raw_2012, "n", swls_suffixes)

data_2006 <- data_2006 %>% rename(childhood_abuse = lb037j, social = lb002) # NOT ON SAME SCALE
data_2008 <- data_2008 %>% rename(childhood_abuse = lb037n, social = lb001f)

# join and filter rows with identical hhidpn in both years
data_2006_2010 <- inner_join(data_2006, data_2010, by = "hhidpn", suffix = c("_0", "_1")) 
data_2008_2012 <- inner_join(data_2008, data_2012, by = "hhidpn", suffix = c("_0", "_1"))

# add "is_dead" column to 2006->2010 (check against exit files for 2012 and 2014), same for 2008->2012
data_2006_2010 <- add_is_dead_column(data_2006_2010, list(exit_2012, exit_2014))
data_2008_2012 <- add_is_dead_column(data_2008_2012, list(exit_2014, exit_2016))

# filter data to keep only rows if dead or alive 4 years later
data_2006_2010 <- data_2006_2010 %>%
  filter(is_dead == 1 | hhidpn %in% raw_2014$hhidpn)
data_2008_2012 <- data_2008_2012 %>%
  filter(is_dead == 1 | hhidpn %in% raw_2016$hhidpn)

# show sum of is_dead column
cat("Total deceased in the 4 following years after 2010:", sum(data_2006_2010$is_dead), "\n")
cat("Total deceased in the 4 following years after 2012:", sum(data_2008_2012$is_dead), "\n")

# concatenate data of the 2 groups
data <- bind_rows(data_2006_2010, data_2008_2012)

# implement the exercise level (for part 3.3) -> 2 if >= 1x/week vigorous or moderate exercise, else 1
data$exercise_level <- ifelse(data$c223 %in% c(1, 2, 7) | data$c224 %in% c(1, 2, 7), 1, 0)

# Cronbach's alpha computation with different indicators
# Create a sample dataset (replace with your actual data)
sample_data <- data.frame(
  X1 = data$lb003a_1,
  X2 = data$lb003b_1,
  X3 = data$lb003c_1,
  X4 = data$lb003d_1,
  X5 = data$lb003e_1
)

# Function to compute Cronbach's alpha for all combinations of variables
compute_cronbach_alpha <- function(data) {
  # Get variable names
  var_names <- colnames(data)
  
  # Store results in a list
  results <- list()
  
  # Generate all combinations of 5 to 2 variables (inverse order)
  for (num_vars in length(var_names):2) {
    combinations <- combn(var_names, num_vars, simplify = FALSE)
    
    for (combo in combinations) {
      # Subset data based on the current combination
      subset_data <- data[, combo]
      
      # Compute Cronbach's alpha
      alpha <- cronbach.alpha(subset_data)$alpha
      
      # Store results in a list
      results <- append(results, list(c(
        paste(combo, collapse = ", "),  # Combination of indicators
        round(alpha, 3)                 # Cronbach's alpha (rounded to 3 decimal places)
      )))
    }
  }
  
  # Convert the list of results into a matrix
  results_matrix <- do.call(rbind, results)
  
  # Assign column names
  colnames(results_matrix) <- c("Combination", "Cronbach_Alpha")
  return(results_matrix)
}

# Compute Cronbach's alpha for all combinations and store in matrix
results_matrix <- compute_cronbach_alpha(sample_data)

# Save the matrix to a CSV file
write.csv(results_matrix, "cronbach_alphas.csv", row.names = FALSE)

#################################### with 4 indicators

### A statistical test dependent on reliability estimates (Section 3.2)

####################################from https://github.com/svsteela/StructuralRejection/tree/main

# Data
z<-data$is_dead
x1<-data[[paste0(swls_suffixes[1], "_2")]]
x2<-data[[paste0(swls_suffixes[2], "_2")]]
x3<-data[[paste0(swls_suffixes[3], "_2")]]
x4<-data[[paste0(swls_suffixes[4], "_2")]]
x5<-data[[paste0(swls_suffixes[5], "_2")]]
x<-c(x1,x2,x3,x4,x5)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)),rep(5,length(x5)))
Z<-c(z,z,z,z,z)

### A statistical test dependent on reliability estimates (Section 3.2)

# Estimate reliabilities (Section 3.1)

cc = complete.cases(cbind(x1,x2,x3,x4,x5))
x1 = x1[cc]
x2 = x2[cc]
x3 = x3[cc]
x4 = x4[cc]
x5 = x5[cc]
z<-z[cc]

print(paste(var(x1),var(x2),var(x3),var(x4),var(x5), sep=" "))

A<-c(cov(x1,x2),cov(x1,x3),cov(x1,x4),cov(x1,x5),cov(x2,x3),cov(x2,x4),cov(x2,x5),cov(x3,x4),cov(x3,x5),cov(x4,x5))
B1<-c(1,1,1,1,0,0,0,0,0,0)
B2<-c(1,0,0,0,1,1,1,0,0,0)
B3<-c(0,1,0,0,1,0,0,1,1,0)
B4<-c(0,0,1,0,0,1,0,1,0,1)
B5<-c(0,0,0,1,0,0,1,0,1,1)
lambda = exp(glm(A~-1+B1+B2+B3+B4+B5,family = quasi(link="log"))$coef)

# Estimating functions for the estimation of reliabilities

u12<-(x1-mean(x1,na.rm=T))*(x2-mean(x2,na.rm=T))-lambda[1]*lambda[2]
u13<-(x1-mean(x1,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[1]*lambda[3]
u14<-(x1-mean(x1,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[1]*lambda[4]
u15<-(x1-mean(x1,na.rm=T))*(x5-mean(x5,na.rm=T))-lambda[1]*lambda[5]
u23<-(x2-mean(x2,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[2]*lambda[3]
u24<-(x2-mean(x2,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[2]*lambda[4]
u25<-(x2-mean(x2,na.rm=T))*(x5-mean(x5,na.rm=T))-lambda[2]*lambda[5]
u34<-(x3-mean(x3,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[3]*lambda[4]
u35<-(x3-mean(x3,na.rm=T))*(x5-mean(x5,na.rm=T))-lambda[3]*lambda[5]
u45<-(x4-mean(x4,na.rm=T))*(x5-mean(x5,na.rm=T))-lambda[4]*lambda[5]
u1<--lambda[2]*u12-lambda[3]*u13-lambda[4]*u14-lambda[5]*u15
u2<--lambda[1]*u12-lambda[3]*u23-lambda[4]*u24-lambda[5]*u25
u3<--lambda[1]*u13-lambda[2]*u23-lambda[4]*u34-lambda[5]*u35
u4<--lambda[1]*u14-lambda[2]*u24-lambda[3]*u34-lambda[5]*u45
u5<--lambda[1]*u15-lambda[2]*u25-lambda[3]*u35-lambda[4]*u45

# Fit constrained model to find initial parameter estimates

x<-c(x1,x2,x3,x4,x5)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)),rep(5,length(x5)))
Z<-rep(z[cc],times=5)
Zy<-Z*(rep(lambda,each=length(x1))/lambda[1])
mod<-lm(x~-1+factor(y)+Zy)
theta<-predict.lm(mod,newdata=data.frame(y=c(1,1,2,2,3,3,4,4,5,5),Zy=c(0,1,0,lambda[2]/lambda[1],0,lambda[3]/lambda[1],0,lambda[4]/lambda[1],0,lambda[5]/lambda[1])))

# Test statistic T_0 that pretends reliabilities are known 
# This test statistic is NOT TO BE USED, 
# but only to understand the impact of adjusting for the uncertainty in the reliabilities
# when comparing with the real test statistic in the next paragraph

mod<-lm(x~-1+factor(y)+Zy)

q<-function(theta){
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[6]),(1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[6]/lambda[1]),(1-z)*(x3-theta[3]),
           z*(x3-theta[3]-lambda[3]*theta[6]/lambda[1]),(1-z)*(x4-theta[4]),z*(x4-theta[4]-lambda[4]*theta[6]/lambda[1]),(1-z)*(x5-theta[5]),z*(x5-theta[5]-lambda[5]*theta[6]/lambda[1]))
  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(u))%*%t(t(g))
}
res <- nlm(q,p=mod$coef)
res$minimum # test statistic
1-pchisq(res$minimum,df=(5-1)*(2-1)) # p-value 

# Test statistic T_0 that recognizes reliabilities are estimated - Section 3.2

q<-function(theta){
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[6]),(1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[6]/lambda[1]),(1-z)*(x3-theta[3]),
           z*(x3-theta[3]-lambda[3]*theta[6]/lambda[1]),(1-z)*(x4-theta[4]),z*(x4-theta[4]-lambda[4]*theta[6]/lambda[1]),(1-z)*(x5-theta[5]),z*(x5-theta[5]-lambda[5]*theta[6]/lambda[1]))
  p<-mean(z)
  dudtheta<--diag(c(1-p,p,1-p,p,1-p,p,1-p,p,1-p,p))
  dthetadlambda<-(theta[6]/lambda[1])*cbind(c(rep(0,3),lambda[2]/lambda[1],0,lambda[3]/lambda[1],0,lambda[4]/lambda[1],0,lambda[5]/lambda[1]),
                                            c(rep(0,3),-1,rep(0,6)),c(rep(0,5),-1,rep(0,4)),c(rep(0,7),-1,rep(0,2)),c(rep(0,9),-1))
  dudlambda<-dudtheta%*%dthetadlambda
  dvdlambda<--cbind(c(-sum(lambda[-1]^2),mean(u12)-lambda[1]*lambda[2],mean(u13)-lambda[1]*lambda[3],mean(u14)-lambda[1]*lambda[4],mean(u15)-lambda[1]*lambda[5]),
                    c(mean(u12)-lambda[1]*lambda[2],-sum(lambda[-2]^2),mean(u23)-lambda[3]*lambda[2],mean(u24)-lambda[4]*lambda[2],mean(u25)-lambda[5]*lambda[2]),
                    c(mean(u13)-lambda[1]*lambda[3],mean(u23)-lambda[3]*lambda[2],-sum(lambda[-3]^2),mean(u34)-lambda[4]*lambda[3],mean(u35)-lambda[3]*lambda[5]),
                    c(mean(u14)-lambda[1]*lambda[4],mean(u24)-lambda[4]*lambda[2],mean(u34)-lambda[3]*lambda[4],-sum(lambda[-4]^2),mean(u45)-lambda[4]*lambda[5]),
                    c(mean(u15)-lambda[1]*lambda[5],mean(u25)-lambda[5]*lambda[2],mean(u35)-lambda[3]*lambda[5],mean(u45)-lambda[4]*lambda[5],-sum(lambda[-5]^2)))
  uadjust<-u-t(dudlambda%*%solve(dvdlambda)%*%rbind(u1,u2,u3,u4,u5))
  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(uadjust))%*%t(t(g))
}

mod<-lm(x~-1+factor(y)+Zy)
res <- nlm(q,p=mod$coef)
res$minimum
1-pchisq(res$minimum,df=(5-1)*(2-1)) # p-value 


#########################################################################
### A statistical test independent on reliability estimates (Section 3.3)


z1<-data$is_dead
z2<-data$exercise_level
z<-z1+2*z2
x1<-data[[paste0(swls_suffixes[1], "_1")]]
x2<-data[[paste0(swls_suffixes[2], "_1")]]
x3<-data[[paste0(swls_suffixes[3], "_1")]]
x4<-data[[paste0(swls_suffixes[4], "_1")]]
x5<-data[[paste0(swls_suffixes[5], "_1")]]

cc = complete.cases(cbind(x1,x2,x3,x4,x5,z))
x1 = x1[cc]
x2 = x2[cc]
x3 = x3[cc]
x4 = x4[cc]
x5 = x5[cc]
z = z[cc]

x<-c(x1,x2,x3,x4,x5)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)),rep(5,length(x5)))
Z<-c(z,z,z,z,z)
xc<-x-rep((x1+x2+x3+x4+x5)/5,5)
y1<-ifelse(y==1,1,0)-1/5
y2<-ifelse(y==2,1,0)-1/5
y3<-ifelse(y==3,1,0)-1/5
y4<-ifelse(y==4,1,0)-1/5
y5<-ifelse(y==5,1,0)-1/5

# Initial estimate of nuisance parameters (see Appendix)

alpha<-vector("numeric",length=5)
beta<-vector("numeric",length=4)
xalpha<-xc
valpha<-xc
xbeta<-xc
vbeta<-xc
for (i in 1:5){
  alpha[i]<-mean(xc[(y==i)&(Z==0)],na.rm=T)
  xalpha[y==i]<-xc[y==i]*alpha[i]
  valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
}

su<-10
max_iter <- 10000
iter <- 0
repeat{
  for (i in 0:3){
    beta[i+1]<-mean(xalpha[Z==i],na.rm=T)/mean(valpha[Z==i]^2,na.rm=T)
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  for (i in 1:5){
    alpha[i]<-mean(xbeta[y==i],na.rm=T)/mean(vbeta[y==i]^2,na.rm=T)
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  u<-vector("numeric",length=7)
  for (i in 1:5){
    u[i]<-sum((xbeta-valpha*vbeta^2)[y==i],na.rm=T)
  }
  for (j in 0:1){
    u[6+j]<-sum((xalpha-valpha^2*vbeta)[Z==j],na.rm=T)
  }
  if (abs(sum(u^2)-su)<1e-25) break
  su<-sum(u^2)
  cat(su,"\n")
  
  # max loops break
  iter <- iter + 1
  if (iter > max_iter) {
    cat("Reached maximum iterations. Loop stopped.\n")
    break
  }
}

# Improvement of initial estimate by minimising test statistic at fixed weight matrix

E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=5,byrow=T)
E<-E[-1,]
g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
gm<-apply(g,2,mean,na.rm=T)
W<-solve(var(g,na.rm=T))

# Final analysis

q<-function(theta){
  alpha<-theta[1:5]
  beta<-theta[6:9]
  for (i in 1:5){
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  for (i in 0:3){
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=5,byrow=T)
  E<-E[-1,]
  g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
  gm<-apply(g,2,mean,na.rm=T)
  length(z)*t(gm)%*%solve(var(g,na.rm=T))%*%t(t(gm))
}

res <- nlm(q,p=theta)
theta <- res$estimate

res
1-pchisq(res$minimum,df=(5-1)*(4-2)) # p-value 
