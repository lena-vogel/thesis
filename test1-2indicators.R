library(haven)
library(dplyr)
library(ltm)

data_2010 <- read.csv("csvs/data_2010.csv")
data_2012 <- read.csv("csvs/data_2012.csv")

# concatenate data of the 2 groups
data1 <- bind_rows(data_2010, data_2012)

########################################################################
### A statistical test dependent on reliability estimates (Section 3.2)
### from https://github.com/svsteela/StructuralRejection/tree/main

# Data
z<-data1$is_dead
x1<-data1$lb003a
x2<-data1$lb003c

x<-c(x1,x2)
y<-c(rep(1,length(x1)),rep(2,length(x2)))
Z<-c(z,z)

### A statistical test dependent on reliability estimates (Section 3.2)

# Estimate reliabilities (Section 3.1)
A<-c(cov(x1,x2))
B1<-c(1)
B2<-c(1)
lambda = exp(glm(A~-1+B1+B2,family = quasi(link="log"))$coef)

# Estimating functions for the estimation of reliabilities

u12<-(x1-mean(x1,na.rm=T))*(x2-mean(x2,na.rm=T))-lambda[1]*lambda[2]
u1<--lambda[2]*u12
u2<--lambda[1]*u12

# Fit constrained model to find initial parameter estimates

x<-c(x1,x2)
y<-c(rep(1,length(x1)),rep(2,length(x2)))
y <- factor(c(rep(1, length(x1)), rep(2, length(x2))), exclude = NULL)
Z<-rep(z,times=2)
Zy<-Z*(rep(lambda,each=length(x1))/lambda[1])

complete_cases <- complete.cases(x, y, Zy)
x <- x[complete_cases]
y <- y[complete_cases]
Zy <- Zy[complete_cases]

unique(Zy)  # Should return "1" and "2"
mod<-lm(x~-1+factor(y)+Zy)
theta<-predict.lm(mod,newdata=data.frame(y=c(1,1,2,2),Zy=c(0,1,
                                                           0,lambda[2]/lambda[1])))
print(theta)
# Test statistic T_0 that pretends reliabilities are known 
# This test statistic is NOT TO BE USED, 
# but only to understand the impact of adjusting for the uncertainty in the reliabilities
# when comparing with the real test statistic in the next paragraph

mod<-lm(x~-1+factor(y)+Zy)

q<-function(theta){
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[3]),(1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[3]/lambda[1]))
  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(u))%*%t(t(g))
}
res <- nlm(q,p=mod$coef)
res$minimum # test statistic
1-pchisq(res$minimum,df=(2-1)*(2-1)) # p-value 

# Test statistic T_0 that recognizes reliabilities are estimated - Section 3.2
print(theta)
q<-function(theta){
  print(paste0("Theta: ",theta))
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[3]),
           (1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[3]/lambda[1]))
  p<-mean(z)
  dudtheta<--diag(c(1-p,p,1-p,p))
  print(theta[3])
  dthetadlambda<-(theta[3]/lambda[1])*cbind(c(0,0,
                                              0,lambda[2]/lambda[1]
                                              ),
                                            c(0,0,0,-1,),
                                            c(0,0,0,0)) ###### A voir quels 0 mettre où ???
  dudlambda<-dudtheta%*%dthetadlambda
  #print(paste0("Dthetalambda: ",dthetadlambda)) #
  
  dvdlambda<--cbind(c(-sum(lambda[-1]^2),mean(u12)-lambda[1]*lambda[2]),
                    c(mean(u12)-lambda[1]*lambda[2],-sum(lambda[-2]^2)))
  uadjust<-u-t(dudlambda%*%solve(dvdlambda)%*%rbind(u1,u2))
  #print(paste0("Uadjust: ",uadjust)) #

  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(uadjust))%*%t(t(g))
}

mod<-lm(x~-1+factor(y)+Zy)
res <- nlm(q,p=mod$coef)
res
1-pchisq(res$minimum,df=(2-1)*(2-1)) # p-value 

