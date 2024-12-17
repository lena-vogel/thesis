library(haven)
library(dplyr)
library(ltm)


folder <- "csvs"
data <- read.csv(paste0(folder,"/data_test2.csv"))

########################################################################
### A statistical test dependent on reliability estimates (Section 3.2)
### from https://github.com/svsteela/StructuralRejection/tree/main

d <- 4

# Data
z <-data$is_dead
x1<-data$lb003b
x2<-data$lb003c
x3<-data$lb003d
x4<-data$lb003a
x<-c(x1,x2,x3,x4)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)))
Z<-c(z,z,z,z)

### A statistical test dependent on reliability estimates (Section 3.2)

# Estimate reliabilities (Section 3.1)
A<-c(cov(x1,x2),cov(x1,x3),cov(x1,x4),cov(x2,x3),cov(x2,x4),cov(x3,x4))
B1<-c(1,1,1,0,0,0)
B2<-c(1,0,0,1,1,0)
B3<-c(0,1,0,1,0,1)
B4<-c(0,0,1,0,1,1)
lambda = exp(glm(A~-1+B1+B2+B3+B4,family = quasi(link="log"))$coef)

# Estimating functions for the estimation of reliabilities

u12<-(x1-mean(x1,na.rm=T))*(x2-mean(x2,na.rm=T))-lambda[1]*lambda[2]
u13<-(x1-mean(x1,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[1]*lambda[3]
u14<-(x1-mean(x1,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[1]*lambda[4]
u23<-(x2-mean(x2,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[2]*lambda[3]
u24<-(x2-mean(x2,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[2]*lambda[4]
u34<-(x3-mean(x3,na.rm=T))*(x4-mean(x4,na.rm=T))-lambda[3]*lambda[4]
u1<--lambda[2]*u12-lambda[3]*u13-lambda[4]*u14
u2<--lambda[1]*u12-lambda[3]*u23-lambda[4]*u24
u3<--lambda[1]*u13-lambda[2]*u23-lambda[4]*u34
u4<--lambda[1]*u14-lambda[2]*u24-lambda[3]*u34

# Fit constrained model to find initial parameter estimates

x<-c(x1,x2,x3,x4)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)))
Z<-rep(z,times=d)
Zy<-Z*(rep(lambda,each=length(x1))/lambda[1])
mod<-lm(x~-1+factor(y)+Zy)
theta<-predict.lm(mod,newdata=data.frame(y=c(1,1,2,2,3,3,4,4),Zy=c(0,1,
                                                                   0,lambda[2]/lambda[1],
                                                                   0,lambda[3]/lambda[1],
                                                                   0,lambda[4]/lambda[1])))
# Test statistic T_0 that pretends reliabilities are known 
# This test statistic is NOT TO BE USED, 
# but only to understand the impact of adjusting for the uncertainty in the reliabilities
# when comparing with the real test statistic in the next paragraph

mod<-lm(x~-1+factor(y)+Zy)

q<-function(theta){
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[d+1]),(1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[d+1]/lambda[1]),(1-z)*(x3-theta[3]),z*(x3-theta[3]-lambda[3]*theta[d+1]/lambda[1]),(1-z)*(x4-theta[4]),z*(x4-theta[4]-lambda[4]*theta[d+1]/lambda[1]))
  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(u))%*%t(t(g))
}
res <- nlm(q,p=mod$coef)
res$minimum # test statistic
1-pchisq(res$minimum,df=(d-1)*(2-1)) # p-value 

# Test statistic T_0 that recognizes reliabilities are estimated - Section 3.2
q<-function(theta){
  u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[d+1]),
           (1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[d+1]/lambda[1]),
           (1-z)*(x3-theta[3]),z*(x3-theta[3]-lambda[3]*theta[d+1]/lambda[1]),
           (1-z)*(x4-theta[4]),z*(x4-theta[4]-lambda[4]*theta[d+1]/lambda[1]))
  p<-mean(z)
  dudtheta<--diag(c(1-p,p,1-p,p,1-p,p,1-p,p))
  dthetadlambda<-(theta[d+1]/lambda[1])*cbind(c(0,0,0,
                                              lambda[2]/lambda[1],0,
                                              lambda[3]/lambda[1],0,
                                              lambda[4]/lambda[1]),
                                            c(0,0,0,-1,0,0,0,0),
                                            c(0,0,0,0,0,-1,0,0),
                                            c(0,0,0,0,0,0,0,-1)) 
  dudlambda<-dudtheta%*%dthetadlambda
  dvdlambda<--cbind(c(-sum(lambda[-1]^2),mean(u12)-lambda[1]*lambda[2],mean(u13)-lambda[1]*lambda[3],mean(u14)-lambda[1]*lambda[4]),
                    c(mean(u12)-lambda[1]*lambda[2],-sum(lambda[-2]^2),mean(u23)-lambda[3]*lambda[2],mean(u24)-lambda[4]*lambda[2]),
                    c(mean(u13)-lambda[1]*lambda[3],mean(u23)-lambda[3]*lambda[2],-sum(lambda[-3]^2),mean(u34)-lambda[4]*lambda[3]),
                    c(mean(u14)-lambda[1]*lambda[4],mean(u24)-lambda[4]*lambda[2],mean(u34)-lambda[3]*lambda[4],-sum(lambda[-4]^2)))
  uadjust<-u-t(dudlambda%*%solve(dvdlambda)%*%rbind(u1,u2,u3,u4))

  g<-apply(u,2,mean)
  length(z)*t(g)%*%solve(var(uadjust))%*%t(t(g))
}

mod<-lm(x~-1+factor(y)+Zy)
res <- nlm(q,p=mod$coef)
res
1-pchisq(res$minimum,df=(d-1)*(2-1)) # p-value 


##############################################################################
### A statistical test independent on reliability estimates (Section 3.3)

p <- 4 #nb of levels of Z 
  
z1<-data$is_dead
z2<-data$exercise_level
z <-z1+2*z2
x1<-data$lb003a
x2<-data$lb003b
x3<-data$lb003c
x4<-data$lb003d

x<-c(x1,x2,x3,x4)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)))
Z<-c(z,z,z,z)
xc<-x-rep((x1+x2+x3+x4)/d,d)
y1<-ifelse(y==1,1,0)-1/d
y2<-ifelse(y==2,1,0)-1/d
y3<-ifelse(y==3,1,0)-1/d
y4<-ifelse(y==4,1,0)-1/d

# Initial estimate of nuisance parameters (see Appendix)

alpha<-vector("numeric",length=d)
beta<-vector("numeric",length=p)
xalpha<-xc
valpha<-xc
xbeta<-xc
vbeta<-xc
for (i in 1:d){
  alpha[i]<-mean(xc[(y==i)&(Z==0)],na.rm=T)
  xalpha[y==i]<-xc[y==i]*alpha[i]
  valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
}

su<-10
max_iter <- 1000
iter <- 0
repeat{
  for (i in 0:(p-1)){
    beta[i+1]<-mean(xalpha[Z==i],na.rm=T)/mean(valpha[Z==i]^2,na.rm=T)
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  for (i in 1:d){
    alpha[i]<-mean(xbeta[y==i],na.rm=T)/mean(vbeta[y==i]^2,na.rm=T)
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  u<-vector("numeric",length=d+2)
  for (i in 1:d){
    u[i]<-sum((xbeta-valpha*vbeta^2)[y==i],na.rm=T)
  }
  for (j in 0:1){
    u[d+1+j]<-sum((xalpha-valpha^2*vbeta)[Z==j],na.rm=T)
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

E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=d,byrow=T)
E<-E[-1,]
g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
gm<-apply(g,2,mean,na.rm=T)
W<-solve(var(g,na.rm=T))

# Final analysis

q<-function(theta){
  alpha<-theta[1:d]
  beta<-theta[(d+1):(d+p)]
  for (i in 1:d){
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  for (i in 0:(p-1)){
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=d,byrow=T)
  E<-E[-1,]
  g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
  gm<-apply(g,2,mean,na.rm=T)
  length(z)*t(gm)%*%solve(var(g,na.rm=T))%*%t(t(gm))
}

res <- nlm(q,p=theta)
theta <- res$estimate

res
1-pchisq(res$minimum,df=(d-1)*(p-2)) # p-value 
