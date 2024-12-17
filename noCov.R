library(haven)
library(dplyr)
library(ltm)

folder <- "csvs"
data2 <- read.csv(paste0(folder,"/data_test2.csv"))

### A statistical test dependent on reliability estimates (Section 3.2)

####################################from https://github.com/svsteela/StructuralRejection/tree/main

# Data
z<-data2$is_dead
x1<-data2$lb003a
x2<-data2$lb003b
x3<-data2$lb003c
x4<-data2$lb003d
x5<-data2$lb003e
x<-c(x1,x2,x3,x4,x5)
y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)),rep(5,length(x5)))
Z<-c(z,z,z,z,z)

### A statistical test dependent on reliability estimates (Section 3)

# Estimate reliabilities (Section 3.1)
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
Z<-rep(z,times=5)
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


##############################################################################
### A statistical test independent on reliability estimates (Section 3.3)

z1<-data2$is_dead
z2<-data2$exercise_level
z<-z1+2*z2
x1<-data2$lb003a
x2<-data2$lb003b
x3<-data2$lb003c
x4<-data2$lb003d
x5<-data2$lb003e

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
theta <- c(rep(1, 5), rep(1, 4))
su<-10
max_iter <- 100000
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

res2 <- nlm(q,p=theta)
theta <- res2$estimate
print(theta)
res
1-pchisq(res$minimum,df=(5-1)*(4-2)) # p-value 

