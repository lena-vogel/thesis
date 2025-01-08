########################################################################
### A statistical test dependent on reliability estimates (Section 3.2)
### from https://github.com/svsteela/StructuralRejection/tree/main
### contains functions for sets of 3, 4 and 5 indicators, with 1 outcome.

stde_3_indicators <- function(combination,z) {
  # Data
  x1 <- combination[,1]
  x2 <- combination[,2]
  x3 <- combination[,3]

  d <- 3
  x<-c(x1,x2,x3)
  y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)))
  Z<-c(rep(z),d)
  
  # Estimate reliabilities (Section 3.1)
  A<-c(cov(x1,x2),cov(x1,x3),cov(x2,x3))
  B1<-c(1,1,0)
  B2<-c(1,0,1)
  B3<-c(0,1,1)
  lambda = exp(glm(A~-1+B1+B2+B3,family = quasi(link="log"))$coef)
  diff <- (max(lambda)-min(lambda))/mean(lambda)
  
  # Estimate functions for the estimation of reliabilities
  u12<-(x1-mean(x1,na.rm=T))*(x2-mean(x2,na.rm=T))-lambda[1]*lambda[2]
  u13<-(x1-mean(x1,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[1]*lambda[3]
  u23<-(x2-mean(x2,na.rm=T))*(x3-mean(x3,na.rm=T))-lambda[2]*lambda[3]
  u1<--lambda[2]*u12-lambda[3]*u13
  u2<--lambda[1]*u12-lambda[3]*u23
  u3<--lambda[1]*u13-lambda[2]*u23
  
  # Fit constrained model to find initial parameter estimates
  x<-c(x1,x2,x3)
  y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)))
  Z<-rep(z,times=d)
  Zy<-Z*(rep(lambda,each=length(x1))/lambda[1])
  unique(Zy)  # Should return "1" and "2"
  mod<-lm(x~-1+factor(y)+Zy)
  theta<-predict.lm(mod,newdata=data.frame(y=c(1,1,2,2,3,3),Zy=c(0,1,
                                                                 0,lambda[2]/lambda[1],
                                                                 0,lambda[3]/lambda[1])))
  
  # Test statistic T_0 that recognizes reliabilities are estimated - Section 3.2
  q<-function(theta){
    u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[d+1]),
             (1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[d+1]/lambda[1]),
             (1-z)*(x3-theta[3]),z*(x3-theta[3]-lambda[3]*theta[d+1]/lambda[1]))
    p<-mean(z)
    dudtheta<--diag(c(1-p,p,1-p,p,1-p,p))
    dthetadlambda<-(theta[4]/lambda[1])*cbind(c(0,0,
                                                0,lambda[2]/lambda[1],
                                                0,lambda[3]/lambda[1]
    ),
    c(0,0,0,-1,0,0),
    c(0,0,0,0,0,-1)) ###### A voir quels 0 mettre où ???
    dudlambda<-dudtheta%*%dthetadlambda
    
    dvdlambda<--cbind(c(-sum(lambda[-1]^2),mean(u12)-lambda[1]*lambda[2],mean(u13)-lambda[1]*lambda[3]),
                      c(mean(u12)-lambda[1]*lambda[2],-sum(lambda[-2]^2),mean(u23)-lambda[3]*lambda[2]),
                      c(mean(u13)-lambda[1]*lambda[3],mean(u23)-lambda[3]*lambda[2],-sum(lambda[-3]^2)))
    uadjust<-u-t(dudlambda%*%solve(dvdlambda)%*%rbind(u1,u2,u3))
    
    g<-apply(u,2,mean)
    length(z)*t(g)%*%solve(var(uadjust))%*%t(t(g))
  }
  
  mod<-lm(x~-1+factor(y)+Zy)
  res <- nlm(q,p=mod$coef)
  return(c(1-pchisq(res$minimum,df=(d-1)*(2-1)), diff)) # p-value 
}


##########################################################################################################
stde_4_indicators <- function(combination,z) {
  x1 <- combination[,1]
  x2 <- combination[,2]
  x3 <- combination[,3]
  x4 <- combination[,4]
  
  d <- 4 # number of indicators X_i's
  
  # Data
  x<-c(x1,x2,x3,x4)
  y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)))
  Z<-c(rep(z),d)
  
  # Estimate reliabilities (Section 3.1)
  A<-c(cov(x1,x2),cov(x1,x3),cov(x1,x4),cov(x2,x3),cov(x2,x4),cov(x3,x4))
  B1<-c(1,1,1,0,0,0)
  B2<-c(1,0,0,1,1,0)
  B3<-c(0,1,0,1,0,1)
  B4<-c(0,0,1,0,1,1)
  lambda = exp(glm(A~-1+B1+B2+B3+B4,family = quasi(link="log"))$coef)
  diff <- (max(lambda)-min(lambda))/mean(lambda)
  
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
  return(c(1-pchisq(res$minimum,df=(d-1)*(2-1)), diff)) # p-value 
}

##########################################################################
stde_5_indicators <- function(combination,z) {
  # Data
  x1 <- combination[,1]
  x2 <- combination[,2]
  x3 <- combination[,3]
  x4 <- combination[,4]
  x5 <- combination[,5]
  
  d <- 5
    
  x<-c(x1,x2,x3,x4,x5)
  y<-c(rep(1,length(x1)),rep(2,length(x2)),rep(3,length(x3)),rep(4,length(x4)),rep(5,length(x5)))
  Z<-c(rep(z),d)
  
  # Estimate reliabilities (Section 3.1)
  A<-c(cov(x1,x2),cov(x1,x3),cov(x1,x4),cov(x1,x5),cov(x2,x3),cov(x2,x4),cov(x2,x5),cov(x3,x4),cov(x3,x5),cov(x4,x5))
  B1<-c(1,1,1,1,0,0,0,0,0,0)
  B2<-c(1,0,0,0,1,1,1,0,0,0)
  B3<-c(0,1,0,0,1,0,0,1,1,0)
  B4<-c(0,0,1,0,0,1,0,1,0,1)
  B5<-c(0,0,0,1,0,0,1,0,1,1)
  lambda = exp(glm(A~-1+B1+B2+B3+B4+B5,family = quasi(link="log"))$coef)
  diff <- (max(lambda)-min(lambda))/mean(lambda)
  
  # Estimate functions for the estimation of reliabilities
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
  
  # Test statistic T_0 that recognizes reliabilities are estimated - Section 3.2
  q<-function(theta){
    u<-cbind((1-z)*(x1-theta[1]),z*(x1-theta[1]-theta[6]),(1-z)*(x2-theta[2]),z*(x2-theta[2]-lambda[2]*theta[6]/lambda[1]),(1-z)*(x3-theta[3]),z*(x3-theta[3]-lambda[3]*theta[6]/lambda[1]),(1-z)*(x4-theta[4]),z*(x4-theta[4]-lambda[4]*theta[6]/lambda[1]),(1-z)*(x5-theta[5]),z*(x5-theta[5]-lambda[5]*theta[6]/lambda[1]))
    p<-mean(z)
    dudtheta<--diag(c(1-p,p,1-p,p,1-p,p,1-p,p,1-p,p))
    dthetadlambda<-(theta[6]/lambda[1])*cbind(c(rep(0,3),
                                                lambda[2]/lambda[1],0,
                                                lambda[3]/lambda[1],0,
                                                lambda[4]/lambda[1],0,
                                                lambda[5]/lambda[1]),
                                              c(rep(0,3),-1,rep(0,6)),
                                              c(rep(0,5),-1,rep(0,4)),
                                              c(rep(0,7),-1,rep(0,2)),
                                              c(rep(0,9),-1))
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
  return(c(1-pchisq(res$minimum,df=(d-1)*(2-1)), diff)) # p-value 
}
