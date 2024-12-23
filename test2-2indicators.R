folder <- "csvs"
data <- read.csv(paste0(folder,"/data_test2.csv"))

########################################################################
### A statistical test independent on reliability estimates (Section 3.3)
### from https://github.com/svsteela/StructuralRejection/tree/main

d <- 2
p_levels <- 4

z1<-data$is_dead
z2<-data$exercise_level
z <-z1+2*z2
x1<-data$lb003a
x2<-data$lb003b

x<-c(x1,x2)
y<-c(rep(1,length(x1)),rep(2,length(x2)))
Z<-c(z,z)
xc<-x-rep((x1+x2)/d,d)
y1<-ifelse(y==1,1,0)-1/d
y2<-ifelse(y==2,1,0)-1/d


# Initial estimate of nuisance parameters (see Appendix)

alpha<-vector("numeric",length=d)
beta<-vector("numeric",length=p_levels)
xalpha<-xc
valpha<-xc
xbeta<-xc
vbeta<-xc
for (i in 1:d){
  alpha[i]<-mean(xc[(y==i)&(Z==0)],na.rm=T)
  xalpha[y==i]<-xc[y==i]*alpha[i]
  valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
}
theta <- c(rep(0.5, d), rep(0.5, p_levels))  ################ added !!!!!

su<-10
iter <- 0
max_iter <- 100
repeat{
  for (i in 0:(p_levels-1)){
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
  
  # max loops break
  iter <- iter + 1
  if (iter > max_iter) {
    cat("Reached maximum iterations. Loop stopped.\n")
    break
  }
}

# Improvement of initial estimate by minimising test statistic at fixed weight matrix

E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=d,byrow=T)
if (d > 2) {E <- E[-1, ]} #### change of dimension if that works otherwise not
g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
gm<-apply(g,2,mean,na.rm=T)
W <- solve(var(g[c('0','2','4','6')], na.rm = T))

q<-function(theta){
  print(theta)
  alpha<-theta[1:d]
  beta<-theta[(d+1):(d+p_levels)]
  for (i in 1:d){
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  for (i in 0:(p_levels-1)){
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=d,byrow=T)
  E<-E[-1,]
  g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
  gm<-apply(g,2,mean,na.rm=T)
  length(z)*t(gm)%*%W%*%t(t(gm))
}

res <- nlm(q,p=theta)
theta <- res$estimate

# Final analysis

q<-function(theta){
  alpha<-theta[1:d]
  beta<-theta[(d+1):(d+p_levels)]
  for (i in 1:d){
    xalpha[y==i]<-xc[y==i]*alpha[i]
    valpha[y==i]<-ifelse(is.na(xc[y==i]),NA,alpha[i])
  }
  for (i in 0:(p_levels-1)){
    xbeta[Z==i]<-xc[Z==i]*beta[i+1]
    vbeta[Z==i]<-ifelse(is.na(xc[Z==i]),NA,beta[i+1])
  }
  E<-matrix(xc-valpha*vbeta,ncol=length(x1),nrow=d,byrow=T)
  E<-E[-1,]
  g<-t(rbind(t(ifelse(z==0,1,0)*t(E)),t(ifelse(z==1,1,0)*t(E)),t(ifelse(z==2,1,0)*t(E)),t(ifelse(z==3,1,0)*t(E))))
  gm<-apply(g,2,mean,na.rm=T)
  length(z)*t(gm)%*%solve(var(g,na.rm=T))%*%t(t(gm))
}

#print(W)
#print(beta)
#print(alpha)
#print(theta)
#print(dim(g))
#print(var(g, na.rm = T))
#print(E)
#print(det(var(g, na.rm = T)))

res <- nlm(q,p=theta)
res
p3 <- 1-pchisq(res$minimum,df=(d-1)*(p_levels-2)) # p-value 

cat("For this set of indicators, the p-values are:\n",
    #sprintf("%.3e", p1), "\n",
    #sprintf("%.3e", p2), "\n",
    sprintf("%.3e", p3), "\n")