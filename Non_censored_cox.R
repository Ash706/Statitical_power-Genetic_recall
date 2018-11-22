#This code is used for cox regression simulations for non censored data

x <- (1:30)/20 - 3 # create the covariates, 30 of them
myrates <- exp(3*x+1) # the risk exp(beta*x), parameters for exp r.v
y <- rexp(30, rate = myrates) # generates the r.v.
survreg(Surv(y,rep(1,30))~x,dist="weibull")$coef[2]
#slope estimate by weibull regression
#we do not have any censoring in the data
coxph(Surv(y,rep(1,30))~x)$coef # estimate from Cox regression

Simu2reg <- function(x , inputrates){
  y <- rexp(length(inputrates), rate=inputrates)
  temp1 <- survreg(Surv(y, rep(1, length(y)))~x,dist="weibull")
  temp2 <- coxph(Surv(y, rep(1, length(y)))~x)
  return(c(temp1$coef[2], temp2$coef))
}
result <- matrix(NA, nrow=2, ncol=5000) #creat a matrix to hold outcome
for(i in 1:100) result[,i]<-Simu2reg(x,myrates) #run the simulation 100 times
#for(i in 101:500) result[,i]<-Simu2reg(x,myrates) #run the simulation 400 times
mean(result[1,])
mean(result[2,])
sd(result[1,])
sd(result[2,])
hist(result[1,], xlim=c(0,6))
hist(result[2,], xlim=c(0,6))