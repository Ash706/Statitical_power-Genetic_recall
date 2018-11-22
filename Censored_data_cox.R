#Cox regression for censored data
x<- (1:30)/2 - 3 # create the covariates, 30 of them
myrates <- exp(3*x+1) # the risk exp(beta*x), parameters for exp r.v.
y <- rexp(30, rate = myrates) # generates the r.v.
cen <- rexp(30, rate = 0.5 )
ycen <- pmin(y, cen)
di <- as.numeric(y <= cen)
#The inference has to be based on x, ycen, di
survreg(Surv(ycen, di)~x, dist="weibull")$coef[2]
coxph(Surv(ycen, di)~x)$coef
#Or putting this into a function as before:
  Simu2regC <- function(x , inputrates){
    y <- rexp(length(inputrates), rate=inputrates)
    cen <- rexp(length(inputrates), rate= mean(inputrates)/2 )
    obs <- pmin(y, cen)
    di <- as.numeric(y <= cen)
    temp1 <- survreg(Surv(obs, di)~x, dist="weibull")
    temp2 <- coxph(Surv(obs, di)~x)
    return(c(temp1$coef[2], temp2$coef))
  }