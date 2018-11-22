##############################################################################
# R program demonstrating simulation to obtain power for Cox regression 
# model based on specified parameters
##############################################################################

# Initialize parameters
# n is sample size for both groups, observations in in simulated data set
# m1 is mean for group 1 survival time distribution
# m2 is mean for group 2 survival time distribution
# cm is mean for censoring time distribution
# M is number of simulations to perform
# a is alpha level of test

n<-1000
m1<-0.5
m2<-0.6
cm<-1.0
M<-1000
a<-0.05
###############################################################################

# Create vector to receive results
reject<-NULL
# Simulation loop
for(i in 1:M) {
  
  
  # Create group indicator
  x<-c( rep(0,n) , rep(1,n) )
  
  # Generate group 1 and group 2 complete survival times.
  y1<-rexp(n, rate=1/m1)
  y2<-rexp(n, rate=1/m2)
  y<-c(y1,y2)
  # print(paste("Mean of Group 1 survival time: ", mean(y1)))
  # print(paste("Mean of Group 2 survival time: ", mean(y2)))
  
  # Generate censoring times
  cen<-rexp(2*n, rate=1/cm)
  
  # Create observed censored survival time
  ycen<-pmin(y,cen)
  
  # Create censoring indicator, 0 for censored (y>cen), 1 for complete (y<=cen)
  censored<-as.numeric(y<=cen)
  
  # Fit Cox model
  out<-coxph(formula = Surv(ycen, censored) ~ x)
  # print(out)
  # print(paste("Estimated relative rate from Cox regression: ", exp(-out$coef)))
  pvaluei<- 2*(1-pnorm(sqrt(out$wald.test))) # Obtain p-value from Wald test
  
  rejecti<-ifelse(pvaluei<a,1,0)  # Determine if test rejects
  reject<-c(reject,rejecti)  # Save result
}
print(paste("Estimated Power of Test: ", mean(reject)))   
