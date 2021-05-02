# inverse probability method


# baseline hazard: Weibull

# N  sample size    
# lambda = scale parameter in h0()
# rho = shape parameter in h0() 1 = exponential
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

simulWeib <- function(N, lambda, rho, beta, C){
  # covariate --> N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))

  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(x * beta)))^(1 / rho)

  # censoring times
  #C <- rexp(n=N, rate=rateC)

  # follow-up times and event indicators
  # administrative censoring at C years
  time <- pmin(Tlat, C)
  
  status <- as.numeric(Tlat <= C)

  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x=x)
}


# Simulate data with a 10% 5-year survival rate for control (assuming an expontial survival , and a hazard ratio of 0.5
prop2haz <- function(ST0,T0){
    h = -log(ST0)/T0   
    h
}

basehaz <- prop2haz(0.1,5)

beta.test <- log(0.5)


nsims <- 1000
n <- 100 # total sample size
# assess the power to detect the 0.5 hazard ratio
library(survival)
set.seed(1234)
p <- rep(NA, nsims)
for(i in 1:nsims) {
  dat <- simulWeib(N=n, lambda=basehaz, rho=1, beta=beta.test, C=5)
  fit <- coxph(Surv(time, status) ~ x, data=dat)
  p[i] <- summary(fit)$coef[5]
}


mean(p<0.05)


