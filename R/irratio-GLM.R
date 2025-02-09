## Incidence rate ratio estimation with a Poisson GLM

# to get help on GLMs in R (type q to exit)
?glm
?family

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)     # rats data
library(lmtest)       # coefci() for Wald confidence intervals

# numbers of events and total rat-weeks
# The subset() function can be used as an alternative to vector indexing.
a <- sum(rats$status[rats$rx == 1])
T1 <- sum(rats$time[rats$rx == 1])
c <- with(subset(rats, rx == 0), sum(status))
T0 <- with(subset(rats, rx == 0), sum(time))
ir1hat <- a / T1
ir0hat <- c / T0
IRRhat <- ir1hat / ir0hat
lnIRRvar <- 1 / a + 1 / c
lnIRRci <- log(IRRhat) + c(-1, 1) * qnorm(0.975) * sqrt(lnIRRvar)

# point and interval estimate of the IRR
IRRhat
exp(lnIRRci)

# Poisson GLM with log(rat-weeks) offset to get incidence rate ratio
# The default link = "log".
IRRglm <- glm(status ~ rx  + offset(log(time)), family = poisson(), data = rats)
summary(IRRglm)                 # point estimates, p-values, CIs, global tests
names(IRRglm)                   # parts of the fitted model
exp(coef(IRRglm))               # estimated incidence rate (x = 0) and IRR
exp(confint(IRRglm))            # likelihood ratio CIs (better)
exp(coefci(IRRglm))             # log-transformed Wald CI for IRR matches above

# point and interval estimates for incidence rate in the exposed
# The vector c(1, 1) represents 1 * beta0 + 1 * beta1,
# and as.numeric() is used to return a number instead of a matrix.
rate1hat <- exp(sum(coef(IRRglm)))
lnrate1var <- as.numeric(c(1, 1) %*% vcov(IRRglm) %*% c(1, 1))
rate1hat
rate1hat * exp(c(-1, 1) * qnorm(0.975) * sqrt(lnrate1var))

# Poisson regression using counts in each exposure group
# Make data frame with counts and total rat-time in each treatment group.
countdat <- data.frame(rx = c(0, 1),
                       count = as.vector(by(rats$status, rats$rx, sum)),
                       time = as.vector(by(rats$time, rats$rx, sum)))
IRRcount <- glm(count ~ rx + offset(log(time)),
                family = poisson(), data = countdat)
exp(coef(IRRcount))       # point estimates identical to IRRglm
exp(confint(IRRcount))    # likelihood ratio CIs identical to IRRglm
exp(coefci(IRRcount))     # Wald CIs also identical to IRRglm