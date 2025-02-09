## Risk ratio estimation with Poisson GLM using grouped data

# to get help on GLMs in R (type q to exit)
?glm
?family

# rats data is in the survival package
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)     # rats data
library(sandwich)     # sandwich() for robust variance
library(lmtest)       # coefci() for Wald confidence intervals

# traditional RR point estimate and Wald 95% confidence interval
# Because follow-up of the rats was complete, risk calculations are valid.
r1 <- sum(rats$rx == 1)
a <- with(rats, sum(status[rx == 1]))
r0 <- sum(rats$rx == 0)
c <- with(rats, sum(status[rx == 0]))
risk1 <- a / r1
risk0 <- c / r0
RRhat <- (a / r1) / (c / r0)
lnRRvar <- 1 / a - 1 / r1 + 1 / c - 1 / r0
lnRRci <- log(RRhat) + c(-1, 1) * qnorm(.975) * sqrt(lnRRvar)

# point and interval estimates of the RR
RRhat
exp(lnRRci)

# Binomial GLM with log link (log-binomial model) for the RR
# Default binomial link = "logit", so we must specify link = "log".
RRglm <- glm(status ~ rx, family = binomial(link = "log"), data = rats)
exp(coef(RRglm))                # estimated risk (x = 0) and RR
exp(confint(RRglm))             # likelihood ratio CIs (better)
exp(coefci(RRglm))              # log-transformed Wald CI for RD matches above

# point and interval estimates for risk in the exposed
# The vector c(1, 1) represents 1 * beta0 + 1 * beta1,
# and as.numeric() is used to return a number instead of a matrix.
p1hat <- exp(sum(coef(RRglm)))
lnp1var <- as.numeric(c(1, 1) %*% vcov(RRglm) %*% c(1, 1))
p1hat
p1hat * exp(c(-1, 1) * qnorm(0.975) * sqrt(lnp1var))

# Poisson regression using counts in each exposure group
# Make data frame with counts and total rat-time in each treatment group.
rats$group <- rep(1:100, times = 300 / 100)
countdat <- data.frame(rx = c(0, 1, 0, 1),
                       count = as.vector(by(rats$status, list(rats$rx, rats$litter), sum)),
                       nrats = as.vector(by(rep(1, nrow(rats)), list(rats$rx, rats$litter), sum)))
RRcount <- glm(count ~ rx + offset(log(nrats)),
               family = poisson(), data = countdat)
summary(RRcount)                  # point estimates, p-values, CIs, global tests
names(RRcount)                    # parts of the fitted model
exp(coef(RRcount))       # point estimates identical to IRRglm
exp(confint(RRcount))    # likelihood ratio CIs are too wide
exp(coefci(RRcount))     # Wald CIs are also too wide
exp(coefci(RRcount, vcov = sandwich))   # robust Wald CI fails

countdat <- data.frame(rx = c(0, 1, 0, 1),
                       count = as.vector(by(rats$status, list(rats$rx, rats$sex), sum)),
                       nrats = as.vector(by(rep(1, nrow(rats)), list(rats$rx, rats$sex), sum)))
RRcount <- glm(count ~ rx + offset(log(nrats)),
               family = poisson(), data = countdat)
summary(RRcount)                  # point estimates, p-values, CIs, global tests
names(RRcount)                    # parts of the fitted model
exp(coef(RRcount))       # point estimates identical to IRRglm
exp(confint(RRcount))    # likelihood ratio CIs are too wide
exp(coefci(RRcount))     # Wald CIs are also too wide
exp(coefci(RRcount, vcov = sandwich))   # robust Wald CI ma
