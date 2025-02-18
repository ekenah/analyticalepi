## Risk difference estimation with GLMs

# to get help on GLMs in R (type q to exit)
?glm
?family

# load packages
# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)     # rats data
library(sandwich)     # sandwich() for robust variance
library(lmtest)       # coefci() for Wald confidence intervals

# traditional RD point estimate and Wald 95% confidence interval
# Because follow-up of the rats was complete, risk calculations are valid.
r1 <- sum(rats$rx == 1)
a <- with(rats, sum(status[rx == 1]))
r0 <- sum(rats$rx == 0)
c <- with(rats, sum(status[rx == 0]))
risk1 <- a / r1
risk0 <- c / r0
RDhat <- risk1 - risk0
RDvar <- risk1 * (1 - risk1) / r1 + risk0 * (1 - risk0) / r0

# point and interval estimates of the RD
RDhat
RDhat + c(-1, 1) * qnorm(0.975) * sqrt(RDvar)

# Binomial GLM with identity link for the RD
# Default binomial link = "logit", so we must specify link = "identity".
RDglm <- glm(status ~ rx, family = binomial(link = "identity"), data = rats)
summary(RDglm)                  # point estimates, p-values, CIs, global tests
names(RDglm)                    # parts of the fitted model
coef(RDglm)                     # estimated risk (x = 0) and RD
confint(RDglm)                  # likelihood ratio CIs (better)
coefci(RDglm)                   # Wald CI for RD matches calculation above

# Gaussian (normal) GLM with identity link and robust variance for the RD
RDglm2 <- glm(status ~ rx, data = rats)
coef(RDglm2)                    # point estimates match binomial GLM above
confint(RDglm2)                 # likelihood ratio CI too narrow
coefci(RDglm2)                  # Wald CI also too narrow
coefci(RDglm2, vcov = sandwich) # robust Wald CIs match binomial GLM above

# point and interval estimates for risk in the exposed
# The vector c(1, 1) represents 1 * beta0 + 1 * beta1,
# and as.numeric() is used to return a number instead of a matrix.
p1hat <- sum(coef(RDglm))
p1var <- as.numeric(c(1, 1) %*% vcov(RDglm) %*% c(1, 1))
p1hat
p1hat + c(-1, 1) * qnorm(0.975) * sqrt(p1var)

# Gaussian GLM with robust variance
sum(coef(RDglm2))                         # matches p1hat above
c(1, 1) %*% sandwich(RDglm2) %*% c(1, 1)  # matches p1var above
