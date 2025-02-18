## Bayesian estimation of risk differences

# load packages
# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(rstanarm)               # Bayesian regression models
library(survival)               # rats data


# binomial GLM with identity link fit using maximum likelihood
RDglm <- glm(status ~ rx, family = binomial(link = "identity"), data = rats)
summary(RDglm)                  # point estimates, p-values, CIs, global tests
coef(RDglm)                     # estimated risk (x = 0) and RD
confint(RDglm)                  # likelihood ratio CIs (better)

# Bayesian GLM with identity link fit using a flat prior
# Use "prior = NULL" to specify a flat prior
RDbayes_non <- stan_glm(status ~ rx, data = rats,
                        family = binomial(link = "identity"), prior = NULL)
