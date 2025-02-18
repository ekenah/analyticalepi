## Bayesian estimation of odds ratios

# load packages
# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(rstanarm)               # Bayesian regression models
library(survival)               # rats data

# GLMs in rstanarm are "stanreg" objects
? stan_glm
? coef.stanreg

# binomial GLM with log link (log-binomial model) for the RR
ORglm <- glm(status ~ rx, family = binomial(link = "log"), data = rats)
exp(coef(ORglm))                # estimated odds (x = 0) and OR
exp(confint(ORglm))             # likelihood ratio CIs

# Bayesian GLM with identity link fit using a flat prior for coefficients
# Use "prior = NULL" to specify a flat prior.
# Samples four chains with 2000 iteractions each by default.
# In each chain, the first 1000 samples are warmup and the last 1000 are kept.
ORbayes_non <- stan_glm(status ~ rx, data = rats,
                        family = binomial(link = "logit"), prior = NULL)
prior_summary(ORbayes_non)      # summary of prior distributions
summary(ORbayes_non)            # summary of model fit (look for Rhat = 1)
coef(ORbayes_non)               # posterior medians (linear predictor scale)
posterior_interval(ORbayes_non) # 90% credible intervals (default)
posterior_interval(ORbayes_non, prob = 0.95)      # 95% credible intervals
exp(coef(ORbayes_non))          # posterior medians for odds (rx = 0) and OR
exp(posterior_interval(ORbayes_non, prob = 0.95)) # 95% credible intervals

# data frame of samples from the posterior distribution
# This allows you to calculate posterior means and functions of coefficients.
post_non <- as.data.frame(ORbayes_non)

# point estimates and credible intervals for pdds in the treated
post_non$lnodds1 <- post_non$"(Intercept)" + post_non$rx
post_non$odds1 <- exp(post_non$lnodds1)
median(post_non$lnodds1)        # posterior median log odds in treated rats
median(post_non$odds1)          # posterior median odds in treated rats
quantile(post_non$odds1, c(0.025, 0.975)) # 95% credible interval for odds

# point estimates and credible intervals for risks and risk ratios
expit <- function(v) 1 / (1 + exp(-v))
post_non$risk0 <- expit(post_non$"(Intercept)")
post_non$risk1 <- expit(post_non$lnodds1)
post_non$oddsratio <- exp(post_non$rx)
post_non$riskratio <- post_non$risk1 / post_non$risk0
median(post_non$riskratio)
quantile(post_non$riskratio, c(0.025, 0.975))

