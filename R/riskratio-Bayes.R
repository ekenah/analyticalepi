## Bayesian estimation of risk ratios

# load packages
# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(rstanarm)               # Bayesian regression models
library(survival)               # rats data

# GLMs in rstanarm are "stanreg" objects
? stan_glm
? coef.stanreg

# binomial GLM with log link (log-binomial model) for the RR
RRglm <- glm(status ~ rx, family = binomial(link = "log"), data = rats)
exp(coef(RRglm))                # estimated risk (x = 0) and RR
exp(confint(RRglm))             # likelihood ratio CIs

# Bayesian GLM with log link fit using a flat prior for coefficients
# Use "prior = NULL" to specify a flat prior.
# Samples four chains with 2000 iteractions each by default.
# In each chain, the first 1000 samples are warmup and the last 1000 are kept.
RRbayes_non <- stan_glm(status ~ rx, data = rats,
                        family = binomial(link = "log"), prior = NULL)
prior_summary(RRbayes_non)      # summary of prior distributions
summary(RRbayes_non)            # summary of model fit (look for Rhat = 1)
coef(RRbayes_non)               # posterior medians (linear predictor scale)
posterior_interval(RRbayes_non) # 90% credible intervals (default)
posterior_interval(RRbayes_non, prob = 0.95)      # 95% credible intervals
exp(coef(RRbayes_non))          # posterior medians for risk (rx = 0) and RR
exp(posterior_interval(RRbayes_non, prob = 0.95)) # 95% credible intervals

# data frame of samples from the posterior distribution
# This allows you to calculate posterior means and functions of coefficients.
# Use double quotes for variable names with non-letters, like (Intercept).
post_non <- as.data.frame(RRbayes_non)
median(post_non$"(Intercept)")  # equals intercept from coef()
median(post_non$rx)             # equals rx coefficient from coef()
mean(post_non$rx)               # posterior mean log RR
quantile(post_non$rx, 0.025)    # lower 95% credible interval for log RR
quantile(post_non$rx, 0.975)    # upper 95% credible interval for log RR
quantile(post_non$rx, c(0.025, 0.975))

# point estimates and credible intervals for risk in the treated
post_non$lnrisk1 <- post_non$"(Intercept)" + post_non$rx
post_non$risk1 <- exp(post_non$lnrisk1)
median(post_non$lnrisk1)        # posterior median log risk in treated rats
median(post_non$risk1)          # posterior median risk in treated rats
quantile(post_non$risk1, c(0.025, 0.975)) # 95% credible interval for risk

# histogram of samples from posterior distribution of log(risk0)
# Use freq = FALSE to get probability densities instead of counts
post_non$lnrisk0 <- post_non$"(Intercept)"      # rename for convenience
summary(post_non$lnrisk0)
post_non$risk0 <- exp(post_non$lnrisk0)
hist(post_non$lnrisk0, freq = FALSE, xlab = "Log risk in untreated rats")
grid()
# add kernel density estimate
lines(density(post_non$lnrisk0), lty = "dashed")
# add approximate normal distribution of MLE
lnp0 <- seq(-4, -1.5, by = 0.01)
lines(lnp0, dnorm(lnp0, mean = coef(RRglm)[1], sd = sqrt(vcov(RRglm)[1, 1])))

# plot of empirical CDF and normal CDF for MLE of risk in untreated rats
# For transformations, comparing CDFs does not require the delta method.
plot(ecdf(post_non$risk0), main = NULL,
     xlab = "Risk in untreated rats",
     ylab = "Cumulative distribution function")
lines(exp(lnp0), pnorm(lnp0, mean = coef(RRglm)[1],
                       sd = sqrt(vcov(RRglm)[1, 1])),
      lty = "dashed")
grid()

# histogram of samples from posterior distribution of log(risk ratio)
summary(post_non$rx)
hist(post_non$rx, freq = FALSE, ylim = c(0, 1.4),
     xlab = "Log risk ratio (treated / untreated)")
grid()
lnrr <- seq(-0.3, 1.7, by = 0.01)
lines(density(post_non$rx), lty = "dashed")
lines(lnrr, dnorm(lnrr, coef(RRglm)[2], sqrt(vcov(RRglm)[2, 2])))

# plot of empirical CDF and normal CDF for MLE of risk ratio
# For transformations, comparing CDFs does not require the delta method.
post_non$rr <- exp(post_non$rx)
plot(ecdf(post_non$rr), main = NULL, lty = "dotted",
     xlab = "Risk ratio (treated / untreated)",
     ylab = "Cumulative distribution function")
lines(exp(lnrr), pnorm(lnrr, mean = coef(RRglm)[2],
                       sd = sqrt(vcov(RRglm)[2, 2])),
      lty = "dashed")
grid()

# using the default prior (weakly informative normal distributions)
# You can use the chains and iter arguments to change the sampling.
# The first half of each chain is warmup, and the last half is kept as samples.
RRbayes_def <- stan_glm(status ~ rx, data = rats, family = binomial(link = "log"),
                        iter = 1500, chains = 6)
prior_summary(RRbayes_def)
summary(RRbayes_def)
exp(coef(RRbayes_def))          # posterior medians for risk (rx = 0) and RR
exp(posterior_interval(RRbayes_def, prob = 0.95)) # 95% credible intervals
post_def <- as.data.frame(RRbayes_def)

# compare distributions of RR under noninformative and default priors
post_def$rr <- exp(post_def$rx)
plot(ecdf(post_def$rr), main = NULL,
     xlab = "Risk ratio (treated / untreated)",
     ylab = "Cumulative distribution function")
lines(ecdf(post_non$rr), col = "darkgray")
grid()
legend("bottomright", bg = "white", lty = "solid",
       col = c("black", "darkgray"),
       legend = c("Weakly informative prior", "Noninformative prior"))

# specifying an informative prior
# For the intercept, we use normal with mean -ln(0.1) and sd = ln(1.5).
# For the coefficient on rx, we use a Laplace (double exponential).
RRbayes <- stan_glm(status ~ rx, data = rats, family = binomial(link = "log"),
                    prior_intercept = normal(location = -log(0.2),
                                             scale = log(2)),
                    prior = normal(location = 0, scale = log(2)))
summary(RRbayes)
prior_summary(RRbayes)
exp(coef(RRbayes))              # posterior medians for risk (rx = 0) and RR
exp(posterior_interval(RRbayes, prob = 0.95)) # 95% credible intervals
post <- as.data.frame(RRbayes)
post$lnrisk1 <- post$"(Intercept)" + post$rx
post$risk1 <- exp(post$lnrisk1)
median(post$risk1)              # posterior median risk in treated rats
quantile(post$risk1, c(0.025, 0.975))   # 95% credible interval for risk
quantile(post$rx, c(0.025, 0.975))      # compare to GLM 95% CI for rx coef
