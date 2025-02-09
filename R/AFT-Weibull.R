## Weibull accelerated failure time (AFT) model

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survreg()

# Weibull accelerated failure time model
# The Weibull distribution is the default for survival::survreg().
# Coefficients are log scale parameters = -log rate parameters.
aft_weib <- survreg(Surv(time, status) ~ rx, data = rats)
exp(-coef(aft_weib)["rx"])      # estimated rate (x = 0) and rate ratio
exp(-confint(aft_weib)["rx", ]) # log-transformed 95% Wald confidence intervals
shapehat <- 1 / aft_weib$scale  # shape parameter point estimate
shapehat

# shape parameter Z-score and p-value
# The null hypothesis shape = 0 corresponds to an exponential distribution.
lnshapevar <- vcov(aft_weib)["Log(scale)", "Log(scale)"]
shapeZ <- log(shapehat) / sqrt(lnshapevar)
shapeZ
2 * pnorm(-abs(shapeZ))

# shape parameter 95% CI
exp(-log(aft_weib$scale) + c(-1, 1) * qnorm(.975) *
    sqrt(vcov(aft_weib)["Log(scale)", "Log(scale)"]))

# estimated rate in the exposed
lnrate1hat <- exp(-sum(coef(aft_weib)))
lnrate1var <- as.numeric(c(1, 1, 0) %*% vcov(aft_weib) %*% c(1, 1, 0))
lnrate1ci <- lnrate1hat + c(-1, 1) * qnorm(0.975) * sqrt(lnrate1var)
exp(lnrate1hat)                 # point estimate
exp(lnrate1ci)                  # log-transformed 95% Wald confidence interval

# compare with nonparametric estimates
# Nelson-Aalen estimates (with Fleming-Harrington correction for ties)
# Formula ~ rx produces a separate estimate for each treatment group.
# Arguments stype = 2 and ctype = 2 produces NA estimate with FH correction.
ratsNA <- survfit(Surv(time, status) ~ rx, data = rats,
                  stype = 2, ctype = 2, conf.type =  "log-log")

# plot of Nelson-Aalen cumulative hazard curves in treated and untreated rats
plot(ratsNA, fun = "cumhaz", col = c("darkgray", "black"),
     xlab = "Weeks after treatment", ylab = "Cumulative hazard")
grid()
legend("topleft", bg = "white", lty = rep(c("solid", "dashed"), times = 2),
       col = rep(c("black", "darkgray"), each = 2),
       legend = c("Control (Nelson-Aalen)", "Control (Weibull AFT model)",
                  "Treated (Nelson-Aalen)", "Treated (Weibull AFT model)"))

# estimated cumulative hazards from Weibull AFT model
aft_weib <- survreg(Surv(time, status) ~ rx, data = rats)
rate1_weib <- as.numeric(exp(-c(1, 1) %*% coef(aft_weib)))
rate0_weib <- exp(-coef(aft_weib)["(Intercept)"])
shape_weib <- 1 / aft_weib$scale
H_weib <- function(t, shape, rate) (rate * t)^shape

# add lines to plot
t <- seq(0, max(ratsNA$time), by = 0.1)
lines(t, H_weib(t, shape_weib, rate1_weib), lty = "dashed")
lines(t, H_weib(t, shape_weib, rate0_weib), lty = "dashed", col = "darkgray")