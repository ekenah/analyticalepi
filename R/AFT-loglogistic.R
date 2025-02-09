## Log-logistic accelerated failure time (AFT) model

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survreg()

# Log-logistic accelerated failure time model
# Default distribution is Weibull, so we must specify dist = "loglogistic".
# Coefficients are log scale parameters = -log rate parameters.
aft_llog <- survreg(Surv(time, status) ~ rx, data = rats, dist = "loglogistic")
exp(-coef(aft_llog)["rx"])      # estimated rate (x = 0) and rate ratio
exp(-confint(aft_llog)["rx", ]) # log-transformed 95% Wald confidence intervals
1 / aft_llog$scale              # shape parameter point estimate

# shape parameter 95% CI
exp(-log(aft_llog$scale) + c(-1, 1) * qnorm(.975) *
    sqrt(vcov(aft_llog)["Log(scale)", "Log(scale)"]))

# estimated rate in the exposed
lnrate1hat <- -sum(coef(aft_llog))
lnrate1var <- as.numeric(c(1, 1, 0) %*% vcov(aft_llog) %*% c(1, 1, 0))
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
       legend = c("Control (Nelson-Aalen)", "Control (Log-logistic AFT model)",
                  "Treated (Nelson-Aalen)", "Treated (Log-logistic AFT model)"))

# estimated cumulative hazards from log-logistic AFT model
aft_llog <- survreg(Surv(time, status) ~ rx, data = rats, dist = "loglogistic")
rate1_llog <- as.numeric(exp(-c(1, 1) %*% coef(aft_llog)))
rate0_llog <- exp(-coef(aft_llog)["(Intercept)"])
shape_llog <- 1 / aft_llog$scale
H_llog <- function(t, shape, rate) log(1 + (rate * t)^shape)

# add lines to plot
t <- seq(0, max(ratsNA$time), by = 0.1)
lines(t, H_llog(t, shape_llog, rate1_llog), lty = "dashed")
lines(t, H_llog(t, shape_llog, rate0_llog), lty = "dashed", col = "darkgray")
