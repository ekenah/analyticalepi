## Exponential accelerated failure time (AFT) model

# The rats data is from:
# Mantel, Bohidar, and Ciminera (1977). Cancer Research 37: 3863-3868.
library(survival)               # rats data and survreg()
library(lmtest)                 # coefci() for Wald confidence intervals

# exponential AFT model
# Default distribution is Weibull, so we must specify dist = "exponential".
# Coefficients are log scale parameters = -log rate parameters.
aft_exp <- survreg(Surv(time, status) ~ rx, data = rats, dist = "exponential")
exp(-coef(aft_exp))             # estimated rate (x = 0) and rate ratio
exp(-confint(aft_exp))          # log-transformed 95% Wald confidence intervals

# estimated rate in the exposed
lnrate1hat <- -sum(coef(aft_exp))
lnrate1var <- as.numeric(c(1, 1) %*% vcov(aft_exp) %*% c(1, 1))
lnrate1ci <- lnrate1hat + c(-1, 1) * qnorm(0.975) * sqrt(lnrate1var)
exp(lnrate1hat)                 # point estimate
exp(lnrate1ci)                  # log-transformed 95% Wald confidence interval

# exponential AFT results match Poisson GLM with log(rat-weeks) offset
IRRglm <- glm(status ~ rx  + offset(log(time)), family = poisson(), data = rats)
exp(coef(IRRglm))               # estimated incidence rate (x = 0) and IRR
exp(coefci(IRRglm))             # log-transformed Wald CIs matches above

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
       legend = c("Control (Nelson-Aalen)", "Control (Exponential AFT model)",
                  "Treated (Nelson-Aalen)", "Treated (Exponential AFT model)"))

# estimated cumulative hazards from exponential AFT model
aft_exp <- survreg(Surv(time, status) ~ rx, data = rats, dist = "exponential")
rate1_exp <- as.numeric(exp(-c(1, 1) %*% coef(aft_exp)))
rate0_exp <- exp(-coef(aft_exp)["(Intercept)"])
H_exp <- function(t, rate) rate * t

# add lines to plot
t <- seq(0, max(ratsNA$time), by = 0.1)
lines(t, H_exp(t, rate1_exp), lty = "dashed", col = "black")
lines(t, H_exp(t, rate0_exp), lty = "dashed", col = "darkgray")
