## Fitting a Weibull distribution
# In R, the shape is 1 / the "scale" parameter.
# In standard terminology, the scale is 1 / rate.
library(survival)

# Weibull distribution and regression (rate = 2, shape = 3)
# Weibull is the default distribution for survival::survreg().
wsample <- rweibull(1000, shape = 3, scale = 1 / 2)
weibdat <- data.frame(time = wsample, event = 1)
weibreg <- survreg(Surv(time, event) ~ 1, data = weibdat)
summary(weibreg)
exp(-coef(weibreg))       # point estimate of rate
exp(-confint(weibreg))    # 95% confidence interval for rate
1 / weibreg$scale         # point estimate of shape

# log-transformed Wald confidence interval for the shape parameter
# vcov() returns the estimated covariance matrix from the model
exp(-log(weibreg$scale) + c(-1, 1)
    * qnorm(.975) * sqrt(vcov(weibreg)["Log(scale)", "Log(scale)"]))

# plot of true and estimated Weibull hazard functions
wrate_est <- exp(-coef(weibreg))
wshape_est <- 1 / weibreg$scale
wrate_true <- 2
wshape_true <- 3
h_weib <- function(time, rate, shape) rate * shape * (time * rate)^(shape - 1)
t <- seq(0, 4, by = 0.01)
plot(t, h_weib(t, wrate_true, wshape_true), type = "n",
     xlab = "Time", ylab = "Hazard (Weibull)")
grid()
lines(t, h_weib(t, wrate_est, wshape_est))
lines(t, h_weib(t, wrate_true, wshape_true), lty = "dashed")
legend("topleft", lty = c("solid", "dashed"), bg = "white",
       legend = c("Estimated hazard function", "True hazard function"))