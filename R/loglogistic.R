## Fitting a log-logistic distribution
# To sample a log-logistic random variable in R, you sample a logistic random
# variable with location = -log(rate) and scale = 1 / shape.
# The exponential of the logistic variable has a log-logistic distribution
# with the correct rate and shape parameters.
library(survival)

# Log-logistic distribution and regression (rate = 2, shape = 3)
llsample <- exp(rlogis(1000, location = -log(2), scale = 1 / 3))
llogdat <- data.frame(time = llsample, event = 1)
llogreg <- survreg(Surv(time, event) ~ 1, data = llogdat,
                   dist = "loglogistic")
exp(-coef(llogreg))       # point estimate of rate
exp(-confint(llogreg))    # 95% confidence interval for rate
1 / llogreg$scale         # point estimate of shape

# log-transformed 95% confidence interval for the shape parameter
exp(-log(llogreg$scale) + c(-1, 1)
    * qnorm(.975) * sqrt(vcov(llogreg)["Log(scale)", "Log(scale)"]))

# plot of true and estimated log-logistic hazard functions
llrate_est <- exp(-coef(llogreg))
llshape_est <- 1 / llogreg$scale
llrate_true <- 2
llshape_true <- 3
h_llog <- function(time, rate, shape) {
  # returns last expression if there is no return() statement
  (rate * shape * (time * rate)^(shape - 1) /
   (1 + (time* rate)^shape))
}
t <- seq(0, 4, by = 0.01)
plot(t, h_llog(t, llrate_true, llshape_true), type = "n",
     xlab = "Time", ylab = "Hazard (Log-logistic)")
grid()
lines(t, h_llog(t, llrate_est, llshape_est))
lines(t, h_llog(t, llrate_true, llshape_true), lty = "dashed")
legend("topright", lty = c("solid", "dashed"), bg = "white",
       legend = c("Estimated hazard function", "True hazard function"))