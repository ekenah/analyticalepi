## Kaplan-Meier survival curve
library(survival)

# right-censored sample from log-logistic dist with rate = 2 and shape = 1
# Uses samples from logistic distribution with location = -log(rate) and
# scale = 1 / shape.
set.seed(42)
llog_f <- exp(rlogis(500, location = -log(2), scale = 1))
llog_c <- exp(rlogis(500, location = -log(2), scale = 2))
t <- pmin(llog_f, llog_c)
d <- ifelse(llog_c < llog_f, 0, 1)
llogdat <- data.frame(time = t, delta = d)

# Kaplan-Meier estimate with complementary log-log confidence intervals
llog_km <- survfit(Surv(time, delta) ~ 1, data = llogdat,
                   conf.type = "log-log")

# Log-logistic survival function
llog_surv <- function(t, lambda=1, gamma=1) 1 / (1 + (lambda * t)^gamma)

# Kaplan-Meier curve and log-logistic survival curve
t <- seq(0, max(llogdat$time), .01)
plot(llog_km, xlim = c(0, 15),
     xlab = "Time", ylab = "Survival probability")
grid()
lines(t, llog_surv(t, 2, 1), col = "darkgray")
legend("topright", bg = "white",
       lty = c("solid", "dashed", "solid"),
       col = c("black", "black", "darkgray"),
       legend = c("Kaplan-Meier estimate", "95% confidence limits",
                  "True survival function"))