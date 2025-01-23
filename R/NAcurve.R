## Nelson-Aalen cumulative hazard curve
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

# Nelson-Aalen estimate of the survival function with FH correction
llog_na <- survfit(Surv(time, delta) ~ 1, data = llogdat,
                   conf.type = "log-log", stype = 2, ctype = 2)

# log-logistic cumulative hazard function
llog_cumhaz <- function(t, lambda, gamma) log(1 + (lambda * t)^gamma)

# Nelson-Aalen curve and log-logistic cumulative hazard curve
t <- seq(0, max(llogdat$time), .01)
plot(llog_na, fun = "cumhaz", xlim = c(0, 20),
     xlab = "Time", ylab = "Cumulative hazard")
grid()
lines(t, llog_cumhaz(t, 2, 1), col = "darkgray")
legend("bottomright", bg = "white", lty = c("solid", "dashed", "solid"),
       col = c("black", "black", "darkgray"),
       legend = c("Nelson-Aalen estimate", "95% confidence limits",
                  "True cumulative hazard function"))
