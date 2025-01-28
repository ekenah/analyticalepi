## Actual power of a Pearson chi-squared test

# calculate Pearson chi-squared test power
# This can take a few minutes to run with large n.
powers <- function(p1, p0, n, level = 0.95) {
  chisq_alpha <- qchisq(level, df = 1)
  htest <- function(r1) {
    r0 <- n - r1
    joint_dbinom <- outer(0:r1, 0:r0,
                          function(a, c) dbinom(a, r1, p1) * dbinom(c, r0, p0))
    joint_include <- outer(0:r1, 0:r0,
                           function(a, c) max(a, c) > 0 & a + c < n)
    acpower <- Vectorize(function(a, c) {
      if (max(a, c) > 0 & a + c < n) {
        b <- r1 - a
        d <- r0 - c
        k1 <- a + c
        k0 <- b + d
        chisqP <- n * (a * d - b * c)^2 / (r1 * r0 * k1 * k0)
        return(chisqP > chisq_alpha)
      } else {
        return(0)
      }
    })
    joint_power <- outer(0:r1, 0:r0, acpower)
    return(sum(joint_dbinom * joint_power) / sum(joint_dbinom * joint_include))
  }
  r1s <- 1:(n - 1)
  powers <- sapply(r1s, htest)
  return(data.frame(r1 = r1s, power = powers, n = n))
}

# optimal value proportion exposed (or proportion cases)
optimphi <- function(p1, p0) 1 / (1 + sqrt(p1 * (1 - p1) / (p0 * (1 - p0))))

# save values of graphical parameter "mar" before changing them
orig_mar <- par("mar")
orig_mfrow <- par("mfrow")
par(mar = c(4, 4, 3, 2))
par(mfrow = c(2, 2))

# Pearson chi-squared test power for p1 = 0.1 and p0 = 0.02
power_10_02_400 <- powers(0.10, 0.02, 400)
power_10_02_200 <- powers(0.10, 0.02, 200)
power_10_02_100 <- powers(0.10, 0.02, 100)
plot(power_10_02_400$r1 / 400, power_10_02_400$power,
     type = "l", ylim = c(0, 1),
     main = expression(paste(p[1], " = 0.10", " and ", p[0], " = 0.02")),
     xlab = "",
     ylab = "Power (Pearson chi-squared test)")
lines(power_10_02_200$r1 / 200, power_10_02_200$power, lty = "dashed")
lines(power_10_02_100$r1 / 100, power_10_02_100$power, lty = "dotted")
abline(v = 0.5, col = "darkgray")
abline(v = optimphi(0.10, 0.02), lty = "dashed", col = "darkgray")
grid()

# Pearson chi-squared test power for p1 = 0.10 and p0 = 0.05
power_10_05_400 <- powers(0.10, 0.05, 400)
power_10_05_200 <- powers(0.10, 0.05, 200)
power_10_05_100 <- powers(0.10, 0.05, 100)
plot(power_10_05_400$r1 / 400, power_10_05_400$power,
     type = "l", ylim = c(0, 1),
     main = expression(paste(p[1], " = 0.10", " and ", p[0], " = 0.05")),
     xlab = "", ylab = "")
lines(power_10_05_200$r1 / 200, power_10_05_200$power, lty = "dashed")
lines(power_10_05_100$r1 / 100, power_10_05_100$power, lty = "dotted")
abline(v = 0.5, col = "darkgray")
abline(v = optimphi(0.10, 0.05), lty = "dashed", col = "darkgray")
grid()
legend("topright", bg = "white", lty = c("solid", "dashed", "dotted"),
       legend = c("n = 400", "n = 200", "n = 100"))

# Pearson chi-squared test power for p1 = 0.2 and p0 = 0.02
power_20_02_400 <- powers(0.20, 0.02, 400)
power_20_02_200 <- powers(0.20, 0.02, 200)
power_20_02_100 <- powers(0.20, 0.02, 100)
plot(power_20_02_400$r1 / 400, power_20_02_400$power,
     type = "l", ylim = c(0, 1),
     main = expression(paste(p[1], " = 0.20", " and ", p[0], " = 0.02")),
     xlab = expression(paste("Proportion exposed (", phi, ")")),
     ylab = "Power (Pearson chi-squared test)")
lines(power_20_02_200$r1 / 200, power_20_02_200$power, lty = "dashed")
lines(power_20_02_100$r1 / 100, power_20_02_100$power, lty = "dotted")
abline(v = 0.5, col = "darkgray")
abline(v = optimphi(0.20, 0.02), lty = "dashed", col = "darkgray")
grid()

# Pearson chi-squared test power for p1 = 0.2 and p0 = 0.05
power_20_05_400 <- powers(0.20, 0.05, 400)
power_20_05_200 <- powers(0.20, 0.05, 200)
power_20_05_100 <- powers(0.20, 0.05, 100)
plot(power_20_05_400$r1 / 400, power_20_05_400$power,
     type = "l", ylim = c(0, 1),
     main = expression(paste(p[1], " = 0.20", " and ", p[0], " = 0.05")),
     xlab = expression(paste("Proportion exposed (", phi, ")")),
     ylab = ""
    #  ylab = "Power (Pearson chi-squared test)"
     )
lines(power_20_05_200$r1 / 200, power_20_05_200$power, lty = "dashed")
lines(power_20_05_100$r1 / 100, power_20_05_100$power, lty = "dotted")
abline(v = 0.5, col = "darkgray")
abline(v = optimphi(0.20, 0.05), lty = "dashed", col = "darkgray")
grid()

# reset graphical parameters "mar" and "mfrow"
par(mar = orig_mar)
par(mfrow = orig_mfrow)
