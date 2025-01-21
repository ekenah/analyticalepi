## Bayesian prior and posterior distributions for a probability

# beta prior and posterior distributions
# The prior is beta(1, 1), which is the uniform(0, 1) distribution.
# Because k = 15 and n = 22, the posterior is beta(15 + 1, 22 - 15 + 1)
p <- seq(0.01, 0.99, by = 0.01)
plot(p, dbeta(p, 16, 8), type = "n", ylim = c(0, 5),
     xlab = "p", ylab = "Probability density")
grid()
lines(p, dbeta(p, 1, 1), lty = "dashed")  # prior PDF
lines(p, dbeta(p, 16, 8))                 # posterior PDF
postlower <- qbeta(0.025, 16, 8)          # 95% credible interval lower bound
postupper <- qbeta(0.975, 16, 8)          # 95% credible interval upper bound
polygon(c(0, seq(0, postlower, by = 0.01), postlower),
        c(0, dbeta(seq(0, postlower, by = 0.01), 16, 8), 0),
        col = "darkgray")
polygon(c(postupper, seq(postupper, 1, by = 0.01), 1),
        c(0, dbeta(seq(postupper, 1, by = 0.01), 16, 8), 0),
        col = "darkgray")
legend("topleft", bg = "white",
       lty = c("dashed", "solid", NA, "dashed", "solid"),
       col = c("black", "black", NA, "darkgray", "darkgray"),
       fill = c(NA, NA, "darkgray", NA, NA),
       border = c(NA, NA, "black", NA, NA),
       legend = c("uniform prior", "posterior (uniform)",
                  "2.5% tails (uniform)", "Jeffreys prior",
                  "posterior (Jeffreys)"))
lines(p, dbeta(p, 0.5, 0.5), lty = "dashed", col = "darkgray")
lines(p, dbeta(p, 15.5, 7.5), col = "darkgray")