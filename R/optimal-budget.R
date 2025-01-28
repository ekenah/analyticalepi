## Relative efficiency of imbalanced study design on fixed budget

# variance ratio comparing balanced study to budget-optimal study
logC <- seq(-3, 3, by = 0.01)
releff <- function(C) 2 * (1 + C) / (1 + sqrt(C))^2
plot(logC, releff(exp(logC)), type = "l", ylim = c(0, 2), xaxt = "n",
     xlab = expression(paste("Cost ratio ", italic("C"), " (log scale)")),
     ylab = "Asymptotic relative efficiency of budget-optimal study")
axis(1, at = log(c(1 / c(20, 10, 5, 2), 1, c(2, 5, 10, 20))),
     labels = c("1/20", "1/10", "1/5", "1/2", 1, 2, 5, 10, 20))
grid()
abline(h = 1, col = "darkgray")
