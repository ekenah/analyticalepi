## Ratio of case-control and cohort standard errors

# plot
cpc <- 1:10    # controls per case
plot(cpc, sqrt(1 + 1 / cpc), ylim = c(0, 2),
     xlab = "Controls per case",
     ylab = expression(sigma[plain(cc)] / sigma[plain(cohort)]))
grid()
abline(h = 1, col = "darkgray")
