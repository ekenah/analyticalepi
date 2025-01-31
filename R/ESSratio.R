## Effective sample size under nondifferential misclassification

# function that returns the effective sample size ratio
ess_ratio <- function(sens, spec, p) {
  # returns the multiplier of the sample size to get the effective sample size
  # under nondifferential misclassification with marginal risk (or prevalence) 
  # p in the sample
  K <- sens + spec - 1
  Kden <- (1 - spec + K * p) * (1 - sens + K * (1 - p))
  return(K^2 * p * (1 - p) / Kden)
}

# data frame for ESSR with specificity = 1 and varying sensitivity
s <- seq(0.01, 1, by = 0.005)
p <- c(0.02, 0.05, 0.1, 0.2, 0.4)
pnames <- c("p02", "p05", "p10", "p20", "p40")
essr_sens <- outer(s, p, function(s, p) ess_ratio(sens = s, spec = 1, p = p))
colnames(essr_sens) <- pnames
essr_sens <- as.data.frame(essr_sens)

# data frame for ESSR with sensitivity = 1 and varying specificity
essr_spec <- outer(s, p, function(s, p) ess_ratio(sens = 1, spec = s, p = p))
colnames(essr_spec) <- pnames
essr_spec <- as.data.frame(essr_spec)

# plot
plot(s, essr_spec$p02, type = "l", asp = 1, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Sensitivity (gray) or specificity (black)",
     ylab = "Effective sample size ratio")
lines(s, essr_spec$p10, lty = "dashed")
lines(s, essr_spec$p40, lty = "dotdash")
lines(s, essr_sens$p02, col = "darkgray")
lines(s, essr_sens$p10, col = "darkgray", lty = "dashed")
lines(s, essr_sens$p40, col = "darkgray", lty = "dotdash")
grid()
legend("topleft", bg = "white",
       lty = c("solid", "dashed", "dotdash"),
       legend = c("p = 0.02", "p = 0.10", "p = 0.40"))
text(0.48, 0.52, srt = 45, col = "darkgray",
     "Specificity = 1 with varying sensitivity")
text(0.68, 0.32, srt = 45, "Sensitivity = 1 with varying specificity")
