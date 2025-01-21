## Predictive values as a function of prevalence

# uses tdat_1hr data and tdat() function from Figure 2.3 (testpos.R)
# tdat_1hr <- read.csv("tdat_1hr.csv")
# generate data using the sensitivity and specificity of the pre-meal test
tdat_pre <- tdat(seq(0, 1, by = .01), sens = 0.443, spec = 0.990)

# plot of PPV and NPV as a function of diabetes prevalence
plot(tdat_1hr$prev, tdat_1hr$ppv, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Prevalence of disease = Pr(D+)",
     ylab = "Predictive value = Pr(D | T)")
grid()
lines(tdat_1hr$prev, tdat_1hr$ppv)
lines(tdat_1hr$prev, tdat_1hr$npv, lty = "dashed")
lines(tdat_pre$prev, tdat_pre$ppv, col = "darkgray")
lines(tdat_pre$prev, tdat_pre$npv, lty = "dashed", col = "darkgray")
legend("bottom", lty = c("solid", "dashed", "solid", "dashed"),
       col = c("darkgray", "darkgray", "black", "black"),
       bg = "white", inset = 0.05,
       legend = c("PPV before meal", "NPV before meal",
                  "PPV 1 hour after", "NPV 1 hour after"))