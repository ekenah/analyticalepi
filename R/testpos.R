## probability of testing positive as a function of prevalence

# function to generate testing data
tdat <- function(prev, sens=0.786, spec=0.906) {
  # defaults are sensitivity and sensitivity one hour after the meal
  truepos <- sens * prev
  falsepos <- (1 - spec) * (1 - prev)
  trueneg <- spec * (1 - prev)
  falseneg <- (1 - spec) * prev
  pos <- truepos + falsepos
  neg <- 1 - pos
  ppv <- truepos / pos
  npv <- trueneg / neg
  return(data.frame(prev = prev, sens = sens, spec = spec,
                    truepos = truepos, falsepos = falsepos,
                    trueneg = trueneg, falseneg = falseneg,
                    pos = pos, neg = neg, ppv = ppv, npv = npv))
}
tdat_1hr <- tdat(seq(0, 1, by = .01))
write.csv(tdat_1hr, "R/tdat_1hr.csv", row.names = FALSE)

# plot
plot(tdat_1hr$prev, tdat_1hr$pos, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Prevalence of disease = Pr(D+)",
     ylab = "Probability of positive test = Pr(T+)")
polygon(c(tdat_1hr$prev, 1, 0), c(tdat_1hr$pos, 0, 0),
        border = NA, col = "gray")
polygon(c(tdat_1hr$prev, 1, 0), c(tdat_1hr$falsepos, 0, 0),
        border = NA, col = "darkgray")
grid()
lines(tdat_1hr$prev, tdat_1hr$falsepos, col = "gray")
lines(tdat_1hr$prev, tdat_1hr$pos)
abline(0, 1, lty = "dotted")
text(0.1, 0.02, adj = c(0, 0), label = "False positives")
text(0.2, 0.1, adj = c(0, 0), label = "True positives")