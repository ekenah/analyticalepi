# data from Table 2 in Remein and Wilkerson (Journal of Chronic Disease, 1961)
SNdat <- data.frame(cutoff = seq(70, 200, by = 10))
SNdat$sens_pre <- c(95.7, 91.4, 82.9, 65.7, 54.3, 50.0, 44.3, 37.1, 30.0,
                    25.7, 25.7, 22.9, 21.4, 17.1) / 100
SNdat$spec_pre <- c(11.0, 36.3, 65.7, 84.7, 92.7, 96.7, 99.0, 99.6, 99.8,
                    99.8, 99.8, 99.8, 100.0, 100.0) / 100
SNdat$sens_1hr <- c(100.0, 97.1, 97.1, 95.7, 92.9, 88.6, 78.6, 68.6, 57.1,
                    52.9, 47.1, 40.0, 34.3, 28.6) / 100
SNdat$spec_1hr <- c(8.2, 22.4, 39.0, 57.3, 70.6, 83.3, 90.6, 95.1, 97.8,
                    99.4, 99.6, 99.8, 100.0, 100.0) / 100
SNdat$sens_2hr <- c(98.6, 97.1, 94.3, 88.6, 85.7, 71.4, 64.3, 57.1, 50.0,
                    47.1, 42.9, 38.6, 34.3, 27.1) / 100
SNdat$spec_2hr <- c(8.8, 25.5, 47.6, 69.8, 84.1, 92.5, 96.9, 99.4, 99.6,
                    99.8, 100.0, 100.0, 100.0, 100.0) / 100
SNdat$sens_3hr <- c(94.3, 91.4, 82.9, 70.0, 60.0, 51.4, 48.6, 41.4, 32.9,
                    28.6, 28.6, 28.6, 24.3, 20.0) / 100
SNdat$spec_3hr <- c(8.6, 34.7, 67.5, 86.5, 95.3, 98.2, 99.8,
                    rep(100.0, 7)) / 100
# write.csv(SNdat, "SNdat.csv", row.names = FALSE)

# ROC curves with labels
plot(1 - SNdat$spec_pre, SNdat$sens_pre, type = "n",
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1 - Specificity = Pr(T+ | D-)",
     ylab = "Sensitivity = Pr(T+ | D+)")
grid()
lines(1 - SNdat$spec_pre, SNdat$sens_pre, col = "darkgray")
lines(1 - SNdat$spec_1hr, SNdat$sens_1hr, lty = "solid")
lines(1 - SNdat$spec_2hr, SNdat$sens_2hr, lty = "dashed")
lines(1 - SNdat$spec_3hr, SNdat$sens_3hr, lty = "dotted")
points(1 - SNdat[SNdat$cutoff == 130, c(3, 5, 7, 9)],
       SNdat[SNdat$cutoff == 130, c(2, 4, 6, 8)])
points(1 - SNdat$spec_pre[seq(2, 12, by = 2)],
       SNdat$sens_pre[seq(2, 12, by = 2)], pch = 8)
text(1 - SNdat$spec_pre[seq(2, 12, by = 2)] + c(0, .09, .09, .1, .1, .1),
     SNdat$sens_pre[seq(2, 12, by = 2)] + c(-.05, -.02, -.02, 0, 0, -.01),
     labels = c("80 mg/dL", "100 mg/dL", "120 mg/dL", "140 mg/dL",
                "160 mg/dL", "180 mg/dL"))
abline(0, 1, lty = "dotted", col = "darkgray")
text(.51, .49, adj = c(.5, 1), srt = 42,
     label = "Useless tests (T and D independent)")
points(c(0, 0, 1), c(0, 1, 1), pch = 3)
text(.01, .99, adj = c(0, 1), label = "Perfect test")
text(.01, .01, adj = c(0, 0), label = "Everyone tests negative")
text(.99, .99, adj = c(1, 0), srt = 90, label = "Everyone tests positive")
legend("bottomright", bg = "white",
       lty = c("solid", "solid", "dashed", "dotted", NA),
       col = c("darkgray", rep("black", 4)), pch = c(rep(NA, 4), 1),
       legend = c("Before meal  (AUC = 0.825)",
                  "1 hour after   (AUC = 0.923)",
                  "2 hours after (AUC = 0.904)",
                  "3 hours after (AUC = 0.839)", "130 mg/dL cutoff"))