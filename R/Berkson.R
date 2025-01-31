## Berkson (1946) example of selection bias

# Pearson chi-squared test for the eligible population (X and D independent)
poptab <- matrix(c(3000, 97000, 29700, 960300), nrow = 2)
chisq.test(poptab, correct = FALSE)
# Fisher's exact test (with confidence limits for odds ratio)
fisher.test(poptab)

# Pearson chi-squared test for the study sample (X and D not independent)
sampletab <- matrix(c(626, 6693, 9504, 192060), nrow = 2)
chisq.test(sampletab, correct = FALSE)
# Fisher's exact test (with confidence limits for odds ratio)
fisher.test(sampletab)
