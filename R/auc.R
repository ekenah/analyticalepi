## areas under the ROC curves

# load Somogyi-Nelson test data generated for Figure 2.2 (if needed)
# The argument can contain a path before the file name.
SNdat <- read.csv("SNdat.csv")

auc <- function(x, y) {
  # x is an increasing list of specificities
  # y is a decreasing list of sensitivities
  roc <- approxfun(c(1, 1 - x, 0), c(1, y, 0), ties = "max")
  area <- integrate(function(x) roc(x), 0, 1)
  return(area)
}
auc(SNdat$spec_pre, SNdat$sens_pre)
auc(SNdat$spec_1hr, SNdat$sens_1hr)
auc(SNdat$spec_2hr, SNdat$sens_2hr)
auc(SNdat$spec_3hr, SNdat$sens_3hr)