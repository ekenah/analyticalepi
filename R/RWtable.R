## Table 2 from Remein and Wilkerson (Journal of Chronic Disease, 1961)

# function to generate numbers based on sensitivity and specificity
RWtable <- function(sens, spec, n1=70, n0=510) {
  # arguments:  sensitivity, specificity,
  #             n1 is number of diabetics, n0 is number of nondiabetics
  tp <- round(sens * n1)
  fp <- round((1 - spec) * n0)
  tn <- round(spec * n0)
  fn <- round((1 - sens) * n1)
  return(c(truepos = tp, falsepos = fp, trueneg = tn, falseneg = fn))
}

RWtable(0.443, 0.990)   # before meal
RWtable(0.786, 0.906)   # one hour after
RWtable(0.643, 0.969)   # two hours after
RWtable(0.486, 0.998)   # three hours after