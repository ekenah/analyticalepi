# life table for male and female mortality in the United State, 2019
lifetab <- read.csv(file = "R/lifetable-2019.csv")
hdat <- subset(lifetab, age <= 80)
hdat$surv_male <- 1 - hdat$mortality_male
hdat$surv_female <- 1 - hdat$mortality_female

# plot hazard (events per year) for ages 0-80
plot(hdat$age, -log(hdat$surv_male), type = "l",
     xlab = "Age (years)", ylab = "Mortality hazard (deaths per year)")
lines(hdat$age, -log(hdat$surv_female), lty = "dashed")
grid()
legend("topleft", bg = "white", lty = c("solid", "dashed"),
       legend = c("Male", "Female"))
