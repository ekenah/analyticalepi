lifetab <- read.csv(file = "R/lifetable-2019.csv")
lifetab$surv_male <- 1 - lifetab$mortality_male
lifetab$surv_female <- 1 - lifetab$mortality_female

# plot hazard (cumulative hazard per year) for ages 0-80
maxage <- 80
lifedat <- lifetab[1:(maxage + 1), ]    # row 1 is age zero
plot(lifedat$age, -log(lifedat$surv_male), type = "l",
     xlab = "Age (years)", ylab = "Mortality hazard (deaths per year)")
lines(lifedat$age, -log(lifedat$surv_female), lty = "dashed")
grid()
legend("topleft", bg = "white", lty = c("solid", "dashed"),
       legend = c("Male", "Female"))
