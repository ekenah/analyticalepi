## Sensitivity and specificity

# generate diagnostic testing data
set.seed(42)
n <- 500
dtdat <- data.frame(disease = rbinom(n, 1, 0.5))
dtdat$testpos <- ifelse(dtdat$disease,
                        rbinom(n, 1, 0.85), rbinom(n, 1, 0.05))

# prevalence
mean(dtdat$disease)
# Pr(T+)
mean(dtdat$testpos)

# sensitivity
mean(dtdat$testpos[dtdat$disease == TRUE])
sum(dtdat$disease & dtdat$testpos) / sum(dtdat$disease)

# specificity
1 - mean(dtdat$testpos[dtdat$disease == FALSE])
mean(!dtdat$testpos[dtdat$disease == FALSE])