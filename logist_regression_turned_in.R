##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

# set up ====
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
labs
names(NH11)

str(NH11$everwrk)
levels(NH11$everwrk)
# collapse missing values to NA
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

# explore some variables to check for cleaning

# explore everwrk, age_p
head(NH11$everwrk, n=50)
head(NH11$age_p, n=50)
tail(NH11$age_p, n=50)
sort(unique(NH11$age_p))

# explore r_maritl
unique(NH11$r_maritl, n=50)
levels(NH11$r_maritl)
# collapse (remove) r_maritl levels
NH11$r_maritl <- factor(NH11$r_maritl, level = c(
  "1 Married - spouse in household",
  "2 Married - spouse not in household",
  "4 Widowed",
  "5 Divorced",
  "6 Separated",
  "7 Never married",
  "8 Living with partner"))

##  1  ## run the regression model (modified variables) ====
# Use glm to conduct a logistic regression to predict ever worked (everwrk) using
# age (age_p) and marital status (r_maritl).
# (note: modified, because we changed the the actual data)
regmod1 <- glm(everwrk~age_p+r_maritl,
                data=NH11, family="binomial")
coef(summary(regmod1))

## Logistic regression coefficients

##   Transforming the coefficients to make them easier to
##   interpret

# original model
regmod1.tab <- coef(summary(regmod1))
regmod1.tab[, "Estimate"] <- exp(coef(regmod1))
regmod1.tab

##  2  ## Generating predicted values ====
# Predict the probability of working for each level of marital status.

## Version A ##
# (based off tutorial, just for practice)

# Create a dataset with predictors set at desired levels
predDat1 <- with(NH11,
                 expand.grid(age_p = c(33, 63),
                             r_maritl = "5 Divorced"))

# predict ever worked at those levels
cbind(predDat1, predict(regmod1, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat1))



## Version B ##
# (switched to one age, all marital statuses)

# Create a dataset with predictors set at desired levels
predDat1 <- with(NH11,
                 expand.grid(age_p = c(35),
                             r_maritl = c("2 Married - spouse not in household",
                                          "4 Widowed", "5 Divorced", "6 Separated",
                                          "7 Never married", "8 Living with partner")))

# predict ever worked at those levels
cbind(predDat1, predict(regmod1, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat1))


## version C ##
# (controlling for sex)
# strange, because the fit for male and female is identical at each age
predDat1 <- with(NH11,
                 expand.grid(age_p = c(35),
                             sex = c("1 Male", "2 Female"),
                             r_maritl = c("2 Married - spouse not in household",
                                          "4 Widowed", "5 Divorced", "6 Separated",
                                          "7 Never married", "8 Living with partner")))

# predict ever worked at those levels
cbind(predDat1, predict(regmod1, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat1))



library(effects)
plot(allEffects(regmod1))