## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
sp <- read.table(file= "./data/species.txt", header= TRUE)

# Check the structure of the data to see if we need to correct anything
str(sp)

# Correct pH so it is read as a factor, reordered such that "low" is the reference level
sp$pH<- factor(sp$pH, levels = c("low", "mid", "high"))

# Get simple descriptives of the data (e.g. what is the range for each variable?)
summary(sp)

# make a list of the variables of interest, for convenience:
VOI<- c("Species", "Biomass", "pH")
pairs(sp[, VOI])
# Negative relationship between Species and Biomass?
# Positive relationship between Species and pH? 
# Biomass tends to increase with pH, which could generate
# some collinearity between these explanatory variables in a model.
# but still plenty of variation in Biomass within each pH, 
# so hopefully this won't be an issue.

coplot(Species ~ Biomass | pH, data = sp)
# the relationships looks consistently negative with biomass across the pH levels


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# y_i ~ Poisson(lambda_i)
# log(lambda_i) = beta_0 + beta_1 * Biomass


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
sp.glm1 <- glm(Species ~ Biomass, 
               family = poisson(link = "log"), 
               data = sp)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Model diagnostic plots:
# Residuals vs Fitted
  # We see a fairly clear "funnel" pattern. We go from having relatively "little" variation
  # when our predicted values (on the link scale) are small, to "lots" of variation
  # when our predicted values are large, and most of this is happening between predicted
  # values of 2.5 to 3.5 (so over a fairly small range of predicted values)
  # This would suggest we're not meeting the assumptions particularly well and
  # is a good indication that we have overdispersion.
# Q-Q Residuals
  # We completely ignore this figure for GLMs.
# Scale-Location
  # We also don't want to see any patterns (as for Resids vs Fitted) but we see the
  # same rapid increase in variation once predicted values are above 2.5
# Residuals vs Leverage
  # We're really only using this to check for values close to a Cook's distance of 1
  # While we don't have any observations that are greater than 1, we have a handful
  # that are getting uncomfortably close. While these values are fine, we're
  # not exactly jumping for joy with these Cook's distances.
# Overall, the diagnostic plots don't look great. Not the worst, but we'd want something
# better.

# To do a quick and crude check for dispersion, we can use the information from summary()
  # We take residual deviance and divide by the degrees of freedom, so for this model:
  # 432.63/88 = 4.9!
  # We have a whopping 4.9 overdispersion! My general rule of thumb is that I start getting
  # concerned when dispersion is somewhere in the 1.5-1.8 region. 4.9 is doomsday!
  # As a result, our standard error for our parameter estimates is going to be artificially
  # small. This in turn leads to both 1) risks of our p value being smaller than it should be
  # for Biomass, and 2) any predictions that include uncertainty being too confident.

par(mfrow = c(2,2)) # Show figures in 2 rows and 2 columns
plot(sp.glm1)       # Plot the model diagnostics
par(mfrow = c(1,1)) # Reset so only 1 figure is shown
summary(sp.glm1)    # Get the summary of the model (to check dispersion)


## ----Q8.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# On the link scale, how many plants would you predict if a plot had 0 kg of biomass?
3.55628 + -0.19598 * 0 # = 3.55628
# On the link scale, how many plants would you predict if a plot had 2.5 kg of biomass?
3.55628 + -0.19598 * 2.5 # = 3.06633
# On the link scale, how many plants would you predict if a plot had 5 kg of biomass?
3.55628 + -0.19598 * 5 # = 2.57638


## ----Q8.2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# On the response scale, how many plants would you predict if a plot had 0 kg of biomass?
exp(3.55628 + -0.19598 * 0) # = 35.0
# On the response scale, how many plants would you predict if a plot had 5 kg of biomass?
exp(3.55628 + -0.19598 * 5) # = 13.1
# On the response scale, how many plants would you predict if a plot had 10 kg of biomass?
exp(3.55628 + -0.19598 * 10) # = 4.9


## ----Q8.3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
biomass_new <- seq(from = min(sp$Biomass), to = max(sp$Biomass), length.out = 5)
biomass_new


## ----Q8.4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
pred_plants <- exp(3.55628 + -0.19598 * biomass_new)


## ----Q8.5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
plot(pred_plants ~ biomass_new)
lines(pred_plants ~ biomass_new)


## ----Q8.6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# We would simply increase the number of biomass values that our seq() code created. Try it out if you're interested.
biomass_new <- seq(from = min(sp$Biomass), to = max(sp$Biomass), length.out = 50)
pred_plants <- exp(3.55628 + -0.19598 * biomass_new)
plot(pred_plants ~ biomass_new)
lines(pred_plants ~ biomass_new)


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
sp.glm2 <- glm(Species ~ Biomass + pH, 
               family = poisson(link = "log"), 
               data = sp)


## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
# Model diagnostic plots:
# Residuals versus Fitted
  # While we do still see a little bit of a funnel, it's a big improvement compared 
  # to previous model. This plot for the new model, while not perfect, is pretty good
# Q-Q Residuals
  # We completely ignore this figure (despite it looking nice in this model)
# Scale-Location
  # We also don't want to see any patterns. There's maybe a touch of increasing
  # error as our predictor gets larger, but not enough to concern me
# Residuals vs Leverage
  # We're really only using this to check for values close to a Cook's distance of 1
  # We can't even see the dashed lines showing a Cook's distance of 0.5, so we're
  # golden here too
# Overall, the diagnostic plots look great

# To do a quick and crude check for dispersion, we can use the information from summary()
  # We take residual deviance and divide by the degrees of freedom, so for this model:
  # 77.128/86 = 0.9
  # Not exactly the ideal value of 1 but 0.9 is nothing to get concerned about
  # Also, remember that *underdispersion* is generally not much of an issue
  # Compared to the first model, this is a fantastic improvement
par(mfrow = c(2,2))
plot(sp.glm2)
par(mfrow = c(1,1))
summary(sp.glm2)


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
summary(sp.glm2)
# "(Intercept)" is the predicted value on the link (log) scale when
# pH is "low". A hint is that there is no labelled coefficient called
# "pHlow".

# "Biomass" is the slope of Biomass.
# It is negative, so assumes a linear decrease (on the log scale)
# For every extra 1 unit (here kg) of biomass "added" to a plot
# the number of species (on the log scale) decreases by -0.16

# "pHmid" is the estimated difference (on the log scale) between
# the "pHmid" and the reference level, "pHlow".
# A value of 0.65 means plots with "mid" pH have 0.65 (on link scale)
# more species than "low" (our intercept).

# "pHhigh" is the estimated difference (on the log scale) between
# the "pHhigh" and the reference level, "pHlow".
# A value of 1.13 means plots with "mid" pH have 1.13 (on link scale)
# more species than "low" (our intercept).

# A mathematical description of the model
# (and how I would present it in the methods section of a paper):
# Species_i ~ Poisson(lambda_i)
# log(lambda_i) = 2.76 + -0.16 * Biomass_i
#    + 0.65 * pHmid_i + 1.13 * pHhigh_i


## ----Q12, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
library(ggeffects)
bio_pred <- ggpredict(sp.glm2, terms = "Biomass")
plot(bio_pred)
ph_pred <- ggpredict(sp.glm2, terms = "pH")
plot(ph_pred)

