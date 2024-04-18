## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# How do ward capacity, staff numbers, chlorhexidine policy, and an interaction 
# between chlorhexidine policy and staff numbers, relate with the proportion of 
# patients infected with MRSE?


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# With the blanks filled in, the equation should be:
# logit(p_i) = beta_0 + beta_1 * Capacity_i + beta_2 * Nopolicy_i + beta_3 * Staff_i + beta_4 * Staff_i * Nopolicy_i


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
icu <- read.table(file= "./data/mrse.txt", header= TRUE)

icu$proportion <- icu$mrse / icu$total_patients
icu$policy <- factor(icu$policy)
icu$fail <- icu$total_patients - icu$mrse

str(icu)
# 'data.frame':	210 obs. of  6 variables:
#  $ mrse          : int  0 4 6 9 0 7 10 9 11 10 ...
#  $ total_patients: int  15 13 10 17 10 18 21 19 18 19 ...
#  $ n_staff       : int  24 16 17 23 20 30 22 26 13 27 ...
#  $ policy        : Factor w/ 2 levels "Not implemented",..: 2 1 1 1 2 1 1 1 1 1 ...
#  $ capacity      : num  0.72 0.86 0.76 0.75 0.66 0.68 0.84 0.67 0.65 0.79 ...
#  $ proportion    : num  0 0.308 0.6 0.529 0 ...
#  $ fail          : int  15 9 4 8 10 11 11 10 7 9 ...

summary(icu)
 #      mrse        total_patients     n_staff                  policy   
 # Min.   : 0.000   Min.   : 7.00   Min.   :10.00   Implemented    : 80  
 # 1st Qu.: 1.000   1st Qu.:12.00   1st Qu.:19.00   Not implemented:130  
 # Median : 5.000   Median :15.00   Median :22.00                        
 # Mean   : 4.757   Mean   :15.07   Mean   :21.79                        
 # 3rd Qu.: 8.000   3rd Qu.:18.00   3rd Qu.:25.00                        
 # Max.   :16.000   Max.   :26.00   Max.   :36.00                        
 #    capacity        proportion           fail      
 # Min.   :0.3400   Min.   :0.00000   Min.   : 1.00  
 # 1st Qu.:0.6900   1st Qu.:0.07692   1st Qu.: 7.00  
 # Median :0.7600   Median :0.32576   Median :10.00  
 # Mean   :0.7517   Mean   :0.31208   Mean   :10.31  
 # 3rd Qu.:0.8500   3rd Qu.:0.50000   3rd Qu.:13.00  
 # Max.   :1.0000   Max.   :0.87500   Max.   :25.00  

# install.packages("ggplot2") # Run this line of code if you do not have ggplot installed
# Once installed, load the package
library(ggplot2)

# A boxplot for proportion ~ policy
# In base R: boxplot(icu$proportion ~ icu$policy)
# In ggplot:
ggplot(icu) +
  geom_boxplot(aes(x = policy, y = proportion))

# A scatterplot for proportion ~ capacity
# In base R: plot(icu$proportion ~ icu$capacity)
# In ggplot:
ggplot(icu) +
  geom_point(aes(x = capacity, y = proportion))

# A scatterplot for proportion ~ n_staff, colour by policy
# In base R: plot(icu$proportion ~ icu$n_staff)
           # points(icu$proportion ~ icu$n_staff, col = icu$policy)
# In ggplot:
ggplot(icu) +
  geom_point(aes(x = n_staff, y = proportion, colour = policy))

# A histogram showing varying number of trials
# In base R: hist(icu$total_patients)
# In ggplot:
ggplot(icu) +
  geom_histogram(aes(x = total_patients))

# Pairs plot with a few extra fancy things added in
# To find these "fancy" functions, check out the examples in ?pairs
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.hist <- function(x, ...)
{
    usr <- par("usr")
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(icu[,c("proportion", "capacity", "n_staff")], diag.panel = panel.hist, upper.panel = panel.cor)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
mod1 <- glm(cbind(mrse, fail) ~ n_staff * policy + capacity, 
            family = binomial(link = "logit"), 
            data = icu)


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Model diagnostic plots:
# Residuals vs Fitted
  # We see some possible patterns going on in the data but for the most part
  # we see relatively constant variation. I am slightly concerned about the "pattern", but 
  # I'll use the Scale-Location plot to help me gauge how if I should be concerned.
  # Specifically, the apparent "pinching" that occurs at a predicted value of ca.
  # -1 (i.e. -1 on the logit link scale, or ca. 25% probability). This may indicate
  # a problem but it may just be because we don't have many observations with that
  # predicted value.
  # Note that the "streak" of residuals at the bottom left of the figure, going 
  # from ca. -2 to -5 (below the dashed line) are instances where we have low
  # predicted logit values, hence low probabilities, so we are approaching the lower
  # bound of the data (i.e. 0%). In these scenarios, we're always going to see a "streak"
  # of residual values that tend towards 0 residual error.
# Q-Q Residuals
  # We completely ignore this figure for GLMs.
# Scale-Location
  # There seems to be less obvious patterns when we view the data here, compared
  # to when we look at the Residuals vs Fitted figure. I am now more comfortable
  # that there's not much going on in the residuals, in terms of patterns.
# Residuals vs Leverage
  # We're really only using this to check for values close to a Cook's distance of 1
  # For this plot, we can just make out the 0.5 dashed line in the top right, so 
  # we have no highly influential points in the data.
# Overall, the diagnostic plots look ok. Not perfect, but good enough that I'd be 
  # comfortable interpreting the results.

# To do a quick and crude check for dispersion, we can use the information from summary()
  # We take residual deviance and divide by the degrees of freedom, so for this model:
  # 229.78/205 = 1.12
  # We have very, very slight overdispersion but nothing that causes me concern!

# From these diagnostic checks, I see no issue for why we can't proceed with interpretation.

# While you might be suspicious that the first model we ran apparently has no issues. 
  # This does happen with real analysis. The risk when this happens, though, is that 
  # sometimes researchers feel compelled to run alternative models to try and "break" 
  # things, or think "maybe we can get away with adding a bit more complexity". I'd 
  # argue neither are appropriate. We set out to answer a question and we've now confirmed,
  # to the best of our ability, that our model is sound. There might be other issues
  # (e.g. the assumption of validity) but this model is perfectly adequate for 
  # doing what we wanted it to do. We're more than justified in running this one
  # model without torturing ourselves further.

par(mfrow = c(2,2)) # Show figures in 2 rows and 2 columns
plot(mod1)          # Plot the model diagnostics
par(mfrow = c(1,1)) # Reset so only 1 figure is shown
summary(mod1)       # Get the summary of the model (to check dispersion)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(mod1)
#                               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -1.90007    0.60588  -3.136  0.00171 ** 
# n_staff                       -0.12192    0.02744  -4.442 8.90e-06 ***
# policyNot implemented          1.04076    0.58723   1.772  0.07634 .  
# capacity                       2.37859    0.34223   6.950 3.65e-12 ***
# n_staff:policyNot implemented  0.07109    0.02930   2.426  0.01525 *  

# (Intercept)
  # This is the predicted value (on the link scale) when all other covariates
  # are set to zero. For "policy", this is when the policy *was* implemented. We know
  # that because the "policy" Estimate (further down) is for "Not implemented", 
  # meaning R has "created" a column of data called "Not implemented" and used 1 to 
  # mean "Yes" and 0 to mean "No". Why did R choose "Implemented" to be our reference? 
  # Because "I" is before "N" in the alphabet.
  # The Estimate value of -1.9 means that when capacity is 0, n_staff
  # is 0, and the policy was not implemented, we estimate -1.9 patients would be
  # MRSE positive (on the link scale).
# n_staff
  # This is the predicted slope for n_staff on the link scale *when the policy was
  # implemented. This caveat comes about because we have an interaction between 
  # policy and n_staff. This means we interpret this Estimate as: for every additional
  # member of staff in a ward *when the policy was implemented*, the proportion 
  # of patients with MRSE *decreases* by -0.12 (we know it decreases because the
  # estimate is negative).
# policyNot implemented
  # This is the difference between when the policy is *not implemented*, compared
  # to when it is implemented. Here, the value of 1.04 means that when the policy
  # is not implemented, we would expect *more* patients to be MRSE positive.
  # Specifically, -1.90007 + 1.04076 (Intercept + policyNot implemented).
  # This would imply the policy is effective.
# capacity
  # This is the slope for ward capacity. For every additional unit increase in capacity
  # the number of MRSE patients *increases* by 2.37. We need to be a little careful
  # here and remind ourselves what a unit increase in capacity represents. Capacity
  # is the proportion of beds occupied in a ward, meaning it can vary between 0
  # and 1. So a unit increase is going from 0% to 100% - from nothing to everything.
  # If we wanted the capacity Estimate to be more intuitive, we could multiple the
  # explanatory variable (in our mrse dataframe) by 100 and rerun our analysis. 
  # If we did so, a unit increase would become an increase in 1%, rather than 100%.
# n_staff:policyNot implemented
  # This is our interaction, which in this case, is the difference in slope 
  # between the slope for n_staff when the policy was implemented (n_staff Estimate) 
  # with the compared to the slope for n_staff when we do not implement the policy.
  # *Importantly*, the slope is not 0.07109. Remember, it's the difference in slope
  # compared to the slope for n_staff when the policy was implemented. Here, the
  # slope is: -0.12192 + 0.07109 = -0.05083


## ----Q8.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, fig.show= ifelse(TRUE, "asis", "hide")----
# Create a fake dataset to feed into our model equation
synth_data <- expand.grid(
  # Set n_staff to median (22 staff)
  n_staff = median(icu$n_staff), 
  # We have to set policy to either Not implemented or Implemented
  policy = "Implemented",        
  capacity = seq(
    # We create a sequence from the minimum capacity observered
    from = min(icu$capacity), 
    # To the maximum capacity observed
    to = max(icu$capacity),   
    # And request 20 values across that range
    length.out = 20))         

# Get predictions on the response scale
synth_data$pred <- predict(mod1, newdata = synth_data, type = "response")
# Or
# synth_data$pred <- plogis(predict(mod1, newdata = synth_data))

# Plot the raw data
plot(icu$capacity, icu$proportion, xlab= "Ward capacity", ylab= "Proportion of patients with MRSE")
# And add a line to show our predicted model fit
lines(synth_data$pred ~ synth_data$capacity, lty = 1, col = 1)


## ----Q8.2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# Create a fake dataset to feed into our model equation
synth_data <- expand.grid(n_staff = seq(from = min(icu$n_staff), to = max(icu$n_staff), length.out = 20), 
                          policy = c("Implemented", "Not implemented"),
                          capacity = 0.5)

synth_data$pred <- predict(mod1, newdata = synth_data, type = "response")

plot(icu$n_staff, icu$proportion, col= icu$policy, xlab= "Number of staff", ylab= "Proportion of patients with MRSE")

lines(synth_data$pred[synth_data$policy == "Implemented"] ~ 
      synth_data$n_staff[synth_data$policy == "Implemented"], lty= 1, col= 1)
lines(synth_data$pred[synth_data$policy == "Not implemented"] ~ 
      synth_data$n_staff[synth_data$policy == "Not implemented"], lty= 1, col= 2)

legend("topright", 
 legend= c("Implemented", "Not implemented"), 
 col= c(2:1), 
 lty= c(1, 1, 1), 
 lwd= c(1, 1, 1))


## ----Q9.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, fig.show= ifelse(TRUE, "asis", "hide")----
# Create a fake dataset to feed into our model equation
synth_data <- expand.grid(
  # Set n_staff to median (22 staff)
  n_staff = median(icu$n_staff), 
  # We have to set policy to either Not implemented or Implemented
  policy = "Implemented",        
  capacity = seq(
    # We create a sequence from the minimum capacity observered
    from = min(icu$capacity), 
    # To the maximum capacity observed
    to = max(icu$capacity),   
    # And request 20 values across that range
    length.out = 20))         

# Get both Estimate and Standard Error for each combination of our synthetic data
pred <- predict(mod1, newdata = synth_data, se.fit = TRUE)

# Extract the mean Estimate on response scale
synth_data$pred <- plogis(pred$fit)
# Subtract SE * 1.96 to get 95% and backtransform for lower 95% CI
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
# Add SE * 1.96 to get 95% and backtransform for upper 95% CI
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot the raw data
plot(icu$capacity, icu$proportion, 
     xlab = "Ward capacity", 
     ylab = "Proportion of patients with MRSE")
# And add a line to show our predicted model fit
lines(synth_data$pred ~ synth_data$capacity, lty = 1, col = "black")
lines(synth_data$low ~ synth_data$capacity, lty = 2, col = "grey")
lines(synth_data$upp ~ synth_data$capacity, lty = 2, col = "grey")


## ----Q9.2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, fig.show= ifelse(TRUE, "asis", "hide")----
library(ggplot2) # not needed if you already had ggplot loaded
library(scales)  # to show y-axis in figure as percentage (install.packages("scales") if needed)

# Create synthetic data and predictions
synth_data <- expand.grid(n_staff = seq(from = min(icu$n_staff), to = max(icu$n_staff), length.out = 20), 
                          policy = c("Implemented", "Not implemented"),
                          capacity = 0.5)      

# Get both Estimate and Standard Error for each combination of our synthetic data
pred <- predict(mod1, newdata = synth_data, se.fit = TRUE)

# Extract the mean Estimate on response scale
synth_data$pred <- plogis(pred$fit)
# Subtract SE * 1.96 to get 95% and backtransform for lower 95% CI
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
# Add SE * 1.96 to get 95% and backtransform for upper 95% CI
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Create the figure
ggplot() +
  geom_point(data = icu, aes(x = n_staff, y = proportion, colour = policy),
             show.legend = FALSE, size = 0.5) +
  geom_line(data = synth_data, aes(x = n_staff, y = pred, colour = policy),
             show.legend = FALSE) +
  geom_ribbon(data = synth_data, aes(x = n_staff, ymin = low, ymax = upp, fill = policy),
              alpha = 0.3, show.legend = FALSE) +
  facet_wrap(~policy) +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Number of ICU staff",
       y = "Predicted proportion\nof MRSE positive patients",
       caption = "Assuming 50% bed occupancy")

