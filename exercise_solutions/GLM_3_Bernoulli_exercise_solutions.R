## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Using the available covariates, can Lord of the Rings engagement be predicted 
# and can we make this prediction as efficient as possible?


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# With the blanks filled in, the equation should be:
# logit(p_i) = beta_0 + beta_1 * Age_i + beta_2 * Evening_i + beta_3 * Morning_i

# Why have I included Evening and then Morning, and not Afternoon?
# Because R will take the level that appears first alphanumerically, and use 
# that as the intercept (here *A*fternoon)
# If we wanted Morning to be the reference level, what R code would we use?


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Load in the data
netflix <- read.table(file= "./data/netflix.txt", header = TRUE)

# Check the structure of the data
# When we do so, we can see a few covariates have been read in a characters
# We want to convert these to factors, and impart an order to some of them
# Including an order will make our figures more legible and our results more
# intuitive.
str(netflix)

# Change variables from characters to factors
# These factors have no particular order to them
  # so I am happy to just leave these in alphabetical order
netflix$premium <- factor(netflix$premium)
netflix$user_device <- factor(netflix$user_device)
netflix$country <- factor(netflix$country)

# For time and day, however, I do want the natural order to be implemented
# To do so, we use the levels = argument, and specify the order we want
# by writing it out within the c() function.
# Note you cannot have typos here - be sure of your spelling
netflix$user_time <- factor(netflix$user_time, 
                            levels = c("Morning", "Afternoon", "Evening"))
netflix$user_day <- factor(netflix$user_day, 
                           levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", 
                                      "Sunday"))


## ----Q4.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# install.packages("ggplot2") # Run this line of code if you do not have ggplot installed
# Once installed, load the package
library(ggplot2)

# I'll use another package called `patchwork` that helps combine ggplot figure together
# install.packages("patchwork") # Run this line of code if you do not have patchwork installed
# Once installed, load the package
library(patchwork)

# Not needed but I'm downloading netflix icon to add to figure to give that corporate feel
# If you want to do this, you'll need to install the `png` package
library(png)
logo <- readPNG("./images/netflix.png")

# Bar chart showing total number of views according to account type
p1 <- ggplot(netflix) +
  geom_jitter(aes(x = premium, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  labs(x = "Subscription type",
       y = "Viewed\nLord of The Rings") +  
  theme_classic() +
  annotation_raster(logo, 
                    xmin = 0.5, xmax = 1,
                    ymin = 4000, ymax = 6000)

# A scatterplot for lotr ~ age, jittered
p2 <- ggplot(netflix) +
  geom_jitter(aes(x = age, y = lotr), 
              width = 0, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "User age",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# A scatterplot for lotr ~ genre_likes, jittered
p3 <- ggplot(netflix) +
  geom_jitter(aes(x = genre_likes, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Number of fantasy likes",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# A scatterplot for lotr ~ actor_likes, jittered
p4 <- ggplot(netflix) +
  geom_jitter(aes(x = actor_likes, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Number of films with same actor liked",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# Using the patchwork package I stich p1 to p4 together into a single figure
(p1 + p2) / (p3 + p4)

# A scatterplot for lotr ~ similar_user_ratings, jittered
p5 <- ggplot(netflix) +
  geom_jitter(aes(x = similar_user_ratings, y = lotr), 
              width = 0, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Proportion of similar users who liked LoTR",
       y = "Viewed\nLord of The Rings") +
  theme_classic()


# A scatterplot for lotr ~ mean_time_genre, jittered
p6 <- ggplot(netflix) +
  geom_jitter(aes(x = mean_time_genre, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Mean time spent watching fantasy films",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# A scatterplot for lotr ~ user_time, jittered
p7 <- ggplot(netflix) +
  geom_jitter(aes(x = user_time, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Time of day",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# A scatterplot for lotr ~ user_day, jittered
p8 <- ggplot(netflix) +
  geom_jitter(aes(x = user_day, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Day of week",
       y = "Viewed\nLord of The Rings") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust = 0.3))


# Using the patchwork package I stich p1 to p4 together into a single figure
(p5 + p6) / (p7 + p8)

# A scatterplot for lotr ~ fam_members, jittered
p9 <- ggplot(netflix) +
    annotation_raster(logo,
                      xmin = 8.3, xmax = 9.6,
                      ymin = -0.3, ymax = 0.3) +
  geom_jitter(aes(x = fam_members, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Number of accounts",
       y = "Viewed\nLord of The Rings") +
  theme_classic()
  
# A scatterplot for lotr ~ user_device, jittered
p10 <- ggplot(netflix) +
  geom_jitter(aes(x = user_device, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "User device",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# A scatterplot for lotr ~ country, jittered
p11 <- ggplot(netflix) +
  geom_jitter(aes(x = country, y = lotr), 
              width = 0.3, height = 0.3,
              colour = "#E50914", alpha = 0.05) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "No", ifelse(y == 1, "Yes", y)),
                     breaks = c(0, 1)) +
  labs(x = "Country of registration",
       y = "Viewed\nLord of The Rings") +
  theme_classic()

# Using the patchwork package I stich p1 to p4 together into a single figure
(p10 + p11) / p9


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
mod1 <- glm(lotr ~ premium + age + genre_likes + actor_likes + similar_user_ratings +
              mean_time_genre + user_time + user_day + user_device + fam_members + country, 
            family = binomial(link = "logit"), 
            data = netflix)


## ----Q6.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
drop1(mod1)

# From the output, we're looking for when removing a covariate lowers AIC by 2 units
# If this happens, it means removing that covariate improves predictive power
  # From the above model, these include:
  # premium
  # country
  # fam_members
# fam_members is right on the threshold with a DeltaAIC of 2
  # Be aware there is some controversy in such cases
  # My personal view is that, based on the penalisation of 2k (from the AIC 
  # equation), variables with exactly 2 AIC are not informative


## ----Q6.2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
mod2 <- glm(lotr ~ age + genre_likes + actor_likes + similar_user_ratings +
              mean_time_genre + user_time + user_day + user_device + fam_members, 
            family = binomial(link = "logit"), 
            data = netflix)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Model diagnostic plots:
# Residuals vs Fitted
  # Normally we want to see no weird patterns, but Bernoulli's always have weird 
  # patterns. This figure is now pretty useless.
# Q-Q Residuals
  # We completely ignore this figure for GLMs.
# Scale-Location
  # Useless for Bernoulli GLMs
# Residuals vs Leverage
  # Still useful, even for Bernoulli GLMs. We don't want any points to be close
  # to a Cook's distance of 1. Here we're pretty safe.
# Overall, the diagnostic plots are not useful for Bernoulli GlMs, other than
  # the Residuals vs Leverage check (which had no issue).

# Ideally we would do a check for over-dispersion, but we can't do this Bernoulli's,
  # so we ignore over-dispersion

# We're now in a position where we don't really know how well our model has performed
  # with the above checks. What to do?

par(mfrow = c(2,2)) # Show figures in 2 rows and 2 columns
plot(mod2, col = ifelse(netflix$lotr == 1, "red", "black")) # Plot diagnostics
par(mfrow = c(1,1)) # Reset so only 1 figure is shown
summary(mod2)       # Get the summary of the model (to check dispersion?)


## ----Q7.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# Extract residuals from model
netflix$resid <- resid(mod2, type = "pearson")

# Using binnedplot() from arm package
library(arm)

# For the most part, the figures below show no substantial issues, with one exception
# It looks very likely that age is not a linear effect. We can see some evidence of 
  # this as we go from lots of negative, to lots of positive error with only a single
  # bin actually contained within our bounds.
  # Clearly, the linear trend we've fit is struggling to deal with age and it seems
  # very likely that the assumption of linearity with age is broken

# What should we do now?


# For categorical covariates in the model
par(mfrow = c(2,2))
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_time), nclass = 3, xlab = "User time")
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_day), nclass = 7, xlab = "User day")
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_device), nclass = 4, xlab = "User device")

par(mfrow = c(3,2))
# For continuous covariates in the model
binnedplot(y = netflix$resid, x = netflix$age, nclass = 6, xlab = "Age")
binnedplot(y = netflix$resid, x = netflix$genre_likes, nclass = 4, xlab = "Genre likes")
binnedplot(y = netflix$resid, x = netflix$actor_likes, nclass = 4, xlab = "Actor likes")
binnedplot(y = netflix$resid, x = netflix$similar_user_ratings, nclass = 4, xlab = "Similar ratings")
binnedplot(y = netflix$resid, x = netflix$mean_time_genre, nclass = 4, xlab = "Mean time spent watching fantasy")
binnedplot(y = netflix$resid, x = netflix$fam_members, nclass = 4, xlab = "Number of family members")


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(mod2)
# How many parameters have been estimated by this model?
  # There are a total of 18 parameters in this model. The number of parameters is

# How might we want to change data collection methods to make the model more efficient in terms of the number of parameters?
  # We have more parameter than covariates because of the categorical variables
  # So just three covariates (user_time, user_day and user_device) are using 11 parameters
  # Two of these (user_time and user_day) could be expressed as numbers but there 
  # are two complications if we do so.
  # For user_time, we have not been provided with the actual time, just a broad
  # window. So we'd need more accurate information to resolve this.
  # For user_day, we can already convert this to numeric, e.g. Monday = 1,
  # Sunday = 7 but this does create an issue. Because weeks are cyclical, the 
  # predicted probability to watch LoTR on Sunday needs to match up, in some manner,
  # with the predicated probability to watch LoTR on Monday.
  # There are techniques to do this, but we don't have the time to cover them on
  # this course. In such cases, having day of week as a categorical is actually a
  # pretty good work-around. If you're interested in the alternative way, look up
  # cyclical splines in Generalised Additive Models.

# * What does the `user_dayFriday` `Estimate` represent?
  # Hopefully this is starting to become second nature at this point. This is the 
  # difference in logit value for Friday, compared to Monday (our reference level).
  # For Friday, this model would predict a user is *less* likely to watch LoTR
  # than on Monday (changing the intercept by -0.0553916).
  # If your summary table looks different, it's most likely because you did not
  # set Monday to be your reference in user_day and Morning to be your reference
  # in user_time (or you set the user_device reference to something other than
  # Gaming Device). It's absolutely fine if so - all it means is that your
  # Estimate values will be different but, importantly, the effects will be exactly 
  # the same.
  # The corresponding P-value indicates that there's a 59% chance that this difference
  # is actually 0.

# * For every additional family member (`fam_members`), how much does the log odds 
# ratio increase by?
  # Adding one more family member decreases the logit value to watch LoTR by
  # -0.0055314. According to this model, single people are *very* slightly more 
  # likely to watch LoTR.


## -----------------------------------------------------------------------------
display(mod2)

# display() includes a reminder of the model you ran,
# The parameters and their estimate (coef.est), where
# coefficient is another name for parameter estimate.
# The standard error for each parameter (coef.se).
# An indicator of how many samples were used in fitting (n = )
# An indicator of how many parameters were estimated by the model (k = )
# And a report of residual deviance and null deviance.

# Personally, I prefer display() for being more concise and removing 
# information I do not care about


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, fig.show= ifelse(TRUE, "asis", "hide")----

# Load packages for plotting
library(ggplot2)
library(scales)
library(patchwork)

###### Age ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # Age is variable of interest for prediction
  age = seq(from = min(netflix$age), to = max(netflix$age), length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  genre_likes = median(netflix$genre_likes), 
  actor_likes = median(netflix$actor_likes),    
  similar_user_ratings = median(netflix$similar_user_ratings),
  mean_time_genre = median(netflix$mean_time_genre),
  fam_members = median(netflix$fam_members)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for age
p1 <- ggplot(synth_data, aes(x = age, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Age of user",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

###### Genre likes ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # genre_likes is variable of interest for prediction
  genre_likes = seq(from = min(netflix$genre_likes), to = max(netflix$genre_likes), length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  age = median(netflix$age), 
  actor_likes = median(netflix$actor_likes),    
  similar_user_ratings = median(netflix$similar_user_ratings),
  mean_time_genre = median(netflix$mean_time_genre),
  fam_members = median(netflix$fam_members)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for genre_likes
p2 <- ggplot(synth_data, aes(x = genre_likes, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Number of fantasy\nmedia liked",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

###### Actor likes ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # actor_likes is variable of interest for prediction
  actor_likes = seq(from = min(netflix$actor_likes), to = max(netflix$actor_likes), length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  age = median(netflix$age), 
  genre_likes = median(netflix$genre_likes),    
  similar_user_ratings = median(netflix$similar_user_ratings),
  mean_time_genre = median(netflix$mean_time_genre),
  fam_members = median(netflix$fam_members)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for actor_likes
p3 <- ggplot(synth_data, aes(x = actor_likes, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Number of media with\ncast member liked",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

###### similar_user_ratings ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # similar_user_ratings is variable of interest for prediction
  similar_user_ratings = seq(from = min(netflix$similar_user_ratings), 
                             to = max(netflix$similar_user_ratings), 
                             length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  genre_likes = median(netflix$genre_likes), 
  actor_likes = median(netflix$actor_likes),    
  age = median(netflix$age),
  mean_time_genre = median(netflix$mean_time_genre),
  fam_members = median(netflix$fam_members)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for similar_user_ratings
p4 <- ggplot(synth_data, aes(x = similar_user_ratings, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Proportion of similar\nusers who liked LoTR",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

###### mean_time_genre ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # mean_time_genre is variable of interest for prediction
  mean_time_genre = seq(from = min(netflix$mean_time_genre), 
                        to = max(netflix$mean_time_genre), 
                        length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  genre_likes = median(netflix$genre_likes), 
  actor_likes = median(netflix$actor_likes),    
  similar_user_ratings = median(netflix$similar_user_ratings),
  age = median(netflix$age),
  fam_members = median(netflix$fam_members)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for mean_time_genre
p5 <- ggplot(synth_data, aes(x = mean_time_genre, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Mean time spent\nwatching fantasy",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

# Add all multiples into a single figure
design_layout <- "
AA
BC
DE
"
p1 + p2 + p3 + p4 + p5 + 
  plot_layout(guides = "collect", axis_titles = "collect", design = design_layout)


## ----Q9.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# Netflix should target:
  # Users watching on TV or PC
  # Users over the age of 40
    # (but remember that we think it's very likely this is non-linear)
  # Users for whom >75% of similar users liked LoTR
  # Users who have spent > 200 hours on average watching fantasy
# The following do help making a prediction but are weak
  # Users who have liked > 5 fantasy films
  # Users who have liked > 7.5 films with same actors

