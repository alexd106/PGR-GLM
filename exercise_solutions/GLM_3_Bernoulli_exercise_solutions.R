## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# Using the available covariates, can Lord of the Rings engagement be predicted and can we make this prediction as efficient as possible?


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# With the blanks filled in, the equation should be:
# logit(p_i) = beta_0 + beta_1 * Age_i + beta_2 * Evening_i + beta_3 * Morning_i

# Why have I included Evening and then Morning, and not Afternoon?
# Because R will take the level that appears first alphanumerically, and use 
# that as the intercept (here *A*fternoon)
# If we wanted Morning to be the reference level, what R code would we use?


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
netflix <- read.table(file= "./data/netflix.txt", header = TRUE)

# Change variables from characters to factors
netflix$user <- factor(netflix$user)
netflix$premium <- factor(netflix$premium)
netflix$user_time <- factor(netflix$user_time, levels = c("Morning", "Afternoon", "Evening"))
netflix$user_day <- factor(netflix$user_day, 
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
netflix$user_device <- factor(netflix$user_device)
netflix$country <- factor(netflix$country)

str(netflix)
# 'data.frame':	15000 obs. of  13 variables:
#  $ user                : Factor w/ 15000 levels "0014074f-9304-4349-b1df-565122630bfa",..: 13625 2447 4081 ...
#  $ lotr                : int  0 1 1 1 0 0 0 0 0 1 ...
#  $ premium             : Factor w/ 2 levels "Premium","Standard": 2 2 2 2 2 2 2 2 2 1 ...
#  $ age                 : int  30 26 27 39 37 37 43 40 36 38 ...
#  $ genre_likes         : int  2 2 2 2 0 2 1 5 2 5 ...
#  $ actor_likes         : int  3 2 4 0 4 3 2 0 4 6 ...
#  $ similar_user_ratings: num  0.62 0.67 0.92 0.75 0.46 0.59 0.77 0.62 0.67 0.54 ...
#  $ mean_time_genre     : num  38.3 66.9 118.4 217.4 127.8 ...
#  $ user_time           : Factor w/ 3 levels "Afternoon","Evening",..: 1 2 1 1 2 1 2 2 2 3 ...
#  $ user_day            : Factor w/ 7 levels "Friday","Monday",..: 5 2 3 4 1 3 1 4 3 3 ...
#  $ user_device         : Factor w/ 4 levels "Gaming Device",..: 4 4 4 4 4 4 4 4 1 2 ...
#  $ fam_members         : int  3 3 2 0 1 2 1 2 2 1 ...
#  $ country             : Factor w/ 7 levels "DE","FR","JP",..: 2 6 7 5 7 6 2 6 6 4 ...

summary(netflix)
 #                                   user            lotr            premium           age         genre_likes   
 # 0014074f-9304-4349-b1df-565122630bfa:    1   Min.   :0.0000   Premium : 3001   Min.   : 7.00   Min.   :0.000  
 # 0017781d-22a6-41d1-9c61-10427b20f846:    1   1st Qu.:0.0000   Standard:11999   1st Qu.:24.00   1st Qu.:1.000  
 # 001f0d02-282e-4812-87b3-723e978da027:    1   Median :1.0000                    Median :29.00   Median :2.000  
 # 0021ddf2-ef09-4abe-b27d-d3fa74fd92b7:    1   Mean   :0.5278                    Mean   :29.98   Mean   :1.998  
 # 0023c3d4-ba8f-4716-8c30-6d6859c77b85:    1   3rd Qu.:1.0000                    3rd Qu.:35.00   3rd Qu.:3.000  
 # 00295ab4-6930-4d7e-b678-b10a65c0f234:    1   Max.   :1.0000                    Max.   :79.00   Max.   :9.000  
 # (Other)                             :14994                                                                    
 #  actor_likes     similar_user_ratings mean_time_genre     user_time         user_day           user_device   
 # Min.   : 0.000   Min.   :0.0300       Min.   :  0.0   Afternoon:4419   Friday   :2724   Gaming Device: 1535  
 # 1st Qu.: 2.000   1st Qu.:0.5300       1st Qu.: 79.9   Evening  :9104   Monday   :1698   Mobile Device: 2066  
 # Median : 3.000   Median :0.6900       Median :120.1   Morning  :1477   Saturday :2792   PC           : 1223  
 # Mean   : 2.973   Mean   :0.6661       Mean   :120.5                    Sunday   :3150   TV           :10176  
 # 3rd Qu.: 4.000   3rd Qu.:0.8300       3rd Qu.:160.3                    Thursday :1805                        
 # Max.   :12.000   Max.   :1.0000       Max.   :344.2                    Tuesday  :1352                        
 #                                                                        Wednesday:1479                        
 #  fam_members    country  
 # Min.   :0.000   DE:2476  
 # 1st Qu.:1.000   FR:2119  
 # Median :2.000   JP: 302  
 # Mean   :1.992   MX:1753  
 # 3rd Qu.:3.000   NZ: 728  
 # Max.   :9.000   UK:2825  
 #                 US:4797  

# install.packages("ggplot2") # Run this line of code if you do not have ggplot installed
# Once installed, load the package
library(ggplot2)

# I'll use another package called `patchwork` that helps combine ggplot figure together
# install.packages("patchwork") # Run this line of code if you do not have patchwork installed
# Once installed, load the package
library(patchwork)

# Not needed but downloading netflix icon to add to figure to give that corporate feel
# If you want to do this, you'll need to install the `png` package
library(png)
logo <- readPNG("./images/netflix.png")

# Bar chart showing total number of views according to account type
p1 <- ggplot(netflix) +
  geom_col(aes(x = premium, y = lotr), linetype = 1,
           alpha = 0.8, colour = "#E50914") +
  labs(x = "Subscription type",
       y = "Total number of\nLord of The Rings views") +  
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
# Model:
# lotr ~ premium + age + genre_likes + actor_likes + similar_user_ratings + 
#     mean_time_genre + user_time + user_day + user_device + fam_members + 
#     country
#                      Df Deviance     AIC
# <none>                    8535.3  8585.3
# premium               1   8535.3  8583.3
# age                   1  14997.9 15045.9
# genre_likes           1   8576.9  8624.9
# actor_likes           1   8552.1  8600.1
# similar_user_ratings  1  15360.9 15408.9
# mean_time_genre       1  10091.2 10139.2
# user_time             2   8677.9  8723.9
# user_day              6   9012.2  9050.2
# user_device           3   9154.8  9198.8
# fam_members           1   8535.4  8583.4
# country               6   8543.7  8581.7

# From this, we're looking for when removing a covariate lowers AIC by 2 units
  # From the above model, these include:
  # premium
  # country
# fam_members is right on the threshold with a DeltaAIC of 1.9
  # There's just enough information in fam_members to warrant leaving it in the 
  # model


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
  # Uselss for Bernoulli GLMs
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
plot(mod1, col = ifelse(netflix$lotr == 1, "red", "black")) # Plot diagnostics
par(mfrow = c(1,1)) # Reset so only 1 figure is shown
summary(mod1)       # Get the summary of the model (to check dispersion?)


## ----Q7.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# Extract residuals from model
netflix$resid <- resid(mod2, type = "pearson")

# Using binnedplot() from arm package
library(arm)

# For categorical covariates in the model
par(mfrow = c(2,2))
# We convert the categories to numeric using as.numeric()
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_time), nclass = 3, xlab = "User time")
# Pretty good. Nothing that is concerning
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_day), nclass = 7, xlab = "User day")
# Pretty good also if slightly too negative
binnedplot(y = netflix$resid, x = as.numeric(netflix$user_device), nclass = 8, xlab = "User device")
# We have to set nclass = 8 to get a bin for each device (4 device types)
# But plot suggests no issues

par(mfrow = c(3,2))
# For continuous covariates in the model
binnedplot(y = netflix$resid, x = netflix$age, nclass = 6, xlab = "Age")
# Very likely that age is not a linear effect
  # We go from lots of negative, to lots of positive error
  # Model is struggling to deal with age
  # Assumption of linearity with age is likely broken here
binnedplot(y = netflix$resid, x = netflix$genre_likes, nclass = 4, xlab = "Genre likes")
# Pretty good - mostly evenly spread around 0
binnedplot(y = netflix$resid, x = netflix$actor_likes, nclass = 4, xlab = "Actor likes")
# Pretty good - all points within bounds
  # Slightly more negative than we might like but ok
binnedplot(y = netflix$resid, x = netflix$similar_user_ratings, nclass = 4, xlab = "Similar ratings")
# Mostly ok. At low similar rating we are below bounds, but not terribly
binnedplot(y = netflix$resid, x = netflix$mean_time_genre, nclass = 4, xlab = "Mean time spent watching fantasy")
# Pretty good. At 100 hours of watch time, we are ever-so-slightly below line but it's ok
binnedplot(y = netflix$resid, x = netflix$fam_members, nclass = 4, xlab = "Number of family members")
# Pretty good


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(mod2)
# Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -2.2170365  0.2170986 -10.212  < 2e-16 ***
# age                      -0.3437752  0.0065373 -52.587  < 2e-16 ***
# genre_likes               0.1249627  0.0195066   6.406 1.49e-10 ***
# actor_likes               0.0643134  0.0160260   4.013 5.99e-05 ***
# similar_user_ratings     12.9650321  0.2390317  54.240  < 2e-16 ***
# mean_time_genre           0.0192068  0.0005529  34.740  < 2e-16 ***
# user_timeAfternoon        0.1658946  0.0994637   1.668   0.0953 .  
# user_timeEvening          0.7851001  0.0937494   8.374  < 2e-16 ***
# user_dayTuesday          -0.2761958  0.1211418  -2.280   0.0226 *  
# user_dayWednesday        -0.5379722  0.1184921  -4.540 5.62e-06 ***
# user_dayThursday         -0.8373645  0.1137974  -7.358 1.86e-13 ***
# user_dayFriday           -0.0553916  0.1033254  -0.536   0.5919    
# user_daySaturday          0.2520935  0.1026561   2.456   0.0141 *  
# user_daySunday            1.0829502  0.1027404  10.541  < 2e-16 ***
# user_deviceMobile Device -0.9108165  0.1132527  -8.042 8.81e-16 ***
# user_devicePC             1.1081704  0.1304357   8.496  < 2e-16 ***
# user_deviceTV             1.0054722  0.0918184  10.951  < 2e-16 ***
# fam_members              -0.0055314  0.0191144  -0.289   0.7723    

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

###### fam_members ########

synth_data <- expand.grid(
  user_time = "Afternoon",
  user_day = "Sunday",
  # fam_members is variable of interest for prediction
  fam_members = seq(from = min(netflix$fam_members), 
                    to = max(netflix$fam_members), 
                    length.out = 50),
  # Will vary this across user device at the same time
  user_device = c("Gaming Device", "Mobile Device", "PC", "TV"),
  genre_likes = median(netflix$genre_likes), 
  actor_likes = median(netflix$actor_likes),    
  similar_user_ratings = median(netflix$similar_user_ratings),
  mean_time_genre = median(netflix$mean_time_genre),
  age = median(netflix$age)
)

# Get mean prediction and 95% CI on the response scale
pred <- predict(mod2, newdata = synth_data, se.fit = TRUE)
synth_data$pred <- plogis(pred$fit)
synth_data$low <- plogis(pred$fit - pred$se.fit * 1.96)
synth_data$upp <- plogis(pred$fit + pred$se.fit * 1.96)

# Plot for fam_members
p6 <- ggplot(synth_data, aes(x = fam_members, y = pred, ymin = low, ymax = upp, 
                       colour = user_device, fill = user_device)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  theme_minimal() +
  labs(x = "Number of family members",
       y = "Predicted probability to watch LoTR",
       colour = "User device",
       fill = "User device")

# Add all multiples into a single figure
design_layout <- "
AB
CD
EF
"
p1 + p2 + p3 + p4 + p5 + p6 + 
  plot_annotation(title = "Users to target") + 
  plot_layout(guides = "collect", axis_titles = "collect", design = design_layout)


## ----Q9.1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# Netflix should target:
  # Users watching on TV or PC
  # Users under the age of 40
  # Users for whom >75% of similar users liked LoTR
  # Users who have spent > 200 hours on average watching fantasy
# The following do have help making a prediction but are weak
  # Users who have liked > 5 fantasy films
  # Users who have liked > 7.5 films with same actors
  # Users with no family members on their account

