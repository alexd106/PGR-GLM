seed <- 1988
set.seed(seed)

N <- 10000

# Netflix has data on 15000 users who had never seen lord of the rings
# Want to predict if user will watch lotr
# Based on:
  # Age
  # Number of movies liked from fantasy genre
  # Number of movies liked with at least one of lotr actors
  # What proportion of users who are "similar" to current user rated lotr
  # Mean time user spent watching movies of similar genre
  # Current time of day

# Covariates --------------------------------------------------------------
age <- round(rgamma(N, shape = 15, rate = 0.5), digits = 0)
genre_likes <- rpois(N, lambda = 2)
actor_likes <- rpois(N, lambda = 3)
similar_user_ratings <- rbeta(N, shape1 = 3, shape2 = 1.5)
mean_time_genre <- rnorm(N, mean = 120, sd = 60)
mean_time_genre <- ifelse(mean_time_genre < 0, 0, mean_time_genre)
user_time <- factor(sample(c("Morning", "Afternoon", "Evening"), 
                           size = N, replace = TRUE, 
                           prob = c(0.1, 0.3, 0.6)),
                    levels = c("Morning", "Afternoon", "Evening"))
user_day <- factor(sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                          size = N, replace = TRUE, 
                          prob = c(0.11, 0.09, 0.1, 0.12, 0.18, 0.19, 0.21)),
                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
user_device <- factor(sample(c("TV", "Mobile Device", "PC", "Gaming Device"), 
                             size = N, replace = TRUE, 
                             prob = c(0.68, 0.14, 0.08, 0.1)),
                      levels = c("TV", "Mobile Device", "PC", "Gaming Device"))

netflix <- data.frame(age, genre_likes, actor_likes, similar_user_ratings, mean_time_genre, user_time, user_day, user_device)

netflix$similar_user_ratings <- round(netflix$similar_user_ratings, digits = 2)
netflix$mean_time_genre <- round(netflix$mean_time_genre, digits = 1)

netflix$Morning <- ifelse(netflix$user_time == "Morning", 1, 0)
netflix$Afternoon <- ifelse(netflix$user_time == "Afternoon", 1, 0)
netflix$Evening <- ifelse(netflix$user_time == "Evening", 1, 0)

netflix$Monday <- ifelse(netflix$user_day == "Monday", 1, 0)
netflix$Tuesday <- ifelse(netflix$user_day == "Tuesday", 1, 0)
netflix$Wednesday <- ifelse(netflix$user_day == "Wednesday", 1, 0)
netflix$Thursday <- ifelse(netflix$user_day == "Thursday", 1, 0)
netflix$Friday <- ifelse(netflix$user_day == "Friday", 1, 0)
netflix$Saturday <- ifelse(netflix$user_day == "Saturday", 1, 0)
netflix$Sunday <- ifelse(netflix$user_day == "Sunday", 1, 0)

netflix$TV <- ifelse(netflix$user_device == "TV", 1, 0)
netflix$Mobile <- ifelse(netflix$user_device == "Mobile Device", 1, 0)
netflix$PC <- ifelse(netflix$user_device == "PC", 1, 0)
netflix$Gaming <- ifelse(netflix$user_device == "Gaming Device", 1, 0)

# Age
# genre_likes
# actor_likes
# similar_user_ratings
# mean_time_genre

# Morning
# Afternoon
# Evening

# Monday
# Tuesday
# Wednesday
# Thursday
# Friday
# Saturday
# Sunday

# TV
# Mobile
# PC
# Gaming

b0 <- -15
# Sine wave for age
A <- 0.5       # amplitude
omega <- 0.15 # rate of change
gamma <- pi-0.5  # starting phase
b1 <- -1    # "offset"

# Remaining linear parameters
b2 <- 0.12 # genre_likes slope
b3 <- 0.08 # actor_likes slope
b4 <- 13.2  # similar_user_ratings slope
b5 <- 0.02 # mean_time_genre
b6 <- 0.2 # Afternoon
b7 <- 0.8 # Evening
b8 <- -0.2 # Tuesday
b9 <- -0.4 # Wednesday
b10 <- -0.8 # Thursday
b11 <- 0.1 # Friday
b12 <- 0.3 # Saturday
b13 <- 1.2 # Sunday
b14 <- -2 # Mobile
b15 <- 0.2 # PC
b16 <- -1.2 # Gaming

netflix$lotr <- rbinom(N, size = 1, prob = plogis(b0 + 
                                                    # Sine wave for age
                                                    (b1 + (A + sin(omega * netflix$age + gamma))) +
                                                    
                                                    # Remaining linear effects
                                                    b2 * netflix$genre_likes + 
                                                    b3 * netflix$actor_likes + 
                                                    b4 * netflix$similar_user_ratings + 
                                                    b5 * netflix$mean_time_genre + 
                                                    b6 * netflix$Afternoon + 
                                                    b7 * netflix$Evening + 
                                                    b8 * netflix$Tuesday + 
                                                    b9 * netflix$Wednesday + 
                                                    b10 * netflix$Thursday + 
                                                    b11 * netflix$Friday + 
                                                    b12 * netflix$Saturday + 
                                                    b13 * netflix$Sunday + 
                                                    b14 * netflix$Mobile + 
                                                    b15 * netflix$PC + 
                                                    b16 * netflix$Gaming
                                                  ))

# Red herring variables ---------------------------------------------------
netflix$premium <- factor(ifelse(rbinom(nrow(netflix), size = 1, prob = 0.2) == 1, "Premium", "Standard"))
netflix$fam_members <- rpois(nrow(netflix), lambda = 2)
netflix$country <- factor(sample(c("US", "UK", "DE", "FR", "JP", "NZ", "MX"), 
                                 size = nrow(netflix), replace = TRUE, 
                                 prob = c(0.32, 0.19, 0.16, 0.14, 0.02, 0.05, 0.12)),
                          levels = c("US", "UK", "DE", "FR", "JP", "NZ", "MX"))

netflix <- netflix[,c("lotr", "premium", "age", "genre_likes", "actor_likes", "similar_user_ratings",
                      "mean_time_genre", "user_time", "user_day", "user_device", "fam_members", "country")]
# 
# library(ggplot2)
# p1 <- ggplot(netflix) +
#   geom_jitter(aes(x = age, y = lotr), alpha = 0.1)
# 
# p2 <- ggplot(netflix) +
#   geom_jitter(aes(x = genre_likes, y = lotr), alpha = 0.1)
# 
# p3 <- ggplot(netflix) +
#   geom_jitter(aes(x = actor_likes, y = lotr), alpha = 0.1)
# 
# p4 <- ggplot(netflix) +
#   geom_jitter(aes(x = similar_user_ratings, y = lotr), alpha = 0.1)
# 
# p5 <- ggplot(netflix) +
#   geom_jitter(aes(x = mean_time_genre, y = lotr), alpha = 0.1)
# 
# p6 <- ggplot(netflix) +
#   geom_jitter(aes(x = user_time, y = lotr), alpha = 0.1)
# 
# p7 <- ggplot(netflix) +
#   geom_jitter(aes(x = user_day, y = lotr), alpha = 0.1)
# 
# p8 <- ggplot(netflix) +
#   geom_jitter(aes(x = user_device, y = lotr), alpha = 0.1)
# 
# p9 <- ggplot(netflix) +
#   geom_jitter(aes(x = premium, y = lotr), alpha = 0.1)
# 
# p10 <- ggplot(netflix) +
#   geom_jitter(aes(x = fam_members, y = lotr), alpha = 0.1)
# 
# p11 <- ggplot(netflix) +
#   geom_jitter(aes(x = country, y = lotr), alpha = 0.1)
# 
# library(patchwork)
# p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11

# # Full model
# mod1 <- glm(lotr ~ age + genre_likes + actor_likes + similar_user_ratings + mean_time_genre + user_time + user_day + user_device +
#               premium + fam_members + country,
#             data = netflix,
#             family = binomial)
# par(mfrow = c(2,2))
# plot(mod1)
# par(mfrow = c(1,1))
# netflix$resid <- resid(mod1, type = "pearson")
# par(mfrow = c(2,3))
# arm::binnedplot(x = netflix$age, y = netflix$resid, xlab = "Age", nclass = 10)
# arm::binnedplot(x = netflix$fam_members, y = netflix$resid, xlab = "N Family", nclass = 10)
# arm::binnedplot(x = netflix$mean_time_genre, y = netflix$resid, xlab = "Mean time", nclass = 10)
# arm::binnedplot(x = netflix$similar_user_ratings, y = netflix$resid, xlab = "User rating", nclass = 10)
# arm::binnedplot(x = netflix$actor_likes, y = netflix$resid, xlab = "Actor likes", nclass = 10)
# arm::binnedplot(x = netflix$genre_likes, y = netflix$resid, xlab = "Genre likes", nclass = 10)
# 
# 
# summary(mod1)
# drop1(mod1)
# 
# plot(ggeffects::ggpredict(mod1, terms = c("age [all]")))
# plot(ggeffects::ggpredict(mod1))

mod2 <- glm(lotr ~ age + genre_likes + actor_likes + similar_user_ratings + mean_time_genre + user_time + user_day + user_device,
            data = netflix,
            family = binomial)
plot(ggeffects::ggpredict(mod2))
summary(mod2)
# netflix$resid <- resid(mod2, type = "pearson")
# 
# par(mfrow = c(2,3))
# arm::binnedplot(x = netflix$age, y = netflix$resid, xlab = "Age", nclass = 10)
# arm::binnedplot(x = netflix$fam_members, y = netflix$resid, xlab = "N Family", nclass = 10)
# arm::binnedplot(x = netflix$mean_time_genre, y = netflix$resid, xlab = "Mean time", nclass = 10)
# arm::binnedplot(x = netflix$similar_user_ratings, y = netflix$resid, xlab = "User rating", nclass = 10)
# arm::binnedplot(x = netflix$actor_likes, y = netflix$resid, xlab = "Actor likes", nclass = 10)
# arm::binnedplot(x = netflix$genre_likes, y = netflix$resid, xlab = "Genre likes", nclass = 10)

# # True model
# library(mgcv)
# mod2 <- gam(lotr ~ s(age, bs = "cr", k = 20) + genre_likes + actor_likes + similar_user_ratings + mean_time_genre + user_time + user_day + user_device,
#             data = netflix,
#             family = binomial,
#             method = "REML")
# par(mfrow = c(2,2))
# plot(mod2)
# par(mfrow = c(1,1))
# summary(mod2)
# drop1(mod2)
# 
# plot(ggeffects::ggpredict(mod2, terms = c("age [all]")))
# 
write.table(netflix, "data//netflix.txt", row.names = FALSE)
