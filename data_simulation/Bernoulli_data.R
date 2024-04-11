seed <- 1988
set.seed(seed)

N <- 50000

# Netflix has data on 50000 users who had never seen lord of the rings
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
hist(age)
genre_likes <- rpois(N, lambda = 2)
hist(genre_likes)
actor_likes <- rpois(N, lambda = 3)
hist(actor_likes)
similar_user_ratings <- rbeta(N, shape1 = 3, shape2 = 1.5)
hist(similar_user_ratings)
mean_time_genre <- rnorm(N, mean = 120, sd = 60)
mean_time_genre <- ifelse(mean_time_genre < 0, 0, mean_time_genre)
hist(mean_time_genre)
user_time <- factor(sample(c("Morning", "Afternoon", "Evening"), 
                           size = N, replace = TRUE, 
                           prob = c(0.1, 0.3, 0.6)),
                    levels = c("Morning", "Afternoon", "Evening"))
plot(user_time)
user_day <- factor(sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                          size = N, replace = TRUE, 
                          prob = c(0.11, 0.09, 0.1, 0.12, 0.18, 0.19, 0.21)),
                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
plot(user_day)
user_device <- factor(sample(c("TV", "Mobile Device", "PC", "Gaming Device"), 
                             size = N, replace = TRUE, 
                             prob = c(0.68, 0.14, 0.08, 0.1)),
                      levels = c("TV", "Mobile Device", "PC", "Gaming Device"))
plot(user_device, xlab = "How users watch")

netflix <- data.frame(age, genre_likes, actor_likes, similar_user_ratings, mean_time_genre, user_time, user_day, user_device)

head(netflix)

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

b0 <- -8
b1 <- 0.05 # age slope
b2 <- 0.08 # genre_likes slope
b3 <- 0.1 # actor_likes slope
b4 <- 2.5  # similar_user_ratings slope
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
                                                    b1 * netflix$age +
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

netflix <- netflix[,c(1:8, 23)]

library(ggplot2)
ggplot(netflix) +
  geom_jitter(aes(x = age, y = lotr))

panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(netflix[,c("age", "genre_likes", "actor_likes", "similar_user_ratings", "mean_time_genre")], 
      upper.panel = panel.cor, 
      diag.panel = panel.hist)

#write.table(mrse, "data//mrse.txt", row.names = FALSE)

mod1 <- glm(lotr ~ .,
            data = netflix,
            family = binomial)

summary(mod1)
drop1(mod1)

plot(ggeffects::ggpredict(mod1))
