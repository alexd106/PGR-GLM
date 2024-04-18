# Data simulation for species richness as a function of biomass and pH
# I use Thomas' estimated coefficients for parameter values here
# But I have removed the interaction from this model (introduced in binomial)
# Also rounded down biomass to make it look less simulated (assuming previous data was sim)
# which will introduce measurement error but shouldn't affect estimates in meaningful way

seed <- 1988
set.seed(seed)
N <- 90
Biomass <- rlnorm(N, 0.7, 0.7)
pH <- rep(c("low", "mid", "high"), each = 30)
df <- data.frame(Biomass, pH)
df$med <- ifelse(df$pH == "mid", 1, 0)
df$high <- ifelse(df$pH == "high", 1, 0)

# (Intercept)  2.71255
# Biomass     -0.12756
# pHmid        0.69123
# pHhigh       1.13639

b0 <- 2.71255
b1 <- -0.12756
b2 <- 0.69123
b3 <- 1.13639

df$Species <- rpois(N, lambda = exp(b0 + b1 * df$Biomass + b2 * df$med + b3 * df$high))

sp <- df[,c("Species", "Biomass", "pH")]

sp$Biomass <- round(sp$Biomass, digits = 1)

write.table(sp, "data//species.txt", row.names = FALSE)
