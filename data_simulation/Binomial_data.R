seed <- 1988
set.seed(seed)

N_hosp <- 210
N_pat <- rpois(N_hosp, lambda = 15)


# Covariates --------------------------------------------------------------
policy <- rbinom(N_hosp, prob = 0.34, size = 1)
staff <- rpois(N_hosp, lambda = exp(3 + 0.01 * (N_pat/2)))
capacity <- rbeta(N_hosp, shape1 = 6, shape2 = 2)
b0 <- -1
b1 <- -0.05 # staff slope
b2 <- -0.5 # policy implemented
b3 <- -0.1 # interaction
b4 <- 2.5  # capacity
mrse <- rbinom(N_hosp, size = N_pat, prob = plogis(b0 + 
                                                     b1 * staff + 
                                                     b2 * policy + 
                                                     b3 * policy * staff +
                                                     b4 * capacity))

policy_f <- ifelse(policy == 1, "Implemented", "Not implemented")
policy_f <- factor(policy_f, levels = c("Implemented", "Not implemented"))
mrse <- data.frame(mrse, total_patients = N_pat, n_staff = staff, policy = policy_f, capacity)
mrse$capacity <- round(mrse$capacity, digits = 2)
write.table(mrse, "data//mrse.txt", row.names = FALSE)
