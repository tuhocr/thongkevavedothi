# Load essential clinical analysis packages
library(survival)
library(clinfun) # Useful for early-phase design functions

# --- PHASE I EXAMPLE: 3+3 Dose Escalation Simulation ---
# Simulating the true toxicity probabilities across 5 escalating doses
true_tox_rates <- c(0.05, 0.12, 0.25, 0.40, 0.55)
# Using clinfun to simulate the behavior of a 3+3 design
phase1_sim <- clinfun::twostage.inference(0.05, 0.20, 0.05, 0.20) 


# --- PHASE II EXAMPLE: Evaluating Efficacy ---
# Calculate the probability of seeing at least 15 responders out of 40 patients
# assuming a baseline target drug efficacy of 30%
prob_success <- pbinom(14, size = 40, prob = 0.30, lower.tail = FALSE)
print(paste("Probability of Phase II success:", round(prob_success, 4)))


# --- PHASE III EXAMPLE: Kaplan-Meier Survival Analysis ---
# Creating dummy Phase III trial data (Treatment vs Placebo)
set.seed(42)
phase3_data <- data.frame(
  patient_id = 1:100,
  treatment  = rep(c("New_Drug", "Placebo"), each = 50),
  time       = c(rexp(50, rate = 0.02), rexp(50, rate = 0.04)), # Survival time in days
  status     = sample(c(0, 1), 100, replace = TRUE, prob = c(0.1, 0.9)) # 1 = event, 0 = censored
)

# Fit a Kaplan-Meier survival model
km_fit <- survfit(Surv(time, status) ~ treatment, data = phase3_data)

# Print the survival summary
summary(km_fit, times = c(12, 24, 36))
# minh kiem duoc cai nay trong AI, anh Duc xem thu dum minh nhe
        