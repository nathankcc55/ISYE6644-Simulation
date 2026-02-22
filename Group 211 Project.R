library(ggplot2)
library(kableExtra)
library(knitr)

set.seed(123)
n_susceptible <- 60
p_infect <- 0.01

# (a) PMF using dbinom
probabilities <- dbinom(0:6, size = n_susceptible, prob = p_infect)
infect_df <- data.frame(Infected = 0:6, Probability = probabilities)

# Plotting distribution of PMF
a_plot <- ggplot(infect_df, aes(x = factor(Infected), y = Probability)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_text(
    aes(label = sprintf("%.3f", Probability)), vjust = -0.5, size = 4) +
  labs(
    title = "Number of Infection(s) Distribution on Day 1",
    x = "Kids Infected",
    y = "Probability"
  ) +
  scale_y_continuous(limits = c(0,1))
a_plot

# (b) What is the expected number of kids that Tommy infects on Day 1?
expected_infections <- n_susceptible * p_infect
expected_infections  # Output: 0.6

# Binomial distributed random variables to approximate E
sim_day1 <- rbinom(5000, size = 60, prob = 0.01)

sim_day1_df <- data.frame(Infections = sim_day1)

b_plot <- ggplot(sim_day1_df, aes(x = Infections)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", boundary = 0) +
  geom_vline(xintercept = mean(sim_day1), color = "red", size = 0.8) +
  annotate("text", x = mean(sim_day1) + 0.5, y = 2500, 
           label = paste0("Mean = ", round(mean(sim_day1), 2)), 
           color = "red", size = 4, hjust = 0) +
  labs(
    title = "Number of kids Infected on Day 1 Simulation",
    x = "Number of Kids Infected",
    y = "Frequency"
  )
b_plot

# (c) What is the expected number of kids that are infected by Day 2? 

trials <- 5000
results_c <- numeric(trials)

for (i in 1:trials) {
  susceptible <- 1:n_susceptible
  infected_day1 <- susceptible[rbinom(n_susceptible, 1, p_infect) == 1]

  # Remove infected kids from susceptibles after day 1
  remaining_after_day1 <- susceptible[!susceptible %in% infected_day1]
  infected_day2_by_tommy <- remaining_after_day1[rbinom(length(remaining_after_day1), 1, p_infect) == 1]
  
  # Remove infected kids from susceptibles after day 2 by Tommy
  remaining_after_tommy <- remaining_after_day1[!remaining_after_day1 %in% infected_day2_by_tommy]
  
  # infected day 1 kids that will infect day 2 susceptible kids
  infected_day2_by_kids <- c()
  for (k in infected_day1) {
    new_infected <- remaining_after_tommy[rbinom(length(remaining_after_tommy), 1, p_infect) == 1]
    infected_day2_by_kids <- unique(c(infected_day2_by_kids, new_infected))
    remaining_after_tommy <- remaining_after_tommy[!remaining_after_tommy %in% new_infected]
  }
  all_infected <- unique(c(0, infected_day1, infected_day2_by_tommy, infected_day2_by_kids))
  results_c[i] <- length(all_infected)
}
round(mean(results_c), 3)

sim_day2_df <- data.frame(Infections = results_c)  # Capital I matches aes()

c_plot <- ggplot(sim_day2_df, aes(x = Infections)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", boundary = 0) +
  geom_vline(xintercept = mean(results_c), color = "red", size = 0.8) +
  annotate("text", x = mean(results_c) + 0.1, y = max(table(results_c)) * 0.8, 
           label = paste0("Mean = ", round(mean(results_c), 2)), 
           color = "red", size = 4, hjust = 0) +
  scale_x_continuous(breaks = seq(min(results_c), max(results_c), by = 1)) +
  labs(
    title = "Total Number of kids Infected by End of Day 2",
    x = "Number of kids Infected",
    y = "Frequency"
  )

c_plot

# (d) Simulate the number of kids that are infected on Days 1, 2…

# Simulate epidemic duration
results_d <- numeric(trials)

for (i in 1:trials) {
  susceptible <- 1:n_susceptible
  infectious <- data.frame(id = 0, days_left = 3)  # Tommy is infectious for 3 days
  day <- 0
  
  repeat {
    day <- day + 1
    new_infected <- c()
    
    for (j in 1:nrow(infectious)) {
      infector <- infectious$id[j]
      for (student in susceptible) {
        if (runif(1) < p_infect) {
          new_infected <- c(new_infected, student)
        }
      }
    }
    
    new_infected <- new_infected[!duplicated(new_infected)]
    susceptible <- susceptible[!(susceptible %in% new_infected)]
    
    # New infections list
    if (length(new_infected) > 0) {
      new_rows <- data.frame(id = new_infected, days_left = 3)
      infectious <- rbind(infectious, new_rows)
    }
    infectious$days_left <- infectious$days_left - 1
    infectious <- infectious[infectious$days_left > 0, ]
    # If no one is infectious anymore, epidemic is over
    if (nrow(infectious) == 0) {
      break
    }
  }
  
  results_d[i] <- day
}

sim_day_d_df <- data.frame(Infections = results_d)

d_plot <- ggplot(sim_day_d_df, aes(x = Infections)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", boundary = 0) +
  geom_vline(xintercept = mean(results_d), color = "red", size = 0.8) +
  annotate("text", x = mean(results_d) + 0.1, y = max(table(results_d)) * 0.8, 
           label = paste0("Mean = ", round(mean(results_d), 2)), 
           color = "red", size = 4, hjust = 0) +
  scale_x_continuous(breaks = seq(min(results_d), max(results_d), by = 1)) +
  labs(
    title = "Duration of Epidemic in 5000 trials",
    x = "Epidemic Duration (Days)",
    y = "Frequency"
  )

d_plot

# Showcasing five number summary
summary_d <- summary(results_d)
results_d_table <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "Mean", "3rd Quartile (Q3)", "Maximum"), Value = as.numeric(summary_d)
)
results_d_table %>%
  kbl(caption = "Five-Number Summary of Epidemic Duration", digits = 2) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Part D continuation: Expected number of infected kids each day
max_days <- 60
results_d_2 <- matrix(0, nrow = trials, ncol = max_days)

for (i in 1:trials) {
  susceptible <- 1:n_susceptible
  infected <- c()
  infectious <- data.frame(id = 0, first_day = 1, third_day = 3)
  
  for (day in 1:max_days) {
    # Check who's infectious today
    active <- infectious[infectious$first_day <= day & infectious$third_day >= day, "id"]
    
    new_infected <- c()
    for (infector in active) {
      infect_targets <- susceptible[rbinom(length(susceptible), 1, p_infect) == 1]
      new_infected <- c(new_infected, infect_targets)
    }
    
    # Update lists
    new_infected <- unique(new_infected)
    susceptible <- setdiff(susceptible, new_infected)
    infected <- unique(c(infected, new_infected))
    
    # Add new infections with their infectious period
    if (length(new_infected) > 0) {
      infectious <- rbind(infectious, data.frame(
        id = new_infected,
        first_day = day + 1,
        third_day = day + 3
      ))
    }
    
    # Record cumulative count for this day
    results_d_2[i, day] <- length(infected) + 1  # +1 for Tommy
    if (nrow(infectious[infectious$third_day >= day, ]) == 0) break
  }
}

expected_by_day <- colMeans(results_d_2)

# Output summary table
expected_infection_df <- data.frame(
  Day = 1:max_days,
  Expected_Infections = round(expected_by_day, 3)
)

ggplot(expected_infection_df, aes(x = Day, y = Expected_Infections)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(
    title = "Expected Number of Infected Kids by Day",
    x = "Day",
    y = "Expected Number of Infected Kids"
  ) +
  scale_x_continuous(breaks = seq(0, max(expected_infection_df$Day), by = 2))

# (e) What if each kid has a 50-50 chance of already being immunized (and the immunization works perfectly)?
  
cumulative_results_e <- matrix(0, nrow = trials, ncol = max_days)

for (i in 1:trials) {
  immunity_status <- rbinom(n_susceptible, 1, 0.5)  # 1 = immune
  susceptible <- which(immunity_status == 0)        # Only non-immune kids
  
  infected <- c()
  # Tommy infects on Day 1–3
  infectious <- data.frame(id = 0, first_day = 1, third_day = 3)
  
  for (day in 1:max_days) {
    active <- infectious[infectious$first_day <= day & infectious$third_day >= day, "id"]
    
    # Infecting new targets
    new_infected <- c()
    for (infector in active) {
      infect_targets <- susceptible[rbinom(length(susceptible), 1, p_infect) == 1]
      new_infected <- c(new_infected, infect_targets)
    }
    
    # Fix list of susceptibles and infected
    new_infected <- unique(new_infected)
    susceptible <- setdiff(susceptible, new_infected)
    infected <- unique(c(infected, new_infected))
    
    # Check to see if there are new infected today
    if (length(new_infected) > 0) {
      infectious <- rbind(infectious, data.frame(
        id = new_infected,
        first_day = day + 1,
        third_day = day + 3
      ))
    }
    cumulative_results_e[i, day] <- length(infected) + 1
    if (nrow(infectious[infectious$third_day >= day, ]) == 0) break
  }
}
expected_infection_e_df <- data.frame(
  Day = 1:max_days,
  Expected_Infections = round(colMeans(cumulative_results_e), 3)
)

ggplot(expected_infection_e_df, aes(x = Day, y = Expected_Infections)) +
  geom_col(fill = "orange", color = "black") +
  labs(
    title = "Expected Number of Infections by Day (50% Immunization)",
    x = "Day",
    y = "Expected Number of Infected Kids"
  ) +
  scale_x_continuous(breaks = seq(0, max(expected_infection_e_df$Day), by = 2))
