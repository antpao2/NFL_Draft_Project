# Set seed for reproducibility
set.seed(123)

# Extract relevant columns from the existing dataset
combined_data <- nr[, c("epa", "round")]

# Number of groups
n_groups <- length(unique(combined_data$round))

# Initialize parameters
alpha <- rep(2, n_groups)
beta <- rep(0.5, n_groups)

# Prior hyperparameters for gamma prior
alpha_prior_shape <- 2
alpha_prior_rate <- 1
beta_prior_shape <- 2
beta_prior_rate <- 1

# Number of iterations
n_iterations <- 2000

# Gibbs sampling for gamma distribution with gamma priors
for (iteration in 1:n_iterations) {
  # Update group shape parameters (alpha)
  for (j in 1:n_groups) {
    group_indices <- combined_data$round == j
    alpha[j] <- rgamma(1, shape = alpha_prior_shape + sum(group_indices), rate = alpha_prior_rate + sum(combined_data$epa[group_indices]))
  }
  
  # Update group rate parameters (beta)
  for (j in 1:n_groups) {
    group_indices <- combined_data$round == j
    beta[j] <- rgamma(1, shape = beta_prior_shape + sum(group_indices), rate = beta_prior_rate + sum(combined_data$epa[group_indices]))
  }
  
  # Print the posterior means for alpha and beta (optional)
  }

# Calculate posterior means for shape and rate parameters
posterior_mean_shape <- mean(alpha)
posterior_mean_rate <- mean(beta)

posterior_samples <- data.frame(alpha = alpha, beta = beta, group = factor(1:n_groups))

# Create a box plot
ggplot(nr, aes(x = factor(round), y = epa)) +
  geom_boxplot() +
  labs(x = "Group", y = "EPA Values", title = "Box Plot of EPA Values by Group")
