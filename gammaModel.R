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
burnin <- 100

# Create matrices to store posterior samples
posterior_samples_alpha <- matrix(NA, nrow = n_iterations, ncol = n_groups)
posterior_samples_beta <- matrix(NA, nrow = n_iterations, ncol = n_groups)

# Gibbs sampling for gamma distribution with gamma priors
for (iteration in 1:n_iterations) {
  # Update group shape parameters (alpha)
  for (j in 1:n_groups) {
    group_indices <- combined_data$rounds == j
    alpha[j] <- rgamma(1, shape = alpha_prior_shape + sum(group_indices), rate = alpha_prior_rate + sum(combined_data$epa[group_indices]))
  }
  
  # Update group rate parameters (beta)
  for (j in 1:n_groups) {
    group_indices <- combined_data$rounds == j
    beta[j] <- rgamma(1, shape = beta_prior_shape + sum(group_indices), rate = beta_prior_rate + sum(combined_data$epa[group_indices]))
  }
  
  # Store posterior samples
  posterior_samples_alpha[iteration, ] <- alpha
  posterior_samples_beta[iteration, ] <- beta
}
posterior_df_alpha = posterior_samples_alpha[-(1:burnin),]
posterior_df_beta = posterior_samples_beta[-(1:burnin),]

# Convert matrices to data frames
posterior_df_alpha <- data.frame(posterior_df_alpha)
posterior_df_beta <- data.frame(posterior_df_beta)

# Combine data frames for plotting
posterior_df_combined <- data.frame(
  Group = rep(1:n_groups, each = n_iterations),
  Alpha = c(posterior_samples_alpha),
  Beta = c(posterior_samples_beta)
)

# Plot box plots using ggplot2
library(ggplot2)

# Box plot for alpha
ggplot(posterior_df_combined, aes(x = factor(Group), y = Alpha)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of Alpha (Shape Parameter)",
       x = "Group",
       y = "Alpha")

# Box plot for beta
ggplot(posterior_df_combined, aes(x = factor(Group), y = Beta)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of Beta (Rate Parameter)",
       x = "Group",
       y = "Beta")
