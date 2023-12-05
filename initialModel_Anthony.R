library(ggplot2)

#this might work, but why don't we use rookie data as original parameters instead of 1

#Pior is a gamma, likelihood is exponetial (Conguagte pairs)

# Set initial values
n_groups <- 7
lambda <- rep(1, n_groups)  # initial values for the rate parameter of the exponential distribution
lambda0 <- 1  # initial value for the overall rate parameter

# Number of iterations
n_iterations <- 2000

# Combine data into a data frame
combined_data <- nr

# Create group indicators
combined_data$group <- factor(combined_data$round)


#Updating the lambda each time 
### We are considering this an exponential distribution --> Have to understand why we went this way



# Print the posterior means
cat("Posterior means for group rates (lambda):\n", lambda, "\n")
cat("Posterior mean for overall rate (lambda0):\n", lambda0, "\n")

# Combine posterior samples into a data frame
posterior_samples <- data.frame(matrix(nrow = n_iterations, ncol = n_groups))
colnames(posterior_samples) <- paste0("lambda_", 1:n_groups)


# Gibbs sampling
for (iteration in 1:n_iterations) {
  # Update group rates (lambda)
  for (j in 1:n_groups){
    #Get the specified group that we are running (all entries that are equal to j)
    group_indices <- combined_data$group == j
    #Get total number of entries in group j
    ni <- sum(group_indices) 
    #Sum the data where j is located
    sum_data <- sum(combined_data$epa[group_indices])
    #Sample from the posterior distibution for each group
    posterior_samples[iteration, j] <- rgamma(1, shape = ni / 2 + 1, rate = sum_data / 2 + 1)
  }
  # Update overall rate (lambda0)
  #shape_lambda0 <- n_groups / 2 + 1
  #rate_lambda0 <- sum(lambda) / 2 + 1
  #lambda0 <- rgamma(1, shape = shape_lambda0, rate = rate_lambda0)
}

# Reshape the data for ggplot2
posterior_samples_long <- tidyr::gather(posterior_samples, key = "parameter", value = "rate")

# Create boxplots
ggplot(posterior_samples_long, aes(x = factor(parameter), y = rate)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of Group Rates",
       x = "Group",
       y = "Rate (lambda)")

#Interpretation: Use CI and compare the group means to each other


#Posterior Predictive
#CI and Quantiles for future prediction


