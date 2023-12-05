library(tidyverse)
library(dplyr)
library(nflreadr)
library(nflfastR)
library(gsisdecoder)
library(qs)
library(fuzzyjoin)
library(dlookr)

#Loading Player Stats by Week
all <- load_player_stats(2003:2022)
wr <- all %>% filter(position == 'WR', season_type == 'REG')
wr <- wr %>% select(names(wr)[c(1,3,4,7,8,35:49)] )
#Go over why some player names won't pull
#wr is per week

#This accumulates stats for a player over a given season (wr data is per week)
p <- wr %>% group_by(player_id, season) %>% summarise(name = unique(player_display_name),  epa = sum(receiving_epa, na.rm = T), rec = sum(receptions), yards = sum(receiving_yards), tds = sum(receiving_tds), targets = sum(targets), target_share = sum(target_share, na.rm=T) / length(target_share))
#P has a row for each player in each season, pull more stats by adding to summarize

#This adds total years played and years experience
p <- p %>% group_by(player_id) %>% mutate(total_seasons = length(player_id), year_exp = order(season)) 

#Remove dup player with same name to avoid matching confusion
p <- p %>% filter(player_id != '00-0020337' & player_id != '00-0020452' & player_id != '00-0021425')

p$name <- ifelse(p$player_id == '00-0023452', 'Mike Williams1', p$name)
p$name <- ifelse(p$player_id == '00-0027702', 'Mike Williams2', p$name )

#Name and rookie szn (Not sure if needed)
o <- p %>% group_by(name) %>% summarise(l = min(season))



#Draft Pick Data
draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")
draft_wr <- draft_picks %>% filter(position == 'WR' & season >= 2003 & season < 2023) 
draft_wr <- draft_wr %>% select(names(draft_wr)[c(1,2,3,4,5,6,10)])

#After using a fuzzy match with max dist to compare strings, manually changed the strings to match
draft_wr$pfr_name <- ifelse(draft_wr$pfr_name == 'Gabriel Davis', 'Gabe Davis', draft_wr$pfr_name)
draft_wr$pfr_name <- ifelse(draft_wr$pfr_name == 'Henry Ruggs III', 'Henry Ruggs', draft_wr$pfr_name)

#Double Matches
draft_wr$pfr_name <- ifelse(draft_wr$pfr_id == 'WillMi03', 'Mike Williams1', draft_wr$pfr_name)
draft_wr$pfr_name <- ifelse(draft_wr$pfr_id == 'WillMi04', 'Mike Williams2', draft_wr$pfr_name)
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'KJ Hamler', 'K.J. Hamler')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'D.K. Metcalf', 'DK Metcalf')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Equanimeous St.Brown','Equanimeous St. Brown')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'JJ Nelson','J.J. Nelson')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'DJ Chark','D.J. Chark')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Laviska Shenault Jr.','Laviska Shenault')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Terrace Marshall Jr.','Terrace Marshall')
draft_wr$pfr_name <- str_replace(draft_wr$pfr_name, 'Michael Pittman Jr.','Michael Pittman')
#Changed these based on my fuzzy join

#Rookie Stats
all <- draft_wr %>% left_join(p, by = c('pfr_name'= 'name', 'season' = 'season'))
#All are all rookie stats with their draft position


#Going to omit the ones that didnt match or didnt record a catch in their rookie season 
###May not have matched bc of name, position different, etc, or may be blank bc no stats (injured, etc.)
all <-(na.omit(all))


dim(all)
#have 453 total stats for rookies



#Non-Rookie Stats: After Year 1
non_rookie <- p %>% filter(year_exp > 1) 
nr <- non_rookie %>% left_join(draft_wr[c(3,4,6)], by = c('name'= 'pfr_name'))
nr <- na.omit(nr)

length(unique(nr$name))
#434 total players have stats from year two and on

dim(nr)

#describe(nr)

#normality(nr)

#plot_normality(nr, epa)

#num <- target_by(nr, epa)

#num_num <- relate(num, year_exp)
#num_num

#plot(y=nr$epa, x=nr$pick)
#boxplot(nr$epa)

#summary(nr)

#cor(nr$epa,nr$pick)


library(ggplot2)

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

# Gibbs sampling
for (iteration in 1:n_iterations) {
  # Update group rates (lambda)
  for (j in 1:n_groups) {
    group_indices <- combined_data$group == j
    ni <- sum(group_indices)
    sum_data <- sum(combined_data$epa[group_indices])
    lambda[j] <- rgamma(1, shape = ni / 2 + 1, rate = sum_data / 2 + 1)
  }
  
  # Update overall rate (lambda0)
  shape_lambda0 <- n_groups / 2 + 1
  rate_lambda0 <- sum(lambda) / 2 + 1
  lambda0 <- rgamma(1, shape = shape_lambda0, rate = rate_lambda0)
}

# Print the posterior means
cat("Posterior means for group rates (lambda):\n", lambda, "\n")
cat("Posterior mean for overall rate (lambda0):\n", lambda0, "\n")

# Combine posterior samples into a data frame
posterior_samples <- data.frame(matrix(nrow = n_iterations, ncol = n_groups))
colnames(posterior_samples) <- paste0("lambda_", 1:n_groups)

# Gibbs sampling
for (iteration in 1:n_iterations) {
  # Update group rates (lambda)
  for (j in 1:n_groups) {
    group_indices <- combined_data$group == j
    ni <- sum(group_indices)
    sum_data <- sum(combined_data$epa[group_indices])
    posterior_samples[iteration, j] <- rgamma(1, shape = ni / 2 + 1, rate = sum_data / 2 + 1)
  }
}

# Reshape the data for ggplot2
posterior_samples_long <- tidyr::gather(posterior_samples, key = "parameter", value = "rate")

# Create boxplots
ggplot(posterior_samples_long, aes(x = factor(parameter), y = rate)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of Group Rates",
       x = "Group",
       y = "Rate (lambda)")

combined_data$group

##################################

#rookie data, finding prior

#all %>% filter(round == 4)

median_r1 <-  median(all$epa[all$round == 1])
median_r1

median_r2 <-  median(all$epa[all$round == 2])
median_r2

median_r3 <-  median(all$epa[all$round == 3])
median_r3

median_r4 <- median(all$epa[all$round == 4])
median_r4

median_r5 <- median(all$epa[all$round == 5])
median_r5

median_r6 <- median(all$epa[all$round == 6])
median_r6

median_r7 <- median(all$epa[all$round == 7])
median_r7

rate <- c(median_r1,median_r2,median_r3,median_r4,median_r5,median_r6,median_r7)
lambda_all <- 1/rate

lambda0 <- mean(lambda_all)

#Gibb's sampling using prior from rookie data
# Set initial values
n_groups <- 7
# initial values for the rate parameter of the exponential distribution
# initial value for the overall rate parameter

# Number of iterations
n_iterations <- 2000

# Combine data into a data frame
combined_data <- nr

# Create group indicators
combined_data$group <- factor(combined_data$round)

# Gibbs sampling
for (iteration in 1:n_iterations) {
  # Update group rates (lambda)
  for (j in 1:n_groups) {
    group_indices <- combined_data$group == j
    ni <- sum(group_indices)
    sum_data <- sum(combined_data$epa[group_indices])
    lambda_all[j] <- rgamma(1, shape = ni / 2 + 1, rate = sum_data / 2 + 1)
  }
  
  # Update overall rate (lambda0)
  #shape_lambda0 <- n_groups / 2 + 1
  #rate_lambda0 <- sum(lambda_all) / 2 + 1
  #lambda0 <- rgamma(1, shape = shape_lambda0, rate = rate_lambda0)
}

# Print the posterior means
cat("Posterior means for group rates (lambda):\n", lambda_all, "\n")
cat("Posterior mean for overall rate (lambda0):\n", lambda0, "\n")

# Combine posterior samples into a data frame
posterior_samples <- data.frame(matrix(nrow = n_iterations, ncol = n_groups))
colnames(posterior_samples) <- paste0("lambda_", 1:n_groups)

# Gibbs sampling
for (iteration in 1:n_iterations) {
  # Update group rates (lambda)
  for (j in 1:n_groups) {
    group_indices <- combined_data$group == j
    ni <- sum(group_indices)
    sum_data <- sum(combined_data$epa[group_indices])
    posterior_samples[iteration, j] <- rgamma(1, shape = ni / 2 + 1, rate = sum_data / 2 + 1)
  }
}

# Reshape the data for ggplot2
posterior_samples_long <- tidyr::gather(posterior_samples, key = "parameter", value = "rate")

# Create boxplots
ggplot(posterior_samples_long, aes(x = factor(parameter), y = rate)) +
  geom_boxplot() +
  labs(title = "Posterior Distribution of Group Rates",
       x = "Group",
       y = "Rate (lambda)")
