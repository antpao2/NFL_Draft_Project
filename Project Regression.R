### Bayesian Reegression Model working by round


#OLS est.
ols_model <- lm(season_ypt ~ -1 + rookie_ypt, data=t[t$round == 1,])
summary(ols_model)


#Bayesian Regression

###Using a normal as a semi-conjugate prior for Beta, gamma for sig^2

#Trying it outside of the loop
sig_2 <-1
sig_0 <- 3
b_0 <- .2
X <- t$rookie_ypt
y <- t$season_ypt
dim(X)
V <- solve(solve(sig_0) + t(X) %*% X / sig_2)
E <- V * (solve(sig_0) * b_0 + t(X)%*%y / sig_2)


theta <- NULL
for (i in 1:1000){
  for (j in 1:7){ #For each round (1-7)
    #Change the data based on the round
    X <- t$rookie_ypt[t$round == i]
    y <- t$season_ypt[t$round == i]
    
    #Go with different priors based on round

    #COME FROM THE DATA
    b_0 <- .1
    sig_0 <- .5
    
    sig_2 <- .3
    
    
    #Calculate Variance and Expectation of Beta
    V <- solve(solve(sig_0) + t(X) %*% X / sig_2)
    E <- V * (solve(sig_0) * b_0 + t(X)%*%y / sig_2)
   
  #Start of the Gibbs Sampler
     
  #Sample from Beta First
  beta_new <- rnorm(1, mean = E, sd = sqrt(V))
  
  #Compute SSR with new beta
  SSR <- t(y) %*% y - 2*t(beta_new) %*% t(X) %*%y + t(beta_new) %*% t(X) %*% X %*% beta_new
  #Sample from new sigma
  sig_new <- 1/rgamma(1, 1+1000/2, (1*sig_2 + SSR) / 2)
  
  #Generate New Sample with new parameters
  theta <- c(theta, rnorm(1, beta_new, sig_new))
  }
}

final_estimates <- t(matrix(theta, nrow=7))


#These are the estimates coefficients
apply(final_estimates, MARGIN = 2, FUN = mean)

#Variance
apply(final_estimates, MARGIN = 2, FUN = var)

#Go Over this
apply(final_estimates, MARGIN = 2, FUN = quantile, probs = c(.025,.95))

