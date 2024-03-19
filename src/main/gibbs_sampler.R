################################################################################
# The following code builds a Bayesian change-point model by constructing a    #
# Gibbs Sampler for simulations                                                #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

library(dplyr)
library(readr)

# A function that constructs the Poisson-Gamma model.
# Inputs: data, which should be any target column from the cleaned dataset;
#         S, the number of simulations;
#         nburn, the number of burn-in simulations at the beginning;
#         alpha and beta, the initial parameters for the priors.

run_poisson_model = function(data, S = 10^4, nburn = 1000, alpha, beta) {
  
  # Initialize the sampler:
  n = length(data)
  k = ceiling(n/2)
  lambda_1 = mean(data[1:k])
  lambda_2 = mean(data[(k+1):n])
  
  # Initialize placeholders for results:
  post_lambda_1 = array(0, c(S, 1))
  post_lambda_2 = array(0, c(S, 1))
  post_k = array(0, c(S, 1))
  
  for(s in 1:(nburn + S)){
    
    # Sample lambda_1:
    lambda_1 = rgamma(n = 1,
                      shape = alpha + sum(data[1:k]),
                      rate = beta + k)
    
    # Sample lambda_2:
    lambda_2 = rgamma(n = 1,
                      shape = alpha + sum(data[(k+1):n]),
                      rate = beta + (n-k))
    
    # Sample k:
    log_g = sapply(1:(n-1), 
                   function(j)
                     {sum(dpois(data[1:j], lambda = lambda_1, log = TRUE)) +
                       sum(dpois(data[(j+1):n], lambda = lambda_2, log = TRUE))})
    
    k = sample(1:(n-1), 1, prob = exp(log_g))
    
    # Save the simulations:
    if(s > nburn){
      post_lambda_1[s - nburn] = lambda_1
      post_lambda_2[s - nburn] = lambda_2
      post_k[s - nburn] = k
    }
    
  }
  
  return(list(post_lambda_1, post_lambda_2, post_k))
  
}


# A function that constructs the Normal-Normal model:

run_normal_model_one_changepoint = function(data, S = 10^4, nburn = 1000, alpha, beta, sigma_theta){
  
  # Initialize the sampler:
  n = length(data)
  k = ceiling(n/2)
  mu_1 = mean(data[1:k])
  mu_2 = mean(data[(k+1):n])
  sigma_1 = sqrt(var(data[1:k]))
  sigma_2 = sqrt(var(data[(k+1):n]))
  
  # Initialize placeholders for results:
  post_mu_1 = array(0, c(S, 1))
  post_mu_2 = array(0, c(S, 1))
  post_sigma_1 = array(0, c(S,1))
  post_sigma_2 = array(0, c(S,1))
  post_k = array(0, c(S, 1))

  for(s in 1:(nburn + S)){
    y_1 = data[1:k]
    y_2 = data[(k+1): n]
    
    # Sample mu_1:
    Q_1 = k * sigma_1^(-2) + (1 / sigma_theta^2)
    ell_1 = sum(y_1) * sigma_1^(-2)
    mu_1 = rnorm(1, mean = ell_1 * (Q_1^(-1)), sd = sqrt(Q_1^(-1)))
    
    # Sample mu_2: 
    Q_2 = (n - k) * sigma_2^(-2) + (1 / sigma_theta^2)
    ell_2 = sum(y_2) * sigma_2^(-2)
    mu_2 = rnorm(1, mean = ell_2 * (Q_2^(-1)), sd = sqrt(Q_2^(-1)))
    
    # Sample sigma_1:
    alpha_1 = (k/2) + alpha
    beta_1 = beta + 0.5 * sum((y_1 - mu_1)^2)
    tau_1 = rgamma(1, shape = alpha_1, rate = beta_1)
    sigma_1 = 1 / sqrt(tau_1)
    
    # Sample sigma_2:
    alpha_2 = (n - k)/2 + alpha
    beta_2 = beta + 0.5 * sum((y_2 - mu_2)^2)
    tau_2 = rgamma(1, shape = alpha_2, rate = beta_2)
    sigma_2 = 1 / sqrt(tau_2)
    
    # Sample k:
    log_g = sapply(1:(n-1), 
                   function(j){
                     sum(dnorm(data[1:j], mean = mu_1, sd = sigma_1, log = TRUE)) +
                     sum(dnorm(data[(j+1):n], mean = mu_2, sd = sigma_2,log = TRUE))
                   })
    
    k = sample(1:(n-1), 1, prob = exp(log_g))
    
    # Save the simulations:
    if(s > nburn){
      post_mu_1[s - nburn] = mu_1
      post_mu_2[s - nburn] = mu_2
      post_sigma_1[s-nburn] = sigma_1
      post_sigma_2[s-nburn] = sigma_2
      post_k[s - nburn] = k
    }
    
  }
  
  return(post_k)
  
}


run_normal_model_two_changepoints = function(data, S = 10^4, nburn = 1000, alpha, beta, sigma_theta){
  
  # Initialize the sampler:
  n = length(data)
  k1 = ceiling(n/3)
  k2 = ceiling(2*n/3)
  mu_1 = mean(data[1:k1])
  mu_2 = mean(data[(k1+1):k2])
  mu_3 = mean(data[(k2+1):n])
  sigma_1 = sqrt(var(data[1:k1]))
  sigma_2 = sqrt(var(data[(k1+1):k2]))
  sigma_3 = sqrt(var(data[(k2+1):n]))
  
  # Initialize placeholders for results:
  post_mu_1 = array(0, c(S, 1))
  post_mu_2 = array(0, c(S, 1))
  post_mu_3 = array(0, c(S, 1))
  post_sigma_1 = array(0, c(S,1))
  post_sigma_2 = array(0, c(S,1))
  post_sigma_3 = array(0, c(S,1))
  post_k1 = array(0, c(S, 1))
  post_k2 = array(0, c(S, 1))
  
  for(s in 1:(nburn + S)){
    
    y_1 = data[1:k1]
    y_2 = data[(k1+1): k2]
    y_3 = data[(k2+1): n]
    
    # Sample mu_1:
    Q_1 = k1 * sigma_1^(-2) + (1 / sigma_theta^2)
    ell_1 = sum(y_1) * sigma_1^(-2)
    mu_1 = rnorm(1, mean = ell_1 * (Q_1^(-1)), sd = sqrt(Q_1^(-1)))
    
    # Sample mu_2:
    Q_2 = (k2 - k1) * sigma_2^(-2) + (1 / sigma_theta^2)
    ell_2 = sum(y_2) * sigma_2^(-2)
    mu_2 = rnorm(1, mean = ell_2 * (Q_2^(-1)), sd = sqrt(Q_2^(-1)))
    
    # Sample mu_3:
    Q_3 = (n - k2) * sigma_3^(-2) + (1 / sigma_theta^2)
    ell_3 = sum(y_3) * sigma_3^(-2)
    mu_3 = rnorm(1, mean = ell_3 * (Q_3^(-1)), sd = sqrt(Q_3^(-1)))
    
    # Sample sigma_1:
    alpha_1 = k1 / 2 + alpha
    beta_1 = beta + 0.5 * sum((y_1 - mu_1)^2)
    tau_1 = rgamma(1, shape = alpha_1, rate = beta_1)
    sigma_1 = 1 / sqrt(tau_1)
    
    # Sample sigma_2:
    alpha_2 = (k2 - k1) / 2 + alpha
    beta_2 = beta + 0.5 * sum((y_2 - mu_2)^2)
    tau_2 = rgamma(1, shape = alpha_2, rate = beta_2)
    sigma_2 = 1/sqrt(tau_2)
    
    # Sample sigma_3:
    alpha_3 = (n - k2) / 2 + alpha
    beta_3 = beta + 0.5 * sum((y_3 - mu_3)^2)
    tau_3 = rgamma(1, shape = alpha_3, rate = beta_3)
    sigma_3 = 1/sqrt(tau_3)
    
    # Sample k1 and k2:
    log_g = matrix(0, nrow = n - 1, ncol = n - 1)
    
    for (a in 1:(n - 1)) {
      for (b in (a + 1):(n - 1)) {
        if (b < n) {
          log_g[a, b] = sum(dnorm(data[1:a], mean = mu_1, sd = sigma_1, log = TRUE)) +
            sum(dnorm(data[(a + 1):b], mean = mu_2, sd = sigma_2, log = TRUE)) +
            sum(dnorm(data[(b + 1):n], mean = mu_3, sd = sigma_3, log = TRUE))
        }
      }
    }
    
    log_g_flat = as.vector(log_g)
    
    # Sample indices corresponding to k1 and k2:
    index = sample(length(log_g_flat), 1, prob = exp(log_g_flat))
    
    # Convert the index back to k1 and k2:
    k1 = floor((index - 1) / (n - 1)) + 1
    k2 = index %% (n - 1)
    if (k2 == 0) {
      k2 = n - 1
    }
    
    # Save the simulations:
    if(s > nburn){
      post_mu_1[s - nburn] = mu_1
      post_mu_2[s - nburn] = mu_2
      post_mu_3[s - nburn] = mu_3
      post_sigma_1[s - nburn] = sigma_1
      post_sigma_2[s - nburn] = sigma_2
      post_sigma_3[s - nburn] = sigma_3
      post_k1[s - nburn] = k1
      post_k2[s - nburn] = k2
    }
    
  }
  
  return(list(post_k1, post_k2))
  
}



  
  
  
