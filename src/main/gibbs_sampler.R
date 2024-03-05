################################################################################
# The following code builds a Bayesian change-point model by constructing a    #
# Gibbs Sampler for simulations                                                #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

library(dplyr)
library(readr)

# A function that constructs the Poisson-Gamma model.
# Inputs: data, which should be any target column from the cleaned dataset
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

