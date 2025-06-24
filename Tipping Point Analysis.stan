//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  // Meta data
  int<lower=1> N; // number of observations
  int<lower=1> J; // number of interventions (excluding control)
  // Data
  array[N] int<lower=0, upper=1> y; // hospitalization status for each patient (y=1 indicates hospitalized)
  matrix<lower=0, upper=1>[N, J] Z; // design matrix of treatment assigments
}
parameters {
  real alpha;
  vector[J] theta;
}
transformed parameters {
  vector[N] mu;
  mu = alpha + Z*theta; // log-odds of response in each covariate/intervention group
}
model {
  // Prior parameters
  alpha ~ student_t(1, -3.48, 2.5);
  theta ~ student_t(1, 0, 2.5);
  // Likelihood
  y ~ bernoulli_logit(mu);
  // missing data can be marginalized out without contributing to the likelihood.
}

