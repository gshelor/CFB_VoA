//
// This Stan program defines a simple win probability model, with a
// vector of values 'win_prob' modeled on a beta distribution with shape 'alpha'
// and rate 'beta'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector of length 'N'.
data {
  int<lower=0> N;
  array[N] int<lower = 0, upper = 1> win_loss;
  vector[N] win_margin;
}

// The parameters accepted by the model.
parameters {
  real alpha;
  real beta_score;
  // real<lower=0> sigma;
}


// The model to be estimated. We model the output
// 'y' to be distributed on the beta distribution with parameters 'alpha' and 'beta'.
model {
    alpha ~ normal(0, 2);
    beta_score ~ normal(0.15, 0.05);
    win_loss ~ bernoulli_logit(alpha + beta_score * win_margin);
}

