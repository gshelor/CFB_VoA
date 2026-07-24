//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] net_st_ppg; // special teams ppg, used as basis for special teams VoA rating
  vector[N] net_kick_return_yds; // net yards per kick return
  vector[N] net_punt_return_yds; // net yards per punt return
  vector[N] net_fg_rate; // net field goal conversion rate
  vector[N] net_st_epa; // net special teams PPA (expected points added)
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real beta_net_kick_return_yds; // coefficient for yards per kick return
  real beta_net_punt_return_yds; // coefficient for yards per punt return
  real beta_net_fg_rate; // coefficient for field goal conversion rate
  real beta_net_st_epa; // coefficient for net special teams PPA (expected points added)
  real sigma; // Standard deviation of the normal distribution
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // priors
  b0 ~ normal(0, 10);
  beta_net_kick_return_yds ~ normal(0, 10);
  beta_net_punt_return_yds ~ normal(0, 5);
  beta_net_fg_rate ~ gamma(2, 1);
  beta_net_st_epa ~ gamma(5, 5);
  sigma ~ gamma(0.25, 5);
  // Define linear predictor directly in the model block
  net_st_ppg ~ normal(b0 + beta_net_kick_return_yds * net_kick_return_yds + beta_net_punt_return_yds * net_punt_return_yds + beta_net_fg_rate * net_fg_rate + beta_net_st_epa * net_st_epa, sigma);
}


