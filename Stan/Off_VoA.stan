//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// currently, it is very much in beta testing, just running to see what happens if I model the mean of FPI, SP+, and SRS with the variables in the data block

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] team_strength; // VoA_Rating, FPI-style metric being modelled
  vector[N] off_ppa; // EPA per play
  vector[N] off_ypp; // Yards per play
  vector[N] def_ppa; // defensive EPA per play allowed
  vector[N] off_success_rate; // Play success rate
  vector[N] def_success_rate; // defensive success rate allowed
  vector[N] off_explosiveness; // offensive explosiveness rate
  vector[N] def_explosiveness; // defensive explosiveness rate
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real beta_off_ppa; // Coefficient for EPA
  real beta_off_ypp; // Coefficient for yards per play
  real beta_def_ppa; // coefficient for def epa
  real beta_off_success_rate; // Coefficient for off success rate
  real beta_def_success_rate; // coefficient for def_success_rate
  real beta_off_explosiveness; // Coefficient for off explosiveness rate
  real beta_def_explosiveness; // coefficient for defensive explosiveness rate
  real sigma; // Standard deviation of the normal distribution
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Define linear predictor directly in the model block
  team_strength ~ normal(b0 + beta_off_ppa * off_ppa + beta_off_ypp * off_ypp + beta_def_ppa * def_ppa + beta_off_success_rate * off_success_rate + beta_def_success_rate * def_success_rate + beta_off_explosiveness * off_explosiveness + beta_def_explosiveness * def_explosiveness, sigma);
}


