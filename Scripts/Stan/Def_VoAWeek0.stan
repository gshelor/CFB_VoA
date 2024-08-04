//
// This Stan program defines a simple model, with a
// vector of values modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// currently, it is very much in beta testing, just running to see what happens if I model points per game to create an defensive VoA rating similar to Bill Connelly's SP+ model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] def_ppg_allowed; // defensive ppg, used as basis for defensive VoA rating
  vector[N] def_ppa; // EPA per play
  vector[N] def_ypp; // Yards per play
  vector[N] def_success_rate; // Play success rate
  vector[N] def_explosiveness; // defensive explosiveness rate
  vector[N] def_plays_pg; // number of defensive plays per game
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real beta_def_ppa; // Coefficient for EPA
  real beta_def_ypp; // Coefficient for yards per play
  real beta_def_success_rate; // Coefficient for off success rate
  real beta_def_explosiveness; // Coefficient for off explosiveness rate
  real beta_off_plays; // coefficient for number of plays run by the offense
  real sigma; // Standard deviation of the normal distribution
}

// transformed parameters {
//   real<lower=0> mu [N];
//   real<lower=0> shape [N];
//   real<lower=0> rate [N];
//   for (i in 1:N){
//     mu[i] = exp(b0 + beta_off_ppa*off_ppa[i] + beta_off_ypp*off_ypp[i] + beta_off_success_rate*off_success_rate[i] + beta_off_explosiveness*off_explosiveness[i]) + ;
//     shape[i] = mu[i]^2 / sigma^2;
//     rate[i] = mu[i] / sigma^2;
//   }
// }

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu' which is equal to a deterministic linear function and standard deviation 'sigma'.
model {
  // Define linear predictor directly in the model block
  def_ppg ~ normal(b0 + beta_off_ppa * off_ppa + beta_off_ypp * off_ypp + beta_off_success_rate * off_success_rate + beta_off_explosiveness * off_explosiveness, sigma);
}


