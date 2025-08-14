//
// This Stan program defines a simple model, with a vector of defensive points per game 
// values 'def_ppg' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
// This model will be used to create the Defensive ratings of the Vortex of Accuracy.

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] def_ppg; // weighted defensive ppg, used as basis for defensive VoA rating
  vector[N] def_ppa; // weighted EPA per play
  vector[N] def_ypp; // weighted Yards per play
  vector[N] def_success_rate; // weighted Play success rate
  vector[N] def_explosiveness; // weighted defensive explosiveness rate
  vector[N] def_third_conv_rate; // weighted third down conversion rate
  vector[N] def_pts_per_opp; // weighted points per scoring opportunity
  vector[N] def_havoc_total; // weighted havoc rate by the defense
  vector[N] def_plays_pg; // weighted number of defensive plays per game
  // vector[N] def_error; // VoA's average defensive error based on previous week's ratings
  // vector[N] def_ppg_aboveavg; // weighted ppg scored by the defense above average defensive ppg
  vector[N] VoA_Output; // weighted VoA output created by averaging rankings in a variety of stats
  vector[N] Conference_Strength; // weighted conference strength metric created using averaging of VoA Output by conference
}

// The parameters accepted by the model.
parameters {
  real b0; // intercept
  real beta_def_ppa; // Coefficient for EPA
  real beta_def_ypp; // Coefficient for yards per play
  real beta_def_success_rate; // Coefficient for def success rate
  real beta_def_explosiveness; // Coefficient for def explosiveness rate
  real beta_def_third_conv_rate; // coefficient for third down conversion rate
  real beta_def_pts_per_opp; // coefficient for defensive points per scoring opportunity
  real beta_def_havoc_total; // coefficient for total defensive havoc rate
  real beta_def_plays_pg; // coefficient for number of plays run by the defense
  // real beta_def_error; // coefficient for VoA's average defensive error based on previous week's ratings
  // real beta_def_ppg_aboveavg; // coefficient for defensive pts above avg
  real beta_VoA_Output; // coefficient for VoA Output
  real beta_Conference_Strength; // coefficient for conference strength
  real sigma; // Standard deviation of the normal distribution
}

// transformed parameters {
//   real<lower=0> mu [N];
//   real<lower=0> shape [N];
//   real<lower=0> rate [N];
//   for (i in 1:N){
//     mu[i] = exp(b0 + beta_def_ppa*def_ppa[i] + beta_def_ypp*def_ypp[i] + beta_def_success_rate*def_success_rate[i] + beta_def_explosiveness*def_explosiveness[i]) + ;
//     shape[i] = mu[i]^2 / sigma^2;
//     rate[i] = mu[i] / sigma^2;
//   }
// }

// The model to be estimated. I model the output 'y' to be normally distributed 
// with mean 'mu' equal to a linear deterministic function and SD 'sigma'.
model {
  // Define linear predictor directly in the model block
  def_ppg ~ normal(b0 + beta_def_ppa * def_ppa + beta_def_ypp * def_ypp + beta_def_success_rate * def_success_rate + beta_def_explosiveness * def_explosiveness + beta_def_third_conv_rate * def_third_conv_rate + beta_def_pts_per_opp * def_pts_per_opp + beta_def_havoc_total * def_havoc_total + beta_def_plays_pg * def_plays_pg + beta_VoA_Output * VoA_Output + beta_Conference_Strength * Conference_Strength, sigma);
}


