//
// This Stan program defines a simple model, with a
// vector of values modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] def_ppg; // defensive ppg, used as basis for defensive VoA rating
  vector[N] def_ppa_PY1; // EPA per play
  vector[N] def_ypp_PY1; // Yards per play
  vector[N] def_success_rate_PY1; // Play success rate
  vector[N] def_explosiveness_PY1; // defensive explosiveness rate
  vector[N] def_third_conv_rate_PY1; // third down conversion rate
  vector[N] def_pts_per_opp_PY1; // points per scoring opportunity
  vector[N] def_plays_pg; // number of defensive plays per game
  vector[N] def_ppg_aboveavg; // ppg scored by the defense above the average defensive ppg
  vector[N] VoA_Output; // VoA output created by averaging rankings in a variety of stats
  vector[N] Conference_Strength; // conference strength metric created using averaging of VoA Output by conference
  vector[N] def_ppa; // EPA per play
  vector[N] def_ypp; // yards per play
  vector[N] def_success_rate; // play success rate
  vector[N] def_explosiveness; // defensive explosiveness rate
  vector[N] def_third_conv_rate; // third down conversion rate
  vector[N] def_pts_per_opp; // pts per scoring opportunity
}

// The parameters accepted by the model.
parameters {
  real b0; // intercept
  real beta_def_ppa_PY1; // Coefficient for EPA
  real beta_def_ypp_PY1; // Coefficient for yards per play
  real beta_def_success_rate_PY1; // Coefficient for def success rate
  real beta_def_explosiveness_PY1; // Coefficient for def explosiveness rate
  real beta_def_third_conv_rate_PY1; // coefficient for third down conversion rate
  real beta_def_pts_per_opp_PY1; // coefficient for defensive points per scoring opportunity
  real beta_def_plays_pg; // coefficient for number of plays run by the defense
  real beta_def_ppg_aboveavg; // coefficient for defensive pts above avg
  real beta_VoA_Output; // coefficient for VoA Output
  real beta_Conference_Strength; // coefficient for conference strength
  real beta_def_ppa; // coefficient for EPA per play
  real beta_def_ypp; // coefficient for yards per play
  real beta_def_success_rate; // coefficient for play success rate
  real beta_def_explosiveness; // coefficient for defensive explosiveness rate
  real beta_def_third_conv_rate; // coefficient for third down conversion rate allowed
  real beta_def_pts_per_opp; // coefficient for pts per scoring opportunity allowed
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
  def_ppg ~ normal(b0 + beta_def_ppa_PY1 * def_ppa_PY1 + beta_def_ypp_PY1 * def_ypp_PY1 + beta_def_success_rate_PY1 * def_success_rate_PY1 + beta_def_explosiveness_PY1 * def_explosiveness_PY1 + beta_def_third_conv_rate_PY1 * def_third_conv_rate_PY1 + beta_def_pts_per_opp_PY1 * def_pts_per_opp_PY1 + beta_def_plays_pg * def_plays_pg + beta_def_ppg_aboveavg * def_ppg_aboveavg + beta_VoA_Output * VoA_Output + beta_Conference_Strength * Conference_Strength + beta_def_ppa * def_ppa + beta_def_ypp * def_ypp + beta_def_success_rate * def_success_rate + beta_def_explosiveness * def_explosiveness + beta_def_third_conv_rate * def_third_conv_rate + beta_def_pts_per_opp * def_pts_per_opp, sigma);
}

