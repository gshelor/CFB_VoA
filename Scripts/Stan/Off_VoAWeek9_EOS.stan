//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// currently, it is very much in beta testing, just running to see what happens if I model points per game to create an offensive VoA rating similar to Bill Connelly's SP+ model

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] off_ppg; // offensive ppg, used as basis for offensive VoA rating
  vector[N] off_plays_pg; // number of offensive plays per game
  vector[N] off_ppg_aboveavg; // ppg scored by the offense above the average offensive ppg
  vector[N] VoA_Output; // VoA output created by averaging rankings in a variety of stats
  vector[N] Conference_Strength; // conference strength metric created using averaging of VoA Output by conference
  vector[N] off_ppa; // EPA per play
  vector[N] off_ypp; // yards per play
  vector[N] off_success_rate; // play success rate
  vector[N] off_explosiveness; // offensive explosiveness rate
  vector[N] third_conv_rate; // third down conversion rate
  vector[N] off_pts_per_opp; // pts per scoring opportunity
}

// The parameters accepted by the model.
parameters {
  real b0; // intercept
  real beta_off_plays_pg; // coefficient for number of plays run by the offense
  real beta_off_ppg_aboveavg; // coefficient for offensive pts above avg
  real beta_VoA_Output; // coefficient for VoA Output
  real beta_Conference_Strength; // coefficient for conference strength
  real beta_off_ppa; // coefficient for EPA per play
  real beta_off_ypp; // coefficient for yards per play
  real beta_off_success_rate; // coefficient for play success rate
  real beta_off_explosiveness; // coefficient for offensive explosiveness rate
  real beta_third_conv_rate; // coefficient for third down conversion rate
  real beta_off_pts_per_opp; // coefficient for pts per scoring opportunity
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

// The model to be estimated. I model the output 'y' to be normally distributed 
// with mean 'mu' equal to a linear deterministic function and SD 'sigma'.
model {
  // Define linear predictor directly in the model block
  off_ppg ~ normal(b0 + beta_off_plays_pg * off_plays_pg + beta_off_ppg_aboveavg * off_ppg_aboveavg + beta_VoA_Output * VoA_Output + beta_Conference_Strength * Conference_Strength + beta_off_ppa * off_ppa + beta_off_ypp * off_ypp + beta_off_success_rate * off_success_rate + beta_off_explosiveness * off_explosiveness + beta_third_conv_rate * third_conv_rate + beta_off_pts_per_opp * off_pts_per_opp, sigma);
}


