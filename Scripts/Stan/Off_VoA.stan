//
// This Stan program defines a simple model, with a vector of offensive points per game 
// values 'off_ppg' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
// This model will be used to create the Preseason Projections of the Vortex of Accuracy.
// On April 25th, 2025 I added some lines which are intended to help quantify team and/or conference effects. They're commented out for now, but I may use them later

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] off_ppg; // offensive ppg, used as basis for offensive VoA rating
  vector[N] off_ppa; // weighted EPA per play
  vector[N] off_ypp; // weighted Yards per play
  vector[N] off_success_rate; // weighted Play success rate
  vector[N] off_explosiveness; // weighted offensive explosiveness rate
  vector[N] third_conv_rate; // weighted third down conversion rate
  vector[N] off_pts_per_opp; // weighted points per scoring opportunity
  vector[N] off_plays_pg; // weighted number of offensive plays per game
  // vector[N] off_error; // VoA's average offensive error based on previous week's ratings
  // vector[N] off_ppg_aboveavg; // weighted ppg scored by the offense above average offensive ppg
  vector[N] VoA_Output; // weighted VoA output created by averaging rankings in a variety of stats
  vector[N] Conference_Strength; // weighted conference strength metric created using averaging of VoA Output by conference
  // int<lower=1> num_teams; // Number of unique teams
  // int<lower=1> team_id[N]; // Index for each team
  // int<lower=1> num_conferences; // Number of unique conferences
  // int<lower=1> conference_id[N]; // Index for the conference of each team
}

// The parameters accepted by the model.
parameters {
  real b0; // intercept
  real beta_off_ppa; // Coefficient for EPA/play
  real beta_off_ypp; // Coefficient for yards per play
  real beta_off_success_rate; // Coefficient for off success rate
  real beta_off_explosiveness; // Coefficient for off explosiveness rate
  real beta_third_conv_rate; // coefficient for third down conversion rate
  real beta_off_pts_per_opp; // coefficient for offensive points per scoring opportunity
  real beta_off_plays_pg; // coefficient for number of plays run by the offense
  // real beta_off_error; // coefficient for VoA's average offensive error based on previous week's ratings
  // real beta_off_ppg_aboveavg; // coefficient for offensive pts above avg
  real beta_VoA_Output; // coefficient for VoA Output
  real beta_Conference_Strength; // coefficient for conference strength
  // vector[num_teams] team_effects; // Random effects for each team
  // vector[num_conferences] conference_effects; // Random effects for each conference
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
  off_ppg ~ normal(b0 + beta_off_ppa * off_ppa + beta_off_ypp * off_ypp + beta_off_success_rate * off_success_rate + beta_off_explosiveness * off_explosiveness + beta_third_conv_rate * third_conv_rate + beta_off_pts_per_opp * off_pts_per_opp + beta_off_plays_pg * off_plays_pg + beta_VoA_Output * VoA_Output + beta_Conference_Strength * Conference_Strength, sigma);
}


// The model to be estimated.
// model {
//   // Priors for the coefficients
//   b0 ~ normal(0, 10);
//   beta_off_ppa ~ normal(0, 5);
//   beta_off_ypp ~ normal(0, 5);
//   beta_off_success_rate ~ normal(0, 5);
//   beta_off_explosiveness ~ normal(0, 5);
//   beta_third_conv_rate ~ normal(0, 5);
//   beta_off_pts_per_opp ~ normal(0, 5);
//   beta_off_plays_pg ~ normal(0, 5);
//   beta_off_error ~ normal(0, 5);
//   beta_VoA_Output ~ normal(0, 5);
//   beta_Conference_Strength ~ normal(0, 5);
//   sigma ~ exponential(1);
// 
//   // Priors for the random effects
//   team_effects ~ normal(0, 2); // Adjust the scale as needed
//   conference_effects ~ normal(0, 2); // Adjust the scale as needed
// 
//   // Define linear predictor with team and conference effects
//   off_ppg ~ normal(b0 + beta_off_ppa * off_ppa + beta_off_ypp * off_ypp + beta_off_success_rate * off_success_rate + beta_off_explosiveness * off_explosiveness + beta_third_conv_rate * third_conv_rate + beta_off_pts_per_opp * off_pts_per_opp + beta_off_plays_pg * off_plays_pg + beta_off_error * off_error + beta_VoA_Output * VoA_Output + beta_Conference_Strength * Conference_Strength + team_effects[team_id] + conference_effects[conference_id], sigma);
// }

