//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // Number of teams
  vector[N] net_st_ppg; // special teams ppg, used as basis for special teams VoA rating
  vector[N] kick_return_avg_PY3; // yards per kick return
  vector[N] kick_return_avg_PY2; // yards per kick return
  vector[N] kick_return_avg_PY1; // yards per kick return
  vector[N] punt_return_avg_PY3; // yards per punt return
  vector[N] punt_return_avg_PY2; // yards per punt return
  vector[N] punt_return_avg_PY1; // yards per punt return
  vector[N] fg_rate_PY3; // field goal conversion rate
  vector[N] fg_rate_PY2; // field goal conversion rate
  vector[N] fg_rate_PY1; // field goal conversion rate
  vector[N] fg_rate_allowed_PY3; // field goal conversion rate by opposition
  vector[N] fg_rate_allowed_PY2; // field goal conversion rate by opposition
  vector[N] fg_rate_allowed_PY1; // field goal conversion rate by opposition
  vector[N] fg_made_pg_PY3; // number of field goals made per game
  vector[N] fg_made_pg_PY2; // number of field goals made per game
  vector[N] fg_made_pg_PY1; // number of field goals made per game
  vector[N] fg_made_pg_allowed_PY3; // number of made field goals allowed per game
  vector[N] fg_made_pg_allowed_PY2; // number of made field goals allowed per game
  vector[N] fg_made_pg_allowed_PY1; // number of made field goals allowed per game
  vector[N] xpts_pg_PY3; // number of extra points made per game
  vector[N] xpts_pg_PY2; // number of extra points made per game
  vector[N] xpts_pg_PY1; // number of extra points made per game
  vector[N] xpts_allowed_pg_PY3; // extra points allowed per game
  vector[N] xpts_allowed_pg_PY2; // extra points allowed per game
  vector[N] xpts_allowed_pg_PY1; // extra points allowed per game
  vector[N] kick_return_yds_avg_allowed_PY3; // yards allowed per kick return
  vector[N] kick_return_yds_avg_allowed_PY2; // yards allowed per kick return
  vector[N] kick_return_yds_avg_allowed_PY1; // yards allowed per kick return
  vector[N] punt_return_yds_avg_allowed_PY3; // yards allowed per punt return
  vector[N] punt_return_yds_avg_allowed_PY2; // yards allowed per punt return
  vector[N] punt_return_yds_avg_allowed_PY1; // yards allowed per punt return
  // current season variables added in
  vector[N] kick_return_avg; // yards per kick return
  vector[N] punt_return_avg; // yards per punt return
  vector[N] fg_rate; // field goal conversion rate
  vector[N] fg_rate_allowed; // field goal conversion rate by opposition
  vector[N] fg_made_pg; // number of field goals made per game
  vector[N] fg_made_pg_allowed; // number of made field goals allowed per game
  vector[N] xpts_pg; // extra points per game
  vector[N] xpts_allowed_pg; // extra points allowed per game
  vector[N] kick_return_yds_avg_allowed; // yards allowed per kick return
  vector[N] punt_return_yds_avg_allowed; // yards allowed per punt return
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real beta_kick_return_avg_PY3; // coefficient for yards per kick return
  real beta_kick_return_avg_PY2; // coefficient for yards per kick return
  real beta_kick_return_avg_PY1; // coefficient for yards per kick return
  real beta_punt_return_avg_PY3; // coefficient for yards per punt return
  real beta_punt_return_avg_PY2; // coefficient for yards per punt return
  real beta_punt_return_avg_PY1; // coefficient for yards per punt return
  real beta_fg_rate_PY3; // coefficient for field goal conversion rate
  real beta_fg_rate_PY2; // coefficient for field goal conversion rate
  real beta_fg_rate_PY1; // coefficient for field goal conversion rate
  real beta_fg_rate_allowed_PY3; // coefficient for field goal conversion rate by opposition
  real beta_fg_rate_allowed_PY2; // coefficient for field goal conversion rate by opposition
  real beta_fg_rate_allowed_PY1; // coefficient for field goal conversion rate by opposition
  real beta_fg_made_pg_PY3; // coefficient for number of field goals made per game
  real beta_fg_made_pg_PY2; // coefficient for number of field goals made per game
  real beta_fg_made_pg_PY1; // coefficient for number of field goals made per game
  real beta_fg_made_pg_allowed_PY3; // coefficient for number of made field goals allowed per game
  real beta_fg_made_pg_allowed_PY2; // coefficient for number of made field goals allowed per game
  real beta_fg_made_pg_allowed_PY1; // coefficient for number of made field goals allowed per game
  real beta_xpts_pg_PY3; // coefficient for number of extra points made per game
  real beta_xpts_pg_PY2; // coefficient for number of extra points made per game
  real beta_xpts_pg_PY1; // coefficient for number of extra points made per game
  real beta_xpts_allowed_pg_PY3; // coefficient for extra points allowed per game
  real beta_xpts_allowed_pg_PY2; // coefficient for extra points allowed per game
  real beta_xpts_allowed_pg_PY1; // coefficient for extra points allowed per game
  real beta_kick_return_yds_avg_allowed_PY3; // coefficient for yards allowed per kick return
  real beta_kick_return_yds_avg_allowed_PY2; // coefficient for yards allowed per kick return
  real beta_kick_return_yds_avg_allowed_PY1; // coefficient for yards allowed per kick return
  real beta_punt_return_yds_avg_allowed_PY3; // coefficient for yards allowed per punt return
  real beta_punt_return_yds_avg_allowed_PY2; // coefficient for yards allowed per punt return
  real beta_punt_return_yds_avg_allowed_PY1; // coefficient for yards allowed per punt return
  real beta_kick_return_avg; // coefficient for yards per kick return
  real beta_punt_return_avg; // coefficient for yards per punt return
  real beta_fg_rate; // coefficient for field goal conversion rate
  real beta_fg_rate_allowed; // coefficient for field goal conversion rate by opposition
  real beta_fg_made_pg; // coefficient for number of field goals made per game
  real beta_fg_made_pg_allowed; // coefficient for number of made field goals allowed per game
  real beta_xpts_pg; // coefficient for extra points per game
  real beta_xpts_allowed_pg; // coefficient for extra points allowed per game
  real beta_kick_return_yds_avg_allowed; // coefficient for yards allowed per kick return
  real beta_punt_return_yds_avg_allowed; // coefficient for yards allowed per punt return
  real sigma; // Standard deviation of the normal distribution
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Define linear predictor directly in the model block
  net_st_ppg ~ normal(b0 + beta_kick_return_avg_PY3 * kick_return_avg_PY3 + beta_kick_return_avg_PY2 * kick_return_avg_PY2 + beta_kick_return_avg_PY1 * kick_return_avg_PY1 + beta_punt_return_avg_PY3 * punt_return_avg_PY3 + beta_punt_return_avg_PY2 * punt_return_avg_PY2 + beta_punt_return_avg_PY1 * punt_return_avg_PY1 + beta_fg_rate_PY3 * fg_rate_PY3 + beta_fg_rate_PY2 * fg_rate_PY2 + beta_fg_rate_PY1 * fg_rate_PY1 + beta_fg_rate_allowed_PY3 * fg_rate_allowed_PY3 + beta_fg_rate_allowed_PY2 * fg_rate_allowed_PY2 + beta_fg_rate_allowed_PY1 * fg_rate_allowed_PY1 + beta_fg_made_pg_PY3 * fg_made_pg_PY3 + beta_fg_made_pg_PY2 * fg_made_pg_PY2 + beta_fg_made_pg_PY1 * fg_made_pg_PY1 + beta_fg_made_pg_allowed_PY3 * fg_made_pg_allowed_PY3 + beta_fg_made_pg_allowed_PY2 * fg_made_pg_allowed_PY2 + beta_fg_made_pg_allowed_PY1 * fg_made_pg_allowed_PY1 + beta_xpts_pg_PY3 * xpts_pg_PY3 + beta_xpts_pg_PY2 * xpts_pg_PY2 + beta_xpts_pg_PY1 * xpts_pg_PY1 + beta_xpts_pg_allowed_PY3 * xpts_pg_allowed_PY3 + beta_xpts_pg_allowed_PY2 * xpts_pg_allowed_PY2 + beta_xpts_pg_allowed_PY1 * xpts_pg_allowed_PY1 + beta_kick_return_yds_avg_allowed_PY3 * kick_return_yds_avg_allowed_PY3 + beta_kick_return_yds_avg_allowed_PY2 * kick_return_yds_avg_allowed_PY2 + beta_kick_return_yds_avg_allowed_PY1 * kick_return_yds_avg_allowed_PY1 + beta_punt_return_yds_avg_allowed_PY3 * punt_return_yds_avg_allowed_PY3 + beta_punt_return_yds_avg_allowed_PY2 * punt_return_yds_avg_allowed_PY2 + beta_punt_return_yds_avg_allowed_PY1 * punt_return_yds_avg_allowed_PY1 + beta_kick_return_avg * kick_return_avg + beta_punt_return_avg * punt_return_avg + beta_fg_rate * fg_rate + beta_fg_rate_allowed * fg_rate_allowed + beta_fg_made_pg * fg_made_pg + beta_fg_made_pg_allowed * fg_made_pg_allowed + beta_xpts_pg * xpts_pg + beta_xpts_allowed_pg * xpts_allowed_pg + beta_kick_return_yds_avg_allowed * kick_return_yds_avg_allowed + beta_punt_return_yds_avg_allowed * punt_return_yds_avg_allowed, sigma);
}

