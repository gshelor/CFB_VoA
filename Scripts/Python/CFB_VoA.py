##### Stan is failing in R, so I'm going to specify the same model in python using PyMC and see if it works #####


import os
import polars as pl
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sbn
# from dotenv import load_dotenv
from datetime import date, datetime, timedelta
import cmdstanpy
import pymc as pm
import arviz as az
import preliz as pz
import statsmodels.formula.api as smf
import random
import pytensor.tensor as pt


VoATrain = pl.read_parquet(os.path.join(os.getcwd(), "Data", "VoA2026", "ModelTraining", "VoATrain.parquet"))
VoAStats = pl.read_parquet(os.path.join(os.getcwd(), "Data", "VoA2026", "ModelTraining", "VoAVariables.parquet"))


modelstarttime = datetime.now()
offVoA_feature_cols = ['adj_off_epa', 'adj_off_ypp', 'off_success_rate', 'adj_off_explosiveness', 'off_third_conv_rate', 'off_pts_per_opp', 'adj_off_plays_pg', 'VoA_Output', 'Conf_Rk']

##### Offensive Model #####
with pm.Model() as offensive_model:
    ### setting datasets to be used to train the model
    X_data = pm.Data("X_data", VoATrain.select(offVoA_feature_cols).to_numpy())
    y_data = pm.Data("y_data", VoATrain['adj_off_ppg'].to_numpy())
    ### Priors for unknown model parameters
    ### using normal priors because I don't know how to use something else (more accurately, I don't know how to fix something else if it goes wrong or doesn't work)
    ## b0 is the intercept
    # b0 = pm.Normal("b0", mu = 25, sigma = 5)
    b0 = pm.Gamma("b0", 20, 1)
    beta_off_epa = pm.Normal("beta_off_epa", 3, 1)
    beta_off_ypp = pm.Normal("beta_off_ypp", 1, 1)
    beta_off_success_rate = pm.Normal("beta_off_success_rate", 3, 1)
    beta_off_explosiveness = pm.Normal("beta_off_explosiveness", 0.5, 0.25)
    beta_third_conv_rate = pm.Normal("beta_third_conv_rate", 0.5, 10)
    beta_off_pts_per_opp = pm.Normal("beta_off_pts_per_opp", 1, 10)
    beta_off_plays_pg = pm.Normal("beta_off_plays_pg", 0.25, 0.25)
    beta_VoA_Output = pm.Normal("beta_VoA_Output", 0, 10)
    beta_Conference_Strength = pm.Normal("beta_Conference_Strength", 0, 20)
    ### variance prior
    sigma = pm.Gamma("sigma", 10, 1)
    # sigma = pm.HalfNormal("sigma", sigma=10)

    ### model formula
    mu = (
        b0 +
        beta_off_epa * VoATrain['adj_off_epa'].to_numpy() +
        beta_off_ypp * VoATrain['adj_off_ypp'].to_numpy() +
        beta_off_success_rate * VoATrain['off_success_rate'].to_numpy() +
        beta_off_explosiveness * VoATrain['adj_off_explosiveness'].to_numpy() +
        beta_third_conv_rate * VoATrain['off_third_conv_rate'].to_numpy() +
        beta_off_pts_per_opp * VoATrain['off_pts_per_opp'].to_numpy() +
        beta_off_plays_pg * VoATrain['off_plays_pg'].to_numpy() +
        beta_VoA_Output * VoATrain['VoA_Output'].to_numpy() +
        beta_Conference_Strength * VoATrain['Conf_Rk'].to_numpy()
    )
    # mu = ('mu',
    #     b0 +
    #     beta_off_epa * X_data[:, 0] +
    #     beta_off_ypp * X_data[:, 1] +
    #     beta_off_success_rate * X_data[:, 2] +
    #     beta_off_explosiveness *  X_data[:, 3] +
    #     beta_third_conv_rate *  X_data[:, 4] +
    #     beta_off_pts_per_opp *  X_data[:, 5] +
    #     beta_off_plays_pg *  X_data[:, 6] +
    #     beta_VoA_Output *  X_data[:, 7] +
    #     beta_Conference_Strength *  X_data[:, 8]
    # )


    ### Likelihood (Sampling distribution of the data)
    Y_obs = pm.Normal(
        "Y_obs", 
        mu = mu, 
        sigma = sigma, 
        observed = VoATrain['adj_off_ppg'].to_numpy()#y_data
    )

    ### Fit the Model (MCMC Sampling)
    idata = pm.sample(
        draws = 7500, 
        tune = 2500, 
        chains = 3, 
        random_seed = 802,
        cores = os.cpu_count() // 2
    )

   ### Swap the data containers with the out-of-sample inference data
    pm.set_data(
        {
            "X_data": VoAStats.select(['weighted_off_epa', 'weighted_off_ypp', 'weighted_off_success_rate', 'weighted_off_explosiveness', 'weighted_off_third_conv_rate', 'weighted_off_pts_per_opp', 'weighted_off_plays_pg', 'VoA_Output', 'Conf_Rk']).to_numpy(),
            # Target dummy zeros matching length of test_df
            "y_data": np.zeros(len(VoAStats))
        }
    )



    ### Predict on VoA Stats
    predictions = pm.sample_posterior_predictive(
        idata, predictions = True, var_names = ['Y_obs'], random_seed = 802, extend_inferencedata = True
    )

modelendtime = datetime.now()
### Extract predictions for the test set using ArviZ structures inside predictions
# posterior_preds = predictions.posterior_predictive["Y_obs"].stack(sample=("chain", "draw")).values
# posterior_preds = predictions.predictions["Y_obs"].stack(sample=("chain", "draw")).values

### extracting posterior samples for paramaters, converting to numpy arrays
b0_vals = predictions.posterior.b0
beta_off_epa_vals = predictions.posterior.beta_off_epa
beta_off_ypp_vals = predictions.posterior.beta_off_ypp
beta_off_success_rate_vals = predictions.posterior.beta_off_success_rate
beta_off_explosiveness_vals = predictions.posterior.beta_off_explosiveness
beta_third_conv_rate_vals = predictions.posterior.beta_third_conv_rate
beta_off_pts_per_opp_vals = predictions.posterior.beta_off_pts_per_opp
beta_off_plays_pg_vals = predictions.posterior.beta_off_plays_pg
beta_VoA_Output_vals = predictions.posterior.beta_VoA_Output
beta_Conference_Strength_vals = predictions.posterior.beta_Conference_Strength
sigma_vals = predictions.posterior.sigma

param_vals_list = [b0_vals, beta_off_epa_vals, beta_off_ypp_vals, beta_off_success_rate_vals, beta_off_explosiveness_vals, beta_third_conv_rate_vals, beta_off_pts_per_opp_vals, beta_off_plays_pg_vals, beta_VoA_Output_vals, beta_Conference_Strength_vals, sigma_vals]
for param in np.arange(0, len(param_vals_list)):
    param_pd = param_vals_list[param].to_dataframe()
    param_pl = pl.from_pandas(param_pd)
    if param == 0:
        OffVoAParams = param_pl
    else:
        OffVoAParams = pl.concat([OffVoAParams, param_pl], how = 'horizontal')

OffVoAParams.write_parquet(os.path.join(os.getcwd(), "Data", "FittedModels", "OffVoAParams.parquet"))


# PosteriorDraws_df = pl.DataFrame(data = {"b0": b0_vals,
# "beta_off_epa": beta_off_epa_vals,
# "beta_off_ypp": beta_off_ypp_vals,
# "beta_off_success_rate": beta_off_success_rate,
# "beta_off_explosiveness": beta_off_explosiveness_vals,
# "beta_third_conv_rate": beta_third_conv_rate_vals,
# "beta_off_pts_per_opp": beta_off_pts_per_opp_vals,
# "beta_off_plays_pg": beta_off_plays_pg_vals,
# "beta_VoA_Output": beta_VoA_Output_vals,
# "beta_Conference_Strength": beta_Conference_Strength_vals})

###  Add mean, median, percentiles back to Polars as new columns
# VoAStats = VoAStats.with_columns(
#     OffVoA_MeanRating = posterior_preds.mean(axis=1),
#     OffVoA_MedRating = np.median(posterior_preds, axis=1),
#     OffVoA_975PctRating = np.percentile(posterior_preds, 97.5, axis=1),
#     OffVoA_025PctRating = np.percentile(posterior_preds, 2.5, axis=1),
#     OffVoA_SD = np.std(posterior_preds, axis = 1))



##### Defensive Model Now #####
modelstarttime = datetime.now()
defVoA_feature_cols = ['adj_def_epa', 'adj_def_ypp', 'def_success_rate', 'adj_def_explosiveness', 'def_third_conv_rate', 'def_pts_per_opp', 'def_havoc_total', 'adj_def_plays_pg', 'VoA_Output', 'Conf_Rk']

with pm.Model() as defensive_model:
    ### setting datasets to be used to train the model
    X_data = pm.Data("X_data", VoATrain.select(defVoA_feature_cols).to_numpy())
    y_data = pm.Data("y_data", VoATrain['adj_def_ppg'].to_numpy())
    ### Priors for unknown model parameters
    ### using normal priors because I don't know how to use something else (more accurately, I don't know how to fix something else if it goes wrong or doesn't work)
    ## b0 is the intercept
    # b0 = pm.Normal("b0", mu = 25, sigma = 5)
    b0 = pm.Gamma("b0", 20, 1)
    beta_def_epa = pm.Normal("beta_def_epa", 3, 1)
    beta_def_ypp = pm.Normal("beta_def_ypp", 1, 1)
    beta_def_success_rate = pm.Normal("beta_def_success_rate", 3, 1)
    beta_def_explosiveness = pm.Normal("beta_def_explosiveness", 0.5, 0.25)
    beta_def_third_conv_rate = pm.Normal("beta_def_third_conv_rate", 0.5, 10)
    beta_def_pts_per_opp = pm.Normal("beta_def_pts_per_opp", 1, 10)
    beta_def_havoc_total = pm.Normal("beta_def_havoc_total", 1, 10)
    beta_def_plays_pg = pm.Normal("beta_def_plays_pg", 0.25, 0.25)
    beta_VoA_Output = pm.Normal("beta_VoA_Output", 0, 10)
    beta_Conference_Strength = pm.Normal("beta_Conference_Strength", 0, 20)
    ### variance prior
    sigma = pm.Gamma("sigma", 10, 1)
    # sigma = pm.HalfNormal("sigma", sigma=10)

    ### model formula
    mu = (
        b0 +
        beta_def_epa * VoATrain['adj_def_epa'].to_numpy() +
        beta_def_ypp * VoATrain['adj_def_ypp'].to_numpy() +
        beta_def_success_rate * VoATrain['def_success_rate'].to_numpy() +
        beta_def_explosiveness * VoATrain['adj_def_explosiveness'].to_numpy() +
        beta_def_third_conv_rate * VoATrain['def_third_conv_rate'].to_numpy() +
        beta_def_pts_per_opp * VoATrain['def_pts_per_opp'].to_numpy() +
        beta_def_havoc_total * VoATrain['def_havoc_total'].to_numpy() +
        beta_def_plays_pg * VoATrain['def_plays_pg'].to_numpy() +
        beta_VoA_Output * VoATrain['VoA_Output'].to_numpy() +
        beta_Conference_Strength * VoATrain['Conf_Rk'].to_numpy()
    )
    # mu = ('mu',
    #     b0 +
    #     beta_def_epa * X_data[:, 0] +
    #     beta_def_ypp * X_data[:, 1] +
    #     beta_def_success_rate * X_data[:, 2] +
    #     beta_def_explosiveness *  X_data[:, 3] +
    #     beta_third_conv_rate *  X_data[:, 4] +
    #     beta_def_pts_per_opp *  X_data[:, 5] +
    #     beta_def_plays_pg *  X_data[:, 6] +
    #     beta_VoA_Output *  X_data[:, 7] +
    #     beta_Conference_Strength *  X_data[:, 8]
    # )


    ### Likelihood (Sampling distribution of the data)
    Y_obs = pm.Normal(
        "Y_obs", 
        mu = mu, 
        sigma = sigma, 
        observed = VoATrain['adj_def_ppg'].to_numpy()#y_data
    )

    ### Fit the Model (MCMC Sampling)
    idata = pm.sample(
        draws = 7500, 
        tune = 2500, 
        chains = 3, 
        random_seed = 802,
        cores = os.cpu_count() // 2
    )

   ### Swap the data containers with the out-of-sample inference data
    pm.set_data(
        {
            "X_data": VoAStats.select(['weighted_def_epa', 'weighted_def_ypp', 'weighted_def_success_rate', 'weighted_def_explosiveness', 'weighted_def_third_conv_rate', 'weighted_def_pts_per_opp', 'weighted_def_havoc_total', 'weighted_def_plays_pg', 'VoA_Output', 'Conf_Rk']).to_numpy(),
            # Target dummy zeros matching length of test_df
            "y_data": np.zeros(len(VoAStats))
        }
    )



    ### Predict on VoA Stats
    predictions = pm.sample_posterior_predictive(
        idata, predictions = True, var_names = ['Y_obs'], random_seed = 802, extend_inferencedata = True
    )

modelendtime = datetime.now()
### Extract predictions for the test set using ArviZ structures inside predictions
# posterior_preds = predictions.posterior_predictive["Y_obs"].stack(sample=("chain", "draw")).values
# posterior_preds = predictions.predictions["Y_obs"].stack(sample=("chain", "draw")).values

### extracting posterior samples for paramaters, converting to numpy arrays
b0_vals = predictions.posterior.b0
beta_def_epa_vals = predictions.posterior.beta_def_epa
beta_def_ypp_vals = predictions.posterior.beta_def_ypp
beta_def_success_rate_vals = predictions.posterior.beta_def_success_rate
beta_def_explosiveness_vals = predictions.posterior.beta_def_explosiveness
beta_def_third_conv_rate_vals = predictions.posterior.beta_def_third_conv_rate
beta_def_pts_per_opp_vals = predictions.posterior.beta_def_pts_per_opp
beta_def_havoc_total_vals = predictions.posterior.beta_def_havoc_total
beta_def_plays_pg_vals = predictions.posterior.beta_def_plays_pg
beta_VoA_Output_vals = predictions.posterior.beta_VoA_Output
beta_Conference_Strength_vals = predictions.posterior.beta_Conference_Strength
sigma_vals = predictions.posterior.sigma

param_vals_list = [b0_vals, beta_def_epa_vals, beta_def_ypp_vals, beta_def_success_rate_vals, beta_def_explosiveness_vals, beta_def_third_conv_rate_vals, beta_def_pts_per_opp_vals, beta_def_havoc_total_vals, beta_def_plays_pg_vals, beta_VoA_Output_vals, beta_Conference_Strength_vals, sigma_vals]
for param in np.arange(0, len(param_vals_list)):
    param_pd = param_vals_list[param].to_dataframe()
    param_pl = pl.from_pandas(param_pd)
    if param == 0:
        DefVoAParams = param_pl
    else:
        DefVoAParams = pl.concat([DefVoAParams, param_pl], how = 'horizontal')

DefVoAParams.write_parquet(os.path.join(os.getcwd(), "Data", "FittedModels", "DefVoAParams.parquet"))


##### Special Teams VoA #####
modelstarttime = datetime.now()
STVoA_feature_cols = ['net_kick_return_yds', 'net_punt_return_yds', 'net_fg_rate', 'net_adj_st_epa']

with pm.Model() as ST_model:
    ### setting datasets to be used to train the model
    X_data = pm.Data("X_data", VoATrain.select(STVoA_feature_cols).to_numpy())
    y_data = pm.Data("y_data", VoATrain['net_adj_st_ppg'].to_numpy())
    ### Priors for unknown model parameters
    ### using normal priors because I don't know how to use something else (more accurately, I don't know how to fix something else if it goes wrong or doesn't work)
    ## b0 is the intercept
    # b0 = pm.Normal("b0", mu = 25, sigma = 5)
    b0 = pm.Normal("b0", 0, 10)
    beta_net_kick_return_yds = pm.Normal("beta_net_kick_return_yds", 0, 10)
    beta_net_punt_return_yds = pm.Normal("beta_net_punt_return_yds", 0, 5)
    beta_net_fg_rate = pm.Gamma("beta_net_fg_rate", 2, 1)
    beta_net_st_epa = pm.Normal("beta_net_st_epa", 5, 5)
    ### variance prior
    sigma = pm.Gamma("sigma", 10, 1)
    # sigma = pm.HalfNormal("sigma", sigma=10)

    ### model formula
    mu = (
        b0 +
        beta_net_kick_return_yds * VoATrain['net_kick_return_yds'].to_numpy() +
        beta_net_punt_return_yds * VoATrain['net_punt_return_yds'].to_numpy() +
        beta_net_fg_rate * VoATrain['net_fg_rate'].to_numpy() +
        beta_net_st_epa * VoATrain['net_adj_st_epa'].to_numpy()
    )


    ### Likelihood (Sampling distribution of the data)
    Y_obs = pm.Normal(
        "Y_obs", 
        mu = mu, 
        sigma = sigma, 
        observed = VoATrain['net_adj_st_ppg'].to_numpy()#y_data
    )

    ### Fit the Model (MCMC Sampling)
    idata = pm.sample(
        draws = 7500, 
        tune = 2500, 
        chains = 3, 
        random_seed = 802,
        cores = os.cpu_count() // 2
    )

   ### Swap the data containers with the out-of-sample inference data
    pm.set_data(
        {
            "X_data": VoAStats.select(['weighted_net_kick_return_yds', 'weighted_net_punt_return_yds', 'weighted_net_fg_rate', 'weighted_net_adj_st_epa']).to_numpy(),
            # Target dummy zeros matching length of test_df
            "y_data": np.zeros(len(VoAStats))
        }
    )



    ### Predict on VoA Stats
    predictions = pm.sample_posterior_predictive(
        idata, predictions = True, var_names = ['Y_obs'], random_seed = 802, extend_inferencedata = True
    )

modelendtime = datetime.now()
### Extract predictions for the test set using ArviZ structures inside predictions
# posterior_preds = predictions.posterior_predictive["Y_obs"].stack(sample=("chain", "draw")).values
# posterior_preds = predictions.predictions["Y_obs"].stack(sample=("chain", "draw")).values

### extracting posterior samples for paramaters, converting to numpy arrays
b0_vals = predictions.posterior.b0
beta_net_kick_return_yds_vals = predictions.posterior.beta_net_kick_return_yds
beta_net_punt_return_yds_vals = predictions.posterior.beta_net_punt_return_yds
beta_net_fg_rate_vals = predictions.posterior.beta_net_fg_rate
beta_net_st_epa_vals = predictions.posterior.beta_net_st_epa
sigma_vals = predictions.posterior.sigma


param_vals_list = [b0_vals, beta_net_kick_return_yds_vals, beta_net_punt_return_yds_vals, beta_net_fg_rate_vals, beta_net_st_epa_vals, sigma_vals]
for param in np.arange(0, len(param_vals_list)):
    param_pd = param_vals_list[param].to_dataframe()
    param_pl = pl.from_pandas(param_pd)
    if param == 0:
        STVoAParams = param_pl
    else:
        STVoAParams = pl.concat([STVoAParams, param_pl], how = 'horizontal')

STVoAParams.write_parquet(os.path.join(os.getcwd(), "Data", "FittedModels", "STVoAParams.parquet"))
