### This script is currently just for beta testing an Offensive VoA metric as I aspire to make my Vortex of Accuracy look more like SP+ (at least in output, I obviously want the actual method to have my own unique touch to it)

### this function is here because I saw it somewhere and thought it was useful but don't know what to do with it yet
# colorRampPallette()

## loading packages
library(pacman)
p_load(tidyverse, here, cfbfastR, rstan, ModelMetrics, parallel)

## pulling in games from cfbfastR
games_2023 <- cfbd_game_info(2023)
data <- read_csv(here("Data", "VoA2023", "2023Week16_VoA.csv"))
games_2023 <- games_2023 |>
  filter(home_team %in% data$team | away_team %in% data$team)

### pulling in stats
stats <- cfbd_stats_season_team(2023) |>
  mutate(off_ppg_PY1 = ((pass_TDs * 6) + (rush_TDs * 6)) / games,
         off_ppg_aboveavg_PY1 = off_ppg_PY1 - mean(off_ppg_PY1),
         def_pts_allowed_PY1 = 0, .before = 5)

adv_stats <- cfbd_stats_season_advanced(2023) |>
  filter(team %in% data$team) |>
  select(-season, -conference)

all_stats <- full_join(stats, adv_stats, by = "team")

### testing out pbp function since I've never done this before
PBP2021 <- load_cfb_pbp(2021)

PBP2021_TDs <- PBP2021 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP2022 <- load_cfb_pbp(2022)
PBP2022_TDs <- PBP2022 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP2023 <- load_cfb_pbp(2023)
PBP2023_TDs <- PBP2023 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP2023_DefTDs <- PBP2023 |>
  filter(play_type == "Fumble Return Touchdown" | play_type == "Interception Return Touchdown")

PBP2023_2Pt <- PBP2023 |>
  filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")

PBP2023_2ptScorePlays <- PBP2023 |>
  filter(scoring_play == 1) |>
  filter(pos_score_pts == 8) |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
  filter(pos_team %in% all_stats$team)

PBP2023_2ptScorePlays <- rbind(PBP2023_2ptScorePlays, PBP2023_2Pt)


for (school in 1:nrow(all_stats)){
  temp_PBP2023_TDs <- PBP2023_TDs |>
    filter(def_pos_team == stats$team[school])
  all_stats$def_pts_allowed_PY1[school] = nrow(temp_PBP2023_TDs) * 6
}


all_stats <- all_stats |>
  mutate(def_ppg_allowed_PY1 = def_pts_allowed_PY1 / games,
         def_ppg_belowavg_PY1 = def_ppg_allowed_PY1 - mean(def_ppg_allowed_PY1), .before = 8) |>
  mutate(off_ppg_adj = case_when(off_ppg_aboveavg_PY1 > 0 ~ off_ppg_PY1 + (off_ppg_aboveavg_PY1 / 2),
                                 TRUE ~ off_ppg_PY1), .before = 5)


## using SRS ratings for FCS teams instead of the randomly sampled VoA rating based on
# bottom half of VoA ratings as done during 2022 CFB season
`%nin%` = Negate(`%in%`)
FCS <- cfbd_ratings_srs(year = as.numeric(2023)) |>
  # filter(team %nin% data$team) |>
  filter(team %in% games_2023$home_team | team %in% games_2023$away_team)


##### DUMMY VoA STAN MODEL #####
## making list of data to declare what goes into stan model
Off_VoA_datalist <- list(N = nrow(all_stats), off_ppg = all_stats$off_ppg_PY1, off_ppa = all_stats$off_ppa, off_success_rate = all_stats$off_success_rate, off_explosiveness = all_stats$off_explosiveness, off_ppg_aboveavg = all_stats$off_ppg_aboveavg_PY1)

### fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
Off_VoA_fit <- stan(file=here("Scripts","Stan", "Off_VoA.stan"),data = Off_VoA_datalist, chains = 3, iter = 10000, warmup = 3000)
Off_VoA_fit


## Extracting Parameters
Off_VoA_pars <- rstan::extract(Off_VoA_fit, c("b0","beta_off_ppa", "beta_off_success_rate", "beta_off_explosiveness", "beta_off_ppg_aboveavg", "sigma"))

## making predictions
## adding in process uncertainty
Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(all_stats))

## process error
set.seed(802)
for (p in 1:length(Off_VoA_pars$b0)){
  for(t in 1:nrow(all_stats)){
    Off_VoA_Rating <- rnorm(1, mean = Off_VoA_pars$b0[p] + Off_VoA_pars$beta_off_ppa[p] * all_stats$off_ppa[t] + Off_VoA_pars$beta_off_success_rate[p] * all_stats$off_success_rate[t] + Off_VoA_pars$beta_off_explosiveness[p] * all_stats$off_explosiveness[t] + Off_VoA_pars$beta_off_ppg_aboveavg[p] * all_stats$off_ppg_aboveavg_PY1[t], sd=Off_VoA_pars$sigma[p])
    Off_VoA_Ratings[p,t] <- Off_VoA_Rating
  }
}


## generating forecasts
MeanPred <- apply(Off_VoA_Ratings,2,mean)
MedianPred <- apply(Off_VoA_Ratings,2,median)
Upper <- apply(Off_VoA_Ratings,2,quantile, prob=.975)
Lower <- apply(Off_VoA_Ratings,2,quantile, prob=.025)

all_stats$OffVoA_StanMeanRating <- MeanPred
all_stats$OffVoA_StanMedRating <- MedianPred
all_stats$OffVoA_StanUpper <- Upper
all_stats$OffVoA_StanLower <- Lower

# Off_data_stan <- all_stats |>
#   select(season, team, conference, OffVoA_StanMeanRating, OffVoA_StanMedRating, OffVoA_StanUpper, OffVoA_StanLower)

Off_data_stan_noAdj <- all_stats |>
  select(season, team, conference, OffVoA_StanMeanRating, OffVoA_StanMedRating, OffVoA_StanUpper, OffVoA_StanLower)


##### def VoA testing #####
## making list of data to declare what goes into stan model
Def_VoA_datalist <- list(N = nrow(all_stats), def_ppg_allowed = all_stats$def_ppg_allowed_PY1, def_ppa = all_stats$def_ppa, def_success_rate = all_stats$def_success_rate, def_explosiveness = all_stats$def_explosiveness)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
Def_VoA_fit <- stan(file=here("Scripts","Stan", "Def_VoA.stan"),data = Def_VoA_datalist, chains = 3, iter = 10000, warmup = 3000)
Def_VoA_fit


## Extracting Parameters
Def_VoA_pars <- rstan::extract(Def_VoA_fit, c("b0","beta_def_ppa", "beta_def_success_rate", "beta_def_explosiveness", "sigma"))

## making predictions
## adding in process uncertainty
Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(all_stats))

## process error
set.seed(802)
for (p in 1:length(Def_VoA_pars$b0)){
  for(t in 1:nrow(all_stats)){
    Def_VoA_Rating <- rnorm(1, mean = Def_VoA_pars$b0[p] + Def_VoA_pars$beta_def_ppa[p] * all_stats$def_ppa[t] + Def_VoA_pars$beta_def_success_rate[p] * all_stats$def_success_rate[t] + Def_VoA_pars$beta_def_explosiveness[p] * all_stats$def_explosiveness[t], sd=Def_VoA_pars$sigma[p])
    Def_VoA_Ratings[p,t] <- Def_VoA_Rating
  }
}


## generating forecasts
MeanPred <- apply(Def_VoA_Ratings,2,mean)
MedianPred <- apply(Def_VoA_Ratings,2,median)
Upper <- apply(Def_VoA_Ratings,2,quantile, prob=.95)
Lower <- apply(Def_VoA_Ratings,2,quantile, prob=.05)

all_stats$DefVoA_StanMeanRating <- MeanPred
all_stats$DefVoA_StanMedRating <- MedianPred
all_stats$DefVoA_StanUpper <- Upper
all_stats$DefVoA_StanLower <- Lower

Defdata_stan <- all_stats |>
  select(team, DefVoA_StanMeanRating, DefVoA_StanMedRating, DefVoA_StanUpper, DefVoA_StanLower)



## plotting forecasts against data
# plot(MeanPred, type='l', ylim = c(-40,40), main = "Plotting Ranges of VoA Rating for Each Team", ylab = "VoA Rating")
# lines(Upper,lty=2)
# lines(Lower,lty=2)
# points(data$FPI,col='steelblue')

## RMSE
# rmse(data$FPI_SP_SRS_mean, MedianPred)





##### BEGINNING OF SOMETHING ELSE TBD #####
## Air Force
AirForce <- games_2023 |>
  filter(home_team == "Air Force" | away_team == "Air Force")

## calculating actual difference
AirForce <- AirForce |>
  mutate(team = "Air Force",
         actual_diff = case_when(home_team == "Air Force" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         team_pts = case_when(home_team == "Air Force" ~ home_points,
                                TRUE ~ away_points),
         opp_pts = case_when(home_team == "Air Force" ~ away_points,
                               TRUE ~ home_points)) |>
  group_by(team) |>
  summarise(MOV = mean(actual_diff),
            mean_team_pts = mean(team_pts),
            mean_opp_pts = mean(opp_pts))


## Akron
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Akron <- completed_games |>
  filter(home_team == "Akron" | away_team == "Akron")

## adding opponent ratings to main team df
Akron <- full_join(Akron, AkronOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Akron <- Akron |>
  mutate(actual_diff = case_when(home_team == "Akron" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points)) |>
  group_by(team) |>
  summarise(MOV = mean(actual_diff))

## determining number of losses
Akron_losses <- Akron |>
  filter(home_team == "Akron" & home_points < away_points | away_team == "Akron" & away_points < home_points)
## storing overall team Resume Score as vector
Akron_resume <- sum(Akron$Resume_Score) - (7 * nrow(Akron_losses))

## Alabama
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Alabama <- completed_games |>
  filter(home_team == "Alabama" | away_team == "Alabama") |>
  mutate(team = "Alabama",
         team_opp = case_when(home_team == "Alabama" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Alabama"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
AlabamaFBSOpps <- VoA_Variables |>
  filter(team %in% Alabama$team_opp) |>
  select(team, VoA_Rating)
AlabamaFCSOpps <- FCS |>
  filter(team %in% Alabama$team_opp) |>
  select(team, rating)
colnames(AlabamaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
AlabamaOpps <- rbind(AlabamaFBSOpps, AlabamaFCSOpps)
colnames(AlabamaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Alabama <- full_join(Alabama, AlabamaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Alabama <- Alabama |>
  mutate(actual_diff = case_when(home_team == "Alabama" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Alabama" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Alabama" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Alabama" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Alabama_losses <- Alabama |>
  filter(home_team == "Alabama" & home_points < away_points | away_team == "Alabama" & away_points < home_points)
## storing overall team Resume Score as vector
Alabama_resume <- sum(Alabama$Resume_Score) - (7 * nrow(Alabama_losses))


## AppalachianSt
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
AppalachianSt <- completed_games |>
  filter(home_team == "Appalachian State" | away_team == "Appalachian State") |>
  mutate(team = "Appalachian State",
         team_opp = case_when(home_team == "Appalachian State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Appalachian State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
AppalachianStFBSOpps <- VoA_Variables |>
  filter(team %in% AppalachianSt$team_opp) |>
  select(team, VoA_Rating)
AppalachianStFCSOpps <- FCS |>
  filter(team %in% AppalachianSt$team_opp) |>
  select(team, rating)
colnames(AppalachianStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
AppalachianStOpps <- rbind(AppalachianStFBSOpps, AppalachianStFCSOpps)
colnames(AppalachianStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
AppalachianSt <- full_join(AppalachianSt, AppalachianStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
AppalachianSt <- AppalachianSt |>
  mutate(actual_diff = case_when(home_team == "Appalachian State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Appalachian State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Appalachian State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Appalachian State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
AppalachianSt_losses <- AppalachianSt |>
  filter(home_team == "Appalachian State" & home_points < away_points | away_team == "Appalachian State" & away_points < home_points)
## storing overall team Resume Score as vector
AppalachianSt_resume <- sum(AppalachianSt$Resume_Score) - (7 * nrow(AppalachianSt_losses))


## Arizona
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Arizona <- completed_games |>
  filter(home_team == "Arizona" | away_team == "Arizona") |>
  mutate(team = "Arizona",
         team_opp = case_when(home_team == "Arizona" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arizona"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ArizonaFBSOpps <- VoA_Variables |>
  filter(team %in% Arizona$team_opp) |>
  select(team, VoA_Rating)
ArizonaFCSOpps <- FCS |>
  filter(team %in% Arizona$team_opp) |>
  select(team, rating)
colnames(ArizonaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ArizonaOpps <- rbind(ArizonaFBSOpps, ArizonaFCSOpps)
colnames(ArizonaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Arizona <- full_join(Arizona, ArizonaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Arizona <- Arizona |>
  mutate(actual_diff = case_when(home_team == "Arizona" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Arizona" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Arizona" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Arizona" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Arizona_losses <- Arizona |>
  filter(home_team == "Arizona" & home_points < away_points | away_team == "Arizona" & away_points < home_points)
## storing overall team Resume Score as vector
Arizona_resume <- sum(Arizona$Resume_Score) - (7 * nrow(Arizona_losses))


## ArizonaSt
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
ArizonaSt <- completed_games |>
  filter(home_team == "Arizona State" | away_team == "Arizona State") |>
  mutate(team = "Arizona State",
         team_opp = case_when(home_team == "Arizona State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arizona State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ArizonaStFBSOpps <- VoA_Variables |>
  filter(team %in% ArizonaSt$team_opp) |>
  select(team, VoA_Rating)
ArizonaStFCSOpps <- FCS |>
  filter(team %in% ArizonaSt$team_opp) |>
  select(team, rating)
colnames(ArizonaStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ArizonaStOpps <- rbind(ArizonaStFBSOpps, ArizonaStFCSOpps)
colnames(ArizonaStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
ArizonaSt <- full_join(ArizonaSt, ArizonaStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
ArizonaSt <- ArizonaSt |>
  mutate(actual_diff = case_when(home_team == "Arizona State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Arizona State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Arizona State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Arizona State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
ArizonaSt_losses <- ArizonaSt |>
  filter(home_team == "Arizona State" & home_points < away_points | away_team == "Arizona State" & away_points < home_points)
## storing overall team Resume Score as vector
ArizonaSt_resume <- sum(ArizonaSt$Resume_Score) - (7 * nrow(ArizonaSt_losses))


## Arkansas
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Arkansas <- completed_games |>
  filter(home_team == "Arkansas" | away_team == "Arkansas") |>
  mutate(team = "Arkansas",
         team_opp = case_when(home_team == "Arkansas" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arkansas"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ArkansasFBSOpps <- VoA_Variables |>
  filter(team %in% Arkansas$team_opp) |>
  select(team, VoA_Rating)
ArkansasFCSOpps <- FCS |>
  filter(team %in% Arkansas$team_opp) |>
  select(team, rating)
colnames(ArkansasFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ArkansasOpps <- rbind(ArkansasFBSOpps, ArkansasFCSOpps)
colnames(ArkansasOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Arkansas <- full_join(Arkansas, ArkansasOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Arkansas <- Arkansas |>
  mutate(actual_diff = case_when(home_team == "Arkansas" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Arkansas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Arkansas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Arkansas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Arkansas_losses <- Arkansas |>
  filter(home_team == "Arkansas" & home_points < away_points | away_team == "Arkansas" & away_points < home_points)
## storing overall team Resume Score as vector
Arkansas_resume <- sum(Arkansas$Resume_Score) - (7 * nrow(Arkansas_losses))


## ArkansasSt
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
ArkansasSt <- completed_games |>
  filter(home_team == "Arkansas State" | away_team == "Arkansas State") |>
  mutate(team = "Arkansas State",
         team_opp = case_when(home_team == "Arkansas State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Arkansas State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ArkansasStFBSOpps <- VoA_Variables |>
  filter(team %in% ArkansasSt$team_opp) |>
  select(team, VoA_Rating)
ArkansasStFCSOpps <- FCS |>
  filter(team %in% ArkansasSt$team_opp) |>
  select(team, rating)
colnames(ArkansasStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ArkansasStOpps <- rbind(ArkansasStFBSOpps, ArkansasStFCSOpps)
colnames(ArkansasStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
ArkansasSt <- full_join(ArkansasSt, ArkansasStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
ArkansasSt <- ArkansasSt |>
  mutate(actual_diff = case_when(home_team == "Arkansas State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Arkansas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Arkansas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Arkansas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
ArkansasSt_losses <- ArkansasSt |>
  filter(home_team == "Arkansas State" & home_points < away_points | away_team == "Arkansas State" & away_points < home_points)
## storing overall team Resume Score as vector
ArkansasSt_resume <- sum(ArkansasSt$Resume_Score) - (7 * nrow(ArkansasSt_losses))

## Army
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Army <- completed_games |>
  filter(home_team == "Army" | away_team == "Army") |>
  mutate(team = "Army",
         team_opp = case_when(home_team == "Army" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Army"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ArmyFBSOpps <- VoA_Variables |>
  filter(team %in% Army$team_opp) |>
  select(team, VoA_Rating)
ArmyFCSOpps <- FCS |>
  filter(team %in% Army$team_opp) |>
  select(team, rating)
colnames(ArmyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ArmyOpps <- rbind(ArmyFBSOpps, ArmyFCSOpps)
colnames(ArmyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Army <- full_join(Army, ArmyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Army <- Army |>
  mutate(actual_diff = case_when(home_team == "Army" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Army" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Army" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Army" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Army_losses <- Army |>
  filter(home_team == "Army" & home_points < away_points | away_team == "Army" & away_points < home_points)
## storing overall team Resume Score as vector
Army_resume <- sum(Army$Resume_Score) - (7 * nrow(Army_losses))

## Auburn
## creating separate data frame for each team's schedule
## using this data frame to calculate how a team did relative to what VoA would 
# currently predict
Auburn <- completed_games |>
  filter(home_team == "Auburn" | away_team == "Auburn") |>
  mutate(team = "Auburn",
         team_opp = case_when(home_team == "Auburn" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Auburn"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
AuburnFBSOpps <- VoA_Variables |>
  filter(team %in% Auburn$team_opp) |>
  select(team, VoA_Rating)
AuburnFCSOpps <- FCS |>
  filter(team %in% Auburn$team_opp) |>
  select(team, rating)
colnames(AuburnFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
AuburnOpps <- rbind(AuburnFBSOpps, AuburnFCSOpps)
colnames(AuburnOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Auburn <- full_join(Auburn, AuburnOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Auburn <- Auburn |>
  mutate(actual_diff = case_when(home_team == "Auburn" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Auburn" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Auburn" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Auburn" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Auburn_losses <- Auburn |>
  filter(home_team == "Auburn" & home_points < away_points | away_team == "Auburn" & away_points < home_points)
## storing overall team Resume Score as vector
Auburn_resume <- sum(Auburn$Resume_Score) - (7 * nrow(Auburn_losses))


## BallSt
BallSt <- completed_games |>
  filter(home_team == "Ball State" | away_team == "Ball State") |>
  mutate(team = "Ball State",
         team_opp = case_when(home_team == "Ball State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ball State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BallStFBSOpps <- VoA_Variables |>
  filter(team %in% BallSt$team_opp) |>
  select(team, VoA_Rating)
BallStFCSOpps <- FCS |>
  filter(team %in% BallSt$team_opp) |>
  select(team, rating)
colnames(BallStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BallStOpps <- rbind(BallStFBSOpps, BallStFCSOpps)
colnames(BallStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
BallSt <- full_join(BallSt, BallStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
BallSt <- BallSt |>
  mutate(actual_diff = case_when(home_team == "Ball State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Ball State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Ball State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Ball State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
BallSt_losses <- BallSt |>
  filter(home_team == "Ball State" & home_points < away_points | away_team == "Ball State" & away_points < home_points)
## storing overall team Resume Score as vector
BallSt_resume <- sum(BallSt$Resume_Score) - (7 * nrow(BallSt_losses))


## Baylor
Baylor <- completed_games |>
  filter(home_team == "Baylor" | away_team == "Baylor") |>
  mutate(team = "Baylor",
         team_opp = case_when(home_team == "Baylor" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Baylor"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BaylorFBSOpps <- VoA_Variables |>
  filter(team %in% Baylor$team_opp) |>
  select(team, VoA_Rating)
BaylorFCSOpps <- FCS |>
  filter(team %in% Baylor$team_opp) |>
  select(team, rating)
colnames(BaylorFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BaylorOpps <- rbind(BaylorFBSOpps, BaylorFCSOpps)
colnames(BaylorOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Baylor <- full_join(Baylor, BaylorOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Baylor <- Baylor |>
  mutate(actual_diff = case_when(home_team == "Baylor" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Baylor" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Baylor" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Baylor" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Baylor_losses <- Baylor |>
  filter(home_team == "Baylor" & home_points < away_points | away_team == "Baylor" & away_points < home_points)
## storing overall team Resume Score as vector
Baylor_resume <- sum(Baylor$Resume_Score) - (7 * nrow(Baylor_losses))


## BoiseSt
BoiseSt <- completed_games |>
  filter(home_team == "Boise State" | away_team == "Boise State") |>
  mutate(team = "Boise State",
         team_opp = case_when(home_team == "Boise State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Boise State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BoiseStFBSOpps <- VoA_Variables |>
  filter(team %in% BoiseSt$team_opp) |>
  select(team, VoA_Rating)
BoiseStFCSOpps <- FCS |>
  filter(team %in% BoiseSt$team_opp) |>
  select(team, rating)
colnames(BoiseStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BoiseStOpps <- rbind(BoiseStFBSOpps, BoiseStFCSOpps)
colnames(BoiseStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
BoiseSt <- full_join(BoiseSt, BoiseStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
BoiseSt <- BoiseSt |>
  mutate(actual_diff = case_when(home_team == "Boise State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Boise State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Boise State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Boise State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
BoiseSt_losses <- BoiseSt |>
  filter(home_team == "Boise State" & home_points < away_points | away_team == "Boise State" & away_points < home_points)
## storing overall team Resume Score as vector
BoiseSt_resume <- sum(BoiseSt$Resume_Score) - (7 * nrow(BoiseSt_losses))


## BC
BC <- completed_games |>
  filter(home_team == "Boston College" | away_team == "Boston College") |>
  mutate(team = "Boston College",
         team_opp = case_when(home_team == "Boston College" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Boston College"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BCFBSOpps <- VoA_Variables |>
  filter(team %in% BC$team_opp) |>
  select(team, VoA_Rating)
BCFCSOpps <- FCS |>
  filter(team %in% BC$team_opp) |>
  select(team, rating)
colnames(BCFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BCOpps <- rbind(BCFBSOpps, BCFCSOpps)
colnames(BCOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
BC <- full_join(BC, BCOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
BC <- BC |>
  mutate(actual_diff = case_when(home_team == "Boston College" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Boston College" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Boston College" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Boston College" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
BC_losses <- BC |>
  filter(home_team == "Boston College" & home_points < away_points | away_team == "Boston College" & away_points < home_points)
## storing overall team Resume Score as vector
BC_resume <- sum(BC$Resume_Score) - (7 * nrow(BC_losses))


## BowlingGreen
BowlingGreen <- completed_games |>
  filter(home_team == "Bowling Green" | away_team == "Bowling Green") |>
  mutate(team = "Bowling Green",
         team_opp = case_when(home_team == "Bowling Green" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Bowling Green"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BowlingGreenFBSOpps <- VoA_Variables |>
  filter(team %in% BowlingGreen$team_opp) |>
  select(team, VoA_Rating)
BowlingGreenFCSOpps <- FCS |>
  filter(team %in% BowlingGreen$team_opp) |>
  select(team, rating)
colnames(BowlingGreenFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BowlingGreenOpps <- rbind(BowlingGreenFBSOpps, BowlingGreenFCSOpps)
colnames(BowlingGreenOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
BowlingGreen <- full_join(BowlingGreen, BowlingGreenOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
BowlingGreen <- BowlingGreen |>
  mutate(actual_diff = case_when(home_team == "Bowling Green" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Bowling Green" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Bowling Green" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Bowling Green" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
BowlingGreen_losses <- BowlingGreen |>
  filter(home_team == "Bowling Green" & home_points < away_points | away_team == "Bowling Green" & away_points < home_points)
## storing overall team Resume Score as vector
BowlingGreen_resume <- sum(BowlingGreen$Resume_Score) - (7 * nrow(BowlingGreen_losses))


## Buffalo
Buffalo <- completed_games |>
  filter(home_team == "Buffalo" | away_team == "Buffalo") |>
  mutate(team = "Buffalo",
         team_opp = case_when(home_team == "Buffalo" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Buffalo"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BuffaloFBSOpps <- VoA_Variables |>
  filter(team %in% Buffalo$team_opp) |>
  select(team, VoA_Rating)
BuffaloFCSOpps <- FCS |>
  filter(team %in% Buffalo$team_opp) |>
  select(team, rating)
colnames(BuffaloFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BuffaloOpps <- rbind(BuffaloFBSOpps, BuffaloFCSOpps)
colnames(BuffaloOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Buffalo <- full_join(Buffalo, BuffaloOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Buffalo <- Buffalo |>
  mutate(actual_diff = case_when(home_team == "Buffalo" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Buffalo" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Buffalo" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Buffalo" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Buffalo_losses <- Buffalo |>
  filter(home_team == "Buffalo" & home_points < away_points | away_team == "Buffalo" & away_points < home_points)
## storing overall team Resume Score as vector
Buffalo_resume <- sum(Buffalo$Resume_Score) - (7 * nrow(Buffalo_losses))


## BYU
BYU <- completed_games |>
  filter(home_team == "BYU" | away_team == "BYU") |>
  mutate(team = "BYU",
         team_opp = case_when(home_team == "BYU" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "BYU"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
BYUFBSOpps <- VoA_Variables |>
  filter(team %in% BYU$team_opp) |>
  select(team, VoA_Rating)
BYUFCSOpps <- FCS |>
  filter(team %in% BYU$team_opp) |>
  select(team, rating)
colnames(BYUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
BYUOpps <- rbind(BYUFBSOpps, BYUFCSOpps)
colnames(BYUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
BYU <- full_join(BYU, BYUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
BYU <- BYU |>
  mutate(actual_diff = case_when(home_team == "BYU" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "BYU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "BYU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "BYU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
BYU_losses <- BYU |>
  filter(home_team == "BYU" & home_points < away_points | away_team == "BYU" & away_points < home_points)
## storing overall team Resume Score as vector
BYU_resume <- sum(BYU$Resume_Score) - (7 * nrow(BYU_losses))


## California
California <- completed_games |>
  filter(home_team == "California" | away_team == "California") |>
  mutate(team = "California",
         team_opp = case_when(home_team == "California" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "California"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
CaliforniaFBSOpps <- VoA_Variables |>
  filter(team %in% California$team_opp) |>
  select(team, VoA_Rating)
CaliforniaFCSOpps <- FCS |>
  filter(team %in% California$team_opp) |>
  select(team, rating)
colnames(CaliforniaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
CaliforniaOpps <- rbind(CaliforniaFBSOpps, CaliforniaFCSOpps)
colnames(CaliforniaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
California <- full_join(California, CaliforniaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
California <- California |>
  mutate(actual_diff = case_when(home_team == "California" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "California" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "California" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "California" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
California_losses <- California |>
  filter(home_team == "California" & home_points < away_points | away_team == "California" & away_points < home_points)
## storing overall team Resume Score as vector
California_resume <- sum(California$Resume_Score) - (7 * nrow(California_losses))


## CMU
CMU <- completed_games |>
  filter(home_team == "Central Michigan" | away_team == "Central Michigan") |>
  mutate(team = "Central Michigan",
         team_opp = case_when(home_team == "Central Michigan" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Central Michigan"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
CMUFBSOpps <- VoA_Variables |>
  filter(team %in% CMU$team_opp) |>
  select(team, VoA_Rating)
CMUFCSOpps <- FCS |>
  filter(team %in% CMU$team_opp) |>
  select(team, rating)
colnames(CMUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
CMUOpps <- rbind(CMUFBSOpps, CMUFCSOpps)
colnames(CMUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
CMU <- full_join(CMU, CMUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
CMU <- CMU |>
  mutate(actual_diff = case_when(home_team == "Central Michigan" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Central Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Central Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Central Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
CMU_losses <- CMU |>
  filter(home_team == "Central Michigan" & home_points < away_points | away_team == "Central Michigan" & away_points < home_points)
## storing overall team Resume Score as vector
CMU_resume <- sum(CMU$Resume_Score) - (7 * nrow(CMU_losses))


## Charlotte
Charlotte <- completed_games |>
  filter(home_team == "Charlotte" | away_team == "Charlotte") |>
  mutate(team = "Charlotte",
         team_opp = case_when(home_team == "Charlotte" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Charlotte"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
CharlotteFBSOpps <- VoA_Variables |>
  filter(team %in% Charlotte$team_opp) |>
  select(team, VoA_Rating)
CharlotteFCSOpps <- FCS |>
  filter(team %in% Charlotte$team_opp) |>
  select(team, rating)
colnames(CharlotteFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
CharlotteOpps <- rbind(CharlotteFBSOpps, CharlotteFCSOpps)
colnames(CharlotteOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Charlotte <- full_join(Charlotte, CharlotteOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Charlotte <- Charlotte |>
  mutate(actual_diff = case_when(home_team == "Charlotte" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Charlotte" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Charlotte" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Charlotte" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Charlotte_losses <- Charlotte |>
  filter(home_team == "Charlotte" & home_points < away_points | away_team == "Charlotte" & away_points < home_points)
## storing overall team Resume Score as vector
Charlotte_resume <- sum(Charlotte$Resume_Score) - (7 * nrow(Charlotte_losses))


## Cincinnati
Cincinnati <- completed_games |>
  filter(home_team == "Cincinnati" | away_team == "Cincinnati") |>
  mutate(team = "Cincinnati",
         team_opp = case_when(home_team == "Cincinnati" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Cincinnati"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
CincinnatiFBSOpps <- VoA_Variables |>
  filter(team %in% Cincinnati$team_opp) |>
  select(team, VoA_Rating)
CincinnatiFCSOpps <- FCS |>
  filter(team %in% Cincinnati$team_opp) |>
  select(team, rating)
colnames(CincinnatiFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
CincinnatiOpps <- rbind(CincinnatiFBSOpps, CincinnatiFCSOpps)
colnames(CincinnatiOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Cincinnati <- full_join(Cincinnati, CincinnatiOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Cincinnati <- Cincinnati |>
  mutate(actual_diff = case_when(home_team == "Cincinnati" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Cincinnati" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Cincinnati" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Cincinnati" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Cincinnati_losses <- Cincinnati |>
  filter(home_team == "Cincinnati" & home_points < away_points | away_team == "Cincinnati" & away_points < home_points)
## storing overall team Resume Score as vector
Cincinnati_resume <- sum(Cincinnati$Resume_Score) - (7 * nrow(Cincinnati_losses))


## Clemson
Clemson <- completed_games |>
  filter(home_team == "Clemson" | away_team == "Clemson") |>
  mutate(team = "Clemson",
         team_opp = case_when(home_team == "Clemson" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Clemson"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ClemsonFBSOpps <- VoA_Variables |>
  filter(team %in% Clemson$team_opp) |>
  select(team, VoA_Rating)
ClemsonFCSOpps <- FCS |>
  filter(team %in% Clemson$team_opp) |>
  select(team, rating)
colnames(ClemsonFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ClemsonOpps <- rbind(ClemsonFBSOpps, ClemsonFCSOpps)
colnames(ClemsonOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Clemson <- full_join(Clemson, ClemsonOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Clemson <- Clemson |>
  mutate(actual_diff = case_when(home_team == "Clemson" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Clemson" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Clemson" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Clemson" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Clemson_losses <- Clemson |>
  filter(home_team == "Clemson" & home_points < away_points | away_team == "Clemson" & away_points < home_points)
## storing overall team Resume Score as vector
Clemson_resume <- sum(Clemson$Resume_Score) - (7 * nrow(Clemson_losses))


## CoastalCarolina
CoastalCarolina <- completed_games |>
  filter(home_team == "Coastal Carolina" | away_team == "Coastal Carolina") |>
  mutate(team = "Coastal Carolina",
         team_opp = case_when(home_team == "Coastal Carolina" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Coastal Carolina"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
CoastalCarolinaFBSOpps <- VoA_Variables |>
  filter(team %in% CoastalCarolina$team_opp) |>
  select(team, VoA_Rating)
CoastalCarolinaFCSOpps <- FCS |>
  filter(team %in% CoastalCarolina$team_opp) |>
  select(team, rating)
colnames(CoastalCarolinaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
CoastalCarolinaOpps <- rbind(CoastalCarolinaFBSOpps, CoastalCarolinaFCSOpps)
colnames(CoastalCarolinaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
CoastalCarolina <- full_join(CoastalCarolina, CoastalCarolinaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
CoastalCarolina <- CoastalCarolina |>
  mutate(actual_diff = case_when(home_team == "Coastal Carolina" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Coastal Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Coastal Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Coastal Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
CoastalCarolina_losses <- CoastalCarolina |>
  filter(home_team == "Coastal Carolina" & home_points < away_points | away_team == "Coastal Carolina" & away_points < home_points)
## storing overall team Resume Score as vector
CoastalCarolina_resume <- sum(CoastalCarolina$Resume_Score) - (7 * nrow(CoastalCarolina_losses))


## Colorado
Colorado <- completed_games |>
  filter(home_team == "Colorado" | away_team == "Colorado") |>
  mutate(team = "Colorado",
         team_opp = case_when(home_team == "Colorado" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Colorado"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ColoradoFBSOpps <- VoA_Variables |>
  filter(team %in% Colorado$team_opp) |>
  select(team, VoA_Rating)
ColoradoFCSOpps <- FCS |>
  filter(team %in% Colorado$team_opp) |>
  select(team, rating)
colnames(ColoradoFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ColoradoOpps <- rbind(ColoradoFBSOpps, ColoradoFCSOpps)
colnames(ColoradoOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Colorado <- full_join(Colorado, ColoradoOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Colorado <- Colorado |>
  mutate(actual_diff = case_when(home_team == "Colorado" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Colorado" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Colorado" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Colorado" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Colorado_losses <- Colorado |>
  filter(home_team == "Colorado" & home_points < away_points | away_team == "Colorado" & away_points < home_points)
## storing overall team Resume Score as vector
Colorado_resume <- sum(Colorado$Resume_Score) - (7 * nrow(Colorado_losses))


## ColoradoSt
ColoradoSt <- completed_games |>
  filter(home_team == "Colorado State" | away_team == "Colorado State") |>
  mutate(team = "Colorado State",
         team_opp = case_when(home_team == "Colorado State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Colorado State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ColoradoStFBSOpps <- VoA_Variables |>
  filter(team %in% ColoradoSt$team_opp) |>
  select(team, VoA_Rating)
ColoradoStFCSOpps <- FCS |>
  filter(team %in% ColoradoSt$team_opp) |>
  select(team, rating)
colnames(ColoradoStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ColoradoStOpps <- rbind(ColoradoStFBSOpps, ColoradoStFCSOpps)
colnames(ColoradoStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
ColoradoSt <- full_join(ColoradoSt, ColoradoStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
ColoradoSt <- ColoradoSt |>
  mutate(actual_diff = case_when(home_team == "Colorado State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Colorado State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Colorado State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Colorado State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
ColoradoSt_losses <- ColoradoSt |>
  filter(home_team == "Colorado State" & home_points < away_points | away_team == "Colorado State" & away_points < home_points)
## storing overall team Resume Score as vector
ColoradoSt_resume <- sum(ColoradoSt$Resume_Score) - (7 * nrow(ColoradoSt_losses))


## UConn
Connecticut <- completed_games |>
  filter(home_team == "Connecticut" | away_team == "Connecticut") |>
  mutate(team = "Connecticut",
         team_opp = case_when(home_team == "Connecticut" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Connecticut"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ConnecticutFBSOpps <- VoA_Variables |>
  filter(team %in% Connecticut$team_opp) |>
  select(team, VoA_Rating)
ConnecticutFCSOpps <- FCS |>
  filter(team %in% Connecticut$team_opp) |>
  select(team, rating)
colnames(ConnecticutFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ConnecticutOpps <- rbind(ConnecticutFBSOpps, ConnecticutFCSOpps)
colnames(ConnecticutOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Connecticut <- full_join(Connecticut, ConnecticutOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Connecticut <- Connecticut |>
  mutate(actual_diff = case_when(home_team == "Connecticut" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Connecticut" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Connecticut" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Connecticut" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Connecticut_losses <- Connecticut |>
  filter(home_team == "Connecticut" & home_points < away_points | away_team == "Connecticut" & away_points < home_points)
## storing overall team Resume Score as vector
Connecticut_resume <- sum(Connecticut$Resume_Score) - (7 * nrow(Connecticut_losses))


## Duke
Duke <- completed_games |>
  filter(home_team == "Duke" | away_team == "Duke") |>
  mutate(team = "Duke",
         team_opp = case_when(home_team == "Duke" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Duke"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
DukeFBSOpps <- VoA_Variables |>
  filter(team %in% Duke$team_opp) |>
  select(team, VoA_Rating)
DukeFCSOpps <- FCS |>
  filter(team %in% Duke$team_opp) |>
  select(team, rating)
colnames(DukeFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
DukeOpps <- rbind(DukeFBSOpps, DukeFCSOpps)
colnames(DukeOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Duke <- full_join(Duke, DukeOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Duke <- Duke |>
  mutate(actual_diff = case_when(home_team == "Duke" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Duke" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Duke" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Duke" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Duke_losses <- Duke |>
  filter(home_team == "Duke" & home_points < away_points | away_team == "Duke" & away_points < home_points)
## storing overall team Resume Score as vector
Duke_resume <- sum(Duke$Resume_Score) - (7 * nrow(Duke_losses))


## EastCarolina
EastCarolina <- completed_games |>
  filter(home_team == "East Carolina" | away_team == "East Carolina") |>
  mutate(team = "East Carolina",
         team_opp = case_when(home_team == "East Carolina" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "East Carolina"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
EastCarolinaFBSOpps <- VoA_Variables |>
  filter(team %in% EastCarolina$team_opp) |>
  select(team, VoA_Rating)
EastCarolinaFCSOpps <- FCS |>
  filter(team %in% EastCarolina$team_opp) |>
  select(team, rating)
colnames(EastCarolinaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
EastCarolinaOpps <- rbind(EastCarolinaFBSOpps, EastCarolinaFCSOpps)
colnames(EastCarolinaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
EastCarolina <- full_join(EastCarolina, EastCarolinaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
EastCarolina <- EastCarolina |>
  mutate(actual_diff = case_when(home_team == "East Carolina" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "East Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "East Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "East Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
EastCarolina_losses <- EastCarolina |>
  filter(home_team == "East Carolina" & home_points < away_points | away_team == "East Carolina" & away_points < home_points)
## storing overall team Resume Score as vector
EastCarolina_resume <- sum(EastCarolina$Resume_Score) - (7 * nrow(EastCarolina_losses))


## EMU
EMU <- completed_games |>
  filter(home_team == "Eastern Michigan" | away_team == "Eastern Michigan") |>
  mutate(team = "Eastern Michigan",
         team_opp = case_when(home_team == "Eastern Michigan" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Eastern Michigan"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
EMUFBSOpps <- VoA_Variables |>
  filter(team %in% EMU$team_opp) |>
  select(team, VoA_Rating)
EMUFCSOpps <- FCS |>
  filter(team %in% EMU$team_opp) |>
  select(team, rating)
colnames(EMUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
EMUOpps <- rbind(EMUFBSOpps, EMUFCSOpps)
colnames(EMUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
EMU <- full_join(EMU, EMUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
EMU <- EMU |>
  mutate(actual_diff = case_when(home_team == "Eastern Michigan" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Eastern Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Eastern Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Eastern Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
EMU_losses <- EMU |>
  filter(home_team == "Eastern Michigan" & home_points < away_points | away_team == "Eastern Michigan" & away_points < home_points)
## storing overall team Resume Score as vector
EMU_resume <- sum(EMU$Resume_Score) - (7 * nrow(EMU_losses))


## Florida
Florida <- completed_games |>
  filter(home_team == "Florida" | away_team == "Florida") |>
  mutate(team = "Florida",
         team_opp = case_when(home_team == "Florida" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
FloridaFBSOpps <- VoA_Variables |>
  filter(team %in% Florida$team_opp) |>
  select(team, VoA_Rating)
FloridaFCSOpps <- FCS |>
  filter(team %in% Florida$team_opp) |>
  select(team, rating)
colnames(FloridaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
FloridaOpps <- rbind(FloridaFBSOpps, FloridaFCSOpps)
colnames(FloridaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Florida <- full_join(Florida, FloridaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Florida <- Florida |>
  mutate(actual_diff = case_when(home_team == "Florida" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Florida" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Florida" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Florida" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Florida_losses <- Florida |>
  filter(home_team == "Florida" & home_points < away_points | away_team == "Florida" & away_points < home_points)
## storing overall team Resume Score as vector
Florida_resume <- sum(Florida$Resume_Score) - (7 * nrow(Florida_losses))


## FAU
FAU <- completed_games |>
  filter(home_team == "Florida Atlantic" | away_team == "Florida Atlantic") |>
  mutate(team = "Florida Atlantic",
         team_opp = case_when(home_team == "Florida Atlantic" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida Atlantic"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
FAUFBSOpps <- VoA_Variables |>
  filter(team %in% FAU$team_opp) |>
  select(team, VoA_Rating)
FAUFCSOpps <- FCS |>
  filter(team %in% FAU$team_opp) |>
  select(team, rating)
colnames(FAUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
FAUOpps <- rbind(FAUFBSOpps, FAUFCSOpps)
colnames(FAUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
FAU <- full_join(FAU, FAUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
FAU <- FAU |>
  mutate(actual_diff = case_when(home_team == "Florida Atlantic" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Florida Atlantic" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Florida Atlantic" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Florida Atlantic" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
FAU_losses <- FAU |>
  filter(home_team == "Florida Atlantic" & home_points < away_points | away_team == "Florida Atlantic" & away_points < home_points)
## storing overall team Resume Score as vector
FAU_resume <- sum(FAU$Resume_Score) - (7 * nrow(FAU_losses))


## FIU
FIU <- completed_games |>
  filter(home_team == "Florida International" | away_team == "Florida International") |>
  mutate(team = "Florida International",
         team_opp = case_when(home_team == "Florida International" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida International"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
FIUFBSOpps <- VoA_Variables |>
  filter(team %in% FIU$team_opp) |>
  select(team, VoA_Rating)
FIUFCSOpps <- FCS |>
  filter(team %in% FIU$team_opp) |>
  select(team, rating)
colnames(FIUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
FIUOpps <- rbind(FIUFBSOpps, FIUFCSOpps)
colnames(FIUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
FIU <- full_join(FIU, FIUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
FIU <- FIU |>
  mutate(actual_diff = case_when(home_team == "Florida International" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Florida International" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Florida International" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Florida International" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
FIU_losses <- FIU |>
  filter(home_team == "Florida International" & home_points < away_points | away_team == "Florida International" & away_points < home_points)
## storing overall team Resume Score as vector
FIU_resume <- sum(FIU$Resume_Score) - (7 * nrow(FIU_losses))


## FloridaSt
FloridaSt <- completed_games |>
  filter(home_team == "Florida State" | away_team == "Florida State") |>
  mutate(team = "Florida State",
         team_opp = case_when(home_team == "Florida State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Florida State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
FloridaStFBSOpps <- VoA_Variables |>
  filter(team %in% FloridaSt$team_opp) |>
  select(team, VoA_Rating)
FloridaStFCSOpps <- FCS |>
  filter(team %in% FloridaSt$team_opp) |>
  select(team, rating)
colnames(FloridaStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
FloridaStOpps <- rbind(FloridaStFBSOpps, FloridaStFCSOpps)
colnames(FloridaStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
FloridaSt <- full_join(FloridaSt, FloridaStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
FloridaSt <- FloridaSt |>
  mutate(actual_diff = case_when(home_team == "Florida State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Florida State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Florida State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Florida State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
FloridaSt_losses <- FloridaSt |>
  filter(home_team == "Florida State" & home_points < away_points | away_team == "Florida State" & away_points < home_points)
## storing overall team Resume Score as vector
FloridaSt_resume <- sum(FloridaSt$Resume_Score) - (7 * nrow(FloridaSt_losses))


## FresnoSt
FresnoSt <- completed_games |>
  filter(home_team == "Fresno State" | away_team == "Fresno State") |>
  mutate(team = "Fresno State",
         team_opp = case_when(home_team == "Fresno State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Fresno State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
FresnoStFBSOpps <- VoA_Variables |>
  filter(team %in% FresnoSt$team_opp) |>
  select(team, VoA_Rating)
FresnoStFCSOpps <- FCS |>
  filter(team %in% FresnoSt$team_opp) |>
  select(team, rating)
colnames(FresnoStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
FresnoStOpps <- rbind(FresnoStFBSOpps, FresnoStFCSOpps)
colnames(FresnoStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
FresnoSt <- full_join(FresnoSt, FresnoStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
FresnoSt <- FresnoSt |>
  mutate(actual_diff = case_when(home_team == "Fresno State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Fresno State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Fresno State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Fresno State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
FresnoSt_losses <- FresnoSt |>
  filter(home_team == "Fresno State" & home_points < away_points | away_team == "Fresno State" & away_points < home_points)
## storing overall team Resume Score as vector
FresnoSt_resume <- sum(FresnoSt$Resume_Score) - (7 * nrow(FresnoSt_losses))


## Georgia
Georgia <- completed_games |>
  filter(home_team == "Georgia" | away_team == "Georgia") |>
  mutate(team = "Georgia",
         team_opp = case_when(home_team == "Georgia" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
GeorgiaFBSOpps <- VoA_Variables |>
  filter(team %in% Georgia$team_opp) |>
  select(team, VoA_Rating)
GeorgiaFCSOpps <- FCS |>
  filter(team %in% Georgia$team_opp) |>
  select(team, rating)
colnames(GeorgiaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
GeorgiaOpps <- rbind(GeorgiaFBSOpps, GeorgiaFCSOpps)
colnames(GeorgiaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Georgia <- full_join(Georgia, GeorgiaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Georgia <- Georgia |>
  mutate(actual_diff = case_when(home_team == "Georgia" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Georgia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Georgia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Georgia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Georgia_losses <- Georgia |>
  filter(home_team == "Georgia" & home_points < away_points | away_team == "Georgia" & away_points < home_points)
## storing overall team Resume Score as vector
Georgia_resume <- sum(Georgia$Resume_Score) - (7 * nrow(Georgia_losses))


## GeorgiaSouthern
GeorgiaSouthern <- completed_games |>
  filter(home_team == "Georgia Southern" | away_team == "Georgia Southern") |>
  mutate(team = "Georgia Southern",
         team_opp = case_when(home_team == "Georgia Southern" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia Southern"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
GeorgiaSouthernFBSOpps <- VoA_Variables |>
  filter(team %in% GeorgiaSouthern$team_opp) |>
  select(team, VoA_Rating)
GeorgiaSouthernFCSOpps <- FCS |>
  filter(team %in% GeorgiaSouthern$team_opp) |>
  select(team, rating)
colnames(GeorgiaSouthernFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
GeorgiaSouthernOpps <- rbind(GeorgiaSouthernFBSOpps, GeorgiaSouthernFCSOpps)
colnames(GeorgiaSouthernOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
GeorgiaSouthern <- full_join(GeorgiaSouthern, GeorgiaSouthernOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
GeorgiaSouthern <- GeorgiaSouthern |>
  mutate(actual_diff = case_when(home_team == "Georgia Southern" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Georgia Southern" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Georgia Southern" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Georgia Southern" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
GeorgiaSouthern_losses <- GeorgiaSouthern |>
  filter(home_team == "Georgia Southern" & home_points < away_points | away_team == "Georgia Southern" & away_points < home_points)
## storing overall team Resume Score as vector
GeorgiaSouthern_resume <- sum(GeorgiaSouthern$Resume_Score) - (7 * nrow(GeorgiaSouthern_losses))


## GeorgiaSt
GeorgiaSt <- completed_games |>
  filter(home_team == "Georgia State" | away_team == "Georgia State") |>
  mutate(team = "Georgia State",
         team_opp = case_when(home_team == "Georgia State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
GeorgiaStFBSOpps <- VoA_Variables |>
  filter(team %in% GeorgiaSt$team_opp) |>
  select(team, VoA_Rating)
GeorgiaStFCSOpps <- FCS |>
  filter(team %in% GeorgiaSt$team_opp) |>
  select(team, rating)
colnames(GeorgiaStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
GeorgiaStOpps <- rbind(GeorgiaStFBSOpps, GeorgiaStFCSOpps)
colnames(GeorgiaStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
GeorgiaSt <- full_join(GeorgiaSt, GeorgiaStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
GeorgiaSt <- GeorgiaSt |>
  mutate(actual_diff = case_when(home_team == "Georgia State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Georgia State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Georgia State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Georgia State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
GeorgiaSt_losses <- GeorgiaSt |>
  filter(home_team == "Georgia State" & home_points < away_points | away_team == "Georgia State" & away_points < home_points)
## storing overall team Resume Score as vector
GeorgiaSt_resume <- sum(GeorgiaSt$Resume_Score) - (7 * nrow(GeorgiaSt_losses))


## GeorgiaTech
GeorgiaTech <- completed_games |>
  filter(home_team == "Georgia Tech" | away_team == "Georgia Tech") |>
  mutate(team = "Georgia Tech",
         team_opp = case_when(home_team == "Georgia Tech" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Georgia Tech"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
GeorgiaTechFBSOpps <- VoA_Variables |>
  filter(team %in% GeorgiaTech$team_opp) |>
  select(team, VoA_Rating)
GeorgiaTechFCSOpps <- FCS |>
  filter(team %in% GeorgiaTech$team_opp) |>
  select(team, rating)
colnames(GeorgiaTechFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
GeorgiaTechOpps <- rbind(GeorgiaTechFBSOpps, GeorgiaTechFCSOpps)
colnames(GeorgiaTechOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
GeorgiaTech <- full_join(GeorgiaTech, GeorgiaTechOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
GeorgiaTech <- GeorgiaTech |>
  mutate(actual_diff = case_when(home_team == "Georgia Tech" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Georgia Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Georgia Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Georgia Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
GeorgiaTech_losses <- GeorgiaTech |>
  filter(home_team == "Georgia Tech" & home_points < away_points | away_team == "Georgia Tech" & away_points < home_points)
## storing overall team Resume Score as vector
GeorgiaTech_resume <- sum(GeorgiaTech$Resume_Score) - (7 * nrow(GeorgiaTech_losses))


## Hawaii
Hawaii <- completed_games |>
  filter(home_team == "Hawai'i" | away_team == "Hawai'i") |>
  mutate(team = "Hawai'i",
         team_opp = case_when(home_team == "Hawai'i" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Hawai'i"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
HawaiiFBSOpps <- VoA_Variables |>
  filter(team %in% Hawaii$team_opp) |>
  select(team, VoA_Rating)
HawaiiFCSOpps <- FCS |>
  filter(team %in% Hawaii$team_opp) |>
  select(team, rating)
colnames(HawaiiFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
HawaiiOpps <- rbind(HawaiiFBSOpps, HawaiiFCSOpps)
colnames(HawaiiOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Hawaii <- full_join(Hawaii, HawaiiOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Hawaii <- Hawaii |>
  mutate(actual_diff = case_when(home_team == "Hawai'i" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Hawai'i" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Hawai'i" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Hawai'i" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Hawaii_losses <- Hawaii |>
  filter(home_team == "Hawai'i" & home_points < away_points | away_team == "Hawai'i" & away_points < home_points)
## storing overall team Resume Score as vector
Hawaii_resume <- sum(Hawaii$Resume_Score) - (7 * nrow(Hawaii_losses))


## Houston
Houston <- completed_games |>
  filter(home_team == "Houston" | away_team == "Houston") |>
  mutate(team = "Houston",
         team_opp = case_when(home_team == "Houston" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Houston"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
HoustonFBSOpps <- VoA_Variables |>
  filter(team %in% Houston$team_opp) |>
  select(team, VoA_Rating)
HoustonFCSOpps <- FCS |>
  filter(team %in% Houston$team_opp) |>
  select(team, rating)
colnames(HoustonFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
HoustonOpps <- rbind(HoustonFBSOpps, HoustonFCSOpps)
colnames(HoustonOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Houston <- full_join(Houston, HoustonOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Houston <- Houston |>
  mutate(actual_diff = case_when(home_team == "Houston" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Houston" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Houston" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Houston" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Houston_losses <- Houston |>
  filter(home_team == "Houston" & home_points < away_points | away_team == "Houston" & away_points < home_points)
## storing overall team Resume Score as vector
Houston_resume <- sum(Houston$Resume_Score) - (7 * nrow(Houston_losses))


## Illinois
Illinois <- completed_games |>
  filter(home_team == "Illinois" | away_team == "Illinois") |>
  mutate(team = "Illinois",
         team_opp = case_when(home_team == "Illinois" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Illinois"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
IllinoisFBSOpps <- VoA_Variables |>
  filter(team %in% Illinois$team_opp) |>
  select(team, VoA_Rating)
IllinoisFCSOpps <- FCS |>
  filter(team %in% Illinois$team_opp) |>
  select(team, rating)
colnames(IllinoisFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
IllinoisOpps <- rbind(IllinoisFBSOpps, IllinoisFCSOpps)
colnames(IllinoisOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Illinois <- full_join(Illinois, IllinoisOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Illinois <- Illinois |>
  mutate(actual_diff = case_when(home_team == "Illinois" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Illinois" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Illinois" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Illinois" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Illinois_losses <- Illinois |>
  filter(home_team == "Illinois" & home_points < away_points | away_team == "Illinois" & away_points < home_points)
## storing overall team Resume Score as vector
Illinois_resume <- sum(Illinois$Resume_Score) - (7 * nrow(Illinois_losses))


## Indiana
Indiana <- completed_games |>
  filter(home_team == "Indiana" | away_team == "Indiana") |>
  mutate(team = "Indiana",
         team_opp = case_when(home_team == "Indiana" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Indiana"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
IndianaFBSOpps <- VoA_Variables |>
  filter(team %in% Indiana$team_opp) |>
  select(team, VoA_Rating)
IndianaFCSOpps <- FCS |>
  filter(team %in% Indiana$team_opp) |>
  select(team, rating)
colnames(IndianaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
IndianaOpps <- rbind(IndianaFBSOpps, IndianaFCSOpps)
colnames(IndianaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Indiana <- full_join(Indiana, IndianaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Indiana <- Indiana |>
  mutate(actual_diff = case_when(home_team == "Indiana" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Indiana" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Indiana" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Indiana" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Indiana_losses <- Indiana |>
  filter(home_team == "Indiana" & home_points < away_points | away_team == "Indiana" & away_points < home_points)
## storing overall team Resume Score as vector
Indiana_resume <- sum(Indiana$Resume_Score) - (7 * nrow(Indiana_losses))


## Iowa
Iowa <- completed_games |>
  filter(home_team == "Iowa" | away_team == "Iowa") |>
  mutate(team = "Iowa",
         team_opp = case_when(home_team == "Iowa" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Iowa"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
IowaFBSOpps <- VoA_Variables |>
  filter(team %in% Iowa$team_opp) |>
  select(team, VoA_Rating)
IowaFCSOpps <- FCS |>
  filter(team %in% Iowa$team_opp) |>
  select(team, rating)
colnames(IowaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
IowaOpps <- rbind(IowaFBSOpps, IowaFCSOpps)
colnames(IowaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Iowa <- full_join(Iowa, IowaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Iowa <- Iowa |>
  mutate(actual_diff = case_when(home_team == "Iowa" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Iowa" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Iowa" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Iowa" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Iowa_losses <- Iowa |>
  filter(home_team == "Iowa" & home_points < away_points | away_team == "Iowa" & away_points < home_points)
## storing overall team Resume Score as vector
Iowa_resume <- sum(Iowa$Resume_Score) - (7 * nrow(Iowa_losses))


## IowaSt
IowaSt <- completed_games |>
  filter(home_team == "Iowa State" | away_team == "Iowa State") |>
  mutate(team = "Iowa State",
         team_opp = case_when(home_team == "Iowa State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Iowa State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
IowaStFBSOpps <- VoA_Variables |>
  filter(team %in% IowaSt$team_opp) |>
  select(team, VoA_Rating)
IowaStFCSOpps <- FCS |>
  filter(team %in% IowaSt$team_opp) |>
  select(team, rating)
colnames(IowaStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
IowaStOpps <- rbind(IowaStFBSOpps, IowaStFCSOpps)
colnames(IowaStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
IowaSt <- full_join(IowaSt, IowaStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
IowaSt <- IowaSt |>
  mutate(actual_diff = case_when(home_team == "Iowa State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Iowa State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Iowa State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Iowa State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
IowaSt_losses <- IowaSt |>
  filter(home_team == "Iowa State" & home_points < away_points | away_team == "Iowa State" & away_points < home_points)
## storing overall team Resume Score as vector
IowaSt_resume <- sum(IowaSt$Resume_Score) - (7 * nrow(IowaSt_losses))


## JamesMadison
JamesMadison <- completed_games |>
  filter(home_team == "James Madison" | away_team == "James Madison") |>
  mutate(team = "James Madison",
         team_opp = case_when(home_team == "James Madison" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "James Madison"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
JamesMadisonFBSOpps <- VoA_Variables |>
  filter(team %in% JamesMadison$team_opp) |>
  select(team, VoA_Rating)
JamesMadisonFCSOpps <- FCS |>
  filter(team %in% JamesMadison$team_opp) |>
  select(team, rating)
colnames(JamesMadisonFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
JamesMadisonOpps <- rbind(JamesMadisonFBSOpps, JamesMadisonFCSOpps)
colnames(JamesMadisonOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
JamesMadison <- full_join(JamesMadison, JamesMadisonOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
JamesMadison <- JamesMadison |>
  mutate(actual_diff = case_when(home_team == "James Madison" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "James Madison" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "James Madison" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "James Madison" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
JamesMadison_losses <- JamesMadison |>
  filter(home_team == "James Madison" & home_points < away_points | away_team == "James Madison" & away_points < home_points)
## storing overall team Resume Score as vector
JamesMadison_resume <- sum(JamesMadison$Resume_Score) - (7 * nrow(JamesMadison_losses))


## Kansas
Kansas <- completed_games |>
  filter(home_team == "Kansas" | away_team == "Kansas") |>
  mutate(team = "Kansas",
         team_opp = case_when(home_team == "Kansas" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kansas"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
KansasFBSOpps <- VoA_Variables |>
  filter(team %in% Kansas$team_opp) |>
  select(team, VoA_Rating)
KansasFCSOpps <- FCS |>
  filter(team %in% Kansas$team_opp) |>
  select(team, rating)
colnames(KansasFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
KansasOpps <- rbind(KansasFBSOpps, KansasFCSOpps)
colnames(KansasOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Kansas <- full_join(Kansas, KansasOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Kansas <- Kansas |>
  mutate(actual_diff = case_when(home_team == "Kansas" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Kansas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Kansas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Kansas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Kansas_losses <- Kansas |>
  filter(home_team == "Kansas" & home_points < away_points | away_team == "Kansas" & away_points < home_points)
## storing overall team Resume Score as vector
Kansas_resume <- sum(Kansas$Resume_Score) - (7 * nrow(Kansas_losses))


## KansasSt
KansasSt <- completed_games |>
  filter(home_team == "Kansas State" | away_team == "Kansas State") |>
  mutate(team = "Kansas State",
         team_opp = case_when(home_team == "Kansas State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kansas State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
KansasStFBSOpps <- VoA_Variables |>
  filter(team %in% KansasSt$team_opp) |>
  select(team, VoA_Rating)
KansasStFCSOpps <- FCS |>
  filter(team %in% KansasSt$team_opp) |>
  select(team, rating)
colnames(KansasStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
KansasStOpps <- rbind(KansasStFBSOpps, KansasStFCSOpps)
colnames(KansasStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
KansasSt <- full_join(KansasSt, KansasStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
KansasSt <- KansasSt |>
  mutate(actual_diff = case_when(home_team == "Kansas State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Kansas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Kansas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Kansas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
KansasSt_losses <- KansasSt |>
  filter(home_team == "Kansas State" & home_points < away_points | away_team == "Kansas State" & away_points < home_points)
## storing overall team Resume Score as vector
KansasSt_resume <- sum(KansasSt$Resume_Score) - (7 * nrow(KansasSt_losses))


## KentSt
KentSt <- completed_games |>
  filter(home_team == "Kent State" | away_team == "Kent State") |>
  mutate(team = "Kent State",
         team_opp = case_when(home_team == "Kent State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kent State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
KentStFBSOpps <- VoA_Variables |>
  filter(team %in% KentSt$team_opp) |>
  select(team, VoA_Rating)
KentStFCSOpps <- FCS |>
  filter(team %in% KentSt$team_opp) |>
  select(team, rating)
colnames(KentStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
KentStOpps <- rbind(KentStFBSOpps, KentStFCSOpps)
colnames(KentStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
KentSt <- full_join(KentSt, KentStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
KentSt <- KentSt |>
  mutate(actual_diff = case_when(home_team == "Kent State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Kent State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Kent State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Kent State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
KentSt_losses <- KentSt |>
  filter(home_team == "Kent State" & home_points < away_points | away_team == "Kent State" & away_points < home_points)
## storing overall team Resume Score as vector
KentSt_resume <- sum(KentSt$Resume_Score) - (7 * nrow(KentSt_losses))


## Kentucky
Kentucky <- completed_games |>
  filter(home_team == "Kentucky" | away_team == "Kentucky") |>
  mutate(team = "Kentucky",
         team_opp = case_when(home_team == "Kentucky" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Kentucky"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
KentuckyFBSOpps <- VoA_Variables |>
  filter(team %in% Kentucky$team_opp) |>
  select(team, VoA_Rating)
KentuckyFCSOpps <- FCS |>
  filter(team %in% Kentucky$team_opp) |>
  select(team, rating)
colnames(KentuckyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
KentuckyOpps <- rbind(KentuckyFBSOpps, KentuckyFCSOpps)
colnames(KentuckyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Kentucky <- full_join(Kentucky, KentuckyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Kentucky <- Kentucky |>
  mutate(actual_diff = case_when(home_team == "Kentucky" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Kentucky" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Kentucky" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Kentucky" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Kentucky_losses <- Kentucky |>
  filter(home_team == "Kentucky" & home_points < away_points | away_team == "Kentucky" & away_points < home_points)
## storing overall team Resume Score as vector
Kentucky_resume <- sum(Kentucky$Resume_Score) - (7 * nrow(Kentucky_losses))


## Liberty
Liberty <- completed_games |>
  filter(home_team == "Liberty" | away_team == "Liberty") |>
  mutate(team = "Liberty",
         team_opp = case_when(home_team == "Liberty" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Liberty"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
LibertyFBSOpps <- VoA_Variables |>
  filter(team %in% Liberty$team_opp) |>
  select(team, VoA_Rating)
LibertyFCSOpps <- FCS |>
  filter(team %in% Liberty$team_opp) |>
  select(team, rating)
colnames(LibertyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
LibertyOpps <- rbind(LibertyFBSOpps, LibertyFCSOpps)
colnames(LibertyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Liberty <- full_join(Liberty, LibertyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Liberty <- Liberty |>
  mutate(actual_diff = case_when(home_team == "Liberty" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Liberty" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Liberty" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Liberty" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Liberty_losses <- Liberty |>
  filter(home_team == "Liberty" & home_points < away_points | away_team == "Liberty" & away_points < home_points)
## storing overall team Resume Score as vector
Liberty_resume <- sum(Liberty$Resume_Score) - (7 * nrow(Liberty_losses))


## Louisiana
Louisiana <- completed_games |>
  filter(home_team == "Louisiana" | away_team == "Louisiana") |>
  mutate(team = "Louisiana",
         team_opp = case_when(home_team == "Louisiana" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
LouisianaFBSOpps <- VoA_Variables |>
  filter(team %in% Louisiana$team_opp) |>
  select(team, VoA_Rating)
LouisianaFCSOpps <- FCS |>
  filter(team %in% Louisiana$team_opp) |>
  select(team, rating)
colnames(LouisianaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
LouisianaOpps <- rbind(LouisianaFBSOpps, LouisianaFCSOpps)
colnames(LouisianaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Louisiana <- full_join(Louisiana, LouisianaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Louisiana <- Louisiana |>
  mutate(actual_diff = case_when(home_team == "Louisiana" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Louisiana" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Louisiana" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Louisiana" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Louisiana_losses <- Louisiana |>
  filter(home_team == "Louisiana" & home_points < away_points | away_team == "Louisiana" & away_points < home_points)
## storing overall team Resume Score as vector
Louisiana_resume <- sum(Louisiana$Resume_Score) - (7 * nrow(Louisiana_losses))


## ULM
ULM <- completed_games |>
  filter(home_team == "Louisiana Monroe" | away_team == "Louisiana Monroe") |>
  mutate(team = "Louisiana Monroe",
         team_opp = case_when(home_team == "Louisiana Monroe" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana Monroe"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ULMFBSOpps <- VoA_Variables |>
  filter(team %in% ULM$team_opp) |>
  select(team, VoA_Rating)
ULMFCSOpps <- FCS |>
  filter(team %in% ULM$team_opp) |>
  select(team, rating)
colnames(ULMFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ULMOpps <- rbind(ULMFBSOpps, ULMFCSOpps)
colnames(ULMOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
ULM <- full_join(ULM, ULMOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
ULM <- ULM |>
  mutate(actual_diff = case_when(home_team == "Louisiana Monroe" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Louisiana Monroe" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Louisiana Monroe" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Louisiana Monroe" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
ULM_losses <- ULM |>
  filter(home_team == "Louisiana Monroe" & home_points < away_points | away_team == "Louisiana Monroe" & away_points < home_points)
## storing overall team Resume Score as vector
ULM_resume <- sum(ULM$Resume_Score) - (7 * nrow(ULM_losses))


## LouisianaTech
LouisianaTech <- completed_games |>
  filter(home_team == "Louisiana Tech" | away_team == "Louisiana Tech") |>
  mutate(team = "Louisiana Tech",
         team_opp = case_when(home_team == "Louisiana Tech" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisiana Tech"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
LouisianaTechFBSOpps <- VoA_Variables |>
  filter(team %in% LouisianaTech$team_opp) |>
  select(team, VoA_Rating)
LouisianaTechFCSOpps <- FCS |>
  filter(team %in% LouisianaTech$team_opp) |>
  select(team, rating)
colnames(LouisianaTechFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
LouisianaTechOpps <- rbind(LouisianaTechFBSOpps, LouisianaTechFCSOpps)
colnames(LouisianaTechOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
LouisianaTech <- full_join(LouisianaTech, LouisianaTechOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
LouisianaTech <- LouisianaTech |>
  mutate(actual_diff = case_when(home_team == "Louisiana Tech" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Louisiana Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Louisiana Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Louisiana Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
LouisianaTech_losses <- LouisianaTech |>
  filter(home_team == "Louisiana Tech" & home_points < away_points | away_team == "Louisiana Tech" & away_points < home_points)
## storing overall team Resume Score as vector
LouisianaTech_resume <- sum(LouisianaTech$Resume_Score) - (7 * nrow(LouisianaTech_losses))


## Louisville
Louisville <- completed_games |>
  filter(home_team == "Louisville" | away_team == "Louisville") |>
  mutate(team = "Louisville",
         team_opp = case_when(home_team == "Louisville" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Louisville"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
LouisvilleFBSOpps <- VoA_Variables |>
  filter(team %in% Louisville$team_opp) |>
  select(team, VoA_Rating)
LouisvilleFCSOpps <- FCS |>
  filter(team %in% Louisville$team_opp) |>
  select(team, rating)
colnames(LouisvilleFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
LouisvilleOpps <- rbind(LouisvilleFBSOpps, LouisvilleFCSOpps)
colnames(LouisvilleOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Louisville <- full_join(Louisville, LouisvilleOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Louisville <- Louisville |>
  mutate(actual_diff = case_when(home_team == "Louisville" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Louisville" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Louisville" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Louisville" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Louisville_losses <- Louisville |>
  filter(home_team == "Louisville" & home_points < away_points | away_team == "Louisville" & away_points < home_points)
## storing overall team Resume Score as vector
Louisville_resume <- sum(Louisville$Resume_Score) - (7 * nrow(Louisville_losses))


## LSU
LSU <- completed_games |>
  filter(home_team == "LSU" | away_team == "LSU") |>
  mutate(team = "LSU",
         team_opp = case_when(home_team == "LSU" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "LSU"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
LSUFBSOpps <- VoA_Variables |>
  filter(team %in% LSU$team_opp) |>
  select(team, VoA_Rating)
LSUFCSOpps <- FCS |>
  filter(team %in% LSU$team_opp) |>
  select(team, rating)
colnames(LSUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
LSUOpps <- rbind(LSUFBSOpps, LSUFCSOpps)
colnames(LSUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
LSU <- full_join(LSU, LSUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
LSU <- LSU |>
  mutate(actual_diff = case_when(home_team == "LSU" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "LSU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "LSU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "LSU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
LSU_losses <- LSU |>
  filter(home_team == "LSU" & home_points < away_points | away_team == "LSU" & away_points < home_points)
## storing overall team Resume Score as vector
LSU_resume <- sum(LSU$Resume_Score) - (7 * nrow(LSU_losses))


## Marshall
Marshall <- completed_games |>
  filter(home_team == "Marshall" | away_team == "Marshall") |>
  mutate(team = "Marshall",
         team_opp = case_when(home_team == "Marshall" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Marshall"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MarshallFBSOpps <- VoA_Variables |>
  filter(team %in% Marshall$team_opp) |>
  select(team, VoA_Rating)
MarshallFCSOpps <- FCS |>
  filter(team %in% Marshall$team_opp) |>
  select(team, rating)
colnames(MarshallFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MarshallOpps <- rbind(MarshallFBSOpps, MarshallFCSOpps)
colnames(MarshallOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Marshall <- full_join(Marshall, MarshallOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Marshall <- Marshall |>
  mutate(actual_diff = case_when(home_team == "Marshall" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Marshall" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Marshall" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Marshall" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Marshall_losses <- Marshall |>
  filter(home_team == "Marshall" & home_points < away_points | away_team == "Marshall" & away_points < home_points)
## storing overall team Resume Score as vector
Marshall_resume <- sum(Marshall$Resume_Score) - (7 * nrow(Marshall_losses))


## Maryland
Maryland <- completed_games |>
  filter(home_team == "Maryland" | away_team == "Maryland") |>
  mutate(team = "Maryland",
         team_opp = case_when(home_team == "Maryland" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Maryland"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MarylandFBSOpps <- VoA_Variables |>
  filter(team %in% Maryland$team_opp) |>
  select(team, VoA_Rating)
MarylandFCSOpps <- FCS |>
  filter(team %in% Maryland$team_opp) |>
  select(team, rating)
colnames(MarylandFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MarylandOpps <- rbind(MarylandFBSOpps, MarylandFCSOpps)
colnames(MarylandOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Maryland <- full_join(Maryland, MarylandOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Maryland <- Maryland |>
  mutate(actual_diff = case_when(home_team == "Maryland" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Maryland" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Maryland" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Maryland" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Maryland_losses <- Maryland |>
  filter(home_team == "Maryland" & home_points < away_points | away_team == "Maryland" & away_points < home_points)
## storing overall team Resume Score as vector
Maryland_resume <- sum(Maryland$Resume_Score) - (7 * nrow(Maryland_losses))


## Memphis
Memphis <- completed_games |>
  filter(home_team == "Memphis" | away_team == "Memphis") |>
  mutate(team = "Memphis",
         team_opp = case_when(home_team == "Memphis" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Memphis"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MemphisFBSOpps <- VoA_Variables |>
  filter(team %in% Memphis$team_opp) |>
  select(team, VoA_Rating)
MemphisFCSOpps <- FCS |>
  filter(team %in% Memphis$team_opp) |>
  select(team, rating)
colnames(MemphisFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MemphisOpps <- rbind(MemphisFBSOpps, MemphisFCSOpps)
colnames(MemphisOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Memphis <- full_join(Memphis, MemphisOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Memphis <- Memphis |>
  mutate(actual_diff = case_when(home_team == "Memphis" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Memphis" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Memphis" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Memphis" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Memphis_losses <- Memphis |>
  filter(home_team == "Memphis" & home_points < away_points | away_team == "Memphis" & away_points < home_points)
## storing overall team Resume Score as vector
Memphis_resume <- sum(Memphis$Resume_Score) - (7 * nrow(Memphis_losses))


## Miami
Miami <- completed_games |>
  filter(home_team == "Miami" | away_team == "Miami") |>
  mutate(team = "Miami",
         team_opp = case_when(home_team == "Miami" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Miami"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MiamiFBSOpps <- VoA_Variables |>
  filter(team %in% Miami$team_opp) |>
  select(team, VoA_Rating)
MiamiFCSOpps <- FCS |>
  filter(team %in% Miami$team_opp) |>
  select(team, rating)
colnames(MiamiFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MiamiOpps <- rbind(MiamiFBSOpps, MiamiFCSOpps)
colnames(MiamiOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Miami <- full_join(Miami, MiamiOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Miami <- Miami |>
  mutate(actual_diff = case_when(home_team == "Miami" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Miami" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Miami" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Miami" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Miami_losses <- Miami |>
  filter(home_team == "Miami" & home_points < away_points | away_team == "Miami" & away_points < home_points)
## storing overall team Resume Score as vector
Miami_resume <- sum(Miami$Resume_Score) - (7 * nrow(Miami_losses))


## MiamiOH
MiamiOH <- completed_games |>
  filter(home_team == "Miami (OH)" | away_team == "Miami (OH)") |>
  mutate(team = "Miami (OH)",
         team_opp = case_when(home_team == "Miami (OH)" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Miami (OH)"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MiamiOHFBSOpps <- VoA_Variables |>
  filter(team %in% MiamiOH$team_opp) |>
  select(team, VoA_Rating)
MiamiOHFCSOpps <- FCS |>
  filter(team %in% MiamiOH$team_opp) |>
  select(team, rating)
colnames(MiamiOHFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MiamiOHOpps <- rbind(MiamiOHFBSOpps, MiamiOHFCSOpps)
colnames(MiamiOHOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
MiamiOH <- full_join(MiamiOH, MiamiOHOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
MiamiOH <- MiamiOH |>
  mutate(actual_diff = case_when(home_team == "Miami (OH)" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Miami (OH)" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Miami (OH)" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Miami (OH)" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
MiamiOH_losses <- MiamiOH |>
  filter(home_team == "Miami (OH)" & home_points < away_points | away_team == "Miami (OH)" & away_points < home_points)
## storing overall team Resume Score as vector
MiamiOH_resume <- sum(MiamiOH$Resume_Score) - (7 * nrow(MiamiOH_losses))


## Michigan
Michigan <- completed_games |>
  filter(home_team == "Michigan" | away_team == "Michigan") |>
  mutate(team = "Michigan",
         team_opp = case_when(home_team == "Michigan" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Michigan"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MichiganFBSOpps <- VoA_Variables |>
  filter(team %in% Michigan$team_opp) |>
  select(team, VoA_Rating)
MichiganFCSOpps <- FCS |>
  filter(team %in% Michigan$team_opp) |>
  select(team, rating)
colnames(MichiganFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MichiganOpps <- rbind(MichiganFBSOpps, MichiganFCSOpps)
colnames(MichiganOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Michigan <- full_join(Michigan, MichiganOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Michigan <- Michigan |>
  mutate(actual_diff = case_when(home_team == "Michigan" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Michigan_losses <- Michigan |>
  filter(home_team == "Michigan" & home_points < away_points | away_team == "Michigan" & away_points < home_points)
## storing overall team Resume Score as vector
Michigan_resume <- sum(Michigan$Resume_Score) - (7 * nrow(Michigan_losses))


## MichiganSt
MichiganSt <- completed_games |>
  filter(home_team == "Michigan State" | away_team == "Michigan State") |>
  mutate(team = "Michigan State",
         team_opp = case_when(home_team == "Michigan State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Michigan State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MichiganStFBSOpps <- VoA_Variables |>
  filter(team %in% MichiganSt$team_opp) |>
  select(team, VoA_Rating)
MichiganStFCSOpps <- FCS |>
  filter(team %in% MichiganSt$team_opp) |>
  select(team, rating)
colnames(MichiganStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MichiganStOpps <- rbind(MichiganStFBSOpps, MichiganStFCSOpps)
colnames(MichiganStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
MichiganSt <- full_join(MichiganSt, MichiganStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
MichiganSt <- MichiganSt |>
  mutate(actual_diff = case_when(home_team == "Michigan State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Michigan State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Michigan State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Michigan State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
MichiganSt_losses <- MichiganSt |>
  filter(home_team == "Michigan State" & home_points < away_points | away_team == "Michigan State" & away_points < home_points)
## storing overall team Resume Score as vector
MichiganSt_resume <- sum(MichiganSt$Resume_Score) - (7 * nrow(MichiganSt_losses))


## MiddleTennessee
MiddleTennessee <- completed_games |>
  filter(home_team == "Middle Tennessee" | away_team == "Middle Tennessee") |>
  mutate(team = "Middle Tennessee",
         team_opp = case_when(home_team == "Middle Tennessee" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Middle Tennessee"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MiddleTennesseeFBSOpps <- VoA_Variables |>
  filter(team %in% MiddleTennessee$team_opp) |>
  select(team, VoA_Rating)
MiddleTennesseeFCSOpps <- FCS |>
  filter(team %in% MiddleTennessee$team_opp) |>
  select(team, rating)
colnames(MiddleTennesseeFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MiddleTennesseeOpps <- rbind(MiddleTennesseeFBSOpps, MiddleTennesseeFCSOpps)
colnames(MiddleTennesseeOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
MiddleTennessee <- full_join(MiddleTennessee, MiddleTennesseeOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
MiddleTennessee <- MiddleTennessee |>
  mutate(actual_diff = case_when(home_team == "Middle Tennessee" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Middle Tennessee" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Middle Tennessee" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Middle Tennessee" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
MiddleTennessee_losses <- MiddleTennessee |>
  filter(home_team == "Middle Tennessee" & home_points < away_points | away_team == "Middle Tennessee" & away_points < home_points)
## storing overall team Resume Score as vector
MiddleTennessee_resume <- sum(MiddleTennessee$Resume_Score) - (7 * nrow(MiddleTennessee_losses))


## Minnesota
Minnesota <- completed_games |>
  filter(home_team == "Minnesota" | away_team == "Minnesota") |>
  mutate(team = "Minnesota",
         team_opp = case_when(home_team == "Minnesota" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Minnesota"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MinnesotaFBSOpps <- VoA_Variables |>
  filter(team %in% Minnesota$team_opp) |>
  select(team, VoA_Rating)
MinnesotaFCSOpps <- FCS |>
  filter(team %in% Minnesota$team_opp) |>
  select(team, rating)
colnames(MinnesotaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MinnesotaOpps <- rbind(MinnesotaFBSOpps, MinnesotaFCSOpps)
colnames(MinnesotaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Minnesota <- full_join(Minnesota, MinnesotaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Minnesota <- Minnesota |>
  mutate(actual_diff = case_when(home_team == "Minnesota" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Minnesota" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Minnesota" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Minnesota" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Minnesota_losses <- Minnesota |>
  filter(home_team == "Minnesota" & home_points < away_points | away_team == "Minnesota" & away_points < home_points)
## storing overall team Resume Score as vector
Minnesota_resume <- sum(Minnesota$Resume_Score) - (7 * nrow(Minnesota_losses))


## MissSt
MissSt <- completed_games |>
  filter(home_team == "Mississippi State" | away_team == "Mississippi State") |>
  mutate(team = "Mississippi State",
         team_opp = case_when(home_team == "Mississippi State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Mississippi State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MissStFBSOpps <- VoA_Variables |>
  filter(team %in% MissSt$team_opp) |>
  select(team, VoA_Rating)
MissStFCSOpps <- FCS |>
  filter(team %in% MissSt$team_opp) |>
  select(team, rating)
colnames(MissStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MissStOpps <- rbind(MissStFBSOpps, MissStFCSOpps)
colnames(MissStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
MissSt <- full_join(MissSt, MissStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
MissSt <- MissSt |>
  mutate(actual_diff = case_when(home_team == "Mississippi State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Mississippi State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Mississippi State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Mississippi State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
MissSt_losses <- MissSt |>
  filter(home_team == "Mississippi State" & home_points < away_points | away_team == "Mississippi State" & away_points < home_points)
## storing overall team Resume Score as vector
MissSt_resume <- sum(MissSt$Resume_Score) - (7 * nrow(MissSt_losses))


## Missouri
Missouri <- completed_games |>
  filter(home_team == "Missouri" | away_team == "Missouri") |>
  mutate(team = "Missouri",
         team_opp = case_when(home_team == "Missouri" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Missouri"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
MissouriFBSOpps <- VoA_Variables |>
  filter(team %in% Missouri$team_opp) |>
  select(team, VoA_Rating)
MissouriFCSOpps <- FCS |>
  filter(team %in% Missouri$team_opp) |>
  select(team, rating)
colnames(MissouriFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
MissouriOpps <- rbind(MissouriFBSOpps, MissouriFCSOpps)
colnames(MissouriOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Missouri <- full_join(Missouri, MissouriOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Missouri <- Missouri |>
  mutate(actual_diff = case_when(home_team == "Missouri" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Missouri" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Missouri" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Missouri" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Missouri_losses <- Missouri |>
  filter(home_team == "Missouri" & home_points < away_points | away_team == "Missouri" & away_points < home_points)
## storing overall team Resume Score as vector
Missouri_resume <- sum(Missouri$Resume_Score) - (7 * nrow(Missouri_losses))


## Navy
Navy <- completed_games |>
  filter(home_team == "Navy" | away_team == "Navy") |>
  mutate(team = "Navy",
         team_opp = case_when(home_team == "Navy" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Navy"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NavyFBSOpps <- VoA_Variables |>
  filter(team %in% Navy$team_opp) |>
  select(team, VoA_Rating)
NavyFCSOpps <- FCS |>
  filter(team %in% Navy$team_opp) |>
  select(team, rating)
colnames(NavyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NavyOpps <- rbind(NavyFBSOpps, NavyFCSOpps)
colnames(NavyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Navy <- full_join(Navy, NavyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Navy <- Navy |>
  mutate(actual_diff = case_when(home_team == "Navy" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Navy" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Navy" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Navy" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Navy_losses <- Navy |>
  filter(home_team == "Navy" & home_points < away_points | away_team == "Navy" & away_points < home_points)
## storing overall team Resume Score as vector
Navy_resume <- sum(Navy$Resume_Score) - (7 * nrow(Navy_losses))


## NCSt
NCSt <- completed_games |>
  filter(home_team == "NC State" | away_team == "NC State") |>
  mutate(team = "NC State",
         team_opp = case_when(home_team == "NC State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "NC State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NCStFBSOpps <- VoA_Variables |>
  filter(team %in% NCSt$team_opp) |>
  select(team, VoA_Rating)
NCStFCSOpps <- FCS |>
  filter(team %in% NCSt$team_opp) |>
  select(team, rating)
colnames(NCStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NCStOpps <- rbind(NCStFBSOpps, NCStFCSOpps)
colnames(NCStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NCSt <- full_join(NCSt, NCStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NCSt <- NCSt |>
  mutate(actual_diff = case_when(home_team == "NC State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "NC State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "NC State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "NC State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NCSt_losses <- NCSt |>
  filter(home_team == "NC State" & home_points < away_points | away_team == "NC State" & away_points < home_points)
## storing overall team Resume Score as vector
NCSt_resume <- sum(NCSt$Resume_Score) - (7 * nrow(NCSt_losses))


## Nebraska
Nebraska <- completed_games |>
  filter(home_team == "Nebraska" | away_team == "Nebraska") |>
  mutate(team = "Nebraska",
         team_opp = case_when(home_team == "Nebraska" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Nebraska"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NebraskaFBSOpps <- VoA_Variables |>
  filter(team %in% Nebraska$team_opp) |>
  select(team, VoA_Rating)
NebraskaFCSOpps <- FCS |>
  filter(team %in% Nebraska$team_opp) |>
  select(team, rating)
colnames(NebraskaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NebraskaOpps <- rbind(NebraskaFBSOpps, NebraskaFCSOpps)
colnames(NebraskaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Nebraska <- full_join(Nebraska, NebraskaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Nebraska <- Nebraska |>
  mutate(actual_diff = case_when(home_team == "Nebraska" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Nebraska" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Nebraska" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Nebraska" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Nebraska_losses <- Nebraska |>
  filter(home_team == "Nebraska" & home_points < away_points | away_team == "Nebraska" & away_points < home_points)
## storing overall team Resume Score as vector
Nebraska_resume <- sum(Nebraska$Resume_Score) - (7 * nrow(Nebraska_losses))


## Nevada
Nevada <- completed_games |>
  filter(home_team == "Nevada" | away_team == "Nevada") |>
  mutate(team = "Nevada",
         team_opp = case_when(home_team == "Nevada" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Nevada"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NevadaFBSOpps <- VoA_Variables |>
  filter(team %in% Nevada$team_opp) |>
  select(team, VoA_Rating)
NevadaFCSOpps <- FCS |>
  filter(team %in% Nevada$team_opp) |>
  select(team, rating)
colnames(NevadaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NevadaOpps <- rbind(NevadaFBSOpps, NevadaFCSOpps)
colnames(NevadaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Nevada <- full_join(Nevada, NevadaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Nevada <- Nevada |>
  mutate(actual_diff = case_when(home_team == "Nevada" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Nevada" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Nevada" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Nevada" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Nevada_losses <- Nevada |>
  filter(home_team == "Nevada" & home_points < away_points | away_team == "Nevada" & away_points < home_points)
## storing overall team Resume Score as vector
Nevada_resume <- sum(Nevada$Resume_Score) - (7 * nrow(Nevada_losses))


## NewMexico
NewMexico <- completed_games |>
  filter(home_team == "New Mexico" | away_team == "New Mexico") |>
  mutate(team = "New Mexico",
         team_opp = case_when(home_team == "New Mexico" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "New Mexico"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NewMexicoFBSOpps <- VoA_Variables |>
  filter(team %in% NewMexico$team_opp) |>
  select(team, VoA_Rating)
NewMexicoFCSOpps <- FCS |>
  filter(team %in% NewMexico$team_opp) |>
  select(team, rating)
colnames(NewMexicoFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NewMexicoOpps <- rbind(NewMexicoFBSOpps, NewMexicoFCSOpps)
colnames(NewMexicoOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NewMexico <- full_join(NewMexico, NewMexicoOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NewMexico <- NewMexico |>
  mutate(actual_diff = case_when(home_team == "New Mexico" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "New Mexico" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "New Mexico" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "New Mexico" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NewMexico_losses <- NewMexico |>
  filter(home_team == "New Mexico" & home_points < away_points | away_team == "New Mexico" & away_points < home_points)
## storing overall team Resume Score as vector
NewMexico_resume <- sum(NewMexico$Resume_Score) - (7 * nrow(NewMexico_losses))


## NewMexicoSt
NewMexicoSt <- completed_games |>
  filter(home_team == "New Mexico State" | away_team == "New Mexico State") |>
  mutate(team = "New Mexico State",
         team_opp = case_when(home_team == "New Mexico State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "New Mexico State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NewMexicoStFBSOpps <- VoA_Variables |>
  filter(team %in% NewMexicoSt$team_opp) |>
  select(team, VoA_Rating)
NewMexicoStFCSOpps <- FCS |>
  filter(team %in% NewMexicoSt$team_opp) |>
  select(team, rating)
colnames(NewMexicoStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NewMexicoStOpps <- rbind(NewMexicoStFBSOpps, NewMexicoStFCSOpps)
colnames(NewMexicoStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NewMexicoSt <- full_join(NewMexicoSt, NewMexicoStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NewMexicoSt <- NewMexicoSt |>
  mutate(actual_diff = case_when(home_team == "New Mexico State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "New Mexico State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "New Mexico State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "New Mexico State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NewMexicoSt_losses <- NewMexicoSt |>
  filter(home_team == "New Mexico State" & home_points < away_points | away_team == "New Mexico State" & away_points < home_points)
## storing overall team Resume Score as vector
NewMexicoSt_resume <- sum(NewMexicoSt$Resume_Score) - (7 * nrow(NewMexicoSt_losses))


## NorthCarolina
NorthCarolina <- completed_games |>
  filter(home_team == "North Carolina" | away_team == "North Carolina") |>
  mutate(team = "North Carolina",
         team_opp = case_when(home_team == "North Carolina" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "North Carolina"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NorthCarolinaFBSOpps <- VoA_Variables |>
  filter(team %in% NorthCarolina$team_opp) |>
  select(team, VoA_Rating)
NorthCarolinaFCSOpps <- FCS |>
  filter(team %in% NorthCarolina$team_opp) |>
  select(team, rating)
colnames(NorthCarolinaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NorthCarolinaOpps <- rbind(NorthCarolinaFBSOpps, NorthCarolinaFCSOpps)
colnames(NorthCarolinaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NorthCarolina <- full_join(NorthCarolina, NorthCarolinaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NorthCarolina <- NorthCarolina |>
  mutate(actual_diff = case_when(home_team == "North Carolina" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "North Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "North Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "North Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NorthCarolina_losses <- NorthCarolina |>
  filter(home_team == "North Carolina" & home_points < away_points | away_team == "North Carolina" & away_points < home_points)
## storing overall team Resume Score as vector
NorthCarolina_resume <- sum(NorthCarolina$Resume_Score) - (7 * nrow(NorthCarolina_losses))


## NorthTexas
NorthTexas <- completed_games |>
  filter(home_team == "North Texas" | away_team == "North Texas") |>
  mutate(team = "North Texas",
         team_opp = case_when(home_team == "North Texas" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "North Texas"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NorthTexasFBSOpps <- VoA_Variables |>
  filter(team %in% NorthTexas$team_opp) |>
  select(team, VoA_Rating)
NorthTexasFCSOpps <- FCS |>
  filter(team %in% NorthTexas$team_opp) |>
  select(team, rating)
colnames(NorthTexasFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NorthTexasOpps <- rbind(NorthTexasFBSOpps, NorthTexasFCSOpps)
colnames(NorthTexasOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NorthTexas <- full_join(NorthTexas, NorthTexasOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NorthTexas <- NorthTexas |>
  mutate(actual_diff = case_when(home_team == "North Texas" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "North Texas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "North Texas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "North Texas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NorthTexas_losses <- NorthTexas |>
  filter(home_team == "North Texas" & home_points < away_points | away_team == "North Texas" & away_points < home_points)
## storing overall team Resume Score as vector
NorthTexas_resume <- sum(NorthTexas$Resume_Score) - (7 * nrow(NorthTexas_losses))


## NorthernIllinois
NorthernIllinois <- completed_games |>
  filter(home_team == "Northern Illinois" | away_team == "Northern Illinois") |>
  mutate(team = "Northern Illinois",
         team_opp = case_when(home_team == "Northern Illinois" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Northern Illinois"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NorthernIllinoisFBSOpps <- VoA_Variables |>
  filter(team %in% NorthernIllinois$team_opp) |>
  select(team, VoA_Rating)
NorthernIllinoisFCSOpps <- FCS |>
  filter(team %in% NorthernIllinois$team_opp) |>
  select(team, rating)
colnames(NorthernIllinoisFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NorthernIllinoisOpps <- rbind(NorthernIllinoisFBSOpps, NorthernIllinoisFCSOpps)
colnames(NorthernIllinoisOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NorthernIllinois <- full_join(NorthernIllinois, NorthernIllinoisOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NorthernIllinois <- NorthernIllinois |>
  mutate(actual_diff = case_when(home_team == "Northern Illinois" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Northern Illinois" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Northern Illinois" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Northern Illinois" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NorthernIllinois_losses <- NorthernIllinois |>
  filter(home_team == "Northern Illinois" & home_points < away_points | away_team == "Northern Illinois" & away_points < home_points)
## storing overall team Resume Score as vector
NorthernIllinois_resume <- sum(NorthernIllinois$Resume_Score) - (7 * nrow(NorthernIllinois_losses))


## Northwestern
Northwestern <- completed_games |>
  filter(home_team == "Northwestern" | away_team == "Northwestern") |>
  mutate(team = "Northwestern",
         team_opp = case_when(home_team == "Northwestern" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Northwestern"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NorthwesternFBSOpps <- VoA_Variables |>
  filter(team %in% Northwestern$team_opp) |>
  select(team, VoA_Rating)
NorthwesternFCSOpps <- FCS |>
  filter(team %in% Northwestern$team_opp) |>
  select(team, rating)
colnames(NorthwesternFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NorthwesternOpps <- rbind(NorthwesternFBSOpps, NorthwesternFCSOpps)
colnames(NorthwesternOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Northwestern <- full_join(Northwestern, NorthwesternOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Northwestern <- Northwestern |>
  mutate(actual_diff = case_when(home_team == "Northwestern" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Northwestern" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Northwestern" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Northwestern" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Northwestern_losses <- Northwestern |>
  filter(home_team == "Northwestern" & home_points < away_points | away_team == "Northwestern" & away_points < home_points)
## storing overall team Resume Score as vector
Northwestern_resume <- sum(Northwestern$Resume_Score) - (7 * nrow(Northwestern_losses))


## NotreDame
NotreDame <- completed_games |>
  filter(home_team == "Notre Dame" | away_team == "Notre Dame") |>
  mutate(team = "Notre Dame",
         team_opp = case_when(home_team == "Notre Dame" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Notre Dame"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
NotreDameFBSOpps <- VoA_Variables |>
  filter(team %in% NotreDame$team_opp) |>
  select(team, VoA_Rating)
NotreDameFCSOpps <- FCS |>
  filter(team %in% NotreDame$team_opp) |>
  select(team, rating)
colnames(NotreDameFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
NotreDameOpps <- rbind(NotreDameFBSOpps, NotreDameFCSOpps)
colnames(NotreDameOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
NotreDame <- full_join(NotreDame, NotreDameOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
NotreDame <- NotreDame |>
  mutate(actual_diff = case_when(home_team == "Notre Dame" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Notre Dame" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Notre Dame" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Notre Dame" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
NotreDame_losses <- NotreDame |>
  filter(home_team == "Notre Dame" & home_points < away_points | away_team == "Notre Dame" & away_points < home_points)
## storing overall team Resume Score as vector
NotreDame_resume <- sum(NotreDame$Resume_Score) - (7 * nrow(NotreDame_losses))


## Ohio
Ohio <- completed_games |>
  filter(home_team == "Ohio" | away_team == "Ohio") |>
  mutate(team = "Ohio",
         team_opp = case_when(home_team == "Ohio" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ohio"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OhioFBSOpps <- VoA_Variables |>
  filter(team %in% Ohio$team_opp) |>
  select(team, VoA_Rating)
OhioFCSOpps <- FCS |>
  filter(team %in% Ohio$team_opp) |>
  select(team, rating)
colnames(OhioFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OhioOpps <- rbind(OhioFBSOpps, OhioFCSOpps)
colnames(OhioOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Ohio <- full_join(Ohio, OhioOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Ohio <- Ohio |>
  mutate(actual_diff = case_when(home_team == "Ohio" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Ohio" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Ohio" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Ohio" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Ohio_losses <- Ohio |>
  filter(home_team == "Ohio" & home_points < away_points | away_team == "Ohio" & away_points < home_points)
## storing overall team Resume Score as vector
Ohio_resume <- sum(Ohio$Resume_Score) - (7 * nrow(Ohio_losses))


## OhioSt
OhioSt <- completed_games |>
  filter(home_team == "Ohio State" | away_team == "Ohio State") |>
  mutate(team = "Ohio State",
         team_opp = case_when(home_team == "Ohio State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ohio State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OhioStFBSOpps <- VoA_Variables |>
  filter(team %in% OhioSt$team_opp) |>
  select(team, VoA_Rating)
OhioStFCSOpps <- FCS |>
  filter(team %in% OhioSt$team_opp) |>
  select(team, rating)
colnames(OhioStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OhioStOpps <- rbind(OhioStFBSOpps, OhioStFCSOpps)
colnames(OhioStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
OhioSt <- full_join(OhioSt, OhioStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
OhioSt <- OhioSt |>
  mutate(actual_diff = case_when(home_team == "Ohio State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Ohio State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Ohio State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Ohio State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
OhioSt_losses <- OhioSt |>
  filter(home_team == "Ohio State" & home_points < away_points | away_team == "Ohio State" & away_points < home_points)
## storing overall team Resume Score as vector
OhioSt_resume <- sum(OhioSt$Resume_Score) - (7 * nrow(OhioSt_losses))


## Oklahoma
Oklahoma <- completed_games |>
  filter(home_team == "Oklahoma" | away_team == "Oklahoma") |>
  mutate(team = "Oklahoma",
         team_opp = case_when(home_team == "Oklahoma" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oklahoma"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OklahomaFBSOpps <- VoA_Variables |>
  filter(team %in% Oklahoma$team_opp) |>
  select(team, VoA_Rating)
OklahomaFCSOpps <- FCS |>
  filter(team %in% Oklahoma$team_opp) |>
  select(team, rating)
colnames(OklahomaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OklahomaOpps <- rbind(OklahomaFBSOpps, OklahomaFCSOpps)
colnames(OklahomaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Oklahoma <- full_join(Oklahoma, OklahomaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Oklahoma <- Oklahoma |>
  mutate(actual_diff = case_when(home_team == "Oklahoma" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Oklahoma" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Oklahoma" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Oklahoma" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Oklahoma_losses <- Oklahoma |>
  filter(home_team == "Oklahoma" & home_points < away_points | away_team == "Oklahoma" & away_points < home_points)
## storing overall team Resume Score as vector
Oklahoma_resume <- sum(Oklahoma$Resume_Score) - (7 * nrow(Oklahoma_losses))


## OklahomaSt
OklahomaSt <- completed_games |>
  filter(home_team == "Oklahoma State" | away_team == "Oklahoma State") |>
  mutate(team = "Oklahoma State",
         team_opp = case_when(home_team == "Oklahoma State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oklahoma State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OklahomaStFBSOpps <- VoA_Variables |>
  filter(team %in% OklahomaSt$team_opp) |>
  select(team, VoA_Rating)
OklahomaStFCSOpps <- FCS |>
  filter(team %in% OklahomaSt$team_opp) |>
  select(team, rating)
colnames(OklahomaStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OklahomaStOpps <- rbind(OklahomaStFBSOpps, OklahomaStFCSOpps)
colnames(OklahomaStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
OklahomaSt <- full_join(OklahomaSt, OklahomaStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
OklahomaSt <- OklahomaSt |>
  mutate(actual_diff = case_when(home_team == "Oklahoma State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Oklahoma State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Oklahoma State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Oklahoma State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
OklahomaSt_losses <- OklahomaSt |>
  filter(home_team == "Oklahoma State" & home_points < away_points | away_team == "Oklahoma State" & away_points < home_points)
## storing overall team Resume Score as vector
OklahomaSt_resume <- sum(OklahomaSt$Resume_Score) - (7 * nrow(OklahomaSt_losses))


## OldDominion
OldDominion <- completed_games |>
  filter(home_team == "Old Dominion" | away_team == "Old Dominion") |>
  mutate(team = "Old Dominion",
         team_opp = case_when(home_team == "Old Dominion" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Old Dominion"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OldDominionFBSOpps <- VoA_Variables |>
  filter(team %in% OldDominion$team_opp) |>
  select(team, VoA_Rating)
OldDominionFCSOpps <- FCS |>
  filter(team %in% OldDominion$team_opp) |>
  select(team, rating)
colnames(OldDominionFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OldDominionOpps <- rbind(OldDominionFBSOpps, OldDominionFCSOpps)
colnames(OldDominionOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
OldDominion <- full_join(OldDominion, OldDominionOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
OldDominion <- OldDominion |>
  mutate(actual_diff = case_when(home_team == "Old Dominion" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Old Dominion" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Old Dominion" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Old Dominion" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
OldDominion_losses <- OldDominion |>
  filter(home_team == "Old Dominion" & home_points < away_points | away_team == "Old Dominion" & away_points < home_points)
## storing overall team Resume Score as vector
OldDominion_resume <- sum(OldDominion$Resume_Score) - (7 * nrow(OldDominion_losses))


## OleMiss
OleMiss <- completed_games |>
  filter(home_team == "Ole Miss" | away_team == "Ole Miss") |>
  mutate(team = "Ole Miss",
         team_opp = case_when(home_team == "Ole Miss" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Ole Miss"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OleMissFBSOpps <- VoA_Variables |>
  filter(team %in% OleMiss$team_opp) |>
  select(team, VoA_Rating)
OleMissFCSOpps <- FCS |>
  filter(team %in% OleMiss$team_opp) |>
  select(team, rating)
colnames(OleMissFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OleMissOpps <- rbind(OleMissFBSOpps, OleMissFCSOpps)
colnames(OleMissOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
OleMiss <- full_join(OleMiss, OleMissOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
OleMiss <- OleMiss |>
  mutate(actual_diff = case_when(home_team == "Ole Miss" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Ole Miss" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Ole Miss" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Ole Miss" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
OleMiss_losses <- OleMiss |>
  filter(home_team == "Ole Miss" & home_points < away_points | away_team == "Ole Miss" & away_points < home_points)
## storing overall team Resume Score as vector
OleMiss_resume <- sum(OleMiss$Resume_Score) - (7 * nrow(OleMiss_losses))


## Oregon
Oregon <- completed_games |>
  filter(home_team == "Oregon" | away_team == "Oregon") |>
  mutate(team = "Oregon",
         team_opp = case_when(home_team == "Oregon" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oregon"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OregonFBSOpps <- VoA_Variables |>
  filter(team %in% Oregon$team_opp) |>
  select(team, VoA_Rating)
OregonFCSOpps <- FCS |>
  filter(team %in% Oregon$team_opp) |>
  select(team, rating)
colnames(OregonFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OregonOpps <- rbind(OregonFBSOpps, OregonFCSOpps)
colnames(OregonOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Oregon <- full_join(Oregon, OregonOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Oregon <- Oregon |>
  mutate(actual_diff = case_when(home_team == "Oregon" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Oregon" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Oregon" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Oregon" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Oregon_losses <- Oregon |>
  filter(home_team == "Oregon" & home_points < away_points | away_team == "Oregon" & away_points < home_points)
## storing overall team Resume Score as vector
Oregon_resume <- sum(Oregon$Resume_Score) - (7 * nrow(Oregon_losses))


## OregonSt
OregonSt <- completed_games |>
  filter(home_team == "Oregon State" | away_team == "Oregon State") |>
  mutate(team = "Oregon State",
         team_opp = case_when(home_team == "Oregon State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Oregon State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
OregonStFBSOpps <- VoA_Variables |>
  filter(team %in% OregonSt$team_opp) |>
  select(team, VoA_Rating)
OregonStFCSOpps <- FCS |>
  filter(team %in% OregonSt$team_opp) |>
  select(team, rating)
colnames(OregonStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
OregonStOpps <- rbind(OregonStFBSOpps, OregonStFCSOpps)
colnames(OregonStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
OregonSt <- full_join(OregonSt, OregonStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
OregonSt <- OregonSt |>
  mutate(actual_diff = case_when(home_team == "Oregon State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Oregon State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Oregon State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Oregon State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
OregonSt_losses <- OregonSt |>
  filter(home_team == "Oregon State" & home_points < away_points | away_team == "Oregon State" & away_points < home_points)
## storing overall team Resume Score as vector
OregonSt_resume <- sum(OregonSt$Resume_Score) - (7 * nrow(OregonSt_losses))


## PennSt
PennSt <- completed_games |>
  filter(home_team == "Penn State" | away_team == "Penn State") |>
  mutate(team = "Penn State",
         team_opp = case_when(home_team == "Penn State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Penn State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
PennStFBSOpps <- VoA_Variables |>
  filter(team %in% PennSt$team_opp) |>
  select(team, VoA_Rating)
PennStFCSOpps <- FCS |>
  filter(team %in% PennSt$team_opp) |>
  select(team, rating)
colnames(PennStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
PennStOpps <- rbind(PennStFBSOpps, PennStFCSOpps)
colnames(PennStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
PennSt <- full_join(PennSt, PennStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
PennSt <- PennSt |>
  mutate(actual_diff = case_when(home_team == "Penn State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Penn State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Penn State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Penn State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
PennSt_losses <- PennSt |>
  filter(home_team == "Penn State" & home_points < away_points | away_team == "Penn State" & away_points < home_points)
## storing overall team Resume Score as vector
PennSt_resume <- sum(PennSt$Resume_Score) - (7 * nrow(PennSt_losses))


## Pittsburgh
Pittsburgh <- completed_games |>
  filter(home_team == "Pittsburgh" | away_team == "Pittsburgh") |>
  mutate(team = "Pittsburgh",
         team_opp = case_when(home_team == "Pittsburgh" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Pittsburgh"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
PittsburghFBSOpps <- VoA_Variables |>
  filter(team %in% Pittsburgh$team_opp) |>
  select(team, VoA_Rating)
PittsburghFCSOpps <- FCS |>
  filter(team %in% Pittsburgh$team_opp) |>
  select(team, rating)
colnames(PittsburghFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
PittsburghOpps <- rbind(PittsburghFBSOpps, PittsburghFCSOpps)
colnames(PittsburghOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Pittsburgh <- full_join(Pittsburgh, PittsburghOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Pittsburgh <- Pittsburgh |>
  mutate(actual_diff = case_when(home_team == "Pittsburgh" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Pittsburgh" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Pittsburgh" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Pittsburgh" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Pittsburgh_losses <- Pittsburgh |>
  filter(home_team == "Pittsburgh" & home_points < away_points | away_team == "Pittsburgh" & away_points < home_points)
## storing overall team Resume Score as vector
Pittsburgh_resume <- sum(Pittsburgh$Resume_Score) - (7 * nrow(Pittsburgh_losses))


## Purdue
Purdue <- completed_games |>
  filter(home_team == "Purdue" | away_team == "Purdue") |>
  mutate(team = "Purdue",
         team_opp = case_when(home_team == "Purdue" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Purdue"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
PurdueFBSOpps <- VoA_Variables |>
  filter(team %in% Purdue$team_opp) |>
  select(team, VoA_Rating)
PurdueFCSOpps <- FCS |>
  filter(team %in% Purdue$team_opp) |>
  select(team, rating)
colnames(PurdueFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
PurdueOpps <- rbind(PurdueFBSOpps, PurdueFCSOpps)
colnames(PurdueOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Purdue <- full_join(Purdue, PurdueOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Purdue <- Purdue |>
  mutate(actual_diff = case_when(home_team == "Purdue" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Purdue" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Purdue" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Purdue" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Purdue_losses <- Purdue |>
  filter(home_team == "Purdue" & home_points < away_points | away_team == "Purdue" & away_points < home_points)
## storing overall team Resume Score as vector
Purdue_resume <- sum(Purdue$Resume_Score) - (7 * nrow(Purdue_losses))


## Rice
Rice <- completed_games |>
  filter(home_team == "Rice" | away_team == "Rice") |>
  mutate(team = "Rice",
         team_opp = case_when(home_team == "Rice" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Rice"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
RiceFBSOpps <- VoA_Variables |>
  filter(team %in% Rice$team_opp) |>
  select(team, VoA_Rating)
RiceFCSOpps <- FCS |>
  filter(team %in% Rice$team_opp) |>
  select(team, rating)
colnames(RiceFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
RiceOpps <- rbind(RiceFBSOpps, RiceFCSOpps)
colnames(RiceOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Rice <- full_join(Rice, RiceOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Rice <- Rice |>
  mutate(actual_diff = case_when(home_team == "Rice" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Rice" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Rice" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Rice" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Rice_losses <- Rice |>
  filter(home_team == "Rice" & home_points < away_points | away_team == "Rice" & away_points < home_points)
## storing overall team Resume Score as vector
Rice_resume <- sum(Rice$Resume_Score) - (7 * nrow(Rice_losses))


## Rutgers
Rutgers <- completed_games |>
  filter(home_team == "Rutgers" | away_team == "Rutgers") |>
  mutate(team = "Rutgers",
         team_opp = case_when(home_team == "Rutgers" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Rutgers"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
RutgersFBSOpps <- VoA_Variables |>
  filter(team %in% Rutgers$team_opp) |>
  select(team, VoA_Rating)
RutgersFCSOpps <- FCS |>
  filter(team %in% Rutgers$team_opp) |>
  select(team, rating)
colnames(RutgersFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
RutgersOpps <- rbind(RutgersFBSOpps, RutgersFCSOpps)
colnames(RutgersOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Rutgers <- full_join(Rutgers, RutgersOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Rutgers <- Rutgers |>
  mutate(actual_diff = case_when(home_team == "Rutgers" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Rutgers" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Rutgers" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Rutgers" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Rutgers_losses <- Rutgers |>
  filter(home_team == "Rutgers" & home_points < away_points | away_team == "Rutgers" & away_points < home_points)
## storing overall team Resume Score as vector
Rutgers_resume <- sum(Rutgers$Resume_Score) - (7 * nrow(Rutgers_losses))


## SanDiegoSt
SanDiegoSt <- completed_games |>
  filter(home_team == "San Diego State" | away_team == "San Diego State") |>
  mutate(team = "San Diego State",
         team_opp = case_when(home_team == "San Diego State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "San Diego State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SanDiegoStFBSOpps <- VoA_Variables |>
  filter(team %in% SanDiegoSt$team_opp) |>
  select(team, VoA_Rating)
SanDiegoStFCSOpps <- FCS |>
  filter(team %in% SanDiegoSt$team_opp) |>
  select(team, rating)
colnames(SanDiegoStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SanDiegoStOpps <- rbind(SanDiegoStFBSOpps, SanDiegoStFCSOpps)
colnames(SanDiegoStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SanDiegoSt <- full_join(SanDiegoSt, SanDiegoStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SanDiegoSt <- SanDiegoSt |>
  mutate(actual_diff = case_when(home_team == "San Diego State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "San Diego State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "San Diego State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "San Diego State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SanDiegoSt_losses <- SanDiegoSt |>
  filter(home_team == "San Diego State" & home_points < away_points | away_team == "San Diego State" & away_points < home_points)
## storing overall team Resume Score as vector
SanDiegoSt_resume <- sum(SanDiegoSt$Resume_Score) - (7 * nrow(SanDiegoSt_losses))


## SanJoseSt
SanJoseSt <- completed_games |>
  filter(home_team == "San José State" | away_team == "San José State") |>
  mutate(team = "San José State",
         team_opp = case_when(home_team == "San José State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "San José State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SanJoseStFBSOpps <- VoA_Variables |>
  filter(team %in% SanJoseSt$team_opp) |>
  select(team, VoA_Rating)
SanJoseStFCSOpps <- FCS |>
  filter(team %in% SanJoseSt$team_opp) |>
  select(team, rating)
colnames(SanJoseStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SanJoseStOpps <- rbind(SanJoseStFBSOpps, SanJoseStFCSOpps)
colnames(SanJoseStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SanJoseSt <- full_join(SanJoseSt, SanJoseStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SanJoseSt <- SanJoseSt |>
  mutate(actual_diff = case_when(home_team == "San José State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "San José State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "San José State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "San José State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SanJoseSt_losses <- SanJoseSt |>
  filter(home_team == "San José State" & home_points < away_points | away_team == "San José State" & away_points < home_points)
## storing overall team Resume Score as vector
SanJoseSt_resume <- sum(SanJoseSt$Resume_Score) - (7 * nrow(SanJoseSt_losses))


## SMU
SMU <- completed_games |>
  filter(home_team == "SMU" | away_team == "SMU") |>
  mutate(team = "SMU",
         team_opp = case_when(home_team == "SMU" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "SMU"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SMUFBSOpps <- VoA_Variables |>
  filter(team %in% SMU$team_opp) |>
  select(team, VoA_Rating)
SMUFCSOpps <- FCS |>
  filter(team %in% SMU$team_opp) |>
  select(team, rating)
colnames(SMUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SMUOpps <- rbind(SMUFBSOpps, SMUFCSOpps)
colnames(SMUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SMU <- full_join(SMU, SMUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SMU <- SMU |>
  mutate(actual_diff = case_when(home_team == "SMU" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "SMU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "SMU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "SMU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SMU_losses <- SMU |>
  filter(home_team == "SMU" & home_points < away_points | away_team == "SMU" & away_points < home_points)
## storing overall team Resume Score as vector
SMU_resume <- sum(SMU$Resume_Score) - (7 * nrow(SMU_losses))


## SouthAlabama
SouthAlabama <- completed_games |>
  filter(home_team == "South Alabama" | away_team == "South Alabama") |>
  mutate(team = "South Alabama",
         team_opp = case_when(home_team == "South Alabama" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Alabama"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SouthAlabamaFBSOpps <- VoA_Variables |>
  filter(team %in% SouthAlabama$team_opp) |>
  select(team, VoA_Rating)
SouthAlabamaFCSOpps <- FCS |>
  filter(team %in% SouthAlabama$team_opp) |>
  select(team, rating)
colnames(SouthAlabamaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SouthAlabamaOpps <- rbind(SouthAlabamaFBSOpps, SouthAlabamaFCSOpps)
colnames(SouthAlabamaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SouthAlabama <- full_join(SouthAlabama, SouthAlabamaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SouthAlabama <- SouthAlabama |>
  mutate(actual_diff = case_when(home_team == "South Alabama" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "South Alabama" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "South Alabama" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "South Alabama" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SouthAlabama_losses <- SouthAlabama |>
  filter(home_team == "South Alabama" & home_points < away_points | away_team == "South Alabama" & away_points < home_points)
## storing overall team Resume Score as vector
SouthAlabama_resume <- sum(SouthAlabama$Resume_Score) - (7 * nrow(SouthAlabama_losses))


## SouthCarolina
SouthCarolina <- completed_games |>
  filter(home_team == "South Carolina" | away_team == "South Carolina") |>
  mutate(team = "South Carolina",
         team_opp = case_when(home_team == "South Carolina" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Carolina"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SouthCarolinaFBSOpps <- VoA_Variables |>
  filter(team %in% SouthCarolina$team_opp) |>
  select(team, VoA_Rating)
SouthCarolinaFCSOpps <- FCS |>
  filter(team %in% SouthCarolina$team_opp) |>
  select(team, rating)
colnames(SouthCarolinaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SouthCarolinaOpps <- rbind(SouthCarolinaFBSOpps, SouthCarolinaFCSOpps)
colnames(SouthCarolinaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SouthCarolina <- full_join(SouthCarolina, SouthCarolinaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SouthCarolina <- SouthCarolina |>
  mutate(actual_diff = case_when(home_team == "South Carolina" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "South Carolina" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "South Carolina" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "South Carolina" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SouthCarolina_losses <- SouthCarolina |>
  filter(home_team == "South Carolina" & home_points < away_points | away_team == "South Carolina" & away_points < home_points)
## storing overall team Resume Score as vector
SouthCarolina_resume <- sum(SouthCarolina$Resume_Score) - (7 * nrow(SouthCarolina_losses))


## SouthFlorida
SouthFlorida <- completed_games |>
  filter(home_team == "South Florida" | away_team == "South Florida") |>
  mutate(team = "South Florida",
         team_opp = case_when(home_team == "South Florida" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "South Florida"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SouthFloridaFBSOpps <- VoA_Variables |>
  filter(team %in% SouthFlorida$team_opp) |>
  select(team, VoA_Rating)
SouthFloridaFCSOpps <- FCS |>
  filter(team %in% SouthFlorida$team_opp) |>
  select(team, rating)
colnames(SouthFloridaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SouthFloridaOpps <- rbind(SouthFloridaFBSOpps, SouthFloridaFCSOpps)
colnames(SouthFloridaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SouthFlorida <- full_join(SouthFlorida, SouthFloridaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SouthFlorida <- SouthFlorida |>
  mutate(actual_diff = case_when(home_team == "South Florida" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "South Florida" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "South Florida" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "South Florida" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SouthFlorida_losses <- SouthFlorida |>
  filter(home_team == "South Florida" & home_points < away_points | away_team == "South Florida" & away_points < home_points)
## storing overall team Resume Score as vector
SouthFlorida_resume <- sum(SouthFlorida$Resume_Score) - (7 * nrow(SouthFlorida_losses))


## SouthernMiss
SouthernMiss <- completed_games |>
  filter(home_team == "Southern Mississippi" | away_team == "Southern Mississippi") |>
  mutate(team = "Southern Mississippi",
         team_opp = case_when(home_team == "Southern Mississippi" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Southern Mississippi"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SouthernMissFBSOpps <- VoA_Variables |>
  filter(team %in% SouthernMiss$team_opp) |>
  select(team, VoA_Rating)
SouthernMissFCSOpps <- FCS |>
  filter(team %in% SouthernMiss$team_opp) |>
  select(team, rating)
colnames(SouthernMissFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SouthernMissOpps <- rbind(SouthernMissFBSOpps, SouthernMissFCSOpps)
colnames(SouthernMissOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SouthernMiss <- full_join(SouthernMiss, SouthernMissOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SouthernMiss <- SouthernMiss |>
  mutate(actual_diff = case_when(home_team == "Southern Mississippi" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Southern Mississippi" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Southern Mississippi" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Southern Mississippi" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SouthernMiss_losses <- SouthernMiss |>
  filter(home_team == "Southern Mississippi" & home_points < away_points | away_team == "Southern Mississippi" & away_points < home_points)
## storing overall team Resume Score as vector
SouthernMiss_resume <- sum(SouthernMiss$Resume_Score) - (7 * nrow(SouthernMiss_losses))


## Stanford
Stanford <- completed_games |>
  filter(home_team == "Stanford" | away_team == "Stanford") |>
  mutate(team = "Stanford",
         team_opp = case_when(home_team == "Stanford" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Stanford"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
StanfordFBSOpps <- VoA_Variables |>
  filter(team %in% Stanford$team_opp) |>
  select(team, VoA_Rating)
StanfordFCSOpps <- FCS |>
  filter(team %in% Stanford$team_opp) |>
  select(team, rating)
colnames(StanfordFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
StanfordOpps <- rbind(StanfordFBSOpps, StanfordFCSOpps)
colnames(StanfordOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Stanford <- full_join(Stanford, StanfordOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Stanford <- Stanford |>
  mutate(actual_diff = case_when(home_team == "Stanford" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Stanford" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Stanford" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Stanford" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Stanford_losses <- Stanford |>
  filter(home_team == "Stanford" & home_points < away_points | away_team == "Stanford" & away_points < home_points)
## storing overall team Resume Score as vector
Stanford_resume <- sum(Stanford$Resume_Score) - (7 * nrow(Stanford_losses))


## Syracuse
Syracuse <- completed_games |>
  filter(home_team == "Syracuse" | away_team == "Syracuse") |>
  mutate(team = "Syracuse",
         team_opp = case_when(home_team == "Syracuse" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Syracuse"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SyracuseFBSOpps <- VoA_Variables |>
  filter(team %in% Syracuse$team_opp) |>
  select(team, VoA_Rating)
SyracuseFCSOpps <- FCS |>
  filter(team %in% Syracuse$team_opp) |>
  select(team, rating)
colnames(SyracuseFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SyracuseOpps <- rbind(SyracuseFBSOpps, SyracuseFCSOpps)
colnames(SyracuseOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Syracuse <- full_join(Syracuse, SyracuseOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Syracuse <- Syracuse |>
  mutate(actual_diff = case_when(home_team == "Syracuse" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Syracuse" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Syracuse" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Syracuse" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Syracuse_losses <- Syracuse |>
  filter(home_team == "Syracuse" & home_points < away_points | away_team == "Syracuse" & away_points < home_points)
## storing overall team Resume Score as vector
Syracuse_resume <- sum(Syracuse$Resume_Score) - (7 * nrow(Syracuse_losses))


## TCU
TCU <- completed_games |>
  filter(home_team == "TCU" | away_team == "TCU") |>
  mutate(team = "TCU",
         team_opp = case_when(home_team == "TCU" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "TCU"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TCUFBSOpps <- VoA_Variables |>
  filter(team %in% TCU$team_opp) |>
  select(team, VoA_Rating)
TCUFCSOpps <- FCS |>
  filter(team %in% TCU$team_opp) |>
  select(team, rating)
colnames(TCUFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TCUOpps <- rbind(TCUFBSOpps, TCUFCSOpps)
colnames(TCUOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
TCU <- full_join(TCU, TCUOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
TCU <- TCU |>
  mutate(actual_diff = case_when(home_team == "TCU" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "TCU" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "TCU" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "TCU" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
TCU_losses <- TCU |>
  filter(home_team == "TCU" & home_points < away_points | away_team == "TCU" & away_points < home_points)
## storing overall team Resume Score as vector
TCU_resume <- sum(TCU$Resume_Score) - (7 * nrow(TCU_losses))


## Temple
Temple <- completed_games |>
  filter(home_team == "Temple" | away_team == "Temple") |>
  mutate(team = "Temple",
         team_opp = case_when(home_team == "Temple" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Temple"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TempleFBSOpps <- VoA_Variables |>
  filter(team %in% Temple$team_opp) |>
  select(team, VoA_Rating)
TempleFCSOpps <- FCS |>
  filter(team %in% Temple$team_opp) |>
  select(team, rating)
colnames(TempleFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TempleOpps <- rbind(TempleFBSOpps, TempleFCSOpps)
colnames(TempleOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Temple <- full_join(Temple, TempleOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Temple <- Temple |>
  mutate(actual_diff = case_when(home_team == "Temple" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Temple" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Temple" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Temple" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Temple_losses <- Temple |>
  filter(home_team == "Temple" & home_points < away_points | away_team == "Temple" & away_points < home_points)
## storing overall team Resume Score as vector
Temple_resume <- sum(Temple$Resume_Score) - (7 * nrow(Temple_losses))


## Tennessee
Tennessee <- completed_games |>
  filter(home_team == "Tennessee" | away_team == "Tennessee") |>
  mutate(team = "Tennessee",
         team_opp = case_when(home_team == "Tennessee" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tennessee"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TennesseeFBSOpps <- VoA_Variables |>
  filter(team %in% Tennessee$team_opp) |>
  select(team, VoA_Rating)
TennesseeFCSOpps <- FCS |>
  filter(team %in% Tennessee$team_opp) |>
  select(team, rating)
colnames(TennesseeFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TennesseeOpps <- rbind(TennesseeFBSOpps, TennesseeFCSOpps)
colnames(TennesseeOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Tennessee <- full_join(Tennessee, TennesseeOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Tennessee <- Tennessee |>
  mutate(actual_diff = case_when(home_team == "Tennessee" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Tennessee" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Tennessee" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Tennessee" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Tennessee_losses <- Tennessee |>
  filter(home_team == "Tennessee" & home_points < away_points | away_team == "Tennessee" & away_points < home_points)
## storing overall team Resume Score as vector
Tennessee_resume <- sum(Tennessee$Resume_Score) - (7 * nrow(Tennessee_losses))


## Texas
Texas <- completed_games |>
  filter(home_team == "Texas" | away_team == "Texas") |>
  mutate(team = "Texas",
         team_opp = case_when(home_team == "Texas" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TexasFBSOpps <- VoA_Variables |>
  filter(team %in% Texas$team_opp) |>
  select(team, VoA_Rating)
TexasFCSOpps <- FCS |>
  filter(team %in% Texas$team_opp) |>
  select(team, rating)
colnames(TexasFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TexasOpps <- rbind(TexasFBSOpps, TexasFCSOpps)
colnames(TexasOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Texas <- full_join(Texas, TexasOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Texas <- Texas |>
  mutate(actual_diff = case_when(home_team == "Texas" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Texas" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Texas" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Texas" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Texas_losses <- Texas |>
  filter(home_team == "Texas" & home_points < away_points | away_team == "Texas" & away_points < home_points)
## storing overall team Resume Score as vector
Texas_resume <- sum(Texas$Resume_Score) - (7 * nrow(Texas_losses))


## TexasAM
TexasAM <- completed_games |>
  filter(home_team == "Texas A&M" | away_team == "Texas A&M") |>
  mutate(team = "Texas A&M",
         team_opp = case_when(home_team == "Texas A&M" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas A&M"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TexasAMFBSOpps <- VoA_Variables |>
  filter(team %in% TexasAM$team_opp) |>
  select(team, VoA_Rating)
TexasAMFCSOpps <- FCS |>
  filter(team %in% TexasAM$team_opp) |>
  select(team, rating)
colnames(TexasAMFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TexasAMOpps <- rbind(TexasAMFBSOpps, TexasAMFCSOpps)
colnames(TexasAMOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
TexasAM <- full_join(TexasAM, TexasAMOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
TexasAM <- TexasAM |>
  mutate(actual_diff = case_when(home_team == "Texas A&M" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Texas A&M" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Texas A&M" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Texas A&M" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
TexasAM_losses <- TexasAM |>
  filter(home_team == "Texas A&M" & home_points < away_points | away_team == "Texas A&M" & away_points < home_points)
## storing overall team Resume Score as vector
TexasAM_resume <- sum(TexasAM$Resume_Score) - (7 * nrow(TexasAM_losses))


## TexasSt
TexasSt <- completed_games |>
  filter(home_team == "Texas State" | away_team == "Texas State") |>
  mutate(team = "Texas State",
         team_opp = case_when(home_team == "Texas State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TexasStFBSOpps <- VoA_Variables |>
  filter(team %in% TexasSt$team_opp) |>
  select(team, VoA_Rating)
TexasStFCSOpps <- FCS |>
  filter(team %in% TexasSt$team_opp) |>
  select(team, rating)
colnames(TexasStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TexasStOpps <- rbind(TexasStFBSOpps, TexasStFCSOpps)
colnames(TexasStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
TexasSt <- full_join(TexasSt, TexasStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
TexasSt <- TexasSt |>
  mutate(actual_diff = case_when(home_team == "Texas State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Texas State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Texas State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Texas State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
TexasSt_losses <- TexasSt |>
  filter(home_team == "Texas State" & home_points < away_points | away_team == "Texas State" & away_points < home_points)
## storing overall team Resume Score as vector
TexasSt_resume <- sum(TexasSt$Resume_Score) - (7 * nrow(TexasSt_losses))


## TexasTech
TexasTech <- completed_games |>
  filter(home_team == "Texas Tech" | away_team == "Texas Tech") |>
  mutate(team = "Texas Tech",
         team_opp = case_when(home_team == "Texas Tech" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Texas Tech"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TexasTechFBSOpps <- VoA_Variables |>
  filter(team %in% TexasTech$team_opp) |>
  select(team, VoA_Rating)
TexasTechFCSOpps <- FCS |>
  filter(team %in% TexasTech$team_opp) |>
  select(team, rating)
colnames(TexasTechFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TexasTechOpps <- rbind(TexasTechFBSOpps, TexasTechFCSOpps)
colnames(TexasTechOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
TexasTech <- full_join(TexasTech, TexasTechOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
TexasTech <- TexasTech |>
  mutate(actual_diff = case_when(home_team == "Texas Tech" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Texas Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Texas Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Texas Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
TexasTech_losses <- TexasTech |>
  filter(home_team == "Texas Tech" & home_points < away_points | away_team == "Texas Tech" & away_points < home_points)
## storing overall team Resume Score as vector
TexasTech_resume <- sum(TexasTech$Resume_Score) - (7 * nrow(TexasTech_losses))


## Toledo
Toledo <- completed_games |>
  filter(home_team == "Toledo" | away_team == "Toledo") |>
  mutate(team = "Toledo",
         team_opp = case_when(home_team == "Toledo" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Toledo"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
ToledoFBSOpps <- VoA_Variables |>
  filter(team %in% Toledo$team_opp) |>
  select(team, VoA_Rating)
ToledoFCSOpps <- FCS |>
  filter(team %in% Toledo$team_opp) |>
  select(team, rating)
colnames(ToledoFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
ToledoOpps <- rbind(ToledoFBSOpps, ToledoFCSOpps)
colnames(ToledoOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Toledo <- full_join(Toledo, ToledoOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Toledo <- Toledo |>
  mutate(actual_diff = case_when(home_team == "Toledo" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Toledo" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Toledo" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Toledo" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Toledo_losses <- Toledo |>
  filter(home_team == "Toledo" & home_points < away_points | away_team == "Toledo" & away_points < home_points)
## storing overall team Resume Score as vector
Toledo_resume <- sum(Toledo$Resume_Score) - (7 * nrow(Toledo_losses))


## Troy
Troy <- completed_games |>
  filter(home_team == "Troy" | away_team == "Troy") |>
  mutate(team = "Troy",
         team_opp = case_when(home_team == "Troy" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Troy"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TroyFBSOpps <- VoA_Variables |>
  filter(team %in% Troy$team_opp) |>
  select(team, VoA_Rating)
TroyFCSOpps <- FCS |>
  filter(team %in% Troy$team_opp) |>
  select(team, rating)
colnames(TroyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TroyOpps <- rbind(TroyFBSOpps, TroyFCSOpps)
colnames(TroyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Troy <- full_join(Troy, TroyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Troy <- Troy |>
  mutate(actual_diff = case_when(home_team == "Troy" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Troy" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Troy" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Troy" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Troy_losses <- Troy |>
  filter(home_team == "Troy" & home_points < away_points | away_team == "Troy" & away_points < home_points)
## storing overall team Resume Score as vector
Troy_resume <- sum(Troy$Resume_Score) - (7 * nrow(Troy_losses))


## Tulane
Tulane <- completed_games |>
  filter(home_team == "Tulane" | away_team == "Tulane") |>
  mutate(team = "Tulane",
         team_opp = case_when(home_team == "Tulane" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tulane"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TulaneFBSOpps <- VoA_Variables |>
  filter(team %in% Tulane$team_opp) |>
  select(team, VoA_Rating)
TulaneFCSOpps <- FCS |>
  filter(team %in% Tulane$team_opp) |>
  select(team, rating)
colnames(TulaneFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TulaneOpps <- rbind(TulaneFBSOpps, TulaneFCSOpps)
colnames(TulaneOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Tulane <- full_join(Tulane, TulaneOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Tulane <- Tulane |>
  mutate(actual_diff = case_when(home_team == "Tulane" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Tulane" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Tulane" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Tulane" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Tulane_losses <- Tulane |>
  filter(home_team == "Tulane" & home_points < away_points | away_team == "Tulane" & away_points < home_points)
## storing overall team Resume Score as vector
Tulane_resume <- sum(Tulane$Resume_Score) - (7 * nrow(Tulane_losses))


## Tulsa
Tulsa <- completed_games |>
  filter(home_team == "Tulsa" | away_team == "Tulsa") |>
  mutate(team = "Tulsa",
         team_opp = case_when(home_team == "Tulsa" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Tulsa"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
TulsaFBSOpps <- VoA_Variables |>
  filter(team %in% Tulsa$team_opp) |>
  select(team, VoA_Rating)
TulsaFCSOpps <- FCS |>
  filter(team %in% Tulsa$team_opp) |>
  select(team, rating)
colnames(TulsaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
TulsaOpps <- rbind(TulsaFBSOpps, TulsaFCSOpps)
colnames(TulsaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Tulsa <- full_join(Tulsa, TulsaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Tulsa <- Tulsa |>
  mutate(actual_diff = case_when(home_team == "Tulsa" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Tulsa" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Tulsa" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Tulsa" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Tulsa_losses <- Tulsa |>
  filter(home_team == "Tulsa" & home_points < away_points | away_team == "Tulsa" & away_points < home_points)
## storing overall team Resume Score as vector
Tulsa_resume <- sum(Tulsa$Resume_Score) - (7 * nrow(Tulsa_losses))


## UAB
UAB <- completed_games |>
  filter(home_team == "UAB" | away_team == "UAB") |>
  mutate(team = "UAB",
         team_opp = case_when(home_team == "UAB" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UAB"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UABFBSOpps <- VoA_Variables |>
  filter(team %in% UAB$team_opp) |>
  select(team, VoA_Rating)
UABFCSOpps <- FCS |>
  filter(team %in% UAB$team_opp) |>
  select(team, rating)
colnames(UABFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UABOpps <- rbind(UABFBSOpps, UABFCSOpps)
colnames(UABOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UAB <- full_join(UAB, UABOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UAB <- UAB |>
  mutate(actual_diff = case_when(home_team == "UAB" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UAB" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UAB" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UAB" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UAB_losses <- UAB |>
  filter(home_team == "UAB" & home_points < away_points | away_team == "UAB" & away_points < home_points)
## storing overall team Resume Score as vector
UAB_resume <- sum(UAB$Resume_Score) - (7 * nrow(UAB_losses))


## UCF
UCF <- completed_games |>
  filter(home_team == "UCF" | away_team == "UCF") |>
  mutate(team = "UCF",
         team_opp = case_when(home_team == "UCF" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UCF"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UCFFBSOpps <- VoA_Variables |>
  filter(team %in% UCF$team_opp) |>
  select(team, VoA_Rating)
UCFFCSOpps <- FCS |>
  filter(team %in% UCF$team_opp) |>
  select(team, rating)
colnames(UCFFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UCFOpps <- rbind(UCFFBSOpps, UCFFCSOpps)
colnames(UCFOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UCF <- full_join(UCF, UCFOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UCF <- UCF |>
  mutate(actual_diff = case_when(home_team == "UCF" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UCF" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UCF" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UCF" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UCF_losses <- UCF |>
  filter(home_team == "UCF" & home_points < away_points | away_team == "UCF" & away_points < home_points)
## storing overall team Resume Score as vector
UCF_resume <- sum(UCF$Resume_Score) - (7 * nrow(UCF_losses))


## UCLA
UCLA <- completed_games |>
  filter(home_team == "UCLA" | away_team == "UCLA") |>
  mutate(team = "UCLA",
         team_opp = case_when(home_team == "UCLA" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UCLA"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UCLAFBSOpps <- VoA_Variables |>
  filter(team %in% UCLA$team_opp) |>
  select(team, VoA_Rating)
UCLAFCSOpps <- FCS |>
  filter(team %in% UCLA$team_opp) |>
  select(team, rating)
colnames(UCLAFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UCLAOpps <- rbind(UCLAFBSOpps, UCLAFCSOpps)
colnames(UCLAOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UCLA <- full_join(UCLA, UCLAOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UCLA <- UCLA |>
  mutate(actual_diff = case_when(home_team == "UCLA" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UCLA" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UCLA" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UCLA" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UCLA_losses <- UCLA |>
  filter(home_team == "UCLA" & home_points < away_points | away_team == "UCLA" & away_points < home_points)
## storing overall team Resume Score as vector
UCLA_resume <- sum(UCLA$Resume_Score) - (7 * nrow(UCLA_losses))


## UMass
UMass <- completed_games |>
  filter(home_team == "UMass" | away_team == "UMass") |>
  mutate(team = "UMass",
         team_opp = case_when(home_team == "UMass" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UMass"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UMassFBSOpps <- VoA_Variables |>
  filter(team %in% UMass$team_opp) |>
  select(team, VoA_Rating)
UMassFCSOpps <- FCS |>
  filter(team %in% UMass$team_opp) |>
  select(team, rating)
colnames(UMassFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UMassOpps <- rbind(UMassFBSOpps, UMassFCSOpps)
colnames(UMassOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UMass <- full_join(UMass, UMassOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UMass <- UMass |>
  mutate(actual_diff = case_when(home_team == "UMass" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UMass" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UMass" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UMass" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UMass_losses <- UMass |>
  filter(home_team == "UMass" & home_points < away_points | away_team == "UMass" & away_points < home_points)
## storing overall team Resume Score as vector
UMass_resume <- sum(UMass$Resume_Score) - (7 * nrow(UMass_losses))


## UNLV
UNLV <- completed_games |>
  filter(home_team == "UNLV" | away_team == "UNLV") |>
  mutate(team = "UNLV",
         team_opp = case_when(home_team == "UNLV" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UNLV"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UNLVFBSOpps <- VoA_Variables |>
  filter(team %in% UNLV$team_opp) |>
  select(team, VoA_Rating)
UNLVFCSOpps <- FCS |>
  filter(team %in% UNLV$team_opp) |>
  select(team, rating)
colnames(UNLVFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UNLVOpps <- rbind(UNLVFBSOpps, UNLVFCSOpps)
colnames(UNLVOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UNLV <- full_join(UNLV, UNLVOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UNLV <- UNLV |>
  mutate(actual_diff = case_when(home_team == "UNLV" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UNLV" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UNLV" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UNLV" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UNLV_losses <- UNLV |>
  filter(home_team == "UNLV" & home_points < away_points | away_team == "UNLV" & away_points < home_points)
## storing overall team Resume Score as vector
UNLV_resume <- sum(UNLV$Resume_Score) - (7 * nrow(UNLV_losses))


## USC
USC <- completed_games |>
  filter(home_team == "USC" | away_team == "USC") |>
  mutate(team = "USC",
         team_opp = case_when(home_team == "USC" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "USC"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
USCFBSOpps <- VoA_Variables |>
  filter(team %in% USC$team_opp) |>
  select(team, VoA_Rating)
USCFCSOpps <- FCS |>
  filter(team %in% USC$team_opp) |>
  select(team, rating)
colnames(USCFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
USCOpps <- rbind(USCFBSOpps, USCFCSOpps)
colnames(USCOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
USC <- full_join(USC, USCOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
USC <- USC |>
  mutate(actual_diff = case_when(home_team == "USC" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "USC" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "USC" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "USC" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
USC_losses <- USC |>
  filter(home_team == "USC" & home_points < away_points | away_team == "USC" & away_points < home_points)
## storing overall team Resume Score as vector
USC_resume <- sum(USC$Resume_Score) - (7 * nrow(USC_losses))


## UTSA
UTSA <- completed_games |>
  filter(home_team == "UT San Antonio" | away_team == "UT San Antonio") |>
  mutate(team = "UT San Antonio",
         team_opp = case_when(home_team == "UT San Antonio" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UT San Antonio"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UTSAFBSOpps <- VoA_Variables |>
  filter(team %in% UTSA$team_opp) |>
  select(team, VoA_Rating)
UTSAFCSOpps <- FCS |>
  filter(team %in% UTSA$team_opp) |>
  select(team, rating)
colnames(UTSAFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UTSAOpps <- rbind(UTSAFBSOpps, UTSAFCSOpps)
colnames(UTSAOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UTSA <- full_join(UTSA, UTSAOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UTSA <- UTSA |>
  mutate(actual_diff = case_when(home_team == "UT San Antonio" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UT San Antonio" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UT San Antonio" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UT San Antonio" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UTSA_losses <- UTSA |>
  filter(home_team == "UT San Antonio" & home_points < away_points | away_team == "UT San Antonio" & away_points < home_points)
## storing overall team Resume Score as vector
UTSA_resume <- sum(UTSA$Resume_Score) - (7 * nrow(UTSA_losses))


## Utah
Utah <- completed_games |>
  filter(home_team == "Utah" | away_team == "Utah") |>
  mutate(team = "Utah",
         team_opp = case_when(home_team == "Utah" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Utah"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UtahFBSOpps <- VoA_Variables |>
  filter(team %in% Utah$team_opp) |>
  select(team, VoA_Rating)
UtahFCSOpps <- FCS |>
  filter(team %in% Utah$team_opp) |>
  select(team, rating)
colnames(UtahFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UtahOpps <- rbind(UtahFBSOpps, UtahFCSOpps)
colnames(UtahOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Utah <- full_join(Utah, UtahOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Utah <- Utah |>
  mutate(actual_diff = case_when(home_team == "Utah" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Utah" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Utah" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Utah" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Utah_losses <- Utah |>
  filter(home_team == "Utah" & home_points < away_points | away_team == "Utah" & away_points < home_points)
## storing overall team Resume Score as vector
Utah_resume <- sum(Utah$Resume_Score) - (7 * nrow(Utah_losses))


## UtahSt
UtahSt <- completed_games |>
  filter(home_team == "Utah State" | away_team == "Utah State") |>
  mutate(team = "Utah State",
         team_opp = case_when(home_team == "Utah State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Utah State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UtahStFBSOpps <- VoA_Variables |>
  filter(team %in% UtahSt$team_opp) |>
  select(team, VoA_Rating)
UtahStFCSOpps <- FCS |>
  filter(team %in% UtahSt$team_opp) |>
  select(team, rating)
colnames(UtahStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UtahStOpps <- rbind(UtahStFBSOpps, UtahStFCSOpps)
colnames(UtahStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UtahSt <- full_join(UtahSt, UtahStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UtahSt <- UtahSt |>
  mutate(actual_diff = case_when(home_team == "Utah State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Utah State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Utah State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Utah State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UtahSt_losses <- UtahSt |>
  filter(home_team == "Utah State" & home_points < away_points | away_team == "Utah State" & away_points < home_points)
## storing overall team Resume Score as vector
UtahSt_resume <- sum(UtahSt$Resume_Score) - (7 * nrow(UtahSt_losses))


## UTEP
UTEP <- completed_games |>
  filter(home_team == "UTEP" | away_team == "UTEP") |>
  mutate(team = "UTEP",
         team_opp = case_when(home_team == "UTEP" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "UTEP"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
UTEPFBSOpps <- VoA_Variables |>
  filter(team %in% UTEP$team_opp) |>
  select(team, VoA_Rating)
UTEPFCSOpps <- FCS |>
  filter(team %in% UTEP$team_opp) |>
  select(team, rating)
colnames(UTEPFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
UTEPOpps <- rbind(UTEPFBSOpps, UTEPFCSOpps)
colnames(UTEPOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
UTEP <- full_join(UTEP, UTEPOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
UTEP <- UTEP |>
  mutate(actual_diff = case_when(home_team == "UTEP" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "UTEP" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "UTEP" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "UTEP" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
UTEP_losses <- UTEP |>
  filter(home_team == "UTEP" & home_points < away_points | away_team == "UTEP" & away_points < home_points)
## storing overall team Resume Score as vector
UTEP_resume <- sum(UTEP$Resume_Score) - (7 * nrow(UTEP_losses))


## Vanderbilt
Vanderbilt <- completed_games |>
  filter(home_team == "Vanderbilt" | away_team == "Vanderbilt") |>
  mutate(team = "Vanderbilt",
         team_opp = case_when(home_team == "Vanderbilt" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Vanderbilt"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
VanderbiltFBSOpps <- VoA_Variables |>
  filter(team %in% Vanderbilt$team_opp) |>
  select(team, VoA_Rating)
VanderbiltFCSOpps <- FCS |>
  filter(team %in% Vanderbilt$team_opp) |>
  select(team, rating)
colnames(VanderbiltFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
VanderbiltOpps <- rbind(VanderbiltFBSOpps, VanderbiltFCSOpps)
colnames(VanderbiltOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Vanderbilt <- full_join(Vanderbilt, VanderbiltOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Vanderbilt <- Vanderbilt |>
  mutate(actual_diff = case_when(home_team == "Vanderbilt" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Vanderbilt" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Vanderbilt" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Vanderbilt" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Vanderbilt_losses <- Vanderbilt |>
  filter(home_team == "Vanderbilt" & home_points < away_points | away_team == "Vanderbilt" & away_points < home_points)
## storing overall team Resume Score as vector
Vanderbilt_resume <- sum(Vanderbilt$Resume_Score) - (7 * nrow(Vanderbilt_losses))


## Virginia
Virginia <- completed_games |>
  filter(home_team == "Virginia" | away_team == "Virginia") |>
  mutate(team = "Virginia",
         team_opp = case_when(home_team == "Virginia" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Virginia"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
VirginiaFBSOpps <- VoA_Variables |>
  filter(team %in% Virginia$team_opp) |>
  select(team, VoA_Rating)
VirginiaFCSOpps <- FCS |>
  filter(team %in% Virginia$team_opp) |>
  select(team, rating)
colnames(VirginiaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
VirginiaOpps <- rbind(VirginiaFBSOpps, VirginiaFCSOpps)
colnames(VirginiaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Virginia <- full_join(Virginia, VirginiaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Virginia <- Virginia |>
  mutate(actual_diff = case_when(home_team == "Virginia" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Virginia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Virginia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Virginia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Virginia_losses <- Virginia |>
  filter(home_team == "Virginia" & home_points < away_points | away_team == "Virginia" & away_points < home_points)
## storing overall team Resume Score as vector
Virginia_resume <- sum(Virginia$Resume_Score) - (7 * nrow(Virginia_losses))


## VirginiaTech
VirginiaTech <- completed_games |>
  filter(home_team == "Virginia Tech" | away_team == "Virginia Tech") |>
  mutate(team = "Virginia Tech",
         team_opp = case_when(home_team == "Virginia Tech" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Virginia Tech"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
VirginiaTechFBSOpps <- VoA_Variables |>
  filter(team %in% VirginiaTech$team_opp) |>
  select(team, VoA_Rating)
VirginiaTechFCSOpps <- FCS |>
  filter(team %in% VirginiaTech$team_opp) |>
  select(team, rating)
colnames(VirginiaTechFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
VirginiaTechOpps <- rbind(VirginiaTechFBSOpps, VirginiaTechFCSOpps)
colnames(VirginiaTechOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
VirginiaTech <- full_join(VirginiaTech, VirginiaTechOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
VirginiaTech <- VirginiaTech |>
  mutate(actual_diff = case_when(home_team == "Virginia Tech" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Virginia Tech" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Virginia Tech" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Virginia Tech" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
VirginiaTech_losses <- VirginiaTech |>
  filter(home_team == "Virginia Tech" & home_points < away_points | away_team == "Virginia Tech" & away_points < home_points)
## storing overall team Resume Score as vector
VirginiaTech_resume <- sum(VirginiaTech$Resume_Score) - (7 * nrow(VirginiaTech_losses))


## WakeForest
WakeForest <- completed_games |>
  filter(home_team == "Wake Forest" | away_team == "Wake Forest") |>
  mutate(team = "Wake Forest",
         team_opp = case_when(home_team == "Wake Forest" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wake Forest"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WakeForestFBSOpps <- VoA_Variables |>
  filter(team %in% WakeForest$team_opp) |>
  select(team, VoA_Rating)
WakeForestFCSOpps <- FCS |>
  filter(team %in% WakeForest$team_opp) |>
  select(team, rating)
colnames(WakeForestFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WakeForestOpps <- rbind(WakeForestFBSOpps, WakeForestFCSOpps)
colnames(WakeForestOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
WakeForest <- full_join(WakeForest, WakeForestOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
WakeForest <- WakeForest |>
  mutate(actual_diff = case_when(home_team == "Wake Forest" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Wake Forest" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Wake Forest" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Wake Forest" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
WakeForest_losses <- WakeForest |>
  filter(home_team == "Wake Forest" & home_points < away_points | away_team == "Wake Forest" & away_points < home_points)
## storing overall team Resume Score as vector
WakeForest_resume <- sum(WakeForest$Resume_Score) - (7 * nrow(WakeForest_losses))


## Washington
Washington <- completed_games |>
  filter(home_team == "Washington" | away_team == "Washington") |>
  mutate(team = "Washington",
         team_opp = case_when(home_team == "Washington" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Washington"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WashingtonFBSOpps <- VoA_Variables |>
  filter(team %in% Washington$team_opp) |>
  select(team, VoA_Rating)
WashingtonFCSOpps <- FCS |>
  filter(team %in% Washington$team_opp) |>
  select(team, rating)
colnames(WashingtonFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WashingtonOpps <- rbind(WashingtonFBSOpps, WashingtonFCSOpps)
colnames(WashingtonOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Washington <- full_join(Washington, WashingtonOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Washington <- Washington |>
  mutate(actual_diff = case_when(home_team == "Washington" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Washington" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Washington" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Washington" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Washington_losses <- Washington |>
  filter(home_team == "Washington" & home_points < away_points | away_team == "Washington" & away_points < home_points)
## storing overall team Resume Score as vector
Washington_resume <- sum(Washington$Resume_Score) - (7 * nrow(Washington_losses))


## WashingtonSt
WashingtonSt <- completed_games |>
  filter(home_team == "Washington State" | away_team == "Washington State") |>
  mutate(team = "Washington State",
         team_opp = case_when(home_team == "Washington State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Washington State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WashingtonStFBSOpps <- VoA_Variables |>
  filter(team %in% WashingtonSt$team_opp) |>
  select(team, VoA_Rating)
WashingtonStFCSOpps <- FCS |>
  filter(team %in% WashingtonSt$team_opp) |>
  select(team, rating)
colnames(WashingtonStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WashingtonStOpps <- rbind(WashingtonStFBSOpps, WashingtonStFCSOpps)
colnames(WashingtonStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
WashingtonSt <- full_join(WashingtonSt, WashingtonStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
WashingtonSt <- WashingtonSt |>
  mutate(actual_diff = case_when(home_team == "Washington State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Washington State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Washington State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Washington State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
WashingtonSt_losses <- WashingtonSt |>
  filter(home_team == "Washington State" & home_points < away_points | away_team == "Washington State" & away_points < home_points)
## storing overall team Resume Score as vector
WashingtonSt_resume <- sum(WashingtonSt$Resume_Score) - (7 * nrow(WashingtonSt_losses))


## WestVirginia
WestVirginia <- completed_games |>
  filter(home_team == "West Virginia" | away_team == "West Virginia") |>
  mutate(team = "West Virginia",
         team_opp = case_when(home_team == "West Virginia" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "West Virginia"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WestVirginiaFBSOpps <- VoA_Variables |>
  filter(team %in% WestVirginia$team_opp) |>
  select(team, VoA_Rating)
WestVirginiaFCSOpps <- FCS |>
  filter(team %in% WestVirginia$team_opp) |>
  select(team, rating)
colnames(WestVirginiaFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WestVirginiaOpps <- rbind(WestVirginiaFBSOpps, WestVirginiaFCSOpps)
colnames(WestVirginiaOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
WestVirginia <- full_join(WestVirginia, WestVirginiaOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
WestVirginia <- WestVirginia |>
  mutate(actual_diff = case_when(home_team == "West Virginia" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "West Virginia" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "West Virginia" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "West Virginia" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
WestVirginia_losses <- WestVirginia |>
  filter(home_team == "West Virginia" & home_points < away_points | away_team == "West Virginia" & away_points < home_points)
## storing overall team Resume Score as vector
WestVirginia_resume <- sum(WestVirginia$Resume_Score) - (7 * nrow(WestVirginia_losses))


## WesternKentucky
WesternKentucky <- completed_games |>
  filter(home_team == "Western Kentucky" | away_team == "Western Kentucky") |>
  mutate(team = "Western Kentucky",
         team_opp = case_when(home_team == "Western Kentucky" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Western Kentucky"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WesternKentuckyFBSOpps <- VoA_Variables |>
  filter(team %in% WesternKentucky$team_opp) |>
  select(team, VoA_Rating)
WesternKentuckyFCSOpps <- FCS |>
  filter(team %in% WesternKentucky$team_opp) |>
  select(team, rating)
colnames(WesternKentuckyFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WesternKentuckyOpps <- rbind(WesternKentuckyFBSOpps, WesternKentuckyFCSOpps)
colnames(WesternKentuckyOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
WesternKentucky <- full_join(WesternKentucky, WesternKentuckyOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
WesternKentucky <- WesternKentucky |>
  mutate(actual_diff = case_when(home_team == "Western Kentucky" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Western Kentucky" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Western Kentucky" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Western Kentucky" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
WesternKentucky_losses <- WesternKentucky |>
  filter(home_team == "Western Kentucky" & home_points < away_points | away_team == "Western Kentucky" & away_points < home_points)
## storing overall team Resume Score as vector
WesternKentucky_resume <- sum(WesternKentucky$Resume_Score) - (7 * nrow(WesternKentucky_losses))


## WesternMichigan
WesternMichigan <- completed_games |>
  filter(home_team == "Western Michigan" | away_team == "Western Michigan") |>
  mutate(team = "Western Michigan",
         team_opp = case_when(home_team == "Western Michigan" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Western Michigan"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WesternMichiganFBSOpps <- VoA_Variables |>
  filter(team %in% WesternMichigan$team_opp) |>
  select(team, VoA_Rating)
WesternMichiganFCSOpps <- FCS |>
  filter(team %in% WesternMichigan$team_opp) |>
  select(team, rating)
colnames(WesternMichiganFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WesternMichiganOpps <- rbind(WesternMichiganFBSOpps, WesternMichiganFCSOpps)
colnames(WesternMichiganOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
WesternMichigan <- full_join(WesternMichigan, WesternMichiganOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
WesternMichigan <- WesternMichigan |>
  mutate(actual_diff = case_when(home_team == "Western Michigan" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Western Michigan" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Western Michigan" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Western Michigan" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
WesternMichigan_losses <- WesternMichigan |>
  filter(home_team == "Western Michigan" & home_points < away_points | away_team == "Western Michigan" & away_points < home_points)
## storing overall team Resume Score as vector
WesternMichigan_resume <- sum(WesternMichigan$Resume_Score) - (7 * nrow(WesternMichigan_losses))


## Wisconsin
Wisconsin <- completed_games |>
  filter(home_team == "Wisconsin" | away_team == "Wisconsin") |>
  mutate(team = "Wisconsin",
         team_opp = case_when(home_team == "Wisconsin" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wisconsin"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WisconsinFBSOpps <- VoA_Variables |>
  filter(team %in% Wisconsin$team_opp) |>
  select(team, VoA_Rating)
WisconsinFCSOpps <- FCS |>
  filter(team %in% Wisconsin$team_opp) |>
  select(team, rating)
colnames(WisconsinFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WisconsinOpps <- rbind(WisconsinFBSOpps, WisconsinFCSOpps)
colnames(WisconsinOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Wisconsin <- full_join(Wisconsin, WisconsinOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Wisconsin <- Wisconsin |>
  mutate(actual_diff = case_when(home_team == "Wisconsin" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Wisconsin" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Wisconsin" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Wisconsin" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Wisconsin_losses <- Wisconsin |>
  filter(home_team == "Wisconsin" & home_points < away_points | away_team == "Wisconsin" & away_points < home_points)
## storing overall team Resume Score as vector
Wisconsin_resume <- sum(Wisconsin$Resume_Score) - (7 * nrow(Wisconsin_losses))


## Wyoming
Wyoming <- completed_games |>
  filter(home_team == "Wyoming" | away_team == "Wyoming") |>
  mutate(team = "Wyoming",
         team_opp = case_when(home_team == "Wyoming" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Wyoming"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
WyomingFBSOpps <- VoA_Variables |>
  filter(team %in% Wyoming$team_opp) |>
  select(team, VoA_Rating)
WyomingFCSOpps <- FCS |>
  filter(team %in% Wyoming$team_opp) |>
  select(team, rating)
colnames(WyomingFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
WyomingOpps <- rbind(WyomingFBSOpps, WyomingFCSOpps)
colnames(WyomingOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
Wyoming <- full_join(Wyoming, WyomingOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
Wyoming <- Wyoming |>
  mutate(actual_diff = case_when(home_team == "Wyoming" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Wyoming" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Wyoming" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Wyoming" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
Wyoming_losses <- Wyoming |>
  filter(home_team == "Wyoming" & home_points < away_points | away_team == "Wyoming" & away_points < home_points)
## storing overall team Resume Score as vector
Wyoming_resume <- sum(Wyoming$Resume_Score) - (7 * nrow(Wyoming_losses))

## Sam Houston State
SamHoustonSt <- completed_games |>
  filter(home_team == "Sam Houston State" | away_team == "Sam Houston State") |>
  mutate(team = "Sam Houston State",
         team_opp = case_when(home_team == "Sam Houston State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Sam Houston State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
SamHoustonStFBSOpps <- VoA_Variables |>
  filter(team %in% SamHoustonSt$team_opp) |>
  select(team, VoA_Rating)
SamHoustonStFCSOpps <- FCS |>
  filter(team %in% SamHoustonSt$team_opp) |>
  select(team, rating)
colnames(SamHoustonStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
SamHoustonStOpps <- rbind(SamHoustonStFBSOpps, SamHoustonStFCSOpps)
colnames(SamHoustonStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
SamHoustonSt <- full_join(SamHoustonSt, SamHoustonStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
SamHoustonSt <- SamHoustonSt |>
  mutate(actual_diff = case_when(home_team == "Sam Houston State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Sam Houston State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Sam Houston State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Sam Houston State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
SamHoustonSt_losses <- SamHoustonSt |>
  filter(home_team == "Sam Houston State" & home_points < away_points | away_team == "Sam Houston State" & away_points < home_points)
## storing overall team Resume Score as vector
SamHoustonSt_resume <- sum(SamHoustonSt$Resume_Score) - (7 * nrow(SamHoustonSt_losses))

## Jacksonville State
JacksonvilleSt <- completed_games |>
  filter(home_team == "Jacksonville State" | away_team == "Jacksonville State") |>
  mutate(team = "Jacksonville State",
         team_opp = case_when(home_team == "Jacksonville State" ~ away_team,
                              TRUE ~ home_team),
         team_VoA_rating = VoA_Variables$VoA_Rating[VoA_Variables$team == "Jacksonville State"]) |>
  filter(completed == TRUE)

## creating df of VoA/SRS ratings of opponents
JacksonvilleStFBSOpps <- VoA_Variables |>
  filter(team %in% JacksonvilleSt$team_opp) |>
  select(team, VoA_Rating)
JacksonvilleStFCSOpps <- FCS |>
  filter(team %in% JacksonvilleSt$team_opp) |>
  select(team, rating)
colnames(JacksonvilleStFCSOpps) <- c("team", "VoA_Rating")
## combining FBS VoA ratings and FCS SRS ratings into 1 df
JacksonvilleStOpps <- rbind(JacksonvilleStFBSOpps, JacksonvilleStFCSOpps)
colnames(JacksonvilleStOpps) <- c("team_opp", "opp_VoA_rating")

## adding opponent ratings to main team df
JacksonvilleSt <- full_join(JacksonvilleSt, JacksonvilleStOpps, by = "team_opp")
## calculating actual difference, projected difference, projected Top12 margin, and individual game resume score
JacksonvilleSt <- JacksonvilleSt |>
  mutate(actual_diff = case_when(home_team == "Jacksonville State" ~ home_points - away_points,
                                 TRUE ~ away_points - home_points),
         projected_diff = case_when(home_team == "Jacksonville State" & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                    away_team == "Jacksonville State" & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                    TRUE ~ team_VoA_rating - opp_VoA_rating),
         Top12_proj = case_when(home_team == "Jacksonville State" & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                TRUE ~ Top12_mean - opp_VoA_rating),
         Resume_Score = actual_diff - Top12_proj)

## determining number of losses
JacksonvilleSt_losses <- JacksonvilleSt |>
  filter(home_team == "Jacksonville State" & home_points < away_points | away_team == "Jacksonville State" & away_points < home_points)
## storing overall team Resume Score as vector
JacksonvilleSt_resume <- sum(JacksonvilleSt$Resume_Score) - (7 * nrow(JacksonvilleSt_losses))

##### Creating Data frame of just teams and Resume VoA Scores #####
ResumeVoA <- VoA_Variables |>
  select(team) |>
  mutate(Resume_VoA=case_when(team == "Air Force" ~ AirForce_resume,
                              team == "Akron" ~ Akron_resume,
                              team == "Alabama" ~ Alabama_resume,
                              team == "Appalachian State" ~ AppalachianSt_resume,
                              team == "Arizona" ~ Arizona_resume,
                              team == "Arizona State" ~ ArizonaSt_resume,
                              team == "Arkansas" ~ Arkansas_resume,
                              team == "Arkansas State" ~ ArkansasSt_resume,
                              team == "Army" ~ Army_resume,
                              team == "Auburn" ~ Auburn_resume,
                              team == "Ball State" ~ BallSt_resume,
                              team == "Baylor" ~ Baylor_resume,
                              team == "Boise State" ~ BoiseSt_resume,
                              team == "Boston College" ~ BC_resume,
                              team == "Bowling Green" ~ BowlingGreen_resume,
                              team == "Buffalo" ~ Buffalo_resume,
                              team == "BYU" ~ BYU_resume,
                              team == "California" ~ California_resume,
                              team == "Central Michigan" ~ CMU_resume,
                              team == "Charlotte" ~ Charlotte_resume,
                              team == "Cincinnati" ~ Cincinnati_resume,
                              team == "Clemson" ~ Clemson_resume,
                              team == "Coastal Carolina" ~ CoastalCarolina_resume,
                              team == "Colorado" ~ Colorado_resume,
                              team == "Colorado State" ~ ColoradoSt_resume,
                              team == "Connecticut" ~ Connecticut_resume,
                              team == "Duke" ~ Duke_resume,
                              team == "East Carolina" ~ EastCarolina_resume,
                              team == "Eastern Michigan" ~ EMU_resume,
                              team == "Florida" ~ Florida_resume,
                              team == "Florida Atlantic" ~ FAU_resume,
                              team == "Florida International" ~ FIU_resume,
                              team == "Florida State" ~ FloridaSt_resume,
                              team == "Fresno State" ~ FresnoSt_resume,
                              team == "Georgia" ~ Georgia_resume,
                              team == "Georgia Southern" ~ GeorgiaSouthern_resume,
                              team == "Georgia State" ~ GeorgiaSt_resume,
                              team == "Georgia Tech" ~ GeorgiaTech_resume,
                              team == "Hawai'i" ~ Hawaii_resume,
                              team == "Houston" ~ Houston_resume,
                              team == "Illinois" ~ Illinois_resume,
                              team == "Indiana" ~ Indiana_resume,
                              team == "Iowa" ~ Iowa_resume,
                              team == "Iowa State" ~ IowaSt_resume,
                              team == "James Madison" ~ JamesMadison_resume,
                              team == "Kansas" ~ Kansas_resume,
                              team == "Kansas State" ~ KansasSt_resume,
                              team == "Kent State" ~ KentSt_resume,
                              team == "Kentucky" ~ Kentucky_resume,
                              team == "Liberty" ~ Liberty_resume,
                              team == "Louisiana" ~ Louisiana_resume,
                              team == "Louisiana Monroe" ~ ULM_resume,
                              team == "Louisiana Tech" ~ LouisianaTech_resume,
                              team == "Louisville" ~ Louisville_resume,
                              team == "LSU" ~ LSU_resume,
                              team == "Marshall" ~ Marshall_resume,
                              team == "Maryland" ~ Maryland_resume,
                              team == "Memphis" ~ Memphis_resume,
                              team == "Miami" ~ Miami_resume,
                              team == "Miami (OH)" ~ MiamiOH_resume,
                              team == "Michigan" ~ Michigan_resume,
                              team == "Michigan State" ~ MichiganSt_resume,
                              team == "Middle Tennessee" ~ MiddleTennessee_resume,
                              team == "Minnesota" ~ Minnesota_resume,
                              team == "Mississippi State" ~ MissSt_resume,
                              team == "Missouri" ~ Missouri_resume,
                              team == "Navy" ~ Navy_resume,
                              team == "NC State" ~ NCSt_resume,
                              team == "Nebraska" ~ Nebraska_resume,
                              team == "Nevada" ~ Nevada_resume,
                              team == "New Mexico" ~ NewMexico_resume,
                              team == "New Mexico State" ~ NewMexicoSt_resume,
                              team == "North Carolina" ~ NorthCarolina_resume,
                              team == "North Texas" ~ NorthTexas_resume,
                              team == "Northern Illinois" ~ NorthernIllinois_resume,
                              team == "Northwestern" ~ Northwestern_resume,
                              team == "Notre Dame" ~ NotreDame_resume,
                              team == "Ohio" ~ Ohio_resume,
                              team == "Ohio State" ~ OhioSt_resume,
                              team == "Oklahoma" ~ Oklahoma_resume,
                              team == "Oklahoma State" ~ OklahomaSt_resume,
                              team == "Old Dominion" ~ OldDominion_resume,
                              team == "Ole Miss" ~ OleMiss_resume,
                              team == "Oregon" ~ Oregon_resume,
                              team == "Oregon State" ~ OregonSt_resume,
                              team == "Penn State" ~ PennSt_resume,
                              team == "Pittsburgh" ~ Pittsburgh_resume,
                              team == "Purdue" ~ Purdue_resume,
                              team == "Rice" ~ Rice_resume,
                              team == "Rutgers" ~ Rutgers_resume,
                              team == "San Diego State" ~ SanDiegoSt_resume,
                              team == "San José State" ~ SanJoseSt_resume,
                              team == "SMU" ~ SMU_resume,
                              team == "South Alabama" ~ SouthAlabama_resume,
                              team == "South Carolina" ~ SouthCarolina_resume,
                              team == "South Florida" ~ SouthFlorida_resume,
                              team == "Southern Mississippi" ~ SouthernMiss_resume,
                              team == "Stanford" ~ Stanford_resume,
                              team == "Syracuse" ~ Syracuse_resume,
                              team == "TCU" ~ TCU_resume,
                              team == "Temple" ~ Temple_resume,
                              team == "Tennessee" ~ Tennessee_resume,
                              team == "Texas" ~ Texas_resume,
                              team == "Texas A&M" ~ TexasAM_resume,
                              team == "Texas State" ~ TexasSt_resume,
                              team == "Texas Tech" ~ TexasTech_resume,
                              team == "Toledo" ~ Toledo_resume,
                              team == "Troy" ~ Troy_resume,
                              team == "Tulane" ~ Tulane_resume,
                              team == "Tulsa" ~ Tulsa_resume,
                              team == "UAB" ~ UAB_resume,
                              team == "UCF" ~ UCF_resume,
                              team == "UCLA" ~ UCLA_resume,
                              team == "UMass" ~ UMass_resume,
                              team == "UNLV" ~ UNLV_resume,
                              team == "USC" ~ USC_resume,
                              team == "UT San Antonio" ~ UTSA_resume,
                              team == "Utah" ~ Utah_resume,
                              team == "Utah State" ~ UtahSt_resume,
                              team == "UTEP" ~ UTEP_resume,
                              team == "Vanderbilt" ~ Vanderbilt_resume,
                              team == "Virginia" ~ Virginia_resume,
                              team == "Virginia Tech" ~ VirginiaTech_resume,
                              team == "Wake Forest" ~ WakeForest_resume,
                              team == "Washington" ~ Washington_resume,
                              team == "Washington State" ~ WashingtonSt_resume,
                              team == "West Virginia" ~ WestVirginia_resume,
                              team == "Western Kentucky" ~ WesternKentucky_resume,
                              team == "Western Michigan" ~ WesternMichigan_resume,
                              team == "Wisconsin" ~ Wisconsin_resume,
                              team == "Wyoming" ~ Wyoming_resume,
                              team == "Sam Houston State" ~ SamHoustonSt_resume,
                              team == "Jacksonville State" ~ JacksonvilleSt_resume),
         Resume_VoA_Rank = dense_rank(desc(Resume_VoA))) |>
  arrange(Resume_VoA_Rank)