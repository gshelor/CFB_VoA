##### Evaluating Error Metrics for CFB Vortex of Accuracy ######
## Analyzing how far off I was compared to the spread provided in collegefootballdata.com's prediction contest for the 2021-22 season and investigating possible patterns and/or trends related to it
### Load Packages
library(pacman)
pacman::p_load(tidyverse, matrixStats, grid, gridExtra, gt, gtExtras, viridis, webshot2, cfbfastR, here, ggsci, RColorBrewer, ggpubr, Metrics, ModelMetrics)

### identifying season and week of season
season <- readline("What season is it? ")
cfb_week <- readline("What week just occurred? ")

### strings I want to preserve
# VoA_text <- "VoA"
VoP_text <- "VoP"
week_text <- "Week"

### Reading in VoA to filter games to only be between FBS teams since VoA only rates/ranks FBS teams
PrevWeekVoA <- read_csv(here("Data", paste0("VoA", season), paste0(season, week_text, as.character(as.numeric(cfb_week) - 1), "_VoA.csv")))


### reading in completed games
if (as.numeric(cfb_week) == 17){
  ### reading in previous week's data
  PrevWeekVoP <- read_csv(here("Data", paste0("VoA", season), "Projections", paste0(season, VoP_text, week_text, "16Games.csv"))) |>
    filter(home %in% PrevWeekVoA$team & away %in% PrevWeekVoA$team) |>
    select(id, predicted)
  colnames(PrevWeekVoP) <- c("game_id", "proj_margin")
  
  LastWeekGames <- cfbd_game_info(as.numeric(season), season_type = "postseason") |>
    filter(completed == TRUE) |>
    filter(game_id %in% PrevWeekVoP$game_id) |>
    select(game_id, season, week, completed, home_team, home_points, away_team, away_points) |>
    mutate(result = away_points - home_points,
           winner = case_when(result > 0 ~ away_team,
                              TRUE ~ home_team))
  
  LastWeekSpreads_temp <- cfbd_betting_lines(year = as.numeric(season), season_type = "postseason") |>
    filter(game_id %in% PrevWeekVoP$game_id) |>
    select(game_id, spread)
} else{
  ### reading in most recent week's projections
  PrevWeekVoP <- read_csv(here("Data", paste0("VoA", season), "Projections", paste0(season, VoP_text, week_text, cfb_week, "Games.csv"))) |>
    filter(home %in% PrevWeekVoA$team & away %in% PrevWeekVoA$team) |>
    select(id, predicted)
  colnames(PrevWeekVoP) <- c("game_id", "proj_margin")
  
  
  LastWeekGames <- cfbd_game_info(as.numeric(season)) |>
    filter(completed == TRUE) |>
    filter(week == as.numeric(cfb_week)) |>
    filter(game_id %in% PrevWeekVoP$game_id) |>
    select(game_id, season, week, completed, home_team, home_points, away_team, away_points) |>
    mutate(result = away_points - home_points,
           winner = case_when(result > 0 ~ away_team,
                              TRUE ~ home_team))
  
  LastWeekSpreads_temp <- cfbd_betting_lines(year = as.numeric(season)) |>
    filter(week == as.numeric(cfb_week)) |>
    filter(game_id %in% PrevWeekVoP$game_id) |>
    select(game_id, spread)
}
### converting spread to number (why isn't already a number???)
LastWeekSpreads_temp$spread <- as.numeric(LastWeekSpreads_temp$spread)

### default betting_lines output comes with multiple spreads for some games
### grouping games by game_id and taking the average of the spreads
LastWeekSpreads <- LastWeekSpreads_temp |>
  group_by(game_id) |>
  summarise(mean_spread = mean(spread))


### merging df with all completed games that have projections, does not include spread
spread_games_list <- list(LastWeekGames, LastWeekSpreads, PrevWeekVoP)
LastWeekGames <- spread_games_list |>
  reduce(full_join, by = "game_id") |>
  ### just gonna ignore games that have NAs for anything interesting
  ### will also be useful if a game gets cancelled like AppSt/Liberty in 2024
  drop_na() |>
  ### calculating error metrics for both my projections and betting spreads
  mutate(abs_error = Metrics::ae(result, proj_margin),
         vegas_abs_error = Metrics::ae(result, mean_spread),
         sqd_error = Metrics::se(result, proj_margin),
         vegas_sqd_error = Metrics::se(result, mean_spread),
         straight_up_win = case_when(result >= 0 & proj_margin >= 0 ~ 1,
                                     result <= 0 & proj_margin <= 0 ~ 1,
                                     TRUE ~ 0),
         vegas_straight_up_win = case_when(result >= 0 & mean_spread >= 0 ~ 1,
                                     result <= 0 & mean_spread <= 0 ~ 1,
                                     TRUE ~ 0),
         ATS_win = case_when(result > mean_spread & proj_margin > mean_spread ~ 1,
                             result < mean_spread & proj_margin < mean_spread ~ 1,
                             TRUE ~ 0),
         AE_ATS_win = case_when(abs_error < vegas_abs_error ~ 1,
                                TRUE ~ 0))

### calculating weekly average error metrics for games with spread info available
WeekMeanAccuracyMetrics <- data.frame(week = as.numeric(cfb_week),
                                      games = nrow(LastWeekGames),
                                      mean_ae = mean(LastWeekGames$abs_error),
                                      mean_vegas_ae = mean(LastWeekGames$vegas_abs_error),
                                      mean_se = mean(LastWeekGames$sqd_error),
                                      mean_vegas_se = mean(LastWeekGames$vegas_sqd_error),
                                      RMSE = rmse(LastWeekGames$result, LastWeekGames$proj_margin),
                                      vegas_RMSE = rmse(LastWeekGames$result, LastWeekGames$mean_spread),
                                      straight_up_win_pct = sum(LastWeekGames$straight_up_win) / nrow(LastWeekGames),
                                      vegas_straight_up_win_pct = sum(LastWeekGames$vegas_straight_up_win) / nrow(LastWeekGames),
                                      ATS_win_pct = sum(LastWeekGames$ATS_win) / nrow(LastWeekGames),
                                      AE_ATS_win_pct = sum(LastWeekGames$AE_ATS_win) / nrow(LastWeekGames))


if (as.numeric(cfb_week) == 1){
  ### writing csv with individual games + accuracy metrics
  write_csv(LastWeekGames, here("Data", paste0("VoA", season), "AccuracyMetrics", "Games", paste0("VoA", season, week_text, "1", week_text, cfb_week, "GameAccuracyMetrics.csv")))
  
  ### writing csv with just weekly average calculated for accuracy metrics
  write_csv(WeekMeanAccuracyMetrics, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, cfb_week, "WeekAccuracyMetrics.csv")))
  
} else if (as.numeric(cfb_week) >= 2){
  ### reading in csv of previous games with error calculated, binding current week's games to that 
  PrevWeekGameAccuracyMetrics <- read_csv(here("Data", paste0("VoA", season), "AccuracyMetrics", "Games", paste0("VoA", season, week_text, "1", week_text, as.character(as.numeric(cfb_week) - 1), "GameAccuracyMetrics.csv")))
  CompletedGames <- rbind(PrevWeekGameAccuracyMetrics, LastWeekGames)
  
  ### writing csv with individual games + accuracy metrics
  write_csv(CompletedGames, here("Data", paste0("VoA", season), "AccuracyMetrics", "Games", paste0("VoA", season, week_text, "1", week_text, cfb_week, "GameAccuracyMetrics.csv")))
  
  ### reading in csv of weekly average, binding current week to it
  PrevWeeklyAvgAccuracyMetrics <- read_csv(here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, as.character(as.numeric(cfb_week) - 1), "WeekAccuracyMetrics.csv")))
  CompletedWeeks <- rbind(PrevWeeklyAvgAccuracyMetrics, WeekMeanAccuracyMetrics)
  
  ### writing csv with just weekly average calculated for accuracy metrics
  write_csv(CompletedWeeks, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, cfb_week, "WeekAccuracyMetrics.csv")))
} else{
  print("week not properly entered")
}


if (as.numeric(cfb_week) >= 5){
  SeasonMetrics <- CompletedGames |>
    group_by(season) |>
    summarize(games = nrow(CompletedGames),
              mean_ae = mean(abs_error),
              mean_vegas_ae = mean(vegas_abs_error),
              mean_se = mean(sqd_error),
              mean_vegas_se = mean(vegas_sqd_error),
              RMSE = rmse(result, proj_margin),
              vegas_RMSE = rmse(result, mean_spread),
              straight_up_win_pct = sum(straight_up_win) / nrow(CompletedGames),
              vegas_straight_up_win_pct = sum(vegas_straight_up_win) / nrow(CompletedGames),
              ATS_win_pct = sum(ATS_win) / nrow(CompletedGames),
              AE_ATS_win_pct = sum(AE_ATS_win) / nrow(CompletedGames)) |>
    drop_na()
  
  write_csv(SeasonMetrics, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, "SeasonAccuracyMetrics.csv")))
} else{
  print("season metrics not being calculated yet!")
}
