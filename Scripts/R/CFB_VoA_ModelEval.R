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

### reading in previous week's data
PrevWeekVoP <- read_csv(here("Data", paste0("VoA", season), "Projections", paste0(season, VoP_text, week_text, cfb_week, "Games.csv"))) |>
  select(id, predicted)
colnames(PrevWeekVoP) <- c("game_id", "proj_margin")

### reading in completed games
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
### converting spread to number (why isn't already a number???)
LastWeekSpreads_temp$spread <- as.numeric(LastWeekSpreads_temp$spread)

### default betting_lines output comes with multiple spreads for some games
### grouping games by game_id and taking the average of the spreads
LastWeekSpreads <- LastWeekSpreads_temp |>
  group_by(game_id) |>
  summarise(mean_spread = mean(spread))


### filtering out projections just to include games where there's a spread available
PrevWeekVoP_spread <- PrevWeekVoP |>
  filter(game_id %in% LastWeekSpreads$game_id)
### filtering out game info to only include games where spread is available
LastWeekGames_spread <- LastWeekGames |>
  filter(game_id %in% LastWeekSpreads$game_id)


### merging df with all completed games that have projections, does not include spread
LastWeekGames <- full_join(LastWeekGames, PrevWeekVoP, by = "game_id")

spread_games_list <- list(LastWeekGames_spread, LastWeekSpreads, PrevWeekVoP_spread)
LastWeekGames_spread <- spread_games_list |>
  reduce(full_join, by = "game_id")

### calculating error metrics for both my projections and betting spreads
LastWeekGames_spread <- LastWeekGames_spread |>
  mutate(abs_error = Metrics::ae(result, proj_margin),
         vegas_abs_error = Metrics::ae(result, mean_spread),
         sqd_error = Metrics::se(result, proj_margin),
         vegas_sqd_error = Metrics::se(result, mean_spread),
         straight_up_win = case_when(result >= 0 & proj_margin >= 0 ~ 1,
                                     result <= 0 & proj_margin <= 0 ~ 1,
                                     TRUE ~ 0),
         ATS_win = case_when(result > mean_spread & proj_margin > mean_spread ~ 1,
                             result < mean_spread & proj_margin < mean_spread ~ 1,
                             TRUE ~ 0),
         AE_ATS_win = case_when(abs_error < vegas_abs_error ~ 1,
                                TRUE ~ 0))

### calculating error metrics for all games, not just games with betting spreads
LastWeekGames <- LastWeekGames |>
  mutate(abs_error = Metrics::ae(result, proj_margin),
         sqd_error = Metrics::se(result, proj_margin),
         straight_up_win = case_when(result >= 0 & proj_margin >= 0 ~ 1,
                                     result <= 0 & proj_margin <= 0 ~ 1,
                                     TRUE ~ 0))

### calculating weekly average error metrics for games with spread info available
WeekMeanAccuracyMetrics_spread <- data.frame(week = as.numeric(cfb_week),
                                      mean_ae = mean(LastWeekGames_spread$abs_error),
                                      mean_vegas_ae = mean(LastWeekGames_spread$vegas_abs_error),
                                      mean_se = mean(LastWeekGames_spread$sqd_error),
                                      mean_vegas_se = mean(LastWeekGames_spread$vegas_sqd_error),
                                      RMSE = rmse(LastWeekGames_spread$result, LastWeekGames_spread$proj_margin),
                                      vegas_RMSE = rmse(LastWeekGames_spread$result, LastWeekGames_spread$mean_spread),
                                      straight_up_win_pct = sum(LastWeekGames_spread$straight_up_win) / nrow(LastWeekGames_spread),
                                      ATS_win_pct = sum(LastWeekGames_spread$ATS_win) / nrow(LastWeekGames_spread),
                                      AE_ATS_win_pct = sum(LastWeekGames_spread$AE_ATS_win) / nrow(LastWeekGames_spread))

### calculating weekly average error metrics for all games
WeekMeanAccuracyMetrics <- data.frame(week = as.numeric(cfb_week),
                                             mean_ae = mean(LastWeekGames$abs_error),
                                             mean_se = mean(LastWeekGames$sqd_error),
                                             RMSE = rmse(LastWeekGames$result, LastWeekGames$proj_margin),
                                             straight_up_win_pct = sum(LastWeekGames$straight_up_win) / nrow(LastWeekGames))


if (as.numeric(cfb_week) == 1){
  ### writing csv with individual games + accuracy metrics
  write_csv(LastWeekGames, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, cfb_week, "GameAccuracyMetrics.csv")))
  
  ### writing csv with individual games + accuracy metrics + spread
  write_csv(LastWeekGames_spread, here("Data", paste0("VoA", season), "AccuracyMetrics", "SpreadGames", paste0("VoA", season, week_text, "1", week_text, cfb_week, "SpreadGameAccuracyMetrics.csv")))
  
  ### writing csv with just weekly average calculated for accuracy metrics
  write_csv(WeekMeanAccuracyMetrics, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, cfb_week, "WeekAccuracyMetrics.csv")))
  
  ### writing csv with just weekly average calculated for accuracy metrics but only for games with spread
  write_csv(WeekMeanAccuracyMetrics, here("Data", paste0("VoA", season), "AccuracyMetrics", "SpreadGames", paste0("VoA", season, week_text, "1", week_text, cfb_week, "WeekAccuracyMetricsSpread.csv")))
} else if (as.numeric(cfb_week) >= 2){
  ### reading in csv of previous games with error calculated, binding current week's games to that 
  PrevWeekGameAccuracyMetrics <- read_csv(here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, as.character(as.numeric(cfb_week) - 1), "GameAccuracyMetrics.csv")))
  CompletedGames <- rbind(PrevWeekGameAccuracyMetrics, LastWeekGames)
  
  ### reading in csv of previous games with spread available and with error calculated, binding current week's games to that 
  PrevWeekGameAccuracyMetrics_spread <- read_csv(here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, week_text, "1", week_text, as.character(as.numeric(cfb_week) - 1), "SpreadGameAccuracyMetrics.csv")))
  CompletedGames_spread <- rbind(PrevWeekGameAccuracyMetrics_spread, LastWeekGames_spread)
  
  ### writing csv with individual games + accuracy metrics
  write_csv(CompletedGames, here("Data", paste0("VoA", season), "AccuracyMetrics", "SpreadGames", paste0("VoA", season, week_text, "1", week_text, cfb_week, "SpreadGameAccuracyMetrics.csv")))
  
  ### reading in csv of weekly average, binding current week to it
  PrevWeeklyAvgAccuracyMetrics <- read_csv(here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, "WeekAccuracyMetrics.csv")))
  CompletedWeeks <- rbind(PrevWeeklyAvgAccuracyMetrics, WeekMeanAccuracyMetrics)
  
  ### writing csv with just weekly average calculated for accuracy metrics
  write_csv(CompletedWeeks, here("Data", paste0("VoA", season), "AccuracyMetrics", paste0("VoA", season, "WeekAccuracyMetrics.csv")))
} else{
  print("week not properly entered")
}


if (as.numeric(cfb_week) >= 5){
  SeasonMetrics_spread <- CompletedGames_spread |>
    group_by(season) |>
    summarize(mean_ae = mean(abs_error),
              mean_vegas_ae = mean(vegas_abs_error),
              mean_se = mean(sqd_error),
              mean_vegas_se = mean(vegas_sqd_error),
              RMSE = rmse(result, proj_margin),
              vegas_RMSE = rmse(result, mean_spread),
              straight_up_win_pct = sum(straight_up_win) / nrow(CompletedGames_spread),
              ATS_win_pct = sum(ATS_win) / nrow(CompletedGames_spread),
              AE_ATS_win_pct = sum(AE_ATS_win) / nrow(CompletedGames_spread))
  
  SeasonMetrics <- CompletedGames |>
    group_by(season) |>
    summarize(mean_ae = mean(abs_error),
              mean_se = mean(sqd_error),
              RMSE = rmse(result, proj_margin),
              straight_up_win_pct = sum(straight_up_win) / nrow(CompletedGames_spread))
}
