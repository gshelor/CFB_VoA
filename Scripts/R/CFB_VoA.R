##### The Vortex of Accuracy, Version 5.1.0 #####
### Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
### Created by Griffin Shelor
### installing packages
# install.packages(c("devtools", "tidyverse", "gt", "viridis", "webshot", "cfbfastR", "here", "RColorBrewer", "remotes", "pacman", "gtExtras", "cfbplotR", "betareg", cmdstanr))
##### Loading Packages #####
start_time <- Sys.time()
library(pacman)
p_load(tidyverse, gt, cfbfastR, here, RColorBrewer, gtExtras, cfbplotR, ggpubr, webshot2, cmdstanr, parallel, fastDummies, glmnet, posterior)
## used to use these packages
# viridis, and also rstan since I'm switching to cmdstanr

### Creating Week and Year String for Top 25 Table Title, eventually could be used as part of reading in cfbfastR/cfbdata API data
year <- readline(prompt = "What year is it? ")
week <- readline(prompt = "What week just occurred? ")

##### setting strings for table titles, file pathways, unintelligible charts #####
`%nin%` = Negate(`%in%`)
output_dir <- here("Outputs", "RVoA", paste0("VoA", year))
data_dir <- here("Data", paste0("VoA", year))
tracking_chart_dir <- here("Data", paste0("VoA", year), "TrackingChartCSVs")
accuracy_data_dir <- here("Data", paste0("VoA", year), "AccuracyMetrics")
PY_data_dir <- here("Data", paste0("VoA", year), "PYData")
Projection_data_dir <- here("Data", paste0("VoA", year), "Projections")
preseason_text <- "Preseason"
resume_text <- "Resume"
VoAString <- "VoA.csv"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "VoATop25.png"
fulltable_png <- "VoAFullTable.png"
VoA_text <- "Vortex of Accuracy"
Postseason_text <- "Postseason"
AAC_text <- "AAC"
ACC_text <- "ACC"
Big12_text <- "Big12"
Big10_text <- "Big10"
CUSA_text <- "CUSA"
Indy_text <- "Independents"
MAC_text <- "MAC"
MWC_text <- "MWC"
Pac2_text <- "Pac2"
SEC_text <- "SEC"
SunBelt_text <- "SunBelt"
FBS_text <- "FBS"
Power_Five_text <- "Power 5"
Group_Five_text <- "Group of 5"
Rating_text <- "_Ratings_Chart.png"
Ranking_text <- "_Rankings_Chart.png"
Histogram_text <- "_RatingHist.png"
Output_Rating_Plot_text <- "VoA Outputs vs VoA Ratings"
Output_Rating_Plot_png <- "Output_Rating.png"

FBS_hist_title <- paste(year, week_text, week, FBS_text, VoA_text, "Ratings")
Power5_hist_title <- paste(year, week_text, week, Power_Five_text, VoA_text, "Ratings")
Group5_hist_title <- paste(year, week_text, week, Group_Five_text, VoA_text, "Ratings")
Output_Rating_Plot_title <- paste(year, week_text, week, Output_Rating_Plot_text)
top25_file_pathway <- paste(year,week_text,week,"_",top25_png, sep = "")
resumetop25_file_pathway <- paste(year,week_text,week,resume_text,"_",top25_png, sep = "")
fulltable_file_pathway <- paste(year,week_text,week,"_",fulltable_png, sep = "")
resumefulltable_file_pathway <- paste(year,week_text,week,resume_text,"_",fulltable_png, sep = "")
AAC_Output_filename <- paste(year,week_text, week, AAC_text, Rating_text, sep = "")
AAC_Ranking_filename <- paste(year,week_text, week, AAC_text, Ranking_text, sep = "")
ACC_Output_filename <- paste(year,week_text, week, ACC_text, Rating_text, sep = "")
ACC_Ranking_filename <- paste(year,week_text, week, ACC_text, Ranking_text, sep = "")
Big12_Output_filename <- paste(year,week_text, week, Big12_text, Rating_text, sep = "")
Big12_Ranking_filename <- paste(year,week_text, week, Big12_text, Ranking_text, sep = "")
Big10_Output_filename <- paste(year,week_text, week, Big10_text, Rating_text, sep = "")
Big10_Ranking_filename <- paste(year,week_text, week, Big10_text, Ranking_text, sep = "")
CUSA_Output_filename <- paste(year,week_text, week, CUSA_text, Rating_text, sep = "")
CUSA_Ranking_filename <- paste(year,week_text, week, CUSA_text, Ranking_text, sep = "")
Indy_Output_filename <- paste(year,week_text, week, Indy_text, Rating_text, sep = "")
Indy_Ranking_filename <- paste(year,week_text, week, Indy_text, Ranking_text, sep = "")
MAC_Output_filename <- paste(year,week_text, week, MAC_text, Rating_text, sep = "")
MAC_Ranking_filename <- paste(year,week_text, week, MAC_text, Ranking_text, sep = "")
MWC_Output_filename <- paste(year,week_text, week, MWC_text, Rating_text, sep = "")
MWC_Ranking_filename <- paste(year,week_text, week, MWC_text, Ranking_text, sep = "")
Pac2_Output_filename <- paste(year,week_text, week, Pac2_text, Rating_text, sep = "")
Pac2_Ranking_filename <- paste(year,week_text, week, Pac2_text, Ranking_text, sep = "")
SEC_Output_filename <- paste(year,week_text, week, SEC_text, Rating_text, sep = "")
SEC_Ranking_filename <- paste(year,week_text, week, SEC_text, Ranking_text, sep = "")
SunBelt_Output_filename <- paste(year,week_text, week, SunBelt_text, Rating_text, sep = "")
SunBelt_Ranking_filename <- paste(year,week_text, week, SunBelt_text, Ranking_text, sep = "")
FBS_hist_filename <- paste(year, week_text, week, "_", FBS_text, Histogram_text, sep = "")
Power5_hist_filename <- paste(year, week_text, week, "_", Power_Five_text, Histogram_text, sep = "")
Group5_hist_filename <- paste(year, week_text, week, "_", Group_Five_text, Histogram_text, sep = "")
Output_Rating_Plot_filename <- paste(year, week_text, week, "_", Output_Rating_Plot_png, sep = "")
### creating string for csv spreadsheet pathway
file_pathway <- paste0(data_dir, "/", year, week_text, week,"_", VoAString)
### creating directories that don't exist
for (i in c(data_dir, output_dir, tracking_chart_dir, Projection_data_dir, PY_data_dir, accuracy_data_dir)){
  if (dir.exists(i) == FALSE){
    dir.create(i, recursive = TRUE)
  }
}
### setting number of cores to use for mcmc chains later
options(mc.cores = parallel::detectCores() / 2)

##### Reading in Data #####
### pulling in data based on week of the season
if (as.numeric(week) == 0) {
  ##### WEEK 0 Data Pull #####
  ### storing names of FCS teams for when they need to be filtered out in PY stats grabs
  PY3Teams <- c("Delaware", "Missouri State", "Kennesaw State", "Sam Houston State", "Jacksonville State", "Sam Houston")
  PY2Teams <- c("Delaware", "Missouri State", "Kennesaw State")
  PY1Teams <- c("Delaware", "Missouri State")
  ### reading in data for 3 previous years
  ### reading in FCS data first, made with FCSCleanup.R
  FCS_PY3 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY3.csv"))
  FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))
  
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedFBSGames_PY3 <- cfbd_game_info(as.numeric(year) - 3) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" & away_classification == "fbs" | home_team %in% PY3Teams | away_team %in% PY3Teams)
  CompletedNeutralGames_PY3 <- CompletedFBSGames_PY3 |>
    filter(neutral_site == TRUE)
  ### PY2 completed games
  CompletedFBSGames_PY2 <- cfbd_game_info(as.numeric(year) - 2) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" & away_classification == "fbs" | home_team %in% PY2Teams | away_team %in% PY2Teams)
  CompletedNeutralGames_PY2 <- CompletedFBSGames_PY2 |>
    filter(neutral_site == TRUE)
  ### PY1 completed games
  CompletedFBSGames_PY1 <- cfbd_game_info(as.numeric(year) - 1) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" & away_classification == "fbs" | home_team %in% PY1Teams | away_team %in% PY1Teams)
  CompletedNeutralGames_PY1 <- CompletedFBSGames_PY1 |>
    filter(neutral_site == TRUE)
  
  ### loading in play-by-play data
  PBP_PY3 <- load_cfb_pbp(seasons = as.numeric(year) - 3)
  PBP_PY2 <- load_cfb_pbp(seasons = as.numeric(year) - 2)
  PBP_PY1 <- load_cfb_pbp(seasons = as.numeric(year) - 1)
  
  ### pulling out relevant plays used to create/input variables later
  ## PY3
  PBP_PY3_Yards <- PBP_PY3 |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames_PY3$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_PY3_3rdDowns <- PBP_PY3_Yards |>
    filter(down == 3)
  
  PBP_PY3_4thDowns <- PBP_PY3_Yards |>
    filter(down == 4)
  
  PBP_PY3_TDs <- PBP_PY3 |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_PY3_2PtConvs <- PBP_PY3 |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_PY3_2ptPlays <- PBP_PY3_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_PY3_2ptPlays <- rbind(PBP_PY3_2ptPlays, PBP_PY3_2PtConvs)
  
  PBP_PY3_FGPlays <- PBP_PY3 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_PY3_XPPlays <- PBP_PY3_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_PY3_ReturnTDs <- PBP_PY3 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PY3_PuntReturnTD <- PBP_PY3 |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY3_KickReturn <- PBP_PY3 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_PY3_Punts <- PBP_PY3 |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment_PY3 <- PBP_PY3_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment_PY3 <- PBP_PY3_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment_PY3 <- PBP_PY3_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment_PY3 <- PBP_PY3_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY3 <- rbind(PBP_PY3_FGPlays, PBP_PY3_XPPlays, PBP_PY3_KickReturn, PBP_PY3_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames_PY3$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment_PY3 <- PBP_STPlays_PY3 |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### PY2
  PBP_PY2_Yards <- PBP_PY2 |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames_PY2$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_PY2_3rdDowns <- PBP_PY2_Yards |>
    filter(down == 3)
  
  PBP_PY2_4thDowns <- PBP_PY2_Yards |>
    filter(down == 4)
  
  PBP_PY2_TDs <- PBP_PY2 |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_PY2_2PtConvs <- PBP_PY2 |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_PY2_2ptPlays <- PBP_PY2_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_PY2_2ptPlays <- rbind(PBP_PY2_2ptPlays, PBP_PY2_2PtConvs)
  
  PBP_PY2_FGPlays <- PBP_PY2 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_PY2_XPPlays <- PBP_PY2_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_PY2_ReturnTDs <- PBP_PY2 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PY2_PuntReturnTD <- PBP_PY2 |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY2_KickReturn <- PBP_PY2 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_PY2_Punts <- PBP_PY2 |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment_PY2 <- PBP_PY2_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment_PY2 <- PBP_PY2_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment_PY2 <- PBP_PY2_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment_PY2 <- PBP_PY2_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY2 <- rbind(PBP_PY2_FGPlays, PBP_PY2_XPPlays, PBP_PY2_KickReturn, PBP_PY2_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames_PY2$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment_PY2 <- PBP_STPlays_PY2 |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ## PY1
  PBP_PY1_Yards <- PBP_PY1 |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames_PY1$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_PY1_3rdDowns <- PBP_PY1_Yards |>
    filter(down == 3)
  
  PBP_PY1_4thDowns <- PBP_PY1_Yards |>
    filter(down == 4)
  
  PBP_PY1_TDs <- PBP_PY1 |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_PY1_2PtConvs <- PBP_PY1 |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_PY1_2ptPlays <- PBP_PY1_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_PY1_2ptPlays <- rbind(PBP_PY1_2ptPlays, PBP_PY1_2PtConvs)
  
  PBP_PY1_FGPlays <- PBP_PY1 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_PY1_XPPlays <- PBP_PY1_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_PY1_ReturnTDs <- PBP_PY1 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PY1_PuntReturnTD <- PBP_PY1 |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY1_KickReturn <- PBP_PY1 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_PY1_Punts <- PBP_PY1 |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment_PY1 <- PBP_PY1_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment_PY1 <- PBP_PY1_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment_PY1 <- PBP_PY1_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment_PY1 <- PBP_PY1_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY1 <- rbind(PBP_PY1_FGPlays, PBP_PY1_XPPlays, PBP_PY1_KickReturn, PBP_PY1_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames_PY1$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment_PY1 <- PBP_STPlays_PY1 |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### reading in regular stats
  Stats_PY1 <- cfbd_stats_season_team(year = as.integer(year) - 1, season_type = "both", start_week = 1, end_week = 15) |>
    filter(team %nin% PY1Teams) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds / games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  ## removing NAs
  Stats_PY1[is.na(Stats_PY1)] = 0
  ## PY2 stats
  Stats_PY2 <- cfbd_stats_season_team(year = as.integer(year) - 2, season_type = "both", start_week = 1, end_week = 15) |>
    filter(team %nin% PY2Teams) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  ## removing NAs
  Stats_PY2[is.na(Stats_PY2)] = 0
  ## PY3 stats
  Stats_PY3 <- cfbd_stats_season_team(year = as.integer(year) - 3, season_type = "both", start_week = 1, end_week = 15) |>
    filter(team %nin% PY3Teams) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds/games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  ## removing NAs
  Stats_PY3[is.na(Stats_PY3)] = 0
  
  ## advanced stats data
  Adv_Stats_PY1 <- cfbd_stats_season_advanced(year = as.integer(year) - 1, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team %nin% PY1Teams) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY1[is.na(Adv_Stats_PY1)] = 0
  ## PY2 advanced stats
  Adv_Stats_PY2 <- cfbd_stats_season_advanced(year = as.integer(year) - 2, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team %nin% PY2Teams) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY2[is.na(Adv_Stats_PY2)] = 0
  ## PY3 advanced stats
  Adv_Stats_PY3 <- cfbd_stats_season_advanced(year = as.integer(year) - 3, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
    filter(team %nin% PY3Teams) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats_PY3[is.na(Adv_Stats_PY3)] = 0
  
  ### pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    # filter(team != "Kennesaw State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, points)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  ### converting recruiting points to numeric type
  recruit_PY1$recruit_pts_PY1 <- as.numeric(recruit_PY1$recruit_pts_PY1)
  
  ### pulling in talent rankings
  talent_df_PY1 <- cfbd_team_talent(year = as.numeric(year) - 1) |>
    # filter(school != "Kennesaw State") |>
    filter(team %in% Stats_PY1$team) |>
    select(team, talent)
  colnames(talent_df_PY1) <- c("team", "talent_PY1")
  
  ### pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    # filter(team != "Kennesaw State" & team != "Sam Houston State" & team != "Jacksonville State") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, points)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  ### converting recruiting points to numeric type
  recruit_PY2$recruit_pts_PY2 <- as.numeric(recruit_PY2$recruit_pts_PY2)
  
  ## pulling in talent rankings
  talent_df_PY2 <- cfbd_team_talent(year = as.numeric(year) - 2) |>
    # filter(school != "Kennesaw State" & school != "Sam Houston State" & school != "Jacksonville State") |>
    filter(team %in% Stats_PY2$team) |>
    select(team, talent)
  colnames(talent_df_PY2) <- c("team", "talent_PY2")
  
  ### pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    # filter(team != "James Madison" & team != "Sam Houston State" & team != "Jacksonville State" & team != "Kennesaw State") |>
    filter(team %in% Stats_PY3$team) |>
    select(team, points)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
  ### converting recruiting points to numeric type
  recruit_PY3$recruit_pts_PY3 <- as.numeric(recruit_PY3$recruit_pts_PY3)
  
  ## pulling in talent rankings
  talent_df_PY3 <- cfbd_team_talent(year = as.numeric(year) - 3) |>
    # filter(school != "James Madison" & school != "Sam Houston State" & school != "Jacksonville State" & school != "Kennesaw State") |>
    filter(team %in% Stats_PY3$team) |>
    select(team, talent)
  colnames(talent_df_PY3) <- c("team", "talent_PY3")
  
  ## incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats_PY1$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
} else if (as.numeric(week) == 1) {
  ##### WEEK 1 Data Pull #####
  ### reading in data for 3 previous years
  ### no need to remove season and conference columns from PY3_df because they are removed before I write the csv in week 0
  PY3_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY3.csv"))
  PY2_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))
  
  ### TEMPORARY 2024 WEEK 1 FIX SINCE BALL STATE DID NOT PLAY A GAME IN WEEK 0 OR 1 and also CMU and ULM are having data issues
  # BallStCMUULM <- PY1_df |>
  #   filter(team == "Ball State" | team == "Central Michigan" | team == "Louisiana Monroe") |>
  #   mutate(season = as.numeric(year), .before = 1) |>
  #   mutate(conference = case_when(team == "Ball State" | team == "Central Michigan" ~ "Mid-American",
  #                                 TRUE ~ "Sun Belt"), .before = 3)
  # colnames(BallStCMUULM) <- c("season", "team", "conference", "games", "completion_pct", "pass_ypa", "pass_ypr", "int_pct", "rush_ypc", "turnovers_pg", "third_conv_rate", "fourth_conv_rate", "penalty_yds_pg", "yards_per_penalty", "kick_return_avg", "punt_return_avg", "total_yds_pg", "pass_yds_pg", "rush_yds_pg", "first_downs_pg", "off_ypp", "def_interceptions_pg", "off_plays_pg", "off_ppg", "def_ppg", "def_yds_pg", "def_plays_pg", "def_third_conv_rate", "def_fourth_conv_rate", "def_ypp", "fg_rate", "fg_rate_allowed", "fg_made_pg", "fg_made_pg_allowed", "xpts_pg", "xpts_allowed_pg", "kick_return_yds_avg_allowed", "punt_return_yds_avg_allowed", "st_ppg", "st_ppg_allowed", "oppdef_ppa", "oppoff_ppa", "off_ppa", "off_success_rate", "off_explosiveness", "off_power_success", "off_stuff_rate", "off_line_yds", "off_second_lvl_yds", "off_open_field_yds", "off_pts_per_opp", "off_field_pos_avg_predicted_points", "off_havoc_total", "off_havoc_front_seven", "off_havoc_db", "off_standard_downs_ppa", "off_standard_downs_success_rate", "off_standard_downs_explosiveness", "off_passing_downs_ppa", "off_passing_downs_success_rate", "off_passing_downs_explosiveness", "off_rushing_plays_ppa", "off_rushing_plays_success_rate", "off_rushing_plays_explosiveness", "off_passing_plays_ppa", "off_passing_plays_success_rate", "off_passing_plays_explosiveness", "def_ppa", "def_success_rate", "def_explosiveness", "def_power_success", "def_stuff_rate", "def_line_yds", "def_second_lvl_yds", "def_open_field_yds", "def_pts_per_opp", "def_field_pos_avg_predicted_points", "def_havoc_total", "def_havoc_front_seven", "def_havoc_db", "def_standard_downs_ppa", "def_standard_downs_success_rate", "def_standard_downs_explosiveness", "def_passing_downs_ppa", "def_passing_downs_success_rate", "def_passing_downs_explosiveness", "def_rushing_plays_ppa", "def_rushing_plays_success_rate", "def_rushing_plays_explosiveness", "def_passing_plays_ppa", "def_passing_plays_success_rate", "def_passing_plays_explosiveness", "recruit_pts", "talent")
  # BallStCMUULM <- BallStCMUULM |>
  #   select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness, recruit_pts)
  
  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))
  
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedFBSGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" | away_classification == "fbs")
  CompletedNeutralGames <- CompletedFBSGames |>
    filter(neutral_site == TRUE)
  
  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))
  
  ### pulling out relevant plays used to create/input variables later
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)
  
  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)
  
  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_2PtConvs <- PBP |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_2ptPlays <- PBP_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)
  
  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PuntReturnTD <- PBP_ReturnTDs |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment <- PBP_STPlays |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds / games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0) #|>
    ### temp 2024 week 1 fix
    # filter(team != "Ball State" & team != "Central Michigan" & team != "Louisiana Monroe")
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  Adv_Stats[is.na(Adv_Stats)] = 0
  
  ### incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points) #|>
    ### temp 2024 week 1 fix
    # filter(school != "Ball State" & school != "Central Michigan" & school != "Louisiana Monroe")
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
} else if (as.numeric(week) <= 5) {
  ##### WEEKS 2-5 DATA PULL #####
  ### reading in Previous year's data as csvs so I don't have to read it in again
  PY2_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))
  
  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))
  
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedFBSGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" | away_classification == "fbs")
  CompletedNeutralGames <- CompletedFBSGames |>
    filter(neutral_site == TRUE)
  
  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))
  
  ### pulling out relevant plays used to create/input variables later
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)
  
  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)
  
  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_2PtConvs <- PBP |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_2ptPlays <- PBP_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)
  
  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment <- PBP_STPlays |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds / games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  Stats[is.na(Stats)] = 0
  
  ## advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  Adv_Stats[is.na(Adv_Stats)] = 0

  ### incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
} else if (as.numeric(week) <= 8) {
  ##### WEEKS 6-8 Data Pull #####
  ### reading in Previous year's data as csvs so I don't have to read it in again
  PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))
  
  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))
  
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedFBSGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" | away_classification == "fbs")
  CompletedNeutralGames <- CompletedFBSGames |>
    filter(neutral_site == TRUE)
  
  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))
  
  ### pulling out relevant plays used to create/input variables later
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)
  
  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)
  
  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_2PtConvs <- PBP |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_2ptPlays <- PBP_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)
  
  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment <- PBP_STPlays |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### CURRENT SEASON STATS
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds / games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  ### removing NAs
  Stats[is.na(Stats)] = 0
  
  ### advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness , def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats[is.na(Adv_Stats)] = 0
  
  ### incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points) |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
} else {
  ##### CURRENT SEASON STATS ONLY Data Pull #####
  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedFBSGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_classification == "fbs" | away_classification == "fbs")
  CompletedNeutralGames <- CompletedFBSGames |>
    filter(neutral_site == TRUE)
  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))
  
  ### pulling out relevant plays used to create/input variables later
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))
  
  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)
  
  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)
  
  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")
  
  PBP_2PtConvs <- PBP |>
    filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")
  
  PBP_2ptPlays <- PBP_TDs |>
    filter(pos_score_pts == 8)
  
  PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)
  
  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")
  
  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)
  
  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")
  
  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")
  
  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Kickoff Return (Offense)" | play_type == "Kickoff")
  
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")
  
  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  PBP_PPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(play_pts_scored = case_when(offense_score_play == 1 & rush_td == 1 ~ 6,
                                       offense_score_play == 1 & pass_td == 1 ~ 6,
                                       TRUE ~ 0),
           hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                           TRUE ~ "Home"))
  
  PBP_STPPA_Adjustment <- PBP_STPlays |>
    select(game_id, home, real_pos_team, real_def_pos_team, ppa, home_neutral) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     real_pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()
  
  ### regular stats
  Stats <- cfbd_stats_season_team(year = as.integer(year), start_week = 1, end_week = as.numeric(week)) |>
    mutate(total_yds_pg = total_yds/games,
           pass_yds_pg = net_pass_yds / games,
           rush_yds_pg = rush_yds/games,
           first_downs_pg = first_downs/games,
           def_interceptions_pg = passes_intercepted/games,
           pass_ypa = net_pass_yds / pass_atts,
           off_ypp = total_yds / (rush_atts + pass_atts),
           completion_pct = pass_comps / pass_atts,
           pass_ypr = net_pass_yds / pass_comps,
           int_pct = interceptions / pass_atts,
           rush_ypc = rush_yds / rush_atts,
           turnovers_pg = turnovers / games,
           third_conv_rate = third_down_convs / third_downs,
           fourth_conv_rate = fourth_down_convs / fourth_downs,
           penalty_yds_pg = penalty_yds / games,
           yards_per_penalty = penalty_yds / penalties,
           kick_return_avg = kick_return_yds / kick_returns,
           punt_return_avg = punt_return_yds / punt_returns,
           ### adding columns which will be filled in later using pbp data
           off_plays_pg = 0,
           off_ppg = 0,
           def_ppg = 0,
           def_yds_pg = 0,
           def_plays_pg = 0,
           def_third_conv_rate = 0,
           def_fourth_conv_rate = 0,
           def_ypp = 0,
           st_ppa = 0,
           st_ppa_allowed = 0,
           fg_rate = 0,
           fg_rate_allowed = 0,
           fg_made_pg = 0,
           fg_made_pg_allowed = 0,
           xpts_pg = 0,
           xpts_allowed_pg = 0,
           kick_return_yds_avg_allowed = 0,
           punt_return_yds_avg_allowed = 0,
           st_ppg = 0,
           st_ppg_allowed = 0,
           oppdef_ppa = 0,
           oppoff_ppa = 0)
  ### removing NAs
  Stats[is.na(Stats)] = 0
  
  ### advanced stats data
  Adv_Stats <- cfbd_stats_season_advanced(year = as.integer(year), excl_garbage_time = FALSE, start_week = 1, end_week = as.numeric(week)) |>
    select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
           off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
           off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, 
           off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
           off_standard_downs_success_rate, off_standard_downs_explosiveness,
           off_passing_downs_ppa, off_passing_downs_success_rate,
           off_passing_downs_explosiveness, off_rushing_plays_ppa,
           off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
           off_passing_plays_ppa, off_passing_plays_success_rate,
           off_passing_plays_explosiveness, def_ppa, def_success_rate,
           def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
           def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, 
           def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
           def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
           def_standard_downs_explosiveness, def_passing_downs_ppa,
           def_passing_downs_success_rate, def_passing_downs_explosiveness,
           def_rushing_plays_ppa, def_rushing_plays_success_rate,
           def_rushing_plays_explosiveness, def_passing_plays_ppa,
           def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## removing NAs
  Adv_Stats[is.na(Adv_Stats)] = 0
  
  ### incoming recruiting class rankings
  recruit <- cfbd_recruiting_team(year = as.numeric(year)) |>
    select(team, points)
  recruit <- recruit |>
    mutate(school = case_when(team == "Florida Intl" ~ "Florida International",
                              TRUE ~ team)) |>
    filter(school %in% Stats$team) |>
    select(school, points)
  recruit[,2] <- recruit[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit) <- c("team", "recruit_pts")
  
  ## pulling in recruiting rankings
  recruit_PY1 <- cfbd_recruiting_team(year = as.numeric(year) - 1) |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY1[,2] <- recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY1) <- c("team", "recruit_pts_PY1")
  
  ## pulling in recruiting rankings
  recruit_PY2 <- cfbd_recruiting_team(year = as.numeric(year) - 2) |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY2[,2] <- recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY2) <- c("team", "recruit_pts_PY2")
  
  ## pulling in recruiting rankings
  recruit_PY3 <- cfbd_recruiting_team(year = as.numeric(year) - 3) |>
    filter(team %in% Stats$team) |>
    select(team, points)
  recruit_PY3[,2] <- recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
  colnames(recruit_PY3) <- c("team", "recruit_pts_PY3")
}


##### merging data frames together #####
if (as.numeric(week) == 0) {
  ##### WEEK 0 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  ## then I will be merging data frames for the same year together
  ## then I will merge years together by team
  PY3_stats_adv_stats_list <- list(Stats_PY3, Adv_Stats_PY3)
  PY3_stats_adv_stats_merge <- PY3_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  
  colnames(PY3_stats_adv_stats_merge) <- c("season", "team", "conference", "games_PY3", "completion_pct_PY3", "pass_ypa_PY3", "pass_ypr_PY3", "int_pct_PY3", "rush_ypc_PY3", "turnovers_pg_PY3", "third_conv_rate_PY3", "fourth_conv_rate_PY3", "penalty_yds_pg_PY3", "yards_per_penalty_PY3", "kick_return_avg_PY3", "punt_return_avg_PY3", "total_yds_pg_PY3", "pass_yds_pg_PY3", "rush_yds_pg_PY3", "first_downs_pg_PY3", "off_ypp_PY3", "def_interceptions_pg_PY3", "off_plays_pg_PY3", "off_ppg_PY3", "def_ppg_PY3", "def_yds_pg_PY3", "def_plays_pg_PY3", "def_third_conv_rate_PY3", "def_fourth_conv_rate_PY3", "def_ypp_PY3", "fg_rate_PY3", "fg_rate_allowed_PY3", "fg_made_pg_PY3", "fg_made_pg_allowed_PY3", "xpts_pg_PY3", "xpts_allowed_pg_PY3", "kick_return_yds_avg_allowed_PY3", "punt_return_yds_avg_allowed_PY3", "st_ppg_PY3", "st_ppg_allowed_PY3", "oppdef_ppa_PY3", "oppoff_ppa_PY3", "off_ppa_PY3", "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3", "off_stuff_rate_PY3", "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3", "off_pts_per_opp_PY3", "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", "off_havoc_front_seven_PY3", "off_havoc_db_PY3", "off_standard_downs_ppa_PY3", "off_standard_downs_success_rate_PY3", "off_standard_downs_explosiveness_PY3", "off_passing_downs_ppa_PY3", "off_passing_downs_success_rate_PY3", "off_passing_downs_explosiveness_PY3", "off_rushing_plays_ppa_PY3", "off_rushing_plays_success_rate_PY3", "off_rushing_plays_explosiveness_PY3", "off_passing_plays_ppa_PY3", "off_passing_plays_success_rate_PY3", "off_passing_plays_explosiveness_PY3", "def_ppa_PY3", "def_success_rate_PY3", "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3", "def_line_yds_PY3", "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3", "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3", "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3", "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3", "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3", "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3", "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3", "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")
  
  ### making list of dfs to be merged
  PY3_df_list <- list(PY3_stats_adv_stats_merge, recruit_PY3, talent_df_PY3)
  ### merging dfs
  PY3_df <- PY3_df_list |>
    reduce(full_join, by = "team")
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main PY3_df
  
  ### list of relevant PBP dfs
  # PBP_PY3_Yards
  # PBP_PY3_3rdDowns
  # PBP_PY3_4thDowns
  # PBP_PY3_TDs
  # PBP_PY3_2ptPlays
  # PBP_PY3_FGPlays
  # PBP_PY3_XPPlays
  ### on ReturnTD plays, pos_team does the scoring, except on punt return TDs
  # PBP_PY3_ReturnTDs
  # PBP_PY3_PuntReturnTD
  ### on KickReturnPlays, pos_team gains yards/does the returning
  # PBP_PY3_KickReturn
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY3_Punts
  for (school in 1:nrow(PY3_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_PY3_yards <- PBP_PY3_Yards |>
      filter(pos_team == PY3_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_PY3_Defyards <- PBP_PY3_Yards |>
      filter(def_pos_team == PY3_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_PY3_3rd <- PBP_PY3_3rdDowns |>
      filter(def_pos_team == PY3_df$team[school])
    temp_PBP_PY3_4th <- PBP_PY3_4thDowns |>
      filter(def_pos_team == PY3_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_PY3_OffTDs <- PBP_PY3_TDs |>
      filter(pos_team == PY3_df$team[school])
    ### going to use this to offset the off_ppg and def_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_PY3_Offplays <- PBP_PY3_Yards |>
      filter(pos_team == PY3_df$team[school])
    temp_PBP_PY3_Defplays <- PBP_PY3_Yards |>
      filter(def_pos_team == PY3_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_PY3_OppDefPPA <- PBP_PY3_Yards |>
      filter(def_pos_team %in% temp_PBP_PY3_Offplays$def_pos_team)
    temp_PBP_PY3_OppOffPPA <- PBP_PY3_Yards |>
      filter(pos_team %in% temp_PBP_PY3_Defplays$pos_team)
    temp_PBP_PY3_DefTDs <- PBP_PY3_TDs |>
      filter(def_pos_team == PY3_df$team[school])
    temp_PBP_PY3_2Pts <- PBP_PY3_2ptPlays |>
      filter(pos_team == PY3_df$team[school])
    temp_PBP_PY3_Def2Pts <- PBP_PY3_2ptPlays |>
      filter(def_pos_team == PY3_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_PY3_FGs <- PBP_PY3_FGPlays |>
      filter(pos_team == PY3_df$team[school])
    temp_PBP_PY3_GoodFGs <- temp_PBP_PY3_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY3_DefFGs <- PBP_PY3_FGPlays |>
      filter(def_pos_team == PY3_df$team[school])
    temp_PBP_PY3_DefGoodFGs <- temp_PBP_PY3_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY3_XPts <- PBP_PY3_XPPlays |>
      filter(pos_team == PY3_df$team[school])
    temp_PBP_PY3_DefXPts <- PBP_PY3_XPPlays |>
      filter(def_pos_team == PY3_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_PY3_KickReturn <- PBP_PY3_KickReturn |>
      filter(def_pos_team == PY3_df$team[school])
    temp_PBP_PY3_PuntReturn <- PBP_PY3_Punts |>
      filter(pos_team == PY3_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_PY3_ReturnTDs <- PBP_PY3_ReturnTDs |>
      filter(def_pos_team == PY3_df$team[school])
    temp_PBP_PY3_OffReturnTDs <- PBP_PY3_ReturnTDs |>
      filter(pos_team == PY3_df$team[school])
    temp_PBP_PY3_PuntTDs <- PBP_PY3_PuntReturnTD |>
      filter(pos_team == PY3_df$team[school])
    
    ### using filtered play datasets to calculate variables
    PY3_df$off_plays_pg_PY3[school] = nrow(temp_PBP_PY3_yards) / PY3_df$games_PY3[school]
    PY3_df$off_ppg_PY3[school] = ((nrow(temp_PBP_PY3_OffTDs) * 6) + (nrow(temp_PBP_PY3_2Pts) * 2)) / PY3_df$games_PY3[school]
    PY3_df$def_ppg_PY3[school] = ((nrow(temp_PBP_PY3_DefTDs) * 6) + (nrow(temp_PBP_PY3_Def2Pts) * 2)) / PY3_df$games_PY3[school]
    PY3_df$def_yds_pg_PY3[school] = sum(temp_PBP_PY3_Defyards$yards_gained, na.rm = TRUE) / PY3_df$games_PY3[school]
    PY3_df$def_plays_pg_PY3[school] = nrow(temp_PBP_PY3_Defyards) / PY3_df$games_PY3[school]
    PY3_df$def_third_conv_rate_PY3[school] = sum(temp_PBP_PY3_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY3_3rd)
    PY3_df$def_fourth_conv_rate_PY3[school] = sum(temp_PBP_PY3_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY3_4th)
    PY3_df$def_ypp_PY3[school] = sum(temp_PBP_PY3_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY3_Defyards)
    PY3_df$fg_rate_PY3[school] = nrow(temp_PBP_PY3_GoodFGs) / nrow(temp_PBP_PY3_FGs)
    PY3_df$fg_rate_allowed_PY3[school] = nrow(temp_PBP_PY3_DefGoodFGs) / nrow(temp_PBP_PY3_DefFGs)
    PY3_df$fg_made_pg_PY3[school] = nrow(temp_PBP_PY3_GoodFGs) / PY3_df$games_PY3[school]
    PY3_df$fg_made_pg_allowed_PY3[school] = nrow(temp_PBP_PY3_DefGoodFGs) / PY3_df$games_PY3[school]
    PY3_df$xpts_pg_PY3[school] = nrow(temp_PBP_PY3_XPts) / PY3_df$games_PY3[school]
    PY3_df$xpts_allowed_pg_PY3[school] = nrow(temp_PBP_PY3_DefXPts) / PY3_df$games_PY3[school]
    PY3_df$kick_return_yds_avg_allowed_PY3[school] = sum(temp_PBP_PY3_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY3_KickReturn)
    PY3_df$punt_return_yds_avg_allowed_PY3[school] = sum(temp_PBP_PY3_PuntReturn$yards_gained, na.rm = TRUE)
    PY3_df$st_ppg_PY3[school] = (nrow(temp_PBP_PY3_OffReturnTDs) * 6 / PY3_df$games_PY3[school]) + (nrow(temp_PBP_PY3_XPts) / PY3_df$games_PY3[school]) + (nrow(temp_PBP_PY3_GoodFGs) / PY3_df$games_PY3[school] * 3) 
    PY3_df$st_ppg_allowed_PY3[school] = (nrow(temp_PBP_PY3_ReturnTDs) * 6 / PY3_df$games_PY3[school]) + (nrow(temp_PBP_PY3_DefXPts) / PY3_df$games_PY3[school]) + (nrow(temp_PBP_PY3_DefGoodFGs) / PY3_df$games_PY3[school] * 3)
    PY3_df$oppdef_ppa_PY3[school] <- mean(temp_PBP_PY3_OppDefPPA$ppa, na.rm = TRUE)
    PY3_df$oppoff_ppa_PY3[school] <- mean(temp_PBP_PY3_OppOffPPA$ppa, na.rm = TRUE)
  }
  
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_PY3_yards, temp_PBP_PY3_Defyards, temp_PBP_PY3_3rd, temp_PBP_PY3_4th, temp_PBP_PY3_OffTDs, temp_PBP_PY3_DefTDs, temp_PBP_PY3_2Pts, temp_PBP_PY3_Def2Pts, temp_PBP_PY3_FGs, temp_PBP_PY3_GoodFGs, temp_PBP_PY3_DefFGs, temp_PBP_PY3_DefGoodFGs, temp_PBP_PY3_XPts, temp_PBP_PY3_DefXPts, temp_PBP_PY3_KickReturn, temp_PBP_PY3_PuntReturn, temp_PBP_PY3_ReturnTDs, temp_PBP_PY3_OffReturnTDs, temp_PBP_PY3_PuntTDs)
  
  ### binding FCS_PY3 df with merged stats 
  PY3_df <- rbind(PY3_df, FCS_PY3)
  
  
  ### Merging PY2 data
  PY2_stats_adv_stats_list <- list(Stats_PY2, Adv_Stats_PY2)
  PY2_stats_adv_stats_merge <- PY2_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  
  colnames(PY2_stats_adv_stats_merge) <- c("team", "games_PY2", "completion_pct_PY2", "pass_ypa_PY2", "pass_ypr_PY2", "int_pct_PY2", "rush_ypc_PY2", "turnovers_pg_PY2", "third_conv_rate_PY2", "fourth_conv_rate_PY2", "penalty_yds_pg_PY2", "yards_per_penalty_PY2", "kick_return_avg_PY2", "punt_return_avg_PY2", "total_yds_pg_PY2", "pass_yds_pg_PY2", "rush_yds_pg_PY2", "first_downs_pg_PY2", "off_ypp_PY2", "def_interceptions_pg_PY2", "off_plays_pg_PY2", "off_ppg_PY2", "def_ppg_PY2", "def_yds_pg_PY2", "def_plays_pg_PY2", "def_third_conv_rate_PY2", "def_fourth_conv_rate_PY2", "def_ypp_PY2", "fg_rate_PY2", "fg_rate_allowed_PY2", "fg_made_pg_PY2", "fg_made_pg_allowed_PY2", "xpts_pg_PY2", "xpts_allowed_pg_PY2", "kick_return_yds_avg_allowed_PY2", "punt_return_yds_avg_allowed_PY2", "st_ppg_PY2", "st_ppg_allowed_PY2", "oppdef_ppa_PY2", "oppoff_ppa_PY2", "off_ppa_PY2", "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2", "off_stuff_rate_PY2", "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2", "off_pts_per_opp_PY2", "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", "off_havoc_front_seven_PY2", "off_havoc_db_PY2", "off_standard_downs_ppa_PY2", "off_standard_downs_success_rate_PY2", "off_standard_downs_explosiveness_PY2", "off_passing_downs_ppa_PY2", "off_passing_downs_success_rate_PY2", "off_passing_downs_explosiveness_PY2", "off_rushing_plays_ppa_PY2", "off_rushing_plays_success_rate_PY2", "off_rushing_plays_explosiveness_PY2", "off_passing_plays_ppa_PY2", "off_passing_plays_success_rate_PY2", "off_passing_plays_explosiveness_PY2", "def_ppa_PY2", "def_success_rate_PY2", "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2", "def_line_yds_PY2", "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2", "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2", "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2", "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2", "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2", "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2", "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2", "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")
  
  ### making list of dfs to be merged
  PY2_df_list <- list(PY2_stats_adv_stats_merge, recruit_PY2, talent_df_PY2)
  ### merging dfs
  PY2_df <- PY2_df_list |>
    reduce(full_join, by = "team")
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main PY2_df
  for (school in 1:nrow(PY2_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_PY2_yards <- PBP_PY2_Yards |>
      filter(pos_team == PY2_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_PY2_Defyards <- PBP_PY2_Yards |>
      filter(def_pos_team == PY2_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_PY2_3rd <- PBP_PY2_3rdDowns |>
      filter(def_pos_team == PY2_df$team[school])
    temp_PBP_PY2_4th <- PBP_PY2_4thDowns |>
      filter(def_pos_team == PY2_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_PY2_OffTDs <- PBP_PY2_TDs |>
      filter(pos_team == PY2_df$team[school])
    ### going to use this to offset the off_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_PY2_Offplays <- PBP_PY2_Yards |>
      filter(pos_team == PY2_df$team[school])
    temp_PBP_PY2_Defplays <- PBP_PY2_Yards |>
      filter(def_pos_team == PY2_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_PY2_OppDefPPA <- PBP_PY2_Yards |>
      filter(def_pos_team %in% temp_PBP_PY2_Offplays$def_pos_team)
    temp_PBP_PY2_OppOffPPA <- PBP_PY2_Yards |>
      filter(pos_team %in% temp_PBP_PY2_Defplays$pos_team)
    temp_PBP_PY2_DefTDs <- PBP_PY2_TDs |>
      filter(def_pos_team == PY2_df$team[school])
    temp_PBP_PY2_2Pts <- PBP_PY2_2ptPlays |>
      filter(pos_team == PY2_df$team[school])
    temp_PBP_PY2_Def2Pts <- PBP_PY2_2ptPlays |>
      filter(def_pos_team == PY2_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_PY2_FGs <- PBP_PY2_FGPlays |>
      filter(pos_team == PY2_df$team[school])
    temp_PBP_PY2_GoodFGs <- temp_PBP_PY2_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY2_DefFGs <- PBP_PY2_FGPlays |>
      filter(def_pos_team == PY2_df$team[school])
    temp_PBP_PY2_DefGoodFGs <- temp_PBP_PY2_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY2_XPts <- PBP_PY2_XPPlays |>
      filter(pos_team == PY2_df$team[school])
    temp_PBP_PY2_DefXPts <- PBP_PY2_XPPlays |>
      filter(def_pos_team == PY2_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_PY2_KickReturn <- PBP_PY2_KickReturn |>
      filter(def_pos_team == PY2_df$team[school])
    temp_PBP_PY2_PuntReturn <- PBP_PY2_Punts |>
      filter(pos_team == PY2_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_PY2_ReturnTDs <- PBP_PY2_ReturnTDs |>
      filter(def_pos_team == PY2_df$team[school])
    temp_PBP_PY2_OffReturnTDs <- PBP_PY2_ReturnTDs |>
      filter(pos_team == PY2_df$team[school])
    temp_PBP_PY2_PuntTDs <- PBP_PY2_PuntReturnTD |>
      filter(pos_team == PY2_df$team[school])
    
    ### using filtered play datasets to calculate variables
    PY2_df$off_plays_pg_PY2[school] = nrow(temp_PBP_PY2_yards) / PY2_df$games_PY2[school]
    PY2_df$off_ppg_PY2[school] = ((nrow(temp_PBP_PY2_OffTDs) * 6) + (nrow(temp_PBP_PY2_2Pts) * 2)) / PY2_df$games_PY2[school]
    PY2_df$def_ppg_PY2[school] = ((nrow(temp_PBP_PY2_DefTDs) * 6) + (nrow(temp_PBP_PY2_Def2Pts) * 2)) / PY2_df$games_PY2[school]
    PY2_df$def_yds_pg_PY2[school] = sum(temp_PBP_PY2_Defyards$yards_gained, na.rm = TRUE) / PY2_df$games_PY2[school]
    PY2_df$def_plays_pg_PY2[school] = nrow(temp_PBP_PY2_Defyards) / PY2_df$games_PY2[school]
    PY2_df$def_third_conv_rate_PY2[school] = sum(temp_PBP_PY2_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY2_3rd)
    PY2_df$def_fourth_conv_rate_PY2[school] = sum(temp_PBP_PY2_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY2_4th)
    PY2_df$def_ypp_PY2[school] = sum(temp_PBP_PY2_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY2_Defyards)
    PY2_df$fg_rate_PY2[school] = nrow(temp_PBP_PY2_GoodFGs) / nrow(temp_PBP_PY2_FGs)
    PY2_df$fg_rate_allowed_PY2[school] = nrow(temp_PBP_PY2_DefGoodFGs) / nrow(temp_PBP_PY2_DefFGs)
    PY2_df$fg_made_pg_PY2[school] = nrow(temp_PBP_PY2_GoodFGs) / PY2_df$games_PY2[school]
    PY2_df$fg_made_pg_allowed_PY2[school] = nrow(temp_PBP_PY2_DefGoodFGs) / PY2_df$games_PY2[school]
    PY2_df$xpts_pg_PY2[school] = nrow(temp_PBP_PY2_XPts) / PY2_df$games_PY2[school]
    PY2_df$xpts_allowed_pg_PY2[school] = nrow(temp_PBP_PY2_DefXPts) / PY2_df$games_PY2[school]
    PY2_df$kick_return_yds_avg_allowed_PY2[school] = sum(temp_PBP_PY2_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY2_KickReturn)
    PY2_df$punt_return_yds_avg_allowed_PY2[school] = sum(temp_PBP_PY2_PuntReturn$yards_gained, na.rm = TRUE)
    PY2_df$st_ppg_PY2[school] = (nrow(temp_PBP_PY2_OffReturnTDs) * 6 / PY2_df$games_PY2[school]) + (nrow(temp_PBP_PY2_XPts) / PY2_df$games_PY2[school]) + (nrow(temp_PBP_PY2_GoodFGs) / PY2_df$games_PY2[school] * 3)
    PY2_df$st_ppg_allowed_PY2[school] = (nrow(temp_PBP_PY2_ReturnTDs) * 6 / PY2_df$games_PY2[school]) + (nrow(temp_PBP_PY2_DefXPts) / PY2_df$games_PY2[school]) + (nrow(temp_PBP_PY2_DefGoodFGs) / PY2_df$games_PY2[school] * 3)
    PY2_df$oppdef_ppa_PY2[school] <- mean(temp_PBP_PY2_OppDefPPA$ppa, na.rm = TRUE)
    PY2_df$oppoff_ppa_PY2[school] <- mean(temp_PBP_PY2_OppOffPPA$ppa, na.rm = TRUE)
  }
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_PY2_yards, temp_PBP_PY2_Defyards, temp_PBP_PY2_3rd, temp_PBP_PY2_4th, temp_PBP_PY2_OffTDs, temp_PBP_PY2_DefTDs, temp_PBP_PY2_2Pts, temp_PBP_PY2_Def2Pts, temp_PBP_PY2_FGs, temp_PBP_PY2_GoodFGs, temp_PBP_PY2_DefFGs, temp_PBP_PY2_DefGoodFGs, temp_PBP_PY2_XPts, temp_PBP_PY2_DefXPts, temp_PBP_PY2_KickReturn, temp_PBP_PY2_PuntReturn, temp_PBP_PY2_ReturnTDs, temp_PBP_PY2_OffReturnTDs, temp_PBP_PY2_PuntTDs, temp_PBP_PY2_OppDefPPA, temp_PBP_PY2_Offplays)
  
  ### binding FCS data from transitioning teams to other PY2 teams
  PY2_df <- rbind(PY2_df, FCS_PY2)
  
  ### PY1
  PY1_stats_adv_stats_list <- list(Stats_PY1, Adv_Stats_PY1)
  PY1_stats_adv_stats_merge <- PY1_stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(team, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  
  colnames(PY1_stats_adv_stats_merge) <- c("team", "games_PY1", "completion_pct_PY1", "pass_ypa_PY1", "pass_ypr_PY1", "int_pct_PY1", "rush_ypc_PY1", "turnovers_pg_PY1", "third_conv_rate_PY1", "fourth_conv_rate_PY1", "penalty_yds_pg_PY1", "yards_per_penalty_PY1", "kick_return_avg_PY1", "punt_return_avg_PY1", "total_yds_pg_PY1", "pass_yds_pg_PY1", "rush_yds_pg_PY1", "first_downs_pg_PY1", "off_ypp_PY1", "def_interceptions_pg_PY1", "off_plays_pg_PY1", "off_ppg_PY1", "def_ppg_PY1", "def_yds_pg_PY1", "def_plays_pg_PY1", "def_third_conv_rate_PY1", "def_fourth_conv_rate_PY1", "def_ypp_PY1", "fg_rate_PY1", "fg_rate_allowed_PY1", "fg_made_pg_PY1", "fg_made_pg_allowed_PY1", "xpts_pg_PY1", "xpts_allowed_pg_PY1", "kick_return_yds_avg_allowed_PY1", "punt_return_yds_avg_allowed_PY1", "st_ppg_PY1", "st_ppg_allowed_PY1", "oppdef_ppa_PY1", "oppoff_ppa_PY1", "off_ppa_PY1", "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1", "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1", "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1", "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1", "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1", "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1", "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1", "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1", "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1", "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1", "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1", "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1", "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1", "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1", "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1", "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1", "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1", "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")
  
  ### making list of dfs to be merged
  PY1_df_list <- list(PY1_stats_adv_stats_merge, recruit_PY1, talent_df_PY1)
  ### merging dfs
  PY1_df <- PY1_df_list |>
    reduce(full_join, by = "team")
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main PY1_df
  
  ### list of relevant PBP dfs
  # PBP_PY1_Yards
  # PBP_PY1_3rdDowns
  # PBP_PY1_4thDowns
  # PBP_PY1_TDs
  # PBP_PY1_2ptPlays
  # PBP_PY1_FGPlays
  # PBP_PY1_XPPlays
  # PBP_PY1_OppDefPPA
  ### on ReturnTD plays, pos_team does the scoring, except on punt return TDs
  # PBP_PY1_ReturnTDs
  # PBP_PY1_PuntReturnTD
  ### on KickReturnPlays, pos_team gains yards/does the returning
  # PBP_PY1_KickReturn
  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY1_Punts
  for (school in 1:nrow(PY1_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_PY1_yards <- PBP_PY1_Yards |>
      filter(pos_team == PY1_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_PY1_Defyards <- PBP_PY1_Yards |>
      filter(def_pos_team == PY1_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_PY1_3rd <- PBP_PY1_3rdDowns |>
      filter(def_pos_team == PY1_df$team[school])
    temp_PBP_PY1_4th <- PBP_PY1_4thDowns |>
      filter(def_pos_team == PY1_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_PY1_OffTDs <- PBP_PY1_TDs |>
      filter(pos_team == PY1_df$team[school])
    ### going to use this to offset the off_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_PY1_Offplays <- PBP_PY1_Yards |>
      filter(pos_team == PY1_df$team[school])
    temp_PBP_PY1_Defplays <- PBP_PY1_Yards |>
      filter(def_pos_team == PY1_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_PY1_OppDefPPA <- PBP_PY1_Yards |>
      filter(def_pos_team %in% temp_PBP_PY1_Offplays$def_pos_team)
    temp_PBP_PY1_OppOffPPA <- PBP_PY1_Yards |>
      filter(pos_team %in% temp_PBP_PY1_Defplays$pos_team)
    temp_PBP_PY1_DefTDs <- PBP_PY1_TDs |>
      filter(def_pos_team == PY1_df$team[school])
    temp_PBP_PY1_2Pts <- PBP_PY1_2ptPlays |>
      filter(pos_team == PY1_df$team[school])
    temp_PBP_PY1_Def2Pts <- PBP_PY1_2ptPlays |>
      filter(def_pos_team == PY1_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_PY1_FGs <- PBP_PY1_FGPlays |>
      filter(pos_team == PY1_df$team[school])
    temp_PBP_PY1_GoodFGs <- temp_PBP_PY1_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY1_DefFGs <- PBP_PY1_FGPlays |>
      filter(def_pos_team == PY1_df$team[school])
    temp_PBP_PY1_DefGoodFGs <- temp_PBP_PY1_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_PY1_XPts <- PBP_PY1_XPPlays |>
      filter(pos_team == PY1_df$team[school])
    temp_PBP_PY1_DefXPts <- PBP_PY1_XPPlays |>
      filter(def_pos_team == PY1_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_PY1_KickReturn <- PBP_PY1_KickReturn |>
      filter(def_pos_team == PY1_df$team[school])
    temp_PBP_PY1_PuntReturn <- PBP_PY1_Punts |>
      filter(pos_team == PY1_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_PY1_ReturnTDs <- PBP_PY1_ReturnTDs |>
      filter(def_pos_team == PY1_df$team[school])
    temp_PBP_PY1_OffReturnTDs <- PBP_PY1_ReturnTDs |>
      filter(pos_team == PY1_df$team[school])
    temp_PBP_PY1_PuntTDs <- PBP_PY1_PuntReturnTD |>
      filter(pos_team == PY1_df$team[school])
    
    ### using filtered play datasets to calculate variables
    PY1_df$off_plays_pg_PY1[school] = nrow(temp_PBP_PY1_yards) / PY1_df$games_PY1[school]
    PY1_df$off_ppg_PY1[school] = ((nrow(temp_PBP_PY1_OffTDs) * 6) + (nrow(temp_PBP_PY1_2Pts) * 2)) / PY1_df$games_PY1[school]
    PY1_df$def_ppg_PY1[school] = ((nrow(temp_PBP_PY1_DefTDs) * 6) + (nrow(temp_PBP_PY1_Def2Pts) * 2)) / PY1_df$games_PY1[school]
    PY1_df$def_yds_pg_PY1[school] = sum(temp_PBP_PY1_Defyards$yards_gained, na.rm = TRUE) / PY1_df$games_PY1[school]
    PY1_df$def_plays_pg_PY1[school] = nrow(temp_PBP_PY1_Defyards) / PY1_df$games_PY1[school]
    PY1_df$def_third_conv_rate_PY1[school] = sum(temp_PBP_PY1_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY1_3rd)
    PY1_df$def_fourth_conv_rate_PY1[school] = sum(temp_PBP_PY1_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_PY1_4th)
    PY1_df$def_ypp_PY1[school] = sum(temp_PBP_PY1_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY1_Defyards)
    PY1_df$fg_rate_PY1[school] = nrow(temp_PBP_PY1_GoodFGs) / nrow(temp_PBP_PY1_FGs)
    PY1_df$fg_rate_allowed_PY1[school] = nrow(temp_PBP_PY1_DefGoodFGs) / nrow(temp_PBP_PY1_DefFGs)
    PY1_df$fg_made_pg_PY1[school] = nrow(temp_PBP_PY1_GoodFGs) / PY1_df$games_PY1[school]
    PY1_df$fg_made_pg_allowed_PY1[school] = nrow(temp_PBP_PY1_DefGoodFGs) / PY1_df$games_PY1[school]
    PY1_df$xpts_pg_PY1[school] = nrow(temp_PBP_PY1_XPts) / PY1_df$games_PY1[school]
    PY1_df$xpts_allowed_pg_PY1[school] = nrow(temp_PBP_PY1_DefXPts) / PY1_df$games_PY1[school]
    PY1_df$kick_return_yds_avg_allowed_PY1[school] = sum(temp_PBP_PY1_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_PY1_KickReturn)
    PY1_df$punt_return_yds_avg_allowed_PY1[school] = sum(temp_PBP_PY1_PuntReturn$yards_gained, na.rm = TRUE)
    PY1_df$st_ppg_PY1[school] = (nrow(temp_PBP_PY1_OffReturnTDs) * 6 / PY1_df$games_PY1[school]) + (nrow(temp_PBP_PY1_XPts) / PY1_df$games_PY1[school]) + (nrow(temp_PBP_PY1_GoodFGs) / PY1_df$games_PY1[school] * 3)
    PY1_df$st_ppg_allowed_PY1[school] = (nrow(temp_PBP_PY1_ReturnTDs) * 6 / PY1_df$games_PY1[school]) + (nrow(temp_PBP_PY1_DefXPts) / PY1_df$games_PY1[school]) + (nrow(temp_PBP_PY1_DefGoodFGs) / PY1_df$games_PY1[school] * 3)
    PY1_df$oppdef_ppa_PY1[school] <- mean(temp_PBP_PY1_OppDefPPA$ppa, na.rm = TRUE)
    PY1_df$oppoff_ppa_PY1[school] <- mean(temp_PBP_PY1_OppOffPPA$ppa, na.rm = TRUE)
  }
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_PY1_yards, temp_PBP_PY1_Defyards, temp_PBP_PY1_3rd, temp_PBP_PY1_4th, temp_PBP_PY1_OffTDs, temp_PBP_PY1_DefTDs, temp_PBP_PY1_2Pts, temp_PBP_PY1_Def2Pts, temp_PBP_PY1_FGs, temp_PBP_PY1_GoodFGs, temp_PBP_PY1_DefFGs, temp_PBP_PY1_DefGoodFGs, temp_PBP_PY1_XPts, temp_PBP_PY1_DefXPts, temp_PBP_PY1_KickReturn, temp_PBP_PY1_PuntReturn, temp_PBP_PY1_ReturnTDs, temp_PBP_PY1_OffReturnTDs, temp_PBP_PY1_PuntTDs, temp_PBP_PY1_OppDefPPA, temp_PBP_PY1_Offplays)
  
  ### binding FCS transitioning teams to df of other PY1 teams
  PY1_df <- rbind(PY1_df, FCS_PY1)
  
  ### Creating Opponent-adjusted stats with ridge regression
  ### PY3
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment_PY3[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY3$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY3$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY3_df <- PY3_df |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa_PY3 = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa_PY3 = ppa) |>
    mutate(adj_ppa_diff_PY3 = adj_off_ppa_PY3 - adj_def_ppa_PY3)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment_PY3[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY3$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY3$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  PY3_df <- PY3_df |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness_PY3 = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness_PY3 = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment_PY3[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY3$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY3$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  PY3_df  <- PY3_df |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp_PY3 = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp_PY3 = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment_PY3[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY3$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY3$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  PY3_df  <- PY3_df |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play_PY3 = play_pts_scored) |>
    mutate(adj_off_ppg_PY3 = adj_off_pts_per_play_PY3 * off_plays_pg_PY3) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play_PY3 = play_pts_scored) |>
    mutate(adj_def_ppg_PY3 = abs(adj_def_pts_per_play_PY3) * def_plays_pg_PY3)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment_PY3[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY3$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY3$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY3_df <- PY3_df |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_PY3 = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed_PY3 = ppa) |>
    mutate(net_adj_st_ppa_PY3 = adj_st_ppa_PY3 - adj_st_ppa_allowed_PY3)
  
  ### PY2
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment_PY2[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY2$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY2$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY2_df <- PY2_df |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa_PY2 = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa_PY2 = ppa) |>
    mutate(adj_ppa_diff_PY2 = adj_off_ppa_PY2 - adj_def_ppa_PY2)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment_PY2[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY2$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY2$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  PY2_df <- PY2_df |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness_PY2 = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness_PY2 = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment_PY2[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY2$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY2$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  PY2_df  <- PY2_df |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp_PY2 = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp_PY2 = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment_PY2[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY2$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY2$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  PY2_df  <- PY2_df |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play_PY2 = play_pts_scored) |>
    mutate(adj_off_ppg_PY2 = adj_off_pts_per_play_PY2 * off_plays_pg_PY2) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play_PY2 = play_pts_scored) |>
    mutate(adj_def_ppg_PY2 = abs(adj_def_pts_per_play_PY2) * def_plays_pg_PY2)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment_PY2[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY2$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY2$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY2_df <- PY2_df |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_PY2 = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed_PY2 = ppa) |>
    mutate(net_adj_st_ppa_PY2 = adj_st_ppa_PY2 - adj_st_ppa_allowed_PY2)
  
  ### PY1
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment_PY1[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY1$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment_PY1$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY1_df <- PY1_df |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa_PY1 = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa_PY1 = ppa) |>
    mutate(adj_ppa_diff_PY1 = adj_off_ppa_PY1 - adj_def_ppa_PY1)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment_PY1[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY1$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment_PY1$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  PY1_df <- PY1_df |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness_PY1 = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness_PY1 = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment_PY1[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY1$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment_PY1$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  PY1_df  <- PY1_df |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp_PY1 = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp_PY1 = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment_PY1[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY1$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment_PY1$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  PY1_df  <- PY1_df |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play_PY1 = play_pts_scored) |>
    mutate(adj_off_ppg_PY1 = adj_off_pts_per_play_PY1 * off_plays_pg_PY1) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play_PY1 = play_pts_scored) |>
    mutate(adj_def_ppg_PY1 = abs(adj_def_pts_per_play_PY1) * def_plays_pg_PY1)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment_PY1[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY1$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment_PY1$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  PY1_df <- PY1_df |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_PY1 = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed_PY1 = ppa) |>
    mutate(net_adj_st_ppa_PY1 = adj_st_ppa_PY1 - adj_st_ppa_allowed_PY1)
  
  
  ### merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(PY3_df, PY2_df, PY1_df, recruit)
  VoA_Variables <- all_PY_df_list |>
    reduce(full_join, by = "team")
  
  ### Making values numeric
  VoA_Variables[,4:ncol(VoA_Variables)] <- VoA_Variables[,4:ncol(VoA_Variables)] |> mutate_if(is.character,as.numeric)
  ### adding difference columns
  VoA_Variables <- VoA_Variables |>
    mutate(PPA_diff_PY3 = adj_off_ppa_PY3 - adj_def_ppa_PY3,
           PPA_diff_PY2 = adj_off_ppa_PY2 - adj_def_ppa_PY2,
           PPA_diff_PY1 = adj_off_ppa_PY1 - adj_def_ppa_PY1,
           SuccessRt_diff_PY3 = off_success_rate_PY3 - def_success_rate_PY3,
           SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
           SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
           HavocRt_diff_PY3 = def_havoc_total_PY3 - off_havoc_total_PY3,
           HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
           HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
           Explosiveness_diff_PY3 = adj_off_explosiveness_PY3 - adj_def_explosiveness_PY3,
           Explosiveness_diff_PY2 = adj_off_explosiveness_PY2 - adj_def_explosiveness_PY2,
           Explosiveness_diff_PY1 = adj_off_explosiveness_PY1 - adj_def_explosiveness_PY1,
           net_st_ppg_PY3 = st_ppg_PY3 - st_ppg_allowed_PY3,
           net_st_ppg_PY2 = st_ppg_PY2 - st_ppg_allowed_PY2,
           net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1)
  
  ### writing csv of PY3_df so that I don't have to run the same code to produce it in Weeks when PY3 data is still being used
  ### since PY3_df is used to transport columns containing what season and conference teams are in for week 0 but not later weeks, I have to remove those columns first
  PY3_df_NoSeasonConf <- PY3_df |>
    select(-one_of("season", "conference"))
  ### writing csv of PY1_df so that I don't have to run the same code to produce it in Weeks when PY1 data is still being used
  ### writing csv of PY2_df so that I don't have to run the same code to produce it in Weeks when PY2 data is still being used
  if (dir.exists(here("Data", paste0("VoA", year), "PYData")) == FALSE){
    dir.create(here("Data", paste0("VoA", year), "PYData"), recursive = TRUE)
  }
  write_csv(PY3_df_NoSeasonConf, here("Data", paste0("VoA", year), "PYData", "PY3.csv"))
  write_csv(PY2_df, here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  write_csv(PY1_df, here("Data", paste0("VoA", year), "PYData", "PY1.csv"))
} else if (as.numeric(week) == 1) {
  ##### WEEK 1 DF Merge #####
  ### merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  ### Previous years data have been saved as csvs to prevent having to pull in data from cfbfastR for rest of season
  ## then I will merge years together by team
  
  ### Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ## merging all current year data frames
  Current_df_list <- list(stats_adv_stats_merge, recruit)
  Current_df <- Current_df_list |>
    reduce(full_join, by = "team")
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main df
  for (school in 1:nrow(Current_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_yards <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_Defyards <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_3rd <- PBP_3rdDowns |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_4th <- PBP_4thDowns |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_OffTDs <- PBP_TDs |>
      filter(pos_team == Current_df$team[school])
    ### going to use this to offset the off_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_Offplays <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Defplays <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_OppDefPPA <- PBP_Yards |>
      filter(def_pos_team %in% temp_PBP_Offplays$def_pos_team)
    temp_PBP_OppOffPPA <- PBP_Yards |>
      filter(pos_team %in% temp_PBP_Defplays$pos_team)
    temp_PBP_DefTDs <- PBP_TDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_2Pts <- PBP_2ptPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Def2Pts <- PBP_2ptPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_FGs <- PBP_FGPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_GoodFGs <- temp_PBP_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_DefFGs <- PBP_FGPlays |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_DefGoodFGs <- temp_PBP_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_XPts <- PBP_XPPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_DefXPts <- PBP_XPPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_KickReturn <- PBP_KickReturn |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_PuntReturn <- PBP_Punts |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_ReturnTDs <- PBP_ReturnTDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_OffReturnTDs <- PBP_ReturnTDs |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_PuntTDs <- PBP_PuntReturnTD |>
      filter(pos_team == Current_df$team[school])
    
    ### using filtered play datasets to calculate variables
    Current_df$off_plays_pg[school] = nrow(temp_PBP_yards) / Current_df$games[school]
    Current_df$off_ppg[school] = ((nrow(temp_PBP_OffTDs) * 6) + (nrow(temp_PBP_2Pts) * 2)) / Current_df$games[school]
    Current_df$def_ppg[school] = ((nrow(temp_PBP_DefTDs) * 6) + (nrow(temp_PBP_Def2Pts) * 2)) / Current_df$games[school]
    Current_df$def_yds_pg[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / Current_df$games[school]
    Current_df$def_plays_pg[school] = nrow(temp_PBP_Defyards) / Current_df$games[school]
    Current_df$def_third_conv_rate[school] = sum(temp_PBP_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_3rd)
    if (nrow(temp_PBP_4th) == 0){
      Current_df$def_fourth_conv_rate[school] = 0
    } else{
      Current_df$def_fourth_conv_rate[school] = sum(temp_PBP_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_4th)
    }
    Current_df$def_ypp[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_Defyards)
    if (nrow(temp_PBP_FGs) == 0){
      Current_df$fg_rate[school] = 0
    } else{
      Current_df$fg_rate[school] = nrow(temp_PBP_GoodFGs) / nrow(temp_PBP_FGs)
    }
    if (nrow(temp_PBP_DefFGs) == 0){
      Current_df$fg_rate_allowed[school] = 0
    } else{
      Current_df$fg_rate_allowed[school] = nrow(temp_PBP_DefGoodFGs) / nrow(temp_PBP_DefFGs)
    }
    Current_df$fg_made_pg[school] = nrow(temp_PBP_GoodFGs) / Current_df$games[school]
    Current_df$fg_made_pg_allowed[school] = nrow(temp_PBP_DefGoodFGs) / Current_df$games[school]
    Current_df$xpts_pg[school] = nrow(temp_PBP_XPts) / Current_df$games[school]
    Current_df$xpts_allowed_pg[school] = nrow(temp_PBP_DefXPts) / Current_df$games[school]
    Current_df$kick_return_yds_avg_allowed[school] = sum(temp_PBP_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_KickReturn)
    Current_df$punt_return_yds_avg_allowed[school] = sum(temp_PBP_PuntReturn$yards_gained, na.rm = TRUE)
    Current_df$st_ppg[school] = (nrow(temp_PBP_OffReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_XPts) / Current_df$games[school]) + (nrow(temp_PBP_GoodFGs) / Current_df$games[school] * 3) 
    Current_df$st_ppg_allowed[school] = (nrow(temp_PBP_ReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_DefXPts) / Current_df$games[school]) + (nrow(temp_PBP_DefGoodFGs) / Current_df$games[school] * 3)
    if (nrow(temp_PBP_OppDefPPA) == 0 | length(unique(temp_PBP_OppDefPPA$def_pos_team)) == 0 | Current_df$games[school] == 0 | is.na(nrow(temp_PBP_OppDefPPA)) | is.na(length(unique(temp_PBP_OppDefPPA$pos_team))) | is.na(Current_df$games[school])){
      Current_df$oppdef_ppa[school] <- 0
    } else{
      Current_df$oppdef_ppa[school] <- mean(temp_PBP_OppDefPPA$ppa, na.rm = TRUE)
    }
    if (nrow(temp_PBP_OppOffPPA) == 0 | length(unique(temp_PBP_OppOffPPA$pos_team)) == 0 | Current_df$games[school] == 0 | is.na(nrow(temp_PBP_OppOffPPA)) | is.na(length(unique(temp_PBP_OppOffPPA$pos_team))) | is.na(Current_df$games[school])){
      Current_df$oppoff_ppa[school] <- 0
    } else{
      Current_df$oppoff_ppa[school] <- mean(temp_PBP_OppOffPPA$ppa, na.rm = TRUE)
    }
  }
  ##### TEMP 2024 week 1 fix for data issues/teams with no games played #####
  # Current_df <- rbind(Current_df, BallStCMUULM)
  ##### End of Temp Fix
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_yards, temp_PBP_Defyards, temp_PBP_3rd, temp_PBP_4th, temp_PBP_OffTDs, temp_PBP_DefTDs, temp_PBP_2Pts, temp_PBP_Def2Pts, temp_PBP_FGs, temp_PBP_GoodFGs, temp_PBP_DefFGs, temp_PBP_DefGoodFGs, temp_PBP_XPts, temp_PBP_DefXPts, temp_PBP_KickReturn, temp_PBP_PuntReturn, temp_PBP_ReturnTDs, temp_PBP_OffReturnTDs, temp_PBP_PuntTDs)
  
  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(Current_df, PY3_df, PY2_df, PY1_df)
  VoA_Variables <- all_PY_df_list |>
    reduce(full_join, by = "team") |>
    mutate(PPA_diff_PY3 = off_ppa_PY3 - def_ppa_PY3,
           PPA_diff_PY2 = off_ppa_PY2 - def_ppa_PY2,
           PPA_diff_PY1 = off_ppa_PY1 - def_ppa_PY1,
           SuccessRt_diff_PY3 = off_success_rate_PY3 - def_success_rate_PY3,
           SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
           SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
           HavocRt_diff_PY3 = def_havoc_total_PY3 - off_havoc_total_PY3,
           HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
           HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
           Explosiveness_diff_PY3 = off_explosiveness_PY3 - def_explosiveness_PY3,
           Explosiveness_diff_PY2 = off_explosiveness_PY2 - def_explosiveness_PY2,
           Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
           PPA_diff = off_ppa - def_ppa,
           SuccessRt_diff = off_success_rate - def_success_rate,
           HavocRt_diff = def_havoc_total - off_havoc_total,
           Explosiveness_diff = off_explosiveness - def_explosiveness,
           net_st_ppg_PY3 = st_ppg_PY3 - st_ppg_allowed_PY3,
           net_st_ppg_PY2 = st_ppg_PY2 - st_ppg_allowed_PY2,
           net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1,
           net_st_ppg = st_ppg - st_ppg_allowed)
  
  ### Creating Opponent-adjusted stats with ridge regression
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa = ppa) |>
    mutate(adj_ppa_diff = adj_off_ppa - adj_def_ppa)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play = play_pts_scored) |>
    mutate(adj_off_ppg = adj_off_pts_per_play * off_plays_pg) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play = play_pts_scored) |>
    mutate(adj_def_ppg = abs(adj_def_pts_per_play) * def_plays_pg)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed = ppa) |>
    mutate(net_adj_st_ppa = adj_st_ppa - adj_st_ppa_allowed)
  
  ### NAs in the special teams columns after week 1, substituting 0s for simplicity
  for (i in 1:nrow(VoA_Variables)){
    if(is.na(VoA_Variables$adj_st_ppa[i]) | is.na(VoA_Variables$adj_st_ppa_allowed[i])){
      VoA_Variables$adj_st_ppa[i] = 0
      VoA_Variables$adj_st_ppa_allowed[i] = 0
    }
    VoA_Variables$net_adj_st_ppa[i] = VoA_Variables$adj_st_ppa[i] - VoA_Variables$adj_st_ppa_allowed[i] 
  }
} else if (as.numeric(week) <= 5) {
  ##### WEEKS 2-5 DF Merge #####
  ### merging data frames together, arranging columns
  ### Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ### merging all current year data frames
  Current_df_list <- list(stats_adv_stats_merge, recruit)
  Current_df <- Current_df_list |>
    reduce(full_join, by = "team")
  
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main df
  for (school in 1:nrow(Current_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_yards <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_Defyards <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_3rd <- PBP_3rdDowns |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_4th <- PBP_4thDowns |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_OffTDs <- PBP_TDs |>
      filter(pos_team == Current_df$team[school])
    ### going to use this to offset the off_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_Offplays <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Defplays <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_OppDefPPA <- PBP_Yards |>
      filter(def_pos_team %in% temp_PBP_Offplays$def_pos_team)
    temp_PBP_OppOffPPA <- PBP_Yards |>
      filter(pos_team %in% temp_PBP_Defplays$pos_team)
    temp_PBP_DefTDs <- PBP_TDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_2Pts <- PBP_2ptPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Def2Pts <- PBP_2ptPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_FGs <- PBP_FGPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_GoodFGs <- temp_PBP_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_DefFGs <- PBP_FGPlays |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_DefGoodFGs <- temp_PBP_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_XPts <- PBP_XPPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_DefXPts <- PBP_XPPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_KickReturn <- PBP_KickReturn |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_PuntReturn <- PBP_Punts |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_ReturnTDs <- PBP_ReturnTDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_OffReturnTDs <- PBP_ReturnTDs |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_PuntTDs <- PBP_PuntReturnTD |>
      filter(pos_team == Current_df$team[school])
    
    ### using filtered play datasets to calculate variables
    Current_df$off_plays_pg[school] = nrow(temp_PBP_yards) / Current_df$games[school]
    Current_df$off_ppg[school] = ((nrow(temp_PBP_OffTDs) * 6) + (nrow(temp_PBP_2Pts) * 2)) / Current_df$games[school]
    Current_df$def_ppg[school] = ((nrow(temp_PBP_DefTDs) * 6) + (nrow(temp_PBP_Def2Pts) * 2)) / Current_df$games[school]
    Current_df$def_yds_pg[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / Current_df$games[school]
    Current_df$def_plays_pg[school] = nrow(temp_PBP_Defyards) / Current_df$games[school]
    if (nrow(temp_PBP_3rd) == 0){
      Current_df$def_third_conv_rate[school] = 0
    } else{
      Current_df$def_third_conv_rate[school] = sum(temp_PBP_3rd$first_by_yards) / nrow(temp_PBP_3rd)
    }
    if (nrow(temp_PBP_4th) == 0){
      Current_df$def_fourth_conv_rate[school] = 0
    } else{
      Current_df$def_fourth_conv_rate[school] = sum(temp_PBP_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_4th)
    }
    if (nrow(temp_PBP_Defyards) == 0){
      Current_df$def_ypp[school] = PY1_df$def_ypp_PY1[PY1_df$team == Current_df$team[school]]
    } else{
      Current_df$def_ypp[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_Defyards)
    }
    if (nrow(temp_PBP_FGs) == 0){
      Current_df$fg_rate[school] = 0
    } else{
      Current_df$fg_rate[school] = nrow(temp_PBP_GoodFGs) / nrow(temp_PBP_FGs)
    }
    if (nrow(temp_PBP_DefFGs) == 0){
      Current_df$fg_rate_allowed[school] = 0
    } else{
      Current_df$fg_rate_allowed[school] = nrow(temp_PBP_DefGoodFGs) / nrow(temp_PBP_DefFGs)
    }
    Current_df$fg_made_pg[school] = nrow(temp_PBP_GoodFGs) / Current_df$games[school]
    Current_df$fg_made_pg_allowed[school] = nrow(temp_PBP_DefGoodFGs) / Current_df$games[school]
    Current_df$xpts_pg[school] = nrow(temp_PBP_XPts) / Current_df$games[school]
    Current_df$xpts_allowed_pg[school] = nrow(temp_PBP_DefXPts) / Current_df$games[school]
    if (nrow(temp_PBP_KickReturn) == 0 | is.na(nrow(temp_PBP_KickReturn))){
      Current_df$kick_return_yds_avg_allowed[school] = 0
    } else{
      Current_df$kick_return_yds_avg_allowed[school] = sum(temp_PBP_KickReturn$yards_gained) / nrow(temp_PBP_KickReturn)
    }
    Current_df$punt_return_yds_avg_allowed[school] = sum(temp_PBP_PuntReturn$yards_gained, na.rm = TRUE)
    Current_df$st_ppg[school] = (nrow(temp_PBP_OffReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_XPts) / Current_df$games[school]) + (nrow(temp_PBP_GoodFGs) / Current_df$games[school] * 3) 
    Current_df$st_ppg_allowed[school] = (nrow(temp_PBP_ReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_DefXPts) / Current_df$games[school]) + (nrow(temp_PBP_DefGoodFGs) / Current_df$games[school] * 3)
    if (nrow(temp_PBP_OppDefPPA) == 0 | length(unique(temp_PBP_OppDefPPA$def_pos_team)) == 0 | Current_df$games[school] == 0 | is.na(nrow(temp_PBP_OppDefPPA)) | is.na(length(unique(temp_PBP_OppDefPPA$pos_team))) | is.na(Current_df$games[school])){
      Current_df$oppdef_ppa[school] = 0
    } else{
      Current_df$oppdef_ppa[school] <- mean(temp_PBP_OppDefPPA$ppa, na.rm = TRUE)
    }
    if (nrow(temp_PBP_OppOffPPA) == 0 | length(unique(temp_PBP_OppOffPPA$pos_team)) == 0 | Current_df$games[school] == 0 | is.na(nrow(temp_PBP_OppOffPPA)) | is.na(length(unique(temp_PBP_OppOffPPA$pos_team))) | is.na(Current_df$games[school])){
      Current_df$oppoff_ppa[school] =  0
    } else{
      Current_df$oppoff_ppa[school] = mean(temp_PBP_OppOffPPA$ppa, na.rm = TRUE)
    }
  }
  
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_yards, temp_PBP_Defyards, temp_PBP_3rd, temp_PBP_4th, temp_PBP_OffTDs, temp_PBP_DefTDs, temp_PBP_2Pts, temp_PBP_Def2Pts, temp_PBP_FGs, temp_PBP_GoodFGs, temp_PBP_DefFGs, temp_PBP_DefGoodFGs, temp_PBP_XPts, temp_PBP_DefXPts, temp_PBP_KickReturn, temp_PBP_PuntReturn, temp_PBP_ReturnTDs, temp_PBP_OffReturnTDs, temp_PBP_PuntTDs, temp_PBP_Defplays, temp_PBP_Offplays, temp_PBP_OppDefPPA, temp_PBP_OppOffPPA)
  
  ### combining all dfs
  all_df_list <- list(Current_df, PY2_df, PY1_df)
  VoA_Variables <- all_df_list |>
    reduce(full_join, by = "team") |>
    mutate(PPA_diff_PY2 = off_ppa_PY2 - def_ppa_PY2,
           PPA_diff_PY1 = off_ppa_PY1 - def_ppa_PY1,
           SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
           SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
           HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
           HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
           Explosiveness_diff_PY2 = off_explosiveness_PY2 - def_explosiveness_PY2,
           Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
           PPA_diff = off_ppa - def_ppa,
           SuccessRt_diff = off_success_rate - def_success_rate,
           HavocRt_diff = def_havoc_total - off_havoc_total,
           Explosiveness_diff = off_explosiveness - def_explosiveness,
           net_st_ppg = st_ppg - st_ppg_allowed,
           net_st_ppg_PY2 = st_ppg_PY2 - st_ppg_allowed_PY2,
           net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1)
  
  ### Creating Opponent-adjusted stats with ridge regression
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa = ppa) |>
    mutate(adj_ppa_diff = adj_off_ppa - adj_def_ppa)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play = play_pts_scored) |>
    mutate(adj_off_ppg = adj_off_pts_per_play * off_plays_pg) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play = play_pts_scored) |>
    mutate(adj_def_ppg = abs(adj_def_pts_per_play) * def_plays_pg)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed = ppa) |>
    mutate(net_adj_st_ppa = adj_st_ppa - adj_st_ppa_allowed)
  
  ### NAs in the special teams columns after week 1, substituting 0s for simplicity
  for (i in 1:nrow(VoA_Variables)){
    if(is.na(VoA_Variables$adj_st_ppa[i]) | is.na(VoA_Variables$adj_st_ppa_allowed[i])){
      VoA_Variables$adj_st_ppa[i] = 0
      VoA_Variables$adj_st_ppa_allowed[i] = 0
    }
    VoA_Variables$net_adj_st_ppa[i] = VoA_Variables$adj_st_ppa[i] - VoA_Variables$adj_st_ppa_allowed[i] 
  }
} else if (as.numeric(week) <= 8) {
  ##### WEEKS 6-8 DF Merge #####
  ## merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  # then I will be merging data frames for the same year together
  # then I will merge years together by team
  ### Current Years dataframes
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  
  ### merging all data frames
  all_df_list <- list(stats_adv_stats_merge, recruit, PY1_df)
  Current_df <- all_df_list |>
    reduce(full_join, by = "team")
  
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main df
  for (school in 1:nrow(Current_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_yards <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_Defyards <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_3rd <- PBP_3rdDowns |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_4th <- PBP_4thDowns |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_OffTDs <- PBP_TDs |>
      filter(pos_team == Current_df$team[school])
    ### going to use this to offset the off_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_Offplays <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Defplays <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_OppDefPPA <- PBP_Yards |>
      filter(def_pos_team %in% temp_PBP_Offplays$def_pos_team)
    temp_PBP_OppOffPPA <- PBP_Yards |>
      filter(pos_team %in% temp_PBP_Defplays$pos_team)
    temp_PBP_DefTDs <- PBP_TDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_2Pts <- PBP_2ptPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Def2Pts <- PBP_2ptPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_FGs <- PBP_FGPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_GoodFGs <- temp_PBP_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_DefFGs <- PBP_FGPlays |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_DefGoodFGs <- temp_PBP_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_XPts <- PBP_XPPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_DefXPts <- PBP_XPPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_KickReturn <- PBP_KickReturn |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_PuntReturn <- PBP_Punts |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate st_ppg_allowed
    temp_PBP_ReturnTDs <- PBP_ReturnTDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_OffReturnTDs <- PBP_ReturnTDs |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_PuntTDs <- PBP_PuntReturnTD |>
      filter(pos_team == Current_df$team[school])
    
    ### using filtered play datasets to calculate variables
    Current_df$off_plays_pg[school] = nrow(temp_PBP_yards) / Current_df$games[school]
    Current_df$off_ppg[school] = ((nrow(temp_PBP_OffTDs) * 6) + (nrow(temp_PBP_2Pts) * 2)) / Current_df$games[school]
    Current_df$def_ppg[school] = ((nrow(temp_PBP_DefTDs) * 6) + (nrow(temp_PBP_Def2Pts) * 2)) / Current_df$games[school]
    Current_df$def_yds_pg[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / Current_df$games[school]
    Current_df$def_plays_pg[school] = nrow(temp_PBP_Defyards) / Current_df$games[school]
    Current_df$def_third_conv_rate[school] = sum(temp_PBP_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_3rd)
    Current_df$def_fourth_conv_rate[school] = sum(temp_PBP_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_4th)
    Current_df$def_ypp[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_Defyards)
    Current_df$fg_rate[school] = nrow(temp_PBP_GoodFGs) / nrow(temp_PBP_FGs)
    if(nrow(temp_PBP_DefFGs) == 0 | is.na(nrow(temp_PBP_DefFGs))){
      Current_df$fg_rate_allowed[school] = 0
    } else{
      Current_df$fg_rate_allowed[school] = nrow(temp_PBP_DefGoodFGs) / nrow(temp_PBP_DefFGs)
    }
    Current_df$fg_made_pg[school] = nrow(temp_PBP_GoodFGs) / Current_df$games[school]
    Current_df$fg_made_pg_allowed[school] = nrow(temp_PBP_DefGoodFGs) / Current_df$games[school]
    Current_df$xpts_pg[school] = nrow(temp_PBP_XPts) / Current_df$games[school]
    Current_df$xpts_allowed_pg[school] = nrow(temp_PBP_DefXPts) / Current_df$games[school]
    Current_df$kick_return_yds_avg_allowed[school] = sum(temp_PBP_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_KickReturn)
    Current_df$punt_return_yds_avg_allowed[school] = sum(temp_PBP_PuntReturn$yards_gained, na.rm = TRUE)
    Current_df$st_ppg[school] = (nrow(temp_PBP_OffReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_XPts) / Current_df$games[school]) + (nrow(temp_PBP_GoodFGs) / Current_df$games[school] * 3) 
    Current_df$st_ppg_allowed[school] = (nrow(temp_PBP_ReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_DefXPts) / Current_df$games[school]) + (nrow(temp_PBP_DefGoodFGs) / Current_df$games[school] * 3)
    Current_df$oppdef_ppa[school] <- mean(temp_PBP_OppDefPPA$ppa, na.rm = TRUE)
    Current_df$oppoff_ppa[school] <- mean(temp_PBP_OppOffPPA$ppa, na.rm = TRUE)
  }
  
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_yards, temp_PBP_Defyards, temp_PBP_3rd, temp_PBP_4th, temp_PBP_OffTDs, temp_PBP_DefTDs, temp_PBP_2Pts, temp_PBP_Def2Pts, temp_PBP_FGs, temp_PBP_GoodFGs, temp_PBP_DefFGs, temp_PBP_DefGoodFGs, temp_PBP_XPts, temp_PBP_DefXPts, temp_PBP_KickReturn, temp_PBP_PuntReturn, temp_PBP_ReturnTDs, temp_PBP_OffReturnTDs, temp_PBP_PuntTDs)
  
  ### now that variables derived from pbp data have been filled in, creating final VoA_Variables df
  VoA_Variables <- Current_df |>
    mutate(PPA_diff_PY1 = off_ppa_PY1 - def_ppa_PY1,
           SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
           HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
           Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
           PPA_diff = off_ppa - def_ppa,
           SuccessRt_diff = off_success_rate - def_success_rate,
           HavocRt_diff = def_havoc_total - off_havoc_total,
           Explosiveness_diff = off_explosiveness - def_explosiveness,
           net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1,
           net_st_ppg = st_ppg - st_ppg_allowed)
  
  ### Creating Opponent-adjusted stats with ridge regression
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa = ppa) |>
    mutate(adj_ppa_diff = adj_off_ppa - adj_def_ppa)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp = yards_gained) #|>
  # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play = play_pts_scored) |>
    mutate(adj_off_ppg = adj_off_pts_per_play * off_plays_pg) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play = play_pts_scored) |>
    mutate(adj_def_ppg = abs(adj_def_pts_per_play) * def_plays_pg)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                               ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed = ppa) |>
    mutate(net_adj_st_ppa = adj_st_ppa - adj_st_ppa_allowed)
} else {
  ##### Week 9-End of Season CURRENT SEASON ONLY DF Merge #####
  ## Current Years data frames
  stats_adv_stats_list <- list(Stats, Adv_Stats)
  stats_adv_stats_merge <- stats_adv_stats_list |>
    reduce(full_join, by = "team") |>
    select(season, team, conference, games, completion_pct, pass_ypa, pass_ypr, int_pct, rush_ypc, turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, total_yds_pg, pass_yds_pg, rush_yds_pg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rushing_plays_success_rate, def_rushing_plays_explosiveness, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness)
  ### merging all current year data frames
  ## there used to be multiple dfs that needed to be merged but then I stopped using SRS in my model (yay independence, I guess, finally my model is fully my own) so it's just 1 df and I call it a different object now I guess
  # Current_df_list <- list(stats_adv_stats_merge)
  Current_df <- stats_adv_stats_merge #|>
    # reduce(full_join, by = "team")
  
  ### adding values to off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed before binding relevant FCS transition teams (their values should already be calculated) to main df
  for (school in 1:nrow(Current_df)){
    ### filtering out relevant plays for the team being iterated through
    ### used to calculate off_plays_pg
    temp_PBP_yards <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    ### used to calculate def_yds_pg, def_plays_pg, def_ypp
    temp_PBP_Defyards <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### used to get 3rd and 4th down conversion rate allowed
    temp_PBP_3rd <- PBP_3rdDowns |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_4th <- PBP_4thDowns |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate off_ppg and def_ppg
    temp_PBP_OffTDs <- PBP_TDs |>
      filter(pos_team == Current_df$team[school])
    ### going to use this to offset the off_ppg and def_ppg
    ### pos team needs to be the team doing the scoring
    ### so that touchdowns allowed are touchdowns allowed by their opposition
    temp_PBP_Offplays <- PBP_Yards |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Defplays <- PBP_Yards |>
      filter(def_pos_team == Current_df$team[school])
    ### this temp pbp is how average opposition TDs allowed will be calculated
    ### the temp df above is just part of how we get there
    temp_PBP_OppDefPPA <- PBP_Yards |>
      filter(def_pos_team %in% temp_PBP_Offplays$def_pos_team)
    temp_PBP_OppOffPPA <- PBP_Yards |>
      filter(pos_team %in% temp_PBP_Defplays$pos_team)
    temp_PBP_DefTDs <- PBP_TDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_2Pts <- PBP_2ptPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_Def2Pts <- PBP_2ptPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used as part of calculation of fg_rate, fg_rate_allowed, st_ppg and st_ppg_allowed
    temp_PBP_FGs <- PBP_FGPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_GoodFGs <- temp_PBP_FGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_DefFGs <- PBP_FGPlays |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_DefGoodFGs <- temp_PBP_DefFGs |>
      filter(play_type == "Field Goal Good")
    temp_PBP_XPts <- PBP_XPPlays |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_DefXPts <- PBP_XPPlays |>
      filter(def_pos_team == Current_df$team[school])
    ### used to calculate kick and punt return yards allowed
    temp_PBP_KickReturn <- PBP_KickReturn |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_PuntReturn <- PBP_Punts |>
      filter(pos_team == Current_df$team[school])
    ### creating dfs to calculate net_st_ppa
    ## first creating inverse dfs of kick and punt return dfs above
    ## kick and punt returns for a given team are given by the stats df
    temp_PBP_OffKickReturn <- PBP_KickReturn |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_OffPuntReturn <- PBP_Punts |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_OffSTPlays <- rbind(temp_PBP_FGs, temp_PBP_XPts, temp_PBP_OffKickReturn, temp_PBP_OffPuntReturn)
    temp_PBP_DefSTPlays <- rbind(temp_PBP_DefFGs, temp_PBP_DefXPts, temp_PBP_KickReturn, temp_PBP_PuntReturn)
    ### used to calculate st_ppg_allowed
    temp_PBP_ReturnTDs <- PBP_ReturnTDs |>
      filter(def_pos_team == Current_df$team[school])
    temp_PBP_OffReturnTDs <- PBP_ReturnTDs |>
      filter(pos_team == Current_df$team[school])
    temp_PBP_PuntTDs <- PBP_PuntReturnTD |>
      filter(pos_team == Current_df$team[school])
    
    ### using filtered play datasets to calculate variables
    Current_df$off_plays_pg[school] = nrow(temp_PBP_yards) / Current_df$games[school]
    Current_df$off_ppg[school] = ((nrow(temp_PBP_OffTDs) * 6) + (nrow(temp_PBP_2Pts) * 2)) / Current_df$games[school]
    Current_df$def_ppg[school] = ((nrow(temp_PBP_DefTDs) * 6) + (nrow(temp_PBP_Def2Pts) * 2)) / Current_df$games[school]
    Current_df$def_yds_pg[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / Current_df$games[school]
    Current_df$def_plays_pg[school] = nrow(temp_PBP_Defyards) / Current_df$games[school]
    Current_df$def_third_conv_rate[school] = sum(temp_PBP_3rd$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_3rd)
    Current_df$def_fourth_conv_rate[school] = sum(temp_PBP_4th$first_by_yards, na.rm = TRUE) / nrow(temp_PBP_4th)
    Current_df$def_ypp[school] = sum(temp_PBP_Defyards$yards_gained, na.rm = TRUE) / nrow(temp_PBP_Defyards)
    Current_df$st_ppa[school] = mean(temp_PBP_OffSTPlays$ppa, na.rm = TRUE)
    Current_df$st_ppa_allowed[school] = mean(temp_PBP_DefSTPlays$ppa, na.rm = TRUE)
    Current_df$fg_rate[school] = nrow(temp_PBP_GoodFGs) / nrow(temp_PBP_FGs)
    Current_df$fg_rate_allowed[school] = nrow(temp_PBP_DefGoodFGs) / nrow(temp_PBP_DefFGs)
    Current_df$fg_made_pg[school] = nrow(temp_PBP_GoodFGs) / Current_df$games[school]
    Current_df$fg_made_pg_allowed[school] = nrow(temp_PBP_DefGoodFGs) / Current_df$games[school]
    Current_df$xpts_pg[school] = nrow(temp_PBP_XPts) / Current_df$games[school]
    Current_df$xpts_allowed_pg[school] = nrow(temp_PBP_DefXPts) / Current_df$games[school]
    Current_df$kick_return_yds_avg_allowed[school] = sum(temp_PBP_KickReturn$yards_gained, na.rm = TRUE) / nrow(temp_PBP_KickReturn)
    Current_df$punt_return_yds_avg_allowed[school] = sum(temp_PBP_PuntReturn$yards_gained, na.rm = TRUE)
    Current_df$st_ppg[school] = (nrow(temp_PBP_OffReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_XPts) / Current_df$games[school]) + (nrow(temp_PBP_GoodFGs) / Current_df$games[school] * 3) 
    Current_df$st_ppg_allowed[school] = (nrow(temp_PBP_ReturnTDs) * 6 / Current_df$games[school]) + (nrow(temp_PBP_DefXPts) / Current_df$games[school]) + (nrow(temp_PBP_DefGoodFGs) / Current_df$games[school] * 3)
    Current_df$oppdef_ppa[school] <- mean(temp_PBP_OppDefPPA$ppa, na.rm = TRUE)
    Current_df$oppoff_ppa[school] <- mean(temp_PBP_OppOffPPA$ppa, na.rm = TRUE)
  }
  
  
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(temp_PBP_yards, temp_PBP_Defyards, temp_PBP_3rd, temp_PBP_4th, temp_PBP_OffTDs, temp_PBP_DefTDs, temp_PBP_2Pts, temp_PBP_Def2Pts, temp_PBP_FGs, temp_PBP_GoodFGs, temp_PBP_DefFGs, temp_PBP_DefGoodFGs, temp_PBP_XPts, temp_PBP_DefXPts, temp_PBP_KickReturn, temp_PBP_PuntReturn, temp_PBP_ReturnTDs, temp_PBP_OffReturnTDs, temp_PBP_PuntTDs)
    
    
    
    
  VoA_Variables <- Current_df |>
    mutate(PPA_diff = off_ppa - def_ppa,
           SuccessRt_diff = off_success_rate - def_success_rate,
           HavocRt_diff = def_havoc_total - off_havoc_total,
           Explosiveness_diff = off_explosiveness - def_explosiveness,
           off_ppg_aboveavg = off_ppg - mean(off_ppg),
           def_ppg_aboveavg = def_ppg - mean(def_ppg),
           net_kick_return_avg = kick_return_avg - kick_return_yds_avg_allowed, 
           net_punt_return_avg = punt_return_avg - punt_return_yds_avg_allowed,
           net_st_ppa = st_ppa - st_ppa_allowed,
           net_fg_rate = fg_rate - fg_rate_allowed,
           net_fg_made_pg = fg_made_pg - fg_made_pg_allowed,
           net_xpts_pg = xpts_pg - xpts_allowed_pg,
           net_st_ppg = st_ppg - st_ppg_allowed,
           off_ppg_aboveavg = off_ppg - mean(off_ppg),
           def_ppg_aboveavg = def_ppg - mean(def_ppg))
  
  ### Creating Opponent-adjusted stats with ridge regression
  ### PPA first
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  PPAOppAdjDummyCols <- dummy_cols(PBP_PPA_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  PPA_cvglmnet <- cv.glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0)
  best_lambda <- PPA_cvglmnet$lambda.min
  ### running ridge regression model
  PPA_glmnet <- glmnet(x = as.matrix(PPAOppAdjDummyCols), y = PBP_PPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  PPA_glmnetcoef <- coef(PPA_glmnet)
  PPA_glmnetcoef_vals <- PPA_glmnetcoef@x
  PPA_adjcoefs <- data.frame(coef_name = colnames(PPAOppAdjDummyCols),
                             ridge_reg_coef = PPA_glmnetcoef_vals[2:length(PPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  PPA_adjcoefs <- PPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense PPA metric
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate some chunks of Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  dfAdjOff_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense PPA metric
  dfAdjdef_PPA <- PPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ppa = ppa) |>
    left_join(dfAdjdef_PPA |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ppa = ppa) |>
    mutate(adj_ppa_diff = adj_off_ppa - adj_def_ppa)
  
  ### Opponent-Adjusting Explosiveness
  ### creating dummy columns from explosive plays
  ExpAdj_dummycols <- dummy_cols(PBP_ExpAdjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Identifying best lambda with cross-validation
  ExpAdj_cvglmnet <- cv.glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0)
  best_lambda <- ExpAdj_cvglmnet$lambda.min
  ExpAdj_glmnet <- glmnet(x = as.matrix(ExpAdj_dummycols), y = PBP_ExpAdjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients from ridge regression model
  ExpAdj_glmnetcoef <- coef(ExpAdj_glmnet)
  ExpAdj_glmnetcoef_vals <- ExpAdj_glmnetcoef@x
  ExpAdj_adjcoefs <- data.frame(coef_name = colnames(ExpAdj_dummycols),
                                ridge_reg_coef = ExpAdj_glmnetcoef_vals[2:length(ExpAdj_glmnetcoef_vals)])
  
  ### creating adjusted coefficient based on unique team coefficient and intercept
  ExpAdj_adjcoefs <- ExpAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + ExpAdj_glmnetcoef_vals[1])
  
  ### storing strings of column names to help with conversion of adjusted coefficients to adjusted metrics, or at least I think that's what's happening
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of adjusted offensive explosiveness
  dfAdjOff_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### look I'm not 100% sure what's happening here with the rename or why it adds an index column only to immediately get rid of it but the end result is that I have a dataframe with 1 adjusted metric for each team so I can join it to the VoA Variables df
  ## I asked gemini to translate Bud Davis's python code for opponent adjusted stats into R code and this is what it came up with and as long as it works I'm not gonna question it too much
  ### anyway, creating df of adjusted defensive explosiveness
  dfAdjdef_Exp <- ExpAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted explosiveness metrics to main VoA Variables df
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_off_explosiveness = ppa) |>
    left_join(dfAdjdef_Exp |> rename(team = coef_name), by = "team") |>
    rename(adj_def_explosiveness = ppa)
  
  ### Creating opponent-adjusted YPP stat
  ### Creating dummy columns
  YPPAdj_dummycols <- dummy_cols(PBP_YPP_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### Using cross-validation to identify best lambda for ridge regression
  YPPAdj_cvglmnet <- cv.glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0)
  best_lambda <- YPPAdj_cvglmnet$lambda.min
  ### performing ridge regression
  YPPAdj_glmnet <- glmnet(x = as.matrix(YPPAdj_dummycols), y = PBP_YPP_Adjustment$yards_gained, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients to finalize adjusted YPP metric
  YPPAdj_glmnetcoef <- coef(YPPAdj_glmnet)
  YPPAdj_glmnetcoef_vals <- YPPAdj_glmnetcoef@x
  YPPAdj_adjcoefs <- data.frame(coef_name = colnames(YPPAdj_dummycols),
                                    ridge_reg_coef = YPPAdj_glmnetcoef_vals[2:length(YPPAdj_glmnetcoef_vals)])
  
  YPPAdj_adjcoefs <- YPPAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + YPPAdj_glmnetcoef_vals[1])
  
  ### strings of columns
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "yards_gained"
  
  dfAdjOff_YPP <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  dfAdjdef_YPP  <- YPPAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_off_ypp = yards_gained) |>
    # mutate(adj_off_ypp = adj_off_yards_gained / off_plays_pg) |>
    left_join(dfAdjdef_YPP |> rename(team = coef_name), by = "team") |>
    rename(adj_def_ypp = yards_gained) #|>
    # mutate(adj_def_ypp = abs(adj_def_yards_gained) / def_plays_pg)
  
  ### Creating opponent-adjusted PPG
  ### creating dummy columns
  PPGAdj_dummycols <- dummy_cols(PBP_PPG_Adjustment[,c("pos_team", "def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross validation to identify best lambda
  PPGAdj_cvglmnet <- cv.glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0)
  best_lambda <- PPGAdj_cvglmnet$lambda.min
  ### performing ridge regression
  PPGAdj_glmnet <- glmnet(x = as.matrix(PPGAdj_dummycols), y = PBP_PPG_Adjustment$play_pts_scored, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients
  PPGAdj_glmnetcoef <- coef(PPGAdj_glmnet)
  PPGAdj_glmnetcoef_vals <- PPGAdj_glmnetcoef@x
  PPGAdj_adjcoefs <- data.frame(coef_name = colnames(PPGAdj_dummycols),
                                ridge_reg_coef = PPGAdj_glmnetcoef_vals[2:length(PPGAdj_glmnetcoef_vals)])
  
  PPGAdj_adjcoefs <- PPGAdj_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + PPGAdj_glmnetcoef_vals[1])
  
  ### storing strings used to match up adjusted stat and team or something, I guess
  offstr = "pos_team"
  hfastr = "hfa"
  defstr = "def_pos_team"
  stat = "play_pts_scored"
  
  ### creating df of team and adjusted stat
  dfAdjOff_playpts <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating df of team and adjusted stat
  dfAdjdef_playpts  <- PPGAdj_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### merging adjusted stat columns back into VoA Variables
  VoA_Variables  <- VoA_Variables |>
    left_join(dfAdjOff_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_off_pts_per_play = play_pts_scored) |>
    mutate(adj_off_ppg = adj_off_pts_per_play * off_plays_pg) |>
    left_join(dfAdjdef_playpts |> rename(team = coef_name), by = "team") |>
    rename(adj_def_pts_per_play = play_pts_scored) |>
    mutate(adj_def_ppg = abs(adj_def_pts_per_play) * def_plays_pg)
  
  
  ### Special Teams PPA
  ### creating dummy variables for ridge regression to adjusted ppa (EPA) metrics
  STPPAOppAdjDummyCols <- dummy_cols(PBP_STPPA_Adjustment[,c("real_pos_team", "real_def_pos_team", "hfa")], remove_selected_columns = TRUE)
  
  ### using cross-validation to identify best lambda for ridge regression
  STPPA_cvglmnet <- cv.glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0)
  best_lambda <- STPPA_cvglmnet$lambda.min
  ### running ridge regression model
  STPPA_glmnet <- glmnet(x = as.matrix(STPPAOppAdjDummyCols), y = PBP_STPPA_Adjustment$ppa, alpha = 0, lambda = best_lambda)
  
  ### extracting coefficients for each team, storing in dataframe
  STPPA_glmnetcoef <- coef(STPPA_glmnet)
  STPPA_glmnetcoef_vals <- STPPA_glmnetcoef@x
  STPPA_adjcoefs <- data.frame(coef_name = colnames(STPPAOppAdjDummyCols),
                             ridge_reg_coef = STPPA_glmnetcoef_vals[2:length(STPPA_glmnetcoef_vals)])
  
  ### calculating true coefficient for each team by adding intercept to each team's coefficient
  STPPA_adjcoefs <- STPPA_adjcoefs |>
    mutate(adj_coef = ridge_reg_coef + STPPA_glmnetcoef_vals[1])
  
  ### storing strings which will help turn team coefficients into adjusting metrics, somehow, I guess
  offstr = "real_pos_team"
  hfastr = "hfa"
  defstr = "real_def_pos_team"
  stat = "ppa"
  
  ### creating dataframe of team names and adjusted offense STPPA metric
  dfAdjOff_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(offstr)) == offstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", offstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### creating dataframe of team names and adjusted defense STPPA metric
  dfAdjdef_STPPA <- STPPA_adjcoefs |>
    filter(str_sub(coef_name, 1, nchar(defstr)) == defstr) |>
    rename(!!stat := adj_coef) |>
    mutate(index = 1:n()) |>
    select(-index) |>
    mutate(coef_name = str_replace(coef_name, paste0("^", defstr, "_"), "")) |>
    select(-ridge_reg_coef)
  
  ### joining adjusted metric columns to VoA Variables
  VoA_Variables <- VoA_Variables |>
    left_join(dfAdjOff_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa = ppa) |>
    left_join(dfAdjdef_STPPA |> rename(team = coef_name), by = "team") |>
    rename(adj_st_ppa_allowed = ppa) |>
    mutate(net_adj_st_ppa = adj_st_ppa - adj_st_ppa_allowed)
  
  ### Making values numeric
  VoA_Variables[,4:ncol(VoA_Variables)] <- VoA_Variables[,4:ncol(VoA_Variables)] |> mutate_if(is.character,as.numeric)
} 
### end of if statement

##### Creating Weighted Variables, weights change by week #####
if (as.numeric(week) == 0){
  ##### Preseason Weighted Variables #####
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.7) + (adj_off_ppg_PY2 * 0.2) + (adj_off_ppg_PY3 * 0.1),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.7) + (adj_def_ppg_PY2 * 0.2) + (adj_def_ppg_PY3 * 0.1),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.7) + (net_st_ppg_PY2 * 0.2) + (net_st_ppg_PY3 * 0.1),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY3 * 0.1) + (adj_off_ppa_PY2 * 0.2) + (adj_off_ppa_PY1 * 0.7),
           weighted_off_ypp = (adj_off_ypp_PY3 * 0.1) + (adj_off_ypp_PY2 * 0.2) + (adj_off_ypp_PY1 * 0.7),
           weighted_off_success_rate = (off_success_rate_PY3 * 0.1) + (off_success_rate_PY2 * 0.2) + (off_success_rate_PY1 * 0.7),
           weighted_off_explosiveness = (adj_off_explosiveness_PY3 * 0.1) + (adj_off_explosiveness_PY2 * 0.2) + (adj_off_explosiveness_PY1 * 0.7),
           weighted_third_conv_rate = (third_conv_rate_PY3 * 0.1) + (third_conv_rate_PY2 * 0.2) + (third_conv_rate_PY1 * 0.7),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY3 * 0.1) + (off_pts_per_opp_PY2 * 0.2) + (off_pts_per_opp_PY1 * 0.7),
           weighted_off_plays_pg = (off_plays_pg_PY3 * 0.1) + (off_plays_pg_PY2 * 0.2) + (off_plays_pg_PY1 * 0.7),
           weighted_def_plays_pg = (def_plays_pg_PY3 * 0.1) + (def_plays_pg_PY2 * 0.2) + (def_plays_pg_PY1 * 0.7),
           weighted_def_ppa = (adj_def_ppa_PY3 * 0.1) + (adj_def_ppa_PY2 * 0.2) + (adj_def_ppa_PY1 * 0.7),
           weighted_def_ypp = (adj_def_ypp_PY3 * 0.1) + (adj_def_ypp_PY2 * 0.2) + (adj_def_ypp_PY1 * 0.7),
           weighted_def_success_rate = (def_success_rate_PY3 * 0.1) + (def_success_rate_PY2 * 0.2) + (def_success_rate_PY1 * 0.7),
           weighted_def_explosiveness = (adj_def_explosiveness_PY3 * 0.1) + (adj_def_explosiveness_PY2 * 0.2) + (adj_def_explosiveness_PY1 * 0.7),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY3 * 0.1) + (def_third_conv_rate_PY2 * 0.2) + (def_third_conv_rate_PY1 * 0.7),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY3 * 0.1) + (def_pts_per_opp_PY2 * 0.2) + (def_pts_per_opp_PY1 * 0.7),
           weighted_def_havoc_total = (def_havoc_total_PY3 * 0.1) + (def_havoc_total_PY2 * 0.2) + (def_havoc_total_PY1 * 0.7),
           weighted_net_kick_return_avg = ((kick_return_avg_PY3 - kick_return_yds_avg_allowed_PY3) * 0.1) + ((kick_return_avg_PY2 - kick_return_yds_avg_allowed_PY2) * 0.2) + ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.7),
           weighted_net_punt_return_avg = ((punt_return_avg_PY3 - punt_return_yds_avg_allowed_PY3) * 0.1) + ((punt_return_avg_PY2 - punt_return_yds_avg_allowed_PY2) * 0.2) + ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.7),
           weighted_net_fg_rate = ((fg_rate_PY3 - fg_rate_allowed_PY3) * 0.1) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.2) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.7),
           weighted_net_fg_made_pg = ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * 0.1) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.2) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.7),
           weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * 0.1) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * 0.2) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.7),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY3 * 0.1) + (net_adj_st_ppa_PY2 * 0.2) + (net_adj_st_ppa_PY1 * 0.7),
           weighted_mean_oppdef_ppa = ((oppdef_ppa_PY3 * 0.1) + (oppdef_ppa_PY2 * 0.2) + (oppdef_ppa_PY1 * 0.7)),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY3 * 0.1) + (oppoff_ppa_PY2 * 0.2) + (oppoff_ppa_PY1 * 0.7))
} else if (as.numeric(week) == 1){
  ##### Week 1 Weighted Variables #####
  ### PY1-3, 1 week of current season
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg * 0.1) + (adj_off_ppg_PY1 * 0.7) + (adj_off_ppg_PY2 * 0.15) + (adj_off_ppg_PY3 * 0.05),
           weighted_def_ppg_mean = (adj_def_ppg * 0.1) + (adj_def_ppg_PY1 * 0.7) + (adj_def_ppg_PY2 * 0.15) + (adj_def_ppg_PY3 * 0.05),
           weighted_net_st_ppg_mean = (net_st_ppg * 0.1) + (net_st_ppg_PY1 * 0.7) + (net_st_ppg_PY2 * 0.15) + (net_st_ppg_PY3 * 0.05),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY3 * 0.05) + (adj_off_ppa_PY2 * 0.15) + (adj_off_ppa_PY1 * 0.7) + (adj_off_ppa * 0.1),
           weighted_off_ypp = (adj_off_ypp_PY3 * 0.05) + (adj_off_ypp_PY2 * 0.15) + (adj_off_ypp_PY1 * 0.7) + (adj_off_ypp * 0.1),
           weighted_off_success_rate = (off_success_rate_PY3 * 0.05) + (off_success_rate_PY2 * 0.15) + (off_success_rate_PY1 * 0.7) + (off_success_rate * 0.1),
           weighted_off_explosiveness = (adj_off_explosiveness_PY3 * 0.05) + (adj_off_explosiveness_PY2 * 0.15) + (adj_off_explosiveness_PY1 * 0.7) + (adj_off_explosiveness * 0.1),
           weighted_third_conv_rate = (third_conv_rate_PY3 * 0.05) + (third_conv_rate_PY2 * 0.15) + (third_conv_rate_PY1 * 0.7) + (third_conv_rate * 0.1),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY3 * 0.05) + (off_pts_per_opp_PY2 * 0.15) + (off_pts_per_opp_PY1 * 0.7) + (off_pts_per_opp * 0.1),
           weighted_off_plays_pg = (off_plays_pg_PY3 * 0.05) + (off_plays_pg_PY2 * 0.15) + (off_plays_pg_PY1 * 0.7) + (off_plays_pg * 0.1),
           weighted_def_plays_pg = (def_plays_pg_PY3 * 0.05) + (def_plays_pg_PY2 * 0.15) + (def_plays_pg_PY1 * 0.7) + (def_plays_pg * 0.1),
           weighted_def_ppa = (adj_def_ppa_PY3 * 0.05) + (adj_def_ppa_PY2 * 0.15) + (adj_def_ppa_PY1 * 0.7) + (adj_def_ppa * 0.1),
           weighted_def_ypp = (adj_def_ypp_PY3 * 0.05) + (adj_def_ypp_PY2 * 0.15) + (adj_def_ypp_PY1 * 0.7) + (adj_def_ypp * 0.1),
           weighted_def_success_rate = (def_success_rate_PY3 * 0.05) + (def_success_rate_PY2 * 0.15) + (def_success_rate_PY1 * 0.7) + (def_success_rate * 0.1),
           weighted_def_explosiveness = (adj_def_explosiveness_PY3 * 0.05) + (adj_def_explosiveness_PY2 * 0.15) + (adj_def_explosiveness_PY1 * 0.7) + (adj_def_explosiveness * 0.1),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY3 * 0.05) + (def_third_conv_rate_PY2 * 0.15) + (def_third_conv_rate_PY1 * 0.7) + (def_third_conv_rate * 0.1),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY3 * 0.05) + (def_pts_per_opp_PY2 * 0.15) + (def_pts_per_opp_PY1 * 0.7) + (def_pts_per_opp * 0.1),
           weighted_def_havoc_total = (def_havoc_total_PY3 * 0.05) + (def_havoc_total_PY2 * 0.15) + (def_havoc_total_PY1 * 0.7) + (def_havoc_total * 0.1),
           weighted_net_kick_return_avg = ((kick_return_avg_PY3 - kick_return_yds_avg_allowed_PY3) * 0.05) + ((kick_return_avg_PY2 - kick_return_yds_avg_allowed_PY2) * 0.15) + ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.7) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.1),
           weighted_net_punt_return_avg = ((punt_return_avg_PY3 - punt_return_yds_avg_allowed_PY3) * 0.05) + ((punt_return_avg_PY2 - punt_return_yds_avg_allowed_PY2) * 0.15) + ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.7) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.1),
           weighted_net_fg_rate = ((fg_rate_PY3 - fg_rate_allowed_PY3) * 0.05) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.15) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.7) + ((fg_rate - fg_rate_allowed) * 0.1),
           weighted_net_fg_made_pg = ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * 0.05) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.15) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.7) + ((fg_made_pg - fg_made_pg_allowed) * 0.1),
           weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * 0.05) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * 0.15) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.7) + ((xpts_pg - xpts_allowed_pg) * 0.1),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY3 * 0.05) + (net_adj_st_ppa_PY2 * 0.15) + (net_adj_st_ppa_PY1 * 0.7) + (net_adj_st_ppa * 0.1),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY3 * 0.05) + (oppdef_ppa_PY2 * 0.15) + (oppdef_ppa_PY1 * 0.7) + (oppdef_ppa * 0.1),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY3 * 0.05) + (oppoff_ppa_PY2 * 0.15) + (oppoff_ppa_PY1 * 0.7) + (oppoff_ppa * 0.1))
} else if (as.numeric(week) <= 3){
  ##### Week 2 - Week 3 Weighted Variables #####
  ### PY2, PY1, current data
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.5) + (adj_off_ppg_PY2 * 0.1) + (adj_off_ppg * 0.4),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.5) + (adj_def_ppg_PY2 * 0.1) + (adj_def_ppg * 0.4),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.5) + (net_st_ppg_PY2 * 0.1) + (net_st_ppg * 0.4),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY2 * 0.1) + (adj_off_ppa_PY1 * 0.5) + (adj_off_ppa * 0.4),
           weighted_off_ypp = (adj_off_ypp_PY2 * 0.1) + (adj_off_ypp_PY1 * 0.5) + (adj_off_ypp * 0.4),
           weighted_off_success_rate = (off_success_rate_PY2 * 0.1) + (off_success_rate_PY1 * 0.5) + (off_success_rate * 0.4),
           weighted_off_explosiveness = (adj_off_explosiveness_PY2 * 0.1) + (adj_off_explosiveness_PY1 * 0.5) + (adj_off_explosiveness * 0.4),
           weighted_third_conv_rate = (third_conv_rate_PY2 * 0.1) + (third_conv_rate_PY1 * 0.5) + (third_conv_rate * 0.4),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY2 * 0.1) + (off_pts_per_opp_PY1 * 0.5) + (off_pts_per_opp * 0.4),
           weighted_off_plays_pg = (off_plays_pg_PY2 * 0.1) + (off_plays_pg_PY1 * 0.5) + (off_plays_pg * 0.4),
           weighted_def_plays_pg = (def_plays_pg_PY2 * 0.1) + (def_plays_pg_PY1 * 0.5) + (def_plays_pg * 0.4),
           weighted_def_ppa = (adj_def_ppa_PY2 * 0.1) + (adj_def_ppa_PY1 * 0.5) + (adj_def_ppa * 0.4),
           weighted_def_ypp = (adj_def_ypp_PY2 * 0.1) + (adj_def_ypp_PY1 * 0.5) + (adj_def_ypp * 0.4),
           weighted_def_success_rate = (def_success_rate_PY2 * 0.1) + (def_success_rate_PY1 * 0.5) + (def_success_rate * 0.4),
           weighted_def_explosiveness = (adj_def_explosiveness_PY2 * 0.1) + (adj_def_explosiveness_PY1 * 0.5) + (adj_def_explosiveness * 0.4),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY2 * 0.1) + (def_third_conv_rate_PY1 * 0.5) + (def_third_conv_rate * 0.4),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY2 * 0.1) + (def_pts_per_opp_PY1 * 0.5) + (def_pts_per_opp * 0.4),
           weighted_def_havoc_total = (def_havoc_total_PY2 * 0.1) + (def_havoc_total_PY1 * 0.5) + (def_havoc_total * 0.4),
           weighted_net_kick_return_avg = ((kick_return_avg_PY2 - kick_return_yds_avg_allowed_PY2) * 0.1) + ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.5) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.4),
           weighted_net_punt_return_avg = ((punt_return_avg_PY2 - punt_return_yds_avg_allowed_PY2) * 0.1) + ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.5) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.4),
           weighted_net_fg_rate = ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.1) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.5) + ((fg_rate - fg_rate_allowed) * 0.4),
           weighted_net_fg_made_pg = ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.1) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.5) + ((fg_made_pg - fg_made_pg_allowed) * 0.4),
           weighted_net_xpts_pg = ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * 0.1) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.5) + ((xpts_pg - xpts_allowed_pg) * 0.4),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY2 * 0.1) + (net_adj_st_ppa_PY1 * 0.5) + (net_adj_st_ppa * 0.4),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY2 * 0.1) + (oppdef_ppa_PY1 * 0.5) + (oppdef_ppa * 0.4),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY2 * 0.1) + (oppoff_ppa_PY1 * 0.5) + (oppoff_ppa * 0.4))
} else if (as.numeric(week) == 4){
  ##### Week 4 Weighted Variables #####
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.4) + (adj_off_ppg_PY2 * 0.1) + (adj_off_ppg * 0.5),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.4) + (adj_def_ppg_PY2 * 0.1) + (adj_def_ppg * 0.5),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.4) + (net_st_ppg_PY2 * 0.1) + (net_st_ppg * 0.5),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY2 * 0.1) + (adj_off_ppa_PY1 * 0.4) + (adj_off_ppa * 0.5),
           weighted_off_ypp = (adj_off_ypp_PY2 * 0.1) + (adj_off_ypp_PY1 * 0.4) + (adj_off_ypp * 0.5),
           weighted_off_success_rate = (off_success_rate_PY2 * 0.1) + (off_success_rate_PY1 * 0.4) + (off_success_rate * 0.5),
           weighted_off_explosiveness = (adj_off_explosiveness_PY2 * 0.1) + (adj_off_explosiveness_PY1 * 0.4) + (adj_off_explosiveness * 0.5),
           weighted_third_conv_rate = (third_conv_rate_PY2 * 0.1) + (third_conv_rate_PY1 * 0.4) + (third_conv_rate * 0.5),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY2 * 0.1) + (off_pts_per_opp_PY1 * 0.4) + (off_pts_per_opp * 0.5),
           weighted_off_plays_pg = (off_plays_pg_PY2 * 0.1) + (off_plays_pg_PY1 * 0.4) + (off_plays_pg * 0.5),
           weighted_def_plays_pg = (def_plays_pg_PY2 * 0.1) + (def_plays_pg_PY1 * 0.4) + (def_plays_pg * 0.5),
           weighted_def_ppa = (adj_def_ppa_PY2 * 0.1) + (adj_def_ppa_PY1 * 0.4) + (adj_def_ppa * 0.5),
           weighted_def_ypp = (adj_def_ypp_PY2 * 0.1) + (adj_def_ypp_PY1 * 0.4) + (adj_def_ypp * 0.5),
           weighted_def_success_rate = (def_success_rate_PY2 * 0.1) + (def_success_rate_PY1 * 0.4) + (def_success_rate * 0.5),
           weighted_def_explosiveness = (adj_def_explosiveness_PY2 * 0.1) + (adj_def_explosiveness_PY1 * 0.4) + (adj_def_explosiveness * 0.5),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY2 * 0.1) + (def_third_conv_rate_PY1 * 0.4) + (def_third_conv_rate * 0.5),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY2 * 0.1) + (def_pts_per_opp_PY1 * 0.4) + (def_pts_per_opp * 0.5),
           weighted_def_havoc_total = (def_havoc_total_PY2 * 0.1) + (def_havoc_total_PY1 * 0.4) + (def_havoc_total * 0.5),
           weighted_net_kick_return_avg = ((kick_return_avg_PY2 - kick_return_yds_avg_allowed_PY2) * 0.1) + ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.4) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.5),
           weighted_net_punt_return_avg = ((punt_return_avg_PY2 - punt_return_yds_avg_allowed_PY2) * 0.1) + ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.4) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.5),
           weighted_net_fg_rate = ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.1) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.4) + ((fg_rate - fg_rate_allowed) * 0.5),
           weighted_net_fg_made_pg = ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.1) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.4) + ((fg_made_pg - fg_made_pg_allowed) * 0.5),
           weighted_net_xpts_pg = ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * 0.1) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.4) + ((xpts_pg - xpts_allowed_pg) * 0.5),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY2 * 0.1) + (net_adj_st_ppa_PY1 * 0.4) + (net_adj_st_ppa * 0.5),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY2 * 0.1) + (oppdef_ppa_PY1 * 0.4) + (oppdef_ppa * 0.5),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY2 * 0.1) + (oppoff_ppa_PY1 * 0.4) + (oppoff_ppa * 0.5))
} else if (as.numeric(week) == 5){
  ##### Week 5 Weighted Variables #####
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.35) + (adj_off_ppg_PY2 * 0.05) + (adj_off_ppg * 0.6),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.35) + (adj_def_ppg_PY2 * 0.05) + (adj_def_ppg * 0.6),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.35) + (net_st_ppg_PY2 * 0.05) + (net_st_ppg * 0.6),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY2 * 0.05) + (adj_off_ppa_PY1 * 0.35) + (adj_off_ppa * 0.6),
           weighted_off_ypp = (adj_off_ypp_PY2 * 0.05) + (adj_off_ypp_PY1 * 0.35) + (adj_off_ypp * 0.6),
           weighted_off_success_rate = (off_success_rate_PY2 * 0.05) + (off_success_rate_PY1 * 0.35) + (off_success_rate * 0.6),
           weighted_off_explosiveness = (adj_off_explosiveness_PY2 * 0.05) + (adj_off_explosiveness_PY1 * 0.35) + (adj_off_explosiveness * 0.6),
           weighted_third_conv_rate = (third_conv_rate_PY2 * 0.05) + (third_conv_rate_PY1 * 0.35) + (third_conv_rate * 0.6),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY2 * 0.05) + (off_pts_per_opp_PY1 * 0.35) + (off_pts_per_opp * 0.6),
           weighted_off_plays_pg = (off_plays_pg_PY2 * 0.05) + (off_plays_pg_PY1 * 0.35) + (off_plays_pg * 0.6),
           weighted_def_plays_pg = (def_plays_pg_PY2 * 0.05) + (def_plays_pg_PY1 * 0.35) + (def_plays_pg * 0.6),
           weighted_def_ppa = (adj_def_ppa_PY2 * 0.05) + (adj_def_ppa_PY1 * 0.35) + (adj_def_ppa * 0.6),
           weighted_def_ypp = (adj_def_ypp_PY2 * 0.05) + (adj_def_ypp_PY1 * 0.35) + (adj_def_ypp * 0.6),
           weighted_def_success_rate = (def_success_rate_PY2 * 0.05) + (def_success_rate_PY1 * 0.35) + (def_success_rate * 0.6),
           weighted_def_explosiveness = (adj_def_explosiveness_PY2 * 0.05) + (adj_def_explosiveness_PY1 * 0.35) + (adj_def_explosiveness * 0.6),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY2 * 0.05) + (def_third_conv_rate_PY1 * 0.35) + (def_third_conv_rate * 0.6),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY2 * 0.05) + (def_pts_per_opp_PY1 * 0.35) + (def_pts_per_opp * 0.6),
           weighted_def_havoc_total = (def_havoc_total_PY2 * 0.05) + (def_havoc_total_PY1 * 0.35) + (def_havoc_total * 0.6),
           weighted_net_kick_return_avg = ((kick_return_avg_PY2 - kick_return_yds_avg_allowed_PY2) * 0.05) + ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.35) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.6),
           weighted_net_punt_return_avg = ((punt_return_avg_PY2 - punt_return_yds_avg_allowed_PY2) * 0.05) + ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.35) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.6),
           weighted_net_fg_rate = ((fg_rate_PY2 - fg_rate_allowed_PY2) * 0.05) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.35) + ((fg_rate - fg_rate_allowed) * 0.6),
           weighted_net_fg_made_pg = ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * 0.05) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.35) + ((fg_made_pg - fg_made_pg_allowed) * 0.6),
           weighted_net_xpts_pg = ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * 0.05) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.35) + ((xpts_pg - xpts_allowed_pg) * 0.6),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY2 * 0.05) + (net_adj_st_ppa_PY1 * 0.35) + (net_adj_st_ppa * 0.6),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY2 * 0.05) + (oppdef_ppa_PY1 * 0.35) + (oppdef_ppa * 0.6),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY2 * 0.05) + (oppoff_ppa_PY1 * 0.35) + (oppoff_ppa * 0.6))
} else if (as.numeric(week) == 6){
  ##### Week 6 Weighted Variables #####
  ### only PY1 and current data
  ### adding weighted variables
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.3) + (adj_off_ppg * 0.7),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.3) + (adj_def_ppg * 0.7),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.3) + (net_st_ppg * 0.7),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY1 * 0.3) + (adj_off_ppa * 0.7),
           weighted_off_ypp = (adj_off_ypp_PY1 * 0.3) + (adj_off_ypp * 0.7),
           weighted_off_success_rate = (off_success_rate_PY1 * 0.3) + (off_success_rate * 0.7),
           weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.3) + (adj_off_explosiveness * 0.7),
           weighted_third_conv_rate = (third_conv_rate_PY1 * 0.3) + (third_conv_rate * 0.7),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.3) + (off_pts_per_opp * 0.7),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.3) + (off_plays_pg * 0.7),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.3) + (def_plays_pg * 0.7),
           weighted_def_ppa = (adj_def_ppa_PY1 * 0.3) + (adj_def_ppa * 0.7),
           weighted_def_ypp = (adj_def_ypp_PY1 * 0.3) + (adj_def_ypp * 0.7),
           weighted_def_success_rate = (def_success_rate_PY1 * 0.3) + (def_success_rate * 0.7),
           weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.3) + (adj_def_explosiveness * 0.7),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.3) + (def_third_conv_rate * 0.7),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.3) + (def_pts_per_opp * 0.7),
           weighted_def_havoc_total = (def_havoc_total_PY1 * 0.3) + (def_havoc_total * 0.7),
           weighted_net_kick_return_avg = ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.3) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.7),
           weighted_net_punt_return_avg = ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.3) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.7),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.3) + ((fg_rate - fg_rate_allowed) * 0.7),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.3) + ((fg_made_pg - fg_made_pg_allowed) * 0.7),
           weighted_net_xpts_pg = ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.3) + ((xpts_pg - xpts_allowed_pg) * 0.7),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY1 * 0.3) + (net_adj_st_ppa * 0.7),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY1 * 0.3) + (oppdef_ppa * 0.7),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY1 * 0.3) + (oppoff_ppa * 0.7))
} else if (as.numeric(week) == 7){
  ##### Week 7 Weighted Variables #####
  ### adding weighted variables
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.2) + (adj_off_ppg * 0.8),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.2) + (adj_def_ppg * 0.8),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.2) + (net_st_ppg * 0.8),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY1 * 0.2) + (adj_off_ppa * 0.8),
           weighted_off_ypp = (adj_off_ypp_PY1 * 0.2) + (adj_off_ypp * 0.8),
           weighted_off_success_rate = (off_success_rate_PY1 * 0.2) + (off_success_rate * 0.8),
           weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.2) + (adj_off_explosiveness * 0.8),
           weighted_third_conv_rate = (third_conv_rate_PY1 * 0.2) + (third_conv_rate * 0.8),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.2) + (off_pts_per_opp * 0.8),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.2) + (off_plays_pg * 0.8),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.2) + (def_plays_pg * 0.8),
           weighted_def_ppa = (adj_def_ppa_PY1 * 0.2) + (adj_def_ppa * 0.8),
           weighted_def_ypp = (adj_def_ypp_PY1 * 0.2) + (adj_def_ypp * 0.8),
           weighted_def_success_rate = (def_success_rate_PY1 * 0.2) + (def_success_rate * 0.8),
           weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.2) + (adj_def_explosiveness * 0.8),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.2) + (def_third_conv_rate * 0.8),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.2) + (def_pts_per_opp * 0.8),
           weighted_def_havoc_total = (def_havoc_total_PY1 * 0.2) + (def_havoc_total * 0.8),
           weighted_net_kick_return_avg = ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.2) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.8),
           weighted_net_punt_return_avg = ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.2) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.8),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.2) + ((fg_rate - fg_rate_allowed) * 0.8),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.2) + ((fg_made_pg - fg_made_pg_allowed) * 0.8),
           weighted_net_xpts_pg = ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.2) + ((xpts_pg - xpts_allowed_pg) * 0.8),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY1 * 0.2) + (net_adj_st_ppa * 0.8),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY1 * 0.2) + (oppdef_ppa * 0.8),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY1 * 0.2) + (oppoff_ppa * 0.8))
} else if (as.numeric(week) == 8){
  ##### Week 8 Weighted Variables #####
  ### adding weighted variables
  VoA_Variables <- VoA_Variables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * 0.1) + (adj_off_ppg * 0.9),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * 0.1) + (adj_def_ppg * 0.9),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * 0.1) + (net_st_ppg * 0.9),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_ppa = (adj_off_ppa_PY1 * 0.1) + (adj_off_ppa * 0.9),
           weighted_off_ypp = (adj_off_ypp_PY1 * 0.1) + (adj_off_ypp * 0.9),
           weighted_off_success_rate = (off_success_rate_PY1 * 0.1) + (off_success_rate * 0.9),
           weighted_off_explosiveness = (adj_off_explosiveness_PY1 * 0.1) + (adj_off_explosiveness * 0.9),
           weighted_third_conv_rate = (third_conv_rate_PY1 * 0.1) + (third_conv_rate * 0.9),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * 0.1) + (off_pts_per_opp * 0.9),
           weighted_off_plays_pg = (off_plays_pg_PY1 * 0.1) + (off_plays_pg * 0.9),
           weighted_def_plays_pg = (def_plays_pg_PY1 * 0.1) + (def_plays_pg * 0.9),
           weighted_def_ppa = (adj_def_ppa_PY1 * 0.1) + (adj_def_ppa * 0.9),
           weighted_def_ypp = (adj_def_ypp_PY1 * 0.1) + (adj_def_ypp * 0.9),
           weighted_def_success_rate = (def_success_rate_PY1 * 0.1) + (def_success_rate * 0.9),
           weighted_def_explosiveness = (adj_def_explosiveness_PY1 * 0.1) + (adj_def_explosiveness * 0.9),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * 0.1) + (def_third_conv_rate * 0.9),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * 0.1) + (def_pts_per_opp * 0.9),
           weighted_def_havoc_total = (def_havoc_total_PY1 * 0.1) + (def_havoc_total * 0.9),
           weighted_net_kick_return_avg = ((kick_return_avg_PY1 - kick_return_yds_avg_allowed_PY1) * 0.1) + ((kick_return_avg - kick_return_yds_avg_allowed) * 0.9),
           weighted_net_punt_return_avg = ((punt_return_avg_PY1 - punt_return_yds_avg_allowed_PY1) * 0.1) + ((punt_return_avg - punt_return_yds_avg_allowed) * 0.9),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * 0.1) + ((fg_rate - fg_rate_allowed) * 0.9),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * 0.1) + ((fg_made_pg - fg_made_pg_allowed) * 0.9),
           weighted_net_xpts_pg = ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * 0.1) + ((xpts_pg - xpts_allowed_pg) * 0.9),
           weighted_net_adj_st_ppa = (net_adj_st_ppa_PY1 * 0.1) + (net_adj_st_ppa * 0.9),
           weighted_mean_oppdef_ppa = (oppdef_ppa_PY1 * 0.1) + (oppdef_ppa * 0.9),
           weighted_mean_oppoff_ppa = (oppoff_ppa_PY1 * 0.1) + (oppoff_ppa * 0.9))
} else{
  print("no weighted variables, all current season data")
}

##### Calculating Mean Error of Offensive and Defensive Ratings in Completed FBS games based on previous week's VoA #####
if (as.numeric(week) == 0){
  print("no error adjustment this week!")
} else if (as.numeric(week) <= 2){
  ##### Week 1 - 2 Off & Def Error Calculations #####
  ### adding dummy off and def error columns, to be filled with real values later
  VoA_Variables <- VoA_Variables |>
    mutate(off_error = -999,
           def_error = -999)
  
  ### reading in previous week's VoA for error calculation
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.numeric(week) - 1, "_", VoAString)))
  ### adding dummy home and away off and def VoA rating columns with values to be filled below
  CompletedFBSGames <- CompletedFBSGames |>
    mutate(home_off_VoA_rating = -999,
           home_def_VoA_rating = -999,
           away_off_VoA_rating = -999,
           away_def_VoA_rating = -999)
  
  ### filling in VoA ratings with previous week's VoA ratings
  for (i in 1:nrow(CompletedFBSGames)){
    ### making sure home team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$home_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$home_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
      CompletedFBSGames$home_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
    } else{
      CompletedFBSGames$home_off_VoA_rating[i] = -999
      CompletedFBSGames$home_def_VoA_rating[i] = -999
    }
    ### making sure away team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$away_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$away_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
      CompletedFBSGames$away_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
    } else{
      CompletedFBSGames$away_off_VoA_rating[i] = -999
      CompletedFBSGames$away_def_VoA_rating[i] = -999
    }
  }
  
  ### calculating error based on differences between off and def ratings and actual point totals
  for (i in 1:nrow(VoA_Variables)){
    temp_games <- CompletedFBSGames |>
      filter(home_team == VoA_Variables$team[i] | away_team == VoA_Variables$team[i]) |>
      mutate(team = VoA_Variables$team[i],
             off_error = case_when(home_team == team ~ home_points - home_off_VoA_rating,
                                   TRUE ~ away_points - away_off_VoA_rating),
             def_error = case_when(home_team == team ~ away_points - home_def_VoA_rating,
                                   TRUE ~ home_points - away_def_VoA_rating))
    
    VoA_Variables$off_error[i] = mean(temp_games$off_error)
    VoA_Variables$def_error[i] = mean(temp_games$def_error)
  }
  
  ### experimenting with making the standard deviation for the random-ish error adjustment bigger so a wider range of values could theoretically be added to the adj ppg vals
  for (i in 1:nrow(VoA_Variables)){
    set.seed(802)
    temp_off_ppg <- VoA_Variables$weighted_off_ppg_mean[i]
    VoA_Variables$weighted_off_ppg_mean[i] = temp_off_ppg + rnorm(1, mean = VoA_Variables$off_error[i] / 3, sd = sd(VoA_Variables$off_error) * 1.15)
    temp_def_ppg <- VoA_Variables$weighted_def_ppg_mean[i]
    VoA_Variables$weighted_def_ppg_mean[i] = temp_def_ppg + rnorm(1, mean = VoA_Variables$def_error[i] / 3, sd = sd(VoA_Variables$def_error) * 1.15)
    
    ### making sure all values are > 0
    set.seed(802)
    if (VoA_Variables$weighted_off_ppg_mean[i] <= 0){
      VoA_Variables$weighted_off_ppg_mean[i] = abs(VoA_Variables$weighted_off_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
    if (VoA_Variables$weighted_def_ppg_mean[i] <= 0){
      VoA_Variables$weighted_def_ppg_mean[i] = abs(VoA_Variables$weighted_def_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
  }
  
} else if (as.numeric(week) <= 4){
  ##### Week 3 - 4 Off & Def Error Calculations #####
  ### adding dummy off and def error columns, to be filled with real values later
  VoA_Variables <- VoA_Variables |>
    mutate(off_error = -999,
           def_error = -999)
  
  ### reading in previous week's VoA for error calculation
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, week, "_", VoAString)))
  ### adding dummy home and away off and def VoA rating columns with values to be filled below
  CompletedFBSGames <- CompletedFBSGames |>
    mutate(home_off_VoA_rating = -999,
           home_def_VoA_rating = -999,
           away_off_VoA_rating = -999,
           away_def_VoA_rating = -999)
  
  ### filling in VoA ratings with previous week's VoA ratings
  for (i in 1:nrow(CompletedFBSGames)){
    ### making sure home team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$home_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$home_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
      CompletedFBSGames$home_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
    } else{
      CompletedFBSGames$home_off_VoA_rating[i] = -999
      CompletedFBSGames$home_def_VoA_rating[i] = -999
    }
    ### making sure away team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$away_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$away_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
      CompletedFBSGames$away_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
    } else{
      CompletedFBSGames$away_off_VoA_rating[i] = -999
      CompletedFBSGames$away_def_VoA_rating[i] = -999
    }
  }
  
  ### calculating error based on differences between off and def ratings and actual point totals
  for (i in 1:nrow(VoA_Variables)){
    temp_games <- CompletedFBSGames |>
      filter(home_team == VoA_Variables$team[i] | away_team == VoA_Variables$team[i]) |>
      mutate(team = VoA_Variables$team[i],
             off_error = case_when(home_team == team ~ home_points - home_off_VoA_rating,
                                   TRUE ~ away_points - away_off_VoA_rating),
             def_error = case_when(home_team == team ~ away_points - home_def_VoA_rating,
                                   TRUE ~ home_points - away_def_VoA_rating))
    
    VoA_Variables$off_error[i] = mean(temp_games$off_error)
    VoA_Variables$def_error[i] = mean(temp_games$def_error)
  }
  
  ### experimenting with making the standard deviation for the random-ish error adjustment bigger so a wider range of values could theoretically be added to the adj ppg vals
  for (i in 1:nrow(VoA_Variables)){
    set.seed(802)
    temp_off_ppg <- VoA_Variables$weighted_off_ppg_mean[i]
    VoA_Variables$weighted_off_ppg_mean[i] = temp_off_ppg + rnorm(1, mean = VoA_Variables$off_error[i] / 2, sd = sd(VoA_Variables$off_error) * 1.1)
    temp_def_ppg <- VoA_Variables$weighted_def_ppg_mean[i]
    VoA_Variables$weighted_def_ppg_mean[i] = temp_def_ppg + rnorm(1, mean = VoA_Variables$def_error[i] / 2, sd = sd(VoA_Variables$def_error) * 1.1)
    
    ### making sure all values are > 0
    set.seed(802)
    if (VoA_Variables$weighted_off_ppg_mean[i] <= 0){
      VoA_Variables$weighted_off_ppg_mean[i] = abs(VoA_Variables$weighted_off_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
    if (VoA_Variables$weighted_def_ppg_mean[i] <= 0){
      VoA_Variables$weighted_def_ppg_mean[i] = abs(VoA_Variables$weighted_def_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
  }
} else if (as.numeric(week) == 5){
  ##### Week 5 Off & Def Error Calculations #####
  ### adding dummy off and def error columns, to be filled with real values later
  VoA_Variables <- VoA_Variables |>
    mutate(off_error = -999,
           def_error = -999)
  
  ### reading in previous week's VoA for error calculation
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, week, "_", VoAString)))
  ### adding dummy home and away off and def VoA rating columns with values to be filled below
  CompletedFBSGames <- CompletedFBSGames |>
    mutate(home_off_VoA_rating = -999,
           home_def_VoA_rating = -999,
           away_off_VoA_rating = -999,
           away_def_VoA_rating = -999)
  
  ### filling in VoA ratings with previous week's VoA ratings
  for (i in 1:nrow(CompletedFBSGames)){
    ### making sure home team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$home_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$home_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
      CompletedFBSGames$home_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
    } else{
      CompletedFBSGames$home_off_VoA_rating[i] = -999
      CompletedFBSGames$home_def_VoA_rating[i] = -999
    }
    ### making sure away team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$away_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$away_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
      CompletedFBSGames$away_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
    } else{
      CompletedFBSGames$away_off_VoA_rating[i] = -999
      CompletedFBSGames$away_def_VoA_rating[i] = -999
    }
  }
  
  ### calculating error based on differences between off and def ratings and actual point totals
  for (i in 1:nrow(VoA_Variables)){
    temp_games <- CompletedFBSGames |>
      filter(home_team == VoA_Variables$team[i] | away_team == VoA_Variables$team[i]) |>
      mutate(team = VoA_Variables$team[i],
             off_error = case_when(home_team == team ~ home_points - home_off_VoA_rating,
                                   TRUE ~ away_points - away_off_VoA_rating),
             def_error = case_when(home_team == team ~ away_points - home_def_VoA_rating,
                                   TRUE ~ home_points - away_def_VoA_rating))
    
    VoA_Variables$off_error[i] = mean(temp_games$off_error)
    VoA_Variables$def_error[i] = mean(temp_games$def_error)
  }
  
  ### experimenting with making the standard deviation for the random-ish error adjustment bigger so a wider range of values could theoretically be added to the adj ppg vals
  for (i in 1:nrow(VoA_Variables)){
    set.seed(802)
    temp_off_ppg <- VoA_Variables$weighted_off_ppg_mean[i]
    VoA_Variables$weighted_off_ppg_mean[i] = temp_off_ppg + rnorm(1, mean = VoA_Variables$off_error[i], sd = sd(VoA_Variables$off_error) * 1.05)
    temp_def_ppg <- VoA_Variables$weighted_def_ppg_mean[i]
    VoA_Variables$weighted_def_ppg_mean[i] = temp_def_ppg + rnorm(1, mean = VoA_Variables$def_error[i], sd = sd(VoA_Variables$def_error) * 1.05)
    
    ### making sure all values are > 0
    set.seed(802)
    if (VoA_Variables$weighted_off_ppg_mean[i] <= 0){
      VoA_Variables$weighted_off_ppg_mean[i] = abs(VoA_Variables$weighted_off_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
    if (VoA_Variables$weighted_def_ppg_mean[i] <= 0){
      VoA_Variables$weighted_def_ppg_mean[i] = abs(VoA_Variables$weighted_def_ppg_mean[i]) + abs(rnorm(1, 5, 1))
    }
  }
} else{
  ##### Week 6 - End of Season Error Calculations #####
  ### creating dummy columns for offensive and defensive error, to be filled with real values at the end
  VoA_Variables <- VoA_Variables |>
    mutate(off_error = -999,
           def_error = -999)
  
  ### reading in previous week's VoA ratings for error calculations
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.numeric(week) - 1, "_", VoAString)))
  
  ### adding dummy rating columns to completed games df, adding in ratings in for loop
  CompletedFBSGames <- CompletedFBSGames |>
    mutate(home_off_VoA_rating = -999,
           home_def_VoA_rating = -999,
           away_off_VoA_rating = -999,
           away_def_VoA_rating = -999)
  ### adding in actual ratings by game
  for (i in 1:nrow(CompletedFBSGames)){
    ### making sure home team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$home_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$home_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
      CompletedFBSGames$home_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$home_team[i]]
    } else{
      CompletedFBSGames$home_off_VoA_rating[i] = -999
      CompletedFBSGames$home_def_VoA_rating[i] = -999
    }
    ### making sure away team is in FBS/VoA before assigning specific VoA rating
    if(CompletedFBSGames$away_team[i] %in% VoA_Variables$team){
      CompletedFBSGames$away_off_VoA_rating[i] = PrevWeek_VoA$OffVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
      CompletedFBSGames$away_def_VoA_rating[i] = PrevWeek_VoA$DefVoA_MeanRating[PrevWeek_VoA$team == CompletedFBSGames$away_team[i]]
    } else{
      CompletedFBSGames$away_off_VoA_rating[i] = -999
      CompletedFBSGames$away_def_VoA_rating[i] = -999
    }
  }
  
  ### calculating deviation of offensive and defensive pts for each game from most recent iteration of VoA ratings
  for (i in 1:nrow(VoA_Variables)){
    temp_games <- CompletedFBSGames |>
      filter(home_team == VoA_Variables$team[i] | away_team == VoA_Variables$team[i]) |>
      mutate(team = VoA_Variables$team[i],
             off_error = case_when(home_team == team ~ home_points - home_off_VoA_rating,
                                   TRUE ~ away_points - away_off_VoA_rating),
             def_error = case_when(home_team == team ~ away_points - home_def_VoA_rating,
                                   TRUE ~ home_points - away_def_VoA_rating))
    
    VoA_Variables$off_error[i] = mean(temp_games$off_error)
    VoA_Variables$def_error[i] = mean(temp_games$def_error)
  }
  
  ### adding the average offensive and defensive errors to each teams, filling in the dummy columns created at the start of this section
  for (i in 1:nrow(VoA_Variables)){
    set.seed(802)
    temp_off_ppg <- VoA_Variables$adj_off_ppg[i]
    VoA_Variables$adj_off_ppg[i] = temp_off_ppg + rnorm(1, mean = VoA_Variables$off_error[i], sd = sd(VoA_Variables$off_error))
    temp_def_ppg <- VoA_Variables$adj_def_ppg[i]
    VoA_Variables$adj_def_ppg[i] = temp_def_ppg + rnorm(1, mean = VoA_Variables$def_error[i], sd = sd(VoA_Variables$def_error))
    
    ### making sure all values are > 0
    set.seed(802)
    if (VoA_Variables$adj_off_ppg[i] <= 0){
      VoA_Variables$adj_off_ppg[i] = abs(VoA_Variables$adj_off_ppg[i]) + abs(rnorm(1, 5, 1))
    }
    if (VoA_Variables$adj_def_ppg[i] <= 0){
      VoA_Variables$adj_def_ppg[i] = abs(VoA_Variables$adj_def_ppg[i]) + abs(rnorm(1, 5, 1))
    }
  }
}


##### Eliminating NAs, fixing conferences, adding Week number to VoA Variables #####
### eliminating NAs that may still exist
### leaving this outside an if statement because this could be an issue regardless of season or CFB_Week
### currently commented out because I added this fix to each individual stat pull in function
### uncommented it because I must once again ask that Florida International University go fuck itself
if (as.numeric(week) %in% c(0, 1, 9:16)){
  VoA_Variables$recruit_pts[is.na(VoA_Variables$recruit_pts)] = 0
  VoA_Variables$recruit_pts_PY3[is.na(VoA_Variables$recruit_pts_PY3)] = 0
}

### Fixing conference errors for Week 0 (Preseason)
if (as.numeric(week) == 0) {
  VoA_Variables <- VoA_Variables |>
    select(-conference)
  current_conferences <- cfbd_team_info(year = as.integer(year)) |>
    filter(school %in% VoA_Variables$team) |>
    select(school, conference)
  colnames(current_conferences) <- c("team", "conference")
  VoA_Variables <- full_join(VoA_Variables, current_conferences, by = "team") |>
    relocate(conference, .after = team)
} else {
  print("current season conferences should be in use!")
}

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoA_Variables <- VoA_Variables |>
  mutate(CFB_Week = rep(as.numeric(week), nrow(VoA_Variables)), .before = 2)


##### checking which column to start ranking at #####
VoA_Ncols <- ncol(VoA_Variables) + 1
# if (as.numeric(week) == 0 | as.numeric(week) == 1 | as.numeric(week) == 2 | as.numeric(week) == 6 | as.numeric(week) == 9){
#   break
# } else{
#   print("Same number of VoA columns this week as last week, or it's preseason and this is being done section by section to make sure it works")
# }


##### Adding Rank Columns #####
### probably going to scale this back at some point
### if Week = 0
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
### if Week = 1
# PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
### if week <= 3
# PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
### if week <= 5
# PY2 weighted 1x, PY1 weighted 2x, current weighted 2x
### if week <= 8
# PY1 weighted 1x, current weighted 2x
### if week > 9
# current will be only data source used, everything weighted "1x" (aside from special variables)

### different stats weighted differently as described below
### EPA/PPA stats, explosiveness stats, success rates, havoc rates, Yards/Play, pts/scoring opp weighted 2x in PYs, 3x for current season,
## all #x above refer to weighting being done on top of weighting being done based on which year the data is from
## recruiting 3x in PY3 and PY2, 2x in PY1, 1x for current year
# recruiting phased out after only current season stats are being used (currently week 7)
## talent ranked 1x in PY3 and PY2, 3x in PY1
if (as.numeric(week) == 0) {
  ##### Week 0 Variable Ranks #####
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Comp_Pct_PY3 = dense_rank(desc(completion_pct_PY3)),
           Rank_Pass_YPA_PY3 = dense_rank(desc(pass_ypa_PY3)),
           Rank_Pass_YPR_PY3 = dense_rank(desc(pass_ypr_PY3)),
           Rank_Int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_Rush_YPC_PY3 = dense_rank(desc(rush_ypc_PY3)),
           Rank_Turnovers_pg_PY3 = dense_rank(turnovers_pg_PY3),
           Rank_Third_Conv_Rate_PY3 = dense_rank(desc(third_conv_rate_PY3)),
           Rank_Fourth_Conv_Rate_PY3 = dense_rank(desc(fourth_conv_rate_PY3)),
           Rank_Penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
           Rank_Yds_Per_Penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_Kick_Return_Avg_PY3 = dense_rank(desc(kick_return_avg_PY3)),
           Rank_Punt_Return_Avg_PY3 = dense_rank(desc(punt_return_avg_PY3)),
           Rank_Total_Yds_pg_PY3 = dense_rank(desc(total_yds_pg_PY3)),
           Rank_Pass_Yds_pg_PY3 = dense_rank(desc(pass_yds_pg_PY3)),
           Rank_Rush_Yds_pg_PY3 = dense_rank(desc(rush_yds_pg_PY3)),
           Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(off_ypp_PY3)),
           Rank_Def_Ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_PPA_PY3 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Second_Lvl_Yds_PY3 = dense_rank(desc(off_second_lvl_yds_PY3)),
           Rank_Off_Open_Field_Yds_PY3 = dense_rank(desc(off_open_field_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           Rank_Def_Second_Lvl_Yds_PY3 = dense_rank(def_second_lvl_yds_PY3),
           Rank_Def_Open_Field_Yds_PY3 = dense_rank(def_open_field_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(def_field_pos_avg_predicted_points_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
           Rank_PPA_diff_PY3 = dense_rank(desc(PPA_diff_PY3)),
           Rank_SuccessRt_diff_PY3 = dense_rank(desc(SuccessRt_diff_PY3)),
           Rank_HavocRt_diff_PY3 = dense_rank(desc(HavocRt_diff_PY3)),
           Rank_Explosiveness_diff_PY3 = dense_rank(desc(Explosiveness_diff_PY3)),
           ## PY2 ranks
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
           Rank_PPA_diff_PY2 = dense_rank(desc(PPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           ## PY2 weighted twice
           Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ## PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
           Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1_col2 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Yds_Per_Penalty_PY1_col3 = dense_rank(yards_per_penalty_PY1),
           Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1_col3 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col3 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col3 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col3 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
           ## incoming recruiting class, weighted once
           Rank_Recruit_Pts = dense_rank(desc(recruit_pts)))
} else if (as.numeric(week) == 1) {
  ##### Week 1 Variable Ranks #####
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  ## PY3 ranks added first, weighted once
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Comp_Pct_PY3 = dense_rank(desc(completion_pct_PY3)),
           Rank_Pass_YPA_PY3 = dense_rank(desc(pass_ypa_PY3)),
           Rank_Pass_YPR_PY3 = dense_rank(desc(pass_ypr_PY3)),
           Rank_Int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_Rush_YPC_PY3 = dense_rank(desc(rush_ypc_PY3)),
           Rank_Turnovers_pg_PY3 = dense_rank(turnovers_pg_PY3),
           Rank_Third_Conv_Rate_PY3 = dense_rank(desc(third_conv_rate_PY3)),
           Rank_Fourth_Conv_Rate_PY3 = dense_rank(desc(fourth_conv_rate_PY3)),
           Rank_Penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
           Rank_Yds_Per_Penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_Kick_Return_Avg_PY3 = dense_rank(desc(kick_return_avg_PY3)),
           Rank_Punt_Return_Avg_PY3 = dense_rank(desc(punt_return_avg_PY3)),
           Rank_Total_Yds_pg_PY3 = dense_rank(desc(total_yds_pg_PY3)),
           Rank_Pass_Yds_pg_PY3 = dense_rank(desc(pass_yds_pg_PY3)),
           Rank_Rush_Yds_pg_PY3 = dense_rank(desc(rush_yds_pg_PY3)),
           Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(off_ypp_PY3)),
           Rank_Def_Ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_PPA_PY3 = dense_rank(desc(off_ppa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Second_Lvl_Yds_PY3 = dense_rank(desc(off_second_lvl_yds_PY3)),
           Rank_Off_Open_Field_Yds_PY3 = dense_rank(desc(off_open_field_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Havoc_Front_PY3 = dense_rank(off_havoc_front_seven_PY3),
           Rank_Off_Havoc_DB_PY3 = dense_rank(off_havoc_db_PY3),
           Rank_Off_Standard_Down_PPA_PY3 = dense_rank(desc(off_standard_downs_ppa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3 = dense_rank(desc(off_standard_downs_success_rate_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_PPA_PY3 = dense_rank(desc(off_passing_downs_ppa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3 = dense_rank(desc(off_passing_downs_success_rate_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_PPA_PY3 = dense_rank(desc(off_rushing_plays_ppa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rushing_plays_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rushing_plays_explosiveness_PY3)),
           Rank_Off_Pass_Play_PPA_PY3 = dense_rank(desc(off_passing_plays_ppa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_passing_plays_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_passing_plays_explosiveness_PY3)),
           Rank_Def_PPA_PY3 = dense_rank(def_ppa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           Rank_Def_Second_Lvl_Yds_PY3 = dense_rank(def_second_lvl_yds_PY3),
           Rank_Def_Open_Field_Yds_PY3 = dense_rank(def_open_field_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY3 = dense_rank(def_field_pos_avg_predicted_points_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Havoc_Front_Seven_PY3 = dense_rank(desc(def_havoc_front_seven_PY3)),
           Rank_Def_Havoc_DB_PY3 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_PPA_PY3 = dense_rank(def_standard_downs_ppa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3 = dense_rank(def_standard_downs_success_rate_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_PPA_PY3 = dense_rank(def_passing_downs_ppa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3 = dense_rank(def_passing_downs_success_rate_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_PPA_PY3 = dense_rank(def_rushing_plays_ppa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rushing_plays_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rushing_plays_explosiveness_PY3),
           Rank_Def_Pass_Play_PPA_PY3 = dense_rank(def_passing_plays_ppa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_passing_plays_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_passing_plays_explosiveness_PY3),
           Rank_PPA_diff_PY3 = dense_rank(desc(PPA_diff_PY3)),
           Rank_SuccessRt_diff_PY3 = dense_rank(desc(SuccessRt_diff_PY3)),
           Rank_HavocRt_diff_PY3 = dense_rank(desc(HavocRt_diff_PY3)),
           Rank_Explosiveness_diff_PY3 = dense_rank(desc(Explosiveness_diff_PY3)),
           Rank_Recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
           ## PY2 ranks
           Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_PPA_diff_PY2 = dense_rank(desc(PPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
           ## PY2 weighted twice
           Rank_Comp_Pct_PY2_col2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2_col2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2_col2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2_col2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2_col2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2_col2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2_col2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2_col2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Total_Yds_pg_PY2_col2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2_col2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2_col2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2_col2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2_col2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2_col2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2_col2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2_col2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2_col2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2_col2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2_col2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2_col2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2_col2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
           Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2_col2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2_col2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2_col2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2_col2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_PPA_diff_PY2_col2 = dense_rank(desc(PPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2_col2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2_col2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2_col2 = dense_rank(desc(Explosiveness_diff_PY2)),
           Rank_Recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ### PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1_col2 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
           Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
           Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
           Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
           Rank_PPA_diff_PY1_col3 = dense_rank(desc(PPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col3 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col3 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col3 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
           ### incoming recruiting class, weighted once
           Rank_Recruit_Pts = dense_rank(desc(recruit_pts)),
           ### Ranking current stats
           Rank_Comp_Pct = dense_rank(desc(completion_pct)),
           Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
           Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
           Rank_Int_Pct = dense_rank(int_pct),
           Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
           Rank_Turnovers_pg = dense_rank(turnovers_pg),
           Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
           Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
           Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
           Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
           Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
           Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
           Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
           Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
           Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
           Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(off_ypp)),
           Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_PPA = dense_rank(desc(off_ppa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
           Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
           Rank_Def_PPA = dense_rank(def_ppa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
           Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
           Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
           Rank_PPA_diff = dense_rank(desc(PPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Extra weighted variables for current year
           Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
           Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
           Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
           Rank_Def_PPA_col2 = dense_rank(def_ppa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
           Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
           Rank_PPA_diff_col2 = dense_rank(desc(PPA_diff)),
           Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else if (as.numeric(week) <= 3) {
  ##### Weeks 2-3 Variable Ranks #####
  # PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  VoA_Variables <- VoA_Variables |>
    ## PY2 ranks
    mutate(Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
           Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
           Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
           Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
           Rank_PPA_diff_PY2 = dense_rank(desc(PPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           Rank_Recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_Talent_PY2 = dense_rank(desc(talent_PY2)),
           ## PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_PPA_diff_PY1 = dense_rank(desc(PPA_diff_PY1)),
      Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
      Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
      Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1 = dense_rank(desc(talent_PY1)),
      ## PY1 weighted 3 times
      Rank_Comp_Pct_PY1_col2 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col2 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col2 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col2 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col2 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col2 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col2 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Total_Yds_pg_PY1_col2 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col2 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col2 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col2 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col2 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col2 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col2 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col2 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col2 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col2 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col2 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col2 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col2 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col2 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col2 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_PPA_diff_PY1_col2 = dense_rank(desc(PPA_diff_PY1)),
      Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
      Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
      Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
      Rank_Recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
      Rank_Talent_PY1_col2 = dense_rank(desc(talent_PY1)),
      ## PY1 weighted 3 times
      Rank_Comp_Pct_PY1_col3 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1_col3 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1_col3 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1_col3 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1_col3 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1_col3 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1_col3 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Total_Yds_pg_PY1_col3 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1_col3 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1_col3 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1_col3 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1_col3 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1_col3 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1_col3 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1_col3 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1_col3 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1_col3 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1_col3 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1_col3 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1_col3 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1_col3 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1_col3 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_PPA_diff_PY1_col3 = dense_rank(desc(PPA_diff_PY1)),
      Rank_SuccessRt_diff_PY1_col3 = dense_rank(desc(SuccessRt_diff_PY1)),
      Rank_HavocRt_diff_PY1_col3 = dense_rank(desc(HavocRt_diff_PY1)),
      Rank_Explosiveness_diff_PY1_col3 = dense_rank(desc(Explosiveness_diff_PY1)),
      Rank_Talent_PY1_col3 = dense_rank(desc(talent_PY1)),
      ## Ranking current stats
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
      ## Extra weighted variables for current year
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col2 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else if (as.numeric(week) <= 5) {
  ##### Weeks 4-5 Variable Ranks #####
  # PY2 weighted 1x, PY1 weighted 1x, current weighted 2x
  ## PY2 ranks
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Comp_Pct_PY2 = dense_rank(desc(completion_pct_PY2)),
           Rank_Pass_YPA_PY2 = dense_rank(desc(pass_ypa_PY2)),
           Rank_Pass_YPR_PY2 = dense_rank(desc(pass_ypr_PY2)),
           Rank_Int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_Rush_YPC_PY2 = dense_rank(desc(rush_ypc_PY2)),
           Rank_Turnovers_pg_PY2 = dense_rank(turnovers_pg_PY2),
           Rank_Third_Conv_Rate_PY2 = dense_rank(desc(third_conv_rate_PY2)),
           Rank_Fourth_Conv_Rate_PY2 = dense_rank(desc(fourth_conv_rate_PY2)),
           Rank_Penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           Rank_Yds_Per_Penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_Kick_Return_Avg_PY2 = dense_rank(desc(kick_return_avg_PY2)),
           Rank_Punt_Return_Avg_PY2 = dense_rank(desc(punt_return_avg_PY2)),
           Rank_Total_Yds_pg_PY2 = dense_rank(desc(total_yds_pg_PY2)),
           Rank_Pass_Yds_pg_PY2 = dense_rank(desc(pass_yds_pg_PY2)),
           Rank_Rush_Yds_pg_PY2 = dense_rank(desc(rush_yds_pg_PY2)),
           Rank_First_Downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(off_ypp_PY2)),
           Rank_Def_Ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_PPA_PY2 = dense_rank(desc(off_ppa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_PPA_PY2 = dense_rank(desc(off_standard_downs_ppa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_PPA_PY2 = dense_rank(desc(off_passing_downs_ppa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_PPA_PY2 = dense_rank(desc(off_rushing_plays_ppa_PY2)),
      Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rushing_plays_success_rate_PY2)),
      Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rushing_plays_explosiveness_PY2)),
      Rank_Off_Pass_Play_PPA_PY2 = dense_rank(desc(off_passing_plays_ppa_PY2)),
      Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_passing_plays_success_rate_PY2)),
      Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_passing_plays_explosiveness_PY2)),
      Rank_Def_PPA_PY2 = dense_rank(def_ppa_PY2),
      Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
      Rank_Def_Explosiveness_PY2 = dense_rank(def_explosiveness_PY2),
      Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
      Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
      Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
      Rank_Def_Second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
      Rank_Def_Open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
      Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(def_field_pos_avg_predicted_points_PY2),
      Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
      Rank_Def_Havoc_Front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
      Rank_Def_Havoc_DB_PY2 = dense_rank(desc(def_havoc_db_PY2)),
      Rank_Def_Standard_Down_PPA_PY2 = dense_rank(def_standard_downs_ppa_PY2),
      Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
      Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
      Rank_Def_Pass_Down_PPA_PY2 = dense_rank(def_passing_downs_ppa_PY2),
      Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
      Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
      Rank_Def_Rush_Play_PPA_PY2 = dense_rank(def_rushing_plays_ppa_PY2),
      Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rushing_plays_success_rate_PY2),
      Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rushing_plays_explosiveness_PY2),
      Rank_Def_Pass_Play_PPA_PY2 = dense_rank(def_passing_plays_ppa_PY2),
      Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_passing_plays_success_rate_PY2),
      Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_passing_plays_explosiveness_PY2),
      Rank_PPA_diff_PY2 = dense_rank(desc(PPA_diff_PY2)),
      Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
      Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
      Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
      ## PY1 ranks
      Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
      Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
      Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
      Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
      Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
      Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
      Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
      Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
      Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
      Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
      Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
      Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
      Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
      Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
      Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
      Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
      Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
      Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
      Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
      Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
      Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_PPA_diff_PY1 = dense_rank(desc(PPA_diff_PY1)),
      Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
      Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
      Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
      ### Ranking current stats
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
      ## Current stats weighted 2x
      Rank_Comp_Pct_col2 = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA_col2 = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR_col2 = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct_col2 = dense_rank(int_pct),
      Rank_Rush_YPC_col2 = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg_col2 = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate_col2 = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate_col2 = dense_rank(desc(fourth_conv_rate)),
      Rank_Total_Yds_pg_col2 = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg_col2 = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg_col2 = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg_col2 = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds_col2 = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col2 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Off_YPP_col3 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col3 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col3 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col3 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col3 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col3 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col3 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col3 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col3 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col3 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col3 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col3 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col3 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col3 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col3 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col3 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col3 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col3 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col3 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col3 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col3 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col3 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col3 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col3 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col3 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col3 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col3 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col3 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col3 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col3 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col3 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col3 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col3 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col3 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col3 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col3 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col3 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col3 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col3 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col3 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col3 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col3 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col3 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col3 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col3 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col3 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col3 = dense_rank(desc(Explosiveness_diff)))
} else if (as.numeric(week) <= 8) {
  ##### Weeks 6-8 Variable Ranks #####
  # PY1 weighted 1x, current weighted 2x
  VoA_Variables <- VoA_Variables |>
    ## PY1 ranks
    mutate(Rank_Comp_Pct_PY1 = dense_rank(desc(completion_pct_PY1)),
           Rank_Pass_YPA_PY1 = dense_rank(desc(pass_ypa_PY1)),
           Rank_Pass_YPR_PY1 = dense_rank(desc(pass_ypr_PY1)),
           Rank_Int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_Rush_YPC_PY1 = dense_rank(desc(rush_ypc_PY1)),
           Rank_Turnovers_pg_PY1 = dense_rank(turnovers_pg_PY1),
           Rank_Third_Conv_Rate_PY1 = dense_rank(desc(third_conv_rate_PY1)),
           Rank_Fourth_Conv_Rate_PY1 = dense_rank(desc(fourth_conv_rate_PY1)),
           Rank_Penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           Rank_Yds_Per_Penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_Kick_Return_Avg_PY1 = dense_rank(desc(kick_return_avg_PY1)),
           Rank_Punt_Return_Avg_PY1 = dense_rank(desc(punt_return_avg_PY1)),
           Rank_Total_Yds_pg_PY1 = dense_rank(desc(total_yds_pg_PY1)),
           Rank_Pass_Yds_pg_PY1 = dense_rank(desc(pass_yds_pg_PY1)),
           Rank_Rush_Yds_pg_PY1 = dense_rank(desc(rush_yds_pg_PY1)),
           Rank_First_Downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(off_ypp_PY1)),
           Rank_Def_Ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_PPA_PY1 = dense_rank(desc(off_ppa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(off_explosiveness_PY1)),
      Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
      Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
      Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
      Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
      Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
      Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
      Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
      Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
      Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
      Rank_Off_Standard_Down_PPA_PY1 = dense_rank(desc(off_standard_downs_ppa_PY1)),
      Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
      Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
      Rank_Off_Pass_Down_PPA_PY1 = dense_rank(desc(off_passing_downs_ppa_PY1)),
      Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
      Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
      Rank_Off_Rush_Play_PPA_PY1 = dense_rank(desc(off_rushing_plays_ppa_PY1)),
      Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rushing_plays_success_rate_PY1)),
      Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rushing_plays_explosiveness_PY1)),
      Rank_Off_Pass_Play_PPA_PY1 = dense_rank(desc(off_passing_plays_ppa_PY1)),
      Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_passing_plays_success_rate_PY1)),
      Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_passing_plays_explosiveness_PY1)),
      Rank_Def_PPA_PY1 = dense_rank(def_ppa_PY1),
      Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
      Rank_Def_Explosiveness_PY1 = dense_rank(def_explosiveness_PY1),
      Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
      Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
      Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
      Rank_Def_Second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
      Rank_Def_Open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
      Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(def_field_pos_avg_predicted_points_PY1),
      Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
      Rank_Def_Havoc_Front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
      Rank_Def_Havoc_DB_PY1 = dense_rank(desc(def_havoc_db_PY1)),
      Rank_Def_Standard_Down_PPA_PY1 = dense_rank(def_standard_downs_ppa_PY1),
      Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
      Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
      Rank_Def_Pass_Down_PPA_PY1 = dense_rank(def_passing_downs_ppa_PY1),
      Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
      Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
      Rank_Def_Rush_Play_PPA_PY1 = dense_rank(def_rushing_plays_ppa_PY1),
      Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rushing_plays_success_rate_PY1),
      Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rushing_plays_explosiveness_PY1),
      Rank_Def_Pass_Play_PPA_PY1 = dense_rank(def_passing_plays_ppa_PY1),
      Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_passing_plays_success_rate_PY1),
      Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_passing_plays_explosiveness_PY1),
      Rank_PPA_diff_PY1 = dense_rank(desc(PPA_diff_PY1)),
      Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
      Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
      Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
      Rank_Recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
      ## Ranking current stats
      Rank_Comp_Pct = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
      Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
      Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
      Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA = dense_rank(def_ppa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
      ## Current stats weighted 2x
      Rank_Comp_Pct_col2 = dense_rank(desc(completion_pct)),
      Rank_Pass_YPA_col2 = dense_rank(desc(pass_ypa)),
      Rank_Pass_YPR_col2 = dense_rank(desc(pass_ypr)),
      Rank_Int_Pct_col2 = dense_rank(int_pct),
      Rank_Rush_YPC_col2 = dense_rank(desc(rush_ypc)),
      Rank_Turnovers_pg_col2 = dense_rank(turnovers_pg),
      Rank_Third_Conv_Rate_col2 = dense_rank(desc(third_conv_rate)),
      Rank_Fourth_Conv_Rate_col2 = dense_rank(desc(fourth_conv_rate)),
      Rank_Total_Yds_pg_col2 = dense_rank(desc(total_yds_pg)),
      Rank_Pass_Yds_pg_col2 = dense_rank(desc(pass_yds_pg)),
      Rank_Rush_Yds_pg_col2 = dense_rank(desc(rush_yds_pg)),
      Rank_First_Downs_pg_col2 = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Def_Ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
      Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
      Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
      Rank_Def_Second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
      Rank_Def_Open_Field_Yds_col2 = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(def_field_pos_avg_predicted_points),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col2 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Off_YPP_col3 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col3 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col3 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col3 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col3 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col3 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col3 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col3 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col3 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col3 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col3 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col3 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col3 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col3 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col3 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col3 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col3 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col3 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col3 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col3 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col3 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col3 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col3 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col3 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col3 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col3 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col3 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col3 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col3 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col3 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col3 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col3 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col3 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col3 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col3 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col3 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col3 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col3 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col3 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col3 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col3 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col3 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col3 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col3 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col3 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col3 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col3 = dense_rank(desc(Explosiveness_diff)))
} else {
  ##### Week 9-End of Season Variable Ranks #####
  ## Recruiting points no longer included
  # current will be only data source used, everything weighted "1x" (aside from special variables, and recruiting)
  ## Ranking current stats
  VoA_Variables <- VoA_Variables |>
    mutate(Rank_Comp_Pct = dense_rank(desc(completion_pct)),
           Rank_Pass_YPA = dense_rank(desc(pass_ypa)),
           Rank_Pass_YPR = dense_rank(desc(pass_ypr)),
           Rank_Int_Pct = dense_rank(int_pct),
           Rank_Rush_YPC = dense_rank(desc(rush_ypc)),
           Rank_Turnovers_pg = dense_rank(turnovers_pg),
           Rank_Third_Conv_Rate = dense_rank(desc(third_conv_rate)),
           Rank_Fourth_Conv_Rate = dense_rank(desc(fourth_conv_rate)),
           Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
           Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
           Rank_Kick_Return_Avg = dense_rank(desc(kick_return_avg)),
           Rank_Punt_Return_Avg = dense_rank(desc(punt_return_avg)),
           Rank_Total_Yds_pg = dense_rank(desc(total_yds_pg)),
           Rank_Pass_Yds_pg = dense_rank(desc(pass_yds_pg)),
           Rank_Rush_Yds_pg = dense_rank(desc(rush_yds_pg)),
           Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(off_ypp)),
           Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_PPA = dense_rank(desc(off_ppa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_PPA = dense_rank(desc(off_standard_downs_ppa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_PPA = dense_rank(desc(off_passing_downs_ppa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_PPA = dense_rank(desc(off_rushing_plays_ppa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rushing_plays_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rushing_plays_explosiveness)),
           Rank_Off_Pass_Play_PPA = dense_rank(desc(off_passing_plays_ppa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_passing_plays_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_passing_plays_explosiveness)),
           Rank_Def_PPA = dense_rank(def_ppa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Field_Pos_Avg_Predicted_Pts = dense_rank(def_field_pos_avg_predicted_points),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           Rank_Def_Havoc_Front_Seven = dense_rank(desc(def_havoc_front_seven)),
           Rank_Def_Havoc_DB = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_PPA = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
      Rank_adj_off_ppa = dense_rank(desc(adj_off_ppa)),
      Rank_adj_def_ppa = dense_rank(adj_def_ppa),
      Rank_adj_ppa_diff = dense_rank(desc(adj_ppa_diff)),
      Rank_adj_off_explosiveness = dense_rank(desc(adj_off_explosiveness)),
      Rank_adj_def_explosiveness = dense_rank(adj_def_explosiveness),
      Rank_adj_off_ypp = dense_rank(desc(adj_off_ypp)),
      Rank_adj_def_ypp = dense_rank(adj_def_ypp),
      Rank_adj_off_ppg = dense_rank(desc(adj_off_ppg)),
      Rank_adj_def_ppg = dense_rank(adj_def_ppg),
      Rank_adj_st_ppa = dense_rank(desc(adj_st_ppa)),
      Rank_adj_st_ppa_allowed = dense_rank(adj_st_ppa_allowed),
      Rank_net_adj_st_ppa = dense_rank(desc(net_adj_st_ppa)),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Off_YPP_col2 = dense_rank(desc(off_ypp)),
      Rank_Off_PPA_col2 = dense_rank(desc(off_ppa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
      Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
      Rank_Off_Standard_Down_PPA_col2 = dense_rank(desc(off_standard_downs_ppa)),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
      Rank_Off_Pass_Down_PPA_col2 = dense_rank(desc(off_passing_downs_ppa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
      Rank_Off_Rush_Play_PPA_col2 = dense_rank(desc(off_rushing_plays_ppa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rushing_plays_success_rate)),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rushing_plays_explosiveness)),
      Rank_Off_Pass_Play_PPA_col2 = dense_rank(desc(off_passing_plays_ppa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_passing_plays_success_rate)),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_passing_plays_explosiveness)),
      Rank_Def_PPA_col2 = dense_rank(def_ppa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Havoc_Front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
      Rank_Def_Havoc_DB_col2 = dense_rank(desc(def_havoc_db)),
      Rank_Def_Standard_Down_PPA_col2 = dense_rank(def_standard_downs_ppa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
      Rank_Def_Pass_Down_PPA_col2 = dense_rank(def_passing_downs_ppa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
      Rank_Def_Rush_Play_PPA_col2 = dense_rank(def_rushing_plays_ppa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rushing_plays_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rushing_plays_explosiveness),
      Rank_Def_Pass_Play_PPA_col2 = dense_rank(def_passing_plays_ppa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_passing_plays_success_rate),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_passing_plays_explosiveness),
      Rank_PPA_diff_col2 = dense_rank(desc(PPA_diff)),
      Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
      Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
      Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
}
### end of if statements





##### calculating the mean stat ranking, VoA_Output #####
if (as.numeric(week) == 0) {
  ## correcting "season" column to reflect the season for which these rankings are being produced
  VoA_Variables$season = rep(as.numeric(year), nrow(VoA_Variables))
  ### Append new column of Model output, which is the mean of all rank columns
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else if (as.numeric(week) <= 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
} else if (as.numeric(week) <= 8) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

##### Using Intial VoA Outputs to add in conference strength metric #####
Conference_Outputs <- VoA_Variables |>
  group_by(conference) |>
  summarize(Rk_mean = mean(VoA_Output), Rk_median = median(VoA_Output)) |>
  mutate(Conf_Rk = dense_rank(Rk_mean))

VoA_Variables <- VoA_Variables |>
  select(-VoA_Output) |>
  mutate(Conference_Strength = case_when(conference == "ACC" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "ACC"],
                                         conference == "American Athletic" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "American Athletic"],
                                         conference == "Big 12" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Big 12"],
                                         conference == "Big Ten" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Big Ten"],
                                         conference == "Conference USA" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Conference USA"],
                                         conference == "FBS Independents" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "FBS Independents"],
                                         conference == "Mid-American" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Mid-American"],
                                         conference == "Mountain West" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Mountain West"],
                                         conference == "Pac-12" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Pac-12"],
                                         conference == "SEC" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "SEC"],
                                         conference == "Sun Belt" ~ Conference_Outputs$Rk_mean[Conference_Outputs$conference == "Sun Belt"],)) |>
  mutate(Conference_Strength_col2 = Conference_Strength,
         Conference_Strength_col3 = Conference_Strength,
         Conference_Strength_col4 = Conference_Strength,
         Conference_Strength_col5 = Conference_Strength,
         Conference_Strength_col6 = Conference_Strength,
         Conference_Strength_col7 = Conference_Strength,
         Conference_Strength_col8 = Conference_Strength,
         Conference_Strength_col9 = Conference_Strength,
         Conference_Strength_col10 = Conference_Strength)

##### Re running rowMeans function to get VoA Output #####
### script wouldn't run properly without a real number in the later weeks so I'll have to come back and edit the number in during the season as I figure out how big VoA_Variables gets
if (as.numeric(week) == 0) {
  ### Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
} else if (as.numeric(week) == 1) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
} else if (as.numeric(week) <= 5) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
} else if (as.numeric(week) <= 8) {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoA_Variables <- VoA_Variables |>
    mutate(VoA_Output = (rowMeans(VoA_Variables[,VoA_Ncols:ncol(VoA_Variables)])))
  ## Append column of VoA Final Rankings
  # VoA_Variables <- VoA_Variables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement


##### using Stan to create FPI/SP+ like metrics #####
if (as.numeric(week) <= 8) {
  ##### Week 0-8 Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(N = nrow(VoA_Variables), off_ppg = VoA_Variables$weighted_off_ppg_mean, off_ppa = VoA_Variables$weighted_off_ppa, off_ypp = VoA_Variables$weighted_off_ypp, off_success_rate = VoA_Variables$weighted_off_success_rate, off_explosiveness = VoA_Variables$weighted_off_explosiveness, third_conv_rate = VoA_Variables$weighted_third_conv_rate, off_pts_per_opp = VoA_Variables$weighted_off_pts_per_opp, off_plays_pg = VoA_Variables$weighted_off_plays_pg, VoA_Output = 1/VoA_Variables$VoA_Output, Conference_Strength = 1/VoA_Variables$Conference_Strength)
  
  ### compile the stan model
  Off_VoA_model <- cmdstan_model(stan_file = here("Scripts", "Stan", "Off_VoA.stan"))
  ### fitting stan model
  set.seed(802)
  Off_VoA_fit <- Off_VoA_model$sample(data = Off_VoA_datalist, chains = 3, iter_sampling = 10000, iter_warmup = 5000, seed = 802)
  Off_VoA_fit
  
  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())
  
  
  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(variables = c("b0", "beta_off_ppa", "beta_off_ypp", "beta_off_success_rate", "beta_off_explosiveness", "beta_third_conv_rate", "beta_off_pts_per_opp", "beta_off_plays_pg", "beta_VoA_Output", "beta_Conference_Strength", "sigma"), format = "draws_df")
  
  ### creating matrix to hold ratings
  Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Off_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Off_VoA_Rating <- rnorm(1, mean = Off_VoA_pars$b0[p] + Off_VoA_pars$beta_off_ppa[p] * VoA_Variables$weighted_off_ppa[t] + Off_VoA_pars$beta_off_ypp[p] * VoA_Variables$weighted_off_ypp[t] + Off_VoA_pars$beta_off_success_rate[p] * VoA_Variables$weighted_off_success_rate[t] + Off_VoA_pars$beta_off_explosiveness[p] * VoA_Variables$weighted_off_explosiveness[t] + Off_VoA_pars$beta_third_conv_rate[p] * VoA_Variables$weighted_third_conv_rate[t] + Off_VoA_pars$beta_off_pts_per_opp[p] * VoA_Variables$weighted_off_pts_per_opp[t] + Off_VoA_pars$beta_off_plays_pg[p] * VoA_Variables$weighted_off_plays_pg[t] + Off_VoA_pars$beta_VoA_Output[p] * (1/VoA_Variables$VoA_Output[t]) + Off_VoA_pars$beta_Conference_Strength[p] * (1/VoA_Variables$Conference_Strength[t]), sd = Off_VoA_pars$sigma[p])
      Off_VoA_Ratings[p,t] <- Off_VoA_Rating
    }
  }
  
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings,2,mean)
  MedianPred <- apply(Off_VoA_Ratings,2,median)
  Upper <- apply(Off_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(Off_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$OffVoA_MeanRating <- MeanPred
  VoA_Variables$OffVoA_MedRating <- MedianPred
  VoA_Variables$OffVoA_95PctRating <- Upper
  VoA_Variables$OffVoA_05PctRating <- Lower
  
  
  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  Def_VoA_datalist <- list(N = nrow(VoA_Variables), def_ppg = VoA_Variables$weighted_def_ppg_mean, def_ppa = VoA_Variables$weighted_def_ppa, def_ypp = VoA_Variables$weighted_def_ypp, def_success_rate = VoA_Variables$weighted_def_success_rate, def_explosiveness = VoA_Variables$weighted_def_explosiveness, def_third_conv_rate = VoA_Variables$weighted_def_third_conv_rate, def_pts_per_opp = VoA_Variables$weighted_def_pts_per_opp, def_havoc_total = VoA_Variables$weighted_def_havoc_total, def_plays_pg = VoA_Variables$weighted_def_plays_pg, VoA_Output = VoA_Variables$VoA_Output, Conference_Strength = VoA_Variables$Conference_Strength)
  
  ### compile the stan model
  Def_VoA_model <- cmdstan_model(stan_file = here("Scripts","Stan", "Def_VoA.stan"))
  ### fitting stan model
  set.seed(802)
  Def_VoA_fit <- Def_VoA_model$sample(data = Def_VoA_datalist, chains = 3, iter_sampling = 10000, iter_warmup = 5000, seed = 802)
  Def_VoA_fit
  
  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())
  
  
  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(variables = c("b0", "beta_def_ppa", "beta_def_ypp", "beta_def_success_rate", "beta_def_explosiveness", "beta_def_third_conv_rate", "beta_def_pts_per_opp", "beta_def_havoc_total", "beta_def_plays_pg", "beta_VoA_Output", "beta_Conference_Strength", "sigma"), format = "draws_df")
  
  ### creating matrix to hold ratings
  Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Def_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Def_VoA_Rating <- rnorm(1, mean = Def_VoA_pars$b0[p] + Def_VoA_pars$beta_def_ppa[p] * VoA_Variables$weighted_def_ppa[t] + Def_VoA_pars$beta_def_ypp[p] * VoA_Variables$weighted_def_ypp[t] + Def_VoA_pars$beta_def_success_rate[p] * VoA_Variables$weighted_def_success_rate[t] + Def_VoA_pars$beta_def_explosiveness[p] * VoA_Variables$weighted_def_explosiveness[t] + Def_VoA_pars$beta_def_third_conv_rate[p] * VoA_Variables$weighted_def_third_conv_rate[t] + Def_VoA_pars$beta_def_pts_per_opp[p] * VoA_Variables$weighted_def_pts_per_opp[t] + Def_VoA_pars$beta_def_havoc_total[p] * VoA_Variables$weighted_def_havoc_total[t] + Def_VoA_pars$beta_def_plays_pg[p] * VoA_Variables$weighted_def_plays_pg[t] + Def_VoA_pars$beta_VoA_Output[p] * VoA_Variables$VoA_Output[t] + Def_VoA_pars$beta_Conference_Strength[p] * VoA_Variables$Conference_Strength[t], sd = Def_VoA_pars$sigma[p])
      Def_VoA_Ratings[p,t] <- Def_VoA_Rating
    }
  }
  
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings,2,mean)
  MedianPred <- apply(Def_VoA_Ratings,2,median)
  Upper <- apply(Def_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(Def_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$DefVoA_MeanRating <- MeanPred
  VoA_Variables$DefVoA_MedRating <- MedianPred
  VoA_Variables$DefVoA_95PctRating <- Upper
  VoA_Variables$DefVoA_05PctRating <- Lower
  
  
  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  ST_VoA_datalist <- list(N = nrow(VoA_Variables), net_st_ppg = VoA_Variables$weighted_net_st_ppg_mean, net_kick_return_avg = VoA_Variables$weighted_net_kick_return_avg, net_punt_return_avg = VoA_Variables$weighted_net_punt_return_avg, net_fg_rate = VoA_Variables$weighted_net_fg_rate, net_st_ppa = VoA_Variables$weighted_net_adj_st_ppa)
  
  ### compile the stan model
  ST_VoA_model <- cmdstan_model(stan_file = here("Scripts", "Stan", "ST_VoA.stan"))
  ### fitting special teams stan model
  set.seed(802)
  ST_VoA_fit <- ST_VoA_model$sample(data = ST_VoA_datalist, chains = 3, iter_sampling = 5000, iter_warmup = 2500, seed = 802)
  ST_VoA_fit
  
  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())
  
  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(variables = c("b0", "beta_net_kick_return_avg", "beta_net_punt_return_avg", "beta_net_fg_rate", "beta_net_st_ppa", "sigma"), format = "draws_df")
  
  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(NA, nrow = length(ST_VoA_pars$b0), ncol = nrow(VoA_Variables))
  
  ### creating special teams VoA_Ratings
  set.seed(802)
  for (p in 1:length(ST_VoA_pars$b0)){
    for (t in 1:nrow(VoA_Variables)){
      ST_VoA_Rating <- rnorm(1, mean = ST_VoA_pars$b0[p] + ST_VoA_pars$beta_net_kick_return_avg[p] * VoA_Variables$weighted_net_kick_return_avg[t] + ST_VoA_pars$beta_net_punt_return_avg[p] * VoA_Variables$weighted_net_punt_return_avg[t] + ST_VoA_pars$beta_net_fg_rate[p] * VoA_Variables$weighted_net_fg_rate[t] + ST_VoA_pars$beta_net_st_ppa[p] * VoA_Variables$weighted_net_adj_st_ppa[t], sd = ST_VoA_pars$sigma[p])
      ST_VoA_Ratings[p,t] <- ST_VoA_Rating
    }
  }
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings,2,mean)
  MedianPred <- apply(ST_VoA_Ratings,2,median)
  Upper <- apply(ST_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(ST_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$STVoA_MeanRating <- MeanPred
  VoA_Variables$STVoA_MedRating <- MedianPred
  VoA_Variables$STVoA_95PctRating <- Upper
  VoA_Variables$STVoA_05PctRating <- Lower
} else {
  ##### Weeks 9-End of Season Stan Models, current season data only #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(N = nrow(VoA_Variables), off_ppg = VoA_Variables$adj_off_ppg, off_ppa = VoA_Variables$adj_off_ppa, off_ypp = VoA_Variables$adj_off_ypp, off_success_rate = VoA_Variables$off_success_rate, off_explosiveness = VoA_Variables$adj_off_explosiveness, third_conv_rate = VoA_Variables$third_conv_rate, off_pts_per_opp = VoA_Variables$off_pts_per_opp, off_plays_pg = VoA_Variables$off_plays_pg, VoA_Output = 1/VoA_Variables$VoA_Output, Conference_Strength = 1/VoA_Variables$Conference_Strength)
  
  ### compile the stan model
  Off_VoA_model <- cmdstan_model(stan_file = here("Scripts","Stan", "Off_VoA.stan"))
  ### fitting stan model
  set.seed(802)
  Off_VoA_fit <- Off_VoA_model$sample(data = Off_VoA_datalist, chains = 3, iter_sampling = 10000, iter_warmup = 2500, seed = 802)
  Off_VoA_fit
  
  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())
  
  
  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(variables = c("b0", "beta_off_ppa", "beta_off_ypp", "beta_off_success_rate", "beta_off_explosiveness", "beta_third_conv_rate", "beta_off_pts_per_opp", "beta_off_plays_pg", "beta_VoA_Output", "beta_Conference_Strength", "sigma"), format = "draws_df")
  
  ### creating matrix to hold ratings
  Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Off_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Off_VoA_Rating <- rnorm(1, mean = Off_VoA_pars$b0[p] + Off_VoA_pars$beta_off_ppa[p] * VoA_Variables$adj_off_ppa[t] + Off_VoA_pars$beta_off_ypp[p] * VoA_Variables$adj_off_ypp[t] + Off_VoA_pars$beta_off_success_rate[p] * VoA_Variables$off_success_rate[t] + Off_VoA_pars$beta_off_explosiveness[p] * VoA_Variables$adj_off_explosiveness[t] + Off_VoA_pars$beta_third_conv_rate[p] * VoA_Variables$third_conv_rate[t] + Off_VoA_pars$beta_off_pts_per_opp[p] * VoA_Variables$off_pts_per_opp[t] + Off_VoA_pars$beta_off_plays_pg[p] * VoA_Variables$off_plays_pg[t] + Off_VoA_pars$beta_VoA_Output[p] * (1/VoA_Variables$VoA_Output[t]) + Off_VoA_pars$beta_Conference_Strength[p] * (1/VoA_Variables$Conference_Strength[t]) , sd = Off_VoA_pars$sigma[p])
      Off_VoA_Ratings[p,t] <- Off_VoA_Rating
    }
  }
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings, 2, mean)
  MedianPred <- apply(Off_VoA_Ratings, 2, median)
  Upper <- apply(Off_VoA_Ratings, 2, quantile, prob=.95)
  Lower <- apply(Off_VoA_Ratings, 2, quantile, prob=.05)
  ### assigning ratings to columns in VoA Variables
  VoA_Variables$OffVoA_MeanRating <- MeanPred
  VoA_Variables$OffVoA_MedRating <- MedianPred
  VoA_Variables$OffVoA_95PctRating <- Upper
  VoA_Variables$OffVoA_05PctRating <- Lower
  
  
  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  Def_VoA_datalist <- list(N = nrow(VoA_Variables), def_ppg = VoA_Variables$adj_def_ppg, def_ppa = VoA_Variables$adj_def_ppa, def_ypp = VoA_Variables$adj_def_ypp, def_success_rate = VoA_Variables$def_success_rate, def_explosiveness = VoA_Variables$adj_def_explosiveness, def_third_conv_rate = VoA_Variables$def_third_conv_rate, def_pts_per_opp = VoA_Variables$def_pts_per_opp, def_havoc_total = VoA_Variables$def_havoc_total, def_plays_pg = VoA_Variables$def_plays_pg, VoA_Output = VoA_Variables$VoA_Output, Conference_Strength = VoA_Variables$Conference_Strength)
  
  ### compile the stan model
  Def_VoA_model <- cmdstan_model(stan_file = here("Scripts","Stan", "Def_VoA.stan"))
  ### fitting stan model
  set.seed(802)
  Def_VoA_fit <- Def_VoA_model$sample(data = Def_VoA_datalist, chains = 3, iter_sampling = 10000, iter_warmup = 2500, seed = 802)
  # Def_VoA_fit
  
  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())
  
  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(variables = c("b0", "beta_def_ppa", "beta_def_ypp", "beta_def_success_rate", "beta_def_explosiveness", "beta_def_third_conv_rate", "beta_def_pts_per_opp", "beta_def_havoc_total", "beta_def_plays_pg", "beta_VoA_Output", "beta_Conference_Strength", "sigma"), format = "list")
  
  ### creating matrix to hold ratings
  ### adding in process uncertainty
  Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoA_Variables))
  
  ### creating ratings
  set.seed(802)
  for (p in 1:length(Def_VoA_pars$b0)){
    for(t in 1:nrow(VoA_Variables)){
      Def_VoA_Rating <- rnorm(1, mean = Def_VoA_pars$b0[p] + Def_VoA_pars$beta_def_ppa[p] * VoA_Variables$adj_def_ppa[t] + Def_VoA_pars$beta_def_ypp[p] * VoA_Variables$adj_def_ypp[t] + Def_VoA_pars$beta_def_success_rate[p] * VoA_Variables$def_success_rate[t] + Def_VoA_pars$beta_def_explosiveness[p] * VoA_Variables$adj_def_explosiveness[t] + Def_VoA_pars$beta_def_third_conv_rate[p] * VoA_Variables$def_third_conv_rate[t] + Def_VoA_pars$beta_def_pts_per_opp[p] * VoA_Variables$def_pts_per_opp[t] + Def_VoA_pars$beta_def_havoc_total[p] * VoA_Variables$def_havoc_total[t]  + Def_VoA_pars$beta_def_plays_pg[p] * VoA_Variables$def_plays_pg[t] + Def_VoA_pars$beta_VoA_Output[p] * VoA_Variables$VoA_Output[t] + Def_VoA_pars$beta_Conference_Strength[p] * VoA_Variables$Conference_Strength[t], sd = Def_VoA_pars$sigma[p])
      Def_VoA_Ratings[p,t] <- Def_VoA_Rating
    }
  }
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings,2,mean)
  MedianPred <- apply(Def_VoA_Ratings,2,median)
  Upper <- apply(Def_VoA_Ratings,2,quantile, prob=.95)
  Lower <- apply(Def_VoA_Ratings,2,quantile, prob=.05)
  
  VoA_Variables$DefVoA_MeanRating <- MeanPred
  VoA_Variables$DefVoA_MedRating <- MedianPred
  VoA_Variables$DefVoA_95PctRating <- Upper
  VoA_Variables$DefVoA_05PctRating <- Lower
  
  
  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  ST_VoA_datalist <- list(N = nrow(VoA_Variables), net_st_ppg = VoA_Variables$net_st_ppg, net_kick_return_avg = VoA_Variables$net_kick_return_avg, net_punt_return_avg = VoA_Variables$net_punt_return_avg, net_fg_rate = VoA_Variables$net_fg_rate, net_st_ppa = VoA_Variables$net_adj_st_ppa)
  
  ### compile the stan model
  ST_VoA_model <- cmdstan_model(stan_file = here("Scripts", "Stan", "ST_VoA.stan"))
  ### fitting special teams stan model
  set.seed(802)
  ST_VoA_fit <- ST_VoA_model$sample(data = ST_VoA_datalist, chains = 3, iter_sampling = 5000, iter_warmup = 2500, seed = 802)
  # ST_VoA_fit
  
  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())
  
  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(variables = c("b0", "beta_net_kick_return_avg", "beta_net_punt_return_avg", "beta_net_fg_rate", "beta_net_st_ppa", "sigma"), format = "list")
  
  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(NA, nrow = length(ST_VoA_pars$b0), ncol = nrow(VoA_Variables))
  
  ### creating special teams VoA_Ratings
  set.seed(802)
  for (p in 1:length(ST_VoA_pars$b0)){
    for (t in 1:nrow(VoA_Variables)){
      ST_VoA_Rating <- rnorm(1, mean = ST_VoA_pars$b0[p] + ST_VoA_pars$beta_net_kick_return_avg[p] * VoA_Variables$net_kick_return_avg[t] + ST_VoA_pars$beta_net_punt_return_avg[p] * VoA_Variables$net_punt_return_avg[t] + ST_VoA_pars$beta_net_fg_rate[p] * VoA_Variables$net_fg_rate[t] + ST_VoA_pars$beta_net_st_ppa[p] * VoA_Variables$net_adj_st_ppa[t], sd = ST_VoA_pars$sigma[p])
      ST_VoA_Ratings[p,t] <- ST_VoA_Rating
    }
  }
  
  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings, 2, mean)
  MedianPred <- apply(ST_VoA_Ratings, 2, median)
  Upper <- apply(ST_VoA_Ratings, 2, quantile, prob=.95)
  Lower <- apply(ST_VoA_Ratings, 2, quantile, prob=.05)
  
  VoA_Variables$STVoA_MeanRating <- MeanPred
  VoA_Variables$STVoA_MedRating <- MedianPred
  VoA_Variables$STVoA_95PctRating <- Upper
  VoA_Variables$STVoA_05PctRating <- Lower
}

### making sure all values are > 0
for (i in 1:nrow(VoA_Variables)){
  set.seed(802)
  if (VoA_Variables$OffVoA_MedRating[i] <= 0){
    VoA_Variables$OffVoA_MedRating[i] = abs(VoA_Variables$OffVoA_MedRating[i]) + abs(rnorm(1, 3, 1))
  }
  if (VoA_Variables$DefVoA_MedRating[i] <= 0){
    VoA_Variables$DefVoA_MedRating[i] = abs(VoA_Variables$DefVoA_MedRating[i]) + abs(rnorm(1, 3, 1))
  }
}

##### Ranking VoA Rating columns #####
VoA_Variables <- VoA_Variables |>
  mutate(VoA_Rating_Ovr = OffVoA_MedRating - DefVoA_MedRating + STVoA_MedRating,
         VoA_Rating_05Pct = OffVoA_05PctRating - DefVoA_05PctRating + STVoA_05PctRating,
         VoA_Rating_95Pct = OffVoA_95PctRating - DefVoA_95PctRating + STVoA_95PctRating,
         VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr)),
         OffVoA_Ranking = dense_rank(desc(OffVoA_MedRating)),
         DefVoA_Ranking = dense_rank(DefVoA_MedRating),
         STVoA_Ranking = dense_rank(desc(STVoA_MedRating)))

### creating data frame with just team, VoA ratings, VoA Rankings, and VoA output
FinalTable <- VoA_Variables |>
  select(team, conference, CFB_Week, VoA_Output, VoA_Rating_Ovr, VoA_Ranking_Ovr, OffVoA_MedRating, OffVoA_Ranking, DefVoA_MedRating, DefVoA_Ranking, STVoA_MedRating, STVoA_Ranking, Conference_Strength) |>
  arrange(VoA_Ranking_Ovr)
### separating out top 25
FinalVoATop25 <- FinalTable |> 
  filter(VoA_Ranking_Ovr < 26)

##### Creating Top 25 and Full Tables Arranged by VoA Rating #####
if (as.numeric(week) == 0) {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, preseason_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr),
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 134 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, preseason_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating_Ovr") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else if (as.numeric(week) > 15) {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr),
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 134 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating_Ovr") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else {
  ## Top 25 Table
  # adding title and subtitle
  VoATop25Table <- FinalVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr),
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ## Full 134 teams table
  # adding title and subtitle
  VoA_Full_Table <- FinalTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # A column (numeric data)
      columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number( # Another column (also numeric data)
      columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(OffVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(DefVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = TRUE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(STVoA_MedRating), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(VoA_Rating_Ovr = "Overall VoA Rating", VoA_Ranking_Ovr = "VoA Ranking", OffVoA_MedRating = "Off VoA Rating", OffVoA_Ranking = "Off Ranking", DefVoA_MedRating = "Def VoA Rating", DefVoA_Ranking = "Def Ranking", STVoA_MedRating = "ST VoA Rating", STVoA_Ranking = "ST Ranking") |> # Update labels
    # cols_move_to_end(columns = "VoA_Rating") |>
    cols_hide(c(conference, CFB_Week, VoA_Output, Conference_Strength)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
}

##### Resume VoA #####
### determining mean VoA Rating of top 12 teams in VoA, comparing how this hypothetical average top 12 team would do given each team's schedule
## choosing top 12 because of future playoff expansion which seems likely if not already certain
## it really should only be 8 max but whatever, I'm gonna be just fine
### Resume VoA only created after week 9 (Week 10 - end of season)
if (as.numeric(week) > 9) {
  ### adding column to VoA_Variables to hold Resume VoA metric
  ## placing dummy value for now, will be filled in for loop further down
  VoA_Variables <- VoA_Variables |>
    mutate(Resume_VoA = -999)
  
  ### calculating top 12 average since 12 teams make the playoff
  Top12 <- VoA_Variables |>
    filter(VoA_Ranking_Ovr <= 12) |>
    select(season, team, OffVoA_MedRating, DefVoA_MedRating, STVoA_MedRating)
  Top12_off_mean <- mean(Top12$OffVoA_MedRating)
  Top12_def_mean <- mean(Top12$DefVoA_MedRating)
  Top12_st_mean <- mean(Top12$STVoA_MedRating)
  Top12_mean <- Top12_off_mean - Top12_def_mean + Top12_st_mean
  
  
  ### pulling in completed games
  completed_games <- cfbd_game_info(as.numeric(year)) |>
    filter(home_team %in% VoA_Variables$team | away_team %in% VoA_Variables$team) |>
    select(game_id, season, week, neutral_site, completed, home_team, home_points, away_team, away_points) |>
    filter(completed == TRUE)
  
  ### using SRS ratings for FCS teams instead of the randomly sampled VoA rating based on
  ## bottom half of VoA ratings as done during 2022 CFB season
  FCS <- cfbd_ratings_srs(year = as.numeric(year)) |>
    filter(team %nin% VoA_Variables$team) |>
    filter(team %in% completed_games$home_team | team %in% completed_games$away_team)
  
  ##### Calculating Resume VoA team by team #####
  for (school in 1:nrow(VoA_Variables)){
    temp_team <- completed_games |>
      filter(home_team == VoA_Variables$team[school] | away_team == VoA_Variables$team[school]) |>
      mutate(team = VoA_Variables$team[school],
             team_opp = case_when(home_team == VoA_Variables$team[school] ~ away_team,
                                  TRUE ~ home_team),
             team_VoA_rating = VoA_Variables$VoA_Rating_Ovr[school])
    ### extracting ratings of FBS opponents
    temp_teamFBSOpps <- VoA_Variables |>
      filter(team %in% temp_team$team_opp) |>
      select(team, VoA_Rating_Ovr)
    ### extracting SRS ratings of FCS opponents
    temp_teamFCSOpps <- FCS |>
      filter(team %in% temp_team$team_opp) |>
      select(team, rating)
    colnames(temp_teamFCSOpps) <- c("team", "VoA_Rating_Ovr")
    temp_teamOpps <- rbind(temp_teamFBSOpps, temp_teamFCSOpps)
    colnames(temp_teamOpps) <- c("team_opp", "opp_VoA_rating")
    
    ### adding opponent ratings to main team df
    temp_team <- full_join(temp_team, temp_teamOpps, by = "team_opp")
    
    ### calculating resume score
    temp_team <- temp_team |>
      mutate(actual_diff = case_when(home_team == VoA_Variables$team[school] ~ home_points - away_points,
                                     TRUE ~ away_points - home_points),
             projected_diff = case_when(home_team == VoA_Variables$team[school] & neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
                                        away_team == VoA_Variables$team[school] & neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
                                        TRUE ~ team_VoA_rating - opp_VoA_rating),
             Top12_proj = case_when(home_team == VoA_Variables$team[school] & neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
                                    TRUE ~ Top12_mean - opp_VoA_rating),
             Resume_Score = actual_diff - Top12_proj)
    
    ## determining number of losses
    temp_team_losses <- temp_team |>
      filter(home_team == VoA_Variables$team[school] & home_points < away_points | away_team == VoA_Variables$team[school] & away_points < home_points)
    ## storing overall team Resume Score as vector
    VoA_Variables$Resume_VoA[school] <- sum(temp_team$Resume_Score) - (7 * nrow(temp_team_losses))
  }
  
  VoA_Variables <- VoA_Variables |>
    mutate(Resume_VoA_Rank = dense_rank(desc(Resume_VoA)))
  
  
  ### filtering resume top 25 out for table
  ResumeVoATop25 <- VoA_Variables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    filter(Resume_VoA_Rank < 26) |>
    arrange(Resume_VoA_Rank)
  
  ### full resume VoA, simplified for table
  FinalResumeTable <- VoA_Variables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    arrange(Resume_VoA_Rank)
} else {
  print("no Resume VoA until Week 10!")
}

##### Creating Top 25 and Full Tables Arranged by Resume VoA #####
if (as.numeric(week) > 9) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ### Full 134 teams table
  # adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, week, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Rank") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else if (as.numeric(week) > 15) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Ranking") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
  
  ### Full 134 teams table
  ### adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy")  |>  # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number( # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(Resume_VoA = "Resume VoA Rating", Resume_VoA_Rank = "Resume VoA Rank") |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
    )
} else {
  print("No Resume VoA until Week 10!")
}


##### Saving tables and final VoA_Variables csv #####
### viewing and saving the gt tables outside the if statement so that I can see them in the RStudio viewer
VoATop25Table
VoATop25Table |>
  gtsave(
    top25_file_pathway, expand = 5,
    path = output_dir
  )
VoA_Full_Table
VoA_Full_Table |>
  gtsave(
    fulltable_file_pathway, expand = 5,
    path = output_dir
  )

## Resume VoA not produced until Week 10
if (as.numeric(week) > 9) {
  ### Resume tables
  ResumeVoATop25Table
  ResumeVoATop25Table |>
    gtsave(
      resumetop25_file_pathway, expand = 5,
      path = output_dir
    )
  Resume_VoA_Table
  Resume_VoA_Table |>
    gtsave(
      resumefulltable_file_pathway, expand = 5,
      path = output_dir
    )
} else{
  print("No Resume VoA tables until week 10!")
}

## Exporting final dataframe as csv
write_csv(VoA_Variables, file_pathway)

##### Setting up the Unintelligible Charts #####
### Tracks VoA Ratings and Rankings by week
### now reading in and merging VoA rating and ranking data up to current week
### changing FinalTable to only be columns needed for Unintelligible Charts
FinalTable <- FinalTable |>
  select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
if (as.numeric(week) == 3) {
  Week0_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, "Week0_VoA.csv"))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week1_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, "Week1_VoA.csv"))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Week2_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, "Week2_VoA.csv"))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Week0_VoA, rbind(Week1_VoA, rbind(Week2_VoA, FinalTable)))
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_3Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 4) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_3Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_4Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 5) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_4Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_5Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 6) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_5Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_6Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 7) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_6Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_7Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 8) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_7Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_8Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 9) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_8Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_9Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 10) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_9Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_10Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 11) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_10Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_11Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 12) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_11Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_12Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 13) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_12Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_13Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 14) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_13Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_14Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 15) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_14Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_15Ratings_Rks.csv", sep = ""))
} else if (as.numeric(week) == 16) {
  Full_Ratings_Rks <- read_csv(here("Data", paste0("VoA", year), "TrackingChartCSVs", paste(year, week_text, "0_15Ratings_Rks.csv", sep = ""))) |>
    select(team, conference, CFB_Week, VoA_Output, VoA_Ranking_Ovr, VoA_Rating_Ovr)
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  ## no need to write out a new tracking csv since "week 16" is the postseason VoA
  ## write_csv(Full_Ratings_Rks, paste(data_dir, "/TrackingChartCSVs", "/", year, week_text, "0_16Ratings_Rks.csv", sep = ""))
} else {
  print("No charts until Week 3!")
}
### end of if statement

### Filtering by conference for unintelligible charts
if (as.numeric(week) >= 3) {
  ### each conference (including independents) gets separate charts
  ### given that the Pac12 is now really the 2Pac, they get lumped in with the Indies
  AAC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "American Athletic")
  ACC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "ACC")
  Big12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big 12")
  Big10_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big Ten")
  CUSA_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Conference USA")
  ### lumping the 2Pac with the Indys for unintelligible chart purposes
  Indy_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "FBS Independents" | conference == "Pac-12")
  MAC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mid-American")
  MWC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mountain West")
  # Pac12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Pac-12")
  SEC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "SEC")
  SunBelt_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Sun Belt")
  
  ##### Creating Charts #####
  ### charting VoA_Rating and VoA_Ranking for each week from week 2 on
  AAC_VoA_Rating_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("American Conference Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Rating_Chart
  ggsave(AAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  AAC_VoA_Ranking_Chart <- ggplot(AAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("American Conference Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  AAC_VoA_Ranking_Chart
  ggsave(AAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Rating_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("ACC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Rating_Chart
  ggsave(ACC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  ACC_VoA_Ranking_Chart <- ggplot(ACC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("ACC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  ACC_VoA_Ranking_Chart
  ggsave(ACC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Rating_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 12 Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Rating_Chart
  ggsave(Big12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big12_VoA_Ranking_Chart <- ggplot(Big12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big12_VoA_Ranking_Chart
  ggsave(Big12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Rating_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 10 Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Rating_Chart
  ggsave(Big10_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Big10_VoA_Ranking_Chart <- ggplot(Big10_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Big 10 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Big10_VoA_Ranking_Chart
  ggsave(Big10_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Rating_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("CUSA Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Rating_Chart
  ggsave(CUSA_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  CUSA_VoA_Ranking_Chart <- ggplot(CUSA_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("CUSA Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  CUSA_VoA_Ranking_Chart
  ggsave(CUSA_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Rating_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Independents Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Rating_Chart
  ggsave(Indy_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  Indy_VoA_Ranking_Chart <- ggplot(Indy_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Independents Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  Indy_VoA_Ranking_Chart
  ggsave(Indy_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Rating_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("MAC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Rating_Chart
  ggsave(MAC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MAC_VoA_Ranking_Chart <- ggplot(MAC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("MAC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MAC_VoA_Ranking_Chart
  ggsave(MAC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Rating_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Mountain West Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Rating_Chart
  ggsave(MWC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  MWC_VoA_Ranking_Chart <- ggplot(MWC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Mountain West Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  MWC_VoA_Ranking_Chart
  ggsave(MWC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  # Pac12_VoA_Rating_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
  #   theme_bw() +
  #   geom_line(linewidth = 1.5) +
  #   geom_point(size = 5) +
  #   xlab("Week") +
  #   ylab("VoA Overall Rating") +
  #   labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  #   ggtitle("Pac 12 Vortex of Accuracy Overall Ratings by Week") +
  #   expand_limits(y = c(floor(floor(min(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
  #   scale_y_continuous(breaks = seq((floor((floor(min(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
  #   scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
  #   geom_cfb_logos(aes(team = team, width = 0.035)) +
  #   theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  # Pac12_VoA_Rating_Chart
  # ggsave(Pac12_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  # 
  # Pac12_VoA_Ranking_Chart <- ggplot(Pac12_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
  #   theme_bw() +
  #   geom_line(linewidth = 1.5) +
  #   geom_point(size = 5) +
  #   xlab("Week") +
  #   ylab("VoA Ranking") +
  #   labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  #   ggtitle("Pac 12 Vortex of Accuracy Rankings by Week") +
  #   expand_limits(y = c(0,130)) +
  #   scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
  #   scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
  #   geom_cfb_logos(aes(team = team, width = 0.035)) +
  #   theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  # Pac12_VoA_Ranking_Chart
  # ggsave(Pac12_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Rating_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("SEC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Rating_Chart
  ggsave(SEC_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SEC_VoA_Ranking_Chart <- ggplot(SEC_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("SEC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SEC_VoA_Ranking_Chart
  ggsave(SEC_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Rating_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Rating_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Sun Belt Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(y = c(floor(floor(min(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10, ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10)) +
    scale_y_continuous(breaks = seq((floor((floor(min(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), (ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10), by = 5)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Rating_Chart
  ggsave(SunBelt_Output_filename, path = output_dir, width = 50, height = 40, units = 'cm')
  
  SunBelt_VoA_Ranking_Chart <- ggplot(SunBelt_Ratings_Rks, aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = team)) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
    ggtitle("Sun Belt Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0,130)) +
    scale_y_continuous(breaks = c(0,20,40,60,80,100,120,140)) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    geom_cfb_logos(aes(team = team, width = 0.035)) +
    theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
  SunBelt_VoA_Ranking_Chart
  ggsave(SunBelt_Ranking_filename, path = output_dir, width = 50, height = 40, units = 'cm')
} else {
  print("No charts until Week 3!")
}

### Creating Histograms of VoA Output for all teams, and separate plots for power 5 and group of 5 teams subsetted out
## plots will be made for each week, not just after week 2 like Unintelligble Charts will
### subsetting teams
Power5_VoA <- VoA_Variables |>
  filter(conference == "ACC" | conference == "Big 12" | conference == "Big Ten" | conference == "FBS Independents" | conference == "Pac-12" | conference == "SEC") |>
  filter(team != "Connecticut" & team != "UMass")

Group5_VoA <- VoA_Variables |>
  filter(conference == "American Athletic" | conference == "Conference USA" | conference == "FBS Independents" | conference == "Mid-American" | conference == "Mountain West" | conference == "Sun Belt") |>
  filter(team != "Notre Dame")

FBS_Rating_histogram <- ggplot(VoA_Variables, aes(VoA_Rating_Ovr)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "orange") +
  scale_x_continuous(breaks = seq(-50,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(FBS_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
FBS_Rating_histogram
ggsave(FBS_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Power5_Rating_histogram <- ggplot(Power5_VoA, aes(VoA_Rating_Ovr)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "blue") +
  scale_x_continuous(breaks = seq(-50,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(Power5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Power5_Rating_histogram
ggsave(Power5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

Group5_Rating_histogram <- ggplot(Group5_VoA, aes(VoA_Rating_Ovr)) +
  geom_histogram(binwidth = 5,
                 col = "black",
                 fill = "pink") +
  scale_x_continuous(breaks = seq(-50,40,5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  ggtitle(Group5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
Group5_Rating_histogram
ggsave(Group5_hist_filename, path = output_dir, width = 50, height = 40, units = 'cm')

## Creating Scatterplot of VoA_Output vs VoA_Rating
VoA_Output_Rating_plot <- ggplot(VoA_Variables, aes(x = VoA_Output, y = VoA_Rating_Ovr)) +
  geom_point(size = 2) +
  geom_smooth() +
  geom_cfb_logos(aes(team = team), width = 0.035) +
  scale_x_continuous(breaks = seq(0,135,10)) +
  scale_y_continuous(breaks = seq(-50,40,5)) +
  ggtitle(Output_Rating_Plot_title) +
  xlab("VoA Output") +
  ylab("VoA Overall Rating") +
  labs(caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR and stats.ncaa.org") +
  theme(plot.title = element_text(size = 35, hjust = 0.5), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 22), legend.text = element_text(size = 20))
VoA_Output_Rating_plot
ggsave(Output_Rating_Plot_filename, path = output_dir, width = 50, height = 40, units = 'cm')




##### End of Script #####
end_time <- Sys.time()
end_time - start_time

