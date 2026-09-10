##### script includes assortment of testing options for data collection, analysis, visualization, etc
### test code for accessing cfb data API
## In this script, garbage time is excluded from Advanced Stats
## This script uses tidymodels random forest function to predict SP+/FPI-style metric instead of lm() function that main script uses
### wow all of the above is out of date in some way since main VoA uses Stan and no longer uses SP+/FPI/SRS averages as a target variable and models its own offensive/defensive/special teams team strength
### At this point this script is mostly for the NA debugging code at the very bottom that I use to figure out which columns in the main VoA script have NAs in them and why and how many and such
### Turning this script into an attempt to figure out if I can use exclusively play by play data to create a VoA that ranks both FBS and FCS teams in order to make more complete game projections, especially during early weeks and weeks with lots of FBS vs FCS games
## haven't messed with the main code here in years, I don't even remember what it does
## I have added in the packages and string setup from the main script though, just need to get into the data loading and formatting
### if some advanced stats can't be gotten from pbp (especially FCS teams) (probably can honestly, I assume that's how Bill creates them so they can be accessed from the API to begin with), then I might just take a lm or random forest or something and predict them using whatever stats I can get from PBP or train using adjusted stats that I know I can get from PBP
library(pacman)
# fmt: skip
p_load(tidyverse, gt, cfbfastR, here, RColorBrewer, gtExtras, cfbplotR, ggpubr, webshot2, cmdstanr, parallel, posterior, data.table, lme4, arrow)
### reading in script of functions (will be called later)
source(here("Scripts", "R", "CFBVoA_funcs.R"))
cfbd_api_key_info()

### inputting week and year info using readline
year <- readline(prompt = "What year is it? (year the season starts in) ")
cfb_week <- readline(prompt = "What week is it? ")
if (as.integer(cfb_week) == 0) {
  # PY4 <- as.integer(year) - 4
  PY3 <- as.integer(year) - 3
  PY2 <- as.integer(year) - 2
  PY1 <- as.integer(year) - 1
}

##### setting strings for table titles, file pathways, unintelligible charts #####
`%nin%` <- Negate(`%in%`)
output_dir <- here("Outputs", "RVoA", paste0("VoA", year), "Test")
data_dir <- here("Data", paste0("VoA", year), "Test")
tracking_chart_dir <- here(data_dir, "TrackingChartCSVs")
accuracy_data_dir <- here(data_dir, "AccuracyMetrics")
PY_data_dir <- here(data_dir, "PYData")
Projection_data_dir <- here(data_dir, "Projections")
preseason_text <- "Preseason"
resume_text <- "Resume"
VoAString <- "TestVoA.parquet"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "TestVoATop25.png"
fulltable_png <- "TestVoAFullTable.png"
VoA_text <- "Test Vortex of Accuracy"
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
FCS_text <- "FCS"
Power_Five_text <- "Power 5"
Group_Five_text <- "Group of 5"
Rating_text <- "_TestRatings_Chart.png"
Ranking_text <- "_TestRankings_Chart.png"
Histogram_text <- "_TestRatingHist.png"
Output_Rating_Plot_text <- "VoA Outputs vs VoA Ratings"
Output_Rating_Plot_png <- "TestOutput_Rating.png"

# fmt: skip
FBS_hist_title <- paste(year, week_text, cfb_week, FBS_text, VoA_text, "Ratings")
# fmt: skip
Power5_hist_title <- paste(year, week_text, cfb_week, Power_Five_text, VoA_text, "Ratings")
# fmt: skip
Group5_hist_title <- paste(year, week_text, cfb_week, Group_Five_text, VoA_text, "Ratings")
# fmt: skip
Output_Rating_Plot_title <- paste(year, week_text, cfb_week, Output_Rating_Plot_text)
top25_file_pathway <- paste(year, week_text, cfb_week, "_", top25_png, sep = "")
# fmt: skip
resumetop25_file_pathway <- paste(year,week_text, cfb_week,resume_text,"_",top25_png, sep = "")
# fmt: skip
fulltable_file_pathway <- paste(year,week_text, cfb_week, "_",fulltable_png, sep = "")
# fmt: skip
resumefulltable_file_pathway <- paste(year,week_text, cfb_week, resume_text,"_",fulltable_png, sep = "")
AAC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  AAC_text,
  Rating_text,
  sep = ""
)
# fmt: skip
AAC_Ranking_filename <- paste(year,week_text, cfb_week, AAC_text, Ranking_text, sep = "")
ACC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  ACC_text,
  Rating_text,
  sep = ""
)
# fmt: skip
ACC_Ranking_filename <- paste(year,week_text, cfb_week, ACC_text, Ranking_text, sep = "")
# fmt: skip
Big12_Output_filename <- paste(year,week_text, cfb_week, Big12_text, Rating_text, sep = "")
# fmt: skip
Big12_Ranking_filename <- paste(year,week_text, cfb_week, Big12_text, Ranking_text, sep = "")
# fmt: skip
Big10_Output_filename <- paste(year,week_text, cfb_week, Big10_text, Rating_text, sep = "")
# fmt: skip
Big10_Ranking_filename <- paste(year,week_text, cfb_week, Big10_text, Ranking_text, sep = "")
# fmt: skip
CUSA_Output_filename <- paste(year,week_text, cfb_week, CUSA_text, Rating_text, sep = "")
# fmt: skip
CUSA_Ranking_filename <- paste(year,week_text, cfb_week, CUSA_text, Ranking_text, sep = "")
# fmt: skip
Indy_Output_filename <- paste(year,week_text, cfb_week, Indy_text, Rating_text, sep = "")
# fmt: skip
Indy_Ranking_filename <- paste(year,week_text, cfb_week, Indy_text, Ranking_text, sep = "")
MAC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  MAC_text,
  Rating_text,
  sep = ""
)
# fmt: skip
MAC_Ranking_filename <- paste(year,week_text, cfb_week, MAC_text, Ranking_text, sep = "")
MWC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  MWC_text,
  Rating_text,
  sep = ""
)
# fmt: skip
MWC_Ranking_filename <- paste(year,week_text, cfb_week, MWC_text, Ranking_text, sep = "")
# fmt: skip
Pac2_Output_filename <- paste(year,week_text, cfb_week, Pac2_text, Rating_text, sep = "")
# fmt: skip
Pac2_Ranking_filename <- paste(year,week_text, cfb_week, Pac2_text, Ranking_text, sep = "")
SEC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  SEC_text,
  Rating_text,
  sep = ""
)
SEC_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  SEC_text,
  Ranking_text,
  sep = ""
)
# fmt: skip
SunBelt_Output_filename <- paste(year,week_text, cfb_week, SunBelt_text, Rating_text, sep = "")
# fmt: skip
SunBelt_Ranking_filename <- paste(year,week_text, cfb_week, SunBelt_text, Ranking_text, sep = "")
# fmt: skip
FBS_hist_filename <- paste(year, week_text, cfb_week, "_", FBS_text, Histogram_text, sep = "")
# fmt: skip
Power5_hist_filename <- paste(year, week_text, cfb_week, "_", Power_Five_text, Histogram_text, sep = "")
# fmt: skip
Group5_hist_filename <- paste(year, week_text, cfb_week, "_", Group_Five_text, Histogram_text, sep = "")
# fmt: skip
Output_Rating_Plot_filename <- paste(year, week_text, cfb_week, "_", Output_Rating_Plot_png, sep = "")
### setting gt title based on whether it's after a playoff week or not
if (as.numeric(cfb_week) == 15) {
  gt_top25_title <- paste(year, "Conference Championship Week", VoA_Top25_text)
  gt_title <- paste(year, "Conference Championship Week", VoA_text)
} else if (as.numeric(cfb_week) == 16) {
  gt_top25_title <- paste(year, "Post Army-Navy Game", VoA_Top25_text)
  gt_title <- paste(year, "Post Army-Navy Game", VoA_text)
} else if (as.numeric(cfb_week) == 17) {
  gt_top25_title <- paste(year, "CFP First Round", VoA_Top25_text)
  gt_title <- paste(year, "CFP First Round", VoA_text)
} else if (as.numeric(cfb_week) == 18) {
  gt_top25_title <- paste(year, "CFP Quarterfinals", VoA_Top25_text)
  gt_title <- paste(year, "CFP Quarterfinals", VoA_text)
} else if (as.numeric(cfb_week) == 19) {
  gt_top25_title <- paste(year, "CFP Semifinals", VoA_Top25_text)
  gt_title <- paste(year, "CFP Semifinals", VoA_text)
} else if (as.numeric(cfb_week) == 20) {
  gt_top25_title <- paste(year, Postseason_text, VoA_Top25_text)
  gt_title <- paste(year, Postseason_text, VoA_text)
} else if (as.numeric(cfb_week) == 0) {
  gt_top25_title <- paste(year, preseason_text, VoA_Top25_text)
  gt_title <- paste(year, preseason_text, VoA_text)
} else {
  gt_top25_title <- paste(year, week_text, cfb_week, VoA_Top25_text)
  gt_title <- paste(year, week_text, cfb_week, VoA_text)
}
### creating string for csv spreadsheet pathway
file_pathway <- paste0(data_dir, "/", year, week_text, cfb_week, "_", VoAString)
### creating directories that don't exist
# fmt: skip
for (i in c(data_dir, output_dir, tracking_chart_dir, Projection_data_dir, PY_data_dir, accuracy_data_dir)){
  if (dir.exists(i) == FALSE){
    dir.create(i, recursive = TRUE)
  }
}
### setting number of cores to use for mcmc chains later
options(mc.cores = parallel::detectCores() / 2)

##### Reading in Data #####
### pulling in data based on week of the season
if (as.numeric(cfb_week) == 0) {
  ##### WEEK 0 Data Pull #####
  ### getting team info for last 4 years
  ## using 4 years to train the model is a pain in my ass from a time and RAM standpoint, so holding off on that for now
  ## filtering to make sure each dataframe only includes D1 teams
  # D1Teams_PY4 <- cfbd_team_info(
  #   only_fbs = FALSE,
  #   year = as.numeric(year) - 4
  # ) |>
  #   filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY3 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.numeric(year) - 3
  ) |>
    filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY2 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.numeric(year) - 2
  ) |>
    filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY1 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.numeric(year) - 1
  ) |>
    filter(classification == "fbs" | classification == "fcs")
  D1Teams <- cfbd_team_info(only_fbs = FALSE, year = as.numeric(year)) |>
    filter(classification == "fbs" | classification == "fcs")

  # ### making sure the elevation column is numeric
  # VoAVariables$elevation <- as.numeric(VoAVariables$elevation)

  ### storing names of FCS teams for when they need to be filtered out in PY stats grabs
  # PY3Teams <- c("Delaware", "Missouri State", "Kennesaw State", "Sam Houston State", "Jacksonville State", "Sam Houston")
  # PY2Teams <- c("Delaware", "Missouri State", "Kennesaw State")
  # PY1Teams <- c("Delaware", "Missouri State")
  ### reading in data for 3 previous years
  ### reading in FCS data first, made with FCSCleanup.R
  # FCS_PY3 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY3.csv"))
  # FCS_PY2 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY2.csv"))
  # FCS_PY1 <- read_csv(here("Data", paste0("VoA", year), "FCSPrevYears", "FCS_PY1.csv"))

  ### pulling in completed games as part of opponent-adjustment of stats later
  ### PY4 completed games
  # CompletedGames_PY4 <- cfbd_game_info(as.numeric(year) - 4) |>
  #   filter(completed == TRUE) |>
  #   filter(
  #     home_team %in% D1Teams_PY4$school & away_team %in% D1Teams_PY4$school
  #   )
  # CompletedNeutralGames_PY4 <- CompletedGames_PY4 |>
  #   filter(neutral_site == TRUE)
  ### PY3 completed games
  CompletedGames_PY3 <- cfbd_game_info(as.numeric(year) - 3) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY3$school & away_team %in% D1Teams_PY3$school
    )
  CompletedNeutralGames_PY3 <- CompletedGames_PY3 |>
    filter(neutral_site == TRUE)
  ### PY2 completed games
  CompletedGames_PY2 <- cfbd_game_info(as.numeric(year) - 2) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY2$school & away_team %in% D1Teams_PY2$school
    )
  CompletedNeutralGames_PY2 <- CompletedGames_PY2 |>
    filter(neutral_site == TRUE)
  ### PY1 completed games
  CompletedGames_PY1 <- cfbd_game_info(as.numeric(year) - 1) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY1$school & away_team %in% D1Teams_PY1$school
    )
  CompletedNeutralGames_PY1 <- CompletedGames_PY1 |>
    filter(neutral_site == TRUE)

  ### loading in play-by-play data, creating VoA Variables dfs
  # PBP_PY4 <- load_cfb_pbp(seasons = as.numeric(year) - 4) |>
  #   filter(home %in% D1Teams_PY4$school & away %in% D1Teams_PY4$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY4$home_team &
  #     home %in% CompletedGames_PY4$away_team &
  #     away %in% CompletedGames_PY4$home_team &
  #     away %in% CompletedGames_PY4$away_team
  # )
  # PBP_PY4 <- fix_pbp_subdivision_nas(PBP_PY4, D1Teams_PY4) |>
  #   mutate(epa_ppa_mean = rowMeans(select(PBP_PY4, c(EPA, ppa)), na.rm = TRUE))
  # VoAVariablesTrain_PY4 <- create_voavarstrain_df(PY4, D1Teams_PY4, PBP_PY4)

  PBP_PY3 <- load_cfb_pbp(seasons = as.numeric(year) - 3) |>
    filter(home %in% D1Teams_PY3$school & away %in% D1Teams_PY3$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY3$home_team &
  #     home %in% CompletedGames_PY3$away_team &
  #     away %in% CompletedGames_PY3$home_team &
  #     away %in% CompletedGames_PY3$away_team
  # )
  PBP_PY3 <- fix_pbp_subdivision_nas(PBP_PY3, D1Teams_PY3) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY3, c(EPA, ppa)), na.rm = TRUE))
  VoAVariablesTrain_PY3 <- create_voavarstrain_df(PY3, D1Teams_PY3, PBP_PY3)

  PBP_PY2 <- load_cfb_pbp(seasons = as.numeric(year) - 2) |>
    filter(home %in% D1Teams_PY2$school & away %in% D1Teams_PY2$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY2$home_team &
  #     home %in% CompletedGames_PY2$away_team &
  #     away %in% CompletedGames_PY2$home_team &
  #     away %in% CompletedGames_PY2$away_team
  # )
  PBP_PY2 <- fix_pbp_subdivision_nas(PBP_PY2, D1Teams_PY2) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY2, c(EPA, ppa)), na.rm = TRUE))
  VoAVariablesTrain_PY2 <- create_voavarstrain_df(PY2, D1Teams_PY2, PBP_PY2)

  PBP_PY1 <- load_cfb_pbp(seasons = as.numeric(year) - 1) |>
    filter(home %in% D1Teams_PY1$school & away %in% D1Teams_PY1$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY1$home_team &
  #     home %in% CompletedGames_PY1$away_team &
  #     away %in% CompletedGames_PY1$home_team &
  #     away %in% CompletedGames_PY1$away_team
  # )
  PBP_PY1 <- fix_pbp_subdivision_nas(PBP_PY1, D1Teams_PY1) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY1, c(EPA, ppa)), na.rm = TRUE))
  VoAVariablesTrain_PY1 <- create_voavarstrain_df(PY1, D1Teams_PY1, PBP_PY1)
  ### VoAVariables df to be used for inference/generating current ratings
  VoAVariables <- create_voavars_df(as.integer(year), as.integer(cfb_week))

  ### pulling out relevant plays used to create/input variables later
  ## PY4
  # PBP_PY4_Yards <- PBP_PY4 |>
  #   filter(
  #     play_type == "Pass Incompletion" |
  #       play_type == "Rush" |
  #       play_type == "Sack" |
  #       play_type == "Fumble Recovery (Own)" |
  #       play_type == "Two Point Pass" |
  #       play_type == "Two Point Rush" |
  #       play_type == "Safety" |
  #       play_type == "Pass Reception" |
  #       play_type == "Pass Completion" |
  #       play_type == "Fumble Recovery (Opponent)" |
  #       play_type == "Pass" |
  #       play_type == "2pt Conversion" |
  #       play_type == "Defensive 2pt Conversion" |
  #       play_type == "Passing Touchdown" |
  #       play_type == "Rushing Touchdown"
  #   ) |>
  #   mutate(
  #     new_drive_pts = case_when(
  #       new_drive_pts < 0 ~ 0,
  #       drive_pts == 8 ~ 8,
  #       TRUE ~ new_drive_pts
  #     ),
  #     home_neutral = case_when(
  #       game_id %in% CompletedNeutralGames_PY4$game_id ~ "Neutral",
  #       TRUE ~ "Home"
  #     )
  #   ) |>
  #   mutate(
  #     play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
  #   )

  # PBP_PY4_ScoringPlays <- PBP_PY4_Yards |>
  #   filter(scoring_play == 1 & play_pts_scored != 3)

  # PBP_PY4_Turnovers <- PBP_PY4_Yards |>
  #   filter(turnover == 1)

  # PBP_PY4_success_plays <- PBP_PY4_Yards |>
  #   filter(
  #     (down == 1 & (yards_gained >= (distance / 2))) |
  #       (down == 2 & (yards_gained >= (distance * 0.7))) |
  #       (down > 2 & (yards_gained >= distance))
  #   )

  # PBP_PY4_3rdDowns <- PBP_PY4_Yards |>
  #   filter(down == 3)

  # PBP_PY4_4thDowns <- PBP_PY4_Yards |>
  #   filter(down == 4)

  # PBP_PY4_passplays <- PBP_PY4_Yards |>
  #   filter(
  #     play_type == "Pass" |
  #       play_type == "Pass Incompletion" |
  #       play_type == "Pass Reception" |
  #       play_type == "Pass Completion" |
  #       play_type == "Two Point Pass"
  #   )

  # PBP_PY4_rushplays <- PBP_PY4_Yards |>
  #   filter(play_type == "Rush" | play_type == "Two Point Rush")

  # PBP_PY4_scoringopp_plays <- PBP_PY4 |>
  #   filter(scoring_opp == 1)

  # PBP_PY4_TDs <- PBP_PY4_Yards |>
  #   filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # # PBP_PY4_2PtConvs <- PBP_PY4 |>
  # #   filter(
  # #     play_type == "Two Point Rush" |
  # #       play_type == "Two Point Pass" |
  # #       play_type == "2pt Conversion"
  # #   )

  # # PBP_PY4_2ptPlays <- PBP_PY4_TDs |>
  # #   filter(pos_score_pts == 8)

  # # PBP_PY4_2ptPlays <- rbind(PBP_PY4_2ptPlays, PBP_PY4_2PtConvs)

  # PBP_PY4_FGPlays <- PBP_PY4 |>
  #   filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  # PBP_PY4_XPPlays <- PBP_PY4_TDs |>
  #   filter(play_pts_scored == 7)

  # ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly quick glance)
  # ## except on punt return TDs
  # # fmt: skip
  # PBP_PY4_ReturnTDs <- PBP_PY4 |>
  #   filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  # PBP_PY4_PuntReturnTD <- PBP_PY4 |>
  #   filter(play_type == "Punt Return Touchdown")

  # ### on KickReturnPlays, pos_team gains yards/does the returning
  # ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  # PBP_PY4_KickReturn <- PBP_PY4 |>
  #   filter(
  #     play_type == "Kickoff Return Touchdown" |
  #       play_type == "Kickoff Return (Offense)" |
  #       play_type == "Kickoff"
  #   )

  # ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY4_Punts <- PBP_PY4 |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  # ### Setting up PBP for adjusted special teams ppa stats
  # PBP_STPlays_PY4 <- PBP_PY4 |>
  #   filter(
  #     play_type %in%
  #       c(
  #         "Punt",
  #         "Punt Return Touchdown",
  #         "Kickoff Return Touchdown",
  #         "Kickoff Return (Offense)",
  #         "Kickoff",
  #         "Blocked Punt Touchdown",
  #         "Blocked Field Goal Touchdown",
  #         "Missed Field Goal Touchdown",
  #         "Field Goal Good",
  #         "Field Goal Missed"
  #       ) |
  #       (play_type %in%
  #         c("Passing Touchdown", "Rushing Touchdown") &
  #         new_drive_pts == 7)
  #   ) |>
  #   mutate(
  #     new_drive_pts = case_when(
  #       new_drive_pts < 0 ~ 0,
  #       drive_pts == 8 ~ 8,
  #       TRUE ~ new_drive_pts
  #     )
  #   ) |>
  #   mutate(
  #     play_pts_scored = case_when(
  #       play_type %in% c("Passing Touchdown", "Rushing Touchdown") ~ 1,
  #       scoring_play == 1 ~ new_drive_pts,
  #       TRUE ~ 0
  #     )
  #   ) |>
  #   mutate(
  #     real_pos_team = case_when(
  #       play_type %in%
  #         c(
  #           "Field Goal Good",
  #           "Field Goal Missed",
  #           "Kickoff Return Touchdown",
  #           "Kickoff Return (Offense)",
  #           "Kickoff"
  #         ) |
  #         play_pts_scored == 1 ~ pos_team,
  #       TRUE ~ def_pos_team
  #     ),
  #     real_def_pos_team = case_when(
  #       play_type %in%
  #         c(
  #           "Field Goal Good",
  #           "Field Goal Missed",
  #           "Kickoff Return Touchdown",
  #           "Kickoff Return (Offense)",
  #           "Kickoff"
  #         ) ~ def_pos_team,
  #       TRUE ~ pos_team
  #     ),
  #     home_neutral = case_when(
  #       game_id %in% CompletedNeutralGames_PY4$game_id ~ "Neutral",
  #       TRUE ~ "Home"
  #     )
  #   )

  # PBP_PY4_STScoringPlays <- PBP_STPlays_PY4 |>
  #   filter(scoring_play == 1)

  ### PY3
  PBP_PY3_Yards <- PBP_PY3 |>
    filter(
      play_type == "Pass Incompletion" |
        play_type == "Rush" |
        play_type == "Sack" |
        play_type == "Fumble Recovery (Own)" |
        play_type == "Two Point Pass" |
        play_type == "Two Point Rush" |
        play_type == "Safety" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Fumble Recovery (Opponent)" |
        play_type == "Pass" |
        play_type == "2pt Conversion" |
        play_type == "Defensive 2pt Conversion" |
        play_type == "Passing Touchdown" |
        play_type == "Rushing Touchdown"
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY3$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_PY3_ScoringPlays <- PBP_PY3_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_PY3_Turnovers <- PBP_PY3_Yards |>
    filter(turnover == 1)

  PBP_PY3_success_plays <- PBP_PY3_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_PY3_3rdDowns <- PBP_PY3_Yards |>
    filter(down == 3)

  PBP_PY3_4thDowns <- PBP_PY3_Yards |>
    filter(down == 4)

  PBP_PY3_passplays <- PBP_PY3_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_PY3_rushplays <- PBP_PY3_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_PY3_scoringopp_plays <- PBP_PY3 |>
    filter(scoring_opp == 1)

  PBP_PY3_TDs <- PBP_PY3_Yards |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_PY3_2PtConvs <- PBP_PY3 |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_PY3_2ptPlays <- PBP_PY3_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_PY3_2ptPlays <- rbind(PBP_PY3_2ptPlays, PBP_PY3_2PtConvs)

  PBP_PY3_FGPlays <- PBP_PY3 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  # PBP_PY3_XPPlays <- PBP_PY3_TDs |>
  #   filter(play_pts_scored == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  # fmt: skip
  PBP_PY3_ReturnTDs <- PBP_PY3 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  PBP_PY3_PuntReturnTD <- PBP_PY3 |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY3_KickReturn <- PBP_PY3 |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY3_Punts <- PBP_PY3 |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY3 <- PBP_PY3 |>
    filter(
      play_type %in%
        c(
          "Punt",
          "Punt Return Touchdown",
          "Kickoff Return Touchdown",
          "Kickoff Return (Offense)",
          "Kickoff",
          "Blocked Punt Touchdown",
          "Blocked Field Goal Touchdown",
          "Missed Field Goal Touchdown",
          "Field Goal Good",
          "Field Goal Missed"
        ) |
        (play_type %in%
          c("Passing Touchdown", "Rushing Touchdown") &
          new_drive_pts == 7)
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      )
    ) |>
    mutate(
      play_pts_scored = case_when(
        play_type %in% c("Passing Touchdown", "Rushing Touchdown") ~ 1,
        scoring_play == 1 ~ new_drive_pts,
        TRUE ~ 0
      )
    ) |>
    mutate(
      real_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) |
          play_pts_scored == 1 ~ pos_team,
        TRUE ~ def_pos_team
      ),
      real_def_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) ~ def_pos_team,
        TRUE ~ pos_team
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY3$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_PY3_STScoringPlays <- PBP_STPlays_PY3 |>
    filter(scoring_play == 1)

  ### PY2
  PBP_PY2_Yards <- PBP_PY2 |>
    filter(
      play_type == "Pass Incompletion" |
        play_type == "Rush" |
        play_type == "Sack" |
        play_type == "Fumble Recovery (Own)" |
        play_type == "Two Point Pass" |
        play_type == "Two Point Rush" |
        play_type == "Safety" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Fumble Recovery (Opponent)" |
        play_type == "Pass" |
        play_type == "2pt Conversion" |
        play_type == "Defensive 2pt Conversion" |
        play_type == "Passing Touchdown" |
        play_type == "Rushing Touchdown"
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY2$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_PY2_ScoringPlays <- PBP_PY2_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_PY2_Turnovers <- PBP_PY2_Yards |>
    filter(turnover == 1)

  PBP_PY2_success_plays <- PBP_PY2_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_PY2_3rdDowns <- PBP_PY2_Yards |>
    filter(down == 3)

  PBP_PY2_4thDowns <- PBP_PY2_Yards |>
    filter(down == 4)

  PBP_PY2_passplays <- PBP_PY2_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_PY2_rushplays <- PBP_PY2_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_PY2_scoringopp_plays <- PBP_PY2 |>
    filter(scoring_opp == 1)

  PBP_PY2_TDs <- PBP_PY2_Yards |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_PY2_2PtConvs <- PBP_PY2 |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_PY2_2ptPlays <- PBP_PY2_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_PY2_2ptPlays <- rbind(PBP_PY2_2ptPlays, PBP_PY2_2PtConvs)

  PBP_PY2_FGPlays <- PBP_PY2 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  # PBP_PY2_XPPlays <- PBP_PY2_TDs |>
  #   filter(play_pts_scored == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  # fmt: skip
  PBP_PY2_ReturnTDs <- PBP_PY2 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  PBP_PY2_PuntReturnTD <- PBP_PY2 |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY2_KickReturn <- PBP_PY2 |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY2_Punts <- PBP_PY2 |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY2 <- PBP_PY2 |>
    filter(
      play_type %in%
        c(
          "Punt",
          "Punt Return Touchdown",
          "Kickoff Return Touchdown",
          "Kickoff Return (Offense)",
          "Kickoff",
          "Blocked Punt Touchdown",
          "Blocked Field Goal Touchdown",
          "Missed Field Goal Touchdown",
          "Field Goal Good",
          "Field Goal Missed"
        ) |
        (play_type %in%
          c("Passing Touchdown", "Rushing Touchdown") &
          new_drive_pts == 7)
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      )
    ) |>
    mutate(
      play_pts_scored = case_when(
        play_type %in% c("Passing Touchdown", "Rushing Touchdown") ~ 1,
        scoring_play == 1 ~ new_drive_pts,
        TRUE ~ 0
      )
    ) |>
    mutate(
      real_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) |
          play_pts_scored == 1 ~ pos_team,
        TRUE ~ def_pos_team
      ),
      real_def_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) ~ def_pos_team,
        TRUE ~ pos_team
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY2$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_PY2_STScoringPlays <- PBP_STPlays_PY2 |>
    filter(scoring_play == 1)

  ## PY1
  PBP_PY1_Yards <- PBP_PY1 |>
    filter(
      play_type == "Pass Incompletion" |
        play_type == "Rush" |
        play_type == "Sack" |
        play_type == "Fumble Recovery (Own)" |
        play_type == "Two Point Pass" |
        play_type == "Two Point Rush" |
        play_type == "Safety" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Fumble Recovery (Opponent)" |
        play_type == "Pass" |
        play_type == "2pt Conversion" |
        play_type == "Defensive 2pt Conversion" |
        play_type == "Passing Touchdown" |
        play_type == "Rushing Touchdown"
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY1$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_PY1_ScoringPlays <- PBP_PY1_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_PY1_Turnovers <- PBP_PY1_Yards |>
    filter(turnover == 1)

  PBP_PY1_success_plays <- PBP_PY1_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_PY1_3rdDowns <- PBP_PY1_Yards |>
    filter(down == 3)

  PBP_PY1_4thDowns <- PBP_PY1_Yards |>
    filter(down == 4)

  PBP_PY1_passplays <- PBP_PY1_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_PY1_rushplays <- PBP_PY1_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_PY1_scoringopp_plays <- PBP_PY1 |>
    filter(scoring_opp == 1)

  PBP_PY1_TDs <- PBP_PY1_Yards |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_PY1_2PtConvs <- PBP_PY1 |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_PY1_2ptPlays <- PBP_PY1_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_PY1_2ptPlays <- rbind(PBP_PY1_2ptPlays, PBP_PY1_2PtConvs)

  PBP_PY1_FGPlays <- PBP_PY1 |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  # PBP_PY1_XPPlays <- PBP_PY1_TDs |>
  #   filter(play_pts_scored == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  # fmt: skip
  PBP_PY1_ReturnTDs <- PBP_PY1 |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  PBP_PY1_PuntReturnTD <- PBP_PY1 |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_PY1_KickReturn <- PBP_PY1 |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  # PBP_PY1_Punts <- PBP_PY1 |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays_PY1 <- PBP_PY1 |>
    filter(
      play_type %in%
        c(
          "Punt",
          "Punt Return Touchdown",
          "Kickoff Return Touchdown",
          "Kickoff Return (Offense)",
          "Kickoff",
          "Blocked Punt Touchdown",
          "Blocked Field Goal Touchdown",
          "Missed Field Goal Touchdown",
          "Field Goal Good",
          "Field Goal Missed"
        ) |
        (play_type %in%
          c("Passing Touchdown", "Rushing Touchdown") &
          new_drive_pts == 7)
    ) |>
    mutate(
      new_drive_pts = case_when(
        new_drive_pts < 0 ~ 0,
        drive_pts == 8 ~ 8,
        TRUE ~ new_drive_pts
      )
    ) |>
    mutate(
      play_pts_scored = case_when(
        play_type %in% c("Passing Touchdown", "Rushing Touchdown") ~ 1,
        scoring_play == 1 ~ new_drive_pts,
        TRUE ~ 0
      )
    ) |>
    mutate(
      real_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) |
          play_pts_scored == 1 ~ pos_team,
        TRUE ~ def_pos_team
      ),
      real_def_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) ~ def_pos_team,
        TRUE ~ pos_team
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames_PY1$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_PY1_STScoringPlays <- PBP_STPlays_PY1 |>
    filter(scoring_play == 1)
} else if (as.integer(cfb_week) == 1) {
  ##### WEEK 1 Data Pull #####
  ### reading in data for 3 previous years
  ### no need to remove season and conference columns from PY3_df because they are removed before I write the csv in week 0
  # PY3_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY3.csv"))
  # PY2_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  # PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))

  ### TEMPORARY 2024 WEEK 1 FIX SINCE BALL STATE DID NOT PLAY A GAME IN WEEK 0 OR 1 and also CMU and ULM are having data issues
  # BallStCMUULM <- PY1_df |>
  #   filter(team == "Ball State" | team == "Central Michigan" | team == "Louisiana Monroe") |>
  #   mutate(season = as.numeric(year), .before = 1) |>
  #   mutate(conference = case_when(team == "Ball State" | team == "Central Michigan" ~ "Mid-American",
  #                                 TRUE ~ "Sun Belt"), .before = 3)
  # colnames(BallStCMUULM) <- c("season", "team", "conference", "games", "completion_pct", "off_pass_ypa", "off_pass_ypr", "int_pct", "off_rush_ypa", "off_turnovers_pg", "third_conv_rate", "fourth_conv_rate", "penalty_yds_pg", "yards_per_penalty", "kick_return_avg", "punt_return_avg", "off_ypg", "off_pass_ypg", "off_rush_ypg", "first_downs_pg", "off_ypp", "def_interceptions_pg", "off_plays_pg", "off_ppg", "def_ppg", "def_yds_pg", "def_plays_pg", "def_third_conv_rate", "def_fourth_conv_rate", "def_ypp", "fg_rate", "fg_rate_allowed", "fg_made_pg", "fg_made_pg_allowed", "xpts_pg", "xpts_allowed_pg", "kick_return_yds_avg_allowed", "punt_return_yds_avg_allowed", "st_ppg", "st_ppg_allowed", "oppdef_ppa", "oppoff_ppa", "off_ppa", "off_success_rate", "off_explosiveness", "off_power_success", "off_stuff_rt", "off_line_yds", "off_second_lvl_yds", "off_open_field_yds", "off_pts_per_opp", "off_field_pos_avg_predicted_points", "off_havoc_total", "off_havoc_front_seven", "off_havoc_db", "off_standard_downs_ppa", "off_standard_downs_success_rate", "off_standard_downs_explosiveness", "off_passing_downs_ppa", "off_passing_downs_success_rate", "off_passing_downs_explosiveness", "off_rushing_plays_ppa", "off_rushing_plays_success_rate", "off_rushing_plays_explosiveness", "off_passing_plays_ppa", "off_passing_plays_success_rate", "off_passing_plays_explosiveness", "def_ppa", "def_success_rate", "def_explosiveness", "def_power_success", "def_stuff_rt", "def_line_yds", "def_second_lvl_yds", "def_open_field_yds", "def_pts_per_opp", "def_field_pos_avg_predicted_points", "def_havoc_total", "def_havoc_front_seven", "def_havoc_db", "def_standard_downs_ppa", "def_standard_downs_success_rate", "def_standard_downs_explosiveness", "def_passing_downs_ppa", "def_passing_downs_success_rate", "def_passing_downs_explosiveness", "def_rushing_plays_ppa", "def_rush_success_rate", "def_rush_explosiveness", "def_passing_plays_ppa", "def_pass_success_rate", "def_pass_explosiveness", "recruit_pts", "talent")
  # BallStCMUULM <- BallStCMUULM |>
  #   select(season, team, conference, games, completion_pct, off_pass_ypa, off_pass_ypr, int_pct, off_rush_ypa, off_turnovers_pg, third_conv_rate, fourth_conv_rate, penalty_yds_pg, yards_per_penalty, kick_return_avg, punt_return_avg, off_ypg, off_pass_ypg, off_rush_ypg, first_downs_pg, off_ypp, def_interceptions_pg, off_plays_pg, off_ppg, def_ppg, def_yds_pg, def_plays_pg, def_third_conv_rate, def_fourth_conv_rate, def_ypp, fg_rate, fg_rate_allowed, fg_made_pg, fg_made_pg_allowed, xpts_pg, xpts_allowed_pg, kick_return_yds_avg_allowed, punt_return_yds_avg_allowed, st_ppg, st_ppg_allowed, oppdef_ppa, oppoff_ppa, off_ppa, off_success_rate, off_explosiveness, off_power_success, off_stuff_rt, off_line_yds, off_second_lvl_yds, off_open_field_yds, off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total, off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa, off_standard_downs_success_rate, off_standard_downs_explosiveness, off_passing_downs_ppa, off_passing_downs_success_rate, off_passing_downs_explosiveness, off_rushing_plays_ppa, off_rushing_plays_success_rate, off_rushing_plays_explosiveness, off_passing_plays_ppa, off_passing_plays_success_rate, off_passing_plays_explosiveness, def_ppa, def_success_rate, def_explosiveness, def_power_success, def_stuff_rt, def_line_yds, def_second_lvl_yds, def_open_field_yds, def_pts_per_opp, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_ppa, def_rush_success_rate, def_rush_explosiveness, def_passing_plays_ppa, def_pass_success_rate, def_pass_explosiveness, recruit_pts)

  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  # FCS_PY2 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY2.csv"
  # ))
  # FCS_PY1 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY1.csv"
  # ))

  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(
      home_division %in% c("fbs", "fcs") & away_division %in% c("fbs", "fcs")
    )
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))

  ### pulling out relevant plays used to create/input variables later
  # fmt: skip
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Reception" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)

  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)

  PBP_passplays <- PBP_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_rushplays <- PBP_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_scoringopp_plays <- PBP |>
    filter(scoring_opp == 1) |>
    drop_na(drive_pts)

  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_2PtConvs <- PBP |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_2ptPlays <- PBP_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)

  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Punt Return Touchdown" |
        play_type == "Blocked Punt Touchdown" |
        play_type == "Blocked Field Goal Touchdown" |
        play_type == "Missed Field Goal Touchdown"
    )

  PBP_PuntReturnTD <- PBP_ReturnTDs |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  # fmt: skip
  PBP_EPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  PBP_ExpAdjustment <- PBP_Yards[, c(
    "game_id",
    "home",
    "pos_team",
    "def_pos_team",
    "success",
    "ppa",
    "offense_conference",
    "defense_conference",
    "home_neutral"
  )] |>
    filter(success == 1) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()

  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[, c(
    "game_id",
    "home",
    "pos_team",
    "def_pos_team",
    "yards_gained",
    "offense_conference",
    "defense_conference",
    "home_neutral"
  )] |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()

  ### Extracting PBP for scoring plays
  PBP_PPG_Adjustment <- PBP_Yards[, c(
    "game_id",
    "home",
    "pos_team",
    "def_pos_team",
    "offense_score_play",
    "rush_td",
    "pass_td",
    "offense_conference",
    "defense_conference",
    "home_neutral",
    "play_pts_scored"
  )] |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()

  ### Setting up PBP for adjusted special teams ppa stats
  # fmt: skip
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_STEPA_Adjustment <- PBP_STPlays |>
    select(
      game_id,
      home,
      real_pos_team,
      real_def_pos_team,
      ppa,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()
} else if (as.integer(cfb_week) <= 5) {
  ##### WEEKS 2-5 DATA PULL #####
  ### reading in Previous year's data as csvs so I don't have to read it in again
  # PY2_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  # PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))

  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  # FCS_PY2 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY2.csv"
  # ))
  # FCS_PY1 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY1.csv"
  # ))

  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(
      home_division %in% c("fbs", "fcs") & away_division %in% c("fbs", "fcs")
    )
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))

  ### pulling out relevant plays used to create/input variables later
  # fmt: skip
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Reception" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)

  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)

  PBP_passplays <- PBP_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_rushplays <- PBP_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_scoringopp_plays <- PBP |>
    filter(scoring_opp == 1) |>
    drop_na(drive_pts)

  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_2PtConvs <- PBP |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_2ptPlays <- PBP_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)

  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  PBP_ReturnTDs <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Punt Return Touchdown" |
        play_type == "Blocked Punt Touchdown" |
        play_type == "Blocked Field Goal Touchdown" |
        play_type == "Missed Field Goal Touchdown"
    )

  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  # fmt: skip
  PBP_EPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  # fmt: skip
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[, c(
    "game_id",
    "home",
    "pos_team",
    "def_pos_team",
    "yards_gained",
    "offense_conference",
    "defense_conference",
    "home_neutral"
  )] |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()

  ### Extracting PBP for scoring plays
  # fmt: skip
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral", "play_pts_scored")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Setting up PBP for adjusted special teams ppa stats
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(
      real_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) |
          pos_score_pts == 7 ~ pos_team,
        TRUE ~ def_pos_team
      ),
      real_def_pos_team = case_when(
        play_type %in%
          c(
            "Field Goal Good",
            "Field Goal Missed",
            "Kickoff Return Touchdown",
            "Kickoff Return (Offense)",
            "Kickoff"
          ) |
          pos_score_pts == 7 ~ def_pos_team,
        TRUE ~ pos_team
      ),
      home_neutral = case_when(
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_STEPA_Adjustment <- PBP_STPlays |>
    select(
      game_id,
      home,
      real_pos_team,
      real_def_pos_team,
      ppa,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()
} else if (as.integer(cfb_week) <= 8) {
  ##### WEEKS 6-8 Data Pull #####
  ### reading in Previous year's data as csvs so I don't have to read it in again
  # PY1_df <- read_csv(here("Data", paste0("VoA", year), "PYData", "PY1.csv"))

  # ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  # FCS_PY2 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY2.csv"
  # ))
  # FCS_PY1 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY1.csv"
  # ))

  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_division == "fbs" | away_division == "fbs")
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))

  ### pulling out relevant plays used to create/input variables later
  # fmt: skip
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Reception" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)

  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)

  PBP_passplays <- PBP_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_rushplays <- PBP_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_scoringopp_plays <- PBP |>
    filter(scoring_opp == 1) |>
    drop_na(drive_pts)

  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_2PtConvs <- PBP |>
  #   filter(
  #     play_type == "Two Point Rush" |
  #       play_type == "Two Point Pass" |
  #       play_type == "2pt Conversion"
  #   )

  # PBP_2ptPlays <- PBP_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)

  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  # fmt: skip
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  # fmt: skip
  PBP_EPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  # fmt: skip
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### extracting specific columns for opponent-adjusted yards per play stat
  PBP_YPP_Adjustment <- PBP_Yards[, c(
    "game_id",
    "home",
    "pos_team",
    "def_pos_team",
    "yards_gained",
    "offense_conference",
    "defense_conference",
    "home_neutral"
  )] |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()

  ### Extracting PBP for scoring plays
  # fmt: skip
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral", "play_pts_scored")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Setting up PBP for adjusted special teams ppa stats
  # fmt: skip
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_STEPA_Adjustment <- PBP_STPlays |>
    select(
      game_id,
      home,
      real_pos_team,
      real_def_pos_team,
      ppa,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()
} else {
  ##### CURRENT SEASON STATS ONLY Data Pull #####
  ### reading in previous year's FCS data so it can be referenced when making ppg adjustments
  # FCS_PY2 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY2.csv"
  # ))
  # FCS_PY1 <- read_csv(here(
  #   "Data",
  #   paste0("VoA", year),
  #   "FCSPrevYears",
  #   "FCS_PY1.csv"
  # ))
  ### pulling in completed games as part of opponent-adjustment of stats later
  CompletedGames <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == TRUE) |>
    filter(home_division == "fbs" | away_division == "fbs")
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)
  ### Current season Play by play data
  PBP <- load_cfb_pbp(seasons = as.numeric(year))

  ### pulling out relevant plays used to create/input variables later
  # fmt: skip
  PBP_Yards <- PBP |>
    filter(play_type == "Pass Incompletion" | play_type == "Pass Reception" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown") |>
    mutate(home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_3rdDowns <- PBP_Yards |>
    filter(down == 3)

  PBP_4thDowns <- PBP_Yards |>
    filter(down == 4)

  PBP_passplays <- PBP_Yards |>
    filter(
      play_type == "Pass" |
        play_type == "Pass Incompletion" |
        play_type == "Pass Reception" |
        play_type == "Pass Completion" |
        play_type == "Two Point Pass"
    )

  PBP_rushplays <- PBP_Yards |>
    filter(play_type == "Rush" | play_type == "Two Point Rush")

  PBP_scoringopp_plays <- PBP |>
    filter(scoring_opp == 1) |>
    drop_na(drive_pts)

  PBP_TDs <- PBP |>
    filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

  # PBP_2PtConvs <- PBP |>
  #   filter(play_type == "Two Point Rush" | play_type == "Two Point Pass" | play_type == "2pt Conversion")

  # PBP_2ptPlays <- PBP_TDs |>
  #   filter(pos_score_pts == 8)

  # PBP_2ptPlays <- rbind(PBP_2ptPlays, PBP_2PtConvs)

  PBP_FGPlays <- PBP |>
    filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

  PBP_XPPlays <- PBP_TDs |>
    filter(pos_score_pts == 7)

  ### on ReturnTD plays, pos_team does the scoring (at least based on an admittedly too-quick glance)
  ## except on punt return TDs
  # fmt: skip
  PBP_ReturnTDs <- PBP |>
    filter(play_type == "Kickoff Return Touchdown" | play_type == "Punt Return Touchdown" | play_type == "Blocked Punt Touchdown" | play_type == "Blocked Field Goal Touchdown" | play_type == "Missed Field Goal Touchdown")

  PBP_PuntReturnTD <- PBP |>
    filter(play_type == "Punt Return Touchdown")

  ### on KickReturnPlays, pos_team gains yards/does the returning
  ## will use data from this subset to evaluate a predictor, kick/punt return yards allowed
  PBP_KickReturn <- PBP |>
    filter(
      play_type == "Kickoff Return Touchdown" |
        play_type == "Kickoff Return (Offense)" |
        play_type == "Kickoff"
    )

  ### on punt plays, pos_team does the punting, def_pos_team does the returning
  PBP_Punts <- PBP |>
    filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### filtering out columns not used to make opponent-adjusted PPA (EPA) stat
  # fmt: skip
  PBP_EPA_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Extracting just successful plays for opponent-adjusted Explosiveness metric
  # fmt: skip
  PBP_ExpAdjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "success", "ppa", "offense_conference", "defense_conference", "home_neutral")] |>
    filter(success == 1) |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### extracting specific columns for opponent-adjusted yards per play stat
  # fmt: skip
  PBP_YPP_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "yards_gained", "offense_conference", "defense_conference", "home_neutral")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Extracting PBP for scoring plays
  # fmt: skip
  PBP_PPG_Adjustment <- PBP_Yards[,c("game_id", "home", "pos_team", "def_pos_team", "offense_score_play", "rush_td", "pass_td", "offense_conference", "defense_conference", "home_neutral", "play_pts_scored")] |>
    mutate(hfa = as.factor(case_when(home_neutral == "Neutral" ~ 0,
                                     ### home team on offense
                                     pos_team == home ~ 1,
                                     ### home team on defense
                                     TRUE ~ -1))) |>
    drop_na()

  ### Setting up PBP for adjusted special teams ppa stats
  # fmt: skip
  PBP_STPlays <- rbind(PBP_FGPlays, PBP_XPPlays, PBP_KickReturn, PBP_Punts) |>
    mutate(real_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ pos_team,
                                     TRUE ~ def_pos_team),
           real_def_pos_team = case_when(play_type %in% c("Field Goal Good", "Field Goal Missed", "Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff") | pos_score_pts == 7 ~ def_pos_team,
                                         TRUE ~ pos_team),
           home_neutral = case_when(game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
                                    TRUE ~ "Home"))

  PBP_STEPA_Adjustment <- PBP_STPlays |>
    select(
      game_id,
      home,
      real_pos_team,
      real_def_pos_team,
      ppa,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      ))
    ) |>
    drop_na()
}


##### Extracting Stats from PBP Data #####
if (as.integer(cfb_week) == 0) {
  ##### WEEK 0 DF Merge #####
  VoAVariablesTrain_PY1 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY1,
    rushpass_plays = PBP_PY1_Yards,
    success_plays = PBP_PY1_success_plays,
    ThirdDowns = PBP_PY1_3rdDowns,
    FourthDowns = PBP_PY1_4thDowns,
    passplays = PBP_PY1_passplays,
    rushplays = PBP_PY1_rushplays,
    scoringopp_plays = PBP_PY1_scoringopp_plays,
    turnovers = PBP_PY1_Turnovers,
    scoringplays = PBP_PY1_ScoringPlays,
    FGs = PBP_PY1_FGPlays,
    # Punts = PBP_PY1_Punts,
    # Kickoffs = PBP_PY1_KickReturn,
    # XPts = PBP_PY1_XPPlays,
    STPlays = PBP_STPlays_PY1
  )
  ### PY2
  VoAVariablesTrain_PY2 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY2,
    rushpass_plays = PBP_PY2_Yards,
    success_plays = PBP_PY2_success_plays,
    ThirdDowns = PBP_PY2_3rdDowns,
    FourthDowns = PBP_PY2_4thDowns,
    passplays = PBP_PY2_passplays,
    rushplays = PBP_PY2_rushplays,
    scoringopp_plays = PBP_PY2_scoringopp_plays,
    turnovers = PBP_PY2_Turnovers,
    scoringplays = PBP_PY2_ScoringPlays,
    FGs = PBP_PY2_FGPlays,
    # Punts = PBP_PY2_Punts,
    # Kickoffs = PBP_PY2_KickReturn,
    # XPts = PBP_PY2_XPPlays,
    STPlays = PBP_STPlays_PY2
  )
  ### PY3
  VoAVariablesTrain_PY3 <- extract_pbp_stats(
    VoA_df = VoAVariablesTrain_PY3,
    rushpass_plays = PBP_PY3_Yards,
    success_plays = PBP_PY3_success_plays,
    ThirdDowns = PBP_PY3_3rdDowns,
    FourthDowns = PBP_PY3_4thDowns,
    passplays = PBP_PY3_passplays,
    rushplays = PBP_PY3_rushplays,
    scoringopp_plays = PBP_PY3_scoringopp_plays,
    turnovers = PBP_PY3_Turnovers,
    scoringplays = PBP_PY3_ScoringPlays,
    FGs = PBP_PY3_FGPlays,
    # Punts = PBP_PY3_Punts,
    # Kickoffs = PBP_PY3_KickReturn,
    # XPts = PBP_PY3_XPPlays,
    STPlays = PBP_STPlays_PY3
  )
  ### PY4
  # VoAVariablesTrain_PY4 <- extract_pbp_stats(
  #   VoA_df = VoAVariablesTrain_PY4,
  #   rushpass_plays = PBP_PY4_Yards,
  #   success_plays = PBP_PY4_success_plays,
  #   ThirdDowns = PBP_PY4_3rdDowns,
  #   FourthDowns = PBP_PY4_4thDowns,
  #   passplays = PBP_PY4_passplays,
  #   rushplays = PBP_PY4_rushplays,
  #   scoringopp_plays = PBP_PY4_scoringopp_plays,
  #   turnovers = PBP_PY4_Turnovers,
  #   scoringplays = PBP_PY4_ScoringPlays,
  #   FGs = PBP_PY4_FGPlays,
  #   Punts = PBP_PY4_Punts,
  #   Kickoffs = PBP_PY4_KickReturn,
  #   XPts = PBP_PY4_XPPlays,
  #   STPlays = PBP_STPlays_PY4
  # )

  ### Extracting PBP data and opponent-adjusted data for df to be used for inference/current season's ratings
  VoAVariables <- extract_preseason_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays_PY1 = PBP_PY1_Yards,
    success_plays_PY1 = PBP_PY1_success_plays,
    ThirdDowns_PY1 = PBP_PY1_3rdDowns,
    FourthDowns_PY1 = PBP_PY1_4thDowns,
    passplays_PY1 = PBP_PY1_passplays,
    rushplays_PY1 = PBP_PY1_rushplays,
    scoringopp_plays_PY1 = PBP_PY1_scoringopp_plays,
    turnovers_PY1 = PBP_PY1_Turnovers,
    scoringplays_PY1 = PBP_PY1_ScoringPlays,
    FGs_PY1 = PBP_PY1_FGPlays,
    # Punts_PY1 = PBP_PY1_Punts,
    # Kickoffs_PY1 = PBP_PY1_KickReturn,
    # XPts_PY1 = PBP_PY1_XPPlays,
    STPlays_PY1 = PBP_STPlays_PY1,
    ### PY2
    rushpass_plays_PY2 = PBP_PY2_Yards,
    success_plays_PY2 = PBP_PY2_success_plays,
    ThirdDowns_PY2 = PBP_PY2_3rdDowns,
    FourthDowns_PY2 = PBP_PY2_4thDowns,
    passplays_PY2 = PBP_PY2_passplays,
    rushplays_PY2 = PBP_PY2_rushplays,
    scoringopp_plays_PY2 = PBP_PY2_scoringopp_plays,
    turnovers_PY2 = PBP_PY2_Turnovers,
    scoringplays_PY2 = PBP_PY2_ScoringPlays,
    FGs_PY2 = PBP_PY2_FGPlays,
    # Punts_PY2 = PBP_PY2_Punts,
    # Kickoffs_PY2 = PBP_PY2_KickReturn,
    # XPts_PY2 = PBP_PY2_XPPlays,
    STPlays_PY2 = PBP_STPlays_PY2,
    ### PY3
    rushpass_plays_PY3 = PBP_PY3_Yards,
    success_plays_PY3 = PBP_PY3_success_plays,
    ThirdDowns_PY3 = PBP_PY3_3rdDowns,
    FourthDowns_PY3 = PBP_PY3_4thDowns,
    passplays_PY3 = PBP_PY3_passplays,
    rushplays_PY3 = PBP_PY3_rushplays,
    scoringopp_plays_PY3 = PBP_PY3_scoringopp_plays,
    turnovers_PY3 = PBP_PY3_Turnovers,
    scoringplays_PY3 = PBP_PY3_ScoringPlays,
    FGs_PY3 = PBP_PY3_FGPlays,
    # Punts_PY3 = PBP_PY3_Punts,
    # Kickoffs_PY3 = PBP_PY3_KickReturn,
    # XPts_PY3 = PBP_PY3_XPPlays,
    STPlays_PY3 = PBP_STPlays_PY3
  )

  ### merging all data frames in order of PY3, PY2, PY1
  # all_PY_df_list <- list(PY3_df, PY2_df, PY1_df, recruit)
  # VoAVariables <- all_PY_df_list |>
  #   reduce(full_join, by = "team")

  ### Making values numeric
  # VoAVariables[, 4:ncol(VoAVariables)] <- VoAVariables[,
  #   4:ncol(VoAVariables)
  # ] |>
  #   mutate_if(is.character, as.numeric)
  ### adding difference columns
  VoAVariables <- VoAVariables |>
    mutate(
      EPA_diff_PY3 = adj_off_epa_PY3 - adj_def_epa_PY3,
      EPA_diff_PY2 = adj_off_epa_PY2 - adj_def_epa_PY2,
      EPA_diff_PY1 = adj_off_epa_PY1 - adj_def_epa_PY1,
      SuccessRt_diff_PY3 = off_success_rt_PY3 - def_success_rt_PY3,
      SuccessRt_diff_PY2 = off_success_rt_PY2 - def_success_rt_PY2,
      SuccessRt_diff_PY1 = off_success_rt_PY1 - def_success_rt_PY1,
      HavocRt_diff_PY3 = def_havoc_total_PY3 - off_havoc_total_PY3,
      HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
      HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
      Explosiveness_diff_PY3 = adj_off_explosiveness_PY3 -
        adj_def_explosiveness_PY3,
      Explosiveness_diff_PY2 = adj_off_explosiveness_PY2 -
        adj_def_explosiveness_PY2,
      Explosiveness_diff_PY1 = adj_off_explosiveness_PY1 -
        adj_def_explosiveness_PY1,
      net_adj_st_epa_PY3 = adj_off_st_epa_PY3 - adj_def_st_epa_PY3,
      net_adj_st_epa_PY2 = adj_off_st_epa_PY2 - adj_def_st_epa_PY2,
      net_adj_st_epa_PY1 = adj_off_st_epa_PY1 - adj_def_st_epa_PY1
    )

  ### writing csv of PY3_df so that I don't have to run the same code to produce it in Weeks when PY3 data is still being used
  if (dir.exists(here("Data", paste0("VoA", year), "PYData")) == FALSE) {
    dir.create(here("Data", paste0("VoA", year), "PYData"), recursive = TRUE)
  }

  ### saving VoAVariables now as a parquet file so I can read it in in future weeks for weighted variable calculation without needing to rerun all the PBP stuff
  write_parquet(
    VoAVariables,
    here(
      "Data",
      paste0("VoA", year),
      "PYData",
      paste0("PYData", year, ".parquet")
    )
  )
  # write_csv(PY3_df_NoSeasonConf, here("Data", paste0("VoA", year), "PYData", "PY3.csv"))
  # write_csv(PY2_df, here("Data", paste0("VoA", year), "PYData", "PY2.csv"))
  # write_csv(PY1_df, here("Data", paste0("VoA", year), "PYData", "PY1.csv"))
} else if (as.integer(cfb_week) == 1) {
  ##### WEEK 1 DF Merge #####
  ### merging data frames together, arranging columns
  ## need to merge stats and advanced stats together first so I can change column names to avoid duplicate column names later on
  ### Previous years data have been saved as csvs to prevent having to pull in data from cfbfastR for rest of season
  ## then I will merge years together by team

  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(
    temp_PBP_yards,
    temp_PBP_Defyards,
    temp_PBP_3rd,
    temp_PBP_4th,
    temp_PBP_OffTDs,
    temp_PBP_DefTDs,
    temp_PBP_2Pts,
    temp_PBP_Def2Pts,
    temp_PBP_FGs,
    temp_PBP_GoodFGs,
    temp_PBP_DefFGs,
    temp_PBP_DefGoodFGs,
    temp_PBP_XPts,
    temp_PBP_DefXPts,
    temp_PBP_KickReturn,
    temp_PBP_PuntReturn,
    temp_PBP_ReturnTDs,
    temp_PBP_OffReturnTDs,
    temp_PBP_PuntTDs
  )

  ## merging all data frames in order of PY3, PY2, PY1
  all_PY_df_list <- list(Current_df, PY3_df, PY2_df, PY1_df)
  VoAVariables <- all_PY_df_list |>
    reduce(full_join, by = "team") |>
    mutate(
      EPA_diff_PY3 = off_epa_PY3 - def_epa_PY3,
      EPA_diff_PY2 = off_epa_PY2 - def_epa_PY2,
      EPA_diff_PY1 = off_epa_PY1 - def_epa_PY1,
      SuccessRt_diff_PY3 = off_success_rate_PY3 - def_success_rate_PY3,
      SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
      SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
      HavocRt_diff_PY3 = def_havoc_total_PY3 - off_havoc_total_PY3,
      HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
      HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
      Explosiveness_diff_PY3 = off_explosiveness_PY3 - def_explosiveness_PY3,
      Explosiveness_diff_PY2 = off_explosiveness_PY2 - def_explosiveness_PY2,
      Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
      EPA_diff = off_epa - def_epa,
      SuccessRt_diff = off_success_rate - def_success_rate,
      HavocRt_diff = def_havoc_total - off_havoc_total,
      Explosiveness_diff = off_explosiveness - def_explosiveness,
      net_st_ppg_PY3 = st_ppg_PY3 - st_ppg_allowed_PY3,
      net_st_ppg_PY2 = st_ppg_PY2 - st_ppg_allowed_PY2,
      net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1,
      net_st_ppg = st_ppg - st_ppg_allowed
    )
} else if (as.integer(cfb_week) <= 5) {
  ##### WEEKS 2-5 DF Merge #####
  ### merging data frames together, arranging columns
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(
    temp_PBP_yards,
    temp_PBP_Defyards,
    temp_PBP_3rd,
    temp_PBP_4th,
    temp_PBP_OffTDs,
    temp_PBP_DefTDs,
    temp_PBP_2Pts,
    temp_PBP_Def2Pts,
    temp_PBP_FGs,
    temp_PBP_GoodFGs,
    temp_PBP_DefFGs,
    temp_PBP_DefGoodFGs,
    temp_PBP_XPts,
    temp_PBP_DefXPts,
    temp_PBP_KickReturn,
    temp_PBP_PuntReturn,
    temp_PBP_ReturnTDs,
    temp_PBP_OffReturnTDs,
    temp_PBP_PuntTDs,
    temp_PBP_Defplays,
    temp_PBP_Offplays,
    temp_PBP_OppDefPPA,
    temp_PBP_OppOffPPA
  )

  ### combining all dfs
  all_df_list <- list(Current_df, PY2_df, PY1_df)
  VoAVariables <- all_df_list |>
    reduce(full_join, by = "team") |>
    mutate(
      EPA_diff_PY2 = off_epa_PY2 - def_epa_PY2,
      EPA_diff_PY1 = off_epa_PY1 - def_epa_PY1,
      SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
      SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
      HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
      HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
      Explosiveness_diff_PY2 = off_explosiveness_PY2 - def_explosiveness_PY2,
      Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
      EPA_diff = off_epa - def_epa,
      SuccessRt_diff = off_success_rate - def_success_rate,
      HavocRt_diff = def_havoc_total - off_havoc_total,
      Explosiveness_diff = off_explosiveness - def_explosiveness,
      net_st_ppg = st_ppg - st_ppg_allowed,
      net_st_ppg_PY2 = st_ppg_PY2 - st_ppg_allowed_PY2,
      net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1
    )
} else if (as.integer(cfb_week) <= 8) {
  ##### WEEKS 6-8 DF Merge #####
  ## merging data frames together, arranging columns

  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(
    temp_PBP_yards,
    temp_PBP_Defyards,
    temp_PBP_3rd,
    temp_PBP_4th,
    temp_PBP_OffTDs,
    temp_PBP_DefTDs,
    temp_PBP_2Pts,
    temp_PBP_Def2Pts,
    temp_PBP_FGs,
    temp_PBP_GoodFGs,
    temp_PBP_DefFGs,
    temp_PBP_DefGoodFGs,
    temp_PBP_XPts,
    temp_PBP_DefXPts,
    temp_PBP_KickReturn,
    temp_PBP_PuntReturn,
    temp_PBP_ReturnTDs,
    temp_PBP_OffReturnTDs,
    temp_PBP_PuntTDs
  )

  ### now that variables derived from pbp data have been filled in, creating final VoAVariables df
  VoAVariables <- Current_df |>
    mutate(
      EPA_diff_PY1 = off_epa_PY1 - def_epa_PY1,
      SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
      HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
      Explosiveness_diff_PY1 = off_explosiveness_PY1 - def_explosiveness_PY1,
      EPA_diff = off_epa - def_epa,
      SuccessRt_diff = off_success_rate - def_success_rate,
      HavocRt_diff = def_havoc_total - off_havoc_total,
      Explosiveness_diff = off_explosiveness - def_explosiveness,
      net_st_ppg_PY1 = st_ppg_PY1 - st_ppg_allowed_PY1,
      net_st_ppg = st_ppg - st_ppg_allowed
    )
} else {
  ##### Week 9-End of Season CURRENT SEASON ONLY DF Merge #####
  ### removing temp variables from the environment in the hope it will stop my R session from crashing
  rm(
    temp_PBP_yards,
    temp_PBP_Defyards,
    temp_PBP_3rd,
    temp_PBP_4th,
    temp_PBP_OffTDs,
    temp_PBP_DefTDs,
    temp_PBP_2Pts,
    temp_PBP_Def2Pts,
    temp_PBP_FGs,
    temp_PBP_GoodFGs,
    temp_PBP_DefFGs,
    temp_PBP_DefGoodFGs,
    temp_PBP_XPts,
    temp_PBP_DefXPts,
    temp_PBP_KickReturn,
    temp_PBP_PuntReturn,
    temp_PBP_ReturnTDs,
    temp_PBP_OffReturnTDs,
    temp_PBP_PuntTDs
  )

  VoAVariables <- Current_df |>
    mutate(
      EPA_diff = off_epa - def_epa,
      SuccessRt_diff = off_success_rate - def_success_rate,
      HavocRt_diff = def_havoc_total - off_havoc_total,
      Explosiveness_diff = off_explosiveness - def_explosiveness,
      off_ppg_aboveavg = off_ppg - mean(off_ppg),
      def_ppg_aboveavg = def_ppg - mean(def_ppg),
      net_st_kick_return_yds = st_kick_return_yds - st_kick_return_yds_allowed,
      net_punt_return_yds = st_punt_return_yds - st_punt_return_yds_allowed,
      net_st_epa = st_epa - st_epa_allowed,
      net_fg_rate = fg_rate - fg_rate_allowed,
      net_fg_made_pg = fg_made_pg - fg_made_pg_allowed,
      # net_xpts_pg = xpts_pg - xpts_allowed_pg,
      net_st_ppg = adj_off_st_ppg - adj_def_st_ppg,
      off_ppg_aboveavg = off_ppg - mean(off_ppg),
      def_ppg_aboveavg = def_ppg - mean(def_ppg)
    )

  ### Making values numeric
  # fmt: skip
  VoAVariables[,4:ncol(VoAVariables)] <- VoAVariables[,4:ncol(VoAVariables)] |> mutate_if(is.character,as.numeric)
}
### end of if statement

##### Creating Weighted Variables, weights change by week #####
### if statement used to set the weights
if (as.integer(cfb_week) == 0) {
  py1weight <- 0.8
  py2weight <- 0.15
  py3weight <- 0.05
} else if (as.integer(cfb_week) == 1) {
  py1weight <- 0.8
  py2weight <- 0.1
  cyweight <- 0.1
} else if (as.integer(cfb_week) == 2) {
  py1weight <- 0.7
  py2weight <- 0.1
  cyweight <- 0.2
} else if (as.integer(cfb_week) == 3) {
  py1weight <- 0.65
  py2weight <- 0.05
  cyweight <- 0.3
} else if (as.integer(cfb_week) == 4) {
  py1weight <- 0.55
  py2weight <- 0.05
  cyweight <- 0.4
} else if (as.integer(cfb_week) == 5) {
  py1weight <- 0.4
  py2weight <- 0.05
  cyweight <- 0.5
} else if (as.integer(cfb_week) == 6) {
  py1weight <- 0.4
  cyweight <- 0.6
} else if (as.integer(cfb_week) == 7) {
  py1weight <- 0.3
  cyweight <- 0.7
} else if (as.integer(cfb_week) == 8) {
  py1weight <- 0.2
  cyweight <- 0.8
} else if (as.integer(cfb_week) == 9) {
  py1weight <- 0.1
  cyweight <- 0.9
} else {
  print("no weights this week, current data only!")
}
if (as.integer(cfb_week) == 0) {
  ##### Preseason Weighted Variables #####
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * py1weight) + (adj_off_ppg_PY2 * py2weight) + (adj_off_ppg_PY3 * py3weight),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * py1weight) + (adj_def_ppg_PY2 * py2weight) + (adj_def_ppg_PY3 * py3weight),
           weighted_net_st_ppg_mean = (adj_net_st_ppg_PY1 * py1weight) + (adj_net_st_ppg_PY2 * py2weight) + (adj_net_st_ppg_PY3 * py3weight),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_epa = (adj_off_epa_PY3 * py3weight) + (adj_off_epa_PY2 * py2weight) + (adj_off_epa_PY1 * py1weight),
           weighted_off_ypp = (adj_off_ypp_PY3 * py3weight) + (adj_off_ypp_PY2 * py2weight) + (adj_off_ypp_PY1 * py1weight),
           weighted_off_success_rt = (off_success_rt_PY3 * py3weight) + (off_success_rt_PY2 * py2weight) + (off_success_rt_PY1 * py1weight),
           weighted_off_explosiveness = (adj_off_explosiveness_PY3 * py3weight) + (adj_off_explosiveness_PY2 * py2weight) + (adj_off_explosiveness_PY1 * py1weight),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY3 * py3weight) + (off_third_conv_rate_PY2 * py2weight) + (off_third_conv_rate_PY1 * py1weight),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY3 * py3weight) + (off_pts_per_opp_PY2 * py2weight) + (off_pts_per_opp_PY1 * py1weight),
           weighted_off_plays_pg = (adj_off_plays_pg_PY3 * py3weight) + (adj_off_plays_pg_PY2 * py2weight) + (adj_off_plays_pg_PY1 * py1weight),
           weighted_def_plays_pg = (adj_def_plays_pg_PY3 * py3weight) + (adj_def_plays_pg_PY2 * py2weight) + (adj_def_plays_pg_PY1 * py1weight),
           weighted_def_epa = (adj_def_epa_PY3 * py3weight) + (adj_def_epa_PY2 * py2weight) + (adj_def_epa_PY1 * py1weight),
           weighted_def_ypp = (adj_def_ypp_PY3 * py3weight) + (adj_def_ypp_PY2 * py2weight) + (adj_def_ypp_PY1 * py1weight),
           weighted_def_success_rt = (def_success_rt_PY3 * py3weight) + (def_success_rt_PY2 * py2weight) + (def_success_rt_PY1 * py1weight),
           weighted_def_explosiveness = (adj_def_explosiveness_PY3 * py3weight) + (adj_def_explosiveness_PY2 * py2weight) + (adj_def_explosiveness_PY1 * py1weight),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY3 * py3weight) + (def_third_conv_rate_PY2 * py2weight) + (def_third_conv_rate_PY1 * py1weight),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY3 * py3weight) + (def_pts_per_opp_PY2 * py2weight) + (def_pts_per_opp_PY1 * py1weight),
           weighted_def_havoc_total = (def_havoc_total_PY3 * py3weight) + (def_havoc_total_PY2 * py2weight) + (def_havoc_total_PY1 * py1weight),
           weighted_net_st_kick_return_yds = ((st_kick_return_yds_PY3 - st_kick_return_yds_allowed_PY3) * py3weight) + ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * py2weight) + ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * py1weight),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY3 - st_punt_return_yds_allowed_PY3) * py3weight) + ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * py2weight) + ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * py1weight),
           weighted_net_fg_rt = ((fg_rate_PY3 - fg_rate_allowed_PY3) * py3weight) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * py2weight) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight),
           weighted_net_fg_made_pg = ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * py3weight) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * py2weight) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * py1weight),
          #  weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * py3weight) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * py2weight) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight),
           weighted_net_adj_st_epa = (net_adj_st_epa_PY3 * py3weight) + (net_adj_st_epa_PY2 * py2weight) + (net_adj_st_epa_PY1 * py1weight)) #,
  #  weighted_mean_oppdef_epa = ((oppdef_epa_PY3 * py3weight) + (oppdef_epa_PY2 * py2weight) + (oppdef_epa_PY1 * py1weight)),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY3 * py3weight) + (oppoff_epa_PY2 * py2weight) + (oppoff_epa_PY1 * py1weight))
} else if (as.integer(cfb_week) <= 5) {
  ##### Week 1-5 Weighted Variables #####
  ### PY 1-2, 1 week of current season
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg * cyweight) + (adj_off_ppg_PY1 * py1weight) + (adj_off_ppg_PY2 * py2weight),
           weighted_def_ppg_mean = (adj_def_ppg * cyweight) + (adj_def_ppg_PY1 * py1weight) + (adj_def_ppg_PY2 * py2weight),
           weighted_net_st_ppg_mean = (adj_net_st_ppg * cyweight) + (adj_net_st_ppg_PY1 * py1weight) + (adj_net_st_ppg_PY2 * py2weight),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_epa = (adj_off_epa_PY2 * py2weight) + (adj_off_epa_PY1 * py1weight) + (adj_off_epa * cyweight),
           weighted_off_ypp = (adj_off_ypp_PY2 * py2weight) + (adj_off_ypp_PY1 * py1weight) + (adj_off_ypp * cyweight),
           weighted_off_success_rt = (off_success_rt_PY2 * py2weight) + (off_success_rt_PY1 * py1weight) + (off_success_rt * cyweight),
           weighted_off_explosiveness = (adj_off_explosiveness_PY2 * py2weight) + (adj_off_explosiveness_PY1 * py1weight) + (adj_off_explosiveness * cyweight),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY2 * py2weight) + (off_third_conv_rate_PY1 * py1weight) + (off_third_conv_rate * cyweight),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY3 * 0.05) + (off_pts_per_opp_PY2 * py2weight) + (off_pts_per_opp_PY1 * py1weight) + (off_pts_per_opp * cyweight),
           weighted_off_plays_pg = (off_plays_pg_PY2 * py2weight) + (off_plays_pg_PY1 * py1weight) + (off_plays_pg * cyweight),
           weighted_def_plays_pg = (def_plays_pg_PY3 * 0.05) + (def_plays_pg_PY2 * py2weight) + (def_plays_pg_PY1 * py1weight) + (def_plays_pg * cyweight),
           weighted_def_epa = (adj_def_epa_PY3 * 0.05) + (adj_def_epa_PY2 * py2weight) + (adj_def_epa_PY1 * py1weight) + (adj_def_epa * cyweight),
           weighted_def_ypp = (adj_def_ypp_PY3 * 0.05) + (adj_def_ypp_PY2 * py2weight) + (adj_def_ypp_PY1 * py1weight) + (adj_def_ypp * cyweight),
           weighted_def_success_rt = (def_success_rt_PY3 * 0.05) + (def_success_rt_PY2 * py2weight) + (def_success_rt_PY1 * py1weight) + (def_success_rt * cyweight),
           weighted_def_explosiveness = (adj_def_explosiveness_PY3 * 0.05) + (adj_def_explosiveness_PY2 * py2weight) + (adj_def_explosiveness_PY1 * py1weight) + (adj_def_explosiveness * cyweight),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY3 * 0.05) + (def_third_conv_rate_PY2 * py2weight) + (def_third_conv_rate_PY1 * py1weight) + (def_third_conv_rate * cyweight),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY3 * 0.05) + (def_pts_per_opp_PY2 * py2weight) + (def_pts_per_opp_PY1 * py1weight) + (def_pts_per_opp * cyweight),
           weighted_def_havoc_total = (def_havoc_total_PY3 * 0.05) + (def_havoc_total_PY2 * py2weight) + (def_havoc_total_PY1 * py1weight) + (def_havoc_total * cyweight),
           weighted_net_st_kick_return_yds = ((st_kick_return_yds_PY3 - st_kick_return_yds_allowed_PY3) * 0.05) + ((st_kick_return_yds_PY2 - st_kick_return_yds_allowed_PY2) * py2weight) + ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * py1weight) + ((st_kick_return_yds - st_kick_return_yds_allowed) * cyweight),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY3 - st_punt_return_yds_allowed_PY3) * 0.05) + ((st_punt_return_yds_PY2 - st_punt_return_yds_allowed_PY2) * py2weight) + ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * py1weight) + ((st_punt_return_yds - st_punt_return_yds_allowed) * cyweight),
           weighted_net_fg_rate = ((fg_rate_PY3 - fg_rate_allowed_PY3) * 0.05) + ((fg_rate_PY2 - fg_rate_allowed_PY2) * py2weight) + ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight) + ((fg_rate - fg_rate_allowed) * cyweight),
           weighted_net_fg_made_pg = ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) * 0.05) + ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * py2weight) + ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * py1weight) + ((fg_made_pg - fg_made_pg_allowed) * cyweight),
          #  weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * 0.05) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * py2weight) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight) + ((xpts_pg - xpts_allowed_pg) * cyweight),
           weighted_net_adj_st_epa = (net_adj_st_epa_PY3 * 0.05) + (net_adj_st_epa_PY2 * py2weight) + (net_adj_st_epa_PY1 * py1weight) + (net_adj_st_epa * cyweight)) #,
  #  weighted_mean_oppdef_epa = (oppdef_epa_PY3 * 0.05) + (oppdef_epa_PY2 * py2weight) + (oppdef_epa_PY1 * py1weight) + (oppdef_epa * cyweight),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY3 * 0.05) + (oppoff_epa_PY2 * py2weight) + (oppoff_epa_PY1 * py1weight) + (oppoff_epa * cyweight))
} else if (as.integer(cfb_week) <= 9) {
  ##### Week 6-9 Weighted Variables #####
  ### only PY1 and current data
  ### adding weighted variables
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(weighted_off_ppg_mean = (adj_off_ppg_PY1 * py1weight) + (adj_off_ppg * cyweight),
           weighted_def_ppg_mean = (adj_def_ppg_PY1 * py1weight) + (adj_def_ppg * cyweight),
           weighted_net_st_ppg_mean = (net_st_ppg_PY1 * py1weight) + (net_st_ppg * cyweight),
           off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
           def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
           weighted_off_epa = (adj_off_epa_PY1 * py1weight) + (adj_off_epa * cyweight),
           weighted_off_ypp = (adj_off_ypp_PY1 * py1weight) + (adj_off_ypp * cyweight),
           weighted_off_success_rt = (off_success_rt_PY1 * py1weight) + (off_success_rt * cyweight),
           weighted_off_explosiveness = (adj_off_explosiveness_PY1 * py1weight) + (adj_off_explosiveness * cyweight),
           weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * py1weight) + (off_third_conv_rate * cyweight),
           weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * py1weight) + (off_pts_per_opp * cyweight),
           weighted_off_plays_pg = (off_plays_pg_PY1 * py1weight) + (off_plays_pg * cyweight),
           weighted_def_plays_pg = (def_plays_pg_PY1 * py1weight) + (def_plays_pg * cyweight),
           weighted_def_epa = (adj_def_epa_PY1 * py1weight) + (adj_def_epa * cyweight),
           weighted_def_ypp = (adj_def_ypp_PY1 * py1weight) + (adj_def_ypp * cyweight),
           weighted_def_success_rt = (def_success_rt_PY1 * py1weight) + (def_success_rt * cyweight),
           weighted_def_explosiveness = (adj_def_explosiveness_PY1 * py1weight) + (adj_def_explosiveness * cyweight),
           weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * py1weight) + (def_third_conv_rate * cyweight),
           weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * py1weight) + (def_pts_per_opp * cyweight),
           weighted_def_havoc_total = (def_havoc_total_PY1 * py1weight) + (def_havoc_total * cyweight),
           weighted_net_st_kick_return_yds = ((st_kick_return_yds_PY1 - st_kick_return_yds_allowed_PY1) * py1weight) + ((st_kick_return_yds - st_kick_return_yds_allowed) * cyweight),
           weighted_net_punt_return_yds = ((st_punt_return_yds_PY1 - st_punt_return_yds_allowed_PY1) * py1weight) + ((st_punt_return_yds - st_punt_return_yds_allowed) * cyweight),
           weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight) + ((fg_rate - fg_rate_allowed) * cyweight),
           weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * py1weight) + ((fg_made_pg - fg_made_pg_allowed) * cyweight),
          #  weighted_net_xpts_pg = ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight) + ((xpts_pg - xpts_allowed_pg) * cyweight),
           weighted_net_adj_st_epa = (net_adj_st_epa_PY1 * py1weight) + (net_adj_st_epa * cyweight)) #,
  #  weighted_mean_oppdef_epa = (oppdef_epa_PY1 * py1weight) + (oppdef_epa * cyweight),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY1 * py1weight) + (oppoff_epa * cyweight))
} else {
  print("no weighted variables, all current season data")
}


##### Eliminating NAs, fixing conferences, adding Week number to VoA Variables #####
### eliminating NAs that may still exist
### leaving this outside an if statement because this could be an issue regardless of season or CFB_Week
### currently commented out because I added this fix to each individual stat pull in function
### uncommented it because I must once again ask that Florida International University go fuck itself
# if (as.integer(cfb_week) %in% c(0, 1, 9:16)) {
#   VoAVariables$recruit_pts[is.na(VoAVariables$recruit_pts)] <- 0
#   VoAVariables$recruit_pts_PY3[is.na(VoAVariables$recruit_pts_PY3)] <- 0
# }

### Fixing conference errors for Week 0 (Preseason)
# if (as.integer(cfb_week) == 0) {
#   VoAVariables <- VoAVariables |>
#     select(-conference)
#   current_conferences <- cfbd_team_info(year = as.integer(year)) |>
#     filter(school %in% VoAVariables$team) |>
#     select(school, conference)
#   colnames(current_conferences) <- c("team", "conference")
#   VoAVariables <- full_join(VoAVariables, current_conferences, by = "team") |>
#     relocate(conference, .after = team)
# } else {
#   print("current season conferences should be in use!")
# }

## Adding Column with CFB Week number
# same number for each team, numeric version of number input in readline function at beginning of script
VoAVariables <- VoAVariables |>
  mutate(CFB_Week = rep(as.integer(cfb_week), nrow(VoAVariables)), .before = 2)


##### checking which column to start ranking at #####
if (as.integer(cfb_week) == 0) {
  VoA_Ncols <- ncol(VoAVariables) + 1
  VoATrain_Ncols <- ncol(VoAVariablesTrain_PY1) + 1
} else {
  VoA_Ncols <- ncol(VoAVariables) + 1
}
# if (as.integer(cfb_week) == 0 | as.integer(cfb_week) == 1 | as.integer(cfb_week) == 2 | as.integer(cfb_week) == 6 | as.integer(cfb_week) == 9){
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
if (as.integer(cfb_week) == 0) {
  ##### Week 0 Variable Ranks #####
  ### applying end of season rank process to dfs which will be used to train Stan model first
  VoAVariablesTrain_PY1 <- rank_voa_cols(VoAVariablesTrain_PY1)
  VoAVariablesTrain_PY2 <- rank_voa_cols(VoAVariablesTrain_PY2)
  VoAVariablesTrain_PY3 <- rank_voa_cols(VoAVariablesTrain_PY3)
  # VoAVariablesTrain_PY4 <- rank_voa_cols(VoAVariablesTrain_PY4)
  ### PY3 ranks added first
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(Rank_Comp_Pct_PY3 = dense_rank(desc(off_comp_pct_PY3)),
           Rank_off_pass_ypa_PY3 = dense_rank(desc(off_pass_ypa_PY3)),
           Rank_off_pass_ypr_PY3 = dense_rank(desc(off_pass_ypr_PY3)),
          #  Rank_int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_off_rush_ypa_PY3 = dense_rank(desc(off_rush_ypa_PY3)),
           Rank_off_turnovers_pg_PY3 = dense_rank(off_turnovers_pg_PY3),
           Rank_third_conv_rate_PY3 = dense_rank(desc(off_third_conv_rate_PY3)),
           Rank_off_fourth_conv_rate_PY3 = dense_rank(desc(off_fourth_conv_rate_PY3)),
          #  Rank_penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
          #  Rank_yds_per_penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_st_kick_return_yds_PY3 = dense_rank(desc(st_kick_return_yds_PY3)),
           Rank_punt_return_yds_PY3 = dense_rank(desc(st_punt_return_yds_PY3)),
           Rank_off_ypg_PY3 = dense_rank(desc(off_ypg_PY3)),
           Rank_off_pass_ypg_PY3 = dense_rank(desc(off_pass_ypg_PY3)),
           Rank_off_rush_ypg_PY3 = dense_rank(desc(off_rush_ypg_PY3)),
          #  Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(adj_off_ypp_PY3)),
          #  Rank_def_ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_EPA_PY3 = dense_rank(desc(adj_off_epa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rt_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(adj_off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rt_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Standard_Down_EPA_PY3 = dense_rank(desc(off_standard_downs_epa_PY3)),
           Rank_Off_Standard_Down_Success_Rt_PY3 = dense_rank(desc(off_standard_downs_success_rt_PY3)),
           Rank_Off_Standard_Down_Explosiveness_PY3 = dense_rank(desc(off_standard_downs_explosiveness_PY3)),
           Rank_Off_Pass_Down_EPA_PY3 = dense_rank(desc(off_passing_downs_epa_PY3)),
           Rank_Off_Pass_Down_Success_Rt_PY3 = dense_rank(desc(off_passing_downs_success_rt_PY3)),
           Rank_Off_Pass_Down_Explosiveness_PY3 = dense_rank(desc(off_passing_downs_explosiveness_PY3)),
           Rank_Off_Rush_Play_EPA_PY3 = dense_rank(desc(off_rush_epa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rush_success_rt_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rush_explosiveness_PY3)),
           Rank_Off_Pass_Play_EPA_PY3 = dense_rank(desc(off_pass_epa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_pass_success_rt_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_pass_explosiveness_PY3)),
           Rank_Def_EPA_PY3 = dense_rank(adj_def_epa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rt_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(adj_def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rt_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           # Rank_def_second_Lvl_Yds_PY3 = dense_rank(def_second_lvl_yds_PY3),
           # Rank_def_open_Field_Yds_PY3 = dense_rank(def_open_field_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           # Rank_def_havoc_front_Seven_PY3 = dense_rank(desc(def_havoc_front_seven_PY3)),
           # Rank_def_havoc_db_PY3 = dense_rank(desc(def_havoc_db_PY3)),
           Rank_Def_Standard_Down_EPA_PY3 = dense_rank(def_standard_downs_epa_PY3),
           Rank_Def_Standard_Down_Success_Rt_PY3 = dense_rank(def_standard_downs_success_rt_PY3),
           Rank_Def_Standard_Down_Explosiveness_PY3 = dense_rank(def_standard_downs_explosiveness_PY3),
           Rank_Def_Pass_Down_EPA_PY3 = dense_rank(def_passing_downs_epa_PY3),
           Rank_Def_Pass_Down_Success_Rt_PY3 = dense_rank(def_passing_downs_success_rt_PY3),
           Rank_Def_Pass_Down_Explosiveness_PY3 = dense_rank(def_passing_downs_explosiveness_PY3),
           Rank_Def_Rush_Play_EPA_PY3 = dense_rank(def_rush_epa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rush_success_rt_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rush_explosiveness_PY3),
           Rank_Def_Pass_Play_EPA_PY3 = dense_rank(def_pass_epa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_pass_success_rt_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_pass_explosiveness_PY3),
           # Rank_recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
           Rank_EPA_diff_PY3 = dense_rank(desc(EPA_diff_PY3)),
           Rank_SuccessRt_diff_PY3 = dense_rank(desc(SuccessRt_diff_PY3)),
           Rank_HavocRt_diff_PY3 = dense_rank(desc(HavocRt_diff_PY3)),
           Rank_Explosiveness_diff_PY3 = dense_rank(desc(Explosiveness_diff_PY3)),
           ## PY2 ranks
           Rank_Comp_Pct_PY2 = dense_rank(desc(off_comp_pct_PY2)),
           Rank_off_pass_ypa_PY2 = dense_rank(desc(off_pass_ypa_PY2)),
           Rank_off_pass_ypr_PY2 = dense_rank(desc(off_pass_ypr_PY2)),
           # Rank_int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_off_rush_ypa_PY2 = dense_rank(desc(off_rush_ypa_PY2)),
           Rank_off_turnovers_pg_PY2 = dense_rank(off_turnovers_pg_PY2),
           Rank_third_conv_rate_PY2 = dense_rank(desc(off_third_conv_rate_PY2)),
           Rank_off_fourth_conv_rate_PY2 = dense_rank(desc(off_fourth_conv_rate_PY2)),
           # Rank_penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           # Rank_yds_per_penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_st_kick_return_yds_PY2 = dense_rank(desc(st_kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(st_punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rt_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rt_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rt_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rt_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rt_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rt_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rt_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rt_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rt_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rt_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rt_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rt_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           # Rank_recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           ## PY2 weighted twice
           # Rank_recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ## PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_st_kick_return_yds_PY1 = dense_rank(desc(st_kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(st_punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1_col2 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1_col2 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1_col2 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1_col2 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1_col2 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1_col2 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1_col2 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1_col2 = dense_rank(yards_per_penalty_PY1),
           Rank_off_ypg_PY1_col2 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1_col2 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1_col2 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1_col2 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col2 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)))
} else if (as.integer(cfb_week) == 1) {
  ##### Week 1 Variable Ranks #####
  # PY3 weighted 1x, PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  ## PY3 ranks added first, weighted once
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(
           ### PY2 ranks
           Rank_Comp_Pct_PY2 = dense_rank(desc(off_comp_pct_PY2)),
           Rank_off_pass_ypa_PY2 = dense_rank(desc(off_pass_ypa_PY2)),
           Rank_off_pass_ypr_PY2 = dense_rank(desc(off_pass_ypr_PY2)),
           # Rank_int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_off_rush_ypa_PY2 = dense_rank(desc(off_rush_ypa_PY2)),
           Rank_off_turnovers_pg_PY2 = dense_rank(off_turnovers_pg_PY2),
           Rank_third_conv_rate_PY2 = dense_rank(desc(off_third_conv_rate_PY2)),
           Rank_off_fourth_conv_rate_PY2 = dense_rank(desc(off_fourth_conv_rate_PY2)),
           # Rank_penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           # Rank_yds_per_penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_kick_return_yds_PY2 = dense_rank(desc(st_kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(st_punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
          #  # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
          #  # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rt_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rt_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rt_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rt_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rt_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rt_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rt_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rt_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
          #  # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
          #  # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
          #  # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
          #  # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rt_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rt_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rt_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rt_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           ## PY2 weighted twice
           Rank_Comp_Pct_PY2_col2 = dense_rank(desc(off_comp_pct_PY2)),
           Rank_off_pass_ypa_PY2_col2 = dense_rank(desc(off_pass_ypa_PY2)),
           Rank_off_pass_ypr_PY2_col2 = dense_rank(desc(off_pass_ypr_PY2)),
           Rank_off_rush_ypa_PY2_col2 = dense_rank(desc(off_rush_ypa_PY2)),
           Rank_off_turnovers_pg_PY2_col2 = dense_rank(off_turnovers_pg_PY2),
           Rank_third_conv_rate_PY2_col2 = dense_rank(desc(off_third_conv_rate_PY2)),
           Rank_off_fourth_conv_rate_PY2_col2 = dense_rank(desc(off_fourth_conv_rate_PY2)),
           Rank_off_ypg_PY2_col2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2_col2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2_col2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2_col2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2_col2 = dense_rank(desc(adj_off_ypp_PY2)),
          #  # Rank_def_ints_pg_PY2_col2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2_col2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rt_PY2)),
           Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rt_PY2),
           Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Standard_Down_EPA_PY2_col2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rt_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2_col2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rt_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2_col2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rush_success_rt_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2_col2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_pass_success_rt_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2_col2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rt_PY2),
           Rank_Def_Explosiveness_PY2_col2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rt_PY2)),
           Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2_col2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rt_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2_col2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rt_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2_col2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rush_success_rt_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2_col2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_pass_success_rt_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2_col2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2_col2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2_col2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2_col2 = dense_rank(desc(Explosiveness_diff_PY2)),
           # Rank_recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
           ### PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_st_kick_return_yds_PY1 = dense_rank(desc(st_kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(st_punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1_col2 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1_col2 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1_col2 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1_col2 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1_col2 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1_col2 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           Rank_off_ypg_PY1_col2 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1_col2 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1_col2 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1_col2 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col2 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1_col3 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1_col3 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1_col3 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1_col3 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1_col3 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1_col3 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           Rank_off_ypg_PY1_col3 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1_col3 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1_col3 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1_col3 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col3 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col3 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col3 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col3 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col3 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col3 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col3 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col3 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col3 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col3 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col3 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col3 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col3 = dense_rank(desc(Explosiveness_diff_PY1)),
           ### incoming recruiting class, weighted once
           # Rank_recruit_Pts = dense_rank(desc(recruit_pts)),
           ### Ranking current stats
           Rank_Comp_Pct = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct = dense_rank(int_pct),
           Rank_off_rush_ypa = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
           # Rank_penalty_Yds_pg = dense_rank(penalty_yds_pg),
           # Rank_yds_per_penalty = dense_rank(yards_per_penalty),
           Rank_st_kick_return_yds = dense_rank(desc(st_kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(st_punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rt),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
          #  # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
          #  # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
          #  # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
          #  # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Extra weighted variables for current year
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rt),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rt),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff_col2 = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else if (as.integer(cfb_week) <= 3) {
  ##### Weeks 2-3 Variable Ranks #####
  # PY2 weighted 2x, PY1 weighted 3x, current weighted 1x
  # fmt: skip
  VoAVariables <- VoAVariables |>
    ## PY2 ranks
    mutate(Rank_Comp_Pct_PY2 = dense_rank(desc(off_comp_pct_PY2)),
           Rank_off_pass_ypa_PY2 = dense_rank(desc(off_pass_ypa_PY2)),
           Rank_off_pass_ypr_PY2 = dense_rank(desc(off_pass_ypr_PY2)),
           # Rank_int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_off_rush_ypa_PY2 = dense_rank(desc(off_rush_ypa_PY2)),
           Rank_off_turnovers_pg_PY2 = dense_rank(off_turnovers_pg_PY2),
           Rank_third_conv_rate_PY2 = dense_rank(desc(off_third_conv_rate_PY2)),
           Rank_off_fourth_conv_rate_PY2 = dense_rank(desc(off_fourth_conv_rate_PY2)),
           # Rank_penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           # Rank_yds_per_penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_st_kick_return_yds_PY2 = dense_rank(desc(st_kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(st_punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rt_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rt_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rt_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rt_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rt_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rt_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rt_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rt_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rt_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rt_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rt_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rt_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           # Rank_recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           ## PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_st_kick_return_yds_PY1 = dense_rank(desc(st_kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(st_punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col2 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1_col2 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1_col2 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1_col2 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1_col2 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1_col2 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1_col2 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1_col2 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           Rank_off_ypg_PY1_col2 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1_col2 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1_col2 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1_col2 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col2 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1_col2 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1_col2 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col2 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           ## PY1 weighted 3 times
           Rank_Comp_Pct_PY1_col3 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1_col3 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1_col3 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1_col3 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1_col3 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1_col3 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1_col3 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1_col3 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           Rank_off_ypg_PY1_col3 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1_col3 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1_col3 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1_col3 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1_col3 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1_col3 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1_col3 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1_col3 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1_col3 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col3 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col3 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1_col3 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1_col3 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1_col3 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col3 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1_col3 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1_col3 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1_col3 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1_col3 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col3 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col3 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col3 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col3 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col3 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col3 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col3 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col3 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col3 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col3 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1_col3 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col3 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col3 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1_col3 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col3 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col3 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col3 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col3 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col3 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col3 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col3 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col3 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col3 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col3 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col3 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col3 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col3 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col3 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col3 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col3 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col3 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col3 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col3 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col3 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col3 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col3 = dense_rank(desc(Explosiveness_diff_PY1)),
           ## Ranking current stats
           Rank_Comp_Pct = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct = dense_rank(int_pct),
           Rank_off_rush_ypa = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
           # Rank_penalty_Yds_pg = dense_rank(penalty_yds_pg),
           # Rank_yds_per_penalty = dense_rank(yards_per_penalty),
           Rank_st_kick_return_yds = dense_rank(desc(st_kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(st_punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rt),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Extra weighted variables for current year
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rt),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rt),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff_col2 = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else if (as.integer(cfb_week) <= 5) {
  ##### Weeks 4-5 Variable Ranks #####
  # PY2 weighted 1x, PY1 weighted 1x, current weighted 2x
  ## PY2 ranks
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(Rank_Comp_Pct_PY2 = dense_rank(desc(off_comp_pct_PY2)),
           Rank_off_pass_ypa_PY2 = dense_rank(desc(off_pass_ypa_PY2)),
           Rank_off_pass_ypr_PY2 = dense_rank(desc(off_pass_ypr_PY2)),
           # Rank_int_Pct_PY2 = dense_rank(int_pct_PY2),
           Rank_off_rush_ypa_PY2 = dense_rank(desc(off_rush_ypa_PY2)),
           Rank_off_turnovers_pg_PY2 = dense_rank(off_turnovers_pg_PY2),
           Rank_third_conv_rate_PY2 = dense_rank(desc(off_third_conv_rate_PY2)),
           Rank_off_fourth_conv_rate_PY2 = dense_rank(desc(off_fourth_conv_rate_PY2)),
           # Rank_penalty_Yds_pg_PY2 = dense_rank(penalty_yds_pg_PY2),
           # Rank_yds_per_penalty_PY2 = dense_rank(yards_per_penalty_PY2),
           Rank_st_kick_return_yds_PY2 = dense_rank(desc(st_kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(st_punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rt_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rt_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
           Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
           Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY2 = dense_rank(desc(off_field_pos_avg_predicted_points_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
           Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rt_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rt_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rt_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rt_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rt_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rt_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rt_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rt_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rt_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rt_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           ## PY1 ranks
           Rank_Comp_Pct_PY1 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_st_kick_return_yds_PY1 = dense_rank(desc(st_kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(st_punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           ### Ranking current stats
           Rank_Comp_Pct = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct = dense_rank(int_pct),
           Rank_off_rush_ypa = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
           # Rank_penalty_Yds_pg = dense_rank(penalty_yds_pg),
           # Rank_yds_per_penalty = dense_rank(yards_per_penalty),
           Rank_st_kick_return_yds = dense_rank(desc(st_kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(st_punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rt),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Current stats weighted 2x
           Rank_Comp_Pct_col2 = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa_col2 = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr_col2 = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct_col2 = dense_rank(int_pct),
           Rank_off_rush_ypa_col2 = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg_col2 = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate_col2 = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate_col2 = dense_rank(desc(off_fourth_conv_rate)),
           Rank_off_ypg_col2 = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg_col2 = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg_col2 = dense_rank(desc(off_rush_ypg)),
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front_col2 = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB_col2 = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rt),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds_col2 = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff_col2 = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else if (as.integer(cfb_week) <= 8) {
  ##### Weeks 6-8 Variable Ranks #####
  # PY1 weighted 1x, current weighted 2x
  # fmt: skip
  VoAVariables <- VoAVariables |>
    ## PY1 ranks
    mutate(Rank_Comp_Pct_PY1 = dense_rank(desc(off_comp_pct_PY1)),
           Rank_off_pass_ypa_PY1 = dense_rank(desc(off_pass_ypa_PY1)),
           Rank_off_pass_ypr_PY1 = dense_rank(desc(off_pass_ypr_PY1)),
           # Rank_int_Pct_PY1 = dense_rank(int_pct_PY1),
           Rank_off_rush_ypa_PY1 = dense_rank(desc(off_rush_ypa_PY1)),
           Rank_off_turnovers_pg_PY1 = dense_rank(off_turnovers_pg_PY1),
           Rank_third_conv_rate_PY1 = dense_rank(desc(off_third_conv_rate_PY1)),
           Rank_off_fourth_conv_rate_PY1 = dense_rank(desc(off_fourth_conv_rate_PY1)),
           # Rank_penalty_Yds_pg_PY1 = dense_rank(penalty_yds_pg_PY1),
           # Rank_yds_per_penalty_PY1 = dense_rank(yards_per_penalty_PY1),
           Rank_st_kick_return_yds_PY1 = dense_rank(desc(st_kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(st_punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rt_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rt_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
           Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_PY1 = dense_rank(desc(off_field_pos_avg_predicted_points_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
           Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rt_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rt_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rt_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rt_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rt_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rt_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rt_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rt_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rt_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rt_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           # Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
           ## Ranking current stats
           Rank_Comp_Pct = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct = dense_rank(int_pct),
           Rank_off_rush_ypa = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
           # Rank_penalty_Yds_pg = dense_rank(penalty_yds_pg),
           # Rank_yds_per_penalty = dense_rank(yards_per_penalty),
           Rank_st_kick_return_yds = dense_rank(desc(st_kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(st_punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
           Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
           Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rt)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rt)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rt),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Current stats weighted 2x
           Rank_Comp_Pct_col2 = dense_rank(desc(off_comp_pct)),
           Rank_off_pass_ypa_col2 = dense_rank(desc(off_pass_ypa)),
           Rank_off_pass_ypr_col2 = dense_rank(desc(off_pass_ypr)),
           # Rank_int_Pct_col2 = dense_rank(int_pct),
           Rank_off_rush_ypa_col2 = dense_rank(desc(off_rush_ypa)),
           Rank_off_turnovers_pg_col2 = dense_rank(off_turnovers_pg),
           Rank_third_conv_rate_col2 = dense_rank(desc(off_third_conv_rate)),
           Rank_off_fourth_conv_rate_col2 = dense_rank(desc(off_fourth_conv_rate)),
           Rank_off_ypg_col2 = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg_col2 = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg_col2 = dense_rank(desc(off_rush_ypg)),
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg_col2 = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rt)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rt),
           Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
           Rank_Off_Second_Lvl_Yds_col2 = dense_rank(desc(off_second_lvl_yds)),
           Rank_Off_Open_Field_Yds_col2 = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Field_Pos_Avg_Predicted_Pts_col2 = dense_rank(desc(off_field_pos_avg_predicted_points)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rt)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rt)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rt),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rt)),
           Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds_col2 = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rt),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rt),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rt),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rt),
           Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff_col2 = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff_col2 = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff_col2 = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff_col2 = dense_rank(desc(Explosiveness_diff)))
} else {
  ##### Week 9-End of Season Variable Ranks #####
  ## Recruiting points no longer included because I got sick of troubleshooting random NAs, team names not matching up, and not all teams being included in the recruiting endpoint
  # current will be only data source used, everything weighted "1x" (aside from special variables, and recruiting)
  ## Ranking current stats
  VoAVariables <- rank_voa_cols(VoAVariables)
}
### end of if statements

##### calculating the mean stat ranking, VoA_Output #####
if (as.integer(cfb_week) == 0) {
  ## correcting "season" column to reflect the season for which these rankings are being produced
  # VoAVariables$season <- rep(as.numeric(year), nrow(VoAVariables))
  ### Append new column of Model output, which is the mean of all rank columns
  VoAVariablesTrain_PY1 <- VoAVariablesTrain_PY1 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY1[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY1)
      ]))
    )
  VoAVariablesTrain_PY2 <- VoAVariablesTrain_PY2 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY2[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY2)
      ]))
    )
  VoAVariablesTrain_PY3 <- VoAVariablesTrain_PY3 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY3[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY3)
      ]))
    )
  # VoAVariablesTrain_PY4 <- VoAVariablesTrain_PY4 |>
  #   mutate(
  #     VoA_Output = (rowMeans(VoAVariablesTrain_PY4[,
  #       VoATrain_Ncols:ncol(VoAVariablesTrain_PY4)
  #     ]))
  #   )
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[, VoA_Ncols:ncol(VoAVariables)]))
    )
  ## Append column of VoA Final Rankings
  # VoAVariables <- VoAVariables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[, VoA_Ncols:ncol(VoAVariables)]))
    )
  ## Append column of VoA Final Rankings
  # VoAVariables <- VoAVariables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
## End of if statement

##### Using Intial VoA Outputs to add in conference strength metric #####
if (as.integer(cfb_week) == 0) {
  VoAVariablesTrain_PY1 <- calc_output_conf_avg(VoAVariablesTrain_PY1)
  VoAVariablesTrain_PY2 <- calc_output_conf_avg(VoAVariablesTrain_PY2)
  VoAVariablesTrain_PY3 <- calc_output_conf_avg(VoAVariablesTrain_PY3)
  # VoAVariablesTrain_PY4 <- calc_output_conf_avg(VoAVariablesTrain_PY4)
  VoAVariables <- calc_output_conf_avg(VoAVariables)
} else {
  VoAVariables <- calc_output_conf_avg(VoAVariables)
}


##### Re running rowMeans function to get VoA Output #####
### script wouldn't run properly without a real number in the later weeks so I'll have to come back and edit the number in during the season as I figure out how big VoAVariables gets
if (as.integer(cfb_week) == 0) {
  ### Append new column of Model output, which is the mean of all rank columns + conference averages
  VoAVariablesTrain_PY1 <- VoAVariablesTrain_PY1 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY1[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY1)
      ]))
    )
  VoAVariablesTrain_PY2 <- VoAVariablesTrain_PY2 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY2[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY2)
      ]))
    )
  VoAVariablesTrain_PY3 <- VoAVariablesTrain_PY3 |>
    mutate(
      VoA_Output = (rowMeans(VoAVariablesTrain_PY3[,
        VoATrain_Ncols:ncol(VoAVariablesTrain_PY3)
      ]))
    )
  # VoAVariablesTrain_PY4 <- VoAVariablesTrain_PY4 |>
  #   mutate(
  #     VoA_Output = (rowMeans(VoAVariablesTrain_PY4[,
  #       VoATrain_Ncols:ncol(VoAVariablesTrain_PY4)
  #     ]))
  #   )
  ### binding train dfs together since there are no more calculations to perform separately
  # VoATrain <- rbind(
  #   VoAVariablesTrain_PY1,
  #   rbind(
  #     VoAVariablesTrain_PY2,
  #     rbind(VoAVariablesTrain_PY3, VoAVariablesTrain_PY4)
  #   )
  # )
  VoATrain <- rbind(
    VoAVariablesTrain_PY1,
    rbind(VoAVariablesTrain_PY2, VoAVariablesTrain_PY3)
  )
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[, VoA_Ncols:ncol(VoAVariables)]))
    )
  ## Append column of VoA Final Rankings
  # VoAVariables <- VoAVariables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
} else {
  ## Append new column of Model output, which is the mean of all variables in VoARanks
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[, VoA_Ncols:ncol(VoAVariables)]))
    )
  ## Append column of VoA Final Rankings
  # VoAVariables <- VoAVariables |>
  #   mutate(VoA_Ranking = dense_rank(VoA_Output))
}
### End of if statement

## using Stan function to create FPI/SP+ like metric
# includes PPA, success rate, explosiveness, VoA_Output, VoA's Conference_Strength, and pts_per_opp (offense and defense where applicable)
# set.seed(802)

##### using Stan to create FPI/SP+ like metrics #####
if (as.numeric(cfb_week) == 0) {
  ##### Week 0 Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(
    N = nrow(VoATrain),
    off_ppg = VoATrain$adj_off_ppg,
    off_epa = VoATrain$adj_off_epa,
    off_ypp = VoATrain$adj_off_ypp,
    off_success_rate = VoATrain$off_success_rt,
    off_explosiveness = VoATrain$adj_off_explosiveness,
    third_conv_rate = VoATrain$off_third_conv_rate,
    off_pts_per_opp = VoATrain$off_pts_per_opp,
    off_plays_pg = VoATrain$adj_off_plays_pg,
    VoA_Output = 1 / VoATrain$VoA_Output,
    Conference_Strength = 1 / VoATrain$Conf_Rk
  )

  ### compile the stan model
  Off_VoA_model <- cmdstan_model(
    stan_file = here("Scripts", "Stan", "Off_VoA.stan")
  )
  ### fitting stan model
  set.seed(802)
  Off_VoA_fit <- Off_VoA_model$sample(
    data = Off_VoA_datalist,
    chains = 3,
    iter_sampling = 7500,
    iter_warmup = 2500,
    seed = 802
  )
  Off_VoA_fit

  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_off_epa",
      "beta_off_ypp",
      "beta_off_success_rate",
      "beta_off_explosiveness",
      "beta_third_conv_rate",
      "beta_off_pts_per_opp",
      "beta_off_plays_pg",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  # Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  # set.seed(802)
  # for (p in 1:length(Off_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     Off_VoA_Rating <- rnorm(
  #       1,
  #       mean = Off_VoA_pars$b0[p] +
  #         Off_VoA_pars$beta_off_epa[p] * VoAVariables$weighted_off_epa[t] +
  #         Off_VoA_pars$beta_off_ypp[p] * VoAVariables$weighted_off_ypp[t] +
  #         Off_VoA_pars$beta_off_success_rate[p] *
  #           VoAVariables$weighted_off_success_rt[t] +
  #         Off_VoA_pars$beta_off_explosiveness[p] *
  #           VoAVariables$weighted_off_explosiveness[t] +
  #         Off_VoA_pars$beta_third_conv_rate[p] *
  #           VoAVariables$weighted_off_third_conv_rate[t] +
  #         Off_VoA_pars$beta_off_pts_per_opp[p] *
  #           VoAVariables$weighted_off_pts_per_opp[t] +
  #         Off_VoA_pars$beta_off_plays_pg[p] *
  #           VoAVariables$weighted_off_plays_pg[t] +
  #         Off_VoA_pars$beta_VoA_Output[p] * (1 / VoAVariables$VoA_Output[t]) +
  #         Off_VoA_pars$beta_Conference_Strength[p] *
  #           (1 / VoAVariables$Conf_Rk[t]),
  #       sd = Off_VoA_pars$sigma[p]
  #     )
  #     Off_VoA_Ratings[p, t] <- Off_VoA_Rating
  #   }
  # }

  ### Create the Design Matrix (Teams x Predictors)
  OffDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_off_epa = VoAVariables$weighted_off_epa,
    beta_off_ypp = VoAVariables$weighted_off_ypp,
    beta_off_success_rate = VoAVariables$weighted_off_success_rt,
    beta_off_explosiveness = VoAVariables$weighted_off_explosiveness,
    beta_third_conv_rate = VoAVariables$weighted_off_third_conv_rate,
    beta_off_pts_per_opp = VoAVariables$weighted_off_pts_per_opp,
    beta_off_plays_pg = VoAVariables$weighted_off_plays_pg,
    beta_VoA_Output = 1 / VoAVariables$VoA_Output,
    beta_Conference_Strength = 1 / VoAVariables$Conf_Rk
  ))

  #### Parameter Matrix (Posterior samples x Predictors)
  Off_VoA_pars_matrix <- as.matrix(Off_VoA_pars[, colnames(OffDesignMatrix)])

  ### Calculate Means for ALL (p, t) pairs in one operation
  ### Off_VoA_pars_matrix %*% t(DesignMatrix) produces a matrix of size (N_draws x N_teams)
  OffMeans_matrix <- Off_VoA_pars_matrix %*% t(OffDesignMatrix)

  ### Add normal noise vectorized using the sigma array
  P <- length(Off_VoA_pars$b0)
  T_num <- nrow(VoAVariables)

  ### applying rnorm to generate a matrix of ratings using the matrix of samples from the posterior distributions
  set.seed(802)
  Off_VoA_Ratings <- matrix(
    rnorm(P * T_num, mean = OffMeans_matrix, sd = Off_VoA_pars$sigma),
    nrow = P,
    ncol = T_num
  )

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings, 2, mean)
  MedianPred <- apply(Off_VoA_Ratings, 2, median)
  Upper <- apply(Off_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(Off_VoA_Ratings, 2, quantile, prob = .05)

  VoAVariables$OffVoA_MeanRating <- MeanPred
  VoAVariables$OffVoA_MedRating <- MedianPred
  VoAVariables$OffVoA_95PctRating <- Upper
  VoAVariables$OffVoA_05PctRating <- Lower

  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  Def_VoA_datalist <- list(
    N = nrow(VoATrain),
    def_ppg = VoATrain$adj_def_ppg,
    def_epa = VoATrain$adj_def_epa,
    def_ypp = VoATrain$adj_def_ypp,
    def_success_rate = VoATrain$def_success_rt,
    def_explosiveness = VoATrain$adj_def_explosiveness,
    def_third_conv_rate = VoATrain$def_third_conv_rate,
    def_pts_per_opp = VoATrain$def_pts_per_opp,
    def_havoc_total = VoATrain$def_havoc_total,
    def_plays_pg = VoATrain$adj_def_plays_pg,
    VoA_Output = VoATrain$VoA_Output,
    Conference_Strength = VoATrain$Conf_Rk
  )

  ### compile the stan model
  Def_VoA_model <- cmdstan_model(
    stan_file = here("Scripts", "Stan", "Def_VoA.stan")
  )
  ### fitting stan model
  set.seed(802)
  Def_VoA_fit <- Def_VoA_model$sample(
    data = Def_VoA_datalist,
    chains = 3,
    iter_sampling = 7500,
    iter_warmup = 2500,
    seed = 802
  )
  Def_VoA_fit

  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_def_epa",
      "beta_def_ypp",
      "beta_def_success_rate",
      "beta_def_explosiveness",
      "beta_def_third_conv_rate",
      "beta_def_pts_per_opp",
      "beta_def_havoc_total",
      "beta_def_plays_pg",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  # Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  # ### creating ratings
  # set.seed(802)
  # for (p in 1:length(Def_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     Def_VoA_Rating <- rnorm(
  #       1,
  #       mean = Def_VoA_pars$b0[p] +
  #         Def_VoA_pars$beta_def_ppa[p] * VoAVariables$weighted_def_ppa[t] +
  #         Def_VoA_pars$beta_def_ypp[p] * VoAVariables$weighted_def_ypp[t] +
  #         Def_VoA_pars$beta_def_success_rate[p] *
  #           VoAVariables$weighted_def_success_rate[t] +
  #         Def_VoA_pars$beta_def_explosiveness[p] *
  #           VoAVariables$weighted_def_explosiveness[t] +
  #         Def_VoA_pars$beta_def_third_conv_rate[p] *
  #           VoAVariables$weighted_def_third_conv_rate[t] +
  #         Def_VoA_pars$beta_def_pts_per_opp[p] *
  #           VoAVariables$weighted_def_pts_per_opp[t] +
  #         Def_VoA_pars$beta_def_havoc_total[p] *
  #           VoAVariables$weighted_def_havoc_total[t] +
  #         Def_VoA_pars$beta_def_plays_pg[p] *
  #           VoAVariables$weighted_def_plays_pg[t] +
  #         Def_VoA_pars$beta_VoA_Output[p] * VoAVariables$VoA_Output[t] +
  #         Def_VoA_pars$beta_Conference_Strength[p] *
  #           VoAVariables$Conference_Strength[t],
  #       sd = Def_VoA_pars$sigma[p]
  #     )
  #     Def_VoA_Ratings[p, t] <- Def_VoA_Rating
  #   }
  # }

  ## Create the Design Matrix (Teams x Predictors)
  DefDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_def_epa = VoAVariables$weighted_def_epa,
    beta_def_ypp = VoAVariables$weighted_def_ypp,
    beta_def_success_rate = VoAVariables$weighted_def_success_rt,
    beta_def_explosiveness = VoAVariables$weighted_def_explosiveness,
    beta_def_third_conv_rate = VoAVariables$weighted_def_third_conv_rate,
    beta_def_pts_per_opp = VoAVariables$weighted_def_pts_per_opp,
    beta_def_havoc_total = VoAVariables$weighted_def_havoc_total,
    beta_def_plays_pg = VoAVariables$weighted_def_plays_pg,
    beta_VoA_Output = VoAVariables$VoA_Output,
    beta_Conference_Strength = VoAVariables$Conf_Rk
  ))

  #### Parameter Matrix (Posterior samples x Predictors)
  Def_VoA_pars_matrix <- as.matrix(Def_VoA_pars[, colnames(DefDesignMatrix)])

  ### Calculate Means for ALL (p, t) pairs in one operation
  ### Def_VoA_pars_matrix %*% t(DesignMatrix) produces a matrix of size (N_draws x N_teams)
  DefMeans_matrix <- Def_VoA_pars_matrix %*% t(DefDesignMatrix)

  ### Add normal noise vectorized using the sigma array
  P <- length(Def_VoA_pars$b0)
  T_num <- nrow(VoAVariables)

  ### applying rnorm to generate a matrix of ratings using the matrix of samples from the posterior distributions
  set.seed(802)
  Def_VoA_Ratings <- matrix(
    rnorm(P * T_num, mean = DefMeans_matrix, sd = Def_VoA_pars$sigma),
    nrow = P,
    ncol = T_num
  )

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings, 2, mean)
  MedianPred <- apply(Def_VoA_Ratings, 2, median)
  Upper <- apply(Def_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(Def_VoA_Ratings, 2, quantile, prob = .05)

  VoAVariables$DefVoA_MeanRating <- MeanPred
  VoAVariables$DefVoA_MedRating <- MedianPred
  VoAVariables$DefVoA_95PctRating <- Upper
  VoAVariables$DefVoA_05PctRating <- Lower

  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  ST_VoA_datalist <- list(
    N = nrow(VoATrain),
    net_st_ppg = VoATrain$net_adj_st_ppg,
    net_kick_return_avg = VoATrain$net_kick_return_yds,
    net_punt_return_avg = VoATrain$net_punt_return_yds,
    net_fg_rate = VoATrain$net_fg_rate,
    net_st_epa = VoAtrain$net_adj_st_epa
  )

  ### compile the stan model
  ST_VoA_model <- cmdstan_model(
    stan_file = here("Scripts", "Stan", "ST_VoA.stan")
  )
  ### fitting special teams stan model
  set.seed(802)
  ST_VoA_fit <- ST_VoA_model$sample(
    data = ST_VoA_datalist,
    chains = 3,
    iter_sampling = 5000,
    iter_warmup = 2500,
    seed = 802
  )
  ST_VoA_fit

  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_net_kick_return_avg",
      "beta_net_punt_return_avg",
      "beta_net_fg_rate",
      "beta_net_st_epa",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to store special teams VoA_Ratings
  # ST_VoA_Ratings <- matrix(
  #   NA,
  #   nrow = length(ST_VoA_pars$b0),
  #   ncol = nrow(VoAVariables)
  # )

  # ### creating special teams VoA_Ratings
  # set.seed(802)
  # for (p in 1:length(ST_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     ST_VoA_Rating <- rnorm(
  #       1,
  #       mean = ST_VoA_pars$b0[p] +
  #         ST_VoA_pars$beta_net_kick_return_avg[p] *
  #           VoAVariables$weighted_net_kick_return_avg[t] +
  #         ST_VoA_pars$beta_net_punt_return_avg[p] *
  #           VoAVariables$weighted_net_punt_return_avg[t] +
  #         ST_VoA_pars$beta_net_fg_rate[p] *
  #           VoAVariables$weighted_net_fg_rate[t] +
  #         ST_VoA_pars$beta_net_st_ppa[p] *
  #           VoAVariables$weighted_net_adj_st_ppa[t],
  #       sd = ST_VoA_pars$sigma[p]
  #     )
  #     ST_VoA_Ratings[p, t] <- ST_VoA_Rating
  #   }
  # }

  ## Create the Design Matrix (Teams x Predictors)
  STDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_net_kick_return_avg = VoAVariables$weighted_net_st_kick_return_yds,
    beta_net_punt_return_avg = VoAVariables$weighted_net_punt_return_yds,
    beta_net_fg_rate = VoAVariables$weighted_net_fg_rt,
    beta_net_st_epa = VoAVariables$weighted_net_adj_st_epa
  ))

  #### Parameter Matrix (Posterior samples x Predictors)
  ST_VoA_pars_matrix <- as.matrix(ST_VoA_pars[, colnames(STDesignMatrix)])

  ### Calculate Means for ALL (p, t) pairs in one operation
  ### Off_VoA_pars_matrix %*% t(DesignMatrix) produces a matrix of size (N_draws x N_teams)
  STMeans_matrix <- ST_VoA_pars_matrix %*% t(STDesignMatrix)

  ### Add normal noise vectorized using the sigma array
  P <- length(ST_VoA_pars$b0)
  T_num <- nrow(VoAVariables)

  ### applying rnorm to generate a matrix of ratings using the matrix of samples from the posterior distributions
  set.seed(802)
  ST_VoA_Ratings <- matrix(
    rnorm(P * T_num, mean = STMeans_matrix, sd = ST_VoA_pars$sigma),
    nrow = P,
    ncol = T_num
  )

  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings, 2, mean)
  MedianPred <- apply(ST_VoA_Ratings, 2, median)
  Upper <- apply(ST_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(ST_VoA_Ratings, 2, quantile, prob = .05)

  VoAVariables$STVoA_MeanRating <- MeanPred
  VoAVariables$STVoA_MedRating <- MedianPred
  VoAVariables$STVoA_95PctRating <- Upper
  VoAVariables$STVoA_05PctRating <- Lower
} else {
  ##### Weeks 9-End of Season Stan Models, current season data only #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  # Off_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   off_ppg = VoAVariables$adj_off_ppg,
  #   off_epa = VoAVariables$adj_off_epa,
  #   off_ypp = VoAVariables$adj_off_ypp,
  #   off_success_rate = VoAVariables$off_success_rate,
  #   off_explosiveness = VoAVariables$adj_off_explosiveness,
  #   third_conv_rate = VoAVariables$third_conv_rate,
  #   off_pts_per_opp = VoAVariables$off_pts_per_opp,
  #   off_plays_pg = VoAVariables$off_plays_pg,
  #   VoA_Output = 1 / VoAVariables$VoA_Output,
  #   Conference_Strength = 1 / VoAVariables$Conference_Strength
  # )

  # ### compile the stan model
  # Off_VoA_model <- cmdstan_model(
  #   stan_file = here("Scripts", "Stan", "Off_VoA.stan")
  # )
  # ### fitting stan model
  # set.seed(802)
  # Off_VoA_fit <- Off_VoA_model$sample(
  #   data = Off_VoA_datalist,
  #   chains = 3,
  #   iter_sampling = 10000,
  #   iter_warmup = 2500,
  #   seed = 802
  # )
  # Off_VoA_fit

  ### Print the diagnostics
  print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Off_VoA_pars <- Off_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_off_ppa",
      "beta_off_ypp",
      "beta_off_success_rate",
      "beta_off_explosiveness",
      "beta_third_conv_rate",
      "beta_off_pts_per_opp",
      "beta_off_plays_pg",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  # Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  # ### creating ratings
  # set.seed(802)
  # for (p in 1:length(Off_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     Off_VoA_Rating <- rnorm(
  #       1,
  #       mean = Off_VoA_pars$b0[p] +
  #         Off_VoA_pars$beta_off_ppa[p] * VoAVariables$adj_off_ppa[t] +
  #         Off_VoA_pars$beta_off_ypp[p] * VoAVariables$adj_off_ypp[t] +
  #         Off_VoA_pars$beta_off_success_rate[p] *
  #           VoAVariables$off_success_rate[t] +
  #         Off_VoA_pars$beta_off_explosiveness[p] *
  #           VoAVariables$adj_off_explosiveness[t] +
  #         Off_VoA_pars$beta_third_conv_rate[p] *
  #           VoAVariables$third_conv_rate[t] +
  #         Off_VoA_pars$beta_off_pts_per_opp[p] *
  #           VoAVariables$off_pts_per_opp[t] +
  #         Off_VoA_pars$beta_off_plays_pg[p] * VoAVariables$off_plays_pg[t] +
  #         Off_VoA_pars$beta_VoA_Output[p] * (1 / VoAVariables$VoA_Output[t]) +
  #         Off_VoA_pars$beta_Conference_Strength[p] *
  #           (1 / VoAVariables$Conference_Strength[t]),
  #       sd = Off_VoA_pars$sigma[p]
  #     )
  #     Off_VoA_Ratings[p, t] <- Off_VoA_Rating
  #   }
  # }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Off_VoA_Ratings, 2, mean)
  MedianPred <- apply(Off_VoA_Ratings, 2, median)
  Upper <- apply(Off_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(Off_VoA_Ratings, 2, quantile, prob = .05)
  ### assigning ratings to columns in VoA Variables
  VoAVariables$OffVoA_MeanRating <- MeanPred
  VoAVariables$OffVoA_MedRating <- MedianPred
  VoAVariables$OffVoA_95PctRating <- Upper
  VoAVariables$OffVoA_05PctRating <- Lower

  ### VoA Defensive Rating Model
  ### making list of data to declare what goes into stan model
  # Def_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   def_ppg = VoAVariables$adj_def_ppg,
  #   def_ppa = VoAVariables$adj_def_ppa,
  #   def_ypp = VoAVariables$adj_def_ypp,
  #   def_success_rate = VoAVariables$def_success_rate,
  #   def_explosiveness = VoAVariables$adj_def_explosiveness,
  #   def_third_conv_rate = VoAVariables$def_third_conv_rate,
  #   def_pts_per_opp = VoAVariables$def_pts_per_opp,
  #   def_havoc_total = VoAVariables$def_havoc_total,
  #   def_plays_pg = VoAVariables$def_plays_pg,
  #   VoA_Output = VoAVariables$VoA_Output,
  #   Conference_Strength = VoAVariables$Conference_Strength
  # )

  ### compile the stan model
  # Def_VoA_model <- cmdstan_model(
  #   stan_file = here("Scripts", "Stan", "Def_VoA.stan")
  # )
  # ### fitting stan model
  # set.seed(802)
  # Def_VoA_fit <- Def_VoA_model$sample(
  #   data = Def_VoA_datalist,
  #   chains = 3,
  #   iter_sampling = 10000,
  #   iter_warmup = 2500,
  #   seed = 802
  # )
  # Def_VoA_fit

  ### Print the diagnostics
  print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  Def_VoA_pars <- Def_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_def_ppa",
      "beta_def_ypp",
      "beta_def_success_rate",
      "beta_def_explosiveness",
      "beta_def_third_conv_rate",
      "beta_def_pts_per_opp",
      "beta_def_havoc_total",
      "beta_def_plays_pg",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  # Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  # ### creating ratings
  # set.seed(802)
  # for (p in 1:length(Def_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     Def_VoA_Rating <- rnorm(
  #       1,
  #       mean = Def_VoA_pars$b0[p] +
  #         Def_VoA_pars$beta_def_ppa[p] * VoAVariables$adj_def_ppa[t] +
  #         Def_VoA_pars$beta_def_ypp[p] * VoAVariables$adj_def_ypp[t] +
  #         Def_VoA_pars$beta_def_success_rate[p] *
  #           VoAVariables$def_success_rate[t] +
  #         Def_VoA_pars$beta_def_explosiveness[p] *
  #           VoAVariables$adj_def_explosiveness[t] +
  #         Def_VoA_pars$beta_def_third_conv_rate[p] *
  #           VoAVariables$def_third_conv_rate[t] +
  #         Def_VoA_pars$beta_def_pts_per_opp[p] *
  #           VoAVariables$def_pts_per_opp[t] +
  #         Def_VoA_pars$beta_def_havoc_total[p] *
  #           VoAVariables$def_havoc_total[t] +
  #         Def_VoA_pars$beta_def_plays_pg[p] * VoAVariables$def_plays_pg[t] +
  #         Def_VoA_pars$beta_VoA_Output[p] * VoAVariables$VoA_Output[t] +
  #         Def_VoA_pars$beta_Conference_Strength[p] *
  #           VoAVariables$Conference_Strength[t],
  #       sd = Def_VoA_pars$sigma[p]
  #     )
  #     Def_VoA_Ratings[p, t] <- Def_VoA_Rating
  #   }
  # }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(Def_VoA_Ratings, 2, mean)
  MedianPred <- apply(Def_VoA_Ratings, 2, median)
  Upper <- apply(Def_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(Def_VoA_Ratings, 2, quantile, prob = .05)

  VoAVariables$DefVoA_MeanRating <- MeanPred
  VoAVariables$DefVoA_MedRating <- MedianPred
  VoAVariables$DefVoA_95PctRating <- Upper
  VoAVariables$DefVoA_05PctRating <- Lower

  ### Special Teams VoA
  ### making list of data to declare what goes into Stan model
  # ST_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   net_st_ppg = VoAVariables$net_st_ppg,
  #   net_kick_return_avg = VoAVariables$net_kick_return_avg,
  #   net_punt_return_avg = VoAVariables$net_punt_return_avg,
  #   net_fg_rate = VoAVariables$net_fg_rate,
  #   net_st_ppa = VoAVariables$net_adj_st_ppa
  # )

  ### compile the stan model
  # ST_VoA_model <- cmdstan_model(
  #   stan_file = here("Scripts", "Stan", "ST_VoA.stan")
  # )
  ### fitting special teams stan model
  # set.seed(802)
  # ST_VoA_fit <- ST_VoA_model$sample(
  #   data = ST_VoA_datalist,
  #   chains = 3,
  #   iter_sampling = 5000,
  #   iter_warmup = 2500,
  #   seed = 802
  # )
  # ST_VoA_fit

  ### Print the diagnostics
  print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_net_kick_return_avg",
      "beta_net_punt_return_avg",
      "beta_net_fg_rate",
      "beta_net_st_ppa",
      "sigma"
    ),
    format = "draws_df"
  )

  ### creating matrix to store special teams VoA_Ratings
  ST_VoA_Ratings <- matrix(
    NA,
    nrow = length(ST_VoA_pars$b0),
    ncol = nrow(VoAVariables)
  )

  ### creating special teams VoA_Ratings
  # set.seed(802)
  # for (p in 1:length(ST_VoA_pars$b0)) {
  #   for (t in 1:nrow(VoAVariables)) {
  #     ST_VoA_Rating <- rnorm(
  #       1,
  #       mean = ST_VoA_pars$b0[p] +
  #         ST_VoA_pars$beta_net_kick_return_avg[p] *
  #           VoAVariables$net_kick_return_avg[t] +
  #         ST_VoA_pars$beta_net_punt_return_avg[p] *
  #           VoAVariables$net_punt_return_avg[t] +
  #         ST_VoA_pars$beta_net_fg_rate[p] * VoAVariables$net_fg_rate[t] +
  #         ST_VoA_pars$beta_net_st_ppa[p] * VoAVariables$net_adj_st_ppa[t],
  #       sd = ST_VoA_pars$sigma[p]
  #     )
  #     ST_VoA_Ratings[p, t] <- ST_VoA_Rating
  #   }
  # }

  ### generating median and mean and quantile ratings
  MeanPred <- apply(ST_VoA_Ratings, 2, mean)
  MedianPred <- apply(ST_VoA_Ratings, 2, median)
  Upper <- apply(ST_VoA_Ratings, 2, quantile, prob = .95)
  Lower <- apply(ST_VoA_Ratings, 2, quantile, prob = .05)

  VoAVariables$STVoA_MeanRating <- MeanPred
  VoAVariables$STVoA_MedRating <- MedianPred
  VoAVariables$STVoA_95PctRating <- Upper
  VoAVariables$STVoA_05PctRating <- Lower
}

### making sure all values are > 0
for (i in 1:nrow(VoAVariables)) {
  set.seed(802)
  if (VoAVariables$OffVoA_MeanRating[i] <= 0) {
    VoAVariables$OffVoA_MeanRating[i] <- abs(VoAVariables$OffVoA_MeanRating[
      i
    ]) +
      abs(rnorm(1, 3, 1))
  }
  if (VoAVariables$DefVoA_MeanRating[i] <= 0) {
    VoAVariables$DefVoA_MeanRating[i] <- abs(VoAVariables$DefVoA_MeanRating[
      i
    ]) +
      abs(rnorm(1, 3, 1))
  }
}

##### Ranking VoA Rating columns #####
VoAVariables <- VoAVariables |>
  mutate(
    VoA_Rating_Ovr = OffVoA_MeanRating - DefVoA_MeanRating + STVoA_MeanRating,
    VoA_Rating_05Pct = OffVoA_05PctRating -
      DefVoA_05PctRating +
      STVoA_05PctRating,
    VoA_Rating_95Pct = OffVoA_95PctRating -
      DefVoA_95PctRating +
      STVoA_95PctRating,
    VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr)),
    OffVoA_Ranking = dense_rank(desc(OffVoA_MeanRating)),
    DefVoA_Ranking = dense_rank(DefVoA_MeanRating),
    STVoA_Ranking = dense_rank(desc(STVoA_MeanRating))
  )


## testing tidymodel workflow to turn VoA_Output into FPI or SP+ like metric
## VoA_bootstrap <- rsample::bootstraps(VoAVariables_Test, times = 20, breaks = 5)
# VoA_rf <- parsnip::rand_forest(
#   mode = "regression",
#   engine = "ranger",
#   mtry = 2,
#   trees = 5000
# )
# # fmt: skip
# VoA_rf_fit <- parsnip::fit(VoA_rf,
#                                 AllPY_FPI_SP_mean ~ off_epa_PY1 + off_epa_PY2 + def_epa_PY1 + def_epa_PY2 + off_epa_PY3 + def_epa_PY3,
#                                 data = VoAVariables_Test)
# VoA_rf_predict <- parsnip::predict_raw(VoA_rf_fit, VoAVariables_Test)
# VoAVariables_Test <- VoAVariables_Test |>
#   mutate(
#     VoA_Rating = VoA_rf_predict$predictions,
#     VoA_Ranking = dense_rank(desc(VoA_Rating))
#   )

## Creating data frames of just variables used for creating gt tables of rankings and Unintelligible Charts™©® showing VoA output and ranking during the season (after week 2)
FinalTable <- VoAVariables |>
  select(
    school,
    classification,
    conference,
    CFB_Week,
    VoA_Output,
    VoA_Rating_Ovr,
    VoA_Ranking_Ovr,
    OffVoA_MedRating,
    OffVoA_Ranking,
    DefVoA_MedRating,
    DefVoA_Ranking,
    STVoA_MedRating,
    STVoA_Ranking,
    Conf_Rk
  ) |>
  arrange(VoA_Ranking_Ovr)
### separating out top 25
FinalVoATop25 <- FinalTable |>
  filter(VoA_Ranking_Ovr < 26)

##### Creating Top 25 and Full Tables Arranged by VoA Rating #####
### Top 25 Table
# adding title and subtitle
VoATop25Table <- FinalVoATop25 |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_espn() |>
  tab_header(
    title = gt_top25_title, # ...with this title
    subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
  ) |> # and this subtitle
  ## tab_style(style = cell_fill("bisque"),
  ##           locations = cells_body()) |>  # add fill color to table
  fmt_number(
    # A column (numeric data)
    columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # Another column (also numeric data)
    columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 0 # I want this column to have zero decimal places
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(VoA_Rating_Ovr),
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(OffVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(DefVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(STVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  cols_label(
    VoA_Rating_Ovr = "Overall VoA Rating",
    VoA_Ranking_Ovr = "VoA Ranking",
    OffVoA_MedRating = "Off VoA Rating",
    OffVoA_Ranking = "Off Ranking",
    DefVoA_MedRating = "Def VoA Rating",
    DefVoA_Ranking = "Def Ranking",
    STVoA_MedRating = "ST VoA Rating",
    STVoA_Ranking = "ST Ranking"
  ) |> # Update labels
  # cols_move_to_end(columns = "VoA_Rating") |>
  cols_hide(c(conference, CFB_Week, VoA_Output, Conf_Rk)) |>
  tab_footnote(
    footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
  )

## Full 134 teams table
# adding title and subtitle
VoA_Full_Table <- FinalTable |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_espn() |>
  tab_header(
    title = gt_title, # ...with this title
    subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
  ) |> # and this subtitle
  ##tab_style(style = cell_fill("bisque"),
  ##        locations = cells_body()) |>  # add fill color to table
  fmt_number(
    # A column (numeric data)
    columns = c(VoA_Rating_Ovr), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(OffVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(DefVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(STVoA_MedRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # Another column (also numeric data)
    columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 0 # I want this column to have zero decimal places
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(VoA_Rating_Ovr), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(OffVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(DefVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(STVoA_MedRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  cols_label(
    VoA_Rating_Ovr = "Overall VoA Rating",
    VoA_Ranking_Ovr = "VoA Ranking",
    OffVoA_MedRating = "Off VoA Rating",
    OffVoA_Ranking = "Off Ranking",
    DefVoA_MedRating = "Def VoA Rating",
    DefVoA_Ranking = "Def Ranking",
    STVoA_MedRating = "ST VoA Rating",
    STVoA_Ranking = "ST Ranking"
  ) |> # Update labels
  # cols_move_to_end(columns = "VoA_Rating_Ovr") |>
  cols_hide(c(conference, CFB_Week, VoA_Output, Conf_Rk)) |>
  tab_footnote(
    footnote = "Table by @gshelor, data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org"
  )
# VoA_Full_Table |>
#   gtsave(
#     fulltable_file_pathway, expand = 5,
#     path = here("RVoA", "Outputs", "Test")
#   )

#### possible future code for trying out different formats for top 25 tables
## testing adding column colors
# VoATableColors <- Final_gt_Top25 |>
#   gt() |> # Make a gt table with it
#   gt_theme_538() |>
#   ## gt_color_rows(VoA_Output, palette = "ggsci::blue_material") |>
#   tab_header(
#     title = paste(year, week_text, cfb_week, VoA_Top25_text), # Add a title
#     subtitle = "it's a brand new upgraded version of a table! Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy" # And a subtitle
#   ) |>
#   fmt_passthrough( # Not sure about this but it works...
#     columns = c(Team) # First column: team (character)
#   ) |>
#   fmt_number(
#     columns = c(VoA_Output), # Second column: VoA_Output (numeric)
#     decimals = 5 # With 5 decimal places
#   ) |>
#   fmt_number(
#     columns = c(VoA_Ranking), # Third column: VoA_Ranking (numeric)
#     decimals = 0 # With 0 decimal places
#   ) |>
#   #  data_color( # Update cell colors...
#   #    columns = c(VoA_Output), # ...for dose column
#   #    colors = scales::col_numeric( # <- bc it's numeric
#   #      palette = c(
#   #        "dodgerblue4","cadetblue1"), # A color scheme (gradient)
#   #      domain = c() # Column scale endpoints
#   #    )
#   # ) |>
#   data_color( # Update cell colors, testing different color palettes
#     columns = c(VoA_Output), # ...for VoA_Output column
#     colors = scales::col_numeric( # <- bc it's numeric
#       palette = brewer.pal(9, "Reds"), # A color scheme (gradient)
#       domain = c() # Column scale endpoints
#     )
#   ) |>
#   cols_label(team= "Team", VoA_Output = "Final VoA Output", VoA_Ranking = "VoA Ranking") |> # Make the column headers
#   tab_footnote(
#     footnote = "rounded to 5 decimals", # Another line of footnote text
#     locations = cells_column_labels(
#       columns = c(VoA_Output) # Associated with column 'VoA_Output'
#     )
#   ) |>
#   cols_move_to_end(columns = "VoA_Output")
# VoATableColors
#
# ## Save GT table with colors in columns
# VoATableColors |>
#   gtsave(
#    "VoAGTwithColors.png", expand = 5,
#    path = here("RVoA", "Outputs")
#  )

##### Testing Resume VoA #####

########### BREAK HERE EVERYTHING BELOW IS NOT PART OF CURRENT TESTING ######
break

##### DEBUGGING WEIRD AND STUPID NA ERRORS #####
## Making values numeric
# VoAVariables[,6:ncol(VoAVariables)] <- VoAVariables[,6:ncol(VoAVariables)] |> mutate_if(is.character,as.numeric)
#
## nas why
nas_why <- data.frame(apply(VoAVariables, 2, anyNA))
nas_sum <- data.frame(apply(VoAVariables, 2, is.na))
nas_sum <- data.frame(apply(nas_sum, 2, sum))
colnames(nas_why) <- c("containsNAs")
colnames(nas_sum) <- c("NAsum")
nas_sum <- nas_sum |>
  filter(NAsum > 0 & NAsum < 100)
nas_why <- nas_why |>
  filter(containsNAs == TRUE)
nas_why_col <- VoAVariables |>
  filter(is.na(off_ypp))
# recruit_nas_teams <- anti_join(VoAVariables, recruit, by = "team")

# colnames(VoAVariables)[apply(VoAVariables, 2, anyNA)]

########## END OF DEBUGGING
