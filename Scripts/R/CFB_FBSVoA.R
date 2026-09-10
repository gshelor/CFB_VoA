##### The Vortex of Accuracy, Version 5.1.1 #####
### Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy
### Created by Griffin Shelor
### installing packages
# install.packages(c("devtools", "tidyverse", "gt", "viridis", "webshot", "cfbfastR", "here", "RColorBrewer", "remotes", "pacman", "gtExtras", "cfbplotR", "betareg", "cmdstanr", "parallel", "posterior", "data.table", "lme4", "arrow"))
##### Loading Packages #####
start_time <- Sys.time()
library(pacman)
# fmt: skip
p_load(tidyverse, gt, cfbfastR, here, RColorBrewer, gtExtras, cfbplotR, ggpubr, webshot2, cmdstanr, parallel, posterior, data.table, lme4, arrow)
## used to use these packages
# viridis, and also rstan since I'm switching to cmdstanr
### reading in script of functions (will be called later)
source(here("Scripts", "R", "CFBVoA_funcs.R"))
cfbd_api_key_info()

### Creating Week and Year String for Top 25 Table Title, eventually could be used as part of reading in cfbfastR/cfbdata API data
## might switch the year one to use Sys.Date()
year <- readline(prompt = "What year is it? (year that the season starts in) ")
cfb_week <- readline(prompt = "What week just occurred? ")
if (as.integer(cfb_week) == 0) {
  PY4 <- as.integer(year) - 4
  PY3 <- as.integer(year) - 3
  PY2 <- as.integer(year) - 2
  PY1 <- as.integer(year) - 1
}

##### setting strings for table titles, file pathways, unintelligible charts #####
`%nin%` <- Negate(`%in%`)
output_dir <- here("Outputs", "RVoA", paste0("VoA", year))
data_dir <- here("Data", paste0("VoA", year))
tracking_chart_dir <- here("Data", paste0("VoA", year), "TrackingChartCSVs")
accuracy_data_dir <- here("Data", paste0("VoA", year), "AccuracyMetrics")
PY_data_dir <- here("Data", paste0("VoA", year), "PYData")
Projection_data_dir <- here("Data", paste0("VoA", year), "Projections")
preseason_text <- "CFB FBS Preseason"
resume_text <- "FBS Resume"
VoAString <- "FBSVoA.parquet"
week_text <- "Week"
VoA_Top25_text <- "Vortex of Accuracy Top 25"
top25_png <- "FBSVoATop25.png"
fulltable_png <- "FBSVoAFullTable.png"
VoA_text <- "FBS Vortex of Accuracy"
Postseason_text <- " CFB Postseason"
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
Rating_text <- "_FBSRatings_Chart.png"
Ranking_text <- "_FBSRankings_Chart.png"
Histogram_text <- "_FBSRatingHist.png"
Output_Rating_Plot_text <- "FBS VoA Outputs vs VoA Ratings"
Output_Rating_Plot_png <- "FBSOutput_Rating.png"
OffDef_Rating_Plot_text <- "FBS VoA Off Rating vs VoA Def Rating"
OffDef_Rating_Plot_png <- "FBSOffDef_Rating.png"
OffDef_EPA_Plot_text <- "FBS Offensive Opponent-Adjusted EPA vs Defensive Opponent-Adjusted EPA"
OffDef_EPA_Plot_png <- "FBSOffDef_AdjEPA.png"

FBS_hist_title <- paste(
  year,
  week_text,
  cfb_week,
  FBS_text,
  VoA_text,
  "Ratings"
)
Power5_hist_title <- paste(
  year,
  week_text,
  cfb_week,
  Power_Five_text,
  VoA_text,
  "Ratings"
)
Group5_hist_title <- paste(
  year,
  week_text,
  cfb_week,
  Group_Five_text,
  VoA_text,
  "Ratings"
)
Output_Rating_Plot_title <- paste(
  year,
  week_text,
  cfb_week,
  Output_Rating_Plot_text
)
OffDef_Rating_Plot_title <- paste(
  year,
  week_text,
  cfb_week,
  OffDef_Rating_Plot_text
)
OffDef_EPA_Plot_title <- paste(year, week_text, cfb_week, OffDef_EPA_Plot_text)
top25_file_pathway <- paste(year, week_text, cfb_week, "_", top25_png, sep = "")
resumetop25_file_pathway <- paste(
  year,
  week_text,
  cfb_week,
  resume_text,
  "_",
  top25_png,
  sep = ""
)
fulltable_file_pathway <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  fulltable_png,
  sep = ""
)
resumefulltable_file_pathway <- paste(
  year,
  week_text,
  cfb_week,
  resume_text,
  "_",
  fulltable_png,
  sep = ""
)
AAC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  AAC_text,
  Rating_text,
  sep = ""
)
AAC_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  AAC_text,
  Ranking_text,
  sep = ""
)
ACC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  ACC_text,
  Rating_text,
  sep = ""
)
ACC_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  ACC_text,
  Ranking_text,
  sep = ""
)
Big12_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  Big12_text,
  Rating_text,
  sep = ""
)
Big12_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  Big12_text,
  Ranking_text,
  sep = ""
)
Big10_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  Big10_text,
  Rating_text,
  sep = ""
)
Big10_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  Big10_text,
  Ranking_text,
  sep = ""
)
CUSA_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  CUSA_text,
  Rating_text,
  sep = ""
)
CUSA_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  CUSA_text,
  Ranking_text,
  sep = ""
)
Indy_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  Indy_text,
  Rating_text,
  sep = ""
)
Indy_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  Indy_text,
  Ranking_text,
  sep = ""
)
MAC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  MAC_text,
  Rating_text,
  sep = ""
)
MAC_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  MAC_text,
  Ranking_text,
  sep = ""
)
MWC_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  MWC_text,
  Rating_text,
  sep = ""
)
MWC_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  MWC_text,
  Ranking_text,
  sep = ""
)
Pac2_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  Pac2_text,
  Rating_text,
  sep = ""
)
Pac2_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  Pac2_text,
  Ranking_text,
  sep = ""
)
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
SunBelt_Output_filename <- paste(
  year,
  week_text,
  cfb_week,
  SunBelt_text,
  Rating_text,
  sep = ""
)
SunBelt_Ranking_filename <- paste(
  year,
  week_text,
  cfb_week,
  SunBelt_text,
  Ranking_text,
  sep = ""
)
FBS_hist_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  FBS_text,
  Histogram_text,
  sep = ""
)
Power5_hist_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  Power_Five_text,
  Histogram_text,
  sep = ""
)
Group5_hist_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  Group_Five_text,
  Histogram_text,
  sep = ""
)
Output_Rating_Plot_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  Output_Rating_Plot_png,
  sep = ""
)
OffDef_Rating_Plot_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  OffDef_Rating_Plot_png,
  sep = ""
)
OffDef_EPA_Plot_filename <- paste(
  year,
  week_text,
  cfb_week,
  "_",
  OffDef_EPA_Plot_png,
  sep = ""
)
### setting gt title based on whether it's after a playoff week or not
if (as.integer(cfb_week) == 15) {
  gt_top25_title <- paste(year, "Conference Championship Week", VoA_Top25_text)
  gt_title <- paste(year, "Conference Championship Week", VoA_text)
} else if (as.integer(cfb_week) == 16) {
  gt_top25_title <- paste(
    year,
    "Post Army-Navy Game (and Possibly a Bowl or 2)",
    VoA_Top25_text
  )
  gt_title <- paste(
    year,
    "Post Army-Navy Game (and Possibly a Bowl or 2)",
    VoA_text
  )
} else if (as.integer(cfb_week) == 17) {
  gt_top25_title <- paste(year, "CFP First Round", VoA_Top25_text)
  gt_title <- paste(year, "CFP First Round", VoA_text)
} else if (as.integer(cfb_week) == 18) {
  gt_top25_title <- paste(year, "CFP Quarterfinals", VoA_Top25_text)
  gt_title <- paste(year, "CFP Quarterfinals", VoA_text)
} else if (as.integer(cfb_week) == 19) {
  gt_top25_title <- paste(year, "CFP Semifinals", VoA_Top25_text)
  gt_title <- paste(year, "CFP Semifinals", VoA_text)
} else if (as.integer(cfb_week) == 20) {
  gt_top25_title <- paste(year, Postseason_text, VoA_Top25_text)
  gt_title <- paste(year, Postseason_text, VoA_text)
} else if (as.integer(cfb_week) == 0) {
  gt_top25_title <- paste(year, preseason_text, VoA_Top25_text)
  gt_title <- paste(year, preseason_text, VoA_text)
} else {
  gt_top25_title <- paste(year, week_text, cfb_week, VoA_Top25_text)
  gt_title <- paste(year, week_text, cfb_week, VoA_text)
}
### creating string for csv spreadsheet pathway
file_pathway <- paste0(data_dir, "/", year, week_text, cfb_week, "_", VoAString)
### creating directories that don't exist
for (i in c(
  data_dir,
  output_dir,
  tracking_chart_dir,
  Projection_data_dir,
  PY_data_dir,
  accuracy_data_dir
)) {
  if (dir.exists(i) == FALSE) {
    dir.create(i, recursive = TRUE)
  }
}
### setting number of cores to use for mcmc chains later
options(mc.cores = parallel::detectCores() / 2)

##### Reading in Data #####
### pulling in data based on week of the season
if (as.integer(cfb_week) == 0) {
  ##### WEEK 0 Data Pull #####
  ### getting team info for last 4 years
  ## using 4 years to train the model is a pain in my ass from a time and RAM standpoint, so holding off on that for now
  ## filtering to make sure each dataframe only includes D1 teams
  D1Teams <- cfbd_team_info(only_fbs = FALSE, year = as.integer(year)) |>
    filter(classification == "fbs") # | classification == "fcs")
  # D1Teams_PY4 <- cfbd_team_info(
  #   only_fbs = FALSE,
  #   year = as.integer(year) - 4
  # ) |>
  #   filter(school %in% D1Teams$school)|>
  # filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY3 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.integer(year) - 3
  ) |>
    filter(school %in% D1Teams$school) |>
    filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY2 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.integer(year) - 2
  ) |>
    filter(school %in% D1Teams$school) |>
    filter(classification == "fbs" | classification == "fcs")
  D1Teams_PY1 <- cfbd_team_info(
    only_fbs = FALSE,
    year = as.integer(year) - 1
  ) |>
    filter(school %in% D1Teams$school) |>
    filter(classification == "fbs" | classification == "fcs")

  # ### making sure the elevation column is numeric
  # VoAVariables$elevation <- as.numeric(VoAVariables$elevation)

  ### pulling in completed games as part of opponent-adjustment of stats later
  ### PY4 completed games
  # CompletedGames_PY4 <- cfbd_game_info(as.integer(year) - 4) |>
  #   filter(completed == TRUE) |>
  #   filter(
  #     home_team %in% D1Teams_PY4$school | away_team %in% D1Teams_PY4$school
  #   )
  # CompletedNeutralGames_PY4 <- CompletedGames_PY4 |>
  #   filter(neutral_site == TRUE)
  ### PY3 completed games
  CompletedGames_PY3 <- cfbd_game_info(as.integer(year) - 3) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY3$school | away_team %in% D1Teams_PY3$school
    )
  CompletedNeutralGames_PY3 <- CompletedGames_PY3 |>
    filter(neutral_site == TRUE)
  ### PY2 completed games
  CompletedGames_PY2 <- cfbd_game_info(as.integer(year) - 2) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY2$school | away_team %in% D1Teams_PY2$school
    )
  CompletedNeutralGames_PY2 <- CompletedGames_PY2 |>
    filter(neutral_site == TRUE)
  ### PY1 completed games
  CompletedGames_PY1 <- cfbd_game_info(as.integer(year) - 1) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams_PY1$school | away_team %in% D1Teams_PY1$school
    )
  CompletedNeutralGames_PY1 <- CompletedGames_PY1 |>
    filter(neutral_site == TRUE)

  ### loading in play-by-play data, creating VoA Variables dfs
  # PBP_PY4 <- load_cfb_pbp(seasons = as.integer(year) - 4) |>
  #   filter(home %in% D1Teams_PY4$school | away %in% D1Teams_PY4$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY4$home_team &
  #     home %in% CompletedGames_PY4$away_team &
  #     away %in% CompletedGames_PY4$home_team &
  #     away %in% CompletedGames_PY4$away_team
  # )
  # PBP_PY4 <- fix_pbp_subdivision_nas(PBP_PY4, D1Teams_PY4) |>
  #   mutate(epa_ppa_mean = rowMeans(select(PBP_PY4, c(EPA, ppa)), na.rm = TRUE))
  # VoATrain_PY4 <- create_voavarstrain_df(PY4, D1Teams_PY4, PBP_PY4)

  PBP_PY3 <- load_cfb_pbp(seasons = as.integer(year) - 3) |>
    filter(home %in% D1Teams_PY3$school | away %in% D1Teams_PY3$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY3$home_team &
  #     home %in% CompletedGames_PY3$away_team &
  #     away %in% CompletedGames_PY3$home_team &
  #     away %in% CompletedGames_PY3$away_team
  # )
  PBP_PY3 <- fix_pbp_subdivision_nas(PBP_PY3, D1Teams_PY3) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY3, c(EPA, ppa)), na.rm = TRUE))
  VoATrain_PY3 <- create_voavarstrain_df(PY3, D1Teams_PY3, PBP_PY3)

  PBP_PY2 <- load_cfb_pbp(seasons = as.integer(year) - 2) |>
    filter(home %in% D1Teams_PY2$school | away %in% D1Teams_PY2$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY2$home_team &
  #     home %in% CompletedGames_PY2$away_team &
  #     away %in% CompletedGames_PY2$home_team &
  #     away %in% CompletedGames_PY2$away_team
  # )
  PBP_PY2 <- fix_pbp_subdivision_nas(PBP_PY2, D1Teams_PY2) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY2, c(EPA, ppa)), na.rm = TRUE))
  VoATrain_PY2 <- create_voavarstrain_df(PY2, D1Teams_PY2, PBP_PY2)

  PBP_PY1 <- load_cfb_pbp(seasons = as.integer(year) - 1) |>
    filter(home %in% D1Teams_PY1$school | away %in% D1Teams_PY1$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames_PY1$home_team &
  #     home %in% CompletedGames_PY1$away_team &
  #     away %in% CompletedGames_PY1$home_team &
  #     away %in% CompletedGames_PY1$away_team
  # )
  PBP_PY1 <- fix_pbp_subdivision_nas(PBP_PY1, D1Teams_PY1) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP_PY1, c(EPA, ppa)), na.rm = TRUE))
  VoATrain_PY1 <- create_voavarstrain_df(PY1, D1Teams_PY1, PBP_PY1)
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

  # ### Setting up PBP for adjusted special teams epa stats
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

  ### Setting up PBP for adjusted special teams epa stats
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

  ### Setting up PBP for adjusted special teams epa stats
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

  ### Setting up PBP for adjusted special teams epa stats
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
} else if (as.integer(cfb_week) <= 5) {
  ##### WEEKS 1-5 Data Pull #####
  ### reading in preseason VoA in case there are teams that didn't play in Week 0 or Week 1
  PreseasonVoA <- read_parquet(paste0(
    data_dir,
    "/",
    year,
    week_text,
    0,
    "_",
    VoAString
  ))

  ### reading in PY data (before rank columns are applied) so I can join appropriate PY data to VoAVariables before weighted variables are calculated
  PYData <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    "PYData",
    paste0("PYData", year, ".parquet")
  )) |>
    select(school, ends_with("_PY2"), ends_with("_PY1")) |>
    select(-one_of(c("recruit_pts_PY2", "recruit_pts_PY1")))

  ### grabbing team info which will be turned into VoAVariables
  ## filtering to make sure each dataframe only includes D1 teams
  D1Teams <- cfbd_team_info(only_fbs = FALSE, year = as.integer(year)) |>
    filter(classification == "fbs")

  # ### making sure the elevation column is numeric
  # VoAVariables$elevation <- as.numeric(VoAVariables$elevation)

  ### pulling in completed games as part of opponent-adjustment of stats later
  ### completed games
  CompletedGames <- cfbd_game_info(as.integer(year) - 1) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams$school | away_team %in% D1Teams$school
    )
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### loading current PBP
  PBP <- load_cfb_pbp(seasons = as.integer(year)) |>
    filter(home %in% D1Teams$school | away %in% D1Teams$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames$home_team &
  #     home %in% CompletedGames$away_team &
  #     away %in% CompletedGames$home_team &
  #     away %in% CompletedGames$away_team
  # )
  PBP <- fix_pbp_subdivision_nas(PBP, D1Teams) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP, c(EPA, ppa)), na.rm = TRUE))
  ### VoAVariables df to be used for inference/generating current ratings
  VoAVariables <- create_voavars_df(as.integer(year), as.integer(cfb_week)) |>
    filter(school %in% PreseasonVoA$school)

  ### pulling out relevant plays used to create/input variables later
  ## PY1
  PBP_Yards <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_ScoringPlays <- PBP_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

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
    filter(scoring_opp == 1)

  PBP_TDs <- PBP_Yards |>
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

  # PBP_XPPlays <- PBP_TDs |>
  #   filter(play_pts_scored == 7)

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
  # PBP_Punts <- PBP |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams epa stats
  PBP_STPlays <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_STScoringPlays <- PBP_STPlays |>
    filter(scoring_play == 1)
} else if (as.integer(cfb_week) <= 9) {
  ##### WEEKS 6-9 Data Pull #####
  ### assuming that everyone will have played a game that's in the PBP dataset by now so not reading in the preseason VoA, just the PY stats so I don't have to recalculate those in the extracting stats section of the script
  ### reading in PY data (before rank columns are applied) so I can join appropriate PY data to VoAVariables before weighted variables are calculated
  PYData <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    "PYData",
    paste0("PYData", year, ".parquet")
  )) |>
    select(school, ends_with("_PY1"))

  ### grabbing team info which will be turned into VoAVariables
  ## filtering to make sure each dataframe only includes D1 teams
  D1Teams <- cfbd_team_info(only_fbs = FALSE, year = as.integer(year)) |>
    filter(classification == "fbs")

  # ### making sure the elevation column is numeric
  # VoAVariables$elevation <- as.numeric(VoAVariables$elevation)

  ### pulling in completed games as part of opponent-adjustment of stats later
  ### completed games
  CompletedGames <- cfbd_game_info(as.integer(year) - 1) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams$school & away_team %in% D1Teams$school
    )
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### loading current PBP
  PBP <- load_cfb_pbp(seasons = as.integer(year)) |>
    filter(home %in% D1Teams$school & away %in% D1Teams$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames$home_team &
  #     home %in% CompletedGames$away_team &
  #     away %in% CompletedGames$home_team &
  #     away %in% CompletedGames$away_team
  # )
  PBP <- fix_pbp_subdivision_nas(PBP, D1Teams) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP, c(EPA, ppa)), na.rm = TRUE))
  ### VoAVariables df to be used for inference/generating current ratings
  VoAVariables <- create_voavars_df(as.integer(year), as.integer(cfb_week)) |>
    filter(school %in% PreseasonVoA$school)

  ### pulling out relevant plays used to create/input variables later
  ## PY1
  PBP_Yards <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_ScoringPlays <- PBP_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

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
    filter(scoring_opp == 1)

  PBP_TDs <- PBP_Yards |>
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

  # PBP_XPPlays <- PBP_TDs |>
  #   filter(play_pts_scored == 7)

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
  # PBP_Punts <- PBP |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams epa stats
  PBP_STPlays <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_STScoringPlays <- PBP_STPlays |>
    filter(scoring_play == 1)
} else {
  ##### CURRENT SEASON STATS ONLY Data Pull #####
  ### grabbing team info which will be turned into VoAVariables
  ## filtering to make sure each dataframe only includes D1 teams
  D1Teams <- cfbd_team_info(only_fbs = FALSE, year = as.integer(year)) |>
    filter(classification == "fbs")

  # ### making sure the elevation column is numeric
  # VoAVariables$elevation <- as.numeric(VoAVariables$elevation)

  ### pulling in completed games as part of opponent-adjustment of stats later
  ### completed games
  CompletedGames <- cfbd_game_info(as.integer(year) - 1) |>
    filter(completed == TRUE) |>
    filter(
      home_team %in% D1Teams$school & away_team %in% D1Teams$school
    )
  CompletedNeutralGames <- CompletedGames |>
    filter(neutral_site == TRUE)

  ### loading current PBP
  PBP <- load_cfb_pbp(seasons = as.integer(year)) |>
    filter(home %in% D1Teams$school & away %in% D1Teams$school) #|>
  # filter(
  #   home %in%
  #     CompletedGames$home_team &
  #     home %in% CompletedGames$away_team &
  #     away %in% CompletedGames$home_team &
  #     away %in% CompletedGames$away_team
  # )
  PBP <- fix_pbp_subdivision_nas(PBP, D1Teams) |>
    mutate(epa_ppa_mean = rowMeans(select(PBP, c(EPA, ppa)), na.rm = TRUE))
  ### VoAVariables df to be used for inference/generating current ratings
  VoAVariables <- create_voavars_df(as.integer(year), as.integer(cfb_week)) |>
    filter(school %in% PreseasonVoA$school)

  ### pulling out relevant plays used to create/input variables later
  ## PY1
  PBP_Yards <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    ) |>
    mutate(
      play_pts_scored = case_when(scoring_play == 1 ~ new_drive_pts, TRUE ~ 0)
    )

  PBP_ScoringPlays <- PBP_Yards |>
    filter(scoring_play == 1 & play_pts_scored != 3)

  PBP_Turnovers <- PBP_Yards |>
    filter(turnover == 1)

  PBP_success_plays <- PBP_Yards |>
    filter(
      (down == 1 & (yards_gained >= (distance / 2))) |
        (down == 2 & (yards_gained >= (distance * 0.7))) |
        (down > 2 & (yards_gained >= distance))
    )

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
    filter(scoring_opp == 1)

  PBP_TDs <- PBP_Yards |>
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

  # PBP_XPPlays <- PBP_TDs |>
  #   filter(play_pts_scored == 7)

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
  # PBP_Punts <- PBP |>
  #   filter(play_type == "Punt" | play_type == "Punt Return Touchdown")

  ### Setting up PBP for adjusted special teams epa stats
  PBP_STPlays <- PBP |>
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
        game_id %in% CompletedNeutralGames$game_id ~ "Neutral",
        TRUE ~ "Home"
      )
    )

  PBP_STScoringPlays <- PBP_STPlays |>
    filter(scoring_play == 1)
}


##### Extracting Stats from PBP Data #####
if (as.integer(cfb_week) == 0) {
  ##### WEEK 0 Stat Collection #####
  VoATrain_PY1 <- extract_pbp_stats(
    VoA_df = VoATrain_PY1,
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
  VoATrain_PY2 <- extract_pbp_stats(
    VoA_df = VoATrain_PY2,
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
  VoATrain_PY3 <- extract_pbp_stats(
    VoA_df = VoATrain_PY3,
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
  # VoATrain_PY4 <- extract_pbp_stats(
  #   VoA_df = VoATrain_PY4,
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
  #   # Punts = PBP_PY4_Punts,
  #   # Kickoffs = PBP_PY4_KickReturn,
  #   # XPts = PBP_PY4_XPPlays,
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

  ### Making values numeric
  # VoAVariables[, 4:ncol(VoAVariables)] <- VoAVariables[,
  #   4:ncol(VoAVariables)
  # ] |>
  #   mutate_if(is.character, as.numeric)

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
} else if (as.integer(cfb_week) <= 5) {
  ##### WEEKS 1-5 STAT CALCULATION #####
  ### Extracting PBP data and opponent-adjusted data for df to be used for inference/current season's ratings
  VoAVariables <- extract_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays = PBP_Yards,
    success_plays = PBP_success_plays,
    ThirdDowns = PBP_3rdDowns,
    FourthDowns = PBP_4thDowns,
    passplays = PBP_passplays,
    rushplays = PBP_rushplays,
    scoringopp_plays = PBP_scoringopp_plays,
    turnovers = PBP_Turnovers,
    scoringplays = PBP_ScoringPlays,
    FGs = PBP_FGPlays,
    # Punts = PBP_Punts,
    # Kickoffs = PBP_KickReturn,
    # XPts = PBP_XPPlays,
    STPlays = PBP_STPlays
  )

  ### subsetting weighted preseason variables for substitution in cases when teams don't have data available or haven't played a game
  WeightedPreseasonVoAVars <- PreseasonVoA |>
    select(school, starts_with("weighted_"))

  ### joining relevant PY data to current season data
  ### fixing NAs in specific columns
  ## if more columns produce NAs in future years, they'll be added here then
  VoAVariables <- VoAVariables |>
    left_join(PYData, by = "school") |>
    left_join(WeightedPreseasonVoAVars, by = "school") |>
    mutate(
      off_passing_downs_explosiveness = case_when(
        is.na(off_passing_downs_explosiveness) ~ weighted_off_explosiveness,
        TRUE ~ off_passing_downs_explosiveness
      ),
      def_passing_downs_explosiveness = case_when(
        is.na(def_passing_downs_explosiveness) ~ weighted_def_explosiveness,
        TRUE ~ def_passing_downs_explosiveness
      ),
      off_fourth_conv_rate = case_when(
        is.na(off_fourth_conv_rate) ~ 0,
        TRUE ~ off_fourth_conv_rate
      ),
      def_fourth_conv_rate = case_when(
        is.na(def_fourth_conv_rate) ~ 0,
        TRUE ~ def_fourth_conv_rate
      ),
      off_power_success = case_when(
        is.na(off_power_success) ~ weighted_off_success_rate,
        TRUE ~ off_power_success
      ),
      def_power_success = case_when(
        is.na(def_power_success) ~ weighted_def_success_rate,
        TRUE ~ def_power_success
      ),
      off_pts_per_opp = case_when(
        is.na(off_pts_per_opp) ~ weighted_off_pts_per_opp,
        TRUE ~ off_pts_per_opp
      ),
      def_pts_per_opp = case_when(
        is.na(def_pts_per_opp) ~ weighted_def_pts_per_opp,
        TRUE ~ def_pts_per_opp
      ),
      off_turnovers_pg = case_when(
        is.na(off_turnovers_pg) ~ 0,
        TRUE ~ off_turnovers_pg
      ),
      def_turnovers_pg = case_when(
        is.na(def_turnovers_pg) ~ 0,
        TRUE ~ def_turnovers_pg
      ),
      off_pts_scored = case_when(
        is.na(off_pts_scored) ~ 0,
        TRUE ~ off_pts_scored
      ),
      def_pts_allowed = case_when(
        is.na(def_pts_allowed) ~ 0,
        TRUE ~ def_pts_allowed
      ),
      fg_rate = case_when(
        is.na(fg_rate) ~ 0,
        TRUE ~ fg_rate
      ),
      fg_rate_allowed = case_when(
        is.na(fg_rate_allowed) ~ 0,
        TRUE ~ fg_rate_allowed
      ),
      fg_made_pg = case_when(
        is.na(fg_made_pg) ~ 0,
        TRUE ~ fg_made_pg
      ),
      fg_made_pg_allowed = case_when(
        is.na(fg_made_pg_allowed) ~ 0,
        TRUE ~ fg_made_pg_allowed
      ),
      punt_return_yds = case_when(
        is.na(punt_return_yds) ~ 0,
        TRUE ~ punt_return_yds
      ),
      punt_return_yds_allowed = case_when(
        is.na(punt_return_yds_allowed) ~ 0,
        TRUE ~ punt_return_yds_allowed
      ),
      kick_return_yds = case_when(
        is.na(kick_return_yds) ~ 0,
        TRUE ~ kick_return_yds
      ),
      kick_return_yds_allowed = case_when(
        is.na(kick_return_yds_allowed) ~ 0,
        TRUE ~ kick_return_yds_allowed
      ),
      off_ppg = case_when(
        is.na(off_ppg) ~ weighted_off_ppg_mean,
        TRUE ~ off_ppg
      ),
      def_ppg = case_when(
        is.na(def_ppg) ~ weighted_def_ppg_mean,
        TRUE ~ def_ppg
      ),
      punt_return_TDs = case_when(
        is.na(punt_return_TDs) ~ 0,
        TRUE ~ punt_return_TDs
      ),
      punt_return_TDs_allowed = case_when(
        is.na(punt_return_TDs_allowed) ~ 0,
        TRUE ~ punt_return_TDs_allowed
      )
    ) |>
    select(-starts_with("weighted_"))
} else if (as.integer(cfb_week) <= 9) {
  ##### WEEKS 6-9 DF Merge #####
  ### Extracting PBP data and opponent-adjusted data for df to be used for inference/current season's ratings
  VoAVariables <- extract_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays = PBP_Yards,
    success_plays = PBP_success_plays,
    ThirdDowns = PBP_3rdDowns,
    FourthDowns = PBP_4thDowns,
    passplays = PBP_passplays,
    rushplays = PBP_rushplays,
    scoringopp_plays = PBP_scoringopp_plays,
    turnovers = PBP_Turnovers,
    scoringplays = PBP_ScoringPlays,
    FGs = PBP_FGPlays,
    # Punts = PBP_Punts,
    # Kickoffs = PBP_KickReturn,
    # XPts = PBP_XPPlays,
    STPlays = PBP_STPlays
  )

  ### joining relevant PY data to current season data
  VoAVariables <- VoAVariables |>
    left_join(PYData, by = "school")
} else {
  ##### Week 10-End of Season CURRENT SEASON ONLY DF Merge #####
  ### Extracting PBP data and opponent-adjusted data for df to be used for inference/current season's ratings
  VoAVariables <- extract_pbp_stats(
    VoA_df = VoAVariables,
    rushpass_plays = PBP_Yards,
    success_plays = PBP_success_plays,
    ThirdDowns = PBP_3rdDowns,
    FourthDowns = PBP_4thDowns,
    passplays = PBP_passplays,
    rushplays = PBP_rushplays,
    scoringopp_plays = PBP_scoringopp_plays,
    turnovers = PBP_Turnovers,
    scoringplays = PBP_ScoringPlays,
    FGs = PBP_FGPlays,
    # Punts = PBP_Punts,
    # Kickoffs = PBP_KickReturn,
    # XPts = PBP_XPPlays,
    STPlays = PBP_STPlays
  )

  ### Making values numeric
  # fmt: skip
  # VoAVariables[,4:ncol(VoAVariables)] <- VoAVariables[,4:ncol(VoAVariables)] |> mutate_if(is.character,as.numeric)
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
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_recruit_pts = (recruit_pts_PY1 * py1weight) +
        (recruit_pts_PY2 * py2weight) +
        (recruit_pts_PY3 * py3weight),
      weighted_off_ppg_mean = (adj_off_ppg_PY1 * py1weight) +
        (adj_off_ppg_PY2 * py2weight) +
        (adj_off_ppg_PY3 * py3weight),
      weighted_def_ppg_mean = (adj_def_ppg_PY1 * py1weight) +
        (adj_def_ppg_PY2 * py2weight) +
        (adj_def_ppg_PY3 * py3weight),
      weighted_net_st_ppg_mean = (net_adj_st_ppg_PY1 * py1weight) +
        (net_adj_st_ppg_PY2 * py2weight) +
        (net_adj_st_ppg_PY3 * py3weight),
      off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
      def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
      weighted_off_epa = (adj_off_epa_PY3 * py3weight) +
        (adj_off_epa_PY2 * py2weight) +
        (adj_off_epa_PY1 * py1weight),
      weighted_off_ypp = (adj_off_ypp_PY3 * py3weight) +
        (adj_off_ypp_PY2 * py2weight) +
        (adj_off_ypp_PY1 * py1weight),
      weighted_off_success_rate = (off_success_rate_PY3 * py3weight) +
        (off_success_rate_PY2 * py2weight) +
        (off_success_rate_PY1 * py1weight),
      weighted_off_explosiveness = (adj_off_explosiveness_PY3 * py3weight) +
        (adj_off_explosiveness_PY2 * py2weight) +
        (adj_off_explosiveness_PY1 * py1weight),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY3 * py3weight) +
        (off_third_conv_rate_PY2 * py2weight) +
        (off_third_conv_rate_PY1 * py1weight),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY3 * py3weight) +
        (off_pts_per_opp_PY2 * py2weight) +
        (off_pts_per_opp_PY1 * py1weight),
      weighted_off_plays_pg = (adj_off_plays_pg_PY3 * py3weight) +
        (adj_off_plays_pg_PY2 * py2weight) +
        (adj_off_plays_pg_PY1 * py1weight),
      weighted_def_plays_pg = (adj_def_plays_pg_PY3 * py3weight) +
        (adj_def_plays_pg_PY2 * py2weight) +
        (adj_def_plays_pg_PY1 * py1weight),
      weighted_def_epa = (adj_def_epa_PY3 * py3weight) +
        (adj_def_epa_PY2 * py2weight) +
        (adj_def_epa_PY1 * py1weight),
      weighted_def_ypp = (adj_def_ypp_PY3 * py3weight) +
        (adj_def_ypp_PY2 * py2weight) +
        (adj_def_ypp_PY1 * py1weight),
      weighted_def_success_rate = (def_success_rate_PY3 * py3weight) +
        (def_success_rate_PY2 * py2weight) +
        (def_success_rate_PY1 * py1weight),
      weighted_def_explosiveness = (adj_def_explosiveness_PY3 * py3weight) +
        (adj_def_explosiveness_PY2 * py2weight) +
        (adj_def_explosiveness_PY1 * py1weight),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY3 * py3weight) +
        (def_third_conv_rate_PY2 * py2weight) +
        (def_third_conv_rate_PY1 * py1weight),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY3 * py3weight) +
        (def_pts_per_opp_PY2 * py2weight) +
        (def_pts_per_opp_PY1 * py1weight),
      weighted_def_havoc_total = (def_havoc_total_PY3 * py3weight) +
        (def_havoc_total_PY2 * py2weight) +
        (def_havoc_total_PY1 * py1weight),
      weighted_net_kick_return_yds = ((kick_return_yds_PY3 -
        kick_return_yds_allowed_PY3) *
        py3weight) +
        ((kick_return_yds_PY2 - kick_return_yds_allowed_PY2) * py2weight) +
        ((kick_return_yds_PY1 - kick_return_yds_allowed_PY1) * py1weight),
      weighted_net_punt_return_yds = ((punt_return_yds_PY3 -
        punt_return_yds_allowed_PY3) *
        py3weight) +
        ((punt_return_yds_PY2 - punt_return_yds_allowed_PY2) * py2weight) +
        ((punt_return_yds_PY1 - punt_return_yds_allowed_PY1) * py1weight),
      weighted_net_fg_rate = ((fg_rate_PY3 - fg_rate_allowed_PY3) * py3weight) +
        ((fg_rate_PY2 - fg_rate_allowed_PY2) * py2weight) +
        ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight),
      weighted_net_fg_made_pg = ((fg_made_pg_PY3 - fg_made_pg_allowed_PY3) *
        py3weight) +
        ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) * py2weight) +
        ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * py1weight),
      #  weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * py3weight) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * py2weight) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight),
      weighted_net_adj_st_epa = (net_adj_st_epa_PY3 * py3weight) +
        (net_adj_st_epa_PY2 * py2weight) +
        (net_adj_st_epa_PY1 * py1weight)
    ) #,
  #  weighted_mean_oppdef_epa = ((oppdef_epa_PY3 * py3weight) + (oppdef_epa_PY2 * py2weight) + (oppdef_epa_PY1 * py1weight)),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY3 * py3weight) + (oppoff_epa_PY2 * py2weight) + (oppoff_epa_PY1 * py1weight))
} else if (as.integer(cfb_week) <= 5) {
  ##### Week 1-5 Weighted Variables #####
  ### PY 1-2, 1 week of current season
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_recruit_pts = (recruit_pts_PY1 * py1weight) +
        (recruit_pts_PY2 * (py2weight + cyweight)),
      weighted_off_ppg_mean = (adj_off_ppg * cyweight) +
        (adj_off_ppg_PY1 * py1weight) +
        (adj_off_ppg_PY2 * py2weight),
      weighted_def_ppg_mean = (adj_def_ppg * cyweight) +
        (adj_def_ppg_PY1 * py1weight) +
        (adj_def_ppg_PY2 * py2weight),
      weighted_net_st_ppg_mean = (net_adj_st_ppg * cyweight) +
        (net_adj_st_ppg_PY1 * py1weight) +
        (net_adj_st_ppg_PY2 * py2weight),
      off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
      def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
      weighted_off_epa = (adj_off_epa_PY2 * py2weight) +
        (adj_off_epa_PY1 * py1weight) +
        (adj_off_epa * cyweight),
      weighted_off_ypp = (adj_off_ypp_PY2 * py2weight) +
        (adj_off_ypp_PY1 * py1weight) +
        (adj_off_ypp * cyweight),
      weighted_off_success_rate = (off_success_rate_PY2 * py2weight) +
        (off_success_rate_PY1 * py1weight) +
        (off_success_rate * cyweight),
      weighted_off_explosiveness = (adj_off_explosiveness_PY2 * py2weight) +
        (adj_off_explosiveness_PY1 * py1weight) +
        (adj_off_explosiveness * cyweight),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY2 * py2weight) +
        (off_third_conv_rate_PY1 * py1weight) +
        (off_third_conv_rate * cyweight),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY2 * py2weight) +
        (off_pts_per_opp_PY1 * py1weight) +
        (off_pts_per_opp * cyweight),
      weighted_off_plays_pg = (off_plays_pg_PY2 * py2weight) +
        (off_plays_pg_PY1 * py1weight) +
        (off_plays_pg * cyweight),
      weighted_def_plays_pg = (def_plays_pg_PY2 * py2weight) +
        (def_plays_pg_PY1 * py1weight) +
        (def_plays_pg * cyweight),
      weighted_def_epa = (adj_def_epa_PY2 * py2weight) +
        (adj_def_epa_PY1 * py1weight) +
        (adj_def_epa * cyweight),
      weighted_def_ypp = (adj_def_ypp_PY2 * py2weight) +
        (adj_def_ypp_PY1 * py1weight) +
        (adj_def_ypp * cyweight),
      weighted_def_success_rate = (def_success_rate_PY2 * py2weight) +
        (def_success_rate_PY1 * py1weight) +
        (def_success_rate * cyweight),
      weighted_def_explosiveness = (adj_def_explosiveness_PY2 * py2weight) +
        (adj_def_explosiveness_PY1 * py1weight) +
        (adj_def_explosiveness * cyweight),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY2 * py2weight) +
        (def_third_conv_rate_PY1 * py1weight) +
        (def_third_conv_rate * cyweight),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY2 * py2weight) +
        (def_pts_per_opp_PY1 * py1weight) +
        (def_pts_per_opp * cyweight),
      weighted_def_havoc_total = (def_havoc_total_PY2 * py2weight) +
        (def_havoc_total_PY1 * py1weight) +
        (def_havoc_total * cyweight),
      weighted_net_kick_return_yds = ((kick_return_yds_PY2 -
        kick_return_yds_allowed_PY2) *
        py2weight) +
        ((kick_return_yds_PY1 - kick_return_yds_allowed_PY1) * py1weight) +
        ((kick_return_yds - kick_return_yds_allowed) * cyweight),
      weighted_net_punt_return_yds = ((punt_return_yds_PY2 -
        punt_return_yds_allowed_PY2) *
        py2weight) +
        ((punt_return_yds_PY1 - punt_return_yds_allowed_PY1) * py1weight) +
        ((punt_return_yds - punt_return_yds_allowed) * cyweight),
      weighted_net_fg_rate = ((fg_rate_PY2 - fg_rate_allowed_PY2) * py2weight) +
        ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight) +
        ((fg_rate - fg_rate_allowed) * cyweight),
      weighted_net_fg_made_pg = ((fg_made_pg_PY2 - fg_made_pg_allowed_PY2) *
        py2weight) +
        ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) * py1weight) +
        ((fg_made_pg - fg_made_pg_allowed) * cyweight),
      #  weighted_net_xpts_pg = ((xpts_pg_PY3 - xpts_allowed_pg_PY3) * py3weight) + ((xpts_pg_PY2 - xpts_allowed_pg_PY2) * py2weight) + ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight) + ((xpts_pg - xpts_allowed_pg) * cyweight),
      weighted_net_adj_st_epa = (net_adj_st_epa_PY2 * py2weight) +
        (net_adj_st_epa_PY1 * py1weight) +
        (net_adj_st_epa * cyweight)
    ) #,
  #  weighted_mean_oppdef_epa = (oppdef_epa_PY3 * py3weight) + (oppdef_epa_PY2 * py2weight) + (oppdef_epa_PY1 * py1weight) + (oppdef_epa * cyweight),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY3 * py3weight) + (oppoff_epa_PY2 * py2weight) + (oppoff_epa_PY1 * py1weight) + (oppoff_epa * cyweight))
} else if (as.integer(cfb_week) <= 9) {
  ##### Week 6-9 Weighted Variables #####
  ### only PY1 and current data
  ### adding weighted variables
  VoAVariables <- VoAVariables |>
    mutate(
      weighted_recruit_pts = (recruit_pts_PY1 * cyweight) +
        (recruit_pts_PY2 * py1weight),
      weighted_off_ppg_mean = (adj_off_ppg_PY1 * py1weight) +
        (adj_off_ppg * cyweight),
      weighted_def_ppg_mean = (adj_def_ppg_PY1 * py1weight) +
        (adj_def_ppg * cyweight),
      weighted_net_st_ppg_mean = (net_st_ppg_PY1 * py1weight) +
        (net_st_ppg * cyweight),
      off_ppg_aboveavg = weighted_off_ppg_mean - mean(weighted_off_ppg_mean),
      def_ppg_aboveavg = weighted_def_ppg_mean - mean(weighted_def_ppg_mean),
      weighted_off_epa = (adj_off_epa_PY1 * py1weight) +
        (adj_off_epa * cyweight),
      weighted_off_ypp = (adj_off_ypp_PY1 * py1weight) +
        (adj_off_ypp * cyweight),
      weighted_off_success_rate = (off_success_rate_PY1 * py1weight) +
        (off_success_rate * cyweight),
      weighted_off_explosiveness = (adj_off_explosiveness_PY1 * py1weight) +
        (adj_off_explosiveness * cyweight),
      weighted_off_third_conv_rate = (off_third_conv_rate_PY1 * py1weight) +
        (off_third_conv_rate * cyweight),
      weighted_off_pts_per_opp = (off_pts_per_opp_PY1 * py1weight) +
        (off_pts_per_opp * cyweight),
      weighted_off_plays_pg = (off_plays_pg_PY1 * py1weight) +
        (off_plays_pg * cyweight),
      weighted_def_plays_pg = (def_plays_pg_PY1 * py1weight) +
        (def_plays_pg * cyweight),
      weighted_def_epa = (adj_def_epa_PY1 * py1weight) +
        (adj_def_epa * cyweight),
      weighted_def_ypp = (adj_def_ypp_PY1 * py1weight) +
        (adj_def_ypp * cyweight),
      weighted_def_success_rate = (def_success_rate_PY1 * py1weight) +
        (def_success_rate * cyweight),
      weighted_def_explosiveness = (adj_def_explosiveness_PY1 * py1weight) +
        (adj_def_explosiveness * cyweight),
      weighted_def_third_conv_rate = (def_third_conv_rate_PY1 * py1weight) +
        (def_third_conv_rate * cyweight),
      weighted_def_pts_per_opp = (def_pts_per_opp_PY1 * py1weight) +
        (def_pts_per_opp * cyweight),
      weighted_def_havoc_total = (def_havoc_total_PY1 * py1weight) +
        (def_havoc_total * cyweight),
      weighted_net_kick_return_yds = ((kick_return_yds_PY1 -
        kick_return_yds_allowed_PY1) *
        py1weight) +
        ((kick_return_yds - kick_return_yds_allowed) * cyweight),
      weighted_net_punt_return_yds = ((punt_return_yds_PY1 -
        punt_return_yds_allowed_PY1) *
        py1weight) +
        ((punt_return_yds - punt_return_yds_allowed) * cyweight),
      weighted_net_fg_rate = ((fg_rate_PY1 - fg_rate_allowed_PY1) * py1weight) +
        ((fg_rate - fg_rate_allowed) * cyweight),
      weighted_net_fg_made_pg = ((fg_made_pg_PY1 - fg_made_pg_allowed_PY1) *
        py1weight) +
        ((fg_made_pg - fg_made_pg_allowed) * cyweight),
      #  weighted_net_xpts_pg = ((xpts_pg_PY1 - xpts_allowed_pg_PY1) * py1weight) + ((xpts_pg - xpts_allowed_pg) * cyweight),
      weighted_net_adj_st_epa = (net_adj_st_epa_PY1 * py1weight) +
        (net_adj_st_epa * cyweight)
    ) #,
  #  weighted_mean_oppdef_epa = (oppdef_epa_PY1 * py1weight) + (oppdef_epa * cyweight),
  #  weighted_mean_oppoff_epa = (oppoff_epa_PY1 * py1weight) + (oppoff_epa * cyweight))
} else {
  print("no weighted variables, all current season data")
}


##### Adding Week number to VoA Variables, Eliminating NAs, fixing conferences #####
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
  VoATrain_Ncols <- ncol(VoATrain_PY1) + 1
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
### EPA stats, explosiveness stats, success rates, havoc rates, Yards/Play, pts/scoring opp weighted 2x in PYs, 3x for current season,
## all #x above refer to weighting being done on top of weighting being done based on which year the data is from
## recruiting 3x in PY3 and PY2, 2x in PY1, 1x for current year
# recruiting phased out after only current season stats are being used (currently week 7)
## talent ranked 1x in PY3 and PY2, 3x in PY1
if (as.integer(cfb_week) == 0) {
  ##### Week 0 Variable Ranks #####
  ### applying end of season rank process to dfs which will be used to train Stan model first
  VoATrain_PY1 <- rank_voa_cols(VoATrain_PY1)
  VoATrain_PY2 <- rank_voa_cols(VoATrain_PY2)
  VoATrain_PY3 <- rank_voa_cols(VoATrain_PY3)
  # VoATrain_PY4 <- rank_voa_cols(VoATrain_PY4)
  ### PY3 ranks added first
  # fmt: skip
  VoAVariables <- VoAVariables |>
    mutate(Rank_Comp_Pct_PY3 = dense_rank(desc(off_comp_pct_PY3)),
           Rank_off_pass_ypa_PY3 = dense_rank(desc(off_pass_ypa_PY3)),
           Rank_off_pass_ypr_PY3 = dense_rank(desc(off_pass_ypr_PY3)),
          #  Rank_int_Pct_PY3 = dense_rank(int_pct_PY3),
           Rank_off_rush_ypa_PY3 = dense_rank(desc(off_rush_ypa_PY3)),
           Rank_third_conv_rate_PY3 = dense_rank(desc(off_third_conv_rate_PY3)),
           Rank_off_fourth_conv_rate_PY3 = dense_rank(desc(off_fourth_conv_rate_PY3)),
          #  Rank_penalty_Yds_pg_PY3 = dense_rank(penalty_yds_pg_PY3),
          #  Rank_yds_per_penalty_PY3 = dense_rank(yards_per_penalty_PY3),
           Rank_kick_return_yds_PY3 = dense_rank(desc(kick_return_yds_PY3)),
           Rank_punt_return_yds_PY3 = dense_rank(desc(punt_return_yds_PY3)),
           Rank_off_ypg_PY3 = dense_rank(desc(off_ypg_PY3)),
           Rank_off_pass_ypg_PY3 = dense_rank(desc(off_pass_ypg_PY3)),
           Rank_off_rush_ypg_PY3 = dense_rank(desc(off_rush_ypg_PY3)),
          #  Rank_First_Downs_pg_PY3 = dense_rank(desc(first_downs_pg_PY3)),
           Rank_Off_YPP_PY3 = dense_rank(desc(adj_off_ypp_PY3)),
          #  Rank_def_ints_pg_PY3 = dense_rank(desc(def_interceptions_pg_PY3)),
           Rank_Off_EPA_PY3 = dense_rank(desc(adj_off_epa_PY3)),
           Rank_Off_Success_Rt_PY3 = dense_rank(desc(off_success_rate_PY3)),
           Rank_Off_Explosiveness_PY3 = dense_rank(desc(adj_off_explosiveness_PY3)),
           Rank_Off_Pwr_Success_PY3 = dense_rank(desc(off_power_success_PY3)),
           Rank_Off_Stuff_Rt_PY3 = dense_rank(off_stuff_rate_PY3),
           Rank_Off_Line_Yds_PY3 = dense_rank(desc(off_line_yds_PY3)),
           Rank_Off_Pts_Per_Opp_PY3 = dense_rank(desc(off_pts_per_opp_PY3)),
           Rank_Off_Havoc_Total_PY3 = dense_rank(off_havoc_total_PY3),
           Rank_Off_Rush_Play_EPA_PY3 = dense_rank(desc(off_rush_epa_PY3)),
           Rank_Off_Rush_Play_Success_Rt_PY3 = dense_rank(desc(off_rush_success_rate_PY3)),
           Rank_Off_Rush_Play_Explosiveness_PY3 = dense_rank(desc(off_rush_explosiveness_PY3)),
           Rank_Off_Pass_Play_EPA_PY3 = dense_rank(desc(off_pass_epa_PY3)),
           Rank_Off_Pass_Play_Success_Rt_PY3 = dense_rank(desc(off_pass_success_rate_PY3)),
           Rank_Off_Pass_Play_Explosiveness_PY3 = dense_rank(desc(off_pass_explosiveness_PY3)),
           Rank_Def_EPA_PY3 = dense_rank(adj_def_epa_PY3),
           Rank_Def_Success_Rt_PY3 = dense_rank(def_success_rate_PY3),
           Rank_Def_Explosiveness_PY3 = dense_rank(adj_def_explosiveness_PY3),
           Rank_Def_Pwr_Success_PY3 = dense_rank(def_power_success_PY3),
           Rank_Def_Stuff_Rt_PY3 = dense_rank(desc(def_stuff_rate_PY3)),
           Rank_Def_Line_Yds_PY3 = dense_rank(def_line_yds_PY3),
           Rank_Def_Pts_Per_Opp_PY3 = dense_rank(def_pts_per_opp_PY3),
           Rank_Def_Havoc_Total_PY3 = dense_rank(desc(def_havoc_total_PY3)),
           Rank_Def_Rush_Play_EPA_PY3 = dense_rank(def_rush_epa_PY3),
           Rank_Def_Rush_Play_Success_Rt_PY3 = dense_rank(def_rush_success_rate_PY3),
           Rank_Def_Rush_Play_Explosiveness_PY3 = dense_rank(def_rush_explosiveness_PY3),
           Rank_Def_Pass_Play_EPA_PY3 = dense_rank(def_pass_epa_PY3),
           Rank_Def_Pass_Play_Success_Rt_PY3 = dense_rank(def_pass_success_rate_PY3),
           Rank_Def_Pass_Play_Explosiveness_PY3 = dense_rank(def_pass_explosiveness_PY3),
           Rank_recruit_Pts_PY3 = dense_rank(desc(recruit_pts_PY3)),
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
           Rank_kick_return_yds_PY2 = dense_rank(desc(kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           ## PY2 weighted twice
           Rank_recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
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
           Rank_kick_return_yds_PY1 = dense_rank(desc(kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
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
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rate_PY1),
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
           Rank_kick_return_yds_PY2 = dense_rank(desc(kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
          #  # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
          #  # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
          #  # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
          #  # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
          #  # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
          #  # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rate_PY2),
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
           Rank_Off_Success_Rt_PY2_col2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2_col2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2_col2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2_col2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2_col2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2_col2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2_col2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2_col2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2_col2 = dense_rank(off_havoc_total_PY2),
           Rank_Off_Standard_Down_EPA_PY2_col2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2_col2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2_col2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2_col2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_rush_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2_col2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2_col2 = dense_rank(desc(off_pass_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2_col2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2_col2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2_col2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2_col2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2_col2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2_col2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2_col2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2_col2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2_col2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2_col2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2_col2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2_col2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2_col2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2_col2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2_col2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2_col2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2_col2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2_col2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2_col2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2_col2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2_col2 = dense_rank(def_rush_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2_col2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2_col2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2_col2 = dense_rank(def_pass_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2_col2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2_col2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2_col2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2_col2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2_col2 = dense_rank(desc(Explosiveness_diff_PY2)),
           Rank_recruit_Pts_PY2_col2 = dense_rank(desc(recruit_pts_PY2)),
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
           Rank_kick_return_yds_PY1 = dense_rank(desc(kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
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
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1_col2 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1_col2 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1_col2 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1_col2 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col2 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
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
           Rank_kick_return_yds = dense_rank(desc(kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
          #  Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
          #  Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
          #  Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
          #  Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
          #  # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
          #  # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
          #  # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
          #  # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rate),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Extra weighted variables for current year
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rate),
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
           Rank_kick_return_yds_PY2 = dense_rank(desc(kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rate_PY2),
           Rank_Def_Pass_Play_Explosiveness_PY2 = dense_rank(def_pass_explosiveness_PY2),
           Rank_EPA_diff_PY2 = dense_rank(desc(EPA_diff_PY2)),
           Rank_SuccessRt_diff_PY2 = dense_rank(desc(SuccessRt_diff_PY2)),
           Rank_HavocRt_diff_PY2 = dense_rank(desc(HavocRt_diff_PY2)),
           Rank_Explosiveness_diff_PY2 = dense_rank(desc(Explosiveness_diff_PY2)),
           Rank_recruit_Pts_PY2 = dense_rank(desc(recruit_pts_PY2)),
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
           Rank_kick_return_yds_PY1 = dense_rank(desc(kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
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
           Rank_Off_Success_Rt_PY1_col2 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1_col2 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1_col2 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1_col2 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1_col2 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1_col2 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1_col2 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Standard_Down_EPA_PY1_col2 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1_col2 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1_col2 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1_col2 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1_col2 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1_col2 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1_col2 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1_col2 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1_col2 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1_col2 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1_col2 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1_col2 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1_col2 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1_col2 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1_col2 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1_col2 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1_col2 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1_col2 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1_col2 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1_col2 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1_col2 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1_col2 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1_col2 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1_col2 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1_col2 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1_col2 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1_col2 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1_col2 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1_col2 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1_col2 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1_col2 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1_col2 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1_col2 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1_col2 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1_col2 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1_col2 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1_col2 = dense_rank(desc(recruit_pts_PY1)),
           ### ranking current season stats now
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
           Rank_kick_return_yds = dense_rank(desc(kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
          #  Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
          #  Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
          #  Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
          #  Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rate),
           Rank_Def_Pass_Play_Explosiveness = dense_rank(def_pass_explosiveness),
           Rank_EPA_diff = dense_rank(desc(EPA_diff)),
           Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
           Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
           Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
           ## Extra weighted variables for current year
           Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
           Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rate),
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
           Rank_kick_return_yds_PY2 = dense_rank(desc(kick_return_yds_PY2)),
           Rank_punt_return_yds_PY2 = dense_rank(desc(punt_return_yds_PY2)),
           Rank_off_ypg_PY2 = dense_rank(desc(off_ypg_PY2)),
           Rank_off_pass_ypg_PY2 = dense_rank(desc(off_pass_ypg_PY2)),
           Rank_off_rush_ypg_PY2 = dense_rank(desc(off_rush_ypg_PY2)),
           # Rank_first_downs_pg_PY2 = dense_rank(desc(first_downs_pg_PY2)),
           Rank_Off_YPP_PY2 = dense_rank(desc(adj_off_ypp_PY2)),
           # Rank_def_ints_pg_PY2 = dense_rank(desc(def_interceptions_pg_PY2)),
           Rank_Off_EPA_PY2 = dense_rank(desc(adj_off_epa_PY2)),
           Rank_Off_Success_Rt_PY2 = dense_rank(desc(off_success_rate_PY2)),
           Rank_Off_Explosiveness_PY2 = dense_rank(desc(adj_off_explosiveness_PY2)),
           Rank_Off_Pwr_Success_PY2 = dense_rank(desc(off_power_success_PY2)),
           Rank_Off_Stuff_Rt_PY2 = dense_rank(off_stuff_rate_PY2),
           Rank_Off_Line_Yds_PY2 = dense_rank(desc(off_line_yds_PY2)),
          #  Rank_Off_Second_Lvl_Yds_PY2 = dense_rank(desc(off_second_lvl_yds_PY2)),
          #  Rank_Off_Open_Field_Yds_PY2 = dense_rank(desc(off_open_field_yds_PY2)),
           Rank_Off_Pts_Per_Opp_PY2 = dense_rank(desc(off_pts_per_opp_PY2)),
           Rank_Off_Havoc_Total_PY2 = dense_rank(off_havoc_total_PY2),
          #  Rank_Off_Havoc_Front_PY2 = dense_rank(off_havoc_front_seven_PY2),
          #  Rank_Off_Havoc_DB_PY2 = dense_rank(off_havoc_db_PY2),
           Rank_Off_Standard_Down_EPA_PY2 = dense_rank(desc(off_standard_downs_epa_PY2)),
           Rank_Off_Standard_Down_Success_Rt_PY2 = dense_rank(desc(off_standard_downs_success_rate_PY2)),
           Rank_Off_Standard_Down_Explosiveness_PY2 = dense_rank(desc(off_standard_downs_explosiveness_PY2)),
           Rank_Off_Pass_Down_EPA_PY2 = dense_rank(desc(off_passing_downs_epa_PY2)),
           Rank_Off_Pass_Down_Success_Rt_PY2 = dense_rank(desc(off_passing_downs_success_rate_PY2)),
           Rank_Off_Pass_Down_Explosiveness_PY2 = dense_rank(desc(off_passing_downs_explosiveness_PY2)),
           Rank_Off_Rush_Play_EPA_PY2 = dense_rank(desc(off_rush_epa_PY2)),
           Rank_Off_Rush_Play_Success_Rt_PY2 = dense_rank(desc(off_rush_success_rate_PY2)),
           Rank_Off_Rush_Play_Explosiveness_PY2 = dense_rank(desc(off_rush_explosiveness_PY2)),
           Rank_Off_Pass_Play_EPA_PY2 = dense_rank(desc(off_pass_epa_PY2)),
           Rank_Off_Pass_Play_Success_Rt_PY2 = dense_rank(desc(off_pass_success_rate_PY2)),
           Rank_Off_Pass_Play_Explosiveness_PY2 = dense_rank(desc(off_pass_explosiveness_PY2)),
           Rank_Def_EPA_PY2 = dense_rank(adj_def_epa_PY2),
           Rank_Def_Success_Rt_PY2 = dense_rank(def_success_rate_PY2),
           Rank_Def_Explosiveness_PY2 = dense_rank(adj_def_explosiveness_PY2),
           Rank_Def_Pwr_Success_PY2 = dense_rank(def_power_success_PY2),
           Rank_Def_Stuff_Rt_PY2 = dense_rank(desc(def_stuff_rate_PY2)),
           Rank_Def_Line_Yds_PY2 = dense_rank(def_line_yds_PY2),
           # Rank_def_second_Lvl_Yds_PY2 = dense_rank(def_second_lvl_yds_PY2),
           # Rank_def_open_Field_Yds_PY2 = dense_rank(def_open_field_yds_PY2),
           Rank_Def_Pts_Per_Opp_PY2 = dense_rank(def_pts_per_opp_PY2),
           Rank_Def_Havoc_Total_PY2 = dense_rank(desc(def_havoc_total_PY2)),
           # Rank_def_havoc_front_Seven_PY2 = dense_rank(desc(def_havoc_front_seven_PY2)),
           # Rank_def_havoc_db_PY2 = dense_rank(desc(def_havoc_db_PY2)),
           Rank_Def_Standard_Down_EPA_PY2 = dense_rank(def_standard_downs_epa_PY2),
           Rank_Def_Standard_Down_Success_Rt_PY2 = dense_rank(def_standard_downs_success_rate_PY2),
           Rank_Def_Standard_Down_Explosiveness_PY2 = dense_rank(def_standard_downs_explosiveness_PY2),
           Rank_Def_Pass_Down_EPA_PY2 = dense_rank(def_passing_downs_epa_PY2),
           Rank_Def_Pass_Down_Success_Rt_PY2 = dense_rank(def_passing_downs_success_rate_PY2),
           Rank_Def_Pass_Down_Explosiveness_PY2 = dense_rank(def_passing_downs_explosiveness_PY2),
           Rank_Def_Rush_Play_EPA_PY2 = dense_rank(def_rush_epa_PY2),
           Rank_Def_Rush_Play_Success_Rt_PY2 = dense_rank(def_rush_success_rate_PY2),
           Rank_Def_Rush_Play_Explosiveness_PY2 = dense_rank(def_rush_explosiveness_PY2),
           Rank_Def_Pass_Play_EPA_PY2 = dense_rank(def_pass_epa_PY2),
           Rank_Def_Pass_Play_Success_Rt_PY2 = dense_rank(def_pass_success_rate_PY2),
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
           Rank_kick_return_yds_PY1 = dense_rank(desc(kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           # Rank_first_downs_pg_PY1 = dense_rank(desc(first_downs_pg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
          #  Rank_Off_Second_Lvl_Yds_PY1 = dense_rank(desc(off_second_lvl_yds_PY1)),
          #  Rank_Off_Open_Field_Yds_PY1 = dense_rank(desc(off_open_field_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
          #  Rank_Off_Havoc_Front_PY1 = dense_rank(off_havoc_front_seven_PY1),
          #  Rank_Off_Havoc_DB_PY1 = dense_rank(off_havoc_db_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rate_PY1),
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
           Rank_kick_return_yds = dense_rank(desc(kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
          #  Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
          #  Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
          #  Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
          #  Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rate),
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
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds_col2 = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rate),
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
           Rank_kick_return_yds_PY1 = dense_rank(desc(kick_return_yds_PY1)),
           Rank_punt_return_yds_PY1 = dense_rank(desc(punt_return_yds_PY1)),
           Rank_off_ypg_PY1 = dense_rank(desc(off_ypg_PY1)),
           Rank_off_pass_ypg_PY1 = dense_rank(desc(off_pass_ypg_PY1)),
           Rank_off_rush_ypg_PY1 = dense_rank(desc(off_rush_ypg_PY1)),
           Rank_Off_YPP_PY1 = dense_rank(desc(adj_off_ypp_PY1)),
           # Rank_def_ints_pg_PY1 = dense_rank(desc(def_interceptions_pg_PY1)),
           Rank_Off_EPA_PY1 = dense_rank(desc(adj_off_epa_PY1)),
           Rank_Off_Success_Rt_PY1 = dense_rank(desc(off_success_rate_PY1)),
           Rank_Off_Explosiveness_PY1 = dense_rank(desc(adj_off_explosiveness_PY1)),
           Rank_Off_Pwr_Success_PY1 = dense_rank(desc(off_power_success_PY1)),
           Rank_Off_Stuff_Rt_PY1 = dense_rank(off_stuff_rate_PY1),
           Rank_Off_Line_Yds_PY1 = dense_rank(desc(off_line_yds_PY1)),
           Rank_Off_Pts_Per_Opp_PY1 = dense_rank(desc(off_pts_per_opp_PY1)),
           Rank_Off_Havoc_Total_PY1 = dense_rank(off_havoc_total_PY1),
           Rank_Off_Standard_Down_EPA_PY1 = dense_rank(desc(off_standard_downs_epa_PY1)),
           Rank_Off_Standard_Down_Success_Rt_PY1 = dense_rank(desc(off_standard_downs_success_rate_PY1)),
           Rank_Off_Standard_Down_Explosiveness_PY1 = dense_rank(desc(off_standard_downs_explosiveness_PY1)),
           Rank_Off_Pass_Down_EPA_PY1 = dense_rank(desc(off_passing_downs_epa_PY1)),
           Rank_Off_Pass_Down_Success_Rt_PY1 = dense_rank(desc(off_passing_downs_success_rate_PY1)),
           Rank_Off_Pass_Down_Explosiveness_PY1 = dense_rank(desc(off_passing_downs_explosiveness_PY1)),
           Rank_Off_Rush_Play_EPA_PY1 = dense_rank(desc(off_rush_epa_PY1)),
           Rank_Off_Rush_Play_Success_Rt_PY1 = dense_rank(desc(off_rush_success_rate_PY1)),
           Rank_Off_Rush_Play_Explosiveness_PY1 = dense_rank(desc(off_rush_explosiveness_PY1)),
           Rank_Off_Pass_Play_EPA_PY1 = dense_rank(desc(off_pass_epa_PY1)),
           Rank_Off_Pass_Play_Success_Rt_PY1 = dense_rank(desc(off_pass_success_rate_PY1)),
           Rank_Off_Pass_Play_Explosiveness_PY1 = dense_rank(desc(off_pass_explosiveness_PY1)),
           Rank_Def_EPA_PY1 = dense_rank(adj_def_epa_PY1),
           Rank_Def_Success_Rt_PY1 = dense_rank(def_success_rate_PY1),
           Rank_Def_Explosiveness_PY1 = dense_rank(adj_def_explosiveness_PY1),
           Rank_Def_Pwr_Success_PY1 = dense_rank(def_power_success_PY1),
           Rank_Def_Stuff_Rt_PY1 = dense_rank(desc(def_stuff_rate_PY1)),
           Rank_Def_Line_Yds_PY1 = dense_rank(def_line_yds_PY1),
           # Rank_def_second_Lvl_Yds_PY1 = dense_rank(def_second_lvl_yds_PY1),
           # Rank_def_open_Field_Yds_PY1 = dense_rank(def_open_field_yds_PY1),
           Rank_Def_Pts_Per_Opp_PY1 = dense_rank(def_pts_per_opp_PY1),
           Rank_Def_Havoc_Total_PY1 = dense_rank(desc(def_havoc_total_PY1)),
           # Rank_def_havoc_front_Seven_PY1 = dense_rank(desc(def_havoc_front_seven_PY1)),
           # Rank_def_havoc_db_PY1 = dense_rank(desc(def_havoc_db_PY1)),
           Rank_Def_Standard_Down_EPA_PY1 = dense_rank(def_standard_downs_epa_PY1),
           Rank_Def_Standard_Down_Success_Rt_PY1 = dense_rank(def_standard_downs_success_rate_PY1),
           Rank_Def_Standard_Down_Explosiveness_PY1 = dense_rank(def_standard_downs_explosiveness_PY1),
           Rank_Def_Pass_Down_EPA_PY1 = dense_rank(def_passing_downs_epa_PY1),
           Rank_Def_Pass_Down_Success_Rt_PY1 = dense_rank(def_passing_downs_success_rate_PY1),
           Rank_Def_Pass_Down_Explosiveness_PY1 = dense_rank(def_passing_downs_explosiveness_PY1),
           Rank_Def_Rush_Play_EPA_PY1 = dense_rank(def_rush_epa_PY1),
           Rank_Def_Rush_Play_Success_Rt_PY1 = dense_rank(def_rush_success_rate_PY1),
           Rank_Def_Rush_Play_Explosiveness_PY1 = dense_rank(def_rush_explosiveness_PY1),
           Rank_Def_Pass_Play_EPA_PY1 = dense_rank(def_pass_epa_PY1),
           Rank_Def_Pass_Play_Success_Rt_PY1 = dense_rank(def_pass_success_rate_PY1),
           Rank_Def_Pass_Play_Explosiveness_PY1 = dense_rank(def_pass_explosiveness_PY1),
           Rank_EPA_diff_PY1 = dense_rank(desc(EPA_diff_PY1)),
           Rank_SuccessRt_diff_PY1 = dense_rank(desc(SuccessRt_diff_PY1)),
           Rank_HavocRt_diff_PY1 = dense_rank(desc(HavocRt_diff_PY1)),
           Rank_Explosiveness_diff_PY1 = dense_rank(desc(Explosiveness_diff_PY1)),
           Rank_recruit_Pts_PY1 = dense_rank(desc(recruit_pts_PY1)),
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
           Rank_kick_return_yds = dense_rank(desc(kick_return_yds)),
           Rank_punt_return_yds = dense_rank(desc(punt_return_yds)),
           Rank_off_ypg = dense_rank(desc(off_ypg)),
           Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
           Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
           # Rank_first_downs_pg = dense_rank(desc(first_downs_pg)),
           Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
           # Rank_def_ints_pg = dense_rank(desc(def_interceptions_pg)),
           Rank_Off_EPA = dense_rank(desc(adj_off_epa)),
           Rank_Off_Success_Rt = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds = dense_rank(desc(off_line_yds)),
          #  Rank_Off_Second_Lvl_Yds = dense_rank(desc(off_second_lvl_yds)),
          #  Rank_Off_Open_Field_Yds = dense_rank(desc(off_open_field_yds)),
           Rank_Off_Pts_Per_Opp = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total = dense_rank(off_havoc_total),
          #  Rank_Off_Havoc_Front = dense_rank(off_havoc_front_seven),
          #  Rank_Off_Havoc_DB = dense_rank(off_havoc_db),
           Rank_Off_Standard_Down_EPA = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
           Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rate)),
           Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(off_rush_explosiveness)),
           Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
           Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rate)),
           Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(off_pass_explosiveness)),
           Rank_Def_EPA = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt = dense_rank(def_success_rate),
           Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rate),
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
           Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
           Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
           Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
           Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
           Rank_Off_Line_Yds_col2 = dense_rank(desc(off_line_yds)),
           Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
           Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
           Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(off_standard_downs_epa)),
           Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(off_standard_downs_success_rate)),
           Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(off_standard_downs_explosiveness)),
           Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
           Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(off_passing_downs_success_rate)),
           Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(off_passing_downs_explosiveness)),
           Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
           Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
           Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
           Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
           Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
           Rank_Def_Line_Yds_col2 = dense_rank(def_line_yds),
           # Rank_def_second_Lvl_Yds_col2 = dense_rank(def_second_lvl_yds),
           # Rank_def_open_Field_Yds_col2 = dense_rank(def_open_field_yds),
           Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
           Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
           # Rank_def_havoc_front_Seven_col2 = dense_rank(desc(def_havoc_front_seven)),
           # Rank_def_havoc_db_col2 = dense_rank(desc(def_havoc_db)),
           Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
           Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(def_standard_downs_success_rate),
           Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(def_standard_downs_explosiveness),
           Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
           Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(def_passing_downs_success_rate),
           Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(def_passing_downs_explosiveness),
           Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
           Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rate),
           Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(def_rush_explosiveness),
           Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
           Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(def_pass_success_rate),
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
  # VoAVariables$season <- rep(as.integer(year), nrow(VoAVariables))
  ### Append new column of Model output, which is the mean of all rank columns
  VoATrain_PY1 <- VoATrain_PY1 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY1[,
        VoATrain_Ncols:ncol(VoATrain_PY1)
      ]))
    )
  VoATrain_PY2 <- VoATrain_PY2 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY2[,
        VoATrain_Ncols:ncol(VoATrain_PY2)
      ]))
    )
  VoATrain_PY3 <- VoATrain_PY3 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY3[,
        VoATrain_Ncols:ncol(VoATrain_PY3)
      ]))
    )
  # VoATrain_PY4 <- VoATrain_PY4 |>
  #   mutate(
  #     VoA_Output = (rowMeans(VoATrain_PY4[,
  #       VoATrain_Ncols:ncol(VoATrain_PY4)
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
  VoATrain_PY1 <- calc_output_conf_avg(VoATrain_PY1)
  VoATrain_PY2 <- calc_output_conf_avg(VoATrain_PY2)
  VoATrain_PY3 <- calc_output_conf_avg(VoATrain_PY3)
  # VoATrain_PY4 <- calc_output_conf_avg(VoATrain_PY4)
  VoAVariables <- calc_output_conf_avg(VoAVariables)
} else {
  VoAVariables <- calc_output_conf_avg(VoAVariables)
}


##### Re running rowMeans function to get VoA Output #####
### script wouldn't run properly without a real number in the later weeks so I'll have to come back and edit the number in during the season as I figure out how big VoAVariables gets
if (as.integer(cfb_week) == 0) {
  ### Append new column of Model output, which is the mean of all rank columns + conference averages
  VoATrain_PY1 <- VoATrain_PY1 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY1[,
        VoATrain_Ncols:ncol(VoATrain_PY1)
      ]))
    )
  VoATrain_PY2 <- VoATrain_PY2 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY2[,
        VoATrain_Ncols:ncol(VoATrain_PY2)
      ]))
    )
  VoATrain_PY3 <- VoATrain_PY3 |>
    mutate(
      VoA_Output = (rowMeans(VoATrain_PY3[,
        VoATrain_Ncols:ncol(VoATrain_PY3)
      ]))
    )
  # VoATrain_PY4 <- VoATrain_PY4 |>
  #   mutate(
  #     VoA_Output = (rowMeans(VoATrain_PY4[,
  #       VoATrain_Ncols:ncol(VoATrain_PY4)
  #     ]))
  #   )
  ### binding train dfs together since there are no more calculations to perform separately
  # VoATrain <- rbind(
  #   VoATrain_PY1,
  #   rbind(
  #     VoATrain_PY2,
  #     rbind(VoATrain_PY3, VoATrain_PY4)
  #   )
  # )
  VoATrain <- rbind(
    VoATrain_PY1,
    rbind(VoATrain_PY2, VoATrain_PY3)
  )
  write_parquet(
    VoATrain,
    here("Data", paste0("VoA", year), "ModelTraining", "VoATrain.parquet")
  )
  VoAVariables <- VoAVariables |>
    mutate(
      VoA_Output = (rowMeans(VoAVariables[, VoA_Ncols:ncol(VoAVariables)]))
    )
  # write_parquet(
  #   VoAVariables,
  #   here("Data", paste0("VoA", year), "ModelTraining", "VoAVariables.parquet")
  # )
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
# includes EPA, success rate, explosiveness, VoA_Output, VoA's Conf_Rk, and pts_per_opp (offense and defense where applicable)
# set.seed(802)

##### using Stan to create FPI/SP+ like metrics #####
if (as.integer(cfb_week) == 0) {
  ##### Week 0 Stan Models #####
  ### VoA Offensive Rating Model
  ### making list of data to declare what goes into stan model
  Off_VoA_datalist <- list(
    N = nrow(VoATrain),
    off_ppg = VoATrain$adj_off_ppg,
    off_epa = VoATrain$adj_off_epa,
    off_ypp = VoATrain$adj_off_ypp,
    off_success_rate = VoATrain$off_success_rate,
    off_explosiveness = VoATrain$adj_off_explosiveness,
    third_conv_rate = VoATrain$off_third_conv_rate,
    off_pts_per_opp = VoATrain$off_pts_per_opp,
    off_plays_pg = VoATrain$adj_off_plays_pg,
    recruit_pts = VoATrain$recruit_pts,
    VoA_Output = VoATrain$VoA_Output,
    Conference_Strength = VoATrain$Conf_Rk
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

  ### saving fitted model object so I don't have to refit the model ever again
  ## annoyingly when I read this object back in I can't do anything with it which is frankly fucking infuriating
  ## fuck you you incessant snobs at mcmc stan
  # write_rds(
  #   Off_VoA_fit,
  #   file = here("Data", "FittedModels", "OffVoAStanFit.rds"),
  #   compress = "gz"
  # )

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
      "beta_recruit_pts",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### writing parameter draws as tabular file since saving the fit as an rds file doesn't work because of stupid bullshit
  write_parquet(
    Off_VoA_pars,
    here("Data", "FittedModels", "OffVoAParams.parquet")
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
  #           VoAVariables$weighted_off_success_rate[t] +
  #         Off_VoA_pars$beta_off_explosiveness[p] *
  #           VoAVariables$weighted_off_explosiveness[t] +
  #         Off_VoA_pars$beta_third_conv_rate[p] *
  #           VoAVariables$weighted_off_third_conv_rate[t] +
  #         Off_VoA_pars$beta_off_pts_per_opp[p] *
  #           VoAVariables$weighted_off_pts_per_opp[t] +
  #         Off_VoA_pars$beta_off_plays_pg[p] *
  #           VoAVariables$weighted_off_plays_pg[t] +
  #         Off_VoA_pars$beta_VoA_Output[p] * (1 / VoAVariables$VoA_Output[t]) +
  #         Off_VoA_pars$beta_Conf_Rk[p] *
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
    beta_off_success_rate = VoAVariables$weighted_off_success_rate,
    beta_off_explosiveness = VoAVariables$weighted_off_explosiveness,
    beta_third_conv_rate = VoAVariables$weighted_off_third_conv_rate,
    beta_off_pts_per_opp = VoAVariables$weighted_off_pts_per_opp,
    beta_off_plays_pg = VoAVariables$weighted_off_plays_pg,
    beta_recruit_pts = VoAVariables$weighted_recruit_pts,
    beta_VoA_Output = VoAVariables$VoA_Output,
    beta_Conference_Strength = VoAVariables$Conf_Rk
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

  ### fixing any values in the posterior sample that are below 0, since that is not possible for this metric
  Off_VoA_Ratings <- ifelse(
    Off_VoA_Ratings <= 0,
    abs(rnorm(1, 0, sd(Off_VoA_Ratings))) / 5,
    Off_VoA_Ratings
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
    def_success_rate = VoATrain$def_success_rate,
    def_explosiveness = VoATrain$adj_def_explosiveness,
    def_third_conv_rate = VoATrain$def_third_conv_rate,
    def_pts_per_opp = VoATrain$def_pts_per_opp,
    def_havoc_total = VoATrain$def_havoc_total,
    def_plays_pg = VoATrain$adj_def_plays_pg,
    recruit_pts = VoATrain$recruit_pts,
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
  ### saving the moel so I don't have to refit or recompile in later weeks, and also to keep initial coefficients stable
  # write_rds(
  #   Def_VoA_fit,
  #   file = here("Data", "FittedModels", "DefVoAStanFit.rds"),
  #   compress = "gz"
  # )

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
      "beta_recruit_pts",
      "beta_VoA_Output",
      "beta_Conference_Strength",
      "sigma"
    ),
    format = "draws_df"
  )

  ### writing out posterior samples as parquet file so I don't have to refit the model again
  write_parquet(
    Def_VoA_pars,
    here("Data", "FittedModels", "DefVoAParams.parquet")
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
  #         Def_VoA_pars$beta_def_epa[p] * VoAVariables$weighted_def_epa[t] +
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
  #         Def_VoA_pars$beta_Conf_Rk[p] *
  #           VoAVariables$Conf_Rk[t],
  #       sd = Def_VoA_pars$sigma[p]
  #     )
  #     Def_VoA_Ratings[p, t] <- Def_VoA_Rating
  #   }
  # }

  ### Create the Design Matrix (Teams x Predictors)
  DefDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_def_epa = VoAVariables$weighted_def_epa,
    beta_def_ypp = VoAVariables$weighted_def_ypp,
    beta_def_success_rate = VoAVariables$weighted_def_success_rate,
    beta_def_explosiveness = VoAVariables$weighted_def_explosiveness,
    beta_def_third_conv_rate = VoAVariables$weighted_def_third_conv_rate,
    beta_def_pts_per_opp = VoAVariables$weighted_def_pts_per_opp,
    beta_def_havoc_total = VoAVariables$weighted_def_havoc_total,
    beta_def_plays_pg = VoAVariables$weighted_def_plays_pg,
    beta_recruit_pts = VoAVariables$weighted_recruit_pts,
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

  ### def voa ratings has a bunch of negative values in it which is not possible for what the value represents (pts conceded against hypothetical avg team on a neutral field)
  ## a better fix is probably to tinker with the priors some more but what if I don't want to because I'm tired of dealing with stan being weird and annoying
  ## on reflection the priors probably don't change much, I'd need to fix the opponent-adjustment process more than anything else so low numbers aren't so frequent
  ## but I'm honestly not sure how to do that
  Def_VoA_Ratings <- ifelse(
    Def_VoA_Ratings <= 0,
    abs(rnorm(1, 0, sd(Def_VoA_Ratings))) / 5,
    Def_VoA_Ratings
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
    net_kick_return_yds = VoATrain$net_kick_return_yds,
    net_punt_return_yds = VoATrain$net_punt_return_yds,
    net_fg_rate = VoATrain$net_fg_rate,
    net_st_epa = VoATrain$net_adj_st_epa
    # net_st_epa = VoATrain$st_net_epa
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

  ### saving model object as rds file
  # write_rds(
  #   ST_VoA_fit,
  #   file = here("Data", "FittedModels", "STVoAStanFit.rds"),
  #   compress = "gz"
  # )

  ### extracting parameters
  ST_VoA_pars <- ST_VoA_fit$draws(
    variables = c(
      "b0",
      "beta_net_kick_return_yds",
      "beta_net_punt_return_yds",
      "beta_net_fg_rate",
      "beta_net_st_epa",
      "sigma"
    ),
    format = "draws_df"
  )

  ### writing out posterior samples as parquet file so I don't have to refit the model again
  write_parquet(
    ST_VoA_pars,
    here("Data", "FittedModels", "STVoAParams.parquet")
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
  #         ST_VoA_pars$beta_net_st_epa[p] *
  #           VoAVariables$weighted_net_adj_st_epa[t],
  #       sd = ST_VoA_pars$sigma[p]
  #     )
  #     ST_VoA_Ratings[p, t] <- ST_VoA_Rating
  #   }
  # }

  ### Create the Design Matrix (Teams x Predictors)
  STDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_net_kick_return_yds = VoAVariables$weighted_net_kick_return_yds,
    beta_net_punt_return_yds = VoAVariables$weighted_net_punt_return_yds,
    beta_net_fg_rate = VoAVariables$weighted_net_fg_rate,
    beta_net_st_epa = VoAVariables$weighted_net_adj_st_epa
  ))

  ### Parameter Matrix (Posterior samples x Predictors)
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
} else if (as.integer(cfb_week) <= 9) {
  ##### Weeks 1-9 Stan Models, current season data only #####
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
  #   VoA_Output = VoAVariables$VoA_Output,
  #   Conference_Strength = VoAVariables$Conf_Rk
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

  ### loading offensive Stan model
  # Off_VoA_fit <- read_rds(here("Data", "FittedModels", "OffVoAStanFit.rds"))

  ### Print the diagnostics
  # print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  # Off_VoA_pars <- Off_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_off_epa",
  #     "beta_off_ypp",
  #     "beta_off_success_rate",
  #     "beta_off_explosiveness",
  #     "beta_third_conv_rate",
  #     "beta_off_pts_per_opp",
  #     "beta_off_plays_pg",
  #     "beta_recruit_pts",
  #     "beta_VoA_Output",
  #     "beta_Conference_Strength",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  Off_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "OffVoAParams.parquet"
  ))

  ### creating matrix to hold ratings
  # Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  ### Create the Design Matrix (Teams x Predictors)
  OffDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_off_epa = VoAVariables$weighted_off_epa,
    beta_off_ypp = VoAVariables$weighted_off_ypp,
    beta_off_success_rate = VoAVariables$weighted_off_success_rate,
    beta_off_explosiveness = VoAVariables$weighted_off_explosiveness,
    beta_third_conv_rate = VoAVariables$weighted_off_third_conv_rate,
    beta_off_pts_per_opp = VoAVariables$weighted_off_pts_per_opp,
    beta_off_plays_pg = VoAVariables$weighted_off_plays_pg,
    beta_recruit_pts = VoAVariables$weighted_recruit_pts,
    beta_VoA_Output = VoAVariables$VoA_Output,
    beta_Conference_Strength = VoAVariables$Conf_Rk
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
  #   def_epa = VoAVariables$adj_def_epa,
  #   def_ypp = VoAVariables$adj_def_ypp,
  #   def_success_rate = VoAVariables$def_success_rate,
  #   def_explosiveness = VoAVariables$adj_def_explosiveness,
  #   def_third_conv_rate = VoAVariables$def_third_conv_rate,
  #   def_pts_per_opp = VoAVariables$def_pts_per_opp,
  #   def_havoc_total = VoAVariables$def_havoc_total,
  #   def_plays_pg = VoAVariables$def_plays_pg,
  #   VoA_Output = VoAVariables$VoA_Output,
  #   Conference_Strength = VoAVariables$Conf_Rk
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

  ### loading defensive Stan model
  # Def_VoA_fit <- read_rds(here(
  #   "Data",
  #   "FittedModels",
  #   "DefVoAStanFit.rds"
  # ))

  ### Print the diagnostics
  # print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  # Def_VoA_pars <- Def_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_def_epa",
  #     "beta_def_ypp",
  #     "beta_def_success_rate",
  #     "beta_def_explosiveness",
  #     "beta_def_third_conv_rate",
  #     "beta_def_pts_per_opp",
  #     "beta_def_havoc_total",
  #     "beta_def_plays_pg",
  #     "beta_recruit_pts",
  #     "beta_VoA_Output",
  #     "beta_Conference_Strength",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  Def_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "DefVoAParams.parquet"
  ))

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  # Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  ### Create the Design Matrix (Teams x Predictors)
  DefDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_def_epa = VoAVariables$weighted_def_epa,
    beta_def_ypp = VoAVariables$weighted_def_ypp,
    beta_def_success_rate = VoAVariables$weighted_def_success_rate,
    beta_def_explosiveness = VoAVariables$weighted_def_explosiveness,
    beta_def_third_conv_rate = VoAVariables$weighted_def_third_conv_rate,
    beta_def_pts_per_opp = VoAVariables$weighted_def_pts_per_opp,
    beta_def_havoc_total = VoAVariables$weighted_def_havoc_total,
    beta_def_plays_pg = VoAVariables$weighted_def_plays_pg,
    beta_recruit_pts = VoAVariables$weighted_recruit_pts,
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
  # ST_VoA_datalist <- list(
  #   N = nrow(VoAVariables),
  #   net_st_ppg = VoAVariables$net_st_ppg,
  #   net_kick_return_avg = VoAVariables$net_kick_return_avg,
  #   net_punt_return_avg = VoAVariables$net_punt_return_avg,
  #   net_fg_rate = VoAVariables$net_fg_rate,
  #   net_st_epa = VoAVariables$net_adj_st_epa
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

  ### loading special teams Stan model
  # ST_VoA_fit <- read_rds(here("Data", "FittedModels", "STVoAStanFit.rds"))

  ### Print the diagnostics
  # print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  # ST_VoA_pars <- ST_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_net_kick_return_avg",
  #     "beta_net_punt_return_avg",
  #     "beta_net_fg_rate",
  #     "beta_net_st_epa",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  ST_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "STVoAParams.parquet"
  ))

  ### creating special teams VoA_Ratings
  ### Create the Design Matrix (Teams x Predictors)
  STDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_net_kick_return_yds = VoAVariables$weighted_net_kick_return_yds,
    beta_net_punt_return_yds = VoAVariables$weighted_net_punt_return_yds,
    beta_net_fg_rate = VoAVariables$weighted_net_fg_rate,
    beta_net_st_epa = VoAVariables$weighted_net_adj_st_epa
  ))

  ### Parameter Matrix (Posterior samples x Predictors)
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
  ##### Weeks 10-End of Season Stan Models, current season data only #####
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
  #   VoA_Output = VoAVariables$VoA_Output,
  #   Conference_Strength = VoAVariables$Conf_Rk
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

  ### loading offensive Stan model posterior samples
  # Off_VoA_fit <- read_rds(here("Data", "FittedModels", "OffVoAStanFit.rds"))

  ### Print the diagnostics
  # print(Off_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  # Off_VoA_pars <- Off_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_off_epa",
  #     "beta_off_ypp",
  #     "beta_off_success_rate",
  #     "beta_off_explosiveness",
  #     "beta_third_conv_rate",
  #     "beta_off_pts_per_opp",
  #     "beta_off_plays_pg",
  #     "beta_recruit_pts",
  #     "beta_VoA_Output",
  #     "beta_Conference_Strength",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  Off_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "OffVoAParams.parquet"
  ))

  ### creating matrix to hold ratings
  # Off_VoA_Ratings <- matrix(NA, length(Off_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  ### Create the Design Matrix (Teams x Predictors)
  OffDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_off_epa = VoAVariables$adj_off_epa,
    beta_off_ypp = VoAVariables$adj_off_ypp,
    beta_off_success_rate = VoAVariables$off_success_rate,
    beta_off_explosiveness = VoAVariables$adj_off_explosiveness,
    beta_third_conv_rate = VoAVariables$off_third_conv_rate,
    beta_off_pts_per_opp = VoAVariables$off_pts_per_opp,
    beta_off_plays_pg = VoAVariables$adj_off_plays_pg,
    beta_recruit_pts = VoAVariables$recruit_pts_PY1,
    beta_VoA_Output = VoAVariables$VoA_Output,
    beta_Conference_Strength = VoAVariables$Conf_Rk
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
  #   def_epa = VoAVariables$adj_def_epa,
  #   def_ypp = VoAVariables$adj_def_ypp,
  #   def_success_rate = VoAVariables$def_success_rate,
  #   def_explosiveness = VoAVariables$adj_def_explosiveness,
  #   def_third_conv_rate = VoAVariables$def_third_conv_rate,
  #   def_pts_per_opp = VoAVariables$def_pts_per_opp,
  #   def_havoc_total = VoAVariables$def_havoc_total,
  #   def_plays_pg = VoAVariables$def_plays_pg,
  #   VoA_Output = VoAVariables$VoA_Output,
  #   Conf_Rk = VoAVariables$Conf_Rk
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

  ### loading defensive Stan model
  # Def_VoA_fit <- read_rds(here("Data", "FittedModels", "DefVoAStanFit.rds"))

  ### Print the diagnostics
  # print(Def_VoA_fit$cmdstan_diagnose())

  ### Extracting Parameters
  # Def_VoA_pars <- Def_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_def_epa",
  #     "beta_def_ypp",
  #     "beta_def_success_rate",
  #     "beta_def_explosiveness",
  #     "beta_def_third_conv_rate",
  #     "beta_def_pts_per_opp",
  #     "beta_def_havoc_total",
  #     "beta_def_plays_pg",
  #     "beta_recruit_pts",
  #     "beta_VoA_Output",
  #     "beta_Conference_Strength",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  Def_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "DefVoAParams.parquet"
  ))

  ### creating matrix to hold ratings
  ### adding in process uncertainty
  # Def_VoA_Ratings <- matrix(NA, length(Def_VoA_pars$b0), nrow(VoAVariables))

  ### creating ratings
  ### Create the Design Matrix (Teams x Predictors)
  DefDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_def_epa = VoAVariables$adj_def_epa,
    beta_def_ypp = VoAVariables$adj_def_ypp,
    beta_def_success_rate = VoAVariables$def_success_rate,
    beta_def_explosiveness = VoAVariables$adj_def_explosiveness,
    beta_def_third_conv_rate = VoAVariables$def_third_conv_rate,
    beta_def_pts_per_opp = VoAVariables$def_pts_per_opp,
    beta_def_havoc_total = VoAVariables$def_havoc_total,
    beta_def_plays_pg = VoAVariables$adj_def_plays_pg,
    beta_recruit_pts = VoAVariables$recruit_pts_PY1,
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
  ### loading special teams Stan model
  # ST_VoA_fit <- read_rds(here("Data", "FittedModels", "STVoAStanFit.rds"))

  ### Print the diagnostics
  # print(ST_VoA_fit$cmdstan_diagnose())

  ### extracting parameters
  # ST_VoA_pars <- ST_VoA_fit$draws(
  #   variables = c(
  #     "b0",
  #     "beta_net_kick_return_avg",
  #     "beta_net_punt_return_avg",
  #     "beta_net_fg_rate",
  #     "beta_net_st_epa",
  #     "sigma"
  #   ),
  #   format = "draws_df"
  # )
  ST_VoA_pars <- read_parquet(here(
    "Data",
    "FittedModels",
    "STVoAParams.parquet"
  ))

  ### creating special teams VoA_Ratings
  ### Create the Design Matrix (Teams x Predictors)
  STDesignMatrix <- as.matrix(cbind(
    b0 = 1,
    beta_net_kick_return_yds = VoAVariables$net_kick_return_yds,
    beta_net_punt_return_yds = VoAVariables$net_punt_return_yds,
    beta_net_fg_rate = VoAVariables$net_fg_rate,
    beta_net_st_epa = VoAVariables$net_adj_st_epa
  ))

  ### Parameter Matrix (Posterior samples x Predictors)
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

### creating data frame with just team, VoA ratings, VoA Rankings, and VoA output
FinalTable <- VoAVariables |>
  select(
    school,
    classification,
    conference,
    CFB_Week,
    VoA_Output,
    VoA_Rating_Ovr,
    VoA_Ranking_Ovr,
    OffVoA_MeanRating,
    OffVoA_Ranking,
    DefVoA_MeanRating,
    DefVoA_Ranking,
    STVoA_MeanRating,
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
    columns = c(OffVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(DefVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(STVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # Another column (also numeric data)
    columns = c(VoA_Ranking_Ovr), # What column variable? FinalVoATop25$V oA_Ranking
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
    columns = c(OffVoA_MeanRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(DefVoA_MeanRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(STVoA_MeanRating), # ...for dose column
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
    OffVoA_MeanRating = "Off VoA Rating",
    OffVoA_Ranking = "Off Ranking",
    DefVoA_MeanRating = "Def VoA Rating",
    DefVoA_Ranking = "Def Ranking",
    STVoA_MeanRating = "ST VoA Rating",
    STVoA_Ranking = "ST Ranking"
  ) |> # Update labels
  # cols_move_to_end(columns = "VoA_Rating") |>
  cols_hide(c(
    classification,
    conference,
    CFB_Week,
    VoA_Output,
    Conf_Rk
  )) |>
  tab_footnote(
    footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
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
    columns = c(OffVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(DefVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |>
  fmt_number(
    # A column (numeric data)
    columns = c(STVoA_MeanRating), # What column variable? FinalVoATop25$VoA_Rating
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
    columns = c(OffVoA_MeanRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(DefVoA_MeanRating), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(STVoA_MeanRating), # ...for dose column
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
    OffVoA_MeanRating = "Off VoA Rating",
    OffVoA_Ranking = "Off Ranking",
    DefVoA_MeanRating = "Def VoA Rating",
    DefVoA_Ranking = "Def Ranking",
    STVoA_MeanRating = "ST VoA Rating",
    STVoA_Ranking = "ST Ranking"
  ) |> # Update labels
  # cols_move_to_end(columns = "VoA_Rating_Ovr") |>
  cols_hide(c(
    classification,
    conference,
    CFB_Week,
    VoA_Output,
    Conf_Rk
  )) |>
  tab_footnote(
    footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
  )

##### Resume VoA #####
### determining mean VoA Rating of top 12 teams in VoA, comparing how this hypothetical average top 12 team would do given each team's schedule
## choosing top 12 because of future playoff expansion which seems likely if not already certain
## it really should only be 8 max but whatever, I'm gonna be just fine
### Resume VoA only created after week 9 (Week 10 - end of season)
if (as.integer(cfb_week) > 11) {
  ### adding column to VoAVariables to hold Resume VoA metric
  ## placing dummy value for now, will be filled in for loop further down
  VoAVariables <- VoAVariables |>
    mutate(Resume_VoA = -999)

  ### calculating top 12 average since 12 teams make the playoff
  Top12 <- VoAVariables |>
    filter(VoA_Ranking_Ovr <= 12) |>
    select(
      season,
      school,
      OffVoA_MeanRating,
      DefVoA_MeanRating,
      STVoA_MeanRating
    )
  Top12_off_mean <- mean(Top12$OffVoA_MeanRating)
  Top12_def_mean <- mean(Top12$DefVoA_MeanRating)
  Top12_st_mean <- mean(Top12$STVoA_MeanRating)
  Top12_mean <- Top12_off_mean - Top12_def_mean + Top12_st_mean

  ### pulling in completed games
  completed_games <- cfbd_game_info(as.integer(year)) |>
    filter(
      home_team %in% VoAVariables$school | away_team %in% VoAVariables$school
    ) |>
    select(
      game_id,
      season,
      week,
      neutral_site,
      completed,
      home_team,
      home_points,
      away_team,
      away_points
    ) |>
    filter(completed == TRUE)

  ### using SRS ratings for FCS teams instead of the randomly sampled VoA rating based on
  ## bottom half of VoA ratings as done during 2022 CFB season
  FCS <- cfbd_ratings_srs(year = as.integer(year)) |>
    filter(team %nin% VoAVariables$school) |>
    filter(
      team %in% completed_games$home_team | team %in% completed_games$away_team
    )

  ##### Calculating Resume VoA team by team #####
  for (x in 1:nrow(VoAVariables)) {
    temp_team <- completed_games |>
      filter(
        home_team == VoAVariables$school[x] |
          away_team == VoAVariables$school[x]
      ) |>
      mutate(
        team = VoAVariables$school[x],
        team_opp = case_when(
          home_team == VoAVariables$school[x] ~ away_team,
          TRUE ~ home_team
        ),
        team_VoA_rating = VoAVariables$VoA_Rating_Ovr[x]
      )
    ### extracting ratings of FBS opponents
    temp_teamFBSOpps <- VoAVariables |>
      filter(school %in% temp_team$team_opp) |>
      select(school, VoA_Rating_Ovr)
    ### extracting SRS ratings of FCS opponents
    temp_teamFCSOpps <- FCS |>
      filter(team %in% temp_team$team_opp) |>
      select(team, rating)
    colnames(temp_teamFCSOpps) <- c("school", "VoA_Rating_Ovr")
    temp_teamOpps <- rbind(temp_teamFBSOpps, temp_teamFCSOpps)
    colnames(temp_teamOpps) <- c("team_opp", "opp_VoA_rating")

    ### adding opponent ratings to main team df
    temp_team <- full_join(temp_team, temp_teamOpps, by = "team_opp")

    ### calculating resume score
    temp_team <- temp_team |>
      mutate(
        actual_diff = case_when(
          home_team == VoAVariables$school[x] ~ home_points - away_points,
          TRUE ~ away_points - home_points
        ),
        projected_diff = case_when(
          home_team == VoAVariables$school[x] &
            neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
          away_team == VoAVariables$school[x] &
            neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
          TRUE ~ team_VoA_rating - opp_VoA_rating
        ),
        Top12_proj = case_when(
          home_team == VoAVariables$school[x] &
            neutral_site == FALSE ~ (Top12_mean + 2) - opp_VoA_rating,
          TRUE ~ Top12_mean - opp_VoA_rating
        ),
        Resume_Score = actual_diff - Top12_proj
      )

    ## determining number of losses
    temp_team_losses <- temp_team |>
      filter(
        home_team == VoAVariables$school[x] &
          home_points < away_points |
          away_team == VoAVariables$school[x] & away_points < home_points
      )
    ## storing overall team Resume Score as vector
    VoAVariables$Resume_VoA[x] <- sum(temp_team$Resume_Score) -
      (7 * nrow(temp_team_losses))
  }

  VoAVariables <- VoAVariables |>
    mutate(Resume_VoA_Rank = dense_rank(desc(Resume_VoA)))

  ### filtering resume top 25 out for table
  ResumeVoATop25 <- VoAVariables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    filter(Resume_VoA_Rank < 26) |>
    arrange(Resume_VoA_Rank)

  ### full resume VoA, simplified for table
  FinalResumeTable <- VoAVariables |>
    select(team, Resume_VoA, Resume_VoA_Rank) |>
    arrange(Resume_VoA_Rank)
} else {
  print("no Resume VoA until Week 12!")
}

##### Creating Top 25 and Full Tables Arranged by Resume VoA #####
if (as.integer(cfb_week) > 11) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, cfb_week, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
    ) |> # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      Resume_VoA = "Resume VoA Rating",
      Resume_VoA_Rank = "Resume VoA Ranking"
    ) |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
    )

  ### Full 134 teams table
  # adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, cfb_week, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
    ) |> # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      Resume_VoA = "Resume VoA Rating",
      Resume_VoA_Rank = "Resume VoA Rank"
    ) |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
    )
} else if (as.integer(cfb_week) == 20) {
  ## Top 25 Table
  # adding title and subtitle
  ResumeVoATop25Table <- ResumeVoATop25 |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_Top25_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
    ) |> # and this subtitle
    ## tab_style(style = cell_fill("bisque"),
    ##           locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      Resume_VoA = "Resume VoA Rating",
      Resume_VoA_Rank = "Resume VoA Ranking"
    ) |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
    )

  ### Full 134 teams table
  ### adding title and subtitle
  Resume_VoA_Table <- FinalResumeTable |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, Postseason_text, resume_text, VoA_text), # ...with this title
      subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
    ) |> # and this subtitle
    ##tab_style(style = cell_fill("bisque"),
    ##        locations = cells_body()) |>  # add fill color to table
    fmt_number(
      # A column (numeric data)
      columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With four decimal places
    ) |>
    fmt_number(
      # Another column (also numeric data)
      columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 0 # I want this column to have zero decimal places
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(Resume_VoA), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      Resume_VoA = "Resume VoA Rating",
      Resume_VoA_Rank = "Resume VoA Rank"
    ) |> # Update labels
    cols_move_to_end(columns = "Resume_VoA") |>
    # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
    tab_footnote(
      footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
    )
} else {
  print("No Resume VoA until Week 12!")
}

# poopypants <- lm(
#   adj_off_ppg ~ adj_off_epa +
#     adj_off_ypp +
#     off_success_rate +
#     adj_off_explosiveness +
#     off_third_conv_rate +
#     off_pts_per_opp +
#     adj_off_plays_pg +
#     recruit_pts +
#     VoA_Output +
#     Conf_Rk,
#   data = VoATrain
# )

# poopypantsVoA <- VoAVariables |>
#   mutate(
#     adj_off_epa = weighted_off_epa,
#     adj_off_ypp = weighted_off_ypp,
#     off_success_rate = weighted_off_success_rate,
#     adj_off_explosiveness = weighted_off_explosiveness,
#     off_third_conv_rate = weighted_off_third_conv_rate,
#     off_pts_per_opp = weighted_off_pts_per_opp,
#     adj_off_plays_pg = weighted_off_plays_pg,
#     recruit_pts = weighted_recruit_pts
#   )
# poopypantsVoA <- poopypantsVoA |>
#   mutate(lm_off_preds = predict(poopypants, poopypantsVoA))

# poopypantsVoA_gt <- poopypantsVoA |>
#   select(school, lm_off_preds) |>
#   arrange(desc(lm_off_preds)) |>
#   gt()
# poopypantsVoA_gt

# poopy = predict(poopypants, poopypantsVoA)

##### Saving tables and final VoAVariables csv #####
### viewing and saving the gt tables outside the if statement so that I can see them in the RStudio viewer
VoATop25Table
VoATop25Table |>
  gtsave(
    top25_file_pathway,
    expand = 5,
    path = output_dir
  )
VoA_Full_Table
VoA_Full_Table |>
  gtsave(
    fulltable_file_pathway,
    expand = 5,
    path = output_dir
  )

## Resume VoA not produced until Week 10
if (as.integer(cfb_week) > 9) {
  ### Resume tables
  ResumeVoATop25Table
  ResumeVoATop25Table |>
    gtsave(
      resumetop25_file_pathway,
      expand = 5,
      path = output_dir
    )
  Resume_VoA_Table
  Resume_VoA_Table |>
    gtsave(
      resumefulltable_file_pathway,
      expand = 5,
      path = output_dir
    )
} else {
  print("No Resume VoA tables until week 12!")
}

## Exporting final dataframe as parquet file
write_parquet(VoAVariables, file_pathway)
### also writing out file represent the "current" VoA ratings so it can be more easily visualized on my website and/or a shiny app maybe
write_csv(VoAVariables, here("Data", "CurrentFBSVoA.csv"))

##### Setting up the Unintelligible Charts #####
### Tracks VoA Ratings and Rankings by week
### now reading in and merging VoA rating and ranking data up to current week
### changing FinalTable to only be columns needed for Unintelligible Charts
FinalTable <- FinalTable |>
  select(
    school,
    conference,
    CFB_Week,
    VoA_Output,
    VoA_Ranking_Ovr,
    VoA_Rating_Ovr
  )
if (as.integer(cfb_week) == 3) {
  Week0_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(year, "Week0_VoA.parquet")
  )) |>
    select(
      team,
      conference,
      CFB_Week,
      VoA_Output,
      VoA_Ranking_Ovr,
      VoA_Rating_Ovr
    )
  Week1_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(year, "Week1_VoA.parquet")
  )) |>
    select(
      team,
      conference,
      CFB_Week,
      VoA_Output,
      VoA_Ranking_Ovr,
      VoA_Rating_Ovr
    )
  Week2_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(year, "Week2_VoA.parquet")
  )) |>
    select(
      team,
      conference,
      CFB_Week,
      VoA_Output,
      VoA_Ranking_Ovr,
      VoA_Rating_Ovr
    )
  Full_Ratings_Rks <- rbind(
    Week0_VoA,
    rbind(Week1_VoA, rbind(Week2_VoA, FinalTable))
  )
  write_parquet(
    Full_Ratings_Rks,
    paste(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      year,
      week_text,
      "0_3Ratings_Rks.parquet",
      sep = ""
    )
  )
} else if (as.integer(cfb_week) > 3) {
  ### reading in previous week's csv of ratings and ranks for charts
  Full_Ratings_Rks <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    "TrackingChartCSVs",
    paste0(
      year,
      week_text,
      "0_",
      as.integer(cfb_week) - 1,
      "Ratings_Rks.parquet"
    )
  )) |>
    select(
      team,
      conference,
      CFB_Week,
      VoA_Output,
      VoA_Ranking_Ovr,
      VoA_Rating_Ovr
    )
  Full_Ratings_Rks <- rbind(Full_Ratings_Rks, FinalTable)
  write_parquet(
    Full_Ratings_Rks,
    paste0(
      data_dir,
      "/TrackingChartCSVs",
      "/",
      year,
      week_text,
      "0_",
      cfb_week,
      "Ratings_Rks.parquet"
    )
  )
} else {
  print("No charts until Week 3!")
}
### end of if statement

### Filtering by conference for unintelligible charts
if (as.integer(cfb_week) >= 3) {
  ### each conference (including independents) gets separate charts
  ### given that the Pac12 is now really the 2Pac, they get lumped in with the Indies
  AAC_Ratings_Rks <- Full_Ratings_Rks |>
    filter(conference == "American Athletic")
  ACC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "ACC")
  Big12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big 12")
  Big10_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Big Ten")
  CUSA_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Conference USA")
  ### lumping the 2Pac with the Indys for unintelligible chart purposes
  Indy_Ratings_Rks <- Full_Ratings_Rks |>
    filter(conference == "FBS Independents" | conference == "Pac-12")
  MAC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mid-American")
  MWC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Mountain West")
  # Pac12_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Pac-12")
  SEC_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "SEC")
  SunBelt_Ratings_Rks <- Full_Ratings_Rks |> filter(conference == "Sun Belt")

  ##### Creating Charts #####
  ### charting VoA_Rating and VoA_Ranking for each week from week 2 on
  AAC_VoA_Rating_Chart <- ggplot(
    AAC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("American Conference Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(AAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  AAC_VoA_Rating_Chart
  ggsave(
    AAC_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  AAC_VoA_Ranking_Chart <- ggplot(
    AAC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("American Conference Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  AAC_VoA_Ranking_Chart
  ggsave(
    AAC_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  ACC_VoA_Rating_Chart <- ggplot(
    ACC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("ACC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(ACC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  ACC_VoA_Rating_Chart
  ggsave(
    ACC_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  ACC_VoA_Ranking_Chart <- ggplot(
    ACC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("ACC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  ACC_VoA_Ranking_Chart
  ggsave(
    ACC_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Big12_VoA_Rating_Chart <- ggplot(
    Big12_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Big 12 Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(Big12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Big12_VoA_Rating_Chart
  ggsave(
    Big12_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Big12_VoA_Ranking_Chart <- ggplot(
    Big12_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Big 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Big12_VoA_Ranking_Chart
  ggsave(
    Big12_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Big10_VoA_Rating_Chart <- ggplot(
    Big10_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Big 10 Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(Big10_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Big10_VoA_Rating_Chart
  ggsave(
    Big10_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Big10_VoA_Ranking_Chart <- ggplot(
    Big10_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Big 10 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Big10_VoA_Ranking_Chart
  ggsave(
    Big10_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  CUSA_VoA_Rating_Chart <- ggplot(
    CUSA_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("CUSA Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(CUSA_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  CUSA_VoA_Rating_Chart
  ggsave(
    CUSA_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  CUSA_VoA_Ranking_Chart <- ggplot(
    CUSA_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("CUSA Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  CUSA_VoA_Ranking_Chart
  ggsave(
    CUSA_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Indy_VoA_Rating_Chart <- ggplot(
    Indy_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Independents Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(Indy_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Indy_VoA_Rating_Chart
  ggsave(
    Indy_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Indy_VoA_Ranking_Chart <- ggplot(
    Indy_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Independents Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Indy_VoA_Ranking_Chart
  ggsave(
    Indy_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  MAC_VoA_Rating_Chart <- ggplot(
    MAC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("MAC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(MAC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  MAC_VoA_Rating_Chart
  ggsave(
    MAC_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  MAC_VoA_Ranking_Chart <- ggplot(
    MAC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("MAC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  MAC_VoA_Ranking_Chart
  ggsave(
    MAC_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  MWC_VoA_Rating_Chart <- ggplot(
    MWC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Mountain West Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(MWC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  MWC_VoA_Rating_Chart
  ggsave(
    MWC_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  MWC_VoA_Ranking_Chart <- ggplot(
    MWC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Mountain West Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  MWC_VoA_Ranking_Chart
  ggsave(
    MWC_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Pac12_VoA_Rating_Chart <- ggplot(
    Pac12_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Pac 12 Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(Pac12_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Pac12_VoA_Rating_Chart
  ggsave(
    Pac12_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  Pac12_VoA_Ranking_Chart <- ggplot(
    Pac12_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Pac 12 Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  Pac12_VoA_Ranking_Chart
  ggsave(
    Pac12_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  SEC_VoA_Rating_Chart <- ggplot(
    SEC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("SEC Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(SEC_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  SEC_VoA_Rating_Chart
  ggsave(
    SEC_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  SEC_VoA_Ranking_Chart <- ggplot(
    SEC_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("SEC Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  SEC_VoA_Ranking_Chart
  ggsave(
    SEC_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  ### geom_cfb_logos used to just throw the NCAA logo in place of the App State logo since for some reason cfb_plotR hasn't been updated in a while and doesn't keep up with the cfbfastR/cfbdata team names
  ## this stopped working so I've had to make this frankly easy fix that I should've just always done so that App shows up as App instead of the NCAA
  SunBelt_Ratings_Rks <- SunBelt_Ratings_Rks |>
    mutate(
      team = case_when(team == "App State" ~ "Appalachian State", TRUE ~ team)
    )
  SunBelt_VoA_Rating_Chart <- ggplot(
    SunBelt_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Rating_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Overall Rating") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Sun Belt Vortex of Accuracy Overall Ratings by Week") +
    expand_limits(
      y = c(
        floor(floor(min(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10) * 10,
        ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        (floor((floor(min(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        (ceiling((ceiling(max(SunBelt_Ratings_Rks$VoA_Rating_Ovr)) / 10)) * 10),
        by = 5
      )
    ) +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  SunBelt_VoA_Rating_Chart
  ggsave(
    SunBelt_Output_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )

  SunBelt_VoA_Ranking_Chart <- ggplot(
    SunBelt_Ratings_Rks,
    aes(x = CFB_Week, y = VoA_Ranking_Ovr, group = school)
  ) +
    theme_bw() +
    geom_line(linewidth = 1.5) +
    # geom_point(size = 5) +
    xlab("Week") +
    ylab("VoA Ranking") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    ggtitle("Sun Belt Vortex of Accuracy Rankings by Week") +
    expand_limits(y = c(0, 130)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = c(
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20
      )
    ) +
    geom_cfb_logos(aes(team = school, width = 0.035)) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
  SunBelt_VoA_Ranking_Chart
  ggsave(
    SunBelt_Ranking_filename,
    path = output_dir,
    width = 50,
    height = 40,
    units = 'cm'
  )
} else {
  print("No charts until Week 3!")
}

### Creating Histograms of VoA Output for all teams, and separate plots for power 5 and group of 5 teams subsetted out
## plots will be made for each week, not just after week 2 like Unintelligble Charts will
### subsetting teams
Power5_VoA <- VoAVariables |>
  filter(
    conference == "ACC" |
      conference == "Big 12" |
      conference == "Big Ten" |
      conference == "FBS Independents" |
      conference == "SEC"
  ) |>
  filter(school != "Connecticut" & school != "UMass")

Group5_VoA <- VoAVariables |>
  filter(
    conference == "Pac-12" |
      conference == "American Athletic" |
      conference == "Conference USA" |
      conference == "FBS Independents" |
      conference == "Mid-American" |
      conference == "Mountain West" |
      conference == "Sun Belt"
  ) |>
  filter(school != "Notre Dame")

### making histogram of ratings for all FBS teams
FBS_Rating_histogram <- ggplot(VoAVariables, aes(VoA_Rating_Ovr)) +
  theme_bw() +
  geom_histogram(binwidth = 5, col = "black", fill = "orange") +
  scale_x_continuous(breaks = seq(-50, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  ggtitle(FBS_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(
    caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
  ) +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
FBS_Rating_histogram
ggsave(
  FBS_hist_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

### histogram of ratings for Power 5 (power 4?) teams
Power5_Rating_histogram <- ggplot(Power5_VoA, aes(VoA_Rating_Ovr)) +
  theme_bw() +
  geom_histogram(binwidth = 5, col = "black", fill = "blue") +
  scale_x_continuous(breaks = seq(-50, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  ggtitle(Power5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(
    caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
  ) +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
Power5_Rating_histogram
ggsave(
  Power5_hist_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

### histogram of VoA ratings for G5 teams
Group5_Rating_histogram <- ggplot(Group5_VoA, aes(VoA_Rating_Ovr)) +
  theme_bw() +
  geom_histogram(binwidth = 5, col = "black", fill = "pink") +
  scale_x_continuous(breaks = seq(-50, 40, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  ggtitle(Group5_hist_title) +
  xlab("VoA Rating") +
  ylab("Frequency") +
  labs(
    caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
  ) +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
Group5_Rating_histogram
ggsave(
  Group5_hist_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

### repeating App St geom_cfb_logos() plot fix
VoAVariables_plot <- VoAVariables |>
  mutate(
    school = case_when(
      school == "App State" ~ "Appalachian State",
      TRUE ~ school
    )
  )
### Creating Scatterplot of VoA_Output vs VoA_Rating
VoA_Output_Rating_plot <- ggplot(
  VoAVariables_plot,
  aes(x = VoA_Output, y = VoA_Rating_Ovr)
) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_smooth() +
  geom_cfb_logos(aes(team = school), width = 0.035) +
  scale_x_continuous(breaks = seq(0, 135, 10)) +
  scale_y_continuous(breaks = seq(-50, 40, 5)) +
  ggtitle(Output_Rating_Plot_title) +
  xlab("VoA Output") +
  ylab("VoA Overall Rating") +
  labs(
    caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
  ) +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
VoA_Output_Rating_plot
ggsave(
  Output_Rating_Plot_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

## Creating Scatterplot of VoA Offensive Rating vs VoA Defensive Rating
VoA_OffDef_Rating_plot <- ggplot(
  VoAVariables_plot,
  aes(x = OffVoA_MeanRating, y = DefVoA_MeanRating)
) +
  theme_bw() +
  # geom_point(size = 2) +
  # geom_smooth() +
  scale_y_reverse() +
  geom_cfb_logos(aes(team = school), width = 0.035) +
  geom_hline(yintercept = mean(VoAVariables$DefVoA_MeanRating)) +
  geom_vline(xintercept = mean(VoAVariables$OffVoA_MeanRating)) +
  # scale_x_continuous(breaks = seq(0,135,10)) +
  # scale_y_continuous(breaks = seq(-50,40,5)) +
  ggtitle(OffDef_Rating_Plot_title) +
  xlab("VoA Offensive Rating") +
  ylab("VoA Defensive Rating") +
  labs(
    caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
  ) +
  theme(
    plot.title = element_text(size = 35, hjust = 0.5),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    legend.text = element_text(size = 20)
  )
VoA_OffDef_Rating_plot
ggsave(
  OffDef_Rating_Plot_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)

### plot for adjusted off/def EPA
if (as.integer(cfb_week) <= 9) {
  VoA_OffDef_EPA_plot <- ggplot(
    VoAVariables_plot,
    aes(
      x = weighted_off_epa,
      y = weighted_def_epa
    )
  ) +
    theme_bw() +
    # geom_point(size = 2) +
    # geom_smooth() +
    scale_y_reverse() +
    geom_cfb_logos(aes(team = school), width = 0.035) +
    geom_hline(yintercept = mean(VoAVariables$weighted_def_epa)) +
    geom_vline(xintercept = mean(VoAVariables$weighted_off_epa)) +
    # scale_x_continuous(breaks = seq(0,135,10)) +
    # scale_y_continuous(breaks = seq(-50,40,5)) +
    ggtitle(OffDef_EPA_Plot_title) +
    xlab("Offense Opponent-Adjusted EPA") +
    ylab("Defense Opponent-Adjusted EPA") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
} else {
  VoA_OffDef_EPA_plot <- ggplot(
    VoAVariables_plot,
    aes(x = adj_off_epa, y = adj_def_epa)
  ) +
    theme_bw() +
    geom_point(size = 2) +
    # geom_smooth() +
    scale_y_reverse() +
    # geom_cfb_logos(aes(team = school), width = 0.035) +
    geom_hline(yintercept = mean(VoAVariables$adj_def_epa)) +
    geom_vline(xintercept = mean(VoAVariables$adj_off_epa)) +
    # scale_x_continuous(breaks = seq(0,135,10)) +
    # scale_y_continuous(breaks = seq(-50,40,5)) +
    ggtitle(OffDef_EPA_Plot_title) +
    xlab("Offense Opponent-Adjusted EPA") +
    ylab("Defense Opponent-Adjusted EPA") +
    labs(
      caption = "chart by @gshelor, data from collegefootballdata.com API via cfbfastR"
    ) +
    theme(
      plot.title = element_text(size = 35, hjust = 0.5),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
}
VoA_OffDef_EPA_plot
ggsave(
  OffDef_EPA_Plot_filename,
  path = output_dir,
  width = 50,
  height = 40,
  units = 'cm'
)


##### End of Script #####
cfbd_api_key_info()
end_time <- Sys.time()
end_time - start_time
