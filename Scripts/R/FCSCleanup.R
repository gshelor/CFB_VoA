##### This script is for cleaning up FCS data csvs to get data for FCS teams moving up to FBS which will be used in current season VoAs #####
### teams currently needing FCS data: Sam Houston, Jacksonville State, Kennesaw State, Delaware, Missouri State
### Advanced analytics mostly unavailable for FCS teams
## other advanced stats will be G5 means used as extremely rough approximation
### basic stats will all be from the relevant FCS teams
### This script should only need to be run once, to create csvs of previous years data to be used in VoA script before and during season
### loading packages
library(pacman)
pacman::p_load(tidyverse, matrixStats, here, cfbfastR)
### gonna use this function as part of making sure the column names match
`%nin%` = Negate(`%in%`)
### PY# represents "Previous Year" and the number is the number of years previous to the current season
### teams with 0-2 seasons in FBS
PY3Teams <- c("Delaware", "Missouri State", "Kennesaw State", "Sam Houston", "Jacksonville State")
### teams with 0-1 seasons in FBS
PY2Teams <- c("Delaware", "Missouri State", "Kennesaw State")
### teams with no seasons in FBS last 3 years
PY1Teams <- c("Delaware", "Missouri State")

### reading in previous season's VoA to establish which teams to take average of for advanced stats later
old_season <- as.numeric(readline("Which season was last year? "))
PrevYearVoA <- read_csv(here("Data", paste0("VoA", old_season), paste0(old_season, "Week16_VoA.csv"))) |>
  filter(VoA_Rating_Ovr < quantile(VoA_Rating_Ovr, probs = 0.1) | VoA_Output > quantile(VoA_Output, probs = 0.9) | conference %in% c("Mid-American", "Conference USA", "Mountain West") | team %in% c("Massachusetts", "UConn")) |>
  filter(team %nin% c("Boise State", "UNLV")) |>
  arrange(desc(VoA_Rating_Ovr))
### PY2 VoA
PrevYear2VoA <- read_csv(here("Data", paste0("VoA", old_season - 1), paste0(old_season - 1, "Week16_VoA.csv"))) |>
  filter(VoA_Rating < quantile(VoA_Rating, probs = 0.1) | VoA_Output > quantile(VoA_Output, probs = 0.9) | conference %in% c("Mid-American", "Conference USA", "Mountain West") | team %in% c("Massachusetts", "UConn")) |>
  filter(team %nin% c("Boise State", "UNLV")) |>
  arrange(desc(VoA_Rating))
### PY3 VoA
PrevYear3VoA <- read_csv(here("Data", paste0("VoA", old_season - 2), paste0(old_season - 2, "Week16_VoA.csv"))) |>
  filter(VoA_Rating < quantile(VoA_Rating, probs = 0.1) | VoA_Output > quantile(VoA_Output, probs = 0.9) | conference %in% c("Mid-American", "Conference USA", "Mountain West") | team %in% c("Massachusetts", "UConn")) |>
  filter(team %nin% c("Boise State", "UNLV")) |>
  arrange(desc(VoA_Rating))

### storing some random team names so I can reassign them later because I don't know how to just summarize the whole df in the exact way I would want to
## I'd set a seed but this isn't really a repeatability thing, I just need team names
sample_team_names <- sort(sample(PrevYearVoA$team, size = length(PY3Teams), replace = FALSE))

##### reading in PY3 CSVs, formatting DFs #####
### defensive interceptions
Ints_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSDefInts2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Ints_PY3_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY3", "Win_Loss", "Opp_Comps_PY3", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY3) <- Ints_PY3_ColNames
Ints_PY3 <- Ints_PY3 |>
  mutate(conference = case_when(team == 'Sam Houston' ~ 'Conference USA',
                                team == 'Delaware' ~ 'Conference USA',
                                team == 'Jacksonville State' ~ 'Conference USA',
                                team == 'Kennesaw State' ~ 'Conference USA',
                                team == 'Missouri State' ~ 'Conference USA',
                                TRUE ~ 'FCS'), .before = 3)
Ints_PY3[,5:ncol(Ints_PY3)] <- Ints_PY3[,5:ncol(Ints_PY3)] |> mutate_if(is.character, as.numeric)
Ints_PY3 <- Ints_PY3 |>
  mutate(def_interceptions_pg_PY3 = INTs / games_PY3) |>
  select(team, conference, games_PY3, def_interceptions_pg_PY3)

### offensive 4th down stats
Fourth_Down_Off_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownOff2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Fourth_Down_Off_PY3_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY3", "fourth_downs_PY3 ", "fourth_conv_rate_PY3")
colnames(Fourth_Down_Off_PY3) <- Fourth_Down_Off_PY3_colnames
Fourth_Down_Off_PY3 <- Fourth_Down_Off_PY3 |>
  select(team, fourth_conv_rate_PY3)

## kick return stats
Kick_Returns_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturns2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Kick_Returns_PY3_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY3")
colnames(Kick_Returns_PY3) <- Kick_Returns_PY3_colnames
Kick_Returns_PY3 <- Kick_Returns_PY3 |>
  mutate(kick_return_tds_pg = Return_TDs / games) |>
  select(team, kick_return_tds_pg, kick_return_avg_PY3)

### PY3 passing offense stats
PassOff_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSPassOffense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
PassOff_PY3_colnames <- c("rk_del","team", "team_del", "games", "win_loss", "pass_atts_PY3", "pass_comps_PY3", "INTs", "Total_Pass_Yds", "pass_ypa_PY3", "pass_ypr_PY3", "Pass_TD", "pass_yds_pg_PY3")
colnames(PassOff_PY3) <- PassOff_PY3_colnames
PassOff_PY3[,5:ncol(PassOff_PY3)] <- PassOff_PY3[,5:ncol(PassOff_PY3)] |> mutate_if(is.character,as.numeric)
PassOff_PY3 <- PassOff_PY3 |>
  mutate(int_pct_PY3 = INTs / pass_atts_PY3) |>
  mutate(completion_pct_PY3 = pass_comps_PY3 / pass_atts_PY3) |>
  select(team, int_pct_PY3, pass_ypa_PY3, pass_ypr_PY3, pass_yds_pg_PY3, completion_pct_PY3)

### PY3 first down stats
FirstDown_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSFirstDowns2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
FirstDown_PY3_colnames <- c("del", "team", "team_del", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY3) <- FirstDown_PY3_colnames
FirstDown_PY3[,3:ncol(FirstDown_PY3)] <- FirstDown_PY3[,3:ncol(FirstDown_PY3)] |> mutate_if(is.character, as.numeric)
FirstDown_PY3 <- FirstDown_PY3 |>
  mutate(first_downs_pg_PY3 = total_fd / games) |>
  select(team, first_downs_pg_PY3)

### PY3 Penalty stats
Penalties_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSPenalties2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Penalties_PY3_colnames <- c("delete", "team", "team_del", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY3")
colnames(Penalties_PY3) <- Penalties_PY3_colnames
Penalties_PY3[,5:ncol(Penalties_PY3)] <- Penalties_PY3[,5:ncol(Penalties_PY3)] |> mutate_if(is.character, as.numeric)
Penalties_PY3 <- Penalties_PY3 |>
  mutate(yards_per_penalty_PY3 = penalty_yds / penalties) |>
  select(team, penalty_yds_pg_PY3, yards_per_penalty_PY3)

### PY3 Punt Return Stats
PuntReturns_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturns2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
PuntReturns_PY3_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY3")
colnames(PuntReturns_PY3) <- PuntReturns_PY3_colnames
PuntReturns_PY3 <- PuntReturns_PY3 |>
  mutate(punt_return_tds_pg = return_tds / games) |>
  select(team, punt_return_tds_pg, punt_return_avg_PY3)

### PY3 Rush Offense
RushOffense_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSRushOffense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
RushOffense_PY3_colnames <- c("delete", "team", "team_del", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY3", "rush_td", "rush_yds_pg_PY3")
colnames(RushOffense_PY3) <- RushOffense_PY3_colnames
RushOffense_PY3 <- RushOffense_PY3 |>
  select(team, rush_ypc_PY3, rush_yds_pg_PY3)

### PY3 Third down stats
ThirdDown_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownOff2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
ThirdDown_PY3_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY3")
colnames(ThirdDown_PY3) <- ThirdDown_PY3_colnames
ThirdDown_PY3 <- ThirdDown_PY3 |>
  select(team, third_conv_rate_PY3)

### PY3 Total Offense
TotalOffense_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalOffense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
TotalOffense_PY3_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "off_ypp_PY3", "tds", "total_yds_pg_PY3")
colnames(TotalOffense_PY3) <- TotalOffense_PY3_colnames

TotalOffense_PY3[,3:ncol(TotalOffense_PY3)] <- TotalOffense_PY3[,3:ncol(TotalOffense_PY3)] |> mutate_if(is.character, as.numeric)
TotalOffense_PY3 <- TotalOffense_PY3 |>
  mutate(off_plays_pg_PY3 = plays / gms) |>
  select(team, off_plays_pg_PY3, off_ypp_PY3, total_yds_pg_PY3)

### PY3 turnover stats
Turnovers_df_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSTurnoversLost2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Turnovers_PY3_colnames <- c("del", "team", "team_del", "gms", "wl", "fum", "int", "total_turnovers_PY3")
colnames(Turnovers_df_PY3) <- Turnovers_PY3_colnames
Turnovers_df_PY3[,3:ncol(Turnovers_df_PY3)] <- Turnovers_df_PY3[,3:ncol(Turnovers_df_PY3)] |> mutate_if(is.character, as.numeric)
Turnovers_df_PY3 <- Turnovers_df_PY3 |>
  mutate(turnovers_pg_PY3 = total_turnovers_PY3 / gms) |>
  select(team, turnovers_pg_PY3)

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
FCS_Adv_Stats_PY3 <- cfbd_stats_season_advanced(2022, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
  filter(team %in% PY3Teams) |>
  # filter(conference == "Mountain West" | conference == "Mid-American" | conference == "Conference USA" | conference == "Sun Belt" | conference == "American Athletic" | conference == "FBS Independents") |>
  # filter(team != "Notre Dame") |>
  # filter(team %in% PrevYear3VoA$team) |>
  select(-one_of("season", "conference")) |>
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

## making all stats columns numeric
FCS_Adv_Stats_PY3 <- FCS_Adv_Stats_PY3 |> mutate_if(is.integer,as.numeric)

### computing means of each column and assigning them to FCS teams
### I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
# FCS_Adv_Stats_PY3 <- Adv_Stats_PY3 |>
#   head(length(PY3Teams)) |>
#   # filter(team %in% sample_team_names[1:length(PY3Teams)]) #|>
#   arrange(team) |>
#   # mutate(team_keep = case_when(team == 'Akron' ~ 'Sam Houston',
#   #                              team == 'Ball State' ~ 'Delaware',
#   #                              team == 'Rice' ~ 'Jacksonville State',
#   #                              team == 'Toledo' ~ 'Kennesaw State',
#   #                              team == 'Wyoming' ~ 'Missouri State',
#   #                              TRUE ~ team), .before = 1) |>
#   select(team, off_ppa, off_success_rate, off_explosiveness, off_power_success,
#          off_stuff_rate, off_line_yds, off_second_lvl_yds, off_open_field_yds,
#          off_pts_per_opp, off_field_pos_avg_predicted_points, off_havoc_total,
#          off_havoc_front_seven, off_havoc_db, off_standard_downs_ppa,
#          off_standard_downs_success_rate, off_standard_downs_explosiveness,
#          off_passing_downs_ppa, off_passing_downs_success_rate,
#          off_passing_downs_explosiveness, off_rushing_plays_ppa,
#          off_rushing_plays_success_rate, off_rushing_plays_explosiveness,
#          off_passing_plays_ppa, off_passing_plays_success_rate,
#          off_passing_plays_explosiveness, def_ppa, def_success_rate,
#          def_explosiveness, def_power_success, def_stuff_rate, def_line_yds,
#          def_second_lvl_yds, def_open_field_yds, def_pts_per_opp,
#          def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven,
#          def_havoc_db, def_standard_downs_ppa, def_standard_downs_success_rate,
#          def_standard_downs_explosiveness , def_passing_downs_ppa,
#          def_passing_downs_success_rate, def_passing_downs_explosiveness,
#          def_rushing_plays_ppa, def_rushing_plays_success_rate,
#          def_rushing_plays_explosiveness, def_passing_plays_ppa,
#          def_passing_plays_success_rate, def_passing_plays_explosiveness)
# 
# ### reassigning team names
# for (i in 1:nrow(FCS_Adv_Stats_PY3)){
#   FCS_Adv_Stats_PY3$team[i] = PY3Teams[i]
# }
# ### making all stats columns numeric
# FCS_Adv_Stats_PY3 <- FCS_Adv_Stats_PY3 |> mutate_if(is.integer,as.numeric)
# for (rating in 2:ncol(FCS_Adv_Stats_PY3)) {
#   FCS_Adv_Stats_PY3[,rating] = colMeans(Adv_Stats_PY3[,rating])
# }

### renaming Advanced stats columns to reflect them being from PY3
colnames(FCS_Adv_Stats_PY3) <- c("team", "off_ppa_PY3", "off_success_rate_PY3", "off_explosiveness_PY3", "off_power_success_PY3", "off_stuff_rate_PY3", "off_line_yds_PY3", "off_second_lvl_yds_PY3", "off_open_field_yds_PY3", "off_pts_per_opp_PY3", "off_field_pos_avg_predicted_points_PY3", "off_havoc_total_PY3", "off_havoc_front_seven_PY3", "off_havoc_db_PY3", "off_standard_downs_ppa_PY3", "off_standard_downs_success_rate_PY3", "off_standard_downs_explosiveness_PY3", "off_passing_downs_ppa_PY3", "off_passing_downs_success_rate_PY3", "off_passing_downs_explosiveness_PY3", "off_rushing_plays_ppa_PY3", "off_rushing_plays_success_rate_PY3", "off_rushing_plays_explosiveness_PY3", "off_passing_plays_ppa_PY3", "off_passing_plays_success_rate_PY3", "off_passing_plays_explosiveness_PY3", "def_ppa_PY3", "def_success_rate_PY3", "def_explosiveness_PY3", "def_power_success_PY3", "def_stuff_rate_PY3", "def_line_yds_PY3", "def_second_lvl_yds_PY3", "def_open_field_yds_PY3", "def_pts_per_opp_PY3", "def_field_pos_avg_predicted_points_PY3", "def_havoc_total_PY3", "def_havoc_front_seven_PY3", "def_havoc_db_PY3", "def_standard_downs_ppa_PY3", "def_standard_downs_success_rate_PY3", "def_standard_downs_explosiveness_PY3", "def_passing_downs_ppa_PY3", "def_passing_downs_success_rate_PY3", "def_passing_downs_explosiveness_PY3", "def_rushing_plays_ppa_PY3", "def_rushing_plays_success_rate_PY3", "def_rushing_plays_explosiveness_PY3", "def_passing_plays_ppa_PY3", "def_passing_plays_success_rate_PY3", "def_passing_plays_explosiveness_PY3")

## pulling in recruiting rankings
FCS_recruit_PY3 <- cfbd_recruiting_team(year = 2022) |>
  select(team, points) |>
  filter(team %in% PY3Teams)

### if some teams aren't in the recruiting info, this is hopefully a fix for that
if (nrow(FCS_recruit_PY3) != length(PY3Teams)){
  Recruiting_PY3 <- cfbd_recruiting_team(year = (old_season - 2)) |>
    select(team, points) #|>
    # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Recruiting_PY3 |>
    head(length(PY3Teams) - nrow(FCS_recruit_PY3))
  MissingTeams$team <- PY3Teams[which(PY3Teams %nin% FCS_recruit_PY3$team)]
  MissingTeams$points <- quantile(Recruiting_PY3$points, probs = 0.01)
  FCS_recruit_PY3 <- rbind(FCS_recruit_PY3, MissingTeams)
}

FCS_recruit_PY3[,2] <- FCS_recruit_PY3[,2] |> mutate_if(is.character, as.numeric)
colnames(FCS_recruit_PY3) <- c("team", "recruit_pts_PY3")

## pulling in talent rankings
FCS_talent_df_PY3 <- cfbd_team_talent(year = 2022) |>
  filter(team %in% PY3Teams) |> # | team == "Jacksonville") |>
  # mutate(team = case_when(team == 'Jacksonville' ~ 'Jacksonville State',
  #                         TRUE ~ team)) |>
  select(team, talent)

### if some teams aren't in the talent info, this is hopefully a somewhat reasonable fix for that
if (nrow(FCS_talent_df_PY3) != length(PY3Teams)){
  Talent_PY3 <- cfbd_team_talent(year = (old_season - 2)) |>
    select(team, talent) #|>
  # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Talent_PY3 |>
    head(length(PY3Teams) - nrow(FCS_talent_df_PY3))
  MissingTeams$team <- PY3Teams[which(PY3Teams %nin% FCS_talent_df_PY3$team)]
  MissingTeams$talent <- quantile(Talent_PY3$talent, probs = 0.1)
  FCS_talent_df_PY3 <- rbind(FCS_talent_df_PY3, MissingTeams)
}
colnames(FCS_talent_df_PY3) <- c("team", "talent_PY3")

##### PY3 PBP stuff #####
### using PBP to calculate mean G5 fg rate and average tds allowed by opposition
PBP_PY3 <- load_cfb_pbp(seasons = 2022)

PBP_PY3_Yards <- PBP_PY3 |>
  filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP_PY3_FGPlays <- PBP_PY3 |>
  filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

PBP_PY3_G5FGs <- PBP_PY3_FGPlays |>
  filter(offense_conference == "Conference USA" | offense_conference == "American Athletic" | offense_conference == "Mountain West" | offense_conference == "Sun Belt" | offense_conference == "Mid-American") |>
  filter(pos_team != "Notre Dame")

PBP_PY3_GoodG5FGs <- PBP_PY3_G5FGs |>
  filter(play_type == "Field Goal Good")

PBP_PY3_G5FGAllowed <- PBP_PY3_FGPlays |>
  filter(defense_conference == "Conference USA" | defense_conference == "American Athletic" | defense_conference == "Mountain West" | defense_conference == "Sun Belt" | defense_conference == "Mid-American") |>
  filter(def_pos_team != "Notre Dame")

PBP_PY3_DefGoodG5FGs <- PBP_PY3_G5FGAllowed |>
  filter(play_type == "Field Goal Good")

PBP_PY3_TDs <- PBP_PY3 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

### going to use this to offset the off_ppg and def_ppg
### so that touchdowns allowed are touchdowns allowed by their opposition
### this temp pbp is how average opposition TDs allowed will be calculated
PBP_PY3_OppDefPPA <- PBP_PY3_Yards |>
  filter(def_pos_team %in% PrevYear3VoA$team)

PBP_PY3_OppOffPPA <- PBP_PY3_Yards |>
  filter(pos_team %in% PrevYear3VoA$team)

PBP_PY3_OppDefTDs <- PBP_PY3_TDs |>
  filter(def_pos_team %in% PrevYear3VoA$team)

PBP_PY3_OppOffTDs <- PBP_PY3_TDs |>
  filter(pos_team %in% PrevYear3VoA$team)


### reading in Offensive scoring csv
OffScoring_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringOffense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)

OffScoring_PY3_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY3", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(OffScoring_PY3) <- OffScoring_PY3_ColNames

OffScoring_PY3[,3:ncol(OffScoring_PY3)] <- OffScoring_PY3[,3:ncol(OffScoring_PY3)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
OffScoring_PY3 <- OffScoring_PY3 |>
  mutate(off_ppg_PY3 = ((TDs * 6) + (TwoPts * 2)) / games_PY3,
         fg_rate_PY3 = nrow(PBP_PY3_GoodG5FGs) / nrow(PBP_PY3_G5FGs),
         fg_rate_allowed_PY3 = nrow(PBP_PY3_DefGoodG5FGs) / nrow(PBP_PY3_G5FGAllowed),
         fg_made_pg_PY3 = FG_del / games_PY3,
         xpts_pg_PY3 = XPts / games_PY3) |>
  select(team, off_ppg_PY3, fg_rate_PY3, fg_rate_allowed_PY3, fg_made_pg_PY3, xpts_pg_PY3)

### Defensive scoring stats
DefScoring_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringDefense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)

DefScoring_PY3_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY3", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(DefScoring_PY3) <- DefScoring_PY3_ColNames

DefScoring_PY3[,3:ncol(DefScoring_PY3)] <- DefScoring_PY3[,3:ncol(DefScoring_PY3)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
DefScoring_PY3 <- DefScoring_PY3 |>
  mutate(def_ppg_PY3 = ((TDs * 6) + (TwoPts * 2)) / games_PY3,
         fg_made_pg_allowed_PY3 = FG_del / games_PY3,
         xpts_allowed_pg_PY3 = XPts / games_PY3) |>
  select(team, def_ppg_PY3, fg_made_pg_allowed_PY3, xpts_allowed_pg_PY3)


### Total Defense stats
TotalDefense_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalDefense2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
TotalDefense_PY3_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "def_ypp_PY3", "tds", "opp_tds", "def_yds_pg_PY3")
colnames(TotalDefense_PY3) <- TotalDefense_PY3_colnames

TotalDefense_PY3[,3:ncol(TotalDefense_PY3)] <- TotalDefense_PY3[,3:ncol(TotalDefense_PY3)] |> mutate_if(is.character, as.numeric)
TotalDefense_PY3 <- TotalDefense_PY3 |>
  mutate(def_plays_pg_PY3 = plays / gms) |>
  select(team, def_plays_pg_PY3, def_ypp_PY3, def_yds_pg_PY3)

### defensive fourth down
Fourth_Down_Def_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownDef2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Fourth_Down_Def_PY3_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY3", "fourth_downs_PY3 ", "def_fourth_conv_rate_PY3")
colnames(Fourth_Down_Def_PY3) <- Fourth_Down_Def_PY3_colnames
Fourth_Down_Def_PY3 <- Fourth_Down_Def_PY3 |>
  select(team, def_fourth_conv_rate_PY3)

### Defensive Third Down Stats
ThirdDownDef_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownDef2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
ThirdDownDef_PY3_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "def_third_conv_rate_PY3")
# ThirdDownDef_PY3[,3:ncol(ThirdDownDef_PY3)] <- ThirdDownDef_PY3[,3:ncol(ThirdDownDef_PY3)] |> mutate_if(is.character, as.numeric)
colnames(ThirdDownDef_PY3) <- ThirdDownDef_PY3_colnames
ThirdDownDef_PY3 <- ThirdDownDef_PY3 |>
  select(team, def_third_conv_rate_PY3)


### Kick Return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
Kick_ReturnsDef_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturnDef2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
Kick_ReturnsDef_PY3_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Touchbacks", "Return_Yds", "def_return_TDs", "kick_return_yds_avg_allowed_PY3")
colnames(Kick_ReturnsDef_PY3) <- Kick_ReturnsDef_PY3_colnames
Kick_ReturnsDef_PY3[,3:ncol(Kick_ReturnsDef_PY3)] <- Kick_ReturnsDef_PY3[,3:ncol(Kick_ReturnsDef_PY3)] |> mutate_if(is.character, as.numeric)
Kick_ReturnsDef_PY3 <- Kick_ReturnsDef_PY3 |>
  mutate(def_return_TDs_pg = def_return_TDs / games) |>
  select(team, def_return_TDs_pg, kick_return_yds_avg_allowed_PY3)

### Punt return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
PuntReturnsDef_PY3 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturnDef2022.csv")) |>
  mutate(team = case_when(Team == 'Sam Houston (WAC)' ~ 'Sam Houston',
                          Team == 'Jacksonville St. (ASUN)' ~ 'Jacksonville State',
                          Team == 'Kennesaw St. (ASUN)' ~ 'Kennesaw State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY3Teams)
PuntReturnsDef_PY3_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds_allowed", "punt_return_yds_avg_allowed_PY3")
colnames(PuntReturnsDef_PY3) <- PuntReturnsDef_PY3_colnames
PuntReturnsDef_PY3 <- PuntReturnsDef_PY3 |>
  mutate(punt_return_tds_pg_allowed = return_tds_allowed / games) |>
  select(team, punt_return_tds_pg_allowed, punt_return_yds_avg_allowed_PY3)




### making a list of all the data frames
df_PY3_list <- list(Ints_PY3, Fourth_Down_Off_PY3, FirstDown_PY3, Kick_Returns_PY3, PassOff_PY3, Penalties_PY3, PuntReturns_PY3, RushOffense_PY3, ThirdDown_PY3, TotalOffense_PY3,Turnovers_df_PY3, FCS_Adv_Stats_PY3, FCS_recruit_PY3, FCS_talent_df_PY3, OffScoring_PY3, DefScoring_PY3, TotalDefense_PY3, ThirdDownDef_PY3, Fourth_Down_Def_PY3, Kick_ReturnsDef_PY3, PuntReturnsDef_PY3)

##### merging all of the PY3 data frames #####
FCS_PY3 <- df_PY3_list |>
  reduce(full_join, by = "team") |>
  mutate(season = 2024, .before = 1) |>
  mutate(st_ppg_PY3 = xpts_pg_PY3 + (fg_made_pg_PY3 * 3) + (punt_return_tds_pg * 6) + (kick_return_tds_pg * 6),
         st_ppg_allowed_PY3 = xpts_allowed_pg_PY3 + (fg_made_pg_allowed_PY3 * 3) + (def_return_TDs_pg * 6) + (punt_return_tds_pg_allowed * 6),
         oppdef_ppa_PY3 = mean(PBP_PY3_OppDefPPA$ppa, na.rm = TRUE),
         oppoff_ppa_PY3 = mean(PBP_PY3_OppOffPPA$ppa, na.rm = TRUE)) |>
  select(season, team, conference, games_PY3, completion_pct_PY3, pass_ypa_PY3, pass_ypr_PY3, int_pct_PY3, rush_ypc_PY3, turnovers_pg_PY3, third_conv_rate_PY3, fourth_conv_rate_PY3, penalty_yds_pg_PY3, yards_per_penalty_PY3, kick_return_avg_PY3, punt_return_avg_PY3, total_yds_pg_PY3, pass_yds_pg_PY3, rush_yds_pg_PY3, first_downs_pg_PY3, off_ypp_PY3, def_interceptions_pg_PY3, off_plays_pg_PY3, off_ppg_PY3, def_ppg_PY3, def_yds_pg_PY3, def_plays_pg_PY3, def_third_conv_rate_PY3, def_fourth_conv_rate_PY3, def_ypp_PY3, fg_rate_PY3, fg_rate_allowed_PY3, fg_made_pg_PY3, fg_made_pg_allowed_PY3, xpts_pg_PY3, xpts_allowed_pg_PY3, kick_return_yds_avg_allowed_PY3, punt_return_yds_avg_allowed_PY3, st_ppg_PY3, st_ppg_allowed_PY3, oppdef_ppa_PY3, oppoff_ppa_PY3, off_ppa_PY3, off_success_rate_PY3, off_explosiveness_PY3,  off_power_success_PY3, off_stuff_rate_PY3, off_line_yds_PY3, off_second_lvl_yds_PY3, off_open_field_yds_PY3, off_pts_per_opp_PY3, off_field_pos_avg_predicted_points_PY3, off_havoc_total_PY3,off_havoc_front_seven_PY3, off_havoc_db_PY3, off_standard_downs_ppa_PY3, off_standard_downs_success_rate_PY3, off_standard_downs_explosiveness_PY3, off_passing_downs_ppa_PY3, off_passing_downs_success_rate_PY3, off_passing_downs_explosiveness_PY3, off_rushing_plays_ppa_PY3, off_rushing_plays_success_rate_PY3, off_rushing_plays_explosiveness_PY3, off_passing_plays_ppa_PY3,  off_passing_plays_success_rate_PY3, off_passing_plays_explosiveness_PY3, def_ppa_PY3, def_success_rate_PY3, def_explosiveness_PY3, def_power_success_PY3, def_stuff_rate_PY3, def_line_yds_PY3, def_second_lvl_yds_PY3, def_open_field_yds_PY3, def_pts_per_opp_PY3, def_field_pos_avg_predicted_points_PY3, def_havoc_total_PY3, def_havoc_front_seven_PY3, def_havoc_db_PY3, def_standard_downs_ppa_PY3, def_standard_downs_success_rate_PY3, def_standard_downs_explosiveness_PY3, def_passing_downs_ppa_PY3, def_passing_downs_success_rate_PY3, def_passing_downs_explosiveness_PY3, def_rushing_plays_ppa_PY3, def_rushing_plays_success_rate_PY3, def_rushing_plays_explosiveness_PY3, def_passing_plays_ppa_PY3, def_passing_plays_success_rate_PY3, def_passing_plays_explosiveness_PY3, recruit_pts_PY3, talent_PY3)



##### CHECKING FOR MISSING COLUMNS IN FCS_PY3 compared to columns being used in VoA #####
# PY3_colnames <- colnames(PY3_df)
# missing_FCSPY3colnames <- c()
# for (i in 1:length(PY3_colnames)){
#   if (PY3_colnames[i] %nin% colnames(FCS_PY3)){
#     missing_FCSPY3colnames <- c(missing_FCSPY3colnames, PY3_colnames[i])
#   }
# }
# missing_FCSPY3colnames
# PY3_colnames <- colnames(FCS_PY3)
# missing_FCSPY3colnames <- c()
# for (i in 1:length(PY3_colnames)){
#   if (PY3_colnames[i] %nin% colnames(PY3_df)){
#     missing_FCSPY3colnames <- c(missing_FCSPY3colnames, PY3_colnames[i])
#   }
# }
# missing_FCSPY3colnames

##### reading in PY2/2023 CSVs #####
## defensive interceptions
Ints_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSDefInts2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Ints_PY2_ColNames <- c("Rk_Delete", "team", "team_del", "games_PY2", "Win_Loss", "Opp_Comps_PY2", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY2) <- Ints_PY2_ColNames

Ints_PY2[,4:ncol(Ints_PY2)] <- Ints_PY2[,4:ncol(Ints_PY2)] |> mutate_if(is.character, as.numeric)
Ints_PY2 <- Ints_PY2 |>
  mutate(def_interceptions_pg_PY2 = INTs / games_PY2) |>
  select(team, games_PY2, def_interceptions_pg_PY2)

### offensive 4th down stats
Fourth_Down_Off_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownOff2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Fourth_Down_Off_PY2_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY2", "fourth_downs_PY2 ", "fourth_conv_rate_PY2")
colnames(Fourth_Down_Off_PY2) <- Fourth_Down_Off_PY2_colnames
Fourth_Down_Off_PY2 <- Fourth_Down_Off_PY2 |>
  select(team, fourth_conv_rate_PY2)


### PY2 Kick Return Stats
Kick_Returns_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturns2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Kick_Returns_PY2_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY2")
colnames(Kick_Returns_PY2) <- Kick_Returns_PY2_colnames
Kick_Returns_PY2 <- Kick_Returns_PY2 |>
  mutate(kick_return_tds_pg = Return_TDs / games) |>
  select(team, kick_return_tds_pg, kick_return_avg_PY2)

### PY2 Pass Offense Stats
PassOff_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSPassOffense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
PassOff_PY2_colnames <- c("rk_del", "team", "team_del", "games", "win_loss", "pass_atts_PY2", "pass_comps_PY2", "INTs", "Total_Pass_Yds", "pass_ypa_PY2", "pass_ypr_PY2", "Pass_TD", "pass_yds_pg_PY2")
colnames(PassOff_PY2) <- PassOff_PY2_colnames
PassOff_PY2[,4:ncol(PassOff_PY2)] <- PassOff_PY2[,4:ncol(PassOff_PY2)] |> mutate_if(is.character,as.numeric)
PassOff_PY2 <- PassOff_PY2 |>
  mutate(int_pct_PY2 = INTs / pass_atts_PY2) |>
  mutate(completion_pct_PY2 = pass_comps_PY2 / pass_atts_PY2) |>
  select(team, int_pct_PY2, pass_ypa_PY2, pass_ypr_PY2, pass_yds_pg_PY2, completion_pct_PY2)

### PY2 first down stats
FirstDown_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSFirstDowns2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
FirstDown_PY2_colnames <- c("del", "team", "team_del", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY2) <- FirstDown_PY2_colnames
FirstDown_PY2[,3:ncol(FirstDown_PY2)] <- FirstDown_PY2[,3:ncol(FirstDown_PY2)] |> mutate_if(is.character, as.numeric)
FirstDown_PY2 <- FirstDown_PY2 |>
  mutate(first_downs_pg_PY2 = total_fd / games) |>
  select(team, first_downs_pg_PY2)

### PY2 Penalty stats
Penalties_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSPenalties2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Penalties_PY2_colnames <- c("delete", "team", "team_del", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY2")
colnames(Penalties_PY2) <- Penalties_PY2_colnames
Penalties_PY2[,4:ncol(Penalties_PY2)] <- Penalties_PY2[,4:ncol(Penalties_PY2)] |> mutate_if(is.character, as.numeric)
Penalties_PY2 <- Penalties_PY2 |>
  mutate(yards_per_penalty_PY2 = penalty_yds / penalties) |>
  select(team, penalty_yds_pg_PY2, yards_per_penalty_PY2)

### PY2 Punt Return stats
PuntReturns_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturns2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
PuntReturns_PY2_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY2")
colnames(PuntReturns_PY2) <- PuntReturns_PY2_colnames
PuntReturns_PY2 <- PuntReturns_PY2 |>
  mutate(punt_return_tds_pg = return_tds / games) |>
  select(team, punt_return_tds_pg, punt_return_avg_PY2)

### PY2 Rush offense stats
RushOffense_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSRushOffense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
RushOffense_PY2_colnames <- c("delete", "team", "team_del", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY2", "rush_td", "rush_yds_pg_PY2")
colnames(RushOffense_PY2) <- RushOffense_PY2_colnames
RushOffense_PY2 <- RushOffense_PY2 |>
  select(team, rush_ypc_PY2, rush_yds_pg_PY2)

### PY2 Offensive third down stats
ThirdDown_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownOff2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
ThirdDown_PY2_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY2")
colnames(ThirdDown_PY2) <- ThirdDown_PY2_colnames
ThirdDown_PY2 <- ThirdDown_PY2 |>
  select(team, third_conv_rate_PY2)

### PY2 Total Offense stats
TotalOffense_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalOffense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
TotalOffense_PY2_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "off_ypp_PY2", "tds", "total_yds_pg_PY2")
colnames(TotalOffense_PY2) <- TotalOffense_PY2_colnames
### converting important columns to numeric type
TotalOffense_PY2[,3:ncol(TotalOffense_PY2)] <- TotalOffense_PY2[,3:ncol(TotalOffense_PY2)] |> mutate_if(is.character, as.numeric)
TotalOffense_PY2 <- TotalOffense_PY2 |>
  mutate(off_plays_pg_PY2 = plays / gms) |>
  select(team, off_plays_pg_PY2, off_ypp_PY2, total_yds_pg_PY2)

### PY2 turnovers stats
Turnovers_df_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSTurnoversLost2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Turnovers_PY2_colnames <- c("del", "team", "team_del", "gms", "wl", "fum", "int", "total_turnovers_PY2")
colnames(Turnovers_df_PY2) <- Turnovers_PY2_colnames
Turnovers_df_PY2[,3:ncol(Turnovers_df_PY2)] <- Turnovers_df_PY2[,3:ncol(Turnovers_df_PY2)] |> mutate_if(is.character, as.numeric)
Turnovers_df_PY2 <- Turnovers_df_PY2 |>
  mutate(turnovers_pg_PY2 = total_turnovers_PY2 / gms) |>
  select(team, turnovers_pg_PY2)

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
Adv_Stats_PY2 <- cfbd_stats_season_advanced(2023, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
  filter(team %in% PrevYear2VoA$team) |>
  # filter(team != "Notre Dame") |>
  # filter(team != "") |>
  # filter(team != "Sam Houston") |>
  # filter(team != "Kennesaw State") |>
  select(-one_of("season", "conference")) |>
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

## making all stats columns numeric
Adv_Stats_PY2 <- Adv_Stats_PY2 |> mutate_if(is.integer,as.numeric)


FCS_Adv_Stats_PY2 <- Adv_Stats_PY2 |>
  head(length(PY2Teams)) |>
  # filter(team %in% sample_team_names[1:length(PY2Teams)]) |>
  # mutate(team_keep = case_when(team == 'Akron' ~ 'Missouri State',
  #                              team == 'Rice' ~ 'Delaware',
  #                              team == 'Toledo' ~ 'Kennesaw State',
  #                              TRUE ~ team), .before = 1) |>
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

### changing team names to match former FCS teams
for (i in 1:nrow(FCS_Adv_Stats_PY2)){
  FCS_Adv_Stats_PY2$team[i] = PY2Teams[i]
}

### making all stats columns numeric
FCS_Adv_Stats_PY2 <- FCS_Adv_Stats_PY2 |> mutate_if(is.integer, as.numeric)

### computing means of each column and assigning them to FCS teams
### I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
for (rating in 2:ncol(FCS_Adv_Stats_PY2)) {
  FCS_Adv_Stats_PY2[1,rating] = colMeans(Adv_Stats_PY2[,rating])
}

## renaming Advanced stats columns to reflect them being from PY2
colnames(FCS_Adv_Stats_PY2) <- c("team", "off_ppa_PY2", "off_success_rate_PY2", "off_explosiveness_PY2", "off_power_success_PY2", "off_stuff_rate_PY2", "off_line_yds_PY2", "off_second_lvl_yds_PY2", "off_open_field_yds_PY2", "off_pts_per_opp_PY2", "off_field_pos_avg_predicted_points_PY2", "off_havoc_total_PY2", "off_havoc_front_seven_PY2", "off_havoc_db_PY2", "off_standard_downs_ppa_PY2", "off_standard_downs_success_rate_PY2", "off_standard_downs_explosiveness_PY2", "off_passing_downs_ppa_PY2", "off_passing_downs_success_rate_PY2", "off_passing_downs_explosiveness_PY2", "off_rushing_plays_ppa_PY2", "off_rushing_plays_success_rate_PY2", "off_rushing_plays_explosiveness_PY2", "off_passing_plays_ppa_PY2", "off_passing_plays_success_rate_PY2", "off_passing_plays_explosiveness_PY2", "def_ppa_PY2", "def_success_rate_PY2", "def_explosiveness_PY2", "def_power_success_PY2", "def_stuff_rate_PY2", "def_line_yds_PY2", "def_second_lvl_yds_PY2", "def_open_field_yds_PY2", "def_pts_per_opp_PY2", "def_field_pos_avg_predicted_points_PY2", "def_havoc_total_PY2", "def_havoc_front_seven_PY2", "def_havoc_db_PY2", "def_standard_downs_ppa_PY2", "def_standard_downs_success_rate_PY2", "def_standard_downs_explosiveness_PY2", "def_passing_downs_ppa_PY2", "def_passing_downs_success_rate_PY2", "def_passing_downs_explosiveness_PY2", "def_rushing_plays_ppa_PY2", "def_rushing_plays_success_rate_PY2", "def_rushing_plays_explosiveness_PY2", "def_passing_plays_ppa_PY2", "def_passing_plays_success_rate_PY2", "def_passing_plays_explosiveness_PY2")

## pulling in recruiting rankings
FCS_recruit_PY2 <- cfbd_recruiting_team(year = 2023) |>
  select(team, points) |>
  filter(team %in% PY2Teams)

### if some teams aren't in the recruiting info, this is hopefully a fix for that
if (nrow(FCS_recruit_PY2) != length(PY2Teams)){
  Recruiting_PY2 <- cfbd_recruiting_team(year = (old_season - 1)) |>
    select(team, points) #|>
  # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Recruiting_PY2 |>
    head(length(PY2Teams) - nrow(FCS_recruit_PY2))
  MissingTeams$team <- PY2Teams[which(PY2Teams %nin% FCS_recruit_PY2$team)]
  MissingTeams$points <- quantile(Recruiting_PY2$points, probs = 0.01)
  FCS_recruit_PY2 <- rbind(FCS_recruit_PY2, MissingTeams)
}
## making recruiting points value numeric
FCS_recruit_PY2[,2] <- FCS_recruit_PY2[,2] |> mutate_if(is.character, as.numeric)
colnames(FCS_recruit_PY2) <- c("team", "recruit_pts_PY2")

### pulling in talent rankings
FCS_talent_df_PY2 <- cfbd_team_talent(year = 2023) |>
  filter(team %in% PY2Teams) |>
  mutate(team = case_when(team == 'Jacksonville' ~ 'Jacksonville State',
                          TRUE ~ team)) |>
  select(team, talent)

### if some teams aren't in the talent info, this is hopefully a somewhat reasonable fix for that
if (nrow(FCS_talent_df_PY2) != length(PY2Teams)){
  Talent_PY2 <- cfbd_team_talent(year = (old_season - 1)) |>
    select(team, talent) #|>
  # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Talent_PY2 |>
    head(length(PY2Teams) - nrow(FCS_talent_df_PY2))
  MissingTeams$team <- PY2Teams[which(PY2Teams %nin% FCS_talent_df_PY2$team)]
  MissingTeams$talent <- quantile(Talent_PY2$talent, probs = 0.1)
  FCS_talent_df_PY2 <- rbind(FCS_talent_df_PY2, MissingTeams)
}

colnames(FCS_talent_df_PY2) <- c("team", "talent_PY2")

##### PY2 PBP stuff #####
### using PBP to calculate mean G5 rate for various stats
PBP_PY2 <- load_cfb_pbp(seasons = 2023)

PBP_PY2_Yards <- PBP_PY2 |>
  filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP_PY2_FGPlays <- PBP_PY2 |>
  filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

PBP_PY2_G5FGs <- PBP_PY2_FGPlays |>
  filter(offense_conference == "Conference USA" | offense_conference == "American Athletic" | offense_conference == "Mountain West" | offense_conference == "Sun Belt" | offense_conference == "Mid-American") |>
  filter(pos_team != "Notre Dame")

PBP_PY2_GoodG5FGs <- PBP_PY2_G5FGs |>
  filter(play_type == "Field Goal Good")

PBP_PY2_G5FGAllowed <- PBP_PY2_FGPlays |>
  filter(defense_conference == "Conference USA" | defense_conference == "American Athletic" | defense_conference == "Mountain West" | defense_conference == "Sun Belt" | defense_conference == "Mid-American") |>
  filter(def_pos_team != "Notre Dame")

PBP_PY2_DefGoodG5FGs <- PBP_PY2_G5FGAllowed |>
  filter(play_type == "Field Goal Good")

PBP_PY2_TDs <- PBP_PY2 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

### going to use this to offset the off_ppg
### so that touchdowns allowed are touchdowns allowed by their opposition
### this temp pbp is how average opposition TDs allowed will be calculated
PBP_PY2_OppDefPPA <- PBP_PY2_Yards |>
  filter(def_pos_team %in% PrevYear2VoA$team)

PBP_PY2_OppOffPPA <- PBP_PY2_Yards |>
  filter(pos_team %in% PrevYear2VoA$team)

PBP_PY2_OppDefTDs <- PBP_PY2_TDs |>
  filter(def_pos_team %in% PrevYear2VoA$team)

PBP_PY2_OppOffTDs <- PBP_PY2_TDs |>
  filter(pos_team %in% PrevYear2VoA$team)


### reading in Offensive scoring csv
OffScoring_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringOffense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)

OffScoring_PY2_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY2", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(OffScoring_PY2) <- OffScoring_PY2_ColNames

OffScoring_PY2[,3:ncol(OffScoring_PY2)] <- OffScoring_PY2[,3:ncol(OffScoring_PY2)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
OffScoring_PY2 <- OffScoring_PY2 |>
  mutate(off_ppg_PY2 = ((TDs * 6) + (TwoPts * 2)) / games_PY2,
         fg_rate_PY2 = nrow(PBP_PY2_GoodG5FGs) / nrow(PBP_PY2_G5FGs),
         fg_rate_allowed_PY2 = nrow(PBP_PY2_DefGoodG5FGs) / nrow(PBP_PY2_G5FGAllowed),
         fg_made_pg_PY2 = FG_del / games_PY2,
         xpts_pg_PY2 = XPts / games_PY2) |>
  select(team, off_ppg_PY2, fg_rate_PY2, fg_rate_allowed_PY2, fg_made_pg_PY2, xpts_pg_PY2)

### Defensive scoring stats
DefScoring_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringDefense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)

DefScoring_PY2_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY2", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(DefScoring_PY2) <- DefScoring_PY2_ColNames

DefScoring_PY2[,3:ncol(DefScoring_PY2)] <- DefScoring_PY2[,3:ncol(DefScoring_PY2)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
DefScoring_PY2 <- DefScoring_PY2 |>
  mutate(def_ppg_PY2 = ((TDs * 6) + (TwoPts * 2)) / games_PY2,
         fg_made_pg_allowed_PY2 = FG_del / games_PY2,
         xpts_allowed_pg_PY2 = XPts / games_PY2) |>
  select(team, def_ppg_PY2, fg_made_pg_allowed_PY2, xpts_allowed_pg_PY2)


### Total Defense stats
TotalDefense_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalDefense2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
TotalDefense_PY2_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "def_ypp_PY2", "tds", "opp_tds", "def_yds_pg_PY2")
colnames(TotalDefense_PY2) <- TotalDefense_PY2_colnames

TotalDefense_PY2[,3:ncol(TotalDefense_PY2)] <- TotalDefense_PY2[,3:ncol(TotalDefense_PY2)] |> mutate_if(is.character, as.numeric)
TotalDefense_PY2 <- TotalDefense_PY2 |>
  mutate(def_plays_pg_PY2 = plays / gms) |>
  select(team, def_plays_pg_PY2, def_ypp_PY2, def_yds_pg_PY2)

### defensive fourth down
Fourth_Down_Def_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownDef2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Fourth_Down_Def_PY2_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY2", "fourth_downs_PY2 ", "def_fourth_conv_rate_PY2")
colnames(Fourth_Down_Def_PY2) <- Fourth_Down_Def_PY2_colnames
Fourth_Down_Def_PY2 <- Fourth_Down_Def_PY2 |>
  select(team, def_fourth_conv_rate_PY2)

### Defensive Third Down Stats
ThirdDownDef_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownDef2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
ThirdDownDef_PY2_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "def_third_conv_rate_PY2")
# ThirdDownDef_PY2[,3:ncol(ThirdDownDef_PY2)] <- ThirdDownDef_PY2[,3:ncol(ThirdDownDef_PY2)] |> mutate_if(is.character, as.numeric)
colnames(ThirdDownDef_PY2) <- ThirdDownDef_PY2_colnames
ThirdDownDef_PY2 <- ThirdDownDef_PY2 |>
  select(team, def_third_conv_rate_PY2)


### Kick Return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
Kick_ReturnsDef_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturnDef2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
Kick_ReturnsDef_PY2_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Touchbacks", "Return_Yds", "def_return_TDs", "kick_return_yds_avg_allowed_PY2")
colnames(Kick_ReturnsDef_PY2) <- Kick_ReturnsDef_PY2_colnames
Kick_ReturnsDef_PY2[,3:ncol(Kick_ReturnsDef_PY2)] <- Kick_ReturnsDef_PY2[,3:ncol(Kick_ReturnsDef_PY2)] |> mutate_if(is.character, as.numeric)
Kick_ReturnsDef_PY2 <- Kick_ReturnsDef_PY2 |>
  mutate(def_return_TDs_pg = def_return_TDs / games) |>
  select(team, def_return_TDs_pg, kick_return_yds_avg_allowed_PY2)

### Punt return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
PuntReturnsDef_PY2 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturnDef2023.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          Team == 'Kennesaw St. (FCS Independent)' ~ 'Kennesaw State',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY2Teams)
PuntReturnsDef_PY2_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds_allowed", "punt_return_yds_avg_allowed_PY2")
colnames(PuntReturnsDef_PY2) <- PuntReturnsDef_PY2_colnames
PuntReturnsDef_PY2 <- PuntReturnsDef_PY2 |>
  mutate(punt_return_tds_pg_allowed = return_tds_allowed / games) |>
  select(team, punt_return_tds_pg_allowed, punt_return_yds_avg_allowed_PY2)



##### merging all the PY2 data frames #####
df_PY2_list <- list(Ints_PY2, Fourth_Down_Off_PY2, FirstDown_PY2, Kick_Returns_PY2, PassOff_PY2, Penalties_PY2, PuntReturns_PY2, RushOffense_PY2, ThirdDown_PY2, TotalOffense_PY2, Turnovers_df_PY2, FCS_Adv_Stats_PY2, FCS_recruit_PY2, FCS_talent_df_PY2, OffScoring_PY2, DefScoring_PY2, TotalDefense_PY2, ThirdDownDef_PY2, Fourth_Down_Def_PY2, Kick_ReturnsDef_PY2, PuntReturnsDef_PY2)
FCS_PY2 <- df_PY2_list |>
  reduce(full_join, by = "team") |>
  mutate(st_ppg_PY2 = xpts_pg_PY2 + (fg_made_pg_PY2 * 3) + (punt_return_tds_pg * 6) + (kick_return_tds_pg * 6),
         st_ppg_allowed_PY2 = xpts_allowed_pg_PY2 + (fg_made_pg_allowed_PY2 * 3) + (def_return_TDs_pg * 6) + (punt_return_tds_pg_allowed * 6),
         oppdef_ppa_PY2 = mean(PBP_PY2_OppDefPPA$ppa, na.rm = TRUE),
         oppoff_ppa_PY2 = mean(PBP_PY2_OppOffPPA$ppa, na.rm = TRUE)) |>
  select(team, games_PY2, completion_pct_PY2, pass_ypa_PY2, pass_ypr_PY2, int_pct_PY2, rush_ypc_PY2, turnovers_pg_PY2, third_conv_rate_PY2, fourth_conv_rate_PY2, penalty_yds_pg_PY2, yards_per_penalty_PY2, kick_return_avg_PY2, punt_return_avg_PY2, total_yds_pg_PY2, pass_yds_pg_PY2, rush_yds_pg_PY2, first_downs_pg_PY2, off_ypp_PY2, def_interceptions_pg_PY2, off_plays_pg_PY2, off_ppg_PY2, def_ppg_PY2, def_yds_pg_PY2, def_plays_pg_PY2, def_third_conv_rate_PY2, def_fourth_conv_rate_PY2, def_ypp_PY2, fg_rate_PY2, fg_rate_allowed_PY2, fg_made_pg_PY2, fg_made_pg_allowed_PY2, xpts_pg_PY2, xpts_allowed_pg_PY2, kick_return_yds_avg_allowed_PY2, punt_return_yds_avg_allowed_PY2, st_ppg_PY2, st_ppg_allowed_PY2, oppdef_ppa_PY2, oppoff_ppa_PY2, off_ppa_PY2, off_success_rate_PY2, off_explosiveness_PY2,  off_power_success_PY2, off_stuff_rate_PY2, off_line_yds_PY2, off_second_lvl_yds_PY2, off_open_field_yds_PY2, off_pts_per_opp_PY2, off_field_pos_avg_predicted_points_PY2, off_havoc_total_PY2,off_havoc_front_seven_PY2, off_havoc_db_PY2, off_standard_downs_ppa_PY2, off_standard_downs_success_rate_PY2, off_standard_downs_explosiveness_PY2, off_passing_downs_ppa_PY2, off_passing_downs_success_rate_PY2, off_passing_downs_explosiveness_PY2, off_rushing_plays_ppa_PY2, off_rushing_plays_success_rate_PY2, off_rushing_plays_explosiveness_PY2, off_passing_plays_ppa_PY2,  off_passing_plays_success_rate_PY2, off_passing_plays_explosiveness_PY2, def_ppa_PY2, def_success_rate_PY2, def_explosiveness_PY2, def_power_success_PY2, def_stuff_rate_PY2, def_line_yds_PY2, def_second_lvl_yds_PY2, def_open_field_yds_PY2, def_pts_per_opp_PY2, def_field_pos_avg_predicted_points_PY2, def_havoc_total_PY2, def_havoc_front_seven_PY2, def_havoc_db_PY2, def_standard_downs_ppa_PY2, def_standard_downs_success_rate_PY2, def_standard_downs_explosiveness_PY2, def_passing_downs_ppa_PY2, def_passing_downs_success_rate_PY2, def_passing_downs_explosiveness_PY2, def_rushing_plays_ppa_PY2, def_rushing_plays_success_rate_PY2, def_rushing_plays_explosiveness_PY2, def_passing_plays_ppa_PY2, def_passing_plays_success_rate_PY2, def_passing_plays_explosiveness_PY2, recruit_pts_PY2, talent_PY2)
  # mutate(season = 2024, .before = 1)

## getting rid of NAs
FCS_PY2[is.na(FCS_PY2)] = 0

##### CHECKING FOR MISSING COLUMNS IN FCS_PY2 compared to columns being used in VoA #####
# missing_FCSPY2colnames <- c()
# for (i in 1:length(PY2_colnames)){
#   if (PY2_colnames[i] %nin% colnames(FCS_PY2)){
#     missing_FCSPY2colnames <- c(missing_FCSPY2colnames, PY2_colnames[i])
#   }
# }
# missing_FCSPY2colnames



##### reading in PY1 CSVs #####
### defensive interceptions
Ints_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSDefInts2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Ints_PY1_ColNames <- c("Rk_Delete", "team", "team_del", "games_PY1", "Win_Loss", "Opp_Comps_PY1", "INTs", "INT_Yds", "INT_TDs")
colnames(Ints_PY1) <- Ints_PY1_ColNames
### making columns numeric
Ints_PY1[,4:ncol(Ints_PY1)] <- Ints_PY1[,4:ncol(Ints_PY1)] |> mutate_if(is.character, as.numeric)
### converting INTs to a per game stat
Ints_PY1 <- Ints_PY1 |>
  mutate(def_interceptions_pg_PY1 = INTs / games_PY1) |>
  select(team, games_PY1, def_interceptions_pg_PY1)

## offensive 4th down stats
Fourth_Down_Off_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownOff2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Fourth_Down_Off_PY1_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY1", "fourth_downs_PY1 ", "fourth_conv_rate_PY1")
colnames(Fourth_Down_Off_PY1) <- Fourth_Down_Off_PY1_colnames
Fourth_Down_Off_PY1 <- Fourth_Down_Off_PY1 |>
  select(team, fourth_conv_rate_PY1)

### PY1 kick returns
Kick_Returns_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturns2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Kick_Returns_PY1_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Return_Yds", "Return_TDs", "kick_return_avg_PY1")
colnames(Kick_Returns_PY1) <- Kick_Returns_PY1_colnames
### making columns numeric
Kick_Returns_PY1[,4:ncol(Kick_Returns_PY1)] <- Kick_Returns_PY1[,4:ncol(Kick_Returns_PY1)] |> mutate_if(is.character, as.numeric)
Kick_Returns_PY1 <- Kick_Returns_PY1 |>
  mutate(kick_return_tds_pg = Return_TDs / games) |>
  select(team, kick_return_tds_pg, kick_return_avg_PY1)

### PY1 passing offense
PassOff_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSPassOffense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
PassOff_PY1_colnames <- c("rk_del", "team", "team_del", "games", "win_loss", "pass_atts_PY1", "pass_comps_PY1", "INTs", "Total_Pass_Yds", "pass_ypa_PY1", "pass_ypr_PY1", "Pass_TD", "pass_yds_pg_PY1")
colnames(PassOff_PY1) <- PassOff_PY1_colnames

PassOff_PY1[,4:ncol(PassOff_PY1)] <- PassOff_PY1[,4:ncol(PassOff_PY1)] |> mutate_if(is.character,as.numeric)
PassOff_PY1 <- PassOff_PY1 |>
  mutate(int_pct_PY1 = INTs / pass_atts_PY1) |>
  mutate(completion_pct_PY1 = pass_comps_PY1 / pass_atts_PY1) |>
  select(team, int_pct_PY1, pass_ypa_PY1, pass_ypr_PY1, pass_yds_pg_PY1, completion_pct_PY1)

### PY1 first down stats
FirstDown_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSFirstDowns2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
FirstDown_PY1_colnames <- c("del", "team", "team_del", "games", "win_loss", "run_fd", "pass_fd", "pen_fd", "total_fd")
colnames(FirstDown_PY1) <- FirstDown_PY1_colnames
FirstDown_PY1[,4:ncol(FirstDown_PY1)] <- FirstDown_PY1[,4:ncol(FirstDown_PY1)] |> mutate_if(is.character, as.numeric)
FirstDown_PY1 <- FirstDown_PY1 |>
  mutate(first_downs_pg_PY1 = total_fd / games) |>
  select(team, first_downs_pg_PY1)

### PY1 penalty stats
Penalties_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSPenalties2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Penalties_PY1_colnames <- c("delete", "team", "team_del", "games", "win_loss", "penalties", "penalty_yds", "penalty_yds_pg_PY1")
colnames(Penalties_PY1) <- Penalties_PY1_colnames
Penalties_PY1[,4:ncol(Penalties_PY1)] <- Penalties_PY1[,4:ncol(Penalties_PY1)] |> mutate_if(is.character, as.numeric)
Penalties_PY1 <- Penalties_PY1 |>
  mutate(yards_per_penalty_PY1 = penalty_yds / penalties) |>
  select(team, penalty_yds_pg_PY1, yards_per_penalty_PY1)

### PY1 Punt return stats
PuntReturns_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturns2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
PuntReturns_PY1_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds", "punt_return_avg_PY1")
colnames(PuntReturns_PY1) <- PuntReturns_PY1_colnames
PuntReturns_PY1[,4:ncol(PuntReturns_PY1)] <- PuntReturns_PY1[,4:ncol(PuntReturns_PY1)] |> mutate_if(is.character, as.numeric)
PuntReturns_PY1 <- PuntReturns_PY1 |>
  mutate(punt_return_tds_pg = return_tds / games) |>
  select(team, punt_return_tds_pg, punt_return_avg_PY1)

### PY1 rush offense
RushOffense_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSRushOffense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
RushOffense_PY1_colnames <- c("delete", "team", "team_del", "games", "win_loss", "rush_atts", "rush_yds", "rush_ypc_PY1", "rush_td", "rush_yds_pg_PY1")
colnames(RushOffense_PY1) <- RushOffense_PY1_colnames
RushOffense_PY1 <- RushOffense_PY1 |>
  select(team, rush_ypc_PY1, rush_yds_pg_PY1)

### PY1 third down offense
ThirdDown_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownOff2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
ThirdDown_PY1_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "third_conv_rate_PY1")
colnames(ThirdDown_PY1) <- ThirdDown_PY1_colnames
ThirdDown_PY1 <- ThirdDown_PY1 |>
  select(team, third_conv_rate_PY1)

### PY1 total offense
TotalOffense_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalOffense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
TotalOffense_PY1_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "off_ypp_PY1", "tds", "total_yds_pg_PY1")
colnames(TotalOffense_PY1) <- TotalOffense_PY1_colnames
TotalOffense_PY1 <- TotalOffense_PY1 |>
  mutate(off_plays_pg_PY1 = plays / gms) |>
  select(team, off_plays_pg_PY1, off_ypp_PY1, total_yds_pg_PY1)

### PY1 offensive turnover stats
Turnovers_df_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSTurnoversLost2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Turnovers_PY1_colnames <- c("del", "team", "team_del", "gms", "wl", "fum", "int", "total_turnovers_PY1")
colnames(Turnovers_df_PY1) <- Turnovers_PY1_colnames
Turnovers_df_PY1[,3:ncol(Turnovers_df_PY1)] <- Turnovers_df_PY1[,3:ncol(Turnovers_df_PY1)] |> mutate_if(is.character, as.numeric)
Turnovers_df_PY1 <- Turnovers_df_PY1 |>
  mutate(turnovers_pg_PY1 = total_turnovers_PY1 / gms) |>
  select(team, turnovers_pg_PY1)

## bringing in advanced stats for G5 and non-Notre Dame Indy teams
Adv_Stats_PY1 <- cfbd_stats_season_advanced(2024, excl_garbage_time = FALSE, start_week = 1, end_week = 15) |>
  filter(team %in% PrevYearVoA$team) |>
  # filter(team != "Notre Dame") |>
  # filter(team != "Kennesaw State") |>
  select(-one_of("season", "conference")) |>
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

## making all stats columns numeric
Adv_Stats_PY1 <- Adv_Stats_PY1 |> mutate_if(is.integer,as.numeric)

## computing means of each column and assigning them to the FCS teams
## I, uh, don't know how to add a row and set all the variable values as the means of the rest of the column
FCS_Adv_Stats_PY1 <- Adv_Stats_PY1 |>
  head(length(PY1Teams)) |>
  # filter(team %in% sample_team_names[1:length(PY1Teams)]) |>
  # mutate(team_keep = case_when(team == 'Toledo' ~ 'Kennesaw State',
  #                              TRUE ~ team), .before = 1) |>
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

### changing team names
for (i in 1:nrow(FCS_Adv_Stats_PY1)){
  FCS_Adv_Stats_PY1$team[i] = PY1Teams[i]
}
## making all stats columns numeric
FCS_Adv_Stats_PY1 <- FCS_Adv_Stats_PY1 |> mutate_if(is.integer,as.numeric)
for (rating in 2:ncol(FCS_Adv_Stats_PY1)) {
  FCS_Adv_Stats_PY1[1,rating] = colMeans(Adv_Stats_PY1[,rating])
}

## renaming Advanced stats columns to reflect them being from PY1
colnames(FCS_Adv_Stats_PY1) <- c("team", "off_ppa_PY1", "off_success_rate_PY1", "off_explosiveness_PY1", "off_power_success_PY1", "off_stuff_rate_PY1", "off_line_yds_PY1", "off_second_lvl_yds_PY1", "off_open_field_yds_PY1", "off_pts_per_opp_PY1", "off_field_pos_avg_predicted_points_PY1", "off_havoc_total_PY1", "off_havoc_front_seven_PY1", "off_havoc_db_PY1", "off_standard_downs_ppa_PY1", "off_standard_downs_success_rate_PY1", "off_standard_downs_explosiveness_PY1", "off_passing_downs_ppa_PY1", "off_passing_downs_success_rate_PY1", "off_passing_downs_explosiveness_PY1", "off_rushing_plays_ppa_PY1", "off_rushing_plays_success_rate_PY1", "off_rushing_plays_explosiveness_PY1", "off_passing_plays_ppa_PY1", "off_passing_plays_success_rate_PY1", "off_passing_plays_explosiveness_PY1", "def_ppa_PY1", "def_success_rate_PY1", "def_explosiveness_PY1", "def_power_success_PY1", "def_stuff_rate_PY1", "def_line_yds_PY1", "def_second_lvl_yds_PY1", "def_open_field_yds_PY1", "def_pts_per_opp_PY1", "def_field_pos_avg_predicted_points_PY1", "def_havoc_total_PY1", "def_havoc_front_seven_PY1", "def_havoc_db_PY1", "def_standard_downs_ppa_PY1", "def_standard_downs_success_rate_PY1", "def_standard_downs_explosiveness_PY1", "def_passing_downs_ppa_PY1", "def_passing_downs_success_rate_PY1", "def_passing_downs_explosiveness_PY1", "def_rushing_plays_ppa_PY1", "def_rushing_plays_success_rate_PY1", "def_rushing_plays_explosiveness_PY1", "def_passing_plays_ppa_PY1", "def_passing_plays_success_rate_PY1", "def_passing_plays_explosiveness_PY1")

## pulling in recruiting rankings
FCS_recruit_PY1 <- cfbd_recruiting_team(year = 2024) |>
  select(team, points) |>
  filter(team %in% PY1Teams)

### if some teams aren't in the recruiting info, this is hopefully a fix for that
if (nrow(FCS_recruit_PY1) != length(PY1Teams)){
  Recruiting_PY1 <- cfbd_recruiting_team(year = old_season) |>
    select(team, points) #|>
  # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Recruiting_PY1 |>
    head(length(PY1Teams) - nrow(FCS_recruit_PY1))
  MissingTeams$team <- PY1Teams[which(PY1Teams %nin% FCS_recruit_PY1$team)]
  MissingTeams$points <- quantile(Recruiting_PY1$points, probs = 0.01)
  FCS_recruit_PY1 <- rbind(FCS_recruit_PY1, MissingTeams)
}

## making recruiting points value numeric
FCS_recruit_PY1[,2] <- FCS_recruit_PY1[,2] |> mutate_if(is.character, as.numeric)
## changing column names
colnames(FCS_recruit_PY1) <- c("team", "recruit_pts_PY1")

## pulling in talent rankings
FCS_talent_df_PY1 <- cfbd_team_talent(year = 2024) |>
  filter(team %in% PY1Teams) |>
  select(team, talent)

### if some teams aren't in the talent info, this is hopefully a somewhat reasonable fix for that
if (nrow(FCS_talent_df_PY1) != length(PY1Teams)){
  Talent_PY1 <- cfbd_team_talent(year = (old_season - 2)) |>
    select(team, talent) #|>
  # filter(team %in% PrevYear3VoA$team)
  MissingTeams <- Talent_PY1 |>
    head(length(PY1Teams) - nrow(FCS_talent_df_PY1))
  MissingTeams$team <- PY1Teams[which(PY1Teams %nin% FCS_talent_df_PY1$team)]
  MissingTeams$talent <- quantile(Talent_PY1$talent, probs = 0.1)
  FCS_talent_df_PY1 <- rbind(FCS_talent_df_PY1, MissingTeams)
}
colnames(FCS_talent_df_PY1) <- c("team", "talent_PY1")

##### PY1 PBP stuff #####
### using PBP to calculate mean G5 fg rate for various stats
PBP_PY1 <- load_cfb_pbp(seasons = 2024)

PBP_PY1_Yards <- PBP_PY1 |>
  filter(play_type == "Pass Incompletion" | play_type == "Pass Completion" | play_type == "Rush" | play_type == "Sack" | play_type == "Fumble Recovery (Own)" | play_type == "Two Point Pass" | play_type == "Two Point Rush" | play_type == "Safety" | play_type == "Pass Reception" | play_type == "Fumble Recovery (Opponent)" | play_type == "Pass" | play_type == "2pt Conversion" | play_type == "Defensive 2pt Conversion" | play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

PBP_PY1_FGPlays <- PBP_PY1 |>
  filter(play_type == "Field Goal Good" | play_type == "Field Goal Missed")

PBP_PY1_G5FGs <- PBP_PY1_FGPlays |>
  # filter(offense_conference == "Conference USA" | offense_conference == "American Athletic" | offense_conference == "Mountain West" | offense_conference == "Sun Belt" | offense_conference == "Mid-American") |>
  filter(pos_team %in% PrevYearVoA$team) #|>
  # filter(pos_team != "Notre Dame")

PBP_PY1_GoodG5FGs <- PBP_PY1_G5FGs |>
  filter(play_type == "Field Goal Good")

PBP_PY1_G5FGAllowed <- PBP_PY1_FGPlays |>
  # filter(defense_conference == "Conference USA" | defense_conference == "American Athletic" | defense_conference == "Mountain West" | defense_conference == "Sun Belt" | defense_conference == "Mid-American") |>
  filter(def_pos_team %in% PrevYearVoA$team)
  # filter(def_pos_team != "Notre Dame")

PBP_PY1_DefGoodG5FGs <- PBP_PY1_G5FGAllowed |>
  filter(play_type == "Field Goal Good")

PBP_PY1_TDs <- PBP_PY1 |>
  filter(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown")

### going to use this to offset the off_ppg and def_ppg
### so that touchdowns allowed are touchdowns allowed by their opposition
### this temp pbp is how average opposition TDs allowed will be calculated
PBP_PY1_OppDefTDs <- PBP_PY1_TDs |>
  # filter(defense_conference == "Conference USA" | defense_conference == "American Athletic" | defense_conference == "Mountain West" | defense_conference == "Sun Belt" | defense_conference == "Mid-American") |>
  filter(def_pos_team %in% PrevYearVoA$team)
  # filter(def_pos_team != "Notre Dame")

PBP_PY1_OppDefPPA <- PBP_PY1_Yards |>
  filter(def_pos_team %in% PrevYearVoA$team)

PBP_PY1_OppOffPPA <- PBP_PY1_Yards |>
  filter(pos_team %in% PrevYearVoA$team)

PBP_PY1_OppOffTDs <- PBP_PY1_TDs |>
  # filter(offense_conference == "Conference USA" | offense_conference == "American Athletic" | offense_conference == "Mountain West" | offense_conference == "Sun Belt" | offense_conference == "Mid-American") |>
  filter(pos_team %in% PrevYearVoA$team)
  # filter(pos_team != "Notre Dame")


### reading in Offensive scoring csv
OffScoring_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringOffense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)

OffScoring_PY1_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY1", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(OffScoring_PY1) <- OffScoring_PY1_ColNames

OffScoring_PY1[,3:ncol(OffScoring_PY1)] <- OffScoring_PY1[,3:ncol(OffScoring_PY1)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
OffScoring_PY1 <- OffScoring_PY1 |>
  mutate(off_ppg_PY1 = ((TDs * 6) + (TwoPts * 2)) / games_PY1,
         fg_rate_PY1 = nrow(PBP_PY1_GoodG5FGs) / nrow(PBP_PY1_G5FGs),
         fg_rate_allowed_PY1 = nrow(PBP_PY1_DefGoodG5FGs) / nrow(PBP_PY1_G5FGAllowed),
         fg_made_pg_PY1 = FG_del / games_PY1,
         xpts_pg_PY1 = XPts / games_PY1) |>
  select(team, off_ppg_PY1, fg_rate_PY1, fg_rate_allowed_PY1, fg_made_pg_PY1, xpts_pg_PY1)

### Defensive scoring stats
DefScoring_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSScoringDefense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)

DefScoring_PY1_ColNames <- c("Rk_Delete", "team", "team_delete", "games_PY1", "Win_Loss", "TDs", "XPts", "TwoPts", "Def_Pts_del", "FG_del", "safety_del", "pts_del", "ppg_del")
colnames(DefScoring_PY1) <- DefScoring_PY1_ColNames

DefScoring_PY1[,3:ncol(DefScoring_PY1)] <- DefScoring_PY1[,3:ncol(DefScoring_PY1)] |> mutate_if(is.character, as.numeric)

### the FG rate stats are just averages generated from the G5 PBP data since the NCAA stats website only provides data on how many FGs were converted
DefScoring_PY1 <- DefScoring_PY1 |>
  mutate(def_ppg_PY1 = ((TDs * 6) + (TwoPts * 2)) / games_PY1,
         fg_made_pg_allowed_PY1 = FG_del / games_PY1,
         xpts_allowed_pg_PY1 = XPts / games_PY1) |>
  select(team, def_ppg_PY1, fg_made_pg_allowed_PY1, xpts_allowed_pg_PY1)


### Total Defense stats
TotalDefense_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSTotalDefense2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
TotalDefense_PY1_colnames <- c("del", "team", "team_del", "gms", "wl", "plays", "totalyds", "def_ypp_PY1", "tds", "opp_tds", "def_yds_pg_PY1")
colnames(TotalDefense_PY1) <- TotalDefense_PY1_colnames

TotalDefense_PY1[,3:ncol(TotalDefense_PY1)] <- TotalDefense_PY1[,3:ncol(TotalDefense_PY1)] |> mutate_if(is.character, as.numeric)
TotalDefense_PY1 <- TotalDefense_PY1 |>
  mutate(def_plays_pg_PY1 = plays / gms) |>
  select(team, def_plays_pg_PY1, def_ypp_PY1, def_yds_pg_PY1)

### defensive fourth down
Fourth_Down_Def_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSFourthDownDef2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Fourth_Down_Def_PY1_colnames <- c("rk_delete", "team", "team_del", "games", "Win_Loss", "fourth_down_convs_PY1", "fourth_downs_PY1 ", "def_fourth_conv_rate_PY1")
colnames(Fourth_Down_Def_PY1) <- Fourth_Down_Def_PY1_colnames
Fourth_Down_Def_PY1 <- Fourth_Down_Def_PY1 |>
  select(team, def_fourth_conv_rate_PY1)

### Defensive Third Down Stats
ThirdDownDef_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSThirdDownDef2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
ThirdDownDef_PY1_colnames <- c("del", "team", "team_del", "gms", "wl", "third_atts", "third_convs", "def_third_conv_rate_PY1")
# ThirdDownDef_PY1[,3:ncol(ThirdDownDef_PY1)] <- ThirdDownDef_PY1[,3:ncol(ThirdDownDef_PY1)] |> mutate_if(is.character, as.numeric)
colnames(ThirdDownDef_PY1) <- ThirdDownDef_PY1_colnames
ThirdDownDef_PY1 <- ThirdDownDef_PY1 |>
  select(team, def_third_conv_rate_PY1)


### Kick Return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
Kick_ReturnsDef_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSKickReturnDef2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
Kick_ReturnsDef_PY1_colnames <- c("rk_delete", "team", "team_del", "games","win_loss", "Returns", "Touchbacks", "Return_Yds", "def_return_TDs", "kick_return_yds_avg_allowed_PY1")
colnames(Kick_ReturnsDef_PY1) <- Kick_ReturnsDef_PY1_colnames
Kick_ReturnsDef_PY1[,3:ncol(Kick_ReturnsDef_PY1)] <- Kick_ReturnsDef_PY1[,3:ncol(Kick_ReturnsDef_PY1)] |> mutate_if(is.character, as.numeric)
Kick_ReturnsDef_PY1 <- Kick_ReturnsDef_PY1 |>
  mutate(def_return_TDs_pg = def_return_TDs / games) |>
  select(team, def_return_TDs_pg, kick_return_yds_avg_allowed_PY1)

### Punt return defense (yards and TDs allowed, TDs allowed will be used to calculate st_ppg_allowed and then removed)
PuntReturnsDef_PY1 <- read_csv(here("Data", "FCSPrevYears", "FCSPuntReturnDef2024.csv")) |>
  mutate(team = case_when(Team == 'Missouri St. (MVFC)' ~ 'Missouri State',
                          Team == 'Delaware (CAA)' ~ 'Delaware',
                          TRUE ~ Team), .before = 2) |>
  filter(team %in% PY1Teams)
PuntReturnsDef_PY1_colnames <- c("delete", "team", "team_del", "games", "win_loss", "punt_returns", "return_yds", "return_tds_allowed", "punt_return_yds_avg_allowed_PY1")
colnames(PuntReturnsDef_PY1) <- PuntReturnsDef_PY1_colnames
PuntReturnsDef_PY1 <- PuntReturnsDef_PY1 |>
  mutate(punt_return_tds_pg_allowed = return_tds_allowed / games) |>
  select(team, punt_return_tds_pg_allowed, punt_return_yds_avg_allowed_PY1)



##### merging all the PY1 data frames #####
df_PY1_list <- list(Ints_PY1, Fourth_Down_Off_PY1, FirstDown_PY1, Kick_Returns_PY1, PassOff_PY1, Penalties_PY1, PuntReturns_PY1, RushOffense_PY1, ThirdDown_PY1, TotalOffense_PY1, Turnovers_df_PY1, FCS_Adv_Stats_PY1, FCS_recruit_PY1, FCS_talent_df_PY1, OffScoring_PY1, DefScoring_PY1, TotalDefense_PY1, ThirdDownDef_PY1, Fourth_Down_Def_PY1, Kick_ReturnsDef_PY1, PuntReturnsDef_PY1)
FCS_PY1 <- df_PY1_list |>
  reduce(full_join, by = "team") |>
  mutate(st_ppg_PY1 = xpts_pg_PY1 + (fg_made_pg_PY1 * 3) + (punt_return_tds_pg * 6) + (kick_return_tds_pg * 6),
         st_ppg_allowed_PY1 = xpts_allowed_pg_PY1 + (fg_made_pg_allowed_PY1 * 3) + (def_return_TDs_pg * 6) + (punt_return_tds_pg_allowed * 6),
         oppdef_ppa_PY1 = mean(PBP_PY1_OppDefPPA$ppa, na.rm = TRUE),
         oppoff_ppa_PY1 = mean(PBP_PY1_OppOffPPA$ppa, na.rm = TRUE)) |>
  ### selecting relevant columns
  select(team, games_PY1, completion_pct_PY1, pass_ypa_PY1, pass_ypr_PY1, int_pct_PY1, rush_ypc_PY1, turnovers_pg_PY1, third_conv_rate_PY1, fourth_conv_rate_PY1, penalty_yds_pg_PY1, yards_per_penalty_PY1, kick_return_avg_PY1, punt_return_avg_PY1, total_yds_pg_PY1, pass_yds_pg_PY1, rush_yds_pg_PY1, first_downs_pg_PY1, off_ypp_PY1, def_interceptions_pg_PY1, off_plays_pg_PY1, off_ppg_PY1, def_ppg_PY1, def_yds_pg_PY1, def_plays_pg_PY1, def_third_conv_rate_PY1, def_fourth_conv_rate_PY1, def_ypp_PY1, fg_rate_PY1, fg_rate_allowed_PY1, fg_made_pg_PY1, fg_made_pg_allowed_PY1, xpts_pg_PY1, xpts_allowed_pg_PY1, kick_return_yds_avg_allowed_PY1, punt_return_yds_avg_allowed_PY1, st_ppg_PY1, st_ppg_allowed_PY1, oppdef_ppa_PY1, oppoff_ppa_PY1, off_ppa_PY1, off_success_rate_PY1, off_explosiveness_PY1,  off_power_success_PY1, off_stuff_rate_PY1, off_line_yds_PY1, off_second_lvl_yds_PY1, off_open_field_yds_PY1, off_pts_per_opp_PY1, off_field_pos_avg_predicted_points_PY1, off_havoc_total_PY1,off_havoc_front_seven_PY1, off_havoc_db_PY1, off_standard_downs_ppa_PY1, off_standard_downs_success_rate_PY1, off_standard_downs_explosiveness_PY1, off_passing_downs_ppa_PY1, off_passing_downs_success_rate_PY1, off_passing_downs_explosiveness_PY1, off_rushing_plays_ppa_PY1, off_rushing_plays_success_rate_PY1, off_rushing_plays_explosiveness_PY1, off_passing_plays_ppa_PY1,  off_passing_plays_success_rate_PY1, off_passing_plays_explosiveness_PY1, def_ppa_PY1, def_success_rate_PY1, def_explosiveness_PY1, def_power_success_PY1, def_stuff_rate_PY1, def_line_yds_PY1, def_second_lvl_yds_PY1, def_open_field_yds_PY1, def_pts_per_opp_PY1, def_field_pos_avg_predicted_points_PY1, def_havoc_total_PY1, def_havoc_front_seven_PY1, def_havoc_db_PY1, def_standard_downs_ppa_PY1, def_standard_downs_success_rate_PY1, def_standard_downs_explosiveness_PY1, def_passing_downs_ppa_PY1, def_passing_downs_success_rate_PY1, def_passing_downs_explosiveness_PY1, def_rushing_plays_ppa_PY1, def_rushing_plays_success_rate_PY1, def_rushing_plays_explosiveness_PY1, def_passing_plays_ppa_PY1, def_passing_plays_success_rate_PY1, def_passing_plays_explosiveness_PY1, recruit_pts_PY1, talent_PY1)
# mutate(season = 2024, .before = 1)

## getting rid of NAs
FCS_PY1[is.na(FCS_PY1)] = 0

##### CHECKING FOR MISSING COLUMNS IN FCS_PY1 compared to columns being used in VoA #####
# missing_FCSPY1colnames <- c()
# for (i in 1:length(PY1_colnames)){
#   if (PY1_colnames[i] %nin% colnames(FCS_PY1)){
#     missing_FCSPY1colnames <- c(missing_FCSPY1colnames, PY1_colnames[i])
#   }
# }
# missing_FCSPY1colnames


##### Writing FCS csvs #####
### creating directory
if (dir.exists(here("Data", paste0("VoA", old_season + 1),"FCSPrevYears")) == FALSE){
  dir.create(here("Data", paste0("VoA", old_season + 1), "FCSPrevYears"), recursive = TRUE)
}
write_csv(FCS_PY3, here("Data", paste0("VoA", old_season + 1), "FCSPrevYears", "FCS_PY3.csv"))
write_csv(FCS_PY2, here("Data", paste0("VoA", old_season + 1), "FCSPrevYears", "FCS_PY2.csv"))
write_csv(FCS_PY1, here("Data", paste0("VoA", old_season + 1), "FCSPrevYears", "FCS_PY1.csv"))



