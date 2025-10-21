##### Vortex of Projection version 1.1.0 #####
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
### loading packages
library(pacman)
p_load(tidyverse, gt, cfbfastR, here, gtExtras, RColorBrewer, cfbplotR, webshot2, betareg)
### Inputting year
year <- readline(prompt = "What Year is it? ")
### Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

### Text Strings for gt table of game projections at the end
week_text <- "Week"
gameprojections_png <- "GameProjections.png"
gameprojections_filename <- paste(year, week_text, upcoming, gameprojections_png, sep = "")

##### reading in most recent VoA overall ratings #####
if (as.numeric(upcoming) == 1){
  FBS_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
    select(team, conference, VoA_Rating_Ovr)
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
    select(team, VoA_Rating_Ovr)
} else if (as.numeric(upcoming) == 16){
  week16_run <- readline(prompt = "Is this the first week 16 run of week 16 projections? (y/n) ")
  if (week16_run == "y"){
    PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
      select(team, VoA_Rating_Ovr)
  } else if (week16_run == "n"){
    PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming)), "_VoA.csv"))) |>
      select(team, VoA_Rating_Ovr)
  } else{
    print("Error, need 'y' or 'n'")
    break
  }
} else{
  FBS_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
    select(team, conference, VoA_Rating_Ovr)
  PrevWeek_VoA <- read_csv(here("Data", paste0("VoA", year), paste0(year, week_text, as.character(as.numeric(upcoming) - 1), "_VoA.csv"))) |>
    select(team, VoA_Rating_Ovr)
}




##### pulling SRS ratings for just FCS teams since I don't have VoA ratings for them #####
### using last year's until SRS ratings are available for current season
## expectation is that this will be sometime between weeks 4 and 6, based on 2023 season
if (as.numeric(upcoming) < 5) {
  FCS_ratings <- cfbd_ratings_srs(as.numeric(year) - 1) |>
    filter(conference != "ACC" & conference != "American Athletic" & conference != "Big 12" & conference != "Big Ten" & conference != "Conference USA" & conference != "FBS Independents" & conference != "Mid-American" & conference != "Mountain West" & conference != "Pac-12" & conference != "SEC" & conference != "Sun Belt") |>
    filter(team != "Kennesaw State") |>
    select(team, rating)
  colnames(FCS_ratings) <- c("team", "VoA_Rating_Ovr")
} else {
  FCS_ratings <- cfbd_ratings_srs(as.numeric(year)) |>
    filter(conference != "ACC" & conference != "American Athletic" & conference != "Big 12" & conference != "Big Ten" & conference != "Conference USA" & conference != "FBS Independents" & conference != "Mid-American" & conference != "Mountain West" & conference != "Pac-12" & conference != "SEC" & conference != "Sun Belt") |>
    filter(team != "Jacksonville State" & team != "Sam Houston State" & team != "James Madison") |>
    select(team, rating)
  colnames(FCS_ratings) <- c("team", "VoA_Rating_Ovr")
}

### Binding most recent VoA (PrevWeek_VoA) and FCS_ratings as if rating systems are the same
PrevWeek_VoA <- rbind(PrevWeek_VoA, FCS_ratings)

##### reading in upcoming games to create df of games and VoA projected margins #####
if (as.numeric(upcoming) == 16) {
  upcoming_games_df <- cfbd_game_info(as.numeric(year), season_type = "postseason") |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
  week16games <- cfbd_game_info(as.numeric(year), week = as.numeric(upcoming)) |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
  upcoming_games_df <- rbind(week16games, upcoming_games_df)
} else if (as.numeric(upcoming) == 1){
  FullSeason_Games <- cfbd_game_info(as.numeric(year)) |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
} else {
  upcoming_games_df <- cfbd_game_info(as.numeric(year), week = as.numeric(upcoming)) |>
    filter(home_team %in% PrevWeek_VoA$team | away_team %in% PrevWeek_VoA$team) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0,
           away_VoA_Rating = 0)
}

##### matching up VoA/SRS ratings with appropriate teams #####
if (as.numeric(upcoming) == 1){
  ### matching up VoA/SRS ratings with appropriate teams
  set.seed(802)
  for (game in 1:nrow(FullSeason_Games)){
    if (FullSeason_Games$home_team[game] %in% PrevWeek_VoA$team){
      FullSeason_Games$home_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == FullSeason_Games$home_team[game]]
    } else {
      ### generating a random rating for team not in VoA/SRS ratings
      FullSeason_Games$home_VoA_Rating[game] = rnorm(1, mean = quantile(FCS_ratings$VoA_Rating_Ovr, 0.1), sd = sd(FCS_ratings$VoA_Rating_Ovr, na.rm = TRUE))
    }
  }
  ### repeating to fill in away ratings
  for (game in 1:nrow(FullSeason_Games)){
    if (FullSeason_Games$away_team[game] %in% PrevWeek_VoA$team){
      FullSeason_Games$away_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == FullSeason_Games$away_team[game]]
    } else {
      ### generating a random rating for team not in VoA/SRS ratings
      FullSeason_Games$away_VoA_Rating[game] = rnorm(1, mean = quantile(FCS_ratings$VoA_Rating_Ovr, 0.1), sd = sd(FCS_ratings$VoA_Rating_Ovr, na.rm = TRUE))
    }
  }
} else {
  ### matching up VoA/SRS ratings with appropriate teams
  set.seed(802)
  for (game in 1:nrow(upcoming_games_df)){
    if (upcoming_games_df$home_team[game] %in% PrevWeek_VoA$team){
      upcoming_games_df$home_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == upcoming_games_df$home_team[game]]
    } else {
      ### generating a random rating for team not in VoA/SRS ratings
      upcoming_games_df$home_VoA_Rating[game] = rnorm(1, mean = quantile(FCS_ratings$VoA_Rating_Ovr, 0.1) - 10, sd = sd(FCS_ratings$VoA_Rating_Ovr, na.rm = TRUE))
    }
  }
  ### repeating to fill in away ratings
  for (game in 1:nrow(upcoming_games_df)){
    if (upcoming_games_df$away_team[game] %in% PrevWeek_VoA$team){
      upcoming_games_df$away_VoA_Rating[game] = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == upcoming_games_df$away_team[game]]
    } else {
      ### generating a random rating for team not in VoA/SRS ratings
      upcoming_games_df$away_VoA_Rating[game] = rnorm(1, mean = quantile(FCS_ratings$VoA_Rating_Ovr, 0.1) - 10, sd = sd(FCS_ratings$VoA_Rating_Ovr, na.rm = TRUE))
    }
  }
}


### Creating Vortex of Projection Spread column for full season games
### called "predicted" so that it can be formatted easily for the CFBD prediction contest
if (as.numeric(upcoming) == 1){
  FullSeason_Games <- FullSeason_Games |>
    mutate(predicted = case_when(neutral_site == FALSE ~ away_VoA_Rating - (home_VoA_Rating + 2),
                                 TRUE ~ away_VoA_Rating - home_VoA_Rating))
  cfbdata_contest_df <- FullSeason_Games |>
    filter(week == as.numeric(upcoming)) |>
    select(game_id, home_team, away_team, predicted)
  colnames(cfbdata_contest_df) <- c("id", "home", "away", "predicted")
  write_csv(cfbdata_contest_df, here("Data", paste("VoA", year, sep = ""), "Projections", paste(year, "VoPWeek", upcoming, "Games.csv", sep = "")))
} else{
  ### Creating Vortex of Projection Spread column for upcoming week's games
  cfbdata_contest_df <- upcoming_games_df |>
    mutate(predicted = case_when(neutral_site == FALSE ~ away_VoA_Rating - (home_VoA_Rating + 2),
                                 TRUE ~ away_VoA_Rating - home_VoA_Rating)) |>
    select(game_id, home_team, away_team, predicted)
  colnames(cfbdata_contest_df) <- c("id", "home", "away", "predicted")
  
  write_csv(cfbdata_contest_df, here("Data", paste("VoA", year, sep = ""), "Projections", paste(year, "VoPWeek", upcoming, "Games.csv", sep = "")))
}

### simple function to take VoA Ratings and field neutrality as inputs
margin_projection <- function(away, home, neutral) {
  margin_proj = PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == away] -  PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$team == home]
  if (neutral == FALSE) {
    margin_proj = margin_proj - 2
  }
  return(margin_proj)
}
### FCS version of above function
# srs <- cfbd_ratings_srs(as.numeric(year))
fcs_margin_projection <- function(away, home, neutral) {
  margin_proj = FCS_ratings$VoA_Rating_Ovr[FCS_ratings$team == away] -  FCS_ratings$VoA_Rating_Ovr[FCS_ratings$team == home]
  if (neutral == FALSE) {
    margin_proj = margin_proj - 2
  }
  return(margin_proj)
}

##### Evaluating VoP's projected winner and their respective win probability #####
### coefficients for calculating win probability aren't just random long decimal numbers, I fit a model using lm() to Bill Connelly's projected win probs and just took them out and wrote them into this script instead of just fitting that model over and over every week
### it's a lazy way of "calculating" win prob but it works well enough for my purposes
### I wanted to fit a stan model but that didn't work so I'm trying a beta regression model with betareg to do something different, see how it goes
# SP_WPdata <- read_csv(here("Data", "SP_Projections", "All_SP.csv")) |>
#   separate(col = "Game", into = c("away_team", "home_team"), sep = " at ") |>
#   drop_na(away_team, home_team) |>
#   filter(home_team == Proj_winner | away_team == Proj_winner) |>
#   mutate(away_WP_pct = case_when(Proj_winner == away_team ~ WP_pct,
#                                  TRUE ~ 1 - WP_pct),
#          Proj_Margin = case_when(Proj_winner == away_team ~ Proj_margin,
#                                  TRUE ~ -1 * Proj_margin))
# 
# ### fitting betareg model
# set.seed(802)
# WP_betareg <- betareg(away_WP_pct ~ Proj_Margin, data = SP_WPdata)
# 
# ### since the model's already fit, I'm just saving it as an RDS file so I don't have to fit it each and every week
# saveRDS(WP_betareg, here("Data", "SP_Projections", "WP_betareg.rds"))
WP_betareg <- read_rds(here("Data", "SP_Projections", "WP_betareg.rds"))
summary(WP_betareg)


if (as.numeric(upcoming) == 1){
  ### adding projected winner, projected win margin, and win probability
  ### home field advantage of 2 points when neutral_site == FALSE
  FullSeason_Games <- FullSeason_Games |>
    mutate(Proj_Winner = case_when(neutral_site == FALSE & (home_VoA_Rating + 2) > away_VoA_Rating ~ home_team, 
                                   neutral_site == FALSE & away_VoA_Rating > (home_VoA_Rating + 2) ~ away_team, 
                                   neutral_site == TRUE & home_VoA_Rating > away_VoA_Rating ~ home_team, 
                                   neutral_site == TRUE & away_VoA_Rating > home_VoA_Rating ~ away_team, 
                                   TRUE ~ "TIE"),
           Proj_Margin = case_when(neutral_site == FALSE ~ abs(away_VoA_Rating - (home_VoA_Rating + 2)), 
                                   TRUE ~ abs(away_VoA_Rating - home_VoA_Rating)))
  FullSeason_Games <- FullSeason_Games |>
    mutate(win_prob = predict(WP_betareg, newdata = FullSeason_Games)) |>
    select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, win_prob) ## |>
  # arrange(desc(Proj_Margin))
  upcoming_games_df <- FullSeason_Games |>
    filter(week == as.numeric(upcoming)) |>
    filter(home_team %in% FBS_VoA$team | away_team %in% FBS_VoA$team)
} else{
  ### making gt table of upcoming games df to display games with close spreads
  upcoming_games_df <- upcoming_games_df |>
    mutate(Proj_Winner = case_when(neutral_site == FALSE & (home_VoA_Rating + 2) > away_VoA_Rating ~ home_team, 
                                   neutral_site == FALSE & away_VoA_Rating > (home_VoA_Rating + 2) ~ away_team, 
                                   neutral_site == TRUE & home_VoA_Rating > away_VoA_Rating ~ home_team, 
                                   neutral_site == TRUE & away_VoA_Rating > home_VoA_Rating ~ away_team, 
                                   TRUE ~ "TIE"),
           Proj_Margin = case_when(neutral_site == FALSE ~ abs(away_VoA_Rating - (home_VoA_Rating + 2)), 
                                   TRUE ~ abs(away_VoA_Rating - home_VoA_Rating)))
  ### calculating win probability based on model built with betareg
  upcoming_games_df <- upcoming_games_df |>
    mutate(win_prob = predict(WP_betareg, newdata = upcoming_games_df)) |>
    select(game_id, season, week, neutral_site, home_team, home_VoA_Rating, away_team, away_VoA_Rating, Proj_Winner, Proj_Margin, win_prob) |>
    filter(home_team %in% FBS_VoA$team | away_team %in% FBS_VoA$team)
  # arrange(desc(Proj_Margin))
}


##### WEEK 0 (week 1 upcoming) ONLY Calculating projected number of wins #####
if (as.numeric(upcoming) == 1){
  ### adding column to store projected number of wins
  ## storing dummy value in it for now
  FBS_VoA <- FBS_VoA |>
    mutate(proj_wins = -999)
  ### calculating median projected wins, storing it in FBS_VoA$Proj_Wins for appropriate teams
  for (school in 1:nrow(FBS_VoA)){
    temp_games_df <- FullSeason_Games |>
      filter(home_team == FBS_VoA$team[school] | away_team == FBS_VoA$team[school])
    temp_wins_df <- temp_games_df |>
      filter(Proj_Winner == FBS_VoA$team[school])
    temp_losses_df <- temp_games_df |>
      filter(Proj_Winner != FBS_VoA$team[school])
    temp_proj_wins <- (sum(temp_wins_df$win_prob)) + (nrow(temp_losses_df) - (sum(temp_losses_df$win_prob)))
    FBS_VoA$proj_wins[school] = temp_proj_wins
  }
  
  ### making tables with gt for each conference showing each team's projected wins
  ### each conference (including independents) gets separate tables
  AAC_ProjWins <- FBS_VoA |> filter(conference == "American Athletic") |>
    arrange(desc(proj_wins))
  ACC_ProjWins <- FBS_VoA |> filter(conference == "ACC") |>
    arrange(desc(proj_wins))
  Big12_ProjWins <- FBS_VoA |> filter(conference == "Big 12") |>
    arrange(desc(proj_wins))
  Big10_ProjWins <- FBS_VoA |> filter(conference == "Big Ten") |>
    arrange(desc(proj_wins))
  CUSA_ProjWins <- FBS_VoA |> filter(conference == "Conference USA") |>
    arrange(desc(proj_wins))
  ### lumping the 2Pac with the Indys
  Indy_2Pac_ProjWins <- FBS_VoA |> filter(conference == "FBS Independents" | conference == "Pac-12") |>
    arrange(desc(proj_wins))
  MAC_ProjWins <- FBS_VoA |> filter(conference == "Mid-American") |>
    arrange(desc(proj_wins))
  MWC_ProjWins <- FBS_VoA |> filter(conference == "Mountain West") |>
    arrange(desc(proj_wins))
  # Pac12_ProjWins <- FBS_VoA |> filter(conference == "Pac-12") |>
    # arrange(desc(proj_wins))
  SEC_ProjWins <- FBS_VoA |> filter(conference == "SEC") |>
    arrange(desc(proj_wins))
  SunBelt_ProjWins <- FBS_VoA |> filter(conference == "Sun Belt") |>
    arrange(desc(proj_wins))
  
  ### Creating gt table
  ## adding title and subtitle
  AAC_ProjWins_gt <- AAC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "AAC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(60))
  AAC_ProjWins_gt
  AAC_ProjWins_gt |>
    gtsave(
      "AACWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  
  ### ACC
  ### Creating gt table
  ## adding title and subtitle
  ACC_ProjWins_gt <- ACC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "ACC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(70))
  ACC_ProjWins_gt
  ACC_ProjWins_gt |>
    gtsave(
      "ACCWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Big 12
  ### Creating gt table
  ## adding title and subtitle
  Big12_ProjWins_gt <- Big12_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Big 12 Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(70))
  Big12_ProjWins_gt
  Big12_ProjWins_gt |>
    gtsave(
      "Big12WinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Big 10
  ### Creating gt table
  ## adding title and subtitle
  Big10_ProjWins_gt <- Big10_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Big 10 Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    Ratings for FCS teams used in these calculations are actually SRS ratings taken from CFB Data API"
    ) |>
    tab_options(table.width = pct(70))
  Big10_ProjWins_gt
  Big10_ProjWins_gt |>
    gtsave(
      "Big10WinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### CUSA
  ### Creating gt table
  ## adding title and subtitle
  CUSA_ProjWins_gt <- CUSA_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "CUSA Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    Ratings for FCS teams used in these calculations are actually SRS ratings taken from CFB Data API"
    ) |>
    tab_options(table.width = pct(50))
  CUSA_ProjWins_gt
  CUSA_ProjWins_gt |>
    gtsave(
      "CUSAWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Indies/2Pac
  ### Creating gt table
  ## adding title and subtitle
  Indy_2Pac_ProjWins_gt <- Indy_2Pac_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Independents & 2Pac Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(40))
  Indy_2Pac_ProjWins_gt
  Indy_2Pac_ProjWins_gt |>
    gtsave(
      "Indy_2PacWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### MAC
  ### Creating gt table
  ## adding title and subtitle
  MAC_ProjWins_gt <- MAC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "MAC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(50))
  MAC_ProjWins_gt
  MAC_ProjWins_gt |>
    gtsave(
      "MACWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### MWC
  ### Creating gt table
  ## adding title and subtitle
  MWC_ProjWins_gt <- MWC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "MWC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(50))
  MWC_ProjWins_gt
  MWC_ProjWins_gt |>
    gtsave(
      "MWCWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### SEC
  ### Creating gt table
  ## adding title and subtitle
  SEC_ProjWins_gt <- SEC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "SEC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(70))
  SEC_ProjWins_gt
  SEC_ProjWins_gt |>
    gtsave(
      "SECWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Sun Belt
  ### Creating gt table
  ## adding title and subtitle
  SunBelt_ProjWins_gt <- SunBelt_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Sun Belt Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>  
    data_color( # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(team = "Team", VoA_Rating_Ovr = "VoA Overall Rating", proj_wins = "Median Projected Wins") |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(60))
  SunBelt_ProjWins_gt
  SunBelt_ProjWins_gt |>
    gtsave(
      "SunBeltWinProjections.png", expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
} else{
  print("Season ongoing")
}



##### making table of games and projected winners and margins #####
## bowl projection table made slightly differently
# not saved, just created for personal use
if (as.numeric(upcoming) == 16) {
  ### Creating gt table
  ## adding title and subtitle
  upcoming_games_gt <- upcoming_games_df |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Vortex of Accuracy Bowl Game Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin),
      decimals = 3 # With 3 decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 3 # I want this column to have 3 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>  
    fmt_number( # Another numeric column
      columns = c(win_prob),
      decimals = 3
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(win_prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", win_prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    )
  
  ### sorting df by projected win margin for ESPN prediction game
  upcoming_games_df_sorted <- upcoming_games_df |>
    arrange(Proj_Margin)
  
  ### Creating gt table
  ## adding title and subtitle
  upcoming_games_gt_sorted <- upcoming_games_df_sorted |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Vortex of Accuracy Bowl Game Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin),
      decimals = 3 # With 3 decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 3 # I want this column to have 3 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>  
    fmt_number( # Another numeric column
      columns = c(win_prob),
      decimals = 3
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(win_prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", win_prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    )
  upcoming_games_gt_sorted
} else {
  ## Creating gt table
  # adding title and subtitle
  upcoming_games_gt <- upcoming_games_df |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, week_text, upcoming, "Vortex of Projection Game Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin), # What column variable? FinalVoATop25$VoA_Rating
      decimals = 3 # With 3 decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 3 # I want this column to have 2 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(win_prob),
      decimals = 3
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(win_prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", win_prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    )
  
  
  ### sorting df by projected win margin for ESPN prediction game
  upcoming_games_df_sorted <- upcoming_games_df |>
    arrange(Proj_Margin)
  
  ### Creating gt table
  ## adding title and subtitle
  upcoming_games_gt_sorted <- upcoming_games_df_sorted |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title =  paste(year, week_text, upcoming, "Vortex of Projection Game Projections, sorted"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection")  |>  # and this subtitle
    fmt_number( # A column (numeric data)
      columns = c(Proj_Margin),
      decimals = 3 # With 3 decimal places
    ) |> 
    fmt_number( # Another column (also numeric data)
      columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
      decimals = 3 # I want this column to have 3 decimal places
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>
    fmt_number( # Another numeric column
      columns = c(away_VoA_Rating),
      decimals = 3
    ) |>  
    fmt_number( # Another numeric column
      columns = c(win_prob),
      decimals = 3
    ) |> 
    data_color( # Update cell colors, testing different color palettes
      columns = c(Proj_Margin), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color( # Update cell colors, testing different color palettes
      columns = c(win_prob), # ...for dose column
      fn = scales::col_numeric( # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(home_team = "Home", away_team = "Away", home_VoA_Rating = "Home VoA Rating", away_VoA_Rating = "Away VoA Rating", Proj_Winner = "Projected Winner", Proj_Margin = "Projected Margin", win_prob = "Win Probability") |> # Update labels
    cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(game_id, season, week, neutral_site)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    )
  upcoming_games_gt_sorted
}
upcoming_games_gt
upcoming_games_gt |>
  gtsave(
    gameprojections_filename, expand = 5,
    path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
  )

### checking my level of remaining API calls
cfbd_api_key_info()
