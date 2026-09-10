##### Vortex of Projection version 1.1.0 #####
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
### loading packages
library(pacman)
# fmt: skip
p_load(tidyverse, gt, cfbfastR, here, gtExtras, RColorBrewer, cfbplotR, webshot2, betareg, arrow)
### function to get the inverse of %in%
`%nin%` <- Negate(`%in%`)
### Inputting year
year <- readline(prompt = "What Year is it? ")
### Inputting upcoming week number
upcoming <- readline(prompt = "What week is upcoming? ")

### Text Strings for gt table of game projections at the end
week_text <- "Week"
gameprojections_png <- "GameProjections.png"
gameprojections_filename <- paste(
  year,
  week_text,
  upcoming,
  gameprojections_png,
  sep = ""
)
### setting gt title based on whether it's after a playoff week or not
if (as.integer(upcoming) == 15) {
  gt_title <- paste(year, "Conference Championship Week Game Projections")
} else if (as.integer(upcoming) == 16) {
  gt_title <- paste(year, "Army-Navy Game and Bowl Game Projections")
} else if (as.integer(upcoming) == 17) {
  gt_title <- paste(
    year,
    "Vortex of Accuracy Bowl Game and CFP First Round Projections"
  )
} else if (as.integer(upcoming) == 18) {
  gt_title <- paste(
    year,
    "Vortex of Accuracy Bowl Game and CFP Quarterfinals Projections"
  )
} else if (as.integer(upcoming) == 19) {
  gt_title <- paste(
    year,
    "Vortex of Accuracy Bowl Game and CFP Semifinals Projections"
  )
} else if (as.integer(upcoming) == 20) {
  gt_title <- paste(year, "Vortex of Accuracy CFP Championship Projection")
} else {
  gt_title <- paste(
    year,
    week_text,
    upcoming,
    "Vortex of Accuracy Game Projections"
  )
}

##### reading in most recent VoA overall ratings #####
if (as.integer(upcoming) == 1) {
  FBS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      as.character(as.integer(upcoming) - 1),
      "_FBSVoA.parquet"
    )
  )) |>
    select(school, conference, VoA_Rating_Ovr)
  ### reading in FCS VoA now
  FCS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      as.character(as.integer(upcoming) - 1),
      "_FCSVoA.parquet"
    )
  )) |>
    select(school, conference, VoA_Rating_Ovr)

  ### Prev week VoA is the ratings directly calculated by the individual models
  PrevWeek_VoA_AllCols <- rbind(FBS_VoA, FCS_VoA)

  PrevWeek_VoA <- PrevWeek_VoA_AllCols |>
    select(school, VoA_Rating_Ovr)

  ### reading in VoA with all D1 teams with VoA ratings adjusted to reflect general differences between FBS and FCS subivisions
  ## unit-specific ratings not included, just overall ratings
  AllD1VoA <- read_csv(here(
    "Data",
    paste0("VoA", year),
    paste0("AllD1", year, week_text, as.integer(upcoming) - 1, "VoA.csv")
  ))

  LowerHalfRatings <- AllD1VoA |>
    filter(VoA_Rating_Ovr < median(AllD1VoA$VoA_Rating_Ovr))
} else {
  FBS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      as.character(as.integer(upcoming) - 1),
      "_FBSVoA.parquet"
    )
  )) |>
    select(school, conference, VoA_Rating_Ovr)
  ### reading in FCS VoA now
  FCS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      as.character(as.integer(upcoming) - 1),
      "_FCSVoA.parquet"
    )
  )) |>
    select(school, conference, VoA_Rating_Ovr)

  ### Prev week VoA is the ratings directly calculated by the individual models
  PrevWeek_VoA_AllCols <- rbind(FBS_VoA, FCS_VoA)

  PrevWeek_VoA <- PrevWeek_VoA_AllCols |>
    select(school, VoA_Rating_Ovr)
  ### reading in VoA with all D1 teams with VoA ratings adjusted to reflect general differences between FBS and FCS subivisions
  ## unit-specific ratings not included, just overall ratings
  AllD1VoA <- read_csv(here(
    "Data",
    paste0("VoA", year),
    paste0("AllD1", year, week_text, as.integer(upcoming) - 1, "VoA.csv")
  ))

  ### bottom 50% of ratings will be used to generate artificial ratings for teams not in the VoA but who are playing VoA teams
  LowerHalfRatings <- AllD1VoA |>
    filter(VoA_Rating_Ovr < median(AllD1VoA$VoA_Rating_Ovr))
}


##### reading in upcoming games to create df of games and VoA projected margins #####
if (as.integer(upcoming) == 16) {
  ##### Week 16 Game Pull #####
  upcoming_games_df <- cfbd_game_info(
    as.numeric(year),
    season_type = "postseason"
  ) |>
    filter(
      home_team %in% PrevWeek_VoA$school | away_team %in% PrevWeek_VoA$school
    ) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0, away_VoA_Rating = 0)
  week16games <- cfbd_game_info(as.numeric(year)) |>
    filter(completed == "FALSE") |>
    filter(
      home_team %in% PrevWeek_VoA$school | away_team %in% PrevWeek_VoA$school
    ) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0, away_VoA_Rating = 0)
  upcoming_games_df <- rbind(week16games, upcoming_games_df)
} else if (as.integer(upcoming) == 1) {
  ##### Preseason Game Pull #####
  FullSeason_Games <- cfbd_game_info(as.numeric(year)) |>
    select(
      game_id,
      season,
      week,
      neutral_site,
      home_team,
      home_division,
      home_conference,
      away_team,
      away_division,
      away_conference
    ) |>
    filter(home_team %in% AllD1VoA$school | away_team %in% AllD1VoA$school)

  ### setting initial temp df for assigning VoA ratings to games where both teams are in VoA
  temp_ratings_df <- PrevWeek_VoA |>
    select(school, VoA_Rating_Ovr)
  colnames(temp_ratings_df) <- c("home_team", "home_VoA_Rating")

  ### assigning ratings for home teams
  ### FBS Games
  FBSGames <- FullSeason_Games |>
    filter(home_team %in% FBS_VoA$school & away_team %in% FBS_VoA$school) |>
    left_join(temp_ratings_df, by = "home_team")
  ### FCS Games
  FCSGames <- FullSeason_Games |>
    filter(home_team %in% FCS_VoA$school & away_team %in% FCS_VoA$school) |>
    left_join(temp_ratings_df, by = "home_team")

  ### assigning ratings for away teams
  colnames(temp_ratings_df) <- c("away_team", "away_VoA_Rating")
  FBSGames <- FBSGames |>
    left_join(temp_ratings_df, by = "away_team")
  FCSGames <- FCSGames |>
    left_join(temp_ratings_df, by = "away_team")

  ### Games where just 1 team from the VoA is involved, not counting games above
  NonVoAGames <- FullSeason_Games |>
    filter(game_id %nin% FBSGames$game_id & game_id %nin% FBSGames$game_id)

  ### setting temp ratings df for games between FBS and FCS teams and maybe D1 (FBS or FCS) teams and D2/D3 teams
  temp_ratings_df <- AllD1VoA |>
    select(school, VoA_Rating_Ovr)
  colnames(temp_ratings_df) <- c("home_team", "home_VoA_Rating")
  ### adding VoA ratings to home teams in NonVoAGames
  NonVoAGames <- NonVoAGames |>
    left_join(temp_ratings_df, by = "home_team")
  ### assigning ratings for away teams
  colnames(temp_ratings_df) <- c("away_team", "away_VoA_Rating")
  ### adding VoA ratings to away teams in NonVoAGames
  NonVoAGames <- NonVoAGames |>
    left_join(temp_ratings_df, by = "away_team")

  ### rejoining all games backtogether to get FullSeason_Games with VoA Ratings attached
  FullSeason_Games <- rbind(FBSGames, rbind(FCSGames, NonVoAGames)) |>
    arrange(week) |>
    mutate(
      home_VoA_Rating = case_when(
        is.na(home_VoA_Rating) ~ rnorm(
          1,
          mean = mean(LowerHalfRatings$VoA_Rating_Ovr),
          sd = sd(LowerHalfRatings$VoA_Rating_Ovr)
        ),
        TRUE ~ home_VoA_Rating
      ),
      away_VoA_Rating = case_when(
        is.na(away_VoA_Rating) ~ rnorm(
          1,
          mean = mean(LowerHalfRatings$VoA_Rating_Ovr),
          sd = sd(LowerHalfRatings$VoA_Rating_Ovr)
        ),
        TRUE ~ away_VoA_Rating
      )
    )
} else if (as.integer(upcoming) > 16) {
  ##### Bowl Season and post-week 16 Game Pull #####
  upcoming_games_df <- cfbd_game_info(
    as.numeric(year),
    season_type = "postseason"
  ) |>
    filter(
      home_team %in% PrevWeek_VoA$school | away_team %in% PrevWeek_VoA$school
    ) |>
    filter(completed == FALSE) |>
    select(game_id, season, week, neutral_site, home_team, away_team) |>
    mutate(home_VoA_Rating = 0, away_VoA_Rating = 0)
} else {
  ##### Regular Season Game Pull #####
  upcoming_games_df <- cfbd_game_info(
    as.numeric(year),
    week = as.integer(upcoming)
  ) |>
    select(
      game_id,
      season,
      week,
      neutral_site,
      home_team,
      home_division,
      home_conference,
      away_team,
      away_division,
      away_conference
    ) |>
    filter(home_team %in% AllD1VoA$school | away_team %in% AllD1VoA$school)

  ### setting initial temp df for assigning VoA ratings to games where both teams are in VoA
  temp_ratings_df <- PrevWeek_VoA |>
    select(school, VoA_Rating_Ovr)
  colnames(temp_ratings_df) <- c("home_team", "home_VoA_Rating")

  ### assigning ratings for home teams
  ### FBS Games
  FBSGames <- upcoming_games_df |>
    filter(home_team %in% FBS_VoA$school & away_team %in% FBS_VoA$school) |>
    left_join(temp_ratings_df, by = "home_team")
  ### FCS Games
  FCSGames <- upcoming_games_df |>
    filter(home_team %in% FCS_VoA$school & away_team %in% FCS_VoA$school) |>
    left_join(temp_ratings_df, by = "home_team")

  ### assigning ratings for away teams
  colnames(temp_ratings_df) <- c("away_team", "away_VoA_Rating")
  FBSGames <- FBSGames |>
    left_join(temp_ratings_df, by = "away_team")
  FCSGames <- FCSGames |>
    left_join(temp_ratings_df, by = "away_team")

  ### Games where just 1 team from the VoA is involved, not counting games above
  NonVoAGames <- upcoming_games_df |>
    filter(game_id %nin% FBSGames$game_id & game_id %nin% FBSGames$game_id)

  ### setting temp ratings df for games between FBS and FCS teams and maybe D1 (FBS or FCS) teams and D2/D3 teams
  temp_ratings_df <- AllD1VoA |>
    select(school, VoA_Rating_Ovr)
  colnames(temp_ratings_df) <- c("home_team", "home_VoA_Rating")
  ### adding VoA ratings to home teams in NonVoAGames
  NonVoAGames <- NonVoAGames |>
    left_join(temp_ratings_df, by = "home_team")
  ### assigning ratings for away teams
  colnames(temp_ratings_df) <- c("away_team", "away_VoA_Rating")
  ### adding VoA ratings to away teams in NonVoAGames
  NonVoAGames <- NonVoAGames |>
    left_join(temp_ratings_df, by = "away_team")

  ### rejoining all games backtogether to get FullSeason_Games with VoA Ratings attached
  upcoming_games_df <- rbind(FBSGames, rbind(FCSGames, NonVoAGames)) |>
    arrange(week) |>
    mutate(
      home_VoA_Rating = case_when(
        is.na(home_VoA_Rating) ~ rnorm(
          1,
          mean = mean(LowerHalfRatings$VoA_Rating_Ovr),
          sd = sd(LowerHalfRatings$VoA_Rating_Ovr)
        ),
        TRUE ~ home_VoA_Rating
      ),
      away_VoA_Rating = case_when(
        is.na(away_VoA_Rating) ~ rnorm(
          1,
          mean = mean(LowerHalfRatings$VoA_Rating_Ovr),
          sd = sd(LowerHalfRatings$VoA_Rating_Ovr)
        ),
        TRUE ~ away_VoA_Rating
      )
    )
}


### Creating Vortex of Projection Spread column for full season games
### called "predicted" so that it can be formatted easily for the CFBD prediction contest
if (as.integer(upcoming) == 1) {
  FullSeason_Games <- FullSeason_Games |>
    mutate(
      predicted = case_when(
        neutral_site == FALSE ~ away_VoA_Rating - (home_VoA_Rating + 2),
        TRUE ~ away_VoA_Rating - home_VoA_Rating
      )
    )
  cfbdata_contest_df <- FullSeason_Games |>
    filter(week == as.integer(upcoming)) |>
    select(game_id, home_team, away_team, predicted)
  colnames(cfbdata_contest_df) <- c("id", "home", "away", "predicted")
  write_csv(
    cfbdata_contest_df,
    here(
      "Data",
      paste("VoA", year, sep = ""),
      "Projections",
      paste(year, "VoPWeek", upcoming, "Games.csv", sep = "")
    )
  )
} else {
  ### Creating Vortex of Projection Spread column for upcoming week's games
  cfbdata_contest_df <- upcoming_games_df |>
    mutate(
      predicted = case_when(
        neutral_site == FALSE ~ away_VoA_Rating - (home_VoA_Rating + 2),
        TRUE ~ away_VoA_Rating - home_VoA_Rating
      )
    ) |>
    select(game_id, home_team, away_team, predicted)
  colnames(cfbdata_contest_df) <- c("id", "home", "away", "predicted")

  write_csv(
    cfbdata_contest_df,
    here(
      "Data",
      paste("VoA", year, sep = ""),
      "Projections",
      paste(year, "VoPWeek", upcoming, "Games.csv", sep = "")
    )
  )
}

### simple function to take VoA Ratings and field neutrality as inputs
margin_projection <- function(away, home, neutral) {
  margin_proj <- PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$school == away] -
    PrevWeek_VoA$VoA_Rating_Ovr[PrevWeek_VoA$school == home]
  if (neutral == FALSE) {
    margin_proj <- margin_proj - 2
  }
  return(margin_proj)
}
### FCS version of above function
# srs <- cfbd_ratings_srs(as.numeric(year))
# fcs_margin_projection <- function(away, home, neutral) {
#   margin_proj <- FCS_ratings$VoA_Rating_Ovr[FCS_ratings$school == away] -
#     FCS_ratings$VoA_Rating_Ovr[FCS_ratings$school == home]
#   if (neutral == FALSE) {
#     margin_proj <- margin_proj - 2
#   }
#   return(margin_proj)
# }

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


if (as.integer(upcoming) == 1) {
  ### adding projected winner, projected win margin, and win probability
  ### home field advantage of 2 points when neutral_site == FALSE
  FullSeason_Games <- FullSeason_Games |>
    mutate(
      Proj_Winner = case_when(
        neutral_site == FALSE &
          (home_VoA_Rating + 2) > away_VoA_Rating ~ home_team,
        neutral_site == FALSE &
          away_VoA_Rating > (home_VoA_Rating + 2) ~ away_team,
        neutral_site == TRUE & home_VoA_Rating > away_VoA_Rating ~ home_team,
        neutral_site == TRUE & away_VoA_Rating > home_VoA_Rating ~ away_team,
        TRUE ~ "TIE"
      ),
      Proj_Margin = case_when(
        neutral_site == FALSE ~ abs(away_VoA_Rating - (home_VoA_Rating + 2)),
        TRUE ~ abs(away_VoA_Rating - home_VoA_Rating)
      )
    )
  FullSeason_Games <- FullSeason_Games |>
    mutate(win_prob = predict(WP_betareg, newdata = FullSeason_Games)) |>
    select(
      game_id,
      season,
      week,
      neutral_site,
      home_team,
      home_division,
      home_conference,
      home_VoA_Rating,
      away_team,
      away_division,
      away_conference,
      away_VoA_Rating,
      Proj_Winner,
      Proj_Margin,
      win_prob
    ) ## |>
  # arrange(desc(Proj_Margin))
  upcoming_games_df <- FullSeason_Games |>
    filter(week == as.integer(upcoming)) #|>
  # filter(home_team %in% AllD1VoA$school | away_team %in% AllD1VoA$school)
} else {
  ### preparing df for making gt table of upcoming games df to display games with close spreads
  upcoming_games_df <- upcoming_games_df |>
    mutate(
      Proj_Winner = case_when(
        neutral_site == FALSE &
          (home_VoA_Rating + 2) > away_VoA_Rating ~ home_team,
        neutral_site == FALSE &
          away_VoA_Rating > (home_VoA_Rating + 2) ~ away_team,
        neutral_site == TRUE & home_VoA_Rating > away_VoA_Rating ~ home_team,
        neutral_site == TRUE & away_VoA_Rating > home_VoA_Rating ~ away_team,
        TRUE ~ "TIE"
      ),
      Proj_Margin = case_when(
        neutral_site == FALSE ~ abs(away_VoA_Rating - (home_VoA_Rating + 2)),
        TRUE ~ abs(away_VoA_Rating - home_VoA_Rating)
      )
    )
  ### calculating win probability based on model built with betareg
  upcoming_games_df <- upcoming_games_df |>
    mutate(win_prob = predict(WP_betareg, newdata = upcoming_games_df)) |>
    select(
      game_id,
      season,
      week,
      neutral_site,
      home_team,
      home_division,
      home_conference,
      home_VoA_Rating,
      away_team,
      away_division,
      away_conference,
      away_VoA_Rating,
      Proj_Winner,
      Proj_Margin,
      win_prob
    ) #|>
  # filter(home_team %in% FBS_VoA$school | away_team %in% FBS_VoA$school)
  # arrange(desc(Proj_Margin))
}


##### WEEK 0 (week 1 upcoming) ONLY Calculating projected number of wins #####
if (as.integer(upcoming) == 1) {
  ### adding column to store projected number of wins
  ## storing dummy value in it for now
  PrevWeek_VoA_AllCols <- PrevWeek_VoA_AllCols |>
    mutate(proj_wins = -999)
  ### calculating median projected wins, storing it in FBS_VoA$Proj_Wins for appropriate teams
  for (team in 1:nrow(PrevWeek_VoA_AllCols)) {
    temp_games_df <- FullSeason_Games |>
      filter(
        home_team == PrevWeek_VoA_AllCols$school[team] |
          away_team == PrevWeek_VoA_AllCols$school[team]
      )
    temp_wins_df <- temp_games_df |>
      filter(Proj_Winner == PrevWeek_VoA_AllCols$school[team])
    temp_losses_df <- temp_games_df |>
      filter(Proj_Winner != PrevWeek_VoA_AllCols$school[team])
    temp_proj_wins <- (sum(temp_wins_df$win_prob)) +
      (nrow(temp_losses_df) - (sum(temp_losses_df$win_prob)))
    PrevWeek_VoA_AllCols$proj_wins[team] <- temp_proj_wins
  }

  ### making tables with gt for each conference showing each team's projected wins
  ### each conference (including independents) gets separate tables
  AAC_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "American Athletic") |>
    arrange(desc(proj_wins))
  ACC_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "ACC") |>
    arrange(desc(proj_wins))
  Big12_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Big 12") |>
    arrange(desc(proj_wins))
  Big10_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Big Ten") |>
    arrange(desc(proj_wins))
  CUSA_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Conference USA") |>
    arrange(desc(proj_wins))
  ### FBS Independents
  Indy_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "FBS Independents") |>
    arrange(desc(proj_wins))
  MAC_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Mid-American") |>
    arrange(desc(proj_wins))
  MWC_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Mountain West") |>
    arrange(desc(proj_wins))
  Pac12_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Pac-12") |>
    arrange(desc(proj_wins))
  SEC_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "SEC") |>
    arrange(desc(proj_wins))
  SunBelt_ProjWins <- PrevWeek_VoA_AllCols |>
    filter(conference == "Sun Belt") |>
    arrange(desc(proj_wins))

  ### Creating gt table
  ## adding title and subtitle
  AAC_ProjWins_gt <- AAC_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "AAC Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "AACWinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "ACCWinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "Big12WinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "Big10WinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "CUSAWinProjections.png",
      expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Indies/2Pac
  ### Creating gt table
  ## adding title and subtitle
  Indy_ProjWins_gt <- Indy_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Independents Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR, FCS data mostly from stats.ncaa.org,
    VoA Ratings for FCS teams are actually SRS ratings taken from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(40))
  Indy_ProjWins_gt
  Indy_ProjWins_gt |>
    gtsave(
      "IndyWinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "MACWinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "MWCWinProjections.png",
      expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
  ### Pac12
  ### Creating gt table
  ## adding title and subtitle
  Pac12_ProjWins_gt <- Pac12_ProjWins |>
    gt() |> # use 'gt' to make an awesome table...
    gt_theme_espn() |>
    tab_header(
      title = paste(year, "Pac 12 Median Win Total Projections"), # ...with this title
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
    # cols_move_to_end(columns = "win_prob") |>
    cols_hide(c(conference)) |>
    tab_footnote(
      footnote = "Data from CFB Data API via cfbfastR"
    ) |>
    tab_options(table.width = pct(50))
  Pac12_ProjWins_gt
  Pac12_ProjWins_gt |>
    gtsave(
      "Pac12WinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "SECWinProjections.png",
      expand = 5,
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
      subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
    ) |> # and this subtitle
    fmt_number(
      # Another numeric column
      columns = c(VoA_Rating_Ovr),
      decimals = 3
    ) |>
    fmt_number(
      # Another numeric column
      columns = c(proj_wins),
      decimals = 1
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(VoA_Rating_Ovr), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    data_color(
      # Update cell colors, testing different color palettes
      columns = c(proj_wins), # ...for dose column
      fn = scales::col_numeric(
        # <- bc it's numeric
        palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
        domain = c(), # Column scale endpoints
        reverse = FALSE
      )
    ) |>
    cols_label(
      school = "School",
      VoA_Rating_Ovr = "VoA Overall Rating",
      proj_wins = "Median Projected Wins"
    ) |> # Update labels
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
      "SunBeltWinProjections.png",
      expand = 5,
      path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
    )
} else {
  print("Season ongoing")
}


##### making table of games and projected winners and margins #####
## bowl projection table made slightly differently
# not saved, just created for personal use
### Creating gt table
## adding title and subtitle
upcoming_games_gt <- upcoming_games_df |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_espn() |>
  tab_header(
    title = gt_title, # ...with this title
    subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
  ) |> # and this subtitle
  fmt_number(
    # A column (numeric data)
    columns = c(Proj_Margin),
    decimals = 3 # With 3 decimal places
  ) |>
  fmt_number(
    # Another column (also numeric data)
    columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 3 decimal places
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(win_prob),
    decimals = 3
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(Proj_Margin), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(win_prob), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  cols_label(
    home_team = "Home",
    away_team = "Away",
    home_VoA_Rating = "Home VoA Rating",
    away_VoA_Rating = "Away VoA Rating",
    Proj_Winner = "Projected Winner",
    Proj_Margin = "Projected Margin",
    win_prob = "Win Probability"
  ) |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(
    game_id,
    season,
    week,
    neutral_site,
    home_division,
    home_conference,
    away_division,
    away_conference
  )) |>
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
    title = gt_title, # ...with this title
    subtitle = "The Unquestionably Puzzling Yet Impeccibly Perceptive Vortex of Projection"
  ) |> # and this subtitle
  fmt_number(
    # A column (numeric data)
    columns = c(Proj_Margin),
    decimals = 3 # With 3 decimal places
  ) |>
  fmt_number(
    # Another column (also numeric data)
    columns = c(home_VoA_Rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 3 decimal places
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_Rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(win_prob),
    decimals = 3
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(Proj_Margin), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdBu"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color(
    # Update cell colors, testing different color palettes
    columns = c(win_prob), # ...for dose column
    fn = scales::col_numeric(
      # <- bc it's numeric
      palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  cols_label(
    home_team = "Home",
    away_team = "Away",
    home_VoA_Rating = "Home VoA Rating",
    away_VoA_Rating = "Away VoA Rating",
    Proj_Winner = "Projected Winner",
    Proj_Margin = "Projected Margin",
    win_prob = "Win Probability"
  ) |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(
    game_id,
    season,
    week,
    neutral_site,
    home_division,
    home_conference,
    away_division,
    away_conference
  )) |>
  tab_footnote(
    footnote = "Data from CFB Data API via cfbfastR"
  )
upcoming_games_gt_sorted
upcoming_games_gt
upcoming_games_gt |>
  gtsave(
    gameprojections_filename,
    expand = 5,
    path = here("Outputs", "RVoA", paste0("VoA", year), "VoP")
  )

### checking my level of remaining API calls
cfbd_api_key_info()
