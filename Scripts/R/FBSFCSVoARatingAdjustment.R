##### FBS-FCS VoA Rating Adjustment #####
### For the 2026 season: In this script, I will read in the VoA ratings from FBS and FCS (calculated separately)
## and I will apply them to previous games and evaluate the impact of D1 subdivision on what the margin actually is compared to what would be expected without an adjustment
### actually
## might just read in the games, build a mixed effects model with home field advantage, team 1 subdivision, team 2 subdivision, and take the coefficient from each subdivision
## win margin would be the target variable
##### loading packages #####
library(pacman)
p_load(here, tidyverse, lme4, arrow, cfbfastR, gt, gtExtras)

### Inputting year
year <- readline(prompt = "What Year is it? ")
### Inputting most recent cfb week number
cfb_week <- readline(prompt = "What week just occurred? ")

### Text Strings
week_text <- "Week"
##### VoA Subdivision-based ratings adjustment #####
if (as.integer(cfb_week) == 0) {
  ##### Preseason ratings adjustment #####
  ### reading in most recent VoAs
  FBS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      0,
      "_FBSVoA.parquet"
    )
  )) |>
    select(school, classification, conference, VoA_Rating_Ovr)
  ### reading in FCS VoA now
  FCS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      0,
      "_FCSVoA.parquet"
    )
  )) |>
    select(school, classification, conference, VoA_Rating_Ovr)

  ### binding FBS and FCS VoAs together rowwise
  AllD1VoA <- rbind(FBS_VoA, FCS_VoA)

  ### loading games
  Games <- cfbd_game_info(as.integer(year) - 1)
  GamesHomeAway <- Games |>
    select(game_id, home_team, away_team)
  Games_Adj <- Games |>
    filter(home_team %in% AllD1VoA$school & away_team %in% AllD1VoA$school) |>
    pivot_longer(
      cols = c("home_team", "away_team"),
      names_to = "home_away_col_names",
      values_to = "team"
    ) |>
    left_join(GamesHomeAway, by = "game_id") |>
    mutate(
      team = as.factor(team),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_team" ~ away_team,
        TRUE ~ home_team
      )),
      team_subdivision = case_when(
        team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      opp_team_subdivision = case_when(
        opp_team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      team_points = case_when(
        team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      opp_team_points = case_when(
        opp_team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      hfa = as.factor(case_when(
        neutral_site == TRUE ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    ) |>
    mutate(team_margin = team_points - opp_team_points)

  ### fitting mixed effects model, treating team and opposing team subdivisions as random effects
  set.seed(802)
  Subdivision_mixed_model <- lmer(
    team_margin ~ hfa + (1 | team_subdivision) + (1 | opp_team_subdivision),
    data = Games_Adj
  )

  write_rds(
    Subdivision_mixed_model,
    here("Data", "FittedModels", "FBSFCSOvrlVoAEffects.rds"),
    compress = "gz"
  )

  subdivision_effects <- ranef(Subdivision_mixed_model)

  subdivision_diff <- max(subdivision_effects$team_subdivision$`(Intercept)`) -
    min(subdivision_effects$team_subdivision$`(Intercept)`)

  ### Final All D1 DF
  FinalAllD1VoA <- AllD1VoA |>
    mutate(
      VoA_Rating_Ovr = case_when(
        classification == "fcs" ~ VoA_Rating_Ovr - (subdivision_diff / 2),
        TRUE ~ VoA_Rating_Ovr + (subdivision_diff / 2)
      ),
    ) |>
    mutate(VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr))) |>
    arrange(desc(VoA_Rating_Ovr))

  write_csv(
    FinalAllD1VoA,
    here(
      "Data",
      paste0("VoA", year),
      paste0("AllD1", year, week_text, cfb_week, "VoA.csv")
    )
  )
} else {
  ##### Post Week 1 VoA subdivision-based ratings adjustment #####
  ### not refitting the mixed effects model to evaluate general margin difference between FBS and FCS because there's not enough data to justify it until the end of the season I think
  ## even if there was it seems like a hassle
  ### actually might just combine two seasons worth of data, previous year and whatever's occurred in the current season, see below team/VoA loads

  ### reading in previous VoAs
  FBS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      cfb_week,
      "_FBSVoA.parquet"
    )
  )) |>
    select(school, classification, conference, VoA_Rating_Ovr)
  ### reading in FCS VoA now
  FCS_VoA <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    paste0(
      year,
      week_text,
      cfb_week,
      "_FCSVoA.parquet"
    )
  )) |>
    select(school, classification, conference, VoA_Rating_Ovr)

  ### binding FBS and FCS VoAs together rowwise
  AllD1VoA <- rbind(FBS_VoA, FCS_VoA)

  ### loading games
  Games_PY1 <- cfbd_game_info(as.integer(year) - 1)
  GamesHomeAway_PY1 <- Games_PY1 |>
    select(game_id, home_team, away_team)
  Games_Adj_PY1 <- Games_PY1 |>
    filter(home_team %in% AllD1VoA$school & away_team %in% AllD1VoA$school) |>
    pivot_longer(
      cols = c("home_team", "away_team"),
      names_to = "home_away_col_names",
      values_to = "team"
    ) |>
    left_join(GamesHomeAway_PY1, by = "game_id") |>
    mutate(
      team = as.factor(team),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_team" ~ away_team,
        TRUE ~ home_team
      )),
      team_subdivision = case_when(
        team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      opp_team_subdivision = case_when(
        opp_team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      team_points = case_when(
        team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      opp_team_points = case_when(
        opp_team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      hfa = as.factor(case_when(
        neutral_site == TRUE ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    ) |>
    mutate(team_margin = team_points - opp_team_points)
  ### loading games
  Games <- cfbd_game_info(as.integer(year))
  GamesHomeAway <- Games |>
    select(game_id, home_team, away_team)
  Games_Adj <- Games |>
    filter(home_team %in% AllD1VoA$school & away_team %in% AllD1VoA$school) |>
    pivot_longer(
      cols = c("home_team", "away_team"),
      names_to = "home_away_col_names",
      values_to = "team"
    ) |>
    left_join(GamesHomeAway, by = "game_id") |>
    mutate(
      team = as.factor(team),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_team" ~ away_team,
        TRUE ~ home_team
      )),
      team_subdivision = case_when(
        team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      opp_team_subdivision = case_when(
        opp_team == home_team ~ home_division,
        TRUE ~ away_division
      ),
      team_points = case_when(
        team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      opp_team_points = case_when(
        opp_team == home_team ~ home_points,
        TRUE ~ away_points
      ),
      hfa = as.factor(case_when(
        neutral_site == TRUE ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    ) |>
    mutate(team_margin = team_points - opp_team_points)

  ### binding previous season's games and current season's games
  Games_Adj <- rbind(Games_Adj_PY1, Games_Adj)

  ### copied and pasted from mixed effects model fit in preseason
  # subdivision_diff <- 29.300704011082225

  ##### fitting mixed effects model, treating team and opposing team subdivisions as random effects #####
  set.seed(802)
  Subdivision_mixed_model <- lmer(
    team_margin ~ hfa + (1 | team_subdivision) + (1 | opp_team_subdivision),
    data = Games_Adj
  )

  write_rds(
    Subdivision_mixed_model,
    here("Data", "FittedModels", "FBSFCSOvrlVoAEffects.rds"),
    compress = "gz"
  )

  subdivision_effects <- ranef(Subdivision_mixed_model)

  subdivision_diff <- max(subdivision_effects$team_subdivision$`(Intercept)`) -
    min(subdivision_effects$team_subdivision$`(Intercept)`)

  ##### Final All D1 DF #####
  FinalAllD1VoA <- AllD1VoA |>
    mutate(
      VoA_Rating_Ovr = case_when(
        classification == "fcs" ~ VoA_Rating_Ovr - (subdivision_diff / 2),
        TRUE ~ VoA_Rating_Ovr + (subdivision_diff / 2)
      ),
    ) |>
    mutate(VoA_Ranking_Ovr = dense_rank(desc(VoA_Rating_Ovr))) |>
    arrange(desc(VoA_Rating_Ovr))

  write_csv(
    FinalAllD1VoA,
    here(
      "Data",
      paste0("VoA", year),
      paste0("AllD1", year, week_text, cfb_week, "VoA.csv")
    )
  )
}
