##### FBS-FCS VoA Rating Adjustment #####
### For the 2026 season: In this script, I will read in the VoA ratings from FBS and FCS (calculated separately)
## and I will apply them to previous games and evaluate the impact of D1 subdivision on what the margin actually is compared to what would be expected without an adjustment
### actually
## might just read in the games, build a mixed effects model with home field advantage, team 1 subdivision, team 2 subdivision, and take the coefficient from each subdivision
## win margin would be the target variable
##### loading packages #####
library(pacman)
p_load(here, tidyverse, lme4, arrow, cfbfastR, gt, gtExtras, dtplyr, data.table)

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
  ### writing out current VoA csv to different folder so I can easily read it in for rendering a table on my website
  write_csv(
    FinalAllD1VoA,
    here(
      "Data",
      paste0("CurrentAllD1VoA.csv")
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

  ### writing out current VoA csv to different folder so I can easily read it in for rendering a table on my website
  write_csv(
    FinalAllD1VoA,
    here(
      "Data",
      paste0("CurrentAllD1VoA.csv")
    )
  )
}

# ##### Resume VoA #####
# ### moving resume VoA here so I can do it with the combined ratings so it's not weird when including games between FBS and FCS teams
# ### determining mean VoA Rating of top 8 teams in VoA, comparing how this hypothetical average top 8 team would do given each team's schedule
# ## choosing top 8 because of future playoff expansion which seems likely if not already certain
# ## it really should only be 8 max but whatever, I'm gonna be just fine
# ### Resume VoA only created after week 9 (Week 10 - end of season)
# if (as.integer(cfb_week) > 11) {
#   ### adding column to VoAVariables to hold Resume VoA metric
#   ## placing dummy value for now, will be filled in for loop further down
#   VoAVariables <- VoAVariables |>
#     mutate(Resume_VoA = -999)

#   ### calculating top 8 average since 8 teams make the playoff
#   Top8 <- fread(here(
#     "Data",
#     paste0("VoA", year),
#     paste0("AllD1", year, week_text, as.integer(cfb_week) - 1, "VoA.csv")
#   )) |>
#     filter(VoA_Ranking_Ovr <= 8) #|>
#   # select(
#   #   season,
#   #   school,
#   #   OffVoA_MeanRating,
#   #   DefVoA_MeanRating,
#   #   STVoA_MeanRating
#   # )
#   Top8_off_mean <- mean(Top8$OffVoA_MeanRating)
#   Top8_def_mean <- mean(Top8$DefVoA_MeanRating)
#   Top8_st_mean <- mean(Top8$STVoA_MeanRating)
#   Top8_mean <- Top8_off_mean - Top8_def_mean + Top8_st_mean

#   ### pulling in completed games
#   completed_games <- cfbd_game_info(as.integer(year)) |>
#     filter(
#       home_team %in% VoAVariables$school | away_team %in% VoAVariables$school
#     ) |>
#     select(
#       game_id,
#       season,
#       week,
#       neutral_site,
#       completed,
#       home_team,
#       home_points,
#       away_team,
#       away_points
#     ) |>
#     filter(completed == TRUE)

#   ### using SRS ratings for FCS teams instead of the randomly sampled VoA rating based on
#   ## bottom half of VoA ratings as done during 2022 CFB season
#   FCS <- fread(here(
#     "Data",
#     paste0("VoA", year),
#     paste0("AllD1", year, week_text, as.integer(cfb_week) - 1, "VoA.csv")
#   )) |>
#     filter(team %nin% VoAVariables$school) |>
#     filter(
#       team %in% completed_games$home_team | team %in% completed_games$away_team
#     )

#   ##### Calculating Resume VoA team by team #####
#   for (x in 1:nrow(VoAVariables)) {
#     temp_team <- completed_games |>
#       filter(
#         home_team == VoAVariables$school[x] |
#           away_team == VoAVariables$school[x]
#       ) |>
#       mutate(
#         team = VoAVariables$school[x],
#         team_opp = case_when(
#           home_team == VoAVariables$school[x] ~ away_team,
#           TRUE ~ home_team
#         ),
#         team_VoA_rating = VoAVariables$VoA_Rating_Ovr[x]
#       )
#     ### extracting ratings of FBS opponents
#     temp_teamFBSOpps <- VoAVariables |>
#       filter(school %in% temp_team$team_opp) |>
#       select(school, VoA_Rating_Ovr)
#     ### extracting SRS ratings of FCS opponents
#     temp_teamFCSOpps <- FCS |>
#       filter(team %in% temp_team$team_opp) |>
#       select(school, rating)
#     colnames(temp_teamFCSOpps) <- c("school", "VoA_Rating_Ovr")
#     temp_teamOpps <- rbind(temp_teamFBSOpps, temp_teamFCSOpps)
#     colnames(temp_teamOpps) <- c("team_opp", "opp_VoA_rating")

#     ### adding opponent ratings to main team df
#     temp_team <- full_join(temp_team, temp_teamOpps, by = "team_opp")

#     ### calculating resume score
#     temp_team <- temp_team |>
#       mutate(
#         actual_diff = case_when(
#           home_team == VoAVariables$school[x] ~ home_points - away_points,
#           TRUE ~ away_points - home_points
#         ),
#         projected_diff = case_when(
#           home_team == VoAVariables$school[x] &
#             neutral_site == FALSE ~ (team_VoA_rating + 2) - opp_VoA_rating,
#           away_team == VoAVariables$school[x] &
#             neutral_site == FALSE ~ team_VoA_rating - (opp_VoA_rating + 2),
#           TRUE ~ team_VoA_rating - opp_VoA_rating
#         ),
#         Top8_proj = case_when(
#           home_team == VoAVariables$school[x] &
#             neutral_site == FALSE ~ (Top8_mean + 2) - opp_VoA_rating,
#           TRUE ~ Top8_mean - opp_VoA_rating
#         ),
#         Resume_Score = actual_diff - Top8_proj
#       )

#     ## determining number of losses
#     temp_team_losses <- temp_team |>
#       filter(
#         home_team == VoAVariables$school[x] &
#           home_points < away_points |
#           away_team == VoAVariables$school[x] & away_points < home_points
#       )
#     ## storing overall team Resume Score as vector
#     VoAVariables$Resume_VoA[x] <- sum(temp_team$Resume_Score) -
#       (7 * nrow(temp_team_losses))
#   }

#   VoAVariables <- VoAVariables |>
#     mutate(Resume_VoA_Rank = dense_rank(desc(Resume_VoA)))

#   ### filtering resume top 25 out for table
#   ResumeVoATop25 <- VoAVariables |>
#     select(school, Resume_VoA, Resume_VoA_Rank) |>
#     filter(Resume_VoA_Rank < 26) |>
#     arrange(Resume_VoA_Rank)

#   ### full resume VoA, simplified for table
#   FinalResumeTable <- VoAVariables |>
#     select(school, Resume_VoA, Resume_VoA_Rank) |>
#     arrange(Resume_VoA_Rank)
# } else {
#   print("no Resume VoA until Week 12!")
# }

# ##### Creating Top 25 and Full Tables Arranged by Resume VoA #####
# if (as.integer(cfb_week) > 11) {
#   ## Top 25 Table
#   # adding title and subtitle
#   ResumeVoATop25Table <- ResumeVoATop25 |>
#     gt() |> # use 'gt' to make an awesome table...
#     gt_theme_espn() |>
#     tab_header(
#       title = paste(year, week_text, cfb_week, resume_text, VoA_Top25_text), # ...with this title
#       subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
#     ) |> # and this subtitle
#     ## tab_style(style = cell_fill("bisque"),
#     ##           locations = cells_body()) |>  # add fill color to table
#     fmt_number(
#       # A column (numeric data)
#       columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
#       decimals = 3 # With four decimal places
#     ) |>
#     fmt_number(
#       # Another column (also numeric data)
#       columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
#       decimals = 0 # I want this column to have zero decimal places
#     ) |>
#     data_color(
#       # Update cell colors, testing different color palettes
#       columns = c(Resume_VoA), # ...for dose column
#       fn = scales::col_numeric(
#         # <- bc it's numeric
#         palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
#         domain = c(), # Column scale endpoints
#         reverse = FALSE
#       )
#     ) |>
#     cols_label(
#       Resume_VoA = "Resume VoA Rating",
#       Resume_VoA_Rank = "Resume VoA Ranking"
#     ) |> # Update labels
#     cols_move_to_end(columns = "Resume_VoA") |>
#     # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
#     tab_footnote(
#       footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
#     )

#   ### Full 134 teams table
#   # adding title and subtitle
#   Resume_VoA_Table <- FinalResumeTable |>
#     gt() |> # use 'gt' to make an awesome table...
#     gt_theme_espn() |>
#     tab_header(
#       title = paste(year, week_text, cfb_week, resume_text, VoA_text), # ...with this title
#       subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
#     ) |> # and this subtitle
#     ##tab_style(style = cell_fill("bisque"),
#     ##        locations = cells_body()) |>  # add fill color to table
#     fmt_number(
#       # A column (numeric data)
#       columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
#       decimals = 3 # With four decimal places
#     ) |>
#     fmt_number(
#       # Another column (also numeric data)
#       columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
#       decimals = 0 # I want this column to have zero decimal places
#     ) |>
#     data_color(
#       # Update cell colors, testing different color palettes
#       columns = c(Resume_VoA), # ...for dose column
#       fn = scales::col_numeric(
#         # <- bc it's numeric
#         palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
#         domain = c(), # Column scale endpoints
#         reverse = FALSE
#       )
#     ) |>
#     cols_label(
#       Resume_VoA = "Resume VoA Rating",
#       Resume_VoA_Rank = "Resume VoA Rank"
#     ) |> # Update labels
#     cols_move_to_end(columns = "Resume_VoA") |>
#     # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
#     tab_footnote(
#       footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
#     )
# } else if (as.integer(cfb_week) == 20) {
#   ## Top 25 Table
#   # adding title and subtitle
#   ResumeVoATop25Table <- ResumeVoATop25 |>
#     gt() |> # use 'gt' to make an awesome table...
#     gt_theme_espn() |>
#     tab_header(
#       title = paste(year, Postseason_text, resume_text, VoA_Top25_text), # ...with this title
#       subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
#     ) |> # and this subtitle
#     ## tab_style(style = cell_fill("bisque"),
#     ##           locations = cells_body()) |>  # add fill color to table
#     fmt_number(
#       # A column (numeric data)
#       columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
#       decimals = 3 # With four decimal places
#     ) |>
#     fmt_number(
#       # Another column (also numeric data)
#       columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
#       decimals = 0 # I want this column to have zero decimal places
#     ) |>
#     data_color(
#       # Update cell colors, testing different color palettes
#       columns = c(Resume_VoA), # ...for dose column
#       fn = scales::col_numeric(
#         # <- bc it's numeric
#         palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
#         domain = c(), # Column scale endpoints
#         reverse = FALSE
#       )
#     ) |>
#     cols_label(
#       Resume_VoA = "Resume VoA Rating",
#       Resume_VoA_Rank = "Resume VoA Ranking"
#     ) |> # Update labels
#     cols_move_to_end(columns = "Resume_VoA") |>
#     # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
#     tab_footnote(
#       footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
#     )

#   ### Full 134 teams table
#   ### adding title and subtitle
#   Resume_VoA_Table <- FinalResumeTable |>
#     gt() |> # use 'gt' to make an awesome table...
#     gt_theme_espn() |>
#     tab_header(
#       title = paste(year, Postseason_text, resume_text, VoA_text), # ...with this title
#       subtitle = "Supremely Excellent Yet Salaciously Godlike And Infallibly Magnificent Vortex of Accuracy"
#     ) |> # and this subtitle
#     ##tab_style(style = cell_fill("bisque"),
#     ##        locations = cells_body()) |>  # add fill color to table
#     fmt_number(
#       # A column (numeric data)
#       columns = c(Resume_VoA), # What column variable? FinalVoATop25$VoA_Rating
#       decimals = 3 # With four decimal places
#     ) |>
#     fmt_number(
#       # Another column (also numeric data)
#       columns = c(Resume_VoA_Rank), # What column variable? FinalVoATop25$VoA_Ranking
#       decimals = 0 # I want this column to have zero decimal places
#     ) |>
#     data_color(
#       # Update cell colors, testing different color palettes
#       columns = c(Resume_VoA), # ...for dose column
#       fn = scales::col_numeric(
#         # <- bc it's numeric
#         palette = brewer.pal(11, "RdYlGn"), # A color scheme (gradient)
#         domain = c(), # Column scale endpoints
#         reverse = FALSE
#       )
#     ) |>
#     cols_label(
#       Resume_VoA = "Resume VoA Rating",
#       Resume_VoA_Rank = "Resume VoA Rank"
#     ) |> # Update labels
#     cols_move_to_end(columns = "Resume_VoA") |>
#     # cols_hide(c(conference, CFB_Week, VoA_Output)) |>
#     tab_footnote(
#       footnote = "Table by @gshelor, data from CFB Data API via cfbfastR"
#     )
# } else {
#   print("No Resume VoA until Week 12!")
# }

## Resume VoA not produced until Week 12
# if (as.integer(cfb_week) > 11) {
#   ### Resume tables
#   ResumeVoATop25Table
#   ResumeVoATop25Table |>
#     gtsave(
#       resumetop25_file_pathway,
#       expand = 5,
#       path = output_dir
#     )
#   Resume_VoA_Table
#   Resume_VoA_Table |>
#     gtsave(
#       resumefulltable_file_pathway,
#       expand = 5,
#       path = output_dir
#     )
# } else {
#   print("No Resume VoA tables until week 12!")
# }
