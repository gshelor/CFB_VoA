##### Vortex of Projection version 1.2.0 #####
### This script will take the most recent csv from the VoA
### It will take the VoA Ratings from that csv and use them to project scoring margins for upcoming FBS games
### switched to pulling games directly from CFBD API using official CFBD python library because for some reason cfbfastR doesn't pull all the games
## but I like the gt R package more than the gt python library so I save the dataframes there
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


##### Reading in data based on week #####
if (as.integer(upcoming) == 1) {
  ### reading in full season's game projections
  FullSeason_Games <- read_parquet(here(
    "Data",
    paste0("VoA", year),
    "Projections",
    paste0("FullSeason", year, "GamesPreds.parquet")
  ))

  upcoming_games_df <- read_parquet(
    here(
      "Data",
      paste0("VoA", year),
      "Projections",
      paste0(year, "VoPWeek", upcoming, "Games.parquet")
    )
  )
} else {
  upcoming_games_df <- read_parquet(
    here(
      "Data",
      paste0("VoA", year),
      "Projections",
      paste0(year, "VoPWeek", upcoming, "Games.parquet")
    )
  )
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
    columns = c(home_VoA_rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 3 decimal places
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_rating),
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
    home_VoA_rating = "Home VoA Rating",
    away_VoA_rating = "Away VoA Rating",
    Proj_Winner = "Projected Winner",
    Proj_Margin = "Projected Margin",
    win_prob = "Win Probability"
  ) |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(
    id,
    season,
    week,
    neutral_site,
    home_classification,
    home_conference,
    away_classification,
    away_conference,
    glm_win_prob
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
    columns = c(home_VoA_rating), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have 3 decimal places
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_rating),
    decimals = 3
  ) |>
  fmt_number(
    # Another numeric column
    columns = c(away_VoA_rating),
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
    home_VoA_rating = "Home VoA Rating",
    away_VoA_rating = "Away VoA Rating",
    Proj_Winner = "Projected Winner",
    Proj_Margin = "Projected Margin",
    win_prob = "Win Probability"
  ) |> # Update labels
  cols_move_to_end(columns = "win_prob") |>
  cols_hide(c(
    id,
    season,
    week,
    neutral_site,
    home_classification,
    home_conference,
    away_classification,
    away_conference,
    glm_win_prob
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
