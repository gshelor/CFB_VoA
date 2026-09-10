##### Functions for preparing or modeling CFB_VoA #####

library(pacman)
p_load(cfbfastR, tidyverse, here, data.table, parallel, cfbplotR, lme4)

create_voavarstrain_df <- function(PY, teams_df, pbp_df) {
  VoAVars <- teams_df |>
    filter(school %in% pbp_df$home & school %in% pbp_df$away) |>
    select(
      team_id,
      school,
      conference,
      division,
      classification,
      state,
      latitude,
      longitude,
      elevation
    ) |>
    mutate(season = as.integer(PY), .before = 1) #|>
  ### adding columns which will be filled in later using pbp data
  # mutate(
  #   first_downs_pg = 0,
  #   xpts_pg = 0,
  #   xpts_allowed_pg = 0,
  #   st_ppg = 0,
  #   st_ppg_allowed = 0,
  # )
  ### returning VoA Variables df, which have stat values filled in for each team later
  ### gathering recruiting info
  Recruit <- cfbd_recruiting_team(as.integer(PY) + 1) |>
    rename(school = team, recruit_pts = points) |>
    select(school, recruit_pts) |>
    mutate(recruit_pts = case_when(is.na(recruit_pts) ~ 0, TRUE ~ recruit_pts))

  ### joining recruit pts to VoAVariables
  VoAVars <- VoAVars |>
    left_join(Recruit, by = "school") |>
    mutate(recruit_pts = case_when(is.na(recruit_pts) ~ 0, TRUE ~ recruit_pts))
  return(VoAVars)
}

### function to create VoAVariables df that will be used for inference/generating current ratings
create_voavars_df <- function(year, week_num) {
  if (week_num == 0) {
    ##### Preseason VoAVariables df creation #####
    ### filtering out teams that haven't played D1 football for the last 3 years so I'm only dealing with teams that have reliable stats
    VoA_df <- D1Teams |>
      filter(
        school %in%
          D1Teams_PY1$school &
          school %in% D1Teams_PY2$school &
          school %in% D1Teams_PY3$school &
          school %in% D1Teams$school
      ) |>
      select(
        team_id,
        school,
        conference,
        division,
        classification,
        state,
        latitude,
        longitude,
        elevation
      ) |>
      mutate(season = as.integer(year), .before = 1) #|>
    ### adding columns which will be filled in later using pbp data
    # mutate(
    #   first_downs_pg_PY3 = 0,
    #   xpts_pg_PY3 = 0,
    #   xpts_allowed_pg_PY3 = 0,
    #   ### PY2 columns
    #   first_downs_pg_PY2 = 0,
    #   penalty_yds_pg_PY2 = 0,
    #   yards_per_penalty_PY2 = 0,
    #   xpts_pg_PY2 = 0,
    #   xpts_allowed_pg_PY2 = 0,
    #   st_ppg_PY2 = 0,
    #   st_ppg_allowed_PY2 = 0,
    #   ### PY1 columns
    #   first_downs_pg_PY1 = 0,
    #   penalty_yds_pg_PY1 = 0,
    #   yards_per_penalty_PY1 = 0,
    # )

    ### gathering recruiting info
    Recruit_PY1 <- cfbd_recruiting_team(as.integer(year)) |>
      rename(school = team, recruit_pts_PY1 = points) |>
      select(school, recruit_pts_PY1) |>
      mutate(
        recruit_pts_PY1 = case_when(
          is.na(recruit_pts_PY1) ~ 0,
          TRUE ~ recruit_pts_PY1
        )
      )

    Recruit_PY2 <- cfbd_recruiting_team(as.integer(year) - 1) |>
      rename(school = team, recruit_pts_PY2 = points) |>
      select(school, recruit_pts_PY2) |>
      mutate(
        recruit_pts_PY2 = case_when(
          is.na(recruit_pts_PY2) ~ 0,
          TRUE ~ recruit_pts_PY2
        )
      )
    Recruit_PY3 <- cfbd_recruiting_team(as.integer(year) - 2) |>
      rename(school = team, recruit_pts_PY3 = points) |>
      select(school, recruit_pts_PY3) |>
      mutate(
        recruit_pts_PY3 = case_when(
          is.na(recruit_pts_PY3) ~ 0,
          TRUE ~ recruit_pts_PY3
        )
      )

    ### joining recruit pts to VoAVariables
    VoA_df <- VoA_df |>
      left_join(Recruit_PY1, by = "school") |>
      left_join(Recruit_PY2, by = "school") |>
      left_join(Recruit_PY3, by = "school") |>
      mutate(
        recruit_pts_PY1 = case_when(
          is.na(recruit_pts_PY1) ~ 0,
          TRUE ~ recruit_pts_PY1
        ),
        recruit_pts_PY2 = case_when(
          is.na(recruit_pts_PY2) ~ 0,
          TRUE ~ recruit_pts_PY2
        ),
        recruit_pts_PY3 = case_when(
          is.na(recruit_pts_PY3) ~ 0,
          TRUE ~ recruit_pts_PY3
        )
      )
  } else {
    ##### Week 1-end of season VoAVariables df creation #####
    VoA_df <- D1Teams |>
      filter(school %in% PreseasonVoA$school) |>
      select(
        team_id,
        school,
        conference,
        division,
        classification,
        state,
        latitude,
        longitude,
        elevation
      ) |>
      mutate(season = as.integer(year), .before = 1) #|>
    ### adding columns which will be filled in later using pbp data
    ### using group_bys and left_joins now, can probably delete this too
    # mutate(
    #   int_pct = 0,
    #   penalty_yds_pg = 0,
    #   yards_per_penalty = 0,
    #   xpts_pg = 0,
    #   xpts_allowed_pg = 0,
    # )

    ### gathering recruiting info
    Recruit_PY1 <- cfbd_recruiting_team(as.integer(year)) |>
      rename(school = team, recruit_pts_PY1 = points) |>
      select(school, recruit_pts_PY1) |>
      mutate(
        recruit_pts_PY1 = case_when(
          is.na(recruit_pts_PY1) ~ 0,
          TRUE ~ recruit_pts_PY1
        )
      )

    Recruit_PY2 <- cfbd_recruiting_team(as.integer(year) - 1) |>
      rename(school = team, recruit_pts_PY2 = points) |>
      select(school, recruit_pts_PY2) |>
      mutate(
        recruit_pts_PY2 = case_when(
          is.na(recruit_pts_PY2) ~ 0,
          TRUE ~ recruit_pts_PY2
        )
      )

    ### joining recruit pts to VoAVariables
    VoA_df <- VoA_df |>
      left_join(Recruit_PY1, by = "school") |>
      left_join(Recruit_PY2, by = "school") |>
      mutate(
        recruit_pts_PY1 = case_when(
          is.na(recruit_pts_PY1) ~ 0,
          TRUE ~ recruit_pts_PY1
        ),
        recruit_pts_PY2 = case_when(
          is.na(recruit_pts_PY2) ~ 0,
          TRUE ~ recruit_pts_PY2
        )
      )
  }

  return(VoA_df)
}

### function to take main PBP dataset for a year and create the subsets that I use to calculate stats and opponent-adjusted stats
## not gonna do this, probably
# create_pbp_subsets <- function(pbp_df) {}

### function which calculates stats directly from PBP data
extract_pbp_stats <- function(
  VoA_df,
  rushpass_plays,
  success_plays,
  ThirdDowns,
  FourthDowns,
  passplays,
  rushplays,
  scoringopp_plays,
  turnovers,
  scoringplays,
  FGs,
  # Punts,
  # Kickoffs,
  # XPts,
  STPlays
) {
  ### offensive and defensive stats
  ### Metrics from rushpass_plays
  off_rushpass_summary <- rushpass_plays |>
    group_by(school = pos_team) |>
    summarize(
      off_plays = n(),
      off_ypp = mean(yards_gained, na.rm = TRUE),
      off_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_epa = mean(epa_ppa_mean, na.rm = TRUE),
      off_plays_pg = n() / n_distinct(game_id),
      # Pre-calculating unique denominators used elsewhere
      games = n_distinct(game_id)
    )

  off_standard_downs_summary <- rushpass_plays |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_standard_downs_epa = mean(epa_ppa_mean, na.rm = TRUE),
      off_standard_downs_success_rate = mean(success, na.rm = TRUE),
      off_standard_downs_explosiveness = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_passing_downs_summary <- rushpass_plays |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = pos_team) |>
    summarize(
      off_passing_downs_epa = mean(epa_ppa_mean, na.rm = TRUE),
      off_passing_downs_success_rate = mean(success, na.rm = TRUE),
      off_passing_downs_explosiveness = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_rushpass_summary <- rushpass_plays |>
    group_by(school = def_pos_team) |>
    summarize(
      def_plays = n(),
      def_ypp = mean(yards_gained, na.rm = TRUE),
      def_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_epa = mean(epa_ppa_mean, na.rm = TRUE),
      def_plays_pg = n() / n_distinct(game_id)
    )

  def_standard_downs_summary <- rushpass_plays |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_standard_downs_epa = mean(epa_ppa_mean, na.rm = TRUE),
      def_standard_downs_success_rate = mean(success, na.rm = TRUE),
      def_standard_downs_explosiveness = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_passing_downs_summary <- rushpass_plays |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_passing_downs_epa = mean(epa_ppa_mean, na.rm = TRUE),
      def_passing_downs_success_rate = mean(success, na.rm = TRUE),
      def_passing_downs_explosiveness = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  # Metrics from success_plays
  off_success_summary <- success_plays |>
    group_by(school = pos_team) |>
    summarize(
      off_success_count = n(),
      off_explosiveness = mean(epa_ppa_mean, na.rm = TRUE)
    )

  def_success_summary <- success_plays |>
    group_by(school = def_pos_team) |>
    summarize(
      def_success_count = n(),
      def_explosiveness = mean(epa_ppa_mean, na.rm = TRUE)
    )

  # Downs conversions
  off_third_summary <- ThirdDowns |>
    group_by(school = pos_team) |>
    summarize(off_third_conv_rate = sum(success, na.rm = TRUE) / n())

  def_third_summary <- ThirdDowns |>
    group_by(school = def_pos_team) |>
    summarize(def_third_conv_rate = sum(success, na.rm = TRUE) / n())

  off_fourth_summary <- FourthDowns |>
    group_by(school = pos_team) |>
    summarize(off_fourth_conv_rate = sum(success, na.rm = TRUE) / n())

  def_fourth_summary <- FourthDowns |>
    group_by(school = def_pos_team) |>
    summarize(def_fourth_conv_rate = sum(success, na.rm = TRUE) / n())

  # Passing metrics
  off_pass_summary <- passplays |>
    group_by(school = pos_team) |>
    summarize(
      off_pass_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_pass_ypa = mean(yards_gained, na.rm = TRUE),
      off_pass_ypr = mean(yards_gained[completion == 1], na.rm = TRUE),
      off_comp_pct = sum(completion, na.rm = TRUE) / n(),
      ### this isn't the true havoc rate since I don't see an obvious way to include pass breakups too but whatever, it'll do I suppose
      off_havoc_total = sum(yards_gained < 0 | turnover == 1) / n(),
      off_pass_epa = mean(epa_ppa_mean, na.rm = TRUE),
      off_pass_success_rate = mean(success, na.rm = TRUE),
      off_pass_explosiveness = mean(epa_ppa_mean[success == 1], na.rm = TRUE)
    )

  def_pass_summary <- passplays |>
    group_by(school = def_pos_team) |>
    summarize(
      def_pass_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_pass_ypa = mean(yards_gained, na.rm = TRUE),
      def_pass_ypr = mean(yards_gained[completion == 1], na.rm = TRUE),
      def_comp_pct = sum(completion, na.rm = TRUE) / n(),
      def_havoc_total = sum(yards_gained < 0 | turnover == 1) / n(),
      def_pass_epa = mean(epa_ppa_mean, na.rm = TRUE),
      def_pass_success_rate = mean(success, na.rm = TRUE),
      def_pass_explosiveness = mean(epa_ppa_mean[success == 1], na.rm = TRUE)
    )

  # Rushing metrics
  off_rush_summary <- rushplays |>
    mutate(
      off_line_yds_count = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_rush_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_rush_ypa = mean(yards_gained, na.rm = TRUE),
      off_stuff_rate = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      off_line_yds = mean(off_line_yds_count),
      off_rush_epa = mean(epa_ppa_mean, na.rm = TRUE),
      off_rush_success_rate = mean(success, na.rm = TRUE),
      off_rush_explosiveness = mean(epa_ppa_mean[success == 1], na.rm = TRUE)
    )

  off_power_rush_plays <- rushplays |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = pos_team) |>
    summarize(off_power_success = sum(yards_gained >= distance) / n())

  def_rush_summary <- rushplays |>
    mutate(
      def_line_yds_count = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_rush_ypg = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_rush_ypa = mean(yards_gained, na.rm = TRUE),
      def_stuff_rate = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      def_line_yds = mean(def_line_yds_count),
      def_rush_epa = mean(epa_ppa_mean, na.rm = TRUE),
      def_rush_success_rate = mean(success, na.rm = TRUE),
      def_rush_explosiveness = mean(epa_ppa_mean[success == 1], na.rm = TRUE)
    )

  def_power_rush_plays <- rushplays |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = def_pos_team) |>
    summarize(def_power_success = sum(yards_gained >= distance) / n())

  # Scoring opportunities (double aggregation handling)
  off_opp_summary <- scoringopp_plays |>
    group_by(school = pos_team, drive_id) |>
    summarize(
      off_opp_pts = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(off_pts_per_opp = mean(off_opp_pts, na.rm = TRUE))

  def_opp_summary <- scoringopp_plays |>
    group_by(school = def_pos_team, drive_id) |>
    summarize(
      def_opp_pts = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(def_pts_per_opp = mean(def_opp_pts, na.rm = TRUE))

  # Turnovers & Points
  off_to_summary <- turnovers |>
    group_by(school = pos_team) |>
    summarize(off_turnovers_pg = n() / n_distinct(game_id))

  def_to_summary <- turnovers |>
    group_by(school = def_pos_team) |>
    summarize(def_turnovers_pg = n() / n_distinct(game_id))

  off_scoring_summary <- scoringplays |>
    group_by(school = pos_team) |>
    summarize(off_pts_scored = sum(new_drive_pts, na.rm = TRUE))

  def_scoring_summary <- scoringplays |>
    group_by(school = def_pos_team) |>
    summarize(def_pts_allowed = sum(new_drive_pts, na.rm = TRUE))

  ### special teams stats
  fg_summary <- FGs |>
    group_by(school = pos_team) |>
    summarize(
      fg_rate = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg = sum(play_type == "Field Goal Good") / n_distinct(game_id)
    )

  fg_allowed_summary <- FGs |>
    group_by(school = def_pos_team) |>
    summarize(
      fg_rate_allowed = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_allowed = sum(play_type == "Field Goal Good") /
        n_distinct(game_id)
    )

  punt_returns_summary <- STPlays |>
    filter(play_type %in% c("Punt", "Punt Return Touchdown")) |>
    group_by(school = real_pos_team) |>
    summarize(
      punt_return_yds = mean(yds_punt_return, na.rm = TRUE),
      punt_return_TDs_count = sum(play_type == "Punt Return Touchdown")
    )

  kickoff_returns_summary <- STPlays |>
    filter(
      play_type %in%
        c("Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff")
    ) |>
    group_by(school = real_pos_team) |>
    summarize(
      kick_return_yds = mean(yds_kickoff_return, na.rm = TRUE),
      kick_return_TDs_count = sum(play_type == "Kickoff Return Touchdown")
    )

  st_off_epa_summary <- STPlays |>
    group_by(school = real_pos_team) |>
    summarize(
      off_st_epa = mean(epa_ppa_mean, na.rm = TRUE),
      st_off_scoring_pts = sum(new_drive_pts[scoring_play == 1], na.rm = TRUE)
    )

  st_def_epa_summary <- STPlays |>
    group_by(school = real_def_pos_team) |>
    summarize(
      def_st_epa = mean(epa_ppa_mean, na.rm = TRUE),
      st_def_scoring_pts = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  punts_allowed <- STPlays |>
    group_by(school = real_def_pos_team) |>
    summarize(
      punt_return_yds_allowed = mean(
        yds_punt_return[play_type %in% c("Punt", "Punt Return Touchdown")],
        na.rm = TRUE
      ),
      punt_return_TDs_allowed_count = sum(
        play_type == "Punt Return Touchdown"
      )
    )
  st_def_plays_detailed <- STPlays |>
    group_by(school = real_def_pos_team) |>
    summarize(
      kick_return_yds_allowed = mean(
        yds_kickoff_return[
          play_type %in%
            c(
              "Kickoff Return Touchdown",
              "Kickoff Return (Offense)",
              "Kickoff"
            )
        ],
        na.rm = TRUE
      ),
      kick_return_TDs_allowed_count = sum(
        play_type == "Kickoff Return Touchdown" & def_pos_team == school
      )
    )

  ### combining dfs with collected summary stats back into VoAVariables
  ### listing summary stats dfs so I can try to join them with reduce and left_join
  SummaryStats_dflist <- list(
    off_rushpass_summary,
    def_rushpass_summary,
    off_standard_downs_summary,
    off_passing_downs_summary,
    def_standard_downs_summary,
    def_passing_downs_summary,
    off_success_summary,
    def_success_summary,
    off_third_summary,
    def_third_summary,
    off_fourth_summary,
    def_fourth_summary,
    off_pass_summary,
    def_pass_summary,
    off_rush_summary,
    off_power_rush_plays,
    def_rush_summary,
    def_power_rush_plays,
    off_opp_summary,
    def_opp_summary,
    off_to_summary,
    def_to_summary,
    off_scoring_summary,
    def_scoring_summary,
    fg_summary,
    fg_allowed_summary,
    punt_returns_summary,
    kickoff_returns_summary,
    st_off_epa_summary,
    st_def_epa_summary,
    punts_allowed,
    st_def_plays_detailed
  )
  SummaryStats_df <- SummaryStats_dflist |>
    reduce(left_join, by = "school")
  VoA_df <- VoA_df |>
    left_join(SummaryStats_df, by = "school") |>
    mutate(
      ### Offensive stats
      off_success_rate = off_success_count / off_plays,
      off_ppg = off_pts_scored / games,
      ### Defensive stats
      def_success_rate = def_success_count / def_plays,
      def_ppg = def_pts_allowed / games,
      ### Special Teams stats
      st_net_epa = off_st_epa - def_st_epa,
      kick_return_TDs = kick_return_TDs_count / games,
      punt_return_TDs = punt_return_TDs_count / games,
      kick_return_TDs_allowed = kick_return_TDs_allowed_count / games,
      punt_return_TDs_allowed = punt_return_TDs_allowed_count / games,
      net_st_ppg = (st_off_scoring_pts - st_def_scoring_pts) / games
    ) |>
    ### removing unwanted columns that were used to calculate different rate stats
    select(
      -ends_with("_count"),
      # -ends_with("_unique_weeks"),
      -ends_with("_unique_games"),
      -ends_with("_plays"),
      # -contains("_st_epa"),
      -contains("_scoring_pts")
    ) |>
    mutate(
      ### getting some net special teams columns, ppg above average columns
      net_punt_return_yds = punt_return_yds -
        punt_return_yds_allowed,
      net_kick_return_yds = kick_return_yds -
        kick_return_yds_allowed,
      net_punt_return_TDs = punt_return_TDs -
        punt_return_TDs_allowed,
      net_kick_return_TDs = kick_return_TDs -
        kick_return_TDs_allowed,
      net_fg_rate = fg_rate - fg_rate_allowed,
      net_fg_made_pg = fg_made_pg - fg_made_pg_allowed,
      off_ppg_aboveavg = off_ppg - mean(off_ppg),
      def_ppg_aboveavg = def_ppg - mean(def_ppg)
    )

  # unwanted_duplicates <- VoA_df |>
  #   select(, ends_with(".x"))
  ### checking to make sure all columns are filled in with actual values and not cancelling out or something
  # zero_cols <- VoA_df %>%
  #   select(where(
  #     ~ is.numeric(.x) &&
  #       min(.x, na.rm = TRUE) == 0 &&
  #       mean(.x, na.rm = TRUE) == 0 &&
  #       max(.x, na.rm = TRUE) == 0
  #   ))

  ##### Creating opponent-adjusted stats #####
  ### EPA/play
  ### subsetting columns for epa/play adjustment
  PBP_EPAAdjustment <- rushpass_plays |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      pos_team_subdivision,
      defense_conference,
      def_pos_team_subdivision,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  epa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision / pos_team_recruit_pts) +
      # (1 | def_pos_team_subdivision / def_team_recruit_pts) +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team), #+
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    # (1 | pos_team_subdivision / offense_conference) +
    # (1 | def_pos_team_subdivision / defense_conference), # +
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    data <- PBP_EPAAdjustment
  )

  PBP_EPAAdjustment <- PBP_EPAAdjustment |>
    mutate(adj_epa_preds = predict(epa_mixed_model, PBP_EPAAdjustment))

  off_adj <- PBP_EPAAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_epa = mean(adj_epa_preds, na.rm = TRUE))
  def_adj <- PBP_EPAAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_epa = mean(adj_epa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_epa = case_when(
  #     classification == "fcs" ~ adj_off_epa - (abs(adj_off_epa) / 2),
  #     TRUE ~ adj_off_epa
  #   ),
  #   adj_def_epa = case_when(
  #     classification == "fcs" ~ adj_def_epa + abs(adj_def_epa),
  #     TRUE ~ adj_def_epa
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(epa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_epa = adj_off_epa + avg_epa,
  #     adj_def_epa = adj_def_epa + avg_epa
  #   )

  ### opponent adjusted plays per game
  PlaysPG_Adjustment <- PBP_EPAAdjustment |>
    group_by(game_id) |>
    summarize(
      home_off_plays = sum(pos_team == home),
      away_off_plays = sum(pos_team == away),
      home_team = unique(home)[1],
      away_team = unique(away)[1],
      home_neutral = unique(home_neutral)[1]
    ) |>
    pivot_longer(
      cols = ends_with("_plays"),
      names_to = "home_away_col_names",
      values_to = "team_plays"
    ) |>
    mutate(
      team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ home_team,
        TRUE ~ away_team
      )),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ away_team,
        TRUE ~ home_team
      )),
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    )

  ### fitting mixed effects model, treating team and opposing team as random effects
  set.seed(802)
  plays_mixed_model <- lmer(
    team_plays ~ hfa + (1 | team) + (1 | opp_team),
    data = PlaysPG_Adjustment
  )

  ### Extract random effects (team adjustments)
  team_effects <- ranef(plays_mixed_model)

  ### Extract offensive adjustments
  off_adj <- as.data.frame(team_effects$team) |>
    rename(adj_off_plays_pg = `(Intercept)`) |>
    mutate(school = rownames(team_effects$team))

  ### extract defensive adjustment
  def_adj <- as.data.frame(team_effects$opp_team) |>
    rename(adj_def_plays_pg = `(Intercept)`) |>
    mutate(school = rownames(team_effects$opp_team))

  ### average plays per game (model intercept)
  avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_plays_pg = adj_off_plays_pg + avg_plays_pg,
      adj_def_plays_pg = adj_def_plays_pg + avg_plays_pg
    )

  ### Explosiveness
  ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
  PBP_ExpAdjustment <- success_plays |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  exp_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_ExpAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_ExpAdjustment <- PBP_ExpAdjustment |>
    mutate(adj_exp_preds = predict(exp_mixed_model, PBP_ExpAdjustment))

  off_adj <- PBP_ExpAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_explosiveness = mean(adj_exp_preds, na.rm = TRUE))
  def_adj <- PBP_ExpAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_explosiveness = mean(adj_exp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_explosiveness = case_when(
  #     classification == "fcs" ~ adj_off_explosiveness -
  #       (abs(adj_off_explosiveness) / 2),
  #     TRUE ~ adj_off_explosiveness
  #   ),
  #   adj_def_explosiveness = case_when(
  #     classification == "fcs" ~ adj_def_explosiveness +
  #       abs(adj_def_explosiveness),
  #     TRUE ~ adj_def_explosiveness
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(exp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_explosiveness = adj_off_explosiveness + avg_explosiveness,
  #     adj_def_explosiveness = adj_def_explosiveness + avg_explosiveness
  #   )

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_PPGAdjustment <- rushpass_plays |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      play_pts_scored,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision / pos_team_recruit_pts) +
      # (1 | def_pos_team_subdivision / def_team_recruit_pts) +
      # # (1 | pos_team_subdivision) +
      # # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # # (1 | offense_conference) +
      # # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team), #+
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_PPGAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_PPGAdjustment <- PBP_PPGAdjustment |>
    mutate(adj_ppg_preds = predict(ppg_mixed_model, PBP_PPGAdjustment))

  off_adj <- PBP_PPGAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_pts_per_play = mean(adj_ppg_preds, na.rm = TRUE))
  def_adj <- PBP_PPGAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_pts_per_play = mean(adj_ppg_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg),
      adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg)
    ) #|>
  # mutate(
  #   adj_off_ppg = case_when(
  #     classification == "fcs" ~ adj_off_pts_per_play *
  #       mean(adj_off_plays_pg) /
  #       2,
  #     TRUE ~ adj_off_pts_per_play *
  #       mean(adj_off_plays_pg)
  #   ),
  #   adj_def_ppg = case_when(
  #     classification == "fcs" ~ adj_def_pts_per_play *
  #       mean(adj_def_plays_pg) *
  #       1.5,
  #     TRUE ~ adj_def_pts_per_play *
  #       mean(adj_def_plays_pg)
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team)) |>
  #   mutate(adj_off_pts_per_play = adj_off_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team)) |>
  #   mutate(adj_def_pts_per_play = adj_def_pts_per_play + avg_ppp)

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg) * 1.25,
  #     adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg) * 1.25
  #   )

  ### yards/play opponent adjustment
  ### subsetting columns for adjustment
  PBP_YPPAdjustment <- rushpass_plays |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team,
      def_pos_team_subdivision,
      yards_gained,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ypp_mixed_model <- lmer(
    yards_gained ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_YPPAdjustment
  )

  PBP_YPPAdjustment <- PBP_YPPAdjustment |>
    mutate(adj_ypp_preds = predict(ypp_mixed_model, PBP_YPPAdjustment))

  off_adj <- PBP_YPPAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_ypp = mean(adj_ypp_preds, na.rm = TRUE))
  def_adj <- PBP_YPPAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_ypp = mean(adj_ypp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_ypp = case_when(
  #     classification == "fcs" ~ adj_off_ypp / 2,
  #     TRUE ~ adj_off_ypp
  #   ),
  #   adj_def_ypp = case_when(
  #     classification == "fcs" ~ adj_def_ypp * 1.5,
  #     TRUE ~ adj_def_ypp
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ypp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ypp = adj_off_ypp + avg_ypp,
  #     adj_def_ypp = adj_def_ypp + avg_ypp
  #   )

  ### Special Teams EPA adjustment
  PBP_STEPAAdjustment <- STPlays |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      real_offense_conference,
      real_defense_conference,
      epa_ppa_mean,
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

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STepa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STEPAAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STEPAAdjustment <- PBP_STEPAAdjustment |>
    mutate(adj_STepa_preds = predict(STepa_mixed_model, PBP_STEPAAdjustment))

  off_adj <- PBP_STEPAAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_epa = mean(adj_STepa_preds, na.rm = TRUE))
  def_adj <- PBP_STEPAAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_epa = mean(adj_STepa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(net_adj_st_epa = adj_off_st_epa - adj_def_st_epa)

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STepa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team))

  # ### average EPA (model intercept)
  # avg_st_epa <- fixef(STepa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_st_epa = adj_off_st_epa + avg_st_epa,
  #     adj_def_st_epa = adj_def_st_epa + avg_st_epa
  #   ) |>
  #   mutate(net_adj_st_epa = adj_off_st_epa - adj_def_st_epa)

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_STPPGAdjustment <- STPlays |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      play_pts_scored,
      real_offense_conference,
      real_defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      real_pos_team = as.factor(real_pos_team),
      real_def_pos_team = as.factor(real_def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STPPGAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STPPGAdjustment <- PBP_STPPGAdjustment |>
    mutate(adj_STppg_preds = predict(STppg_mixed_model, PBP_STPPGAdjustment))

  off_adj <- PBP_STPPGAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_pts_per_play = mean(adj_STppg_preds, na.rm = TRUE))
  def_adj <- PBP_STPPGAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_pts_per_play = mean(adj_STppg_preds, na.rm = TRUE))

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_STppp <- fixef(STppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team)) |>
  #   mutate(adj_off_st_pts_per_play = adj_off_st_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team)) |>
  #   mutate(adj_def_st_pts_per_play = adj_def_st_pts_per_play + avg_STppp)

  ### getting average special teams plays per game for both possessing teams and non-possessing teams
  MeanOffSTPlays_df <- STPlays |>
    group_by(real_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))
  MeanDefSTPlays_df <- STPlays |>
    group_by(real_def_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_st_ppg = adj_off_st_pts_per_play *
        mean(MeanOffSTPlays_df$mean_plays),
      adj_def_st_ppg = adj_def_st_pts_per_play *
        mean(MeanDefSTPlays_df$mean_plays)
    ) |>
    mutate(
      net_adj_st_ppg = adj_off_st_ppg - adj_def_st_ppg,
      ### adding difference columns
      EPA_diff = adj_off_epa - adj_def_epa,
      SuccessRt_diff = off_success_rate - def_success_rate,
      HavocRt_diff = def_havoc_total - off_havoc_total,
      Explosiveness_diff = adj_off_explosiveness -
        adj_def_explosiveness
    )

  ### return VoAVariables object
  return(VoA_df)
}


### function which calculates stats directly from PBP data, but specifically for VoAVariables now (since PY data gets their own columns in one df, instead of different rows for each season)
extract_preseason_pbp_stats <- function(
  VoA_df,
  ### PY3 dfs as inputs
  rushpass_plays_PY3,
  success_plays_PY3,
  ThirdDowns_PY3,
  FourthDowns_PY3,
  passplays_PY3,
  rushplays_PY3,
  scoringopp_plays_PY3,
  turnovers_PY3,
  scoringplays_PY3,
  FGs_PY3,
  # Punts_PY3,
  # Kickoffs_PY3,
  # XPts_PY3,
  STPlays_PY3,
  ### PY2 dfs as inputs
  rushpass_plays_PY2,
  success_plays_PY2,
  ThirdDowns_PY2,
  FourthDowns_PY2,
  passplays_PY2,
  rushplays_PY2,
  scoringopp_plays_PY2,
  turnovers_PY2,
  scoringplays_PY2,
  FGs_PY2,
  # Punts_PY2,
  # Kickoffs_PY2,
  # XPts_PY2,
  STPlays_PY2,
  ### PY1 dfs as inputs
  rushpass_plays_PY1,
  success_plays_PY1,
  ThirdDowns_PY1,
  FourthDowns_PY1,
  passplays_PY1,
  rushplays_PY1,
  scoringopp_plays_PY1,
  turnovers_PY1,
  scoringplays_PY1,
  FGs_PY1,
  # Punts_PY1,
  Kickoffs_PY1,
  # XPts_PY1,
  STPlays_PY1
) {
  ### taking PY1 teams and filtering to get teams that were in FCS last year
  ## going to use it to try to prevent FCS teams from ranking absurdly high in the preseason ratings
  ## it'll be arbitrary but at this point I've tried anything statistical like ridge regression or mixed effects to do it automatically and it's just not working
  ## some of that may be RAM related, but most of it is probably just that I tried things that didn't work and I'm out of ideas
  ## anyway here's wonderwall
  ### D1Teams_PY1 should already be in the environment from the data loading section so I'm hoping it'll just recognize it without me needing to make it a specific argument to the function
  ## if not it'll probably be easy and fine
  PY1_FCS <- D1Teams_PY1 |>
    filter(classification == "fcs")
  PY2_FCS <- D1Teams_PY2 |>
    filter(classification == "fcs")
  PY3_FCS <- D1Teams_PY3 |>
    filter(classification == "fcs")
  ### PY3 stat collection
  ### offensive and defensive stats
  ### Metrics from rushpass_plays
  off_rushpass_summary_PY3 <- rushpass_plays_PY3 |>
    group_by(school = pos_team) |>
    summarize(
      off_plays_PY3 = n(),
      off_ypp_PY3 = mean(yards_gained, na.rm = TRUE),
      off_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      off_plays_pg_PY3 = n() / n_distinct(game_id),
      # Pre-calculating unique denominators used elsewhere
      games_PY3 = n_distinct(game_id)
    )

  off_standard_downs_summary_PY3 <- rushpass_plays_PY3 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_standard_downs_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      off_standard_downs_success_rate_PY3 = mean(success, na.rm = TRUE),
      off_standard_downs_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_passing_downs_summary_PY3 <- rushpass_plays_PY3 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = pos_team) |>
    summarize(
      off_passing_downs_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      off_passing_downs_success_rate_PY3 = mean(success, na.rm = TRUE),
      off_passing_downs_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_rushpass_summary_PY3 <- rushpass_plays_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_plays_PY3 = n(),
      def_ypp_PY3 = mean(yards_gained, na.rm = TRUE),
      def_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      def_plays_pg_PY3 = n() / n_distinct(game_id)
    )

  def_standard_downs_summary_PY3 <- rushpass_plays_PY3 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_standard_downs_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      def_standard_downs_success_rate_PY3 = mean(success, na.rm = TRUE),
      def_standard_downs_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_passing_downs_summary_PY3 <- rushpass_plays_PY3 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_passing_downs_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      def_passing_downs_success_rate_PY3 = mean(success, na.rm = TRUE),
      def_passing_downs_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  # Metrics from success_plays
  off_success_summary_PY3 <- success_plays_PY3 |>
    group_by(school = pos_team) |>
    summarize(
      off_success_count_PY3 = n(),
      off_explosiveness_PY3 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  def_success_summary_PY3 <- success_plays_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_success_count_PY3 = n(),
      def_explosiveness_PY3 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  # Downs conversions
  off_third_summary_PY3 <- ThirdDowns_PY3 |>
    group_by(school = pos_team) |>
    summarize(off_third_conv_rate_PY3 = sum(success, na.rm = TRUE) / n())

  def_third_summary_PY3 <- ThirdDowns_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(def_third_conv_rate_PY3 = sum(success, na.rm = TRUE) / n())

  off_fourth_summary_PY3 <- FourthDowns_PY3 |>
    group_by(school = pos_team) |>
    summarize(off_fourth_conv_rate_PY3 = sum(success, na.rm = TRUE) / n())

  def_fourth_summary_PY3 <- FourthDowns_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(def_fourth_conv_rate_PY3 = sum(success, na.rm = TRUE) / n())

  # Passing metrics
  off_pass_summary_PY3 <- passplays_PY3 |>
    group_by(school = pos_team) |>
    summarize(
      off_pass_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_pass_ypa_PY3 = mean(yards_gained, na.rm = TRUE),
      off_pass_ypr_PY3 = mean(yards_gained[completion == 1], na.rm = TRUE),
      off_comp_pct_PY3 = sum(completion, na.rm = TRUE) / n(),
      ### this isn't the true havoc rate since I don't see an obvious way to include pass breakups too but whatever, it'll do I suppose
      off_havoc_total_PY3 = sum(yards_gained < 0 | turnover == 1) / n(),
      off_pass_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      off_pass_success_rate_PY3 = mean(success, na.rm = TRUE),
      off_pass_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_pass_summary_PY3 <- passplays_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_pass_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_pass_ypa_PY3 = mean(yards_gained, na.rm = TRUE),
      def_pass_ypr_PY3 = mean(yards_gained[completion == 1], na.rm = TRUE),
      def_comp_pct_PY3 = sum(completion, na.rm = TRUE) / n(),
      def_havoc_total_PY3 = sum(yards_gained < 0 | turnover == 1) / n(),
      def_pass_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      def_pass_success_rate_PY3 = mean(success, na.rm = TRUE),
      def_pass_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  ### Rushing metrics
  off_rush_summary_PY3 <- rushplays_PY3 |>
    mutate(
      off_line_yds_count_PY3 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_rush_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_rush_ypa_PY3 = mean(yards_gained, na.rm = TRUE),
      off_stuff_rate_PY3 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      off_line_yds_PY3 = mean(off_line_yds_count_PY3),
      off_rush_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      off_rush_success_rate_PY3 = mean(success, na.rm = TRUE),
      off_rush_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_power_rush_plays_PY3 <- rushplays_PY3 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = pos_team) |>
    summarize(off_power_success_PY3 = sum(yards_gained >= distance) / n())

  def_rush_summary_PY3 <- rushplays_PY3 |>
    mutate(
      def_line_yds_count_PY3 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_rush_ypg_PY3 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_rush_ypa_PY3 = mean(yards_gained, na.rm = TRUE),
      def_stuff_rate_PY3 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      def_line_yds_PY3 = mean(def_line_yds_count_PY3),
      def_rush_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      def_rush_success_rate_PY3 = mean(success, na.rm = TRUE),
      def_rush_explosiveness_PY3 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_power_rush_plays_PY3 <- rushplays_PY3 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = def_pos_team) |>
    summarize(def_power_success_PY3 = sum(yards_gained >= distance) / n())

  ### Scoring opportunities
  off_opp_summary_PY3 <- scoringopp_plays_PY3 |>
    group_by(school = pos_team, drive_id) |>
    summarize(
      off_opp_pts_PY3 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(off_pts_per_opp_PY3 = mean(off_opp_pts_PY3, na.rm = TRUE))

  def_opp_summary_PY3 <- scoringopp_plays_PY3 |>
    group_by(school = def_pos_team, drive_id) |>
    summarize(
      def_opp_pts_PY3 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(def_pts_per_opp_PY3 = mean(def_opp_pts_PY3, na.rm = TRUE))

  ### Turnovers
  off_to_summary_PY3 <- turnovers_PY3 |>
    group_by(school = pos_team) |>
    summarize(off_turnovers_pg_PY3 = n() / n_distinct(game_id))

  def_to_summary_PY3 <- turnovers_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(def_turnovers_pg_PY3 = n() / n_distinct(game_id))

  ### points scored
  off_scoring_summary_PY3 <- scoringplays_PY3 |>
    group_by(school = pos_team) |>
    summarize(off_pts_scored_PY3 = sum(new_drive_pts, na.rm = TRUE))

  def_scoring_summary_PY3 <- scoringplays_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(def_pts_allowed_PY3 = sum(new_drive_pts, na.rm = TRUE))

  ### special teams stats
  fg_summary_PY3 <- FGs_PY3 |>
    group_by(school = pos_team) |>
    summarize(
      fg_rate_PY3 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_PY3 = sum(play_type == "Field Goal Good") / n_distinct(game_id)
    )

  fg_allowed_summary_PY3 <- FGs_PY3 |>
    group_by(school = def_pos_team) |>
    summarize(
      fg_rate_allowed_PY3 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_allowed_PY3 = sum(play_type == "Field Goal Good") /
        n_distinct(game_id)
    )

  punt_returns_summary_PY3 <- STPlays_PY3 |>
    filter(play_type %in% c("Punt", "Punt Return Touchdown")) |>
    group_by(school = real_pos_team) |>
    summarize(
      punt_return_yds_PY3 = mean(yds_punt_return, na.rm = TRUE),
      punt_return_TDs_count_PY3 = sum(play_type == "Punt Return Touchdown")
    )

  kickoff_returns_summary_PY3 <- STPlays_PY3 |>
    filter(
      play_type %in%
        c("Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff")
    ) |>
    group_by(school = real_pos_team) |>
    summarize(
      kick_return_yds_PY3 = mean(yds_kickoff_return, na.rm = TRUE),
      kick_return_TDs_count_PY3 = sum(
        play_type == "Kickoff Return Touchdown"
      )
    )

  st_off_epa_summary_PY3 <- STPlays_PY3 |>
    group_by(school = real_pos_team) |>
    summarize(
      off_st_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      st_off_scoring_pts_PY3 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  st_def_epa_summary_PY3 <- STPlays_PY3 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      def_st_epa_PY3 = mean(epa_ppa_mean, na.rm = TRUE),
      st_def_scoring_pts_PY3 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  punts_allowed_PY3 <- STPlays_PY3 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      punt_return_yds_allowed_PY3 = mean(
        yds_punt_return[play_type %in% c("Punt", "Punt Return Touchdown")],
        na.rm = TRUE
      ),
      punt_return_TDs_allowed_count_PY3 = sum(
        play_type == "Punt Return Touchdown"
      )
    )
  st_def_plays_detailed_PY3 <- STPlays_PY3 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      kick_return_yds_allowed_PY3 = mean(
        yds_kickoff_return[
          play_type %in%
            c(
              "Kickoff Return Touchdown",
              "Kickoff Return (Offense)",
              "Kickoff"
            )
        ],
        na.rm = TRUE
      ),
      kick_return_TDs_allowed_count_PY3 = sum(
        play_type == "Kickoff Return Touchdown" & def_pos_team == school
      )
    )

  ##### PY2 #####
  ### offensive and defensive stats
  ### Metrics from rushpass_plays
  off_rushpass_summary_PY2 <- rushpass_plays_PY2 |>
    group_by(school = pos_team) |>
    summarize(
      off_plays_PY2 = n(),
      off_ypp_PY2 = mean(yards_gained, na.rm = TRUE),
      off_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      off_plays_pg_PY2 = n() / n_distinct(game_id),
      # Pre-calculating unique denominators used elsewhere
      games_PY2 = n_distinct(game_id)
    )

  off_standard_downs_summary_PY2 <- rushpass_plays_PY2 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_standard_downs_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      off_standard_downs_success_rate_PY2 = mean(success, na.rm = TRUE),
      off_standard_downs_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_passing_downs_summary_PY2 <- rushpass_plays_PY2 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = pos_team) |>
    summarize(
      off_passing_downs_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      off_passing_downs_success_rate_PY2 = mean(success, na.rm = TRUE),
      off_passing_downs_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_rushpass_summary_PY2 <- rushpass_plays_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_plays_PY2 = n(),
      def_ypp_PY2 = mean(yards_gained, na.rm = TRUE),
      def_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      def_plays_pg_PY2 = n() / n_distinct(game_id)
    )

  def_standard_downs_summary_PY2 <- rushpass_plays_PY2 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_standard_downs_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      def_standard_downs_success_rate_PY2 = mean(success, na.rm = TRUE),
      def_standard_downs_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_passing_downs_summary_PY2 <- rushpass_plays_PY2 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_passing_downs_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      def_passing_downs_success_rate_PY2 = mean(success, na.rm = TRUE),
      def_passing_downs_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  # Metrics from success_plays
  off_success_summary_PY2 <- success_plays_PY2 |>
    group_by(school = pos_team) |>
    summarize(
      off_success_count_PY2 = n(),
      off_explosiveness_PY2 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  def_success_summary_PY2 <- success_plays_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_success_count_PY2 = n(),
      def_explosiveness_PY2 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  # Downs conversions
  off_third_summary_PY2 <- ThirdDowns_PY2 |>
    group_by(school = pos_team) |>
    summarize(off_third_conv_rate_PY2 = sum(success, na.rm = TRUE) / n())

  def_third_summary_PY2 <- ThirdDowns_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(def_third_conv_rate_PY2 = sum(success, na.rm = TRUE) / n())

  off_fourth_summary_PY2 <- FourthDowns_PY2 |>
    group_by(school = pos_team) |>
    summarize(off_fourth_conv_rate_PY2 = sum(success, na.rm = TRUE) / n())

  def_fourth_summary_PY2 <- FourthDowns_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(def_fourth_conv_rate_PY2 = sum(success, na.rm = TRUE) / n())

  # Passing metrics
  off_pass_summary_PY2 <- passplays_PY2 |>
    group_by(school = pos_team) |>
    summarize(
      off_pass_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_pass_ypa_PY2 = mean(yards_gained, na.rm = TRUE),
      off_pass_ypr_PY2 = mean(yards_gained[completion == 1], na.rm = TRUE),
      off_comp_pct_PY2 = sum(completion, na.rm = TRUE) / n(),
      ### this isn't the true havoc rate since I don't see an obvious way to include pass breakups too but whatever, it'll do I suppose
      off_havoc_total_PY2 = sum(yards_gained < 0 | turnover == 1) / n(),
      off_pass_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      off_pass_success_rate_PY2 = mean(success, na.rm = TRUE),
      off_pass_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_pass_summary_PY2 <- passplays_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_pass_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_pass_ypa_PY2 = mean(yards_gained, na.rm = TRUE),
      def_pass_ypr_PY2 = mean(yards_gained[completion == 1], na.rm = TRUE),
      def_comp_pct_PY2 = sum(completion, na.rm = TRUE) / n(),
      def_havoc_total_PY2 = sum(yards_gained < 0 | turnover == 1) / n(),
      def_pass_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      def_pass_success_rate_PY2 = mean(success, na.rm = TRUE),
      def_pass_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  ### Rushing metrics
  off_rush_summary_PY2 <- rushplays_PY2 |>
    mutate(
      off_line_yds_count_PY2 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_rush_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_rush_ypa_PY2 = mean(yards_gained, na.rm = TRUE),
      off_stuff_rate_PY2 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      off_line_yds_PY2 = mean(off_line_yds_count_PY2),
      off_rush_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      off_rush_success_rate_PY2 = mean(success, na.rm = TRUE),
      off_rush_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_power_rush_plays_PY2 <- rushplays_PY2 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = pos_team) |>
    summarize(off_power_success_PY2 = sum(yards_gained >= distance) / n())

  def_rush_summary_PY2 <- rushplays_PY2 |>
    mutate(
      def_line_yds_count_PY2 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_rush_ypg_PY2 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_rush_ypa_PY2 = mean(yards_gained, na.rm = TRUE),
      def_stuff_rate_PY2 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      def_line_yds_PY2 = mean(def_line_yds_count_PY2),
      def_rush_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      def_rush_success_rate_PY2 = mean(success, na.rm = TRUE),
      def_rush_explosiveness_PY2 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_power_rush_plays_PY2 <- rushplays_PY2 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = def_pos_team) |>
    summarize(def_power_success_PY2 = sum(yards_gained >= distance) / n())

  ### Scoring opportunities
  off_opp_summary_PY2 <- scoringopp_plays_PY2 |>
    group_by(school = pos_team, drive_id) |>
    summarize(
      off_opp_pts_PY2 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(off_pts_per_opp_PY2 = mean(off_opp_pts_PY2, na.rm = TRUE))

  def_opp_summary_PY2 <- scoringopp_plays_PY2 |>
    group_by(school = def_pos_team, drive_id) |>
    summarize(
      def_opp_pts_PY2 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(def_pts_per_opp_PY2 = mean(def_opp_pts_PY2, na.rm = TRUE))

  ### Turnovers
  off_to_summary_PY2 <- turnovers_PY2 |>
    group_by(school = pos_team) |>
    summarize(off_turnovers_pg_PY2 = n() / n_distinct(game_id))

  def_to_summary_PY2 <- turnovers_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(def_turnovers_pg_PY2 = n() / n_distinct(game_id))

  ### points scored
  off_scoring_summary_PY2 <- scoringplays_PY2 |>
    group_by(school = pos_team) |>
    summarize(off_pts_scored_PY2 = sum(new_drive_pts, na.rm = TRUE))

  def_scoring_summary_PY2 <- scoringplays_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(def_pts_allowed_PY2 = sum(new_drive_pts, na.rm = TRUE))

  ### special teams stats
  fg_summary_PY2 <- FGs_PY2 |>
    group_by(school = pos_team) |>
    summarize(
      fg_rate_PY2 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_PY2 = sum(play_type == "Field Goal Good") / n_distinct(game_id)
    )

  fg_allowed_summary_PY2 <- FGs_PY2 |>
    group_by(school = def_pos_team) |>
    summarize(
      fg_rate_allowed_PY2 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_allowed_PY2 = sum(play_type == "Field Goal Good") /
        n_distinct(game_id)
    )

  punt_returns_summary_PY2 <- STPlays_PY2 |>
    filter(play_type %in% c("Punt", "Punt Return Touchdown")) |>
    group_by(school = real_pos_team) |>
    summarize(
      punt_return_yds_PY2 = mean(yds_punt_return, na.rm = TRUE),
      punt_return_TDs_count_PY2 = sum(play_type == "Punt Return Touchdown")
    )

  kickoff_returns_summary_PY2 <- STPlays_PY2 |>
    filter(
      play_type %in%
        c("Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff")
    ) |>
    group_by(school = real_pos_team) |>
    summarize(
      kick_return_yds_PY2 = mean(yds_kickoff_return, na.rm = TRUE),
      kick_return_TDs_count_PY2 = sum(
        play_type == "Kickoff Return Touchdown"
      )
    )

  st_off_epa_summary_PY2 <- STPlays_PY2 |>
    group_by(school = real_pos_team) |>
    summarize(
      off_st_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      st_off_scoring_pts_PY2 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  st_def_epa_summary_PY2 <- STPlays_PY2 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      def_st_epa_PY2 = mean(epa_ppa_mean, na.rm = TRUE),
      st_def_scoring_pts_PY2 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  punts_allowed_PY2 <- STPlays_PY2 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      punt_return_yds_allowed_PY2 = mean(
        yds_punt_return[play_type %in% c("Punt", "Punt Return Touchdown")],
        na.rm = TRUE
      ),
      punt_return_TDs_allowed_count_PY2 = sum(
        play_type == "Punt Return Touchdown"
      )
    )
  st_def_plays_detailed_PY2 <- STPlays_PY2 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      kick_return_yds_allowed_PY2 = mean(
        yds_kickoff_return[
          play_type %in%
            c(
              "Kickoff Return Touchdown",
              "Kickoff Return (Offense)",
              "Kickoff"
            )
        ],
        na.rm = TRUE
      ),
      kick_return_TDs_allowed_count_PY2 = sum(
        play_type == "Kickoff Return Touchdown" & def_pos_team == school
      )
    )

  ##### PY1 #####
  ### offensive and defensive stats
  ### Metrics from rushpass_plays
  off_rushpass_summary_PY1 <- rushpass_plays_PY1 |>
    group_by(school = pos_team) |>
    summarize(
      off_plays_PY1 = n(),
      off_ypp_PY1 = mean(yards_gained, na.rm = TRUE),
      off_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      off_plays_pg_PY1 = n() / n_distinct(game_id),
      # Pre-calculating unique denominators used elsewhere
      games_PY1 = n_distinct(game_id)
    )

  off_standard_downs_summary_PY1 <- rushpass_plays_PY1 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_standard_downs_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      off_standard_downs_success_rate_PY1 = mean(success, na.rm = TRUE),
      off_standard_downs_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_passing_downs_summary_PY1 <- rushpass_plays_PY1 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = pos_team) |>
    summarize(
      off_passing_downs_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      off_passing_downs_success_rate_PY1 = mean(success, na.rm = TRUE),
      off_passing_downs_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_rushpass_summary_PY1 <- rushpass_plays_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_plays_PY1 = n(),
      def_ypp_PY1 = mean(yards_gained, na.rm = TRUE),
      def_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      def_plays_pg_PY1 = n() / n_distinct(game_id)
    )

  def_standard_downs_summary_PY1 <- rushpass_plays_PY1 |>
    filter(
      down == 1 |
        (down == 2 & distance < 7) |
        (down %in% c(3, 4) & distance < 5)
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_standard_downs_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      def_standard_downs_success_rate_PY1 = mean(success, na.rm = TRUE),
      def_standard_downs_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_passing_downs_summary_PY1 <- rushpass_plays_PY1 |>
    filter((down == 2 & distance >= 7) | (down %in% c(3, 4) & distance >= 5)) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_passing_downs_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      def_passing_downs_success_rate_PY1 = mean(success, na.rm = TRUE),
      def_passing_downs_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  # Metrics from success_plays
  off_success_summary_PY1 <- success_plays_PY1 |>
    group_by(school = pos_team) |>
    summarize(
      off_success_count_PY1 = n(),
      off_explosiveness_PY1 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  def_success_summary_PY1 <- success_plays_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_success_count_PY1 = n(),
      def_explosiveness_PY1 = mean(epa_ppa_mean, na.rm = TRUE)
    )

  # Downs conversions
  off_third_summary_PY1 <- ThirdDowns_PY1 |>
    group_by(school = pos_team) |>
    summarize(off_third_conv_rate_PY1 = sum(success, na.rm = TRUE) / n())

  def_third_summary_PY1 <- ThirdDowns_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(def_third_conv_rate_PY1 = sum(success, na.rm = TRUE) / n())

  off_fourth_summary_PY1 <- FourthDowns_PY1 |>
    group_by(school = pos_team) |>
    summarize(off_fourth_conv_rate_PY1 = sum(success, na.rm = TRUE) / n())

  def_fourth_summary_PY1 <- FourthDowns_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(def_fourth_conv_rate_PY1 = sum(success, na.rm = TRUE) / n())

  # Passing metrics
  off_pass_summary_PY1 <- passplays_PY1 |>
    group_by(school = pos_team) |>
    summarize(
      off_pass_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_pass_ypa_PY1 = mean(yards_gained, na.rm = TRUE),
      off_pass_ypr_PY1 = mean(yards_gained[completion == 1], na.rm = TRUE),
      off_comp_pct_PY1 = sum(completion, na.rm = TRUE) / n(),
      ### this isn't the true havoc rate since I don't see an obvious way to include pass breakups too but whatever, it'll do I suppose
      off_havoc_total_PY1 = sum(yards_gained < 0 | turnover == 1) / n(),
      off_pass_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      off_pass_success_rate_PY1 = mean(success, na.rm = TRUE),
      off_pass_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_pass_summary_PY1 <- passplays_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(
      def_pass_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_pass_ypa_PY1 = mean(yards_gained, na.rm = TRUE),
      def_pass_ypr_PY1 = mean(yards_gained[completion == 1], na.rm = TRUE),
      def_comp_pct_PY1 = sum(completion, na.rm = TRUE) / n(),
      def_havoc_total_PY1 = sum(yards_gained < 0 | turnover == 1) / n(),
      def_pass_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      def_pass_success_rate_PY1 = mean(success, na.rm = TRUE),
      def_pass_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  ### Rushing metrics
  off_rush_summary_PY1 <- rushplays_PY1 |>
    mutate(
      off_line_yds_count_PY1 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = pos_team) |>
    summarize(
      off_rush_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      off_rush_ypa_PY1 = mean(yards_gained, na.rm = TRUE),
      off_stuff_rate_PY1 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      off_line_yds_PY1 = mean(off_line_yds_count_PY1),
      off_rush_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      off_rush_success_rate_PY1 = mean(success, na.rm = TRUE),
      off_rush_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  off_power_rush_plays_PY1 <- rushplays_PY1 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = pos_team) |>
    summarize(off_power_success_PY1 = sum(yards_gained >= distance) / n())

  def_rush_summary_PY1 <- rushplays_PY1 |>
    mutate(
      def_line_yds_count_PY1 = case_when(
        yards_gained < 0 ~ yards_gained * 1.2,
        yards_gained >= 0 & yards_gained <= 4 ~ yards_gained,
        yards_gained > 4 & yards_gained <= 10 ~ yards_gained / 2,
        TRUE ~ 0
      )
    ) |>
    group_by(school = def_pos_team) |>
    summarize(
      def_rush_ypg_PY1 = sum(yards_gained, na.rm = TRUE) / n_distinct(game_id),
      def_rush_ypa_PY1 = mean(yards_gained, na.rm = TRUE),
      def_stuff_rate_PY1 = sum(yards_gained <= 0, na.rm = TRUE) / n(),
      def_line_yds_PY1 = mean(def_line_yds_count_PY1),
      def_rush_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      def_rush_success_rate_PY1 = mean(success, na.rm = TRUE),
      def_rush_explosiveness_PY1 = mean(
        epa_ppa_mean[success == 1],
        na.rm = TRUE
      )
    )

  def_power_rush_plays_PY1 <- rushplays_PY1 |>
    filter((down %in% c(3, 4) & distance <= 2) | yards_to_goal <= 2) |>
    group_by(school = def_pos_team) |>
    summarize(def_power_success_PY1 = sum(yards_gained >= distance) / n())

  ### Scoring opportunities
  off_opp_summary_PY1 <- scoringopp_plays_PY1 |>
    group_by(school = pos_team, drive_id) |>
    summarize(
      off_opp_pts_PY1 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(off_pts_per_opp_PY1 = mean(off_opp_pts_PY1, na.rm = TRUE))

  def_opp_summary_PY1 <- scoringopp_plays_PY1 |>
    group_by(school = def_pos_team, drive_id) |>
    summarize(
      def_opp_pts_PY1 = mean(new_drive_pts, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    summarize(def_pts_per_opp_PY1 = mean(def_opp_pts_PY1, na.rm = TRUE))

  ### Turnovers
  off_to_summary_PY1 <- turnovers_PY1 |>
    group_by(school = pos_team) |>
    summarize(off_turnovers_pg_PY1 = n() / n_distinct(game_id))

  def_to_summary_PY1 <- turnovers_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(def_turnovers_pg_PY1 = n() / n_distinct(game_id))

  ### points scored
  off_scoring_summary_PY1 <- scoringplays_PY1 |>
    group_by(school = pos_team) |>
    summarize(off_pts_scored_PY1 = sum(new_drive_pts, na.rm = TRUE))

  def_scoring_summary_PY1 <- scoringplays_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(def_pts_allowed_PY1 = sum(new_drive_pts, na.rm = TRUE))

  ### special teams stats
  fg_summary_PY1 <- FGs_PY1 |>
    group_by(school = pos_team) |>
    summarize(
      fg_rate_PY1 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_PY1 = sum(play_type == "Field Goal Good") / n_distinct(game_id)
    )

  fg_allowed_summary_PY1 <- FGs_PY1 |>
    group_by(school = def_pos_team) |>
    summarize(
      fg_rate_allowed_PY1 = sum(play_type == "Field Goal Good") / n(),
      fg_made_pg_allowed_PY1 = sum(play_type == "Field Goal Good") /
        n_distinct(game_id)
    )

  punt_returns_summary_PY1 <- STPlays_PY1 |>
    filter(play_type %in% c("Punt", "Punt Return Touchdown")) |>
    group_by(school = real_pos_team) |>
    summarize(
      punt_return_yds_PY1 = mean(yds_punt_return, na.rm = TRUE),
      punt_return_TDs_count_PY1 = sum(play_type == "Punt Return Touchdown")
    )

  kickoff_returns_summary_PY1 <- STPlays_PY1 |>
    filter(
      play_type %in%
        c("Kickoff Return Touchdown", "Kickoff Return (Offense)", "Kickoff")
    ) |>
    group_by(school = real_pos_team) |>
    summarize(
      kick_return_yds_PY1 = mean(yds_kickoff_return, na.rm = TRUE),
      kick_return_TDs_count_PY1 = sum(
        play_type == "Kickoff Return Touchdown"
      )
    )

  st_off_epa_summary_PY1 <- STPlays_PY1 |>
    group_by(school = real_pos_team) |>
    summarize(
      off_st_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      st_off_scoring_pts_PY1 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  st_def_epa_summary_PY1 <- STPlays_PY1 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      def_st_epa_PY1 = mean(epa_ppa_mean, na.rm = TRUE),
      st_def_scoring_pts_PY1 = sum(
        new_drive_pts[scoring_play == 1],
        na.rm = TRUE
      )
    )

  punts_allowed_PY1 <- STPlays_PY1 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      punt_return_yds_allowed_PY1 = mean(
        yds_punt_return[play_type %in% c("Punt", "Punt Return Touchdown")],
        na.rm = TRUE
      ),
      punt_return_TDs_allowed_count_PY1 = sum(
        play_type == "Punt Return Touchdown"
      )
    )
  st_def_plays_detailed_PY1 <- STPlays_PY1 |>
    group_by(school = real_def_pos_team) |>
    summarize(
      kick_return_yds_allowed_PY1 = mean(
        yds_kickoff_return[
          play_type %in%
            c(
              "Kickoff Return Touchdown",
              "Kickoff Return (Offense)",
              "Kickoff"
            )
        ],
        na.rm = TRUE
      ),
      kick_return_TDs_allowed_count_PY1 = sum(
        play_type == "Kickoff Return Touchdown" & def_pos_team == school
      )
    )

  ### combining dfs with collected summary stats back into VoAVariables
  ### listing summary stats dfs so I can try to join them with reduce and left_join
  SummaryStats_dflist <- list(
    ### PY3 dfs to be bound together
    off_rushpass_summary_PY3,
    def_rushpass_summary_PY3,
    off_standard_downs_summary_PY3,
    off_passing_downs_summary_PY3,
    def_standard_downs_summary_PY3,
    def_passing_downs_summary_PY3,
    off_success_summary_PY3,
    def_success_summary_PY3,
    off_third_summary_PY3,
    def_third_summary_PY3,
    off_fourth_summary_PY3,
    def_fourth_summary_PY3,
    off_pass_summary_PY3,
    def_pass_summary_PY3,
    off_rush_summary_PY3,
    off_power_rush_plays_PY3,
    def_rush_summary_PY3,
    def_power_rush_plays_PY3,
    off_opp_summary_PY3,
    def_opp_summary_PY3,
    off_to_summary_PY3,
    def_to_summary_PY3,
    off_scoring_summary_PY3,
    def_scoring_summary_PY3,
    fg_summary_PY3,
    fg_allowed_summary_PY3,
    punt_returns_summary_PY3,
    kickoff_returns_summary_PY3,
    st_off_epa_summary_PY3,
    st_def_epa_summary_PY3,
    punts_allowed_PY3,
    st_def_plays_detailed_PY3,
    ### PY2 dfs to be bound together
    off_rushpass_summary_PY2,
    def_rushpass_summary_PY2,
    off_standard_downs_summary_PY2,
    off_passing_downs_summary_PY2,
    def_standard_downs_summary_PY2,
    def_passing_downs_summary_PY2,
    off_success_summary_PY2,
    def_success_summary_PY2,
    off_third_summary_PY2,
    def_third_summary_PY2,
    off_fourth_summary_PY2,
    def_fourth_summary_PY2,
    off_pass_summary_PY2,
    def_pass_summary_PY2,
    off_rush_summary_PY2,
    off_power_rush_plays_PY2,
    def_rush_summary_PY2,
    def_power_rush_plays_PY2,
    off_opp_summary_PY2,
    def_opp_summary_PY2,
    off_to_summary_PY2,
    def_to_summary_PY2,
    off_scoring_summary_PY2,
    def_scoring_summary_PY2,
    fg_summary_PY2,
    fg_allowed_summary_PY2,
    punt_returns_summary_PY2,
    kickoff_returns_summary_PY2,
    st_off_epa_summary_PY2,
    st_def_epa_summary_PY2,
    punts_allowed_PY2,
    st_def_plays_detailed_PY2,
    ### PY1 dfs to be bound together
    off_rushpass_summary_PY1,
    def_rushpass_summary_PY1,
    off_standard_downs_summary_PY1,
    off_passing_downs_summary_PY1,
    def_standard_downs_summary_PY1,
    def_passing_downs_summary_PY1,
    off_success_summary_PY1,
    def_success_summary_PY1,
    off_third_summary_PY1,
    def_third_summary_PY1,
    off_fourth_summary_PY1,
    def_fourth_summary_PY1,
    off_pass_summary_PY1,
    def_pass_summary_PY1,
    off_rush_summary_PY1,
    off_power_rush_plays_PY1,
    def_rush_summary_PY1,
    def_power_rush_plays_PY1,
    off_opp_summary_PY1,
    def_opp_summary_PY1,
    off_to_summary_PY1,
    def_to_summary_PY1,
    off_scoring_summary_PY1,
    def_scoring_summary_PY1,
    fg_summary_PY1,
    fg_allowed_summary_PY1,
    punt_returns_summary_PY1,
    kickoff_returns_summary_PY1,
    st_off_epa_summary_PY1,
    st_def_epa_summary_PY1,
    punts_allowed_PY1,
    st_def_plays_detailed_PY1
  )
  SummaryStats_df <- SummaryStats_dflist |>
    reduce(left_join, by = "school")
  VoA_df <- VoA_df |>
    left_join(SummaryStats_df, by = "school") |>
    mutate(
      ### Offensive stats
      off_success_rate_PY3 = off_success_count_PY3 / off_plays_PY3,
      off_success_rate_PY2 = off_success_count_PY2 / off_plays_PY2,
      off_success_rate_PY1 = off_success_count_PY1 / off_plays_PY1,
      off_ppg_PY3 = off_pts_scored_PY3 / games_PY3,
      off_ppg_PY2 = off_pts_scored_PY2 / games_PY2,
      off_ppg_PY1 = off_pts_scored_PY1 / games_PY1,
      ### Defensive stats
      def_success_rate_PY3 = def_success_count_PY3 / def_plays_PY3,
      def_success_rate_PY2 = def_success_count_PY2 / def_plays_PY2,
      def_success_rate_PY1 = def_success_count_PY1 / def_plays_PY1,
      def_ppg_PY3 = def_pts_allowed_PY3 / games_PY3,
      def_ppg_PY2 = def_pts_allowed_PY2 / games_PY2,
      def_ppg_PY1 = def_pts_allowed_PY1 / games_PY1,
      ### Special Teams stats
      st_net_epa_PY3 = off_st_epa_PY3 - def_st_epa_PY3,
      st_net_epa_PY2 = off_st_epa_PY2 - def_st_epa_PY2,
      st_net_epa_PY1 = off_st_epa_PY1 - def_st_epa_PY1,
      kick_return_TDs_PY3 = kick_return_TDs_count_PY3 / games_PY3,
      kick_return_TDs_PY2 = kick_return_TDs_count_PY2 / games_PY2,
      kick_return_TDs_PY1 = kick_return_TDs_count_PY1 / games_PY1,
      punt_return_TDs_PY3 = punt_return_TDs_count_PY3 / games_PY3,
      punt_return_TDs_PY2 = punt_return_TDs_count_PY2 / games_PY2,
      punt_return_TDs_PY1 = punt_return_TDs_count_PY1 / games_PY1,
      kick_return_TDs_allowed_PY3 = kick_return_TDs_allowed_count_PY3 /
        games_PY3,
      kick_return_TDs_allowed_PY2 = kick_return_TDs_allowed_count_PY2 /
        games_PY2,
      kick_return_TDs_allowed_PY1 = kick_return_TDs_allowed_count_PY1 /
        games_PY1,
      punt_return_TDs_allowed_PY3 = punt_return_TDs_allowed_count_PY3 /
        games_PY3,
      punt_return_TDs_allowed_PY2 = punt_return_TDs_allowed_count_PY2 /
        games_PY2,
      punt_return_TDs_allowed_PY1 = punt_return_TDs_allowed_count_PY1 /
        games_PY1,
      net_st_ppg_PY3 = (st_off_scoring_pts_PY3 - st_def_scoring_pts_PY3) /
        games_PY3,
      net_st_ppg_PY2 = (st_off_scoring_pts_PY2 - st_def_scoring_pts_PY2) /
        games_PY2,
      net_st_ppg_PY1 = (st_off_scoring_pts_PY1 - st_def_scoring_pts_PY1) /
        games_PY1
    ) |>
    ### removing unwanted columns that were used to calculate different rate stats
    select(
      -ends_with("_count_PY3"),
      -ends_with("_count_PY2"),
      -ends_with("_count_PY1"),
      # -ends_with("_unique_weeks"),
      -ends_with("_unique_games"),
      -ends_with("_plays_PY3"),
      -ends_with("_plays_PY2"),
      -ends_with("_plays_PY1"),
      # -contains("_st_epa"),
      -contains("_scoring_pts_PY3"),
      -contains("_scoring_pts_PY2"),
      -contains("_scoring_pts_PY1")
    ) |>
    mutate(
      ### getting some net special teams columns, ppg above average columns
      net_punt_return_yds_PY3 = punt_return_yds_PY3 -
        punt_return_yds_allowed_PY3,
      net_punt_return_yds_PY2 = punt_return_yds_PY2 -
        punt_return_yds_allowed_PY2,
      net_punt_return_yds_PY1 = punt_return_yds_PY1 -
        punt_return_yds_allowed_PY1,
      net_kick_return_yds_PY3 = kick_return_yds_PY3 -
        kick_return_yds_allowed_PY3,
      net_kick_return_yds_PY2 = kick_return_yds_PY2 -
        kick_return_yds_allowed_PY2,
      net_kick_return_yds_PY1 = kick_return_yds_PY1 -
        kick_return_yds_allowed_PY1,
      net_punt_return_TDs_PY3 = punt_return_TDs_PY3 -
        punt_return_TDs_allowed_PY3,
      net_punt_return_TDs_PY2 = punt_return_TDs_PY2 -
        punt_return_TDs_allowed_PY2,
      net_punt_return_TDs_PY1 = punt_return_TDs_PY1 -
        punt_return_TDs_allowed_PY1,
      net_kick_return_TDs_PY3 = kick_return_TDs_PY3 -
        kick_return_TDs_allowed_PY3,
      net_kick_return_TDs_PY2 = kick_return_TDs_PY2 -
        kick_return_TDs_allowed_PY2,
      net_kick_return_TDs_PY1 = kick_return_TDs_PY1 -
        kick_return_TDs_allowed_PY1,
      net_fg_rate_PY3 = fg_rate_PY3 - fg_rate_allowed_PY3,
      net_fg_rate_PY2 = fg_rate_PY2 - fg_rate_allowed_PY2,
      net_fg_rate_PY1 = fg_rate_PY1 - fg_rate_allowed_PY1,
      net_fg_made_pg_PY3 = fg_made_pg_PY3 - fg_made_pg_allowed_PY3,
      net_fg_made_pg_PY2 = fg_made_pg_PY2 - fg_made_pg_allowed_PY2,
      net_fg_made_pg_PY1 = fg_made_pg_PY1 - fg_made_pg_allowed_PY1,
      off_ppg_aboveavg_PY3 = off_ppg_PY3 - mean(off_ppg_PY3),
      off_ppg_aboveavg_PY2 = off_ppg_PY2 - mean(off_ppg_PY2),
      off_ppg_aboveavg_PY1 = off_ppg_PY1 - mean(off_ppg_PY1),
      def_ppg_aboveavg_PY3 = def_ppg_PY3 - mean(def_ppg_PY3),
      def_ppg_aboveavg_PY2 = def_ppg_PY2 - mean(def_ppg_PY2),
      def_ppg_aboveavg_PY1 = def_ppg_PY1 - mean(def_ppg_PY1)
    )

  # unwanted_duplicates <- VoA_df |>
  #   select(, ends_with(".x"))
  ### checking to make sure all columns are filled in with actual values and not cancelling out or something
  # zero_cols <- VoA_df %>%
  #   select(where(
  #     ~ is.numeric(.x) &&
  #       min(.x, na.rm = TRUE) == 0 &&
  #       mean(.x, na.rm = TRUE) == 0 &&
  #       max(.x, na.rm = TRUE) == 0
  #   ))

  ### Creating opponent-adjusted stats
  ##### PY3 mixed models #####
  ### EPA/play
  ### subsetting columns for epa/play adjustment
  PBP_EPAAdjustment <- rushpass_plays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      pos_team_subdivision,
      defense_conference,
      def_pos_team_subdivision,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  epa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team), #+
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    # (1 | pos_team_subdivision / offense_conference) +
    # (1 | def_pos_team_subdivision / defense_conference), # +
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    data <- PBP_EPAAdjustment
  )

  PBP_EPAAdjustment <- PBP_EPAAdjustment |>
    mutate(adj_epa_preds = predict(epa_mixed_model, PBP_EPAAdjustment))

  off_adj <- PBP_EPAAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_epa_PY3 = mean(adj_epa_preds, na.rm = TRUE))
  def_adj <- PBP_EPAAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_epa_PY3 = mean(adj_epa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_epa_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_off_epa_PY3 -
  #       (abs(adj_off_epa_PY3) / 2),
  #     TRUE ~ adj_off_epa_PY3
  #   ),
  #   adj_def_epa_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_def_epa_PY3 +
  #       (abs(adj_def_epa_PY3) / 2),
  #     TRUE ~ adj_def_epa_PY3
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(epa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_epa = adj_off_epa + avg_epa,
  #     adj_def_epa = adj_def_epa + avg_epa
  #   )

  ### opponent adjusted plays per game
  PlaysPG_Adjustment <- PBP_EPAAdjustment |>
    group_by(game_id) |>
    summarize(
      home_off_plays = sum(pos_team == home),
      away_off_plays = sum(pos_team == away),
      home_team = unique(home)[1],
      away_team = unique(away)[1],
      home_neutral = unique(home_neutral)[1]
    ) |>
    pivot_longer(
      cols = ends_with("_plays"),
      names_to = "home_away_col_names",
      values_to = "team_plays"
    ) |>
    mutate(
      team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ home_team,
        TRUE ~ away_team
      )),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ away_team,
        TRUE ~ home_team
      )),
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    )

  ### fitting mixed effects model, treating team and opposing team as random effects
  set.seed(802)
  plays_mixed_model <- lmer(
    team_plays ~ hfa + (1 | team) + (1 | opp_team),
    data = PlaysPG_Adjustment
  )

  ### Extract random effects (team adjustments)
  team_effects <- ranef(plays_mixed_model)

  ### Extract offensive adjustments
  off_adj <- as.data.frame(team_effects$team) |>
    rename(adj_off_plays_pg_PY3 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$team))

  ### extract defensive adjustment
  def_adj <- as.data.frame(team_effects$opp_team) |>
    rename(adj_def_plays_pg_PY3 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$opp_team))

  ### average plays per game (model intercept)
  avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_plays_pg_PY3 = adj_off_plays_pg_PY3 + avg_plays_pg,
      adj_def_plays_pg_PY3 = adj_def_plays_pg_PY3 + avg_plays_pg
    )

  ### Explosiveness
  ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
  PBP_ExpAdjustment <- success_plays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  exp_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_ExpAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_ExpAdjustment <- PBP_ExpAdjustment |>
    mutate(adj_exp_preds = predict(exp_mixed_model, PBP_ExpAdjustment))

  off_adj <- PBP_ExpAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_explosiveness_PY3 = mean(adj_exp_preds, na.rm = TRUE))
  def_adj <- PBP_ExpAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_explosiveness_PY3 = mean(adj_exp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_explosiveness_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_off_explosiveness_PY3 / 2,
  #     TRUE ~ adj_off_explosiveness_PY3
  #   ),
  #   adj_def_explosiveness_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_def_explosiveness_PY3 * 1.5,
  #     TRUE ~ adj_def_explosiveness_PY3
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(exp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_explosiveness = adj_off_explosiveness + avg_explosiveness,
  #     adj_def_explosiveness = adj_def_explosiveness + avg_explosiveness
  #   )

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_PPGAdjustment <- rushpass_plays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      play_pts_scored,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_PPGAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_PPGAdjustment <- PBP_PPGAdjustment |>
    mutate(adj_ppg_preds = predict(ppg_mixed_model, PBP_PPGAdjustment))

  off_adj <- PBP_PPGAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_pts_per_play_PY3 = mean(adj_ppg_preds, na.rm = TRUE))
  def_adj <- PBP_PPGAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_pts_per_play_PY3 = mean(adj_ppg_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_ppg_PY3 = adj_off_pts_per_play_PY3 * mean(adj_off_plays_pg_PY3),
      adj_def_ppg_PY3 = adj_def_pts_per_play_PY3 * mean(adj_def_plays_pg_PY3)
    )
  # mutate(
  #   adj_off_ppg_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_off_pts_per_play_PY3 *
  #       mean(adj_off_plays_pg_PY3) /
  #       2,
  #     TRUE ~ adj_off_pts_per_play_PY3 *
  #       mean(adj_off_plays_pg_PY3)
  #   ),
  #   adj_def_ppg_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_def_pts_per_play_PY3 *
  #       mean(adj_def_plays_pg_PY3) *
  #       1.5,
  #     TRUE ~ adj_def_pts_per_play_PY3 *
  #       mean(adj_def_plays_pg_PY3)
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team)) |>
  #   mutate(adj_off_pts_per_play = adj_off_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team)) |>
  #   mutate(adj_def_pts_per_play = adj_def_pts_per_play + avg_ppp)

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg) * 1.25,
  #     adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg) * 1.25
  #   )

  ### yards/play opponent adjustment
  ### subsetting columns for adjustment
  PBP_YPPAdjustment <- rushpass_plays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team,
      def_pos_team_subdivision,
      yards_gained,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ypp_mixed_model <- lmer(
    yards_gained ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_YPPAdjustment
  )

  PBP_YPPAdjustment <- PBP_YPPAdjustment |>
    mutate(adj_ypp_preds = predict(ypp_mixed_model, PBP_YPPAdjustment))

  off_adj <- PBP_YPPAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_ypp_PY3 = mean(adj_ypp_preds, na.rm = TRUE))
  def_adj <- PBP_YPPAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_ypp_PY3 = mean(adj_ypp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_ypp_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_off_ypp_PY3 / 2,
  #     TRUE ~ adj_off_ypp_PY3
  #   ),
  #   adj_def_ypp_PY3 = case_when(
  #     school %in% PY3_FCS$school ~ adj_def_ypp_PY3 * 1.5,
  #     TRUE ~ adj_def_ypp_PY3
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ypp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ypp = adj_off_ypp + avg_ypp,
  #     adj_def_ypp = adj_def_ypp + avg_ypp
  #   )

  ### Special Teams EPA adjustment
  PBP_STEPAAdjustment <- STPlays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      real_offense_conference,
      real_defense_conference,
      epa_ppa_mean,
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

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STepa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STEPAAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STEPAAdjustment <- PBP_STEPAAdjustment |>
    mutate(adj_STepa_preds = predict(STepa_mixed_model, PBP_STEPAAdjustment))

  off_adj <- PBP_STEPAAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_epa_PY3 = mean(adj_STepa_preds, na.rm = TRUE))
  def_adj <- PBP_STEPAAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_epa_PY3 = mean(adj_STepa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(net_adj_st_epa_PY3 = adj_off_st_epa_PY3 - adj_def_st_epa_PY3)

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STepa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team))

  # ### average EPA (model intercept)
  # avg_st_epa <- fixef(STepa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_st_epa = adj_off_st_epa + avg_st_epa,
  #     adj_def_st_epa = adj_def_st_epa + avg_st_epa
  #   ) |>
  #   mutate(net_adj_st_epa = adj_off_st_epa - adj_def_st_epa)

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_STPPGAdjustment <- STPlays_PY3 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      play_pts_scored,
      real_offense_conference,
      real_defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      real_pos_team = as.factor(real_pos_team),
      real_def_pos_team = as.factor(real_def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STPPGAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STPPGAdjustment <- PBP_STPPGAdjustment |>
    mutate(adj_STppg_preds = predict(STppg_mixed_model, PBP_STPPGAdjustment))

  off_adj <- PBP_STPPGAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_pts_per_play_PY3 = mean(adj_STppg_preds, na.rm = TRUE))
  def_adj <- PBP_STPPGAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_pts_per_play_PY3 = mean(adj_STppg_preds, na.rm = TRUE))

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_STppp <- fixef(STppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team)) |>
  #   mutate(adj_off_st_pts_per_play = adj_off_st_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team)) |>
  #   mutate(adj_def_st_pts_per_play = adj_def_st_pts_per_play + avg_STppp)

  ### getting average special teams plays per game for both possessing teams and non-possessing teams
  MeanOffSTPlays_df <- STPlays_PY3 |>
    group_by(real_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))
  MeanDefSTPlays_df <- STPlays_PY3 |>
    group_by(real_def_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_st_ppg_PY3 = adj_off_st_pts_per_play_PY3 *
        mean(MeanOffSTPlays_df$mean_plays),
      adj_def_st_ppg_PY3 = adj_def_st_pts_per_play_PY3 *
        mean(MeanDefSTPlays_df$mean_plays)
    ) |>
    mutate(
      net_adj_st_ppg_PY3 = adj_off_st_ppg_PY3 - adj_def_st_ppg_PY3,
      ### adding difference columns
      EPA_diff_PY3 = adj_off_epa_PY3 - adj_def_epa_PY3,
      SuccessRt_diff_PY3 = off_success_rate_PY3 - def_success_rate_PY3,
      HavocRt_diff_PY3 = def_havoc_total_PY3 - off_havoc_total_PY3,
      Explosiveness_diff_PY3 = adj_off_explosiveness_PY3 -
        adj_def_explosiveness_PY3
    )

  ##### PY2 mixed models #####
  ### EPA/play
  ### subsetting columns for epa/play adjustment
  PBP_EPAAdjustment <- rushpass_plays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      pos_team_subdivision,
      defense_conference,
      def_pos_team_subdivision,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  epa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team), #+
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    # (1 | pos_team_subdivision / offense_conference) +
    # (1 | def_pos_team_subdivision / defense_conference), # +
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    data <- PBP_EPAAdjustment
  )

  PBP_EPAAdjustment <- PBP_EPAAdjustment |>
    mutate(adj_epa_preds = predict(epa_mixed_model, PBP_EPAAdjustment))

  off_adj <- PBP_EPAAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_epa_PY2 = mean(adj_epa_preds, na.rm = TRUE))
  def_adj <- PBP_EPAAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_epa_PY2 = mean(adj_epa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_epa_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_off_epa_PY2 -
  #       (abs(adj_off_epa_PY2) / 2),
  #     TRUE ~ adj_off_epa_PY2
  #   ),
  #   adj_def_epa_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_def_epa_PY2 +
  #       (abs(adj_def_epa_PY2) / 2),
  #     TRUE ~ adj_def_epa_PY2
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(epa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_epa = adj_off_epa + avg_epa,
  #     adj_def_epa = adj_def_epa + avg_epa
  #   )

  ### opponent adjusted plays per game
  PlaysPG_Adjustment <- PBP_EPAAdjustment |>
    group_by(game_id) |>
    summarize(
      home_off_plays = sum(pos_team == home),
      away_off_plays = sum(pos_team == away),
      home_team = unique(home)[1],
      away_team = unique(away)[1],
      home_neutral = unique(home_neutral)[1]
    ) |>
    pivot_longer(
      cols = ends_with("_plays"),
      names_to = "home_away_col_names",
      values_to = "team_plays"
    ) |>
    mutate(
      team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ home_team,
        TRUE ~ away_team
      )),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ away_team,
        TRUE ~ home_team
      )),
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    )

  ### fitting mixed effects model, treating team and opposing team as random effects
  set.seed(802)
  plays_mixed_model <- lmer(
    team_plays ~ hfa + (1 | team) + (1 | opp_team),
    data = PlaysPG_Adjustment
  )

  ### Extract random effects (team adjustments)
  team_effects <- ranef(plays_mixed_model)

  ### Extract offensive adjustments
  off_adj <- as.data.frame(team_effects$team) |>
    rename(adj_off_plays_pg_PY2 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$team))

  ### extract defensive adjustment
  def_adj <- as.data.frame(team_effects$opp_team) |>
    rename(adj_def_plays_pg_PY2 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$opp_team))

  ### average plays per game (model intercept)
  avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_plays_pg_PY2 = adj_off_plays_pg_PY2 + avg_plays_pg,
      adj_def_plays_pg_PY2 = adj_def_plays_pg_PY2 + avg_plays_pg
    )

  ### Explosiveness
  ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
  PBP_ExpAdjustment <- success_plays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  exp_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_ExpAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_ExpAdjustment <- PBP_ExpAdjustment |>
    mutate(adj_exp_preds = predict(exp_mixed_model, PBP_ExpAdjustment))

  off_adj <- PBP_ExpAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_explosiveness_PY2 = mean(adj_exp_preds, na.rm = TRUE))
  def_adj <- PBP_ExpAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_explosiveness_PY2 = mean(adj_exp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_explosiveness_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_off_explosiveness_PY2 / 2,
  #     TRUE ~ adj_off_explosiveness_PY2
  #   ),
  #   adj_def_explosiveness_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_def_explosiveness_PY2 * 1.5,
  #     TRUE ~ adj_def_explosiveness_PY2
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(exp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_explosiveness = adj_off_explosiveness + avg_explosiveness,
  #     adj_def_explosiveness = adj_def_explosiveness + avg_explosiveness
  #   )

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_PPGAdjustment <- rushpass_plays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      play_pts_scored,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_PPGAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_PPGAdjustment <- PBP_PPGAdjustment |>
    mutate(adj_ppg_preds = predict(ppg_mixed_model, PBP_PPGAdjustment))

  off_adj <- PBP_PPGAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_pts_per_play_PY2 = mean(adj_ppg_preds, na.rm = TRUE))
  def_adj <- PBP_PPGAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_pts_per_play_PY2 = mean(adj_ppg_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_ppg_PY2 = adj_off_pts_per_play_PY2 * mean(adj_off_plays_pg_PY2),
      adj_def_ppg_PY2 = adj_def_pts_per_play_PY2 * mean(adj_def_plays_pg_PY2)
    )
  # mutate(
  #   adj_off_ppg_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_off_pts_per_play_PY2 *
  #       mean(adj_off_plays_pg_PY2) /
  #       2,
  #     TRUE ~ adj_off_pts_per_play_PY2 *
  #       mean(adj_off_plays_pg_PY2)
  #   ),
  #   adj_def_ppg_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_def_pts_per_play_PY2 *
  #       mean(adj_def_plays_pg_PY2) *
  #       1.5,
  #     TRUE ~ adj_def_pts_per_play_PY2 *
  #       mean(adj_def_plays_pg_PY2)
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team)) |>
  #   mutate(adj_off_pts_per_play = adj_off_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team)) |>
  #   mutate(adj_def_pts_per_play = adj_def_pts_per_play + avg_ppp)

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg) * 1.25,
  #     adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg) * 1.25
  #   )

  ### yards/play opponent adjustment
  ### subsetting columns for adjustment
  PBP_YPPAdjustment <- rushpass_plays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team,
      def_pos_team_subdivision,
      yards_gained,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ypp_mixed_model <- lmer(
    yards_gained ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_YPPAdjustment
  )

  PBP_YPPAdjustment <- PBP_YPPAdjustment |>
    mutate(adj_ypp_preds = predict(ypp_mixed_model, PBP_YPPAdjustment))

  off_adj <- PBP_YPPAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_ypp_PY2 = mean(adj_ypp_preds, na.rm = TRUE))
  def_adj <- PBP_YPPAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_ypp_PY2 = mean(adj_ypp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_ypp_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_off_ypp_PY2 / 2,
  #     TRUE ~ adj_off_ypp_PY2
  #   ),
  #   adj_def_ypp_PY2 = case_when(
  #     school %in% PY2_FCS$school ~ adj_def_ypp_PY2 * 1.5,
  #     TRUE ~ adj_def_ypp_PY2
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ypp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ypp = adj_off_ypp + avg_ypp,
  #     adj_def_ypp = adj_def_ypp + avg_ypp
  #   )

  ### Special Teams EPA adjustment
  PBP_STEPAAdjustment <- STPlays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      real_offense_conference,
      real_defense_conference,
      epa_ppa_mean,
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

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STepa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STEPAAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STEPAAdjustment <- PBP_STEPAAdjustment |>
    mutate(adj_STepa_preds = predict(STepa_mixed_model, PBP_STEPAAdjustment))

  off_adj <- PBP_STEPAAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_epa_PY2 = mean(adj_STepa_preds, na.rm = TRUE))
  def_adj <- PBP_STEPAAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_epa_PY2 = mean(adj_STepa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(net_adj_st_epa_PY2 = adj_off_st_epa_PY2 - adj_def_st_epa_PY2)

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STepa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team))

  # ### average EPA (model intercept)
  # avg_st_epa <- fixef(STepa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_st_epa = adj_off_st_epa + avg_st_epa,
  #     adj_def_st_epa = adj_def_st_epa + avg_st_epa
  #   ) |>
  #   mutate(net_adj_st_epa = adj_off_st_epa - adj_def_st_epa)

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_STPPGAdjustment <- STPlays_PY2 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      play_pts_scored,
      real_offense_conference,
      real_defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      real_pos_team = as.factor(real_pos_team),
      real_def_pos_team = as.factor(real_def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    #   (1 | pos_team_subdivision / real_pos_team) +
    #   (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STPPGAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STPPGAdjustment <- PBP_STPPGAdjustment |>
    mutate(adj_STppg_preds = predict(STppg_mixed_model, PBP_STPPGAdjustment))

  off_adj <- PBP_STPPGAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_pts_per_play_PY2 = mean(adj_STppg_preds, na.rm = TRUE))
  def_adj <- PBP_STPPGAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_pts_per_play_PY2 = mean(adj_STppg_preds, na.rm = TRUE))

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_STppp <- fixef(STppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team)) |>
  #   mutate(adj_off_st_pts_per_play = adj_off_st_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team)) |>
  #   mutate(adj_def_st_pts_per_play = adj_def_st_pts_per_play + avg_STppp)

  ### getting average special teams plays per game for both possessing teams and non-possessing teams
  MeanOffSTPlays_df <- STPlays_PY2 |>
    group_by(real_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))
  MeanDefSTPlays_df <- STPlays_PY2 |>
    group_by(real_def_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_st_ppg_PY2 = adj_off_st_pts_per_play_PY2 *
        mean(MeanOffSTPlays_df$mean_plays),
      adj_def_st_ppg_PY2 = adj_def_st_pts_per_play_PY2 *
        mean(MeanDefSTPlays_df$mean_plays)
    ) |>
    mutate(
      net_adj_st_ppg_PY2 = adj_off_st_ppg_PY2 - adj_def_st_ppg_PY2,
      ### adding difference columns
      EPA_diff_PY2 = adj_off_epa_PY2 - adj_def_epa_PY2,
      SuccessRt_diff_PY2 = off_success_rate_PY2 - def_success_rate_PY2,
      HavocRt_diff_PY2 = def_havoc_total_PY2 - off_havoc_total_PY2,
      Explosiveness_diff_PY2 = adj_off_explosiveness_PY2 -
        adj_def_explosiveness_PY2
    )

  ##### PY1 mixed models #####
  ### EPA/play
  ### subsetting columns for epa/play adjustment
  PBP_EPAAdjustment <- rushpass_plays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      pos_team_subdivision,
      defense_conference,
      def_pos_team_subdivision,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  epa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team), #+
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    # (1 | pos_team_subdivision / offense_conference) +
    # (1 | def_pos_team_subdivision / defense_conference), # +
    # (pos_team | pos_team_subdivision) +
    # (def_pos_team | def_pos_team_subdivision),
    data <- PBP_EPAAdjustment
  )

  PBP_EPAAdjustment <- PBP_EPAAdjustment |>
    mutate(adj_epa_preds = predict(epa_mixed_model, PBP_EPAAdjustment))

  off_adj <- PBP_EPAAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_epa_PY1 = mean(adj_epa_preds, na.rm = TRUE))
  def_adj <- PBP_EPAAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_epa_PY1 = mean(adj_epa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_epa_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_off_epa_PY1 -
  #       (abs(adj_off_epa_PY1) / 2),
  #     TRUE ~ adj_off_epa_PY1
  #   ),
  #   adj_def_epa_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_def_epa_PY1 +
  #       (abs(adj_def_epa_PY1) / 2),
  #     TRUE ~ adj_def_epa_PY1
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(epa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_epa <- fixef(epa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_epa = adj_off_epa + avg_epa,
  #     adj_def_epa = adj_def_epa + avg_epa
  #   )

  ### opponent adjusted plays per game
  PlaysPG_Adjustment <- PBP_EPAAdjustment |>
    group_by(game_id) |>
    summarize(
      home_off_plays = sum(pos_team == home),
      away_off_plays = sum(pos_team == away),
      home_team = unique(home)[1],
      away_team = unique(away)[1],
      home_neutral = unique(home_neutral)[1]
    ) |>
    pivot_longer(
      cols = ends_with("_plays"),
      names_to = "home_away_col_names",
      values_to = "team_plays"
    ) |>
    mutate(
      team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ home_team,
        TRUE ~ away_team
      )),
      opp_team = as.factor(case_when(
        home_away_col_names == "home_off_plays" ~ away_team,
        TRUE ~ home_team
      )),
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        home_team == team ~ 1,
        TRUE ~ -1
      ))
    )

  ### fitting mixed effects model, treating team and opposing team as random effects
  set.seed(802)
  plays_mixed_model <- lmer(
    team_plays ~ hfa + (1 | team) + (1 | opp_team),
    data = PlaysPG_Adjustment
  )

  ### Extract random effects (team adjustments)
  team_effects <- ranef(plays_mixed_model)

  ### Extract offensive adjustments
  off_adj <- as.data.frame(team_effects$team) |>
    rename(adj_off_plays_pg_PY1 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$team))

  ### extract defensive adjustment
  def_adj <- as.data.frame(team_effects$opp_team) |>
    rename(adj_def_plays_pg_PY1 = `(Intercept)`) |>
    mutate(school = rownames(team_effects$opp_team))

  ### average plays per game (model intercept)
  avg_plays_pg <- fixef(plays_mixed_model)["(Intercept)"]

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_plays_pg_PY1 = adj_off_plays_pg_PY1 + avg_plays_pg,
      adj_def_plays_pg_PY1 = adj_def_plays_pg_PY1 + avg_plays_pg
    )

  ### Explosiveness
  ### subsetting columns for epa/play (explosiveness, so only EPA/play on successful plays) adjustment
  PBP_ExpAdjustment <- success_plays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      epa_ppa_mean,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  exp_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_ExpAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_ExpAdjustment <- PBP_ExpAdjustment |>
    mutate(adj_exp_preds = predict(exp_mixed_model, PBP_ExpAdjustment))

  off_adj <- PBP_ExpAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_explosiveness_PY1 = mean(adj_exp_preds, na.rm = TRUE))
  def_adj <- PBP_ExpAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_explosiveness_PY1 = mean(adj_exp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_explosiveness_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_off_explosiveness_PY1 / 2,
  #     TRUE ~ adj_off_explosiveness_PY1
  #   ),
  #   adj_def_explosiveness_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_def_explosiveness_PY1 * 1.5,
  #     TRUE ~ adj_def_explosiveness_PY1
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(exp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_explosiveness = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_explosiveness <- fixef(exp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_explosiveness = adj_off_explosiveness + avg_explosiveness,
  #     adj_def_explosiveness = adj_def_explosiveness + avg_explosiveness
  #   )

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_PPGAdjustment <- rushpass_plays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team_subdivision,
      def_pos_team,
      play_pts_scored,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_PPGAdjustment
  )

  ### making predictions with model, grouping by offense and defense to get adjusted values for each unit
  PBP_PPGAdjustment <- PBP_PPGAdjustment |>
    mutate(adj_ppg_preds = predict(ppg_mixed_model, PBP_PPGAdjustment))

  off_adj <- PBP_PPGAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_pts_per_play_PY1 = mean(adj_ppg_preds, na.rm = TRUE))
  def_adj <- PBP_PPGAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_pts_per_play_PY1 = mean(adj_ppg_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_ppg_PY1 = adj_off_pts_per_play_PY1 * mean(adj_off_plays_pg_PY1),
      adj_def_ppg_PY1 = adj_def_pts_per_play_PY1 * mean(adj_def_plays_pg_PY1)
    )
  # mutate(
  #   adj_off_ppg_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_off_pts_per_play_PY1 *
  #       mean(adj_off_plays_pg_PY1) /
  #       2,
  #     TRUE ~ adj_off_pts_per_play_PY1 *
  #       mean(adj_off_plays_pg_PY1)
  #   ),
  #   adj_def_ppg_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_def_pts_per_play_PY1 *
  #       mean(adj_def_plays_pg_PY1) *
  #       1.5,
  #     TRUE ~ adj_def_pts_per_play_PY1 *
  #       mean(adj_def_plays_pg_PY1)
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_ppp <- fixef(ppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team)) |>
  #   mutate(adj_off_pts_per_play = adj_off_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team)) |>
  #   mutate(adj_def_pts_per_play = adj_def_pts_per_play + avg_ppp)

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ppg = adj_off_pts_per_play * mean(adj_off_plays_pg) * 1.25,
  #     adj_def_ppg = adj_def_pts_per_play * mean(adj_def_plays_pg) * 1.25
  #   )

  ### yards/play opponent adjustment
  ### subsetting columns for adjustment
  PBP_YPPAdjustment <- rushpass_plays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    select(
      game_id,
      home,
      away,
      pos_team,
      pos_team_subdivision,
      def_pos_team,
      def_pos_team_subdivision,
      yards_gained,
      offense_conference,
      defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      pos_team = as.factor(pos_team),
      def_pos_team = as.factor(def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  ypp_mixed_model <- lmer(
    yards_gained ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / offense_conference) +
      # (1 | def_pos_team_subdivision / defense_conference) +
      # (1 | offense_conference) +
      # (1 | defense_conference) +
      # (1 | offense_conference / pos_team) +
      # (1 | defense_conference / def_pos_team),
      (1 | pos_team) +
      (1 | def_pos_team),
    # (1 | pos_team_subdivision / pos_team) +
    # (1 | def_pos_team_subdivision / def_pos_team),
    data = PBP_YPPAdjustment
  )

  PBP_YPPAdjustment <- PBP_YPPAdjustment |>
    mutate(adj_ypp_preds = predict(ypp_mixed_model, PBP_YPPAdjustment))

  off_adj <- PBP_YPPAdjustment |>
    filter(pos_team %in% VoA_df$school) |>
    group_by(school = pos_team) |>
    summarize(adj_off_ypp_PY1 = mean(adj_ypp_preds, na.rm = TRUE))
  def_adj <- PBP_YPPAdjustment |>
    filter(def_pos_team %in% VoA_df$school) |>
    group_by(school = def_pos_team) |>
    summarise(adj_def_ypp_PY1 = mean(adj_ypp_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") #|>
  # mutate(
  #   adj_off_ypp_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_off_ypp_PY1 / 2,
  #     TRUE ~ adj_off_ypp_PY1
  #   ),
  #   adj_def_ypp_PY1 = case_when(
  #     school %in% PY1_FCS$school ~ adj_def_ypp_PY1 * 1.5,
  #     TRUE ~ adj_def_ypp_PY1
  #   )
  # )

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(ypp_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$pos_team) |>
  #   rename(adj_off_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$def_pos_team) |>
  #   rename(adj_def_ypp = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$def_pos_team))

  # ### average EPA (model intercept)
  # avg_ypp <- fixef(ypp_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_ypp = adj_off_ypp + avg_ypp,
  #     adj_def_ypp = adj_def_ypp + avg_ypp
  #   )

  ### Special Teams EPA adjustment
  PBP_STEPAAdjustment <- STPlays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      real_offense_conference,
      real_defense_conference,
      epa_ppa_mean,
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

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STepa_mixed_model <- lmer(
    epa_ppa_mean ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STEPAAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STEPAAdjustment <- PBP_STEPAAdjustment |>
    mutate(adj_STepa_preds = predict(STepa_mixed_model, PBP_STEPAAdjustment))

  off_adj <- PBP_STEPAAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_epa_PY1 = mean(adj_STepa_preds, na.rm = TRUE))
  def_adj <- PBP_STEPAAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_epa_PY1 = mean(adj_STepa_preds, na.rm = TRUE))

  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(net_adj_st_epa_PY1 = adj_off_st_epa_PY1 - adj_def_st_epa_PY1)

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STepa_mixed_model)

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team))

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_epa = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team))

  # ### average EPA (model intercept)
  # avg_st_epa <- fixef(STepa_mixed_model)["(Intercept)"]

  # ### combine and join back to VoA_df
  # VoA_df <- VoA_df |>
  #   left_join(off_adj, by = "school") |>
  #   left_join(def_adj, by = "school") |>
  #   mutate(
  #     adj_off_st_epa = adj_off_st_epa + avg_st_epa,
  #     adj_def_st_epa = adj_def_st_epa + avg_st_epa
  #   ) |>
  #   mutate(net_adj_st_epa = adj_off_st_epa - adj_def_st_epa)

  ### ppg
  ## this will initially give me pts/play, then I will multiply it by off/def plays per game when binding to VoA_df
  ### subsetting columns for pts/play adjustment
  PBP_STPPGAdjustment <- STPlays_PY1 |>
    mutate(
      pos_team_subdivision = case_when(
        real_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      ),
      def_pos_team_subdivision = case_when(
        real_def_pos_team == home ~ home_team_division,
        TRUE ~ away_team_division
      )
    ) |>
    mutate(
      real_offense_conference = case_when(
        real_pos_team == pos_team ~ offense_conference,
        TRUE ~ defense_conference
      ),
      real_defense_conference = case_when(
        real_pos_team == pos_team ~ defense_conference,
        TRUE ~ offense_conference
      )
    ) |>
    select(
      game_id,
      home,
      away,
      real_pos_team,
      pos_team_subdivision,
      real_def_pos_team,
      def_pos_team_subdivision,
      play_pts_scored,
      real_offense_conference,
      real_defense_conference,
      home_neutral
    ) |>
    mutate(
      hfa = as.factor(case_when(
        home_neutral == "Neutral" ~ 0,
        ### home team on offense
        real_pos_team == home ~ 1,
        ### home team on defense
        TRUE ~ -1
      )),
      real_pos_team = as.factor(real_pos_team),
      real_def_pos_team = as.factor(real_def_pos_team)
    ) |>
    drop_na()

  ### fitting mixed effects model, treating posessing team and defensive team as random effects
  set.seed(802)
  STppg_mixed_model <- lmer(
    play_pts_scored ~ hfa +
      # (1 | pos_team_subdivision) +
      # (1 | def_pos_team_subdivision) +
      # (1 | pos_team_subdivision / real_offense_conference) +
      # (1 | def_pos_team_subdivision / real_defense_conference) +
      # (1 | real_offense_conference) +
      # (1 | real_defense_conference) +
      # (1 | real_offense_conference / real_pos_team) +
      # (1 | real_defense_conference / real_def_pos_team),
      (1 | real_pos_team) +
      (1 | real_def_pos_team),
    # (1 | pos_team_subdivision / real_pos_team) +
    # (1 | def_pos_team_subdivision / real_def_pos_team),
    data = PBP_STPPGAdjustment
  )

  ### making predictions of adjusted values from mixed LM, grouping by offensive/defensive unit
  PBP_STPPGAdjustment <- PBP_STPPGAdjustment |>
    mutate(adj_STppg_preds = predict(STppg_mixed_model, PBP_STPPGAdjustment))

  off_adj <- PBP_STPPGAdjustment |>
    filter(real_pos_team %in% VoA_df$school) |>
    group_by(school = real_pos_team) |>
    summarize(adj_off_st_pts_per_play_PY1 = mean(adj_STppg_preds, na.rm = TRUE))
  def_adj <- PBP_STPPGAdjustment |>
    filter(real_def_pos_team %in% VoA_df$school) |>
    group_by(school = real_def_pos_team) |>
    summarise(adj_def_st_pts_per_play_PY1 = mean(adj_STppg_preds, na.rm = TRUE))

  ### Extract random effects (team adjustments)
  # team_effects <- ranef(STppg_mixed_model)

  # ### average EPA (model intercept)
  # avg_STppp <- fixef(STppg_mixed_model)["(Intercept)"]

  # ### Extract offensive adjustments
  # off_adj <- as.data.frame(team_effects$real_pos_team) |>
  #   rename(adj_off_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_pos_team)) |>
  #   mutate(adj_off_st_pts_per_play = adj_off_st_pts_per_play + avg_ppp)

  # ### extract defensive adjustment
  # def_adj <- as.data.frame(team_effects$real_def_pos_team) |>
  #   rename(adj_def_st_pts_per_play = `(Intercept)`) |>
  #   mutate(school = rownames(team_effects$real_def_pos_team)) |>
  #   mutate(adj_def_st_pts_per_play = adj_def_st_pts_per_play + avg_STppp)

  ### getting average special teams plays per game for both possessing teams and non-possessing teams
  MeanOffSTPlays_df <- STPlays_PY1 |>
    group_by(real_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))
  MeanDefSTPlays_df <- STPlays_PY1 |>
    group_by(real_def_pos_team) |>
    summarize(mean_plays = n() / length(unique(game_id)))

  ### combine and join back to VoA_df
  VoA_df <- VoA_df |>
    left_join(off_adj, by = "school") |>
    left_join(def_adj, by = "school") |>
    mutate(
      adj_off_st_ppg_PY1 = adj_off_st_pts_per_play_PY1 *
        mean(MeanOffSTPlays_df$mean_plays),
      adj_def_st_ppg_PY1 = adj_def_st_pts_per_play_PY1 *
        mean(MeanDefSTPlays_df$mean_plays)
    ) |>
    mutate(
      net_adj_st_ppg_PY1 = adj_off_st_ppg_PY1 - adj_def_st_ppg_PY1,
      ### adding difference columns
      EPA_diff_PY1 = adj_off_epa_PY1 - adj_def_epa_PY1,
      SuccessRt_diff_PY1 = off_success_rate_PY1 - def_success_rate_PY1,
      HavocRt_diff_PY1 = def_havoc_total_PY1 - off_havoc_total_PY1,
      Explosiveness_diff_PY1 = adj_off_explosiveness_PY1 -
        adj_def_explosiveness_PY1
    )

  ### return VoAVariables object
  return(VoA_df)
}


fix_pbp_subdivision_nas <- function(PBP_df, teams_df) {
  ### filtering out FBS teams since there's only two subdivisions in the teams dfs that will be used in this function
  FBSTeams <- teams_df |>
    filter(classification == "fbs") |>
    select(school, classification)
  ### correcting NAs in subdivision columns in PBP data
  PBP_df <- PBP_df |>
    mutate(
      home_team_division = case_when(
        is.na(home_team_division) == TRUE &
          home %in% FBSTeams$school ~ "fbs",
        is.na(home_team_division) == TRUE &
          home %nin% FBSTeams$school ~ "fcs",
        TRUE ~ home_team_division
      ),
      away_team_division = case_when(
        is.na(away_team_division) == TRUE &
          away %in% FBSTeams$school ~ "fbs",
        is.na(away_team_division) == TRUE &
          away %nin% FBSTeams$school ~ "fcs",
        TRUE ~ away_team_division
      )
    )

  return(PBP_df)
}

rank_voa_cols <- function(VoA_df) {
  VoA_df <- VoA_df |>
    mutate(
      Rank_Comp_Pct = dense_rank(desc(off_comp_pct)),
      Rank_Pass_YPA = dense_rank(desc(off_pass_ypa)),
      Rank_Pass_YPR = dense_rank(desc(off_pass_ypr)),
      # Rank_Int_Pct = dense_rank(int_pct),
      Rank_Rush_YPC = dense_rank(desc(off_rush_ypa)),
      Rank_Turnovers_pg = dense_rank(off_turnovers_pg),
      Rank_third_conv_rate = dense_rank(desc(off_third_conv_rate)),
      Rank_Fourth_conv_rate = dense_rank(desc(off_fourth_conv_rate)),
      # Rank_Penalty_Yds_pg = dense_rank(penalty_yds_pg),
      # Rank_Yds_Per_Penalty = dense_rank(yards_per_penalty),
      Rank_kick_return_yds = dense_rank(desc(kick_return_yds)),
      Rank_punt_return_yds = dense_rank(desc(punt_return_yds)),
      Rank_off_ypg = dense_rank(desc(off_ypg)),
      Rank_off_pass_ypg = dense_rank(desc(off_pass_ypg)),
      Rank_off_rush_ypg = dense_rank(desc(off_rush_ypg)),
      # Rank_First_Downs_pg = dense_rank(desc(first_downs_pg)),
      Rank_Off_YPP = dense_rank(desc(adj_off_ypp)),
      #  Rank_Def_Ints_pg = dense_rank(desc(def_interceptions_pg)),
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
      Rank_Off_Standard_Down_Success_Rt = dense_rank(desc(
        off_standard_downs_success_rate
      )),
      Rank_Off_Standard_Down_Explosiveness = dense_rank(desc(
        off_standard_downs_explosiveness
      )),
      Rank_Off_Pass_Down_EPA = dense_rank(desc(off_passing_downs_epa)),
      Rank_Off_Pass_Down_Success_Rt = dense_rank(desc(
        off_passing_downs_success_rate
      )),
      Rank_Off_Pass_Down_Explosiveness = dense_rank(desc(
        off_passing_downs_explosiveness
      )),
      Rank_Off_Rush_Play_EPA = dense_rank(desc(off_rush_epa)),
      Rank_Off_Rush_Play_Success_Rt = dense_rank(desc(off_rush_success_rate)),
      Rank_Off_Rush_Play_Explosiveness = dense_rank(desc(
        off_rush_explosiveness
      )),
      Rank_Off_Pass_Play_EPA = dense_rank(desc(off_pass_epa)),
      Rank_Off_Pass_Play_Success_Rt = dense_rank(desc(off_pass_success_rate)),
      Rank_Off_Pass_Play_Explosiveness = dense_rank(desc(
        off_pass_explosiveness
      )),
      Rank_Def_EPA = dense_rank(adj_def_epa),
      Rank_Def_Success_Rt = dense_rank(def_success_rate),
      Rank_Def_Explosiveness = dense_rank(adj_def_explosiveness),
      Rank_Def_Pwr_Success = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Line_Yds = dense_rank(def_line_yds),
      # Rank_Def_Second_Lvl_Yds = dense_rank(def_second_lvl_yds),
      # Rank_Def_Open_Field_Yds = dense_rank(def_open_field_yds),
      Rank_Def_Pts_Per_Opp = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total = dense_rank(desc(def_havoc_total)),
      Rank_Def_Standard_Down_EPA = dense_rank(def_standard_downs_epa),
      Rank_Def_Standard_Down_Success_Rt = dense_rank(
        def_standard_downs_success_rate
      ),
      Rank_Def_Standard_Down_Explosiveness = dense_rank(
        def_standard_downs_explosiveness
      ),
      Rank_Def_Pass_Down_EPA = dense_rank(def_passing_downs_epa),
      Rank_Def_Pass_Down_Success_Rt = dense_rank(
        def_passing_downs_success_rate
      ),
      Rank_Def_Pass_Down_Explosiveness = dense_rank(
        def_passing_downs_explosiveness
      ),
      Rank_Def_Rush_Play_EPA = dense_rank(def_rush_epa),
      Rank_Def_Rush_Play_Success_Rt = dense_rank(def_rush_success_rate),
      Rank_Def_Rush_Play_Explosiveness = dense_rank(
        def_rush_explosiveness
      ),
      Rank_Def_Pass_Play_EPA = dense_rank(def_pass_epa),
      Rank_Def_Pass_Play_Success_Rt = dense_rank(def_pass_success_rate),
      Rank_Def_Pass_Play_Explosiveness = dense_rank(
        def_pass_explosiveness
      ),
      # Rank_EPA_diff = dense_rank(desc(EPA_diff)),
      # Rank_SuccessRt_diff = dense_rank(desc(SuccessRt_diff)),
      # Rank_HavocRt_diff = dense_rank(desc(HavocRt_diff)),
      # Rank_Explosiveness_diff = dense_rank(desc(Explosiveness_diff)),
      # Rank_adj_off_epa = dense_rank(desc(adj_off_epa)),
      # Rank_adj_def_epa = dense_rank(adj_def_epa),
      # Rank_adj_epa_diff = dense_rank(desc(adj_epa_diff)),
      Rank_adj_off_explosiveness = dense_rank(desc(adj_off_explosiveness)),
      Rank_adj_def_explosiveness = dense_rank(adj_def_explosiveness),
      # Rank_adj_off_ypp = dense_rank(desc(adj_off_ypp)),
      # Rank_adj_def_ypp = dense_rank(adj_def_ypp),
      Rank_adj_off_ppg = dense_rank(desc(adj_off_ppg)),
      Rank_adj_def_ppg = dense_rank(adj_def_ppg),
      Rank_adj_off_st_epa = dense_rank(desc(adj_off_st_epa)),
      Rank_adj_def_st_epa = dense_rank(adj_def_st_epa),
      Rank_st_net_epa = dense_rank(desc(st_net_epa)),
      ## Extra weighted variables for current year (weighted 2x)
      Rank_Off_YPP_col2 = dense_rank(desc(adj_off_ypp)),
      Rank_Off_EPA_col2 = dense_rank(desc(adj_off_epa)),
      Rank_Off_Success_Rt_col2 = dense_rank(desc(off_success_rate)),
      Rank_Off_Explosiveness_col2 = dense_rank(desc(adj_off_explosiveness)),
      Rank_Off_Pwr_Success_col2 = dense_rank(desc(off_power_success)),
      Rank_Off_Stuff_Rt_col2 = dense_rank(off_stuff_rate),
      Rank_Off_Pts_Per_Opp_col2 = dense_rank(desc(off_pts_per_opp)),
      Rank_Off_Havoc_Total_col2 = dense_rank(off_havoc_total),
      Rank_Off_Standard_Down_EPA_col2 = dense_rank(desc(
        off_standard_downs_epa
      )),
      Rank_Off_Standard_Down_Success_Rt_col2 = dense_rank(desc(
        off_standard_downs_success_rate
      )),
      Rank_Off_Standard_Down_Explosiveness_col2 = dense_rank(desc(
        off_standard_downs_explosiveness
      )),
      Rank_Off_Pass_Down_EPA_col2 = dense_rank(desc(off_passing_downs_epa)),
      Rank_Off_Pass_Down_Success_Rt_col2 = dense_rank(desc(
        off_passing_downs_success_rate
      )),
      Rank_Off_Pass_Down_Explosiveness_col2 = dense_rank(desc(
        off_passing_downs_explosiveness
      )),
      Rank_Off_Rush_Play_EPA_col2 = dense_rank(desc(off_rush_epa)),
      Rank_Off_Rush_Play_Success_Rt_col2 = dense_rank(desc(
        off_rush_success_rate
      )),
      Rank_Off_Rush_Play_Explosiveness_col2 = dense_rank(desc(
        off_rush_explosiveness
      )),
      Rank_Off_Pass_Play_EPA_col2 = dense_rank(desc(off_pass_epa)),
      Rank_Off_Pass_Play_Success_Rt_col2 = dense_rank(desc(
        off_pass_success_rate
      )),
      Rank_Off_Pass_Play_Explosiveness_col2 = dense_rank(desc(
        off_pass_explosiveness
      )),
      Rank_Def_EPA_col2 = dense_rank(adj_def_epa),
      Rank_Def_Success_Rt_col2 = dense_rank(def_success_rate),
      Rank_Def_Explosiveness_col2 = dense_rank(adj_def_explosiveness),
      Rank_Def_Pwr_Success_col2 = dense_rank(def_power_success),
      Rank_Def_Stuff_Rt_col2 = dense_rank(desc(def_stuff_rate)),
      Rank_Def_Pts_Per_Opp_col2 = dense_rank(def_pts_per_opp),
      Rank_Def_Havoc_Total_col2 = dense_rank(desc(def_havoc_total)),
      Rank_Def_Standard_Down_EPA_col2 = dense_rank(def_standard_downs_epa),
      Rank_Def_Standard_Down_Success_Rt_col2 = dense_rank(
        def_standard_downs_success_rate
      ),
      Rank_Def_Standard_Down_Explosiveness_col2 = dense_rank(
        def_standard_downs_explosiveness
      ),
      Rank_Def_Pass_Down_EPA_col2 = dense_rank(def_passing_downs_epa),
      Rank_Def_Pass_Down_Success_Rt_col2 = dense_rank(
        def_passing_downs_success_rate
      ),
      Rank_Def_Pass_Down_Explosiveness_col2 = dense_rank(
        def_passing_downs_explosiveness
      ),
      Rank_Def_Rush_Play_EPA_col2 = dense_rank(def_rush_epa),
      Rank_Def_Rush_Play_Success_Rt_col2 = dense_rank(def_rush_success_rate),
      Rank_Def_Rush_Play_Explosiveness_col2 = dense_rank(
        def_rush_explosiveness
      ),
      Rank_Def_Pass_Play_EPA_col2 = dense_rank(def_pass_epa),
      Rank_Def_Pass_Play_Success_Rt_col2 = dense_rank(
        def_pass_success_rate
      ),
      Rank_Def_Pass_Play_Explosiveness_col2 = dense_rank(
        def_pass_explosiveness
      )
    )

  return(VoA_df)
}

calc_output_conf_avg <- function(VoA_df) {
  ### calculating conference averages
  Conference_Outputs <- VoA_df |>
    group_by(conference) |>
    summarize(Rk_mean = mean(VoA_Output), Rk_median = median(VoA_Output)) |>
    mutate(Conf_Rk = dense_rank(Rk_mean))

  ### binding conference averages to VoAVariables, adding extra couples so it weights extra in the recalculation of VoA_Output
  VoA_df <- left_join(
    VoA_df,
    Conference_Outputs,
    by = "conference"
  ) |>
    select(-VoA_Output) |>
    mutate(
      Conf_Rk_col2 = Conf_Rk,
      Conf_Rk_col3 = Conf_Rk,
      Conf_Rk_col4 = Conf_Rk,
      Conf_Rk_col5 = Conf_Rk,
      Conf_Rk_col6 = Conf_Rk,
      Conf_Rk_col7 = Conf_Rk,
      Conf_Rk_col8 = Conf_Rk,
      Conf_Rk_col9 = Conf_Rk,
      Conf_Rk_col10 = Conf_Rk
    )

  return(VoA_df)
}

fix_VoA_NA_cols <- function(VoA_df) {
  VoA_df <- VoA_df |>
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
      ),
      off_ypp = case_when(
        is.na(off_ypp) ~ weighted_off_ypp,
        TRUE ~ off_ypp
      ),
      def_ypp = case_when(
        is.na(def_ypp) ~ weighted_def_ypp,
        TRUE ~ def_ypp
      ),
      off_ypg = case_when(
        is.na(off_ypg) ~ off_ypg_PY1,
        TRUE ~ off_ypg
      ),
      def_ypg = case_when(
        is.na(def_ypg) ~ def_ypg_PY1,
        TRUE ~ def_ypg
      ),
      off_epa = case_when(
        is.na(off_epa) ~ weighted_off_epa,
        TRUE ~ off_epa
      ),
      def_epa = case_when(
        is.na(def_epa) ~ weighted_def_epa,
        TRUE ~ def_epa
      ),
      off_plays_pg = case_when(
        is.na(off_plays_pg) ~ weighted_off_plays_pg,
        TRUE ~ off_plays_pg
      ),
      def_plays_pg = case_when(
        is.na(def_plays_pg) ~ weighted_def_plays_pg,
        TRUE ~ def_plays_pg
      ),
      games = case_when(
        is.na(games) ~ 0,
        TRUE ~ games
      ),
      off_standard_downs_epa = case_when(
        is.na(off_standard_downs_epa) ~ weighted_off_epa,
        TRUE ~ off_standard_downs_epa
      ),
      def_standard_downs_epa = case_when(
        is.na(def_standard_downs_epa) ~ weighted_def_epa,
        TRUE ~ def_standard_downs_epa
      ),
      off_standard_downs_success_rate = case_when(
        is.na(off_standard_downs_success_rate) ~ weighted_off_success_rate,
        TRUE ~ off_standard_downs_success_rate
      ),
      def_standard_downs_success_rate = case_when(
        is.na(def_standard_downs_success_rate) ~ weighted_def_success_rate,
        TRUE ~ def_standard_downs_success_rate
      ),
      off_standard_downs_explosiveness = case_when(
        is.na(off_standard_downs_explosiveness) ~ weighted_off_explosiveness,
        TRUE ~ off_standard_downs_explosiveness
      ),
      def_standard_downs_explosiveness = case_when(
        is.na(def_standard_downs_explosiveness) ~ weighted_def_explosiveness,
        TRUE ~ def_standard_downs_explosiveness
      ),
      off_passing_downs_epa = case_when(
        is.na(off_passing_downs_epa) ~ weighted_off_epa,
        TRUE ~ off_passing_downs_epa
      ),
      def_passing_downs_epa = case_when(
        is.na(def_passing_downs_epa) ~ weighted_def_epa,
        TRUE ~ def_passing_downs_epa
      ),
      off_passing_downs_success_rate = case_when(
        is.na(off_passing_downs_success_rate) ~ weighted_off_success_rate,
        TRUE ~ off_passing_downs_success_rate
      ),
      def_passing_downs_success_rate = case_when(
        is.na(def_passing_downs_success_rate) ~ weighted_def_success_rate,
        TRUE ~ def_passing_downs_success_rate
      ),
      off_passing_downs_explosiveness = case_when(
        is.na(off_passing_downs_explosiveness) ~ weighted_off_explosiveness,
        TRUE ~ off_passing_downs_explosiveness
      ),
      def_passing_downs_explosiveness = case_when(
        is.na(def_passing_downs_explosiveness) ~ weighted_def_explosiveness,
        TRUE ~ def_passing_downs_explosiveness
      ),
      off_explosiveness = case_when(
        is.na(off_explosiveness) ~ weighted_off_explosiveness,
        TRUE ~ off_explosiveness
      ),
      def_explosiveness = case_when(
        is.na(def_explosiveness) ~ weighted_def_explosiveness,
        TRUE ~ def_explosiveness
      ),
      off_third_conv_rate = case_when(
        is.na(off_third_conv_rate) ~ weighted_off_third_conv_rate,
        TRUE ~ off_third_conv_rate
      ),
      def_third_conv_rate = case_when(
        is.na(def_third_conv_rate) ~ weighted_def_third_conv_rate,
        TRUE ~ def_third_conv_rate
      ),
      off_pass_ypg = case_when(
        is.na(off_pass_ypg) ~ off_pass_ypg_PY1,
        TRUE ~ off_pass_ypg
      ),
      def_pass_ypg = case_when(
        is.na(def_pass_ypg) ~ def_pass_ypg_PY1,
        TRUE ~ def_pass_ypg
      ),
      off_pass_ypa = case_when(
        is.na(off_pass_ypa) ~ off_pass_ypa_PY1,
        TRUE ~ off_pass_ypa
      ),
      def_pass_ypa = case_when(
        is.na(def_pass_ypa) ~ def_pass_ypa_PY1,
        TRUE ~ def_pass_ypa
      ),
      off_pass_ypr = case_when(
        is.na(off_pass_ypr) ~ off_pass_ypr_PY1,
        TRUE ~ off_pass_ypr
      ),
      def_pass_ypr = case_when(
        is.na(def_pass_ypr) ~ def_pass_ypr_PY1,
        TRUE ~ def_pass_ypr
      ),
      off_comp_pct = case_when(
        is.na(off_comp_pct) ~ off_comp_pct_PY1,
        TRUE ~ off_comp_pct
      ),
      def_comp_pct = case_when(
        is.na(def_comp_pct) ~ def_comp_pct_PY1,
        TRUE ~ def_comp_pct
      ),
      off_havoc_total = case_when(
        is.na(off_havoc_total) ~ off_havoc_total_PY1,
        TRUE ~ off_havoc_total
      ),
      def_havoc_total = case_when(
        is.na(def_havoc_total) ~ weighted_def_havoc_total,
        TRUE ~ def_havoc_total
      ),
      off_pass_epa = case_when(
        is.na(off_pass_epa) ~ off_pass_epa_PY1,
        TRUE ~ off_pass_epa
      ),
      def_pass_epa = case_when(
        is.na(def_pass_epa) ~ def_pass_epa_PY1,
        TRUE ~ def_pass_epa
      ),
      off_pass_success_rate = case_when(
        is.na(off_pass_success_rate) ~ off_pass_success_rate_PY1,
        TRUE ~ off_pass_success_rate
      ),
      def_pass_success_rate = case_when(
        is.na(def_pass_success_rate) ~ def_pass_success_rate_PY1,
        TRUE ~ def_pass_success_rate
      ),
      off_pass_explosiveness = case_when(
        is.na(off_pass_explosiveness) ~ off_pass_explosiveness_PY1,
        TRUE ~ off_pass_explosiveness
      ),
      def_pass_explosiveness = case_when(
        is.na(def_pass_explosiveness) ~ def_pass_explosiveness_PY1,
        TRUE ~ def_pass_explosiveness
      ),
      off_rush_epa = case_when(
        is.na(off_rush_epa) ~ off_rush_epa_PY1,
        TRUE ~ off_rush_epa
      ),
      def_rush_epa = case_when(
        is.na(def_rush_epa) ~ def_rush_epa_PY1,
        TRUE ~ def_rush_epa
      ),
      off_rush_success_rate = case_when(
        is.na(off_rush_success_rate) ~ off_rush_success_rate_PY1,
        TRUE ~ off_rush_success_rate
      ),
      def_rush_success_rate = case_when(
        is.na(def_rush_success_rate) ~ def_rush_success_rate_PY1,
        TRUE ~ def_rush_success_rate
      ),
      off_rush_explosiveness = case_when(
        is.na(off_rush_explosiveness) ~ off_rush_explosiveness_PY1,
        TRUE ~ off_rush_explosiveness
      ),
      def_rush_explosiveness = case_when(
        is.na(def_rush_explosiveness) ~ def_rush_explosiveness_PY1,
        TRUE ~ def_rush_explosiveness
      ),
      off_rush_ypg = case_when(
        is.na(off_rush_ypg) ~ off_rush_ypg_PY1,
        TRUE ~ off_rush_ypg
      ),
      def_rush_ypg = case_when(
        is.na(def_rush_ypg) ~ def_rush_ypg_PY1,
        TRUE ~ def_rush_ypg
      ),
      off_rush_ypa = case_when(
        is.na(off_rush_ypa) ~ off_rush_ypa_PY1,
        TRUE ~ off_rush_ypa
      ),
      def_rush_ypa = case_when(
        is.na(def_rush_ypa) ~ def_rush_ypa_PY1,
        TRUE ~ def_rush_ypa
      ),
      off_stuff_rate = case_when(
        is.na(off_stuff_rate) ~ off_stuff_rate_PY1,
        TRUE ~ off_stuff_rate
      ),
      def_stuff_rate = case_when(
        is.na(def_stuff_rate) ~ def_stuff_rate_PY1,
        TRUE ~ def_stuff_rate
      ),
      off_line_yds = case_when(
        is.na(off_line_yds) ~ off_line_yds_PY1,
        TRUE ~ off_line_yds
      ),
      def_line_yds = case_when(
        is.na(def_line_yds) ~ def_line_yds_PY1,
        TRUE ~ def_line_yds
      ),
      off_st_epa = case_when(
        is.na(off_st_epa) ~ off_st_epa_PY1,
        TRUE ~ off_st_epa
      ),
      def_st_epa = case_when(
        is.na(def_st_epa) ~ def_st_epa_PY1,
        TRUE ~ def_st_epa
      ),
      off_success_rate = case_when(
        is.na(off_success_rate) ~ weighted_off_success_rate,
        TRUE ~ off_success_rate
      ),
      def_success_rate = case_when(
        is.na(def_success_rate) ~ weighted_def_success_rate,
        TRUE ~ def_success_rate
      ),
      kick_return_TDs = case_when(
        is.na(kick_return_TDs) ~ 0,
        TRUE ~ kick_return_TDs
      ),
      kick_return_TDs_allowed = case_when(
        is.na(kick_return_TDs_allowed) ~ 0,
        TRUE ~ kick_return_TDs_allowed
      ),
      net_st_ppg = case_when(
        is.na(net_st_ppg) ~ weighted_net_st_ppg_mean,
        TRUE ~ net_st_ppg
      ),
      net_punt_return_yds = case_when(
        is.na(net_punt_return_yds) ~ weighted_net_punt_return_yds,
        TRUE ~ net_punt_return_yds
      ),
      net_kick_return_yds = case_when(
        is.na(net_kick_return_yds) ~ weighted_net_kick_return_yds,
        TRUE ~ net_kick_return_yds
      ),
      net_punt_return_TDs = case_when(
        is.na(net_punt_return_TDs) ~ 0,
        TRUE ~ net_punt_return_TDs
      ),
      net_kick_return_TDs = case_when(
        is.na(net_kick_return_TDs) ~ 0,
        TRUE ~ net_kick_return_TDs
      ),
      net_fg_rate = case_when(
        is.na(net_fg_rate) ~ weighted_net_fg_rate,
        TRUE ~ net_fg_rate
      ),
      net_fg_made_pg = case_when(
        is.na(net_fg_made_pg) ~ weighted_net_fg_made_pg,
        TRUE ~ net_fg_made_pg
      ),
      adj_off_epa = case_when(
        is.na(adj_off_epa) ~ weighted_off_epa,
        TRUE ~ adj_off_epa
      ),
      adj_def_epa = case_when(
        is.na(adj_def_epa) ~ weighted_def_epa,
        TRUE ~ adj_def_epa
      ),
      adj_off_plays_pg = case_when(
        is.na(adj_off_plays_pg) ~ weighted_off_plays_pg,
        TRUE ~ adj_off_plays_pg
      ),
      adj_def_plays_pg = case_when(
        is.na(adj_def_plays_pg) ~ weighted_def_plays_pg,
        TRUE ~ adj_def_plays_pg
      ),
      adj_off_explosiveness = case_when(
        is.na(adj_off_explosiveness) ~ weighted_off_explosiveness,
        TRUE ~ adj_off_explosiveness
      ),
      adj_def_explosiveness = case_when(
        is.na(adj_def_explosiveness) ~ weighted_def_explosiveness,
        TRUE ~ adj_def_explosiveness
      ),
      adj_off_pts_per_play = case_when(
        is.na(adj_off_pts_per_play) ~ adj_off_pts_per_play_PY1,
        TRUE ~ adj_off_pts_per_play
      ),
      adj_def_pts_per_play = case_when(
        is.na(adj_def_pts_per_play) ~ adj_def_pts_per_play_PY1,
        TRUE ~ adj_def_pts_per_play
      ),
      adj_off_ypp = case_when(
        is.na(adj_off_ypp) ~ weighted_off_ypp,
        TRUE ~ adj_off_ypp
      ),
      adj_def_ypp = case_when(
        is.na(adj_def_ypp) ~ weighted_def_ypp,
        TRUE ~ adj_def_ypp
      ),
      adj_off_st_epa = case_when(
        is.na(adj_off_st_epa) ~ adj_off_st_epa_PY1,
        TRUE ~ adj_off_st_epa
      ),
      adj_def_st_epa = case_when(
        is.na(adj_def_st_epa) ~ adj_def_st_epa_PY1,
        TRUE ~ adj_def_st_epa
      ),
      net_adj_st_epa = case_when(
        is.na(net_adj_st_epa) ~ weighted_net_adj_st_epa,
        TRUE ~ net_adj_st_epa
      ),
      adj_off_st_pts_per_play = case_when(
        is.na(adj_off_st_pts_per_play) ~ adj_off_st_pts_per_play_PY1,
        TRUE ~ adj_off_st_pts_per_play
      ),
      adj_def_st_pts_per_play = case_when(
        is.na(adj_def_st_pts_per_play) ~ adj_def_st_pts_per_play_PY1,
        TRUE ~ adj_def_st_pts_per_play
      ),
      adj_off_st_ppg = case_when(
        is.na(adj_off_st_ppg) ~ adj_off_st_ppg_PY1,
        TRUE ~ adj_off_st_ppg
      ),
      adj_def_st_ppg = case_when(
        is.na(adj_def_st_ppg) ~ adj_def_st_ppg_PY1,
        TRUE ~ adj_def_st_ppg
      )
    ) |>
    mutate(
      st_net_epa = case_when(
        is.na(st_net_epa) ~ off_st_epa - def_st_epa,
        TRUE ~ st_net_epa
      ),
      net_adj_st_ppg = case_when(
        is.na(net_adj_st_ppg) ~ adj_off_st_ppg - adj_def_st_ppg,
        TRUE ~ net_adj_st_ppg
      ),
      EPA_diff = case_when(
        is.na(EPA_diff) ~ adj_off_epa - adj_def_epa,
        TRUE ~ EPA_diff
      ),
      SuccessRt_diff = case_when(
        is.na(SuccessRt_diff) ~ off_success_rate - def_success_rate,
        TRUE ~ SuccessRt_diff
      ),
      HavocRt_diff = case_when(
        is.na(HavocRt_diff) ~ off_havoc_total - def_havoc_total,
        TRUE ~ HavocRt_diff
      ),
      Explosiveness_diff = case_when(
        is.na(Explosiveness_diff) ~ adj_off_explosiveness -
          adj_def_explosiveness,
        TRUE ~ Explosiveness_diff
      )
    )

  return(VoA_df)
}

print("VoA functions read into R environment")
