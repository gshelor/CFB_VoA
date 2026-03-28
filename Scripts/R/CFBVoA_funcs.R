##### Functions for preparing or modeling CFB_VoA #####

library(pacman)
p_load(cfbfastR, tidyverse, here, data.table, parallel, cfbplotR, lme4)

create_voavars_df <- function(teams_df, pbp_df) {
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
    ### adding columns which will be filled in later using pbp data
    mutate(
      total_yds_pg = 0,
      pass_yds_pg = 0,
      rush_yds_pg = 0,
      first_downs_pg = 0,
      def_interceptions_pg = 0,
      pass_ypa = 0,
      off_ypp = 0,
      completion_pct = 0,
      pass_ypr = 0,
      int_pct = 0,
      rush_ypc = 0,
      turnovers_pg = 0,
      third_conv_rate = 0,
      fourth_conv_rate = 0,
      penalty_yds_pg = 0,
      yards_per_penalty = 0,
      kick_return_avg = 0,
      punt_return_avg = 0,
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
      off_ppa = 0,
      off_success_rate = 0,
      off_explosiveness = 0,
      off_power_success = 0,
      off_stuff_rate = 0,
      off_line_yds = 0,
      off_pts_per_opp = 0,
      off_havoc_total = 0,
      off_standard_downs_ppa = 0,
      off_standard_downs_success_rate = 0,
      off_standard_downs_explosiveness = 0,
      off_passing_downs_ppa = 0,
      off_passing_downs_success_rate = 0,
      off_passing_downs_explosiveness = 0,
      off_rushing_plays_ppa = 0,
      off_rushing_plays_success_rate = 0,
      off_rushing_plays_explosiveness = 0,
      off_passing_plays_ppa = 0,
      off_passing_plays_success_rate = 0,
      off_passing_plays_explosiveness = 0,
      def_ppa = 0,
      def_success_rate = 0,
      def_explosiveness = 0,
      def_power_success = 0,
      def_stuff_rate = 0,
      def_line_yds = 0,
      def_pts_per_opp = 0,
      def_havoc_total = 0,
      def_standard_downs_ppa = 0,
      def_standard_downs_success_rate = 0,
      def_standard_downs_explosiveness = 0,
      def_passing_downs_ppa = 0,
      def_passing_downs_success_rate = 0,
      def_passing_downs_explosiveness = 0,
      def_rushing_plays_ppa = 0,
      def_rushing_plays_success_rate = 0,
      def_rushing_plays_explosiveness = 0,
      def_passing_plays_ppa = 0,
      def_passing_plays_success_rate = 0,
      def_passing_plays_explosiveness = 0
    )
  ### returning VoA Variables df, which have stat values filled in for each team later
  return(VoAVars)
}
create_pbp_subsets <- function(pbp_df) {}


print("VoA functions read into environment")
