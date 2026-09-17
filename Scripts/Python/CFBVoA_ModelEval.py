##### CFB Vortex of Accuracy Model Evaluation #####
### cfbfastR returns fewer games than hitting the CFBD API directly so I'm using python to do that now

##### importing libraries #####
import polars as pl
import pandas as pd
import numpy as np
import os
import cfbd
from dotenv import load_dotenv

### loading environmental variables (API token, so it's not directly printed in the code for security)
load_dotenv()


### identifying season and week of season
season = input(prompt = "What season is it? ")
cfb_week = input(prompt = "What week just occurred? ")

### strings I want to preserve
# VoA_text = "VoA"
VoP_text = "VoP"
week_text = "Week"

### setting up cfbd api client
configuration = cfbd.Configuration(
    host="https://api.collegefootballdata.com", 
    access_token = os.getenv("CFB_TOKEN"))
api_client = cfbd.ApiClient(configuration)


### setting up API instance
games_api_instance = cfbd.GamesApi(api_client)
betting_api_instance = cfbd.BettingApi(api_client)

##### Reading in VoA to filter games to only be between teams in the VoA #####
PrevWeekFBSVoA = pl.read_parquet(os.path.join(os.getcwd(),
  "Data",
  "VoA" + season,
  season + week_text + str(int(cfb_week) - 1) + "_FBSVoA.parquet"
  )
)
PrevWeekFCSVoA = pl.read_parquet(os.path.join(os.getcwd(),
  "Data",
  "VoA" + season,
  season + week_text + str(int(cfb_week) - 1) + "_FCSVoA.parquet"
  )
)
PrevWeekFBSFCSVoA = pl.read_csv(os.path.join(os.getcwd(),
  "Data",
  "VoA" + season,
  "AllD1" + season + week_text + str(int(cfb_week) - 1) + "VoA.csv"
  )
)


##### Reading in completed games #####
if int(cfb_week) >= 17:
    print("fix the below code")
  ### reading in most recent week's projections
#   PrevWeekVoP = pl.read_parquet(os.path.join(os.getcwd(),
#     "Data",
#     "VoA" +season,
#     "Projections",
#     season + VoP_text + week_text, cfb_week, "Games.csv")
#   ).filter(
#     (pl.col("home_team").is_in(PrevWeekFBSFCSVoA['school'].implode())) | (pl.col("away_team").is_in(PrevWeekFBSFCSVoA['school'].implode())).select(["id", "predicted"]
#         ))
#     PrevWeekVoP = PrevWeekVoP.rename({"game_id", "proj_margin")

#   LastWeekGames = cfbd_game_info(
#     int(season),
#     season_type = "postseason"
#   ) |>
#     filter(completed == TRUE) |>
#     filter(game_id %in% PrevWeekVoP$game_id) |>
#     select(
#       game_id,
#       season,
#       week,
#       completed,
#       home_team,
#       home_points,
#       away_team,
#       away_points
#     ) |>
#     mutate(
#       result = away_points - home_points,
#       winner = case_when(result > 0 ~ away_team, TRUE ~ home_team)
#     )

#   LastWeekSpreads_temp = cfbd_betting_lines(
#     year = int(season),
#     season_type = "postseason"
#   ) |>
#     filter(game_id %in% PrevWeekVoP$game_id) |>
#     select(game_id, spread, home_moneyline, away_moneyline)
elif int(cfb_week) == 16:
    print("fix the below code")
    ### reading in most recent week's projections
    PrevWeekVoP = pl.read_csv(os.path.join(os.getcwd(),
    "Data",
    "VoA" + season,
    "Projections",
    "CFBD",
    season + VoP_text + week_text + cfb_week + "Games.csv")
    ).filter(
        (pl.col("home").is_in(PrevWeekFBSFCSVoA["school"].implode())) & (pl.col("away").is_in(PrevWeekFBSFCSVoA["school"].implode()))
        ).select(
            ["id", "predicted"]
            )

    # PrevWeekVoP = PrevWeekVoP.select(["id", "predicted"])
    PrevWeekVoP = PrevWeekVoP.rename({"predicted": "proj_margin"})

#   ### reading in completed games from previous week
#   ## in week 16, this should just be Army-Navy, with any completed bowl games being read in separately below
#   LastWeekGames = cfbd_game_info(int(season)) |>
#     filter(completed == TRUE) |>
#     filter(week == int(cfb_week)) |>
#     filter(game_id %in% PrevWeekVoP$game_id) |>
#     select(
#       game_id,
#       season,
#       week,
#       completed,
#       home_team,
#       home_points,
#       away_team,
#       away_points
#     ) |>
#     mutate(
#       result = away_points - home_points,
#       winner = case_when(result > 0 ~ away_team, TRUE ~ home_team)
#     )
#   ### because of conference realignment and CFP expansion, bowl games now happen on the same weekend as the Army-Navy game because we as a society insist on constantly creating new personal affronts to god
#   LastWeekBowlGames = cfbd_game_info(
#     int(season),
#     season_type = "postseason"
#   ) |>
#     filter(completed == TRUE) |>
#     filter(game_id %in% PrevWeekVoP$game_id) |>
#     select(
#       game_id,
#       season,
#       week,
#       completed,
#       home_team,
#       home_points,
#       away_team,
#       away_points
#     ) |>
#     mutate(
#       result = away_points - home_points,
#       winner = case_when(result > 0 ~ away_team, TRUE ~ home_team)
#     )

    ### binding regular season games and completed bowl games together for error calculation
    LastWeekGames = pl.concat([LastWeekGames, LastWeekBowlGames], how = "vertical")
#   ### pulling spread lines to compare to VoA error
#   LastWeekSpreads_temp = cfbd_betting_lines(year = int(season)) |>
#     filter(week == int(cfb_week)) |>
#     filter(game_id %in% PrevWeekVoP$game_id) |>
#     select(game_id, spread, home_moneyline, away_moneyline)
else:
    ### reading in most recent week's projections
    PrevWeekVoP = pl.read_csv(os.path.join(os.getcwd(),
    "Data",
    "VoA" + season,
    "Projections",
    "CFBD",
    season + VoP_text + week_text + cfb_week + "Games.csv")
    ).filter(
        (pl.col("home").is_in(PrevWeekFBSFCSVoA["school"].implode())) & (pl.col("away").is_in(PrevWeekFBSFCSVoA["school"].implode()))
        ).select(
            ["id", "predicted"]
            )

    # PrevWeekVoP = PrevWeekVoP.select(["id", "predicted"])
    PrevWeekVoP = PrevWeekVoP.rename({"predicted": "proj_margin"})
  # colnames(PrevWeekFBSVoP) = c("game_id", "proj_margin")
  # colnames(PrevWeekFCSVoP) = c("game_id", "proj_margin")
  # colnames(PrevWeekCrossDivVoP) = c("game_id", "proj_margin")

    LastWeekGames = pl.DataFrame(games_api_instance.get_games(year = int(season), week = int(cfb_week)), infer_schema_length = None).filter(
        pl.col("completed") == True
        ).filter(
            pl.col("week") == int(cfb_week)
            ).filter(
                pl.col("id").is_in(PrevWeekVoP["id"].implode())
                ).select(
                    ["id",
                    "season",
                    "week",
                    "completed",
                    "home_team",
                    "home_classification",
                    "home_points",
                    "away_team",
                    "away_classification",
                    "away_points"]
                    ).with_columns(
                        result = pl.col("away_points") - pl.col("home_points")
                    ).with_columns(
                        winner = pl.when(pl.col("result") > 0).then(pl.col("away_team")).otherwise(pl.col("home_team"))
                        )

    LastWeekSpreads = pl.DataFrame(betting_api_instance.get_lines(year = int(season), week = int(cfb_week)), infer_schema_length = None, strict = False).filter(
        pl.col("week") == int(cfb_week)
        ).filter(
            pl.col("id").is_in(PrevWeekVoP["id"].implode())
            ).explode('lines').unnest('lines').pivot(
        values=["spread", "over_under"],
        index=["id", "season", "season_type", "start_date", "home_team_id", "home_team", "home_conference", "home_classification", "home_score", "away_team_id", "away_team", "away_conference", "away_classification", "away_score", "home_moneyline", "away_moneyline"], # game identifier
        on="provider"
    ).with_columns(
        mean_spread = pl.mean_horizontal(pl.selectors.starts_with("spread"), ignore_nulls = True)#,
        ### calculating actual margin, to be compared to mean_spread and VoA projection
        ## away score minus home score because of how the CFBD API takes projections, so that's how I set up the VoA projections too
        # result = pl.col('away_score') - pl.col('home_score')
    ).select(
        ['id', 'mean_spread', 'home_moneyline', 'away_moneyline']
    )

    ### joining games and spread info
    LastWeekGames = LastWeekGames.join(LastWeekSpreads, on = "id", how = "left").join(
        PrevWeekVoP, on = "id", how = "left"
    ).with_columns(
        VoA_AE = (pl.col('result') - pl.col('proj_margin')).abs(),
        vegas_AE = (pl.col('result') - pl.col('mean_spread')).abs(),
        VoA_SE = (pl.col('result') - pl.col('proj_margin')).pow(2),
        vegas_SE = (pl.col('result') - pl.col('mean_spread')).pow(2),
        VoA_correct_winner = pl.when(
            ((pl.col('proj_margin') < 0) & (pl.col('result') < 0)) | 
            ((pl.col('proj_margin') > 0) & (pl.col('result') > 0))).then(1)
            .otherwise(0),
        vegas_correct_winner = pl.when(
            ((pl.col('result') < 0) & (pl.col('mean_spread') < 0)) | 
            ((pl.col('result') > 0) & (pl.col('mean_spread') > 0))).then(1)
            .otherwise(0),
        VoA_ATS_winner = pl.when(
            ((pl.col('proj_margin') < pl.col('mean_spread')) & (pl.col('result') < pl.col('mean_spread'))) |
            ((pl.col('proj_margin') > pl.col('mean_spread')) & (pl.col('result') > pl.col('mean_spread')))).then(1)
        .otherwise(0)
    ).with_columns(
        VoA_AEATS_winner = pl.when(pl.col('VoA_AE') < pl.col('vegas_AE')).then(1).otherwise(0)
    )

    ### filtering out games by subdivision or if cross-subdivision to calculate specific errors for those kinds of games
    PrevWeekFBSGames = LastWeekGames.filter(
        (pl.col("home_classification") == "fbs") & (pl.col("away_classification") == "fbs")
        )

    PrevWeekFCSGames = LastWeekGames.filter(
        (pl.col("home_classification") == "fcs") & (pl.col("away_classification") == "fcs")
        )

    PrevWeekCrossDivGames = LastWeekGames.filter(
        pl.col("home_classification") != pl.col("away_classification")
        )

    ### going to put an if/else statement here inside of the else for the purposes of binding games from week 2-onwards to previous week's games where error was previously already calculated
    ### end of if/else statement


### creating season summary stats from completed games
WeekAccuracy = pl.DataFrame({
    'season' : int(season),
    'week' : int(cfb_week),
    'games' : LastWeekGames.height,
    'fbs_games': PrevWeekFBSGames.height,
    'fcs_games': PrevWeekFCSGames.height,
    # 'cross_div_games': PrevWeekCrossDivGames.height,
    'VoA_MAE' : LastWeekGames['VoA_AE'].mean(),
    'vegas_MAE' : LastWeekGames['vegas_AE'].mean(),
    'VoA_FBS_MAE': PrevWeekFBSGames['VoA_AE'].mean(),
    'vegas_FBS_MAE': PrevWeekFBSGames['vegas_AE'].mean(),
    'VoA_FCS_MAE': PrevWeekFCSGames['VoA_AE'].mean(),
    'vegas_FCS_MAE': PrevWeekFCSGames['vegas_AE'].mean(),
    # 'VoA_crossdiv_MAE': PrevWeekCrossDivGames['VoA_AE'].mean(),
    # 'vegas_crossdiv_MAE': PrevWeekCrossDivGames['vegas_AE'].mean(),
    'VoA_MSE' : LastWeekGames['VoA_SE'].mean(),
    'vegas_MSE' : LastWeekGames['vegas_SE'].mean(),
    'VoA_FBS_MSE': PrevWeekFBSGames['VoA_SE'].mean(),
    'vegas_FBS_MSE': PrevWeekFBSGames['vegas_SE'].mean(),
    'VoA_FCS_MSE': PrevWeekFCSGames['VoA_SE'].mean(),
    'vegas_FCS_MSE': PrevWeekFCSGames['vegas_SE'].mean(),
    # 'VoA_crossdiv_MSE': PrevWeekCrossDivGames['VoA_SE'].mean(),
    # 'vegas_crossdiv_MSE': PrevWeekCrossDivGames['vegas_SE'].mean(),
    'VoA_RMSE' : np.sqrt(LastWeekGames['VoA_SE'].mean()),
    'vegas_RMSE' : np.sqrt(LastWeekGames['vegas_SE'].mean()),
    'VoA_FBS_RMSE': np.sqrt(PrevWeekFBSGames['VoA_SE'].mean()),
    'vegas_FBS_RMSE': np.sqrt(PrevWeekFBSGames['vegas_SE'].mean()),
    'VoA_FCS_RMSE': np.sqrt(PrevWeekFCSGames['VoA_SE'].mean()),
    'vegas_FCS_RMSE': np.sqrt(PrevWeekFCSGames['vegas_SE'].mean()),
    # 'VoA_crossdiv_RMSE': np.sqrt(PrevWeekCrossDivGames['VoA_SE'].mean()),
    # 'vegas_crossdiv_RMSE': np.sqrt(PrevWeekCrossDivGames['vegas_SE'].mean()),
    'VoA_win_pct' : LastWeekGames['VoA_correct_winner'].mean(),
    'vegas_win_pct' : LastWeekGames['vegas_correct_winner'].mean(),
    'VoA_FBS_win_pct': PrevWeekFBSGames['VoA_correct_winner'].mean(),
    'vegas_FBS_win_pct': PrevWeekFBSGames['vegas_correct_winner'].mean(),
    'VoA_FCS_win_pct': PrevWeekFCSGames['VoA_correct_winner'].mean(),
    'vegas_FCS_win_pct': PrevWeekFCSGames['vegas_correct_winner'].mean(),
    # 'VoA_crossdiv_win_pct': PrevWeekCrossDivGames['VoA_correct_winner'].mean(),
    # 'vegas_crossdiv_win_pct': PrevWeekCrossDivGames['vegas_correct_winner'].mean(),
    'VoA_ATS_win_pct' : LastWeekGames['VoA_ATS_winner'].mean(),
    'VoA_AEATS_win_pct' : LastWeekGames['VoA_AEATS_winner'].mean(),
    'VoA_FBS_ATS_win_pct': PrevWeekFBSGames['VoA_ATS_winner'].mean(),
    'VoA_FCS_ATS_win_pct': PrevWeekFCSGames['VoA_ATS_winner'].mean(),
    # 'VoA_crossdiv_ATS_win_pct': PrevWeekCrossDivGames['VoA_ATS_winner'].mean(),
    'VoA_FBS_AEATS_win_pct': PrevWeekFBSGames['VoA_AEATS_winner'].mean(),
    'VoA_FCS_AEATS_win_pct': PrevWeekFCSGames['VoA_AEATS_winner'].mean()#,
    # 'VoA_crossdiv_AEATS_win_pct': PrevWeekCrossDivGames['VoA_AEATS_winner'].mean()
}
)

### saving df of games and week summary error stats
if int(cfb_week) == 1:
    ### saving csv of individual games
    LastWeekGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "GameAccuracyMetrics.csv"
        )
    )

    ### FBS
    PrevWeekFBSGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "FBSGameAccuracyMetrics.csv"
        )
    )

    ### FBS
    PrevWeekFCSGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "FCSGameAccuracyMetrics.csv"
        )
    )

    ### saving week accuracy averages
    WeekAccuracy.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "WeekAccuracyMetrics.csv"
        )
    )
else:
    ### reading in csv of previous games with error calculated, binding current week's games to that
    PrevWeekGameAccuracyMetrics = pl.read_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + season,
        "AccuracyMetrics",
        "Games",
        "VoA" + season + week_text + "1" + week_text + str(int(cfb_week) - 1) + "GameAccuracyMetrics.csv"
        )
    )
    CompletedGames = pl.concat([PrevWeekGameAccuracyMetrics, LastWeekGames], how = "vertical_relaxed")

    ### FBS specifically
    PrevWeekFBSGameAccuracyMetrics = pl.read_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + season,
        "AccuracyMetrics",
        "Games",
        "VoA" + season + week_text + "1" + week_text + str(int(cfb_week) - 1) + "FBSGameAccuracyMetrics.csv"
        )
    )
    FBSCompletedGames = pl.concat([PrevWeekFBSGameAccuracyMetrics, PrevWeekFBSGames], how = "vertical_relaxed")

    ### FBS specifically
    PrevWeekFCSGameAccuracyMetrics = pl.read_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + season,
        "AccuracyMetrics",
        "Games",
        "VoA" + season + week_text + "1" + week_text + str(int(cfb_week) - 1) + "FBSGameAccuracyMetrics.csv"
        )
    )
    FCSCompletedGames = pl.concat([PrevWeekFCSGameAccuracyMetrics, PrevWeekFCSGames], how = "vertical_relaxed")
    
    ### writing csv with individual games + accuracy metrics
    CompletedGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "GameAccuracyMetrics.csv"
      )
    )

    ### writing csv with individual games + accuracy metrics
    FBSCompletedGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "FBSGameAccuracyMetrics.csv"
      )
    )

    ### writing csv with individual games + accuracy metrics
    FCSCompletedGames.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "Games",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "FCSGameAccuracyMetrics.csv"
      )
    )

    ### reading in csv of weekly average, binding current week to it
    PrevWeeklyAvgAccuracyMetrics = pl.read_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + season,
        "AccuracyMetrics",
        "VoA" + season + week_text + "1" + week_text + str(int(cfb_week) - 1) + "WeekAccuracyMetrics.csv"
        )
    )
    
    CompletedWeeks = pl.concat([PrevWeeklyAvgAccuracyMetrics, WeekAccuracy], how = "vertical_relaxed")

    ### writing csv with just weekly average calculated for accuracy metrics
    CompletedWeeks.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "VoA" + season + week_text + "1" + week_text + cfb_week + "WeekAccuracyMetrics.csv"
      )
    )

    ### end of if/else statement


if int(cfb_week) >= 2:
    SeasonMetrics = CompletedGames.group_by('season').agg(
        games = CompletedGames.height,
        VoA_MAE = CompletedGames['VoA_AE'].mean(),
        vegas_MAE = CompletedGames['vegas_AE'].mean(),
        VoA_FBS_MAE = FBSCompletedGames['VoA_AE'].mean(),
        vegas_FBS_MAE = FBSCompletedGames['vegas_AE'].mean(),
        VoA_FCS_MAE = FCSCompletedGames['VoA_AE'].mean(),
        vegas_FCS_MAE = FCSCompletedGames['vegas_AE'].mean(),
        VoA_MSE = CompletedGames['VoA_SE'].mean(),
        vegas_MSE = CompletedGames['vegas_SE'].mean(),
        VoA_FBS_MSE = FBSCompletedGames['VoA_SE'].mean(),
        vegas_FBS_MSE = FBSCompletedGames['vegas_SE'].mean(),
        VoA_FCS_MSE = FCSCompletedGames['VoA_SE'].mean(),
        vegas_FCS_MSE = FCSCompletedGames['vegas_SE'].mean(),
        VoA_RMSE = np.sqrt(CompletedGames['VoA_SE'].mean()),
        vegas_RMSE = np.sqrt(CompletedGames['vegas_SE'].mean()),
        VoA_FBS_RMSE = np.sqrt(FBSCompletedGames['VoA_SE'].mean()),
        vegas_FBS_RMSE = np.sqrt(FBSCompletedGames['vegas_SE'].mean()),
        VoA_FCS_RMSE = np.sqrt(FCSCompletedGames['VoA_SE'].mean()),
        vegas_FCS_RMSE = np.sqrt(FCSCompletedGames['vegas_SE'].mean()),
        VoA_win_pct = CompletedGames['VoA_correct_winner'].mean(),
        vegas_win_pct = CompletedGames['vegas_correct_winner'].mean(),
        VoA_FBS_win_pct = FBSCompletedGames['VoA_correct_winner'].mean(),
        vegas_FBS_win_pct = FBSCompletedGames['vegas_correct_winner'].mean(),
        VoA_FCS_win_pct = FCSCompletedGames['VoA_correct_winner'].mean(),
        vegas_FCS_win_pct = FCSCompletedGames['vegas_correct_winner'].mean(),
        VoA_ATS_win_pct = CompletedGames['VoA_ATS_winner'].mean(),
        VoA_FBS_ATS_win_pct = FBSCompletedGames['VoA_ATS_winner'].mean(),
        VoA_FCS_ATS_win_pct = FCSCompletedGames['VoA_ATS_winner'].mean(),
        VoA_AEATS_win_pct = CompletedGames['VoA_AEATS_winner'].mean(),
        VoA_FBS_AEATS_win_pct = FBSCompletedGames['VoA_AEATS_winner'].mean(),
        VoA_FCS_AEATS_win_pct = FCSCompletedGames['VoA_AEATS_winner'].mean()
        )
        
        
        
    SeasonMetrics.write_csv(
        os.path.join(
            os.getcwd(),
            "Data",
            "VoA" + season,
            "AccuracyMetrics",
            "VoA" + season + "SeasonAccuracyMetrics.csv"
        )
    )
else:
  print("season metrics not being calculated yet!")



##### POOPYPANTS TESTING BELOW HERE #####
# poopypants = LastWeekGames.filter(pl.col("result") > 10000000)
# if poopypants.height == 0:
#     print("you did it")

# poopypants2 = pl.concat([LastWeekGames, poopypants], how = "vertical")
