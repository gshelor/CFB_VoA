##### trying to see something in the cfbd python api #####

##### importing libraries and setting up python environment and API connection #####
import cfbd
import polars as pl
# import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sbn
from dotenv import load_dotenv
from datetime import date, datetime, timedelta
# import cmdstanpy
# import pymc as pm
# import arviz as az
# import preliz as pz
# import statsmodels.formula.api as smf
import statsmodels.api as sm
# from statsmodels.othermod.betareg import BetaModel
# from sklearn.linear_model import LogisticRegression
import random
# from great_tables import GT
# import selenium
# import sportsdataverse
# import json
import pickle

### loading environmental variables (API token, so it's not directly printed in the code for security)
load_dotenv()

### Inputting year
cfb_year = input(prompt = "What Year is it? (year season started in) ")
### Inputting upcoming week number
upcoming = input(prompt = "What week is upcoming? ")

### Text Strings for gt table of game projections at the end
week_text = "Week"
gameprojections_png = "GameProjections.png"
gameprojections_filename = cfb_year + week_text + upcoming + gameprojections_png


### setting up cfbd api client
configuration = cfbd.Configuration(
    host="https://api.collegefootballdata.com", 
    access_token = os.getenv("CFB_TOKEN"))
api_client = cfbd.ApiClient(configuration)


### setting up API instance
games_api_instance = cfbd.GamesApi(api_client)

##### reading in most recent VoA overall ratings #####
if int(upcoming) == 1:
    FBS_VoA = pl.read_parquet(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    cfb_year + week_text + str(int(upcoming) - 1) + "_FBSVoA.parquet"
    )).select(["school", "conference", "VoA_Rating_Ovr"])
    ### reading in FCS VoA now
    FCS_VoA = pl.read_parquet(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    cfb_year + week_text + str(int(upcoming) - 1) + "_FCSVoA.parquet"
    )).select(["school", "conference", "VoA_Rating_Ovr"])
  
    ### Prev week VoA is the ratings directly calculated by the individual models
    PrevWeek_VoA_AllCols = pl.concat([FBS_VoA, FCS_VoA], how = "vertical")

    PrevWeek_VoA = PrevWeek_VoA_AllCols.select(["school", "VoA_Rating_Ovr"])

    ### reading in VoA with all D1 teams with VoA ratings adjusted to reflect general differences between FBS and FCS subivisions
    ## unit-specific ratings not included, just overall ratings
    AllD1VoA = pl.read_csv(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    "AllD1" + cfb_year + week_text + str(int(upcoming) - 1) + "VoA.csv"))

    LowerQtrRatings = AllD1VoA.filter(pl.col("VoA_Rating_Ovr") < pl.quantile("VoA_Rating_Ovr", 0.25))
else:
    ##### Reading VoAs after Preseason #####
    ### reading in VoAs but not including the conference column because I just left_join the ratings to the games df (games are pulled later)
    FBS_VoA = pl.read_parquet(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    cfb_year + week_text + str(int(upcoming) - 1) + "_FBSVoA.parquet"
    )).select(["school", "VoA_Rating_Ovr"])
    ### reading in FCS VoA now
    FCS_VoA = pl.read_parquet(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    cfb_year + week_text + str(int(upcoming) - 1) + "_FCSVoA.parquet"
    )).select(["school", "VoA_Rating_Ovr"])

    ### Prev week VoA is the ratings directly calculated by the individual models
    # PrevWeek_VoA_AllCols = rbind(FBS_VoA, FCS_VoA)

    PrevWeek_VoA = pl.concat([FBS_VoA, FCS_VoA], how = "vertical")
    ### reading in VoA with all D1 teams with VoA ratings adjusted to reflect general differences between FBS and FCS subivisions
    ## unit-specific ratings not included, just overall ratings
    AllD1VoA = pl.read_csv(os.path.join(os.getcwd(),
    "Data",
    "VoA" + cfb_year,
    "AllD1" + cfb_year + week_text + str(int(upcoming) - 1) + "VoA.csv"))

    ### bottom 50% of ratings will be used to generate artificial ratings for teams not in the VoA but who are playing VoA teams
    LowerQtrRatings = AllD1VoA.filter(pl.col("VoA_Rating_Ovr") < pl.quantile("VoA_Rating_Ovr", 0.25))

##### Pulling Games #####
if int(upcoming) == 1:
    ### converting API output to a polars dataframe, and removing duplicate game ids since each team in a game gets its own row for some reason
    FullSeasonGames_df = pl.DataFrame(games_api_instance.get_games(year = int(cfb_year)), infer_schema_length = None).select(
      ["id",
      "season",
      "week",
      "neutral_site",
      "home_team",
      "home_classification",
      "home_conference",
      "away_team",
      "away_classification",
      "away_conference"]).filter(
        (pl.col("home_team").is_in(PrevWeek_VoA['school'].implode())) | (pl.col("away_team").is_in(PrevWeek_VoA['school'].implode()))
      )

    ### setting initial temp df for assigning VoA ratings to games where both teams are in VoA
    temp_ratings_df = PrevWeek_VoA.select(["school", "VoA_Rating_Ovr"]).rename({
    "school": "home_team",
    "VoA_Rating_Ovr": "home_VoA_rating"})

    ### assigning ratings for home teams
    ### FBS Games
    FBSGames = FullSeasonGames_df.filter((pl.col("home_team").is_in(FBS_VoA["school"].implode())) & (pl.col("away_team").is_in(FBS_VoA["school"].implode()))).join(temp_ratings_df, on = "home_team", how = "left")
    ### FCS Games
    FCSGames = FullSeasonGames_df.filter((pl.col("home_team").is_in(FCS_VoA["school"].implode())) & (pl.col("away_team").is_in(FCS_VoA["school"].implode()))).join(temp_ratings_df, on = "home_team", how = "left")

    ### assigning ratings for away teams
    temp_ratings_df = temp_ratings_df.rename({
        "home_team": "away_team", 
        "home_VoA_rating": "away_VoA_rating"})
    FBSGames = FBSGames.join(temp_ratings_df, on = "away_team", how = "left")
    FCSGames = FCSGames.join(temp_ratings_df, on = "away_team", how = "left")

    ### Games where just 1 team from the VoA is involved, not counting games above
    NonVoAGames = FullSeasonGames_df.filter((pl.col("id").is_in(FBSGames["id"].implode()).not_()) & (pl.col("id").is_in(FCSGames["id"].implode()).not_()))

    ### setting temp ratings df for games between FBS and FCS teams and maybe D1 (FBS or FCS) teams and D2/D3 teams
    temp_ratings_df = AllD1VoA.select(["school", "VoA_Rating_Ovr"]).rename({
        "school": "home_team",
        "VoA_Rating_Ovr": "home_VoA_rating"})
    ### adding VoA ratings to home teams in NonVoAGames
    NonVoAGames = NonVoAGames.join(temp_ratings_df, on = "home_team", how = "left")
    ### assigning ratings for away teams
    temp_ratings_df = temp_ratings_df.rename({
        "home_team": "away_team", 
        "home_VoA_rating": "away_VoA_rating"})
    ### adding VoA ratings to away teams in NonVoAGames
    NonVoAGames = NonVoAGames.join(temp_ratings_df, on = "away_team", how = "left")

    ### rejoining all games backtogether to get FullSeason_Games with VoA Ratings attached
    random.seed(802)
    FullSeasonGames_df = pl.concat([FBSGames, FCSGames, NonVoAGames], how = "vertical").sort("week", descending = False).with_columns(
        home_VoA_rating = pl.col("home_VoA_rating").fill_null(np.random.normal(LowerQtrRatings['VoA_Rating_Ovr'].mean(), LowerQtrRatings['VoA_Rating_Ovr'].std(), size=len(FullSeasonGames_df))),
        away_VoA_rating = pl.col("away_VoA_rating").fill_null(np.random.normal(LowerQtrRatings['VoA_Rating_Ovr'].mean(), LowerQtrRatings['VoA_Rating_Ovr'].std(), size=len(FullSeasonGames_df)))
    )

elif int(upcoming) == 16:
    ### copy game stuff from above, change where needed
    games = games_api_instance.get_games(year = int(cfb_year), season_type = "postseason")
elif int(upcoming) > 16:
    games = games_api_instance.get_games(year = int(cfb_year), season_type = "postseason")
else:
    ### pulling games for any regular season week between 2 and 15 (inclusive)
    ### only including games where both teams are in VoA after week 1 since I am not trying to make full season win total projections except during week 1
    upcoming_games_df = pl.DataFrame(games_api_instance.get_games(year = int(cfb_year), week = int(upcoming)), infer_schema_length = None).select(
      ["id",
      "season",
      "week",
      "neutral_site",
      "home_team",
      "home_classification",
      "home_conference",
      "away_team",
      "away_classification",
      "away_conference"]).filter(
        (pl.col("home_team").is_in(PrevWeek_VoA['school'].implode())) & (pl.col("away_team").is_in(PrevWeek_VoA['school'].implode()))
      )
      ### setting initial temp df for assigning VoA ratings to games where both teams are in VoA
    temp_ratings_df = PrevWeek_VoA.select(["school", "VoA_Rating_Ovr"]).rename({
    "school": "home_team",
    "VoA_Rating_Ovr": "home_VoA_rating"})

    ### assigning ratings for home teams
    ### FBS Games
    FBSGames = upcoming_games_df.filter((pl.col("home_team").is_in(FBS_VoA["school"].implode())) & (pl.col("away_team").is_in(FBS_VoA["school"].implode()))).join(temp_ratings_df, on = "home_team", how = "left")
    ### FCS Games
    FCSGames = upcoming_games_df.filter((pl.col("home_team").is_in(FCS_VoA["school"].implode())) & (pl.col("away_team").is_in(FCS_VoA["school"].implode()))).join(temp_ratings_df, on = "home_team", how = "left")

    ### assigning ratings for away teams
    temp_ratings_df = temp_ratings_df.rename({
        "home_team": "away_team", 
        "home_VoA_rating": "away_VoA_rating"})
    FBSGames = FBSGames.join(temp_ratings_df, on = "away_team", how = "left")
    FCSGames = FCSGames.join(temp_ratings_df, on = "away_team", how = "left")

    ### Games where just 1 team from the VoA is involved, not counting games above
    NonVoAGames = upcoming_games_df.filter((pl.col("id").is_in(FBSGames["id"].implode()).not_()) & (pl.col("id").is_in(FCSGames["id"].implode()).not_()))

    ### setting temp ratings df for games between FBS and FCS teams and maybe D1 (FBS or FCS) teams and D2/D3 teams
    temp_ratings_df = AllD1VoA.select(["school", "VoA_Rating_Ovr"]).rename({
        "school": "home_team",
        "VoA_Rating_Ovr": "home_VoA_rating"})
    ### adding VoA ratings to home teams in NonVoAGames
    NonVoAGames = NonVoAGames.join(temp_ratings_df, on = "home_team", how = "left")
    ### assigning ratings for away teams
    temp_ratings_df = temp_ratings_df.rename({
        "home_team": "away_team", 
        "home_VoA_rating": "away_VoA_rating"})
    ### adding VoA ratings to away teams in NonVoAGames
    NonVoAGames = NonVoAGames.join(temp_ratings_df, on = "away_team", how = "left")

    ### rejoining all games backtogether to get FullSeason_Games with VoA Ratings attached
    random.seed(802)
    upcoming_games_df = pl.concat([FBSGames, FCSGames, NonVoAGames], how = "vertical").sort("id", descending = False).with_columns(
        home_VoA_rating = pl.col("home_VoA_rating").fill_null(np.random.normal(LowerQtrRatings['VoA_Rating_Ovr'].mean(), LowerQtrRatings['VoA_Rating_Ovr'].std(), size=len(upcoming_games_df))),
        away_VoA_rating = pl.col("away_VoA_rating").fill_null(np.random.normal(LowerQtrRatings['VoA_Rating_Ovr'].mean(), LowerQtrRatings['VoA_Rating_Ovr'].std(), size=len(upcoming_games_df)))
    )
    ### end of game pull if/else statement

##### Creating VoA Projected Margin and Projected Winner Columns #####
if int(upcoming) == 1:
    ### adding column to indicate predicted margin based on VoA ratings and whether the game is taking place at a neutral site
    FullSeasonGames_df = FullSeasonGames_df.with_columns(
        predicted = pl.when(pl.col("neutral_site").not_())
        .then(pl.col("away_VoA_rating") - (pl.col("home_VoA_rating") + 2))
        .otherwise(pl.col("away_VoA_rating") - pl.col("home_VoA_rating"))
    )

    ### filtering the df of the full season's games to get just this week's games for the CFBD pick'em contest
    cfbdata_contest_df = FullSeasonGames_df.filter(pl.col("week") == int(upcoming)).filter((pl.col('home_team').is_in(PrevWeek_VoA['school'].implode())) & (pl.col('away_team').is_in(PrevWeek_VoA['school'].implode()))).select(
        ["id", "home_team", "away_team", "predicted"]).rename(
            {"home_team": "home", "away_team": "away"})

    ### saving dataframe with columns formatted for proper submission to CFBData pick'em contest as a csv
    cfbdata_contest_df.write_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + cfb_year,
        "Projections",
        "CFBD",
        cfb_year + "VoPWeek" + upcoming + "Games.csv"
    ))
else:
    ### adding column to indicate predicted margin based on VoA ratings and whether the game is taking place at a neutral site
    cfbdata_contest_df = upcoming_games_df.with_columns(
        predicted = pl.when(~pl.col("neutral_site"))
        .then(pl.col("away_VoA_rating") - (pl.col("home_VoA_rating") + 2))
        .otherwise(pl.col("away_VoA_rating") - pl.col("home_VoA_rating"))
        ).select(
            ["id", "home_team", "away_team", "predicted"]).rename({
                "home_team": "home", "away_team": "away"})

    ### saving dataframe with columns formatted for proper submission to CFBData pick'em contest as a csv
    cfbdata_contest_df.write_csv(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + cfb_year,
        "Projections",
        "CFBD",
        cfb_year + "VoPWeek" + upcoming + "Games.csv"
    ))


### Function to calculate margin projection individually
def margin_projection(
    away: str, home: str, neutral: bool, prev_week_voa: pl.DataFrame
) -> float:
    away_rating = (
        prev_week_voa.filter(pl.col("school") == away)
        .select("VoA_Rating_Ovr")
        .item()
    )
    home_rating = (
        prev_week_voa.filter(pl.col("school") == home)
        .select("VoA_Rating_Ovr")
        .item()
    )

    margin_proj = away_rating - home_rating
    if not neutral:
        margin_proj -= 2

    return margin_proj
    ### end of function


##### Creating Win Probability Model using historical SP+ data #####
### reading in historical sp+ win probability data
SP_WPdata = pl.read_csv(os.path.join(os.getcwd(), "Data", "SP_Projections", "All_SP.csv"))

### preparing data for modeling
SP_WPdata = (
    SP_WPdata.with_columns(
        ### Separate 'Game' column on ' at ' into away_team and home_team
        pl.col("Game")
        .str.split_exact(" at ", 1)
        .struct.rename_fields(["away_team", "home_team"])
    )
    .unnest("Game")
    ### Drop missing values in the team columns
    .drop_nulls(subset=["away_team", "home_team"])
    ### Filter where home_team or away_team matches Proj_winner
    .filter(
        (pl.col("home_team") == pl.col("Proj_winner"))
        | (pl.col("away_team") == pl.col("Proj_winner"))
    )
    ### add columns on projected win prob and projected margin
    .with_columns(
        away_WP_pct = pl.when(pl.col("Proj_winner") == pl.col("away_team"))
        .then(pl.col("WP_pct"))
        .otherwise(1 - pl.col("WP_pct")),
        Proj_Margin = pl.when(pl.col("Proj_winner") == pl.col("away_team"))
        .then(pl.col("Proj_margin"))
        .otherwise(-1 * pl.col("Proj_margin")),
    ).with_columns(
        away_WP_pct = pl.when(pl.col("away_WP_pct") == 0).then(pl.col("away_WP_pct") + 0.0001).otherwise(pl.col("away_WP_pct"))
    )
)


##### Fitting a logistic regression model using old VoA projections #####
PrevVoAPreds = pl.read_csv(os.path.join(os.getcwd(), "Data", "VoA" + str(int(cfb_year) - 1), "AccuracyMetrics", "Games", "VoA" + str(int(cfb_year) - 1) + "Week1Week20GameAccuracyMetrics.csv")).select(["proj_margin", "straight_up_win"])
# poopypants2 = pl.read_csv(os.path.join(os.getcwd(), "Data", "VoA" + cfb_year, "AccuracyMetrics", "Games", "VoA" + cfb_year + "Week1" + "Week" + upcoming + "GameAccuracyMetrics.csv")).select("proj_margin", "straight_up_win")

WinProb_x = PrevVoAPreds['proj_margin'].abs()
WinProb_y = PrevVoAPreds['straight_up_win']

### setting random seed for reproducability and fitting glm (Binomial family / Logistic Regression)
random.seed(802)
WinProb_glm = sm.GLM(WinProb_y.to_numpy(), WinProb_x.to_numpy(), family=sm.families.Binomial()).fit()

### model summary
print(WinProb_glm.summary())


##### Adding Projected Winner, projected margin, and win probability columns
if int(upcoming) == 1:
    ### Add Proj_Winner and Proj_Margin columns
    FullSeasonGames_df = FullSeasonGames_df.with_columns(
        Proj_Winner=pl.when(
            (~pl.col("neutral_site"))
            & ((pl.col("home_VoA_rating") + 2) > pl.col("away_VoA_rating"))
        )
        .then(pl.col("home_team"))
        .when(
            (~pl.col("neutral_site"))
            & (pl.col("away_VoA_rating") > (pl.col("home_VoA_rating") + 2))
        )
        .then(pl.col("away_team"))
        .when(
            (pl.col("neutral_site"))
            & (pl.col("home_VoA_rating") > pl.col("away_VoA_rating"))
        )
        .then(pl.col("home_team"))
        .when(
            (pl.col("neutral_site"))
            & (pl.col("away_VoA_rating") > pl.col("home_VoA_rating"))
        )
        .then(pl.col("away_team"))
        .otherwise(pl.lit("TIE")),
        Proj_Margin=pl.when(~pl.col("neutral_site"))
        .then((pl.col("away_VoA_rating") - (pl.col("home_VoA_rating") + 2)).abs())
        .otherwise((pl.col("away_VoA_rating") - pl.col("home_VoA_rating")).abs()),
    )

    ### adding win_prob column via model predict and select columns
    random.seed(802)
    WinProb_preds = WinProb_glm.predict(FullSeasonGames_df['Proj_Margin'].abs().to_numpy())
    FullSeasonGames_df = (
        FullSeasonGames_df.with_columns(
            win_prob=pl.Series(WinProb_preds)
        ).select(
            [
                "id",
                "season",
                "week",
                "neutral_site",
                "home_team",
                "home_classification",
                "home_conference",
                "home_VoA_rating",
                "away_team",
                "away_classification",
                "away_conference",
                "away_VoA_rating",
                "Proj_Winner",
                "Proj_Margin",
                "win_prob",
            ]
        )
    )

    ### Filter full season games df to just include games for the current upcoming week
    upcoming_games_df = FullSeasonGames_df.filter(pl.col("week") == int(upcoming)).filter(
        (pl.col('home_team').is_in(PrevWeek_VoA['school'].implode())) & (pl.col('away_team').is_in(PrevWeek_VoA['school'].implode()))
    )

    ### saving out files so I can make the gt tables in R
    FullSeasonGames_df.write_parquet(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + cfb_year,
        "Projections",
        "FullSeason" + cfb_year + "GamesPreds.parquet"
    ))

    upcoming_games_df.write_parquet(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + cfb_year,
        "Projections",
        cfb_year + "VoPWeek" + upcoming + "Games.parquet"
    ))

else:
    ### Add Proj_Winner and Proj_Margin columns
    upcoming_games_df = upcoming_games_df.with_columns(
        Proj_Winner=pl.when(
            (~pl.col("neutral_site"))
            & ((pl.col("home_VoA_rating") + 2) > pl.col("away_VoA_rating"))
        )
        .then(pl.col("home_team"))
        .when(
            (~pl.col("neutral_site"))
            & (pl.col("away_VoA_rating") > (pl.col("home_VoA_rating") + 2))
        )
        .then(pl.col("away_team"))
        .when(
            (pl.col("neutral_site"))
            & (pl.col("home_VoA_rating") > pl.col("away_VoA_rating"))
        )
        .then(pl.col("home_team"))
        .when(
            (pl.col("neutral_site"))
            & (pl.col("away_VoA_rating") > pl.col("home_VoA_rating"))
        )
        .then(pl.col("away_team"))
        .otherwise(pl.lit("TIE")),
        Proj_Margin=pl.when(~pl.col("neutral_site"))
        .then((pl.col("away_VoA_rating") - (pl.col("home_VoA_rating") + 2)).abs())
        .otherwise((pl.col("away_VoA_rating") - pl.col("home_VoA_rating")).abs()),
    )

    ### adding win_prob column via model predict and select columns
    random.seed(802)
    WinProb_preds = WinProb_glm.predict(upcoming_games_df['Proj_Margin'].abs().to_numpy())
    upcoming_games_df = (
        upcoming_games_df.with_columns(
            win_prob=pl.Series(WinProb_preds)
        ).select(
            [
                "id",
                "season",
                "week",
                "neutral_site",
                "home_team",
                "home_classification",
                "home_conference",
                "home_VoA_rating",
                "away_team",
                "away_classification",
                "away_conference",
                "away_VoA_rating",
                "Proj_Winner",
                "Proj_Margin",
                "win_prob",
            ]
        )
    ).filter(
        (pl.col('home_team').is_in(PrevWeek_VoA['school'].implode())) & (pl.col('away_team').is_in(PrevWeek_VoA['school'].implode()))
    )

    ### saving out file so I can make the gt tables in R
    upcoming_games_df.write_parquet(os.path.join(
        os.getcwd(),
        "Data",
        "VoA" + cfb_year,
        "Projections",
        cfb_year + "VoPWeek" + upcoming + "Games.parquet"
    ))




##### POOPYPANTS BULLSHIT BELOW HERE #####