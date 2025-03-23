### This script is designed to read in Bill Connelly's SP+ csvs, select the game, winner, 
# margin and win probability columns, string together each week's csv into 1 data frame,
# export data frame as csv, and read in that csv in the VoP script to derive a
# VoA-based Win Probability metric based on Vortex of Projection's projected margin for
# each game in a given week
# Essentially, I'm trying to use Bill Connelly's win probabilities as training data
# currently I'm using lm() with a quadratic formula of Bill's Proj_margins
# the coefficients are then grabbed, stored in a data frame, applied in VoP script
# VoP script should be run immediately after this script with all the objects retained in the global environment
## Created by Griffin Shelor
## installing packages
# install.packages(c("devtools", "tidyverse", "matrixStats", "gt", "viridis", "webshot", "rvest", "cfbfastR", "here", "ggsci", "RColorBrewer", "ggpubr", "remotes", "pacman", "gtExtras", cfbplotR))
## Load Packages for Ranking Variables
library(pacman)
pacman::p_load(tidyverse, gt, here, RColorBrewer, gtExtras, ggpubr, Metrics)

## Reading in csvs
# some commented out until that week's projections are released
week1_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek1FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week2_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek2FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week3_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek3FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week4_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek4FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week5_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek5FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week6_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek6FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week7_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek7FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week8_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek8FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week9_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek9FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week10_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek10FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week11_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek11FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week12_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek12FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week13_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek13FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week15_sp <- read_csv(here("Data", "SP_Projections", "2022SPWeek15FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week12023_sp <- read_csv(here("Data", "SP_Projections", "2023SPWeek1FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week02024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek0FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week12024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek1FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week22024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek2FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week32024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek3FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week42024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek4FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week52024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek5FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week62024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek6FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week72024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek7FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week82024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek8FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week92024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek9FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week102024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek10FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week112024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek11FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week122024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek12FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week132024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek13FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week142024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek14FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
week152024_sp <- read_csv(here("Data", "SP_Projections", "2024SPWeek15FBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)
Bowls2024_sp <- read_csv(here("Data", "SP_Projections", "2024SPBowlPicksFBS.csv")) |>
  select(Game, Proj_winner, Proj_margin, Win_prob)

## combining weekly SP+ data frames to get one df of games and SP+ proj margins and win probabilities
all_sp <- rbind(week1_sp, week2_sp)
all_sp <- rbind(all_sp, week3_sp)
all_sp <- rbind(all_sp, week4_sp)
all_sp <- rbind(all_sp, week5_sp)
all_sp <- rbind(all_sp, week6_sp)
all_sp <- rbind(all_sp, week7_sp)
all_sp <- rbind(all_sp, week8_sp)
all_sp <- rbind(all_sp, week9_sp)
all_sp <- rbind(all_sp, week10_sp)
all_sp <- rbind(all_sp, week11_sp)
all_sp <- rbind(all_sp, week12_sp)
all_sp <- rbind(all_sp, week13_sp)
all_sp <- rbind(all_sp, week15_sp)
all_sp <- rbind(all_sp, week12023_sp)
all_sp <- rbind(all_sp, week02024_sp)
all_sp <- rbind(all_sp, week12024_sp)
all_sp <- rbind(all_sp, week22024_sp)
all_sp <- rbind(all_sp, week32024_sp)
all_sp <- rbind(all_sp, week42024_sp)
all_sp <- rbind(all_sp, week52024_sp)
all_sp <- rbind(all_sp, week62024_sp)
all_sp <- rbind(all_sp, week72024_sp)
all_sp <- rbind(all_sp, week82024_sp)
all_sp <- rbind(all_sp, week92024_sp)
all_sp <- rbind(all_sp, week102024_sp)
all_sp <- rbind(all_sp, week112024_sp)
all_sp <- rbind(all_sp, week122024_sp)
all_sp <- rbind(all_sp, week132024_sp)
all_sp <- rbind(all_sp, week142024_sp)
all_sp <- rbind(all_sp, week152024_sp)
all_sp <- rbind(all_sp, Bowls2024_sp)

## removing nas
all_sp <- all_sp |>
  drop_na()

## converting win probs into numeric format, dropping % sign
all_sp <- all_sp |>
  separate(col = Win_prob, into = "Win_prob",sep = "%")
all_sp$Win_prob <- as.numeric(all_sp$Win_prob)
all_sp <- all_sp |>
  mutate(WP_pct = Win_prob / 100)

## exporting all_sp as csv
write_csv(all_sp, here("Data", "SP_Projections", "All_SP.csv"))
