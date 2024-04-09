################################################################################
# The following code cleans the raw data collected from Basketball Reference   #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

# Load packages for data cleaning:
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
# source("main/data_collection.R")


# A function to rename column names to correct headers:
rename_column = function(df){
  
  df = df %>%
    rename(
      `FG%` = "FG.",
      `3P` = "X3P",
      `3PA` = "X3PA",
      `3P%` = "X3P.",
      `2P` = "X2P",
      `2PA` = "X2PA",
      `2P%` = "X2P.",
      `eFG%` = "eFG.",
      `FT%` = "FT."
    )
  
  return(df)
  
}

rename_team_column = function(df){
  
  df = df %>%
    rename(
      `FG%` = "FG.",
      `3P` = "X3P",
      `3PA` = "X3PA",
      `3P%` = "X3P.",
      `2P` = "X2P",
      `2PA` = "X2PA",
      `2P%` = "X2P.",
      `FT%` = "FT."
    )
  
  return(df)
  
}


# A function to remove the same rows to the headers:
remove_duplicated_headers = function(df){
  
  df = df %>%
    filter(Rk != "Rk")
  
  return(df)
  
}

remove_duplicated_team_headers = function(df){
  
  df = df %>%
    filter(!is.na(Rk))
  
  return(df)
  
}

# A function to remove current headers:
remove_and_set_header = function(df) {
  
  # Remove current header
  new_df = df[-1, ]
  
  # Set the first row as the new header
  colnames(new_df) = unlist(df[1, ])
  
  return(new_df)
  
}


# A function to remove special symbols like * from the data frame:
remove_symbols = function(df){
  
  df = df %>%
    mutate_all(~ str_replace_all(., "[!@#$%^&*~]", ""))
  
  return(df)
  
}


# A function to modify the season column to the correct years:
update_season = function(df, start_year){
  
  df$Season = as.numeric(df$Season) + start_year - 1
  
  return(df)
  
}

# A function to drop unnecessary columns in advanced data:
drop_unnecessary_columns = function(df) {
  
  return(df[, 1:16, drop = FALSE])
  
}


# A function to remove duplicated players who played in different teams in the same season:
remove_traded_player_duplicates = function(df){
  
  df_filtered = df %>%
    group_by(Season, Player, Age) %>%
    filter(G == max(as.numeric(G))) %>%
    ungroup()
  
  return(df_filtered)
  
}


# Automated Data Cleaning Process:
auto_cleaning_player_data = function(df, start_year){
  
  print("Renaming all columns...")
  df = rename_column(df)
  
  print("Removing duplicated headers...")
  df = remove_duplicated_headers(df)
  
  print("Removing unnecessary symbols...")
  df = remove_symbols(df)
  
  print("Updating the seasons...")
  df = update_season(df, start_year)
  
  print("Removing duplicated players due to trades...")
  df = remove_traded_player_duplicates(df)
  
  print("Finished data cleaning")
  
  return(df)
  
}

auto_cleaning_team_data = function(df, start_year){
  
  print("Renaming all columns...")
  df = rename_team_column(df)
  
  print("Removing duplicated headers...")
  df = remove_duplicated_team_headers(df)
  
  print("Removing unnecessary symbols...")
  df = remove_symbols(df)
  
  print("Updating the seasons...")
  df = update_season(df, start_year)
  
  print("Finished data cleaning")
  
  return(df)
  
}

auto_cleaning_advanced_data = function(df, start_year){
  
  print("Dropping unnecessary columns...")
  df = drop_unnecessary_columns(df)
  
  print("Removing current headers...")
  df = remove_and_set_header(df)
  
  colnames(df)[1] = "Season"
  
  print("Removing duplicated headers...")
  df = remove_duplicated_headers(df)
  
  print("Removing unnecessary symbols...")
  df = remove_symbols(df)
  
  print("Updating the seasons...")
  df = update_season(df, start_year)
  
  print("Finished data cleaning")
  
  return(df)
  
}


# -----------------------------------------------------------------------------------------------


# Two ways to load data: read_csv or call the function from data_collection.R

# Example: clean all the player data and merge the datasets:

player_stats_1 = read_csv("data/player_stats_1.csv")
player_stats_2 = read_csv("data/player_stats_2.csv")
player_stats_3 = read_csv("data/player_stats_3.csv")
player_stats_4 = read_csv("data/player_stats_4.csv")

player_stats_1_cleaned = auto_cleaning_player_data(player_stats_1, start_year = 1977)
player_stats_2_cleaned = auto_cleaning_player_data(player_stats_2, start_year = 1991)
player_stats_3_cleaned = auto_cleaning_player_data(player_stats_3, start_year = 2001)
player_stats_4_cleaned = auto_cleaning_player_data(player_stats_4, start_year = 2011)

player_stats = bind_rows(list(player_stats_1_cleaned, player_stats_2_cleaned, 
                              player_stats_3_cleaned, player_stats_4_cleaned))

# Optional: store in csv files for future use

write_csv(player_stats, "data/player_stats.csv")

# -----------------------------------------------------------------------------------------------

# Same thing for the team data:

team_per_game_1 = read_csv("data/team_per_game_stats_1.csv")
team_per_game_2 = read_csv("data/team_per_game_stats_2.csv")
team_per_game_3 = read_csv("data/team_per_game_stats_3.csv")
team_per_game_4 = read_csv("data/team_per_game_stats_4.csv")

team_per_game_1_cleaned = auto_cleaning_team_data(team_per_game_1, start_year = 1977)
team_per_game_2_cleaned = auto_cleaning_team_data(team_per_game_2, start_year = 1991)
team_per_game_3_cleaned = auto_cleaning_team_data(team_per_game_3, start_year = 2001)
team_per_game_4_cleaned = auto_cleaning_team_data(team_per_game_4, start_year = 2011)

team_per_game_stats = bind_rows(list(team_per_game_1_cleaned, team_per_game_2_cleaned, 
                                     team_per_game_3_cleaned, team_per_game_4_cleaned))

# Optional: store in csv files for future use

write_csv(team_per_game_stats, "data/team_per_game_stats.csv")

# -----------------------------------------------------------------------------------------------

# Same thing for the team advanced data:

team_advanced_1 = read_csv("data/team_advanced_stats_1.csv")
team_advanced_2 = read_csv("data/team_advanced_stats_2.csv")
team_advanced_3 = read_csv("data/team_advanced_stats_3.csv")
team_advanced_4 = read_csv("data/team_advanced_stats_4.csv")

team_advanced_1_cleaned = auto_cleaning_advanced_data(team_advanced_1, start_year = 1977)
team_advanced_2_cleaned = auto_cleaning_advanced_data(team_advanced_2, start_year = 1991)
team_advanced_3_cleaned = auto_cleaning_advanced_data(team_advanced_3, start_year = 2001)
team_advanced_4_cleaned = auto_cleaning_advanced_data(team_advanced_4, start_year = 2011)

team_advanced_stats = bind_rows(list(team_advanced_1_cleaned, team_advanced_2_cleaned, 
                                     team_advanced_3_cleaned, team_advanced_4_cleaned))

# Optional: store in csv files for future use

write_csv(team_advanced_stats, "data/team_advanced_stats.csv")

