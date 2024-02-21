################################################################################
# The following code cleans the raw data collected from Basketball Reference   #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

# Load packages for data cleaning:
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
source("main/data_collection.R")


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


# A function to remove the same rows to the headers:
remove_duplicated_headers = function(df){
  
  df = df %>%
    filter(Rk != "Rk")
  
  return(df)
  
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


# A function to remove duplicated players who played in different teams in the same season:
remove_traded_player_duplicates = function(df){
  
  df_filtered = df %>%
    group_by(Season, Player, Age) %>%
    filter(G == max(as.numeric(G))) %>%
    ungroup()
  
  return(df_filtered)
  
}


# Automated Data Cleaning Process:
auto_cleaning_player_data = function(df){
  
  
  
}




# Two ways to load data: read_csv or call the function from data_collection.R
