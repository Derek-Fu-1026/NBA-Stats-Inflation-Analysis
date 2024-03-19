################################################################################
# The following code extracts a sub-dataset needed for modeling and analysis   #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

# Load packages for data extraction:
library(tidyverse)
library(dplyr)
library(readr)

# You can optionally call functions inside these files to get new data:
# source("main/data_collection.R")
# source("main/data_cleaning.R")

# A function to get the top n scorers in each season:
get_top_n_scorers = function(df, n_players) {
  
  top_scorers = df %>%
    group_by(Season) %>%
    mutate(Total_PTS = as.integer(PTS * G)) %>%
    slice_max(order_by = Total_PTS, n = n_players) %>%
    arrange(Season, desc(Total_PTS))
  
  return(top_scorers)
  
}

# A function to get the sum of total points by the top n scorers in each season:
get_top_scorers_sum = function(df) {
  
  top_scorers_sum = df %>%
    group_by(Season) %>%
    summarise(Total_PTS = sum(Total_PTS))
  
  return(top_scorers_sum)
  
}

# A function to get the average points per game in each season:
get_team_average_points = function(df){
  
  average_points = df %>%
    group_by(Season) %>%
    summarise(AVG_PTS = mean(as.numeric(PTS))) %>%
    arrange(Season)
  
  return(average_points)
  
}



