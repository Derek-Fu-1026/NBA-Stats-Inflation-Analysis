################################################################################
# The following code aims to web-scrape player and team data needed from       #
# Basketball Reference.                                                        #
################################################################################

setwd("D:/NBA-Stats-Inflation/src")

# Load packages for web-scraping:
library(rvest)
library(dplyr)
library(readr)


# A function to web-scrape per-game player stats for given seasons:
get_player_stats = function(start_year, end_year) {
  
  all_seasons_stats = list()
  
  for (year in start_year:end_year) {
    
    url = paste0('https://www.basketball-reference.com/leagues/NBA_', year, '_per_game.html')
    
    # Read the HTML content from the URL
    page = read_html(url)
    
    # Extract the HTML table
    table = page %>%
      html_nodes('table#per_game_stats') %>%
      html_table()
    
    # Convert the list to a data frame
    df = as.data.frame(table)
    
    # Store the data frame in a list
    all_seasons_stats[[year]] = df
    
    player_stats_df = bind_rows(all_seasons_stats, .id = "Season")
    
  }
  
  return(player_stats_df)
  
}


# A function to web-scrape per-game team stats for given seasons:
get_team_stats_per_game = function(start_year, end_year) {
  
  all_seasons_stats = list()
  
  for (year in start_year:end_year) {
    
    url = paste0('https://www.basketball-reference.com/leagues/NBA_', year, '.html')
    
    # Read the HTML content from the URL
    page = read_html(url)
    
    # Extract the HTML table
    table = page %>%
      html_nodes('table#per_game-team') %>%
      html_table()
    
    # Convert the list to a data frame
    df = as.data.frame(table)
    
    # Store the data frame in a list
    all_seasons_stats[[year]] = df
    
    team_per_game_stats_df = bind_rows(all_seasons_stats, .id = "Season")
    
  }
  
  return(team_per_game_stats_df)
  
}


# A function to web-scrape advanced team stats for given seasons:
get_team_stats_advanced = function(start_year, end_year) {
  
  all_seasons_stats = list()
  
  for (year in start_year:end_year) {
    
    url = paste0('https://www.basketball-reference.com/leagues/NBA_', year, '.html')
    
    # Read the HTML content from the URL
    page = read_html(url)
    
    # Extract the HTML table
    table = page %>%
      html_nodes('table#advanced-team') %>%
      html_table()
    
    # Convert the list to a data frame
    df = as.data.frame(table)
    
    # Store the data frame in a list
    all_seasons_stats[[year]] = df
    
    team_advanced_stats_df = bind_rows(all_seasons_stats, .id = "Season")
    
  }
  
  return(team_advanced_stats_df)
  
}

# -----------------------------------------------------------------------------------------------

# Example: scrape all the player per-game data from 1977 to 2024 (before All-Stars)
# Separate into 4 different data frames to avoid having problems requesting access to the website.

player_stats_1 = get_player_stats(1977, 1990)
player_stats_2 = get_player_stats(1991, 2000)
player_stats_3 = get_player_stats(2001, 2010)
player_stats_4 = get_player_stats(2011, 2024)

# Optional: store them in csv files for future use

# write_csv(player_stats_1, "data/player_stats_1.csv")
# write_csv(player_stats_2, "data/player_stats_2.csv")
# write_csv(player_stats_3, "data/player_stats_3.csv")
# write_csv(player_stats_4, "data/player_stats_4.csv")

# -----------------------------------------------------------------------------------------------

# Example: scrape all the team per-game data from 1977 to 2024
# Separate into 4 different data frames to avoid having problems requesting access to the website.

team_per_game_stats_1 = get_team_stats_per_game(1977, 1990)
team_per_game_stats_2 = get_team_stats_per_game(1991, 2000)
team_per_game_stats_3 = get_team_stats_per_game(2001, 2010)
team_per_game_stats_4 = get_team_stats_per_game(2011, 2024)

# Optional: store them in csv files for future use

# write_csv(team_per_game_stats_1, "data/team_per_game_stats_1.csv")
# write_csv(team_per_game_stats_2, "data/team_per_game_stats_2.csv")
# write_csv(team_per_game_stats_3, "data/team_per_game_stats_3.csv")
# write_csv(team_per_game_stats_4, "data/team_per_game_stats_4.csv")

# -----------------------------------------------------------------------------------------------

# Example: scrape all the team advanced data from 1977 to 2024
# Separate into 4 different data frames to avoid having problems requesting access to the website.

team_advanced_stats_1 = get_team_stats_advanced(1977, 1990)
team_advanced_stats_2 = get_team_stats_advanced(1991, 2000)
team_advanced_stats_3 = get_team_stats_advanced(2001, 2010)
team_advanced_stats_4 = get_team_stats_advanced(2011, 2024)

# Optional: store them in csv files for future use

write_csv(team_advanced_stats_1, "data/team_advanced_stats_1.csv")
write_csv(team_advanced_stats_2, "data/team_advanced_stats_2.csv")
write_csv(team_advanced_stats_3, "data/team_advanced_stats_3.csv")
write_csv(team_advanced_stats_4, "data/team_advanced_stats_4.csv")
