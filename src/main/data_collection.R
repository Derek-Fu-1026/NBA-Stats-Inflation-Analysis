################################################################################
# The following code aims to web-scrape player and team data needed from       #
# Basketball Reference.                                                        #
################################################################################

# Load packages for web-scraping:
library(rvest)
library(dplyr)

# A function to web-scrape per-game stats for a given season:
scrape_player_stats = function(year) {
  
  # Specify the urls:
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_game.html")
  webpage = read_html(url)
  
  # Extract the table:
  table = html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]

  return(table)
  
}

# A function to web-scrape data for specified seasons and create a new data frame:
get_player_stats = function(from, to){
  
  player_stats = lapply(from:to, scrape_player_stats)
  
  # Merge all the data:
  player_stats_df = bind_rows(player_stats, .id = "Season")
  
  return(player_stats_df)
  
}


