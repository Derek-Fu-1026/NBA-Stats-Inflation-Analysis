################################################################################
# The following code cleans the raw data collected from Basketball Reference   #
################################################################################

# Load packages for data cleaning:
library(tidyverse)
library(dplyr)
source("data_collection.R")

player_stats = get_player_stats(1970, 2024)

View(player_stats)