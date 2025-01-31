library(tidyverse)
library(baseballr)
library(httr)

# Set variables for the season to scrape and the start and end date for that season
season <- 2020
season_start_date <- '2020-07-23'
season_end_date <- '2020-09-27'

# Function to grab CSV pitch-by-pitch data from baseball savant and save them into temporary files
getStatcastPitchByPitch <- function(date_in, date_out) {
  url <- paste0('https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC&hfSea=', season, '%7C&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&player_type=batter&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&game_date_gt=', date_in, '&game_date_lt=', date_out, '&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details')
  download.file(url, destfile = paste0('data/temp/temp-savant-', date_in, '-', date_out, '.csv'), mode = "wb")
}

# Create an array of dates from the first to last day of the season
season_dates <- seq.Date(as.Date(season_start_date), as.Date(season_end_date), by = 'day')

# Loop through the array of dates and use the function to download pitch-by-pitch data in three data increments
lapply(1:(length(season_dates) / 3), function(i) {
  getStatcastPitchByPitch(season_dates[i * 3 - 2], season_dates[i * 3])
})

# Read in all of the downloaded temp files and combine them into one data.frame, filtered by regular season only
savant_pitch_by_pitch <- lapply(list.files(path = 'data/temp', pattern = "^temp.*\\.csv$", full.names = TRUE), function(i) {
  read.csv(i)
}) %>%
  bind_rows() %>%
  filter(game_type == 'R')

# Save the data.frame of pitches for the season into an RDS file labeled by the season
saveRDS(savant_pitch_by_pitch, paste0('data/savant_pbp', season, '.rds'))

# Delete the downloaded temp files
file.remove(list.files(path = 'data/temp', pattern = "^temp.*\\.csv$", full.names = TRUE))





