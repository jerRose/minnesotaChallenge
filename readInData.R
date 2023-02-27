require(tidyverse)
require(data.table)
require(stringr)

mlbSchedule2023 <- fread('data/2023_MLBSchedule.csv')
originalSchedules <- read.csv('data/OriginalSchedules.csv', fileEncoding = 'UTF-16')
gameLogs <- fread('data/smallGameLogs.csv') %>%
  select(-V1)
elo538 <- fread('data/mlb_elo.csv')
