require(tidyverse)
require(data.table)
require(stringr)
require(stringi)

convert538Teams <- function(df538){
  numVec <- sapply(df538, class) %>% sapply(function(x) x[1])  %>% unlist() %>% unname() %in% c('integer', 'numeric')
  
  newNames <- lapply(df538, function(x) stri_replace_all_regex(x,
                                                         pattern = c('NYY','SDP','LAD','NYM','STL','CHC','CHW','FLA','KCR','SFG','TBD','WSN'),
                                                         replacement = c('NYA','SDN','LAN','NYN','SLN','CHN','CHA','FLO','KCA','SFN','TBA','WAS'),
                                                         vectorize = F)) %>%
    data.frame()
  
  newNames[,numVec] <- apply(newNames[,numVec], 2, function(x) as.numeric(x))
  
  return(newNames)  
}

mlbSchedule2023 <- fread('data/2023_MLBSchedule.csv')
originalSchedules <- read.csv('data/OriginalSchedules.csv', fileEncoding = 'UTF-16')
gameLogs <- fread('data/smallGameLogs.csv') %>%
  select(-V1)
elo538 <- fread('data/mlb_elo.csv') %>%
  convert538Teams()



