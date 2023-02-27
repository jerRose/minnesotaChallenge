##expected wins
logisticFunction <- function(maxVal = 1, middleVal, steepness, x){
  y <- maxVal / (1 + exp(-steepness * (x - middleVal)))
  return(y)
}

elo538Small <- elo538 %>%
  filter(season > 2000) %>%
  select(date, elo1_pre, elo2_pre, season, team1, team2) %>%
  mutate(rowN = 1:n())

startOfSeasonElo <- elo538Small %>%
  select(date, team1, season, elo1_pre, rowN) %>%
  rename('team' = 'team1',
        'elo' = 'elo1_pre') %>% 
  rbind(elo538Small %>%
          select(date, team2, season, elo2_pre, rowN) %>%
          rename('team' = 'team2','elo' = 'elo2_pre')) %>%
  arrange(rowN) %>%
  group_by(season, team) %>%
  summarise(across(everything(), last)) %>%
  select(season, team, elo)

elo538wInit <- elo538Small %>%
  left_join(.,startOfSeasonElo, by = c('season','team1' = 'team')) %>%
  left_join(.,startOfSeasonElo, by = c('season','team2' = 'team')) %>%
  select(-rowN) %>%
  rename('elo1PreSeason' = 'elo.x',
         'elo2PreSeason' = 'elo.y') %>%
  mutate(eloDiff1 = elo1_pre - elo1PreSeason,
         eloDiff2 = elo2_pre - elo2PreSeason) 






