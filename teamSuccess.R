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

gameNum <- elo538Small %>%
  select(date, team1, season, elo1_pre, rowN) %>%
  rename('team' = 'team1',
         'elo' = 'elo1_pre') %>% 
  rbind(elo538Small %>%
          select(date, team2, season, elo2_pre, rowN) %>%
          rename('team' = 'team2','elo' = 'elo2_pre')) %>%
  arrange(desc(rowN)) %>%
  group_by(season, team) %>%
  mutate(gameNum = 1:n()) %>%
  mutate(Date = gsub('-','', date)) %>%
  ungroup() %>%
  select(Date, date, team, gameNum, elo)

fullTeamPerformance <- elo538wInit %>%
  left_join(.,gameNum %>% select(date, gameNum, team, elo), by = c('date' = 'date', 'team1' = 'team', 'elo1_pre' = 'elo')) %>%
  left_join(., gameNum %>% select(date, gameNum, team, elo), by = c('date' = 'date', 'team2' = 'team', 'elo2_pre' = 'elo')) %>%
  rename('gameNum1' = 'gameNum.x',
         'gameNum2' = 'gameNum.y') %>%
  left_join(., elo538 %>% select(date, team1, team2, elo1_pre, elo2_pre, score1, score2), by = c('date' = 'date', 'team1' = 'team1', 'team2'='team2','elo1_pre' = 'elo1_pre', 'elo2_pre' = 'elo2_pre')) %>%
  mutate(teamPerfLogistic = eloDiff1 * logisticFunction(middleVal = 50, steepness = .05, x = gameNum2),
         date = as.numeric(gsub('-','',date))) %>%
  inner_join(.,gameLogs %>% select(Date, Attendance, HomeTeam, VisitingTeam, VistingTeamScore, HomeTeamScore), 
            by = c('date' = 'Date', 'team1' = 'HomeTeam', 'team2' = 'VisitingTeam', 'score1' = 'HomeTeamScore', 'score2' = 'VistingTeamScore')) %>% ###14 DUPES PLEASE GO BACK AND CHECK LATER (CAUSED BY DOUBLE HEADERS W SAME SCORE) -- wait maybe not but still weird
  group_by(team1, season) %>%
  mutate(avgAtt = mean(Attendance)) %>%
  ungroup() %>%
  mutate(diffFromAvgAtt = Attendance - avgAtt)

ggplot(fullTeamPerformance, aes(teamPerfLogistic, diffFromAvgAtt)) + geom_point() + geom_smooth(method = 'lm', se = F)