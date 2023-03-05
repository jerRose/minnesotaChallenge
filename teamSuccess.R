##expected wins
require(furrr)
require(rgl)
plan(multisession)

logisticFunction <- function(maxVal = 1, middleVal, steepness, x){
  y <- maxVal / (1 + exp(-steepness * (x - middleVal)))
  return(y)
}

elo538Small <- elo538 %>%
  filter(season > 2000) %>%
  select(date, elo1_pre, elo2_pre, season, team1, team2, score1, score2) %>%
  mutate(rowN = 1:n(),
         team1win = ifelse(score1 > score2, 1, 0),
         team2win = ifelse(score2 > score1, 1, 0)) %>%
  select(-c(score1,score2))

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
  select(date, team1, season, elo1_pre, rowN, team1win) %>%
  rename('team' = 'team1',
         'elo' = 'elo1_pre',
         'win' = 'team1win') %>% 
  rbind(elo538Small %>%
          select(date, team2, season, elo2_pre, rowN, team2win) %>%
          rename('team' = 'team2','elo' = 'elo2_pre', 'win' = 'team2win')) %>%
  arrange(desc(rowN)) %>%
  group_by(season, team) %>%
  mutate(gameNum = 1:n()) %>%
  mutate(Date = gsub('-','', date)) %>%
  mutate(numWins = cumsum(win),
         winPerc = numWins/gameNum) %>%
  select(Date, date, team, gameNum, elo, numWins, winPerc)

fullTeamPerformance <- elo538wInit %>%
  left_join(.,gameNum %>% select(date, gameNum, team, elo, numWins, winPerc), by = c('date' = 'date', 'team1' = 'team', 'elo1_pre' = 'elo')) %>%
  left_join(., gameNum %>% select(date, gameNum, team, elo, numWins, winPerc), by = c('date' = 'date', 'team2' = 'team', 'elo2_pre' = 'elo')) %>%
  rename('gameNum1' = 'gameNum.x',
         'gameNum2' = 'gameNum.y') %>%
  left_join(., elo538 %>% select(date, team1, team2, elo1_pre, elo2_pre, score1, score2), by = c('date' = 'date', 'team1' = 'team1', 'team2'='team2','elo1_pre' = 'elo1_pre', 'elo2_pre' = 'elo2_pre')) %>%
  mutate(teamPerfLogistic = eloDiff1 * logisticFunction(middleVal = 91, steepness = .075, x = gameNum2),
         date = as.numeric(gsub('-','',date))) %>%
  inner_join(.,gameLogs %>% select(Date, Attendance, HomeTeam, VisitingTeam, VistingTeamScore, HomeTeamScore, DayofWeek), 
            by = c('date' = 'Date', 'team1' = 'HomeTeam', 'team2' = 'VisitingTeam', 'score1' = 'HomeTeamScore', 'score2' = 'VistingTeamScore')) %>% ###14 DUPES PLEASE GO BACK AND CHECK LATER (CAUSED BY DOUBLE HEADERS W SAME SCORE) -- wait maybe not but still weird
  group_by(team1, season) %>%
  mutate(avgAtt = mean(Attendance),
         sdAtt = sd(Attendance)) %>%
  ungroup() %>%
  mutate(diffFromAvgAtt = Attendance - avgAtt,
         percDiffFromAvg = (Attendance - avgAtt)/avgAtt, 
         zScoreAtt = (Attendance - avgAtt)/sdAtt,
         weekend = ifelse(DayofWeek %in% c('Fri','Sat','Sun'), 1, 0))

ggplot(fullTeamPerformance, aes(teamPerfLogistic, diffFromAvgAtt)) + geom_point(aes(color = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, aes(fill = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, color = 'red')
ggplot(fullTeamPerformance, aes(teamPerfLogistic, zScoreAtt)) + geom_point(aes(color = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, aes(fill = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, color = 'red')

cor(fullTeamPerformance[fullTeamPerformance$weekend == 1,]$teamPerfLogistic, fullTeamPerformance[fullTeamPerformance$weekend == 1,]$diffFromAvgAtt, use="complete.obs")
cor(fullTeamPerformance[fullTeamPerformance$weekend == 1,]$teamPerfLogistic, fullTeamPerformance[fullTeamPerformance$weekend == 1,]$percDiffFromAvg, use="complete.obs")
cor(fullTeamPerformance[fullTeamPerformance$weekend == 0,]$teamPerfLogistic, fullTeamPerformance[fullTeamPerformance$weekend == 0,]$zScoreDiffFromAvg, use="complete.obs")
cor(fullTeamPerformance$teamPerfLogistic, fullTeamPerformance$zScoreDiffFromAvg, use="complete.obs")
##
measureLogistic <- function(midVal, steepVal){
  ft <- fullTeamPerformance %>%
    mutate(teamPerfLogistic = eloDiff1 * logisticFunction(middleVal = midVal, steepness = steepVal, x = gameNum2))
  
  all <- cor(ft$teamPerfLogistic, ft$zScoreAtt, use="complete.obs")
  weekend <- cor(ft[ft$weekend == 1,]$teamPerfLogistic, ft[ft$weekend == 1,]$zScoreAtt, use="complete.obs")
  weekday <- cor(ft[ft$weekend == 0,]$teamPerfLogistic, ft[ft$weekend == 0,]$zScoreAtt, use="complete.obs")
  
  finalMat <- matrix(c(midVal, steepVal, all, weekend, weekday), nrow = 1) %>% data.frame()
  names(finalMat) <- c('midVal', 'steepVal', 'corAll', 'corWeekend', 'corWeekday')
  return(finalMat)
}
midValVec <- seq.int(0, 90, by = .5)
steepValVec <- seq.int(0, .6, by = .005)
allCombs <- expand.grid(midValVec, steepValVec)

corrMatrixLogistic <- future_map2_dfr(as.list(allCombs$Var1), as.list(allCombs$Var2), measureLogistic, .progress = T)
plot3d(corrMatrixLogistic$midVal, corrMatrixLogistic$steepVal, corrMatrixLogistic$corWeekend)
ggplot(fullTeamPerformance, aes(teamPerfLogistic, diffFromAvgAtt)) + geom_point(aes(color = as.factor(weekend))) + geom_smooth(se = F, aes(fill = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, color = 'red')
ggplot(fullTeamPerformance[fullTeamPerformance$weekend == 1,], aes(teamPerfLogistic, diffFromAvgAtt)) + geom_point(aes(color = as.factor(weekend))) + geom_smooth(se = F, aes(fill = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, color = 'red')
ggplot(fullTeamPerformance[fullTeamPerformance$weekend == 0,], aes(teamPerfLogistic, diffFromAvgAtt)) + geom_point(aes(color = as.factor(weekend))) + geom_smooth(se = F, aes(fill = as.factor(weekend))) + geom_smooth(method = 'lm', se = F, color = 'red')

ggplot(fullTeamPerformance) + geom_density(aes(zScoreAtt, group = as.factor(season), color = as.factor(season))) + facet_wrap(~team1) + ylim(0,2)