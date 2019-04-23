## Get all data sets
library(dplyr)
library(magrittr)
library(stats4)
library(ggplot2)
list.files("~/Documents/Kaggle/NHL/")
setwd("~/Documents/Kaggle/NHL/")
game_goalie_stats <- read.csv("game_goalie_stats.csv")
game_plays_players <- read.csv("game_plays_players.csv")
game_plays <- read.csv("game_plays.csv")
game_shifts <- read.csv("game_shifts.csv")
game_skater_stats <- read.csv("game_skater_stats.csv")
game_teams_stats <- read.csv("game_teams_stats.csv")
game <- read.csv("game.csv")
player_info <- read.csv("player_info.csv")
team_info <- read.csv("team_info.csv")

goal_counts <- game_teams_stats %>%
  inner_join((
    game %>% 
      filter(type=='R' & season==20172018) %>%
      select(game_id, date_time) %>%
      distinct()
  ), by = c("game_id")) %>%
  group_by(team_id) %>%
  arrange(team_id, date_time) %>%
  inner_join(team_info, by = "team_id") %>%
  select(game_id, date_time, team_id, teamName, goals, shots) %>%
  mutate(season_goals = cumsum(goals),
         goals_0 = cumsum(ifelse(goals==0,1,0)),
         goals_1 = cumsum(ifelse(goals==1,1,0)),
         goals_2 = cumsum(ifelse(goals==2,1,0)),
         goals_3 = cumsum(ifelse(goals==3,1,0)),
         goals_4 = cumsum(ifelse(goals==4,1,0)),
         goals_5 = cumsum(ifelse(goals==5,1,0)),
         goals_6 = cumsum(ifelse(goals==6,1,0)),
         goals_7 = cumsum(ifelse(goals==7,1,0)),
         goals_8 = cumsum(ifelse(goals==8,1,0)),
         goals_9 = cumsum(ifelse(goals==9,1,0)),
         goals_10 = cumsum(ifelse(goals>=10,1,0))) %>%
  ungroup()



## Log likelihood function for NBD with spike at 2
LL_NBD <- function(r, alpha, X, fx, spike_val, spike_prob) {
  probs <- dnbinom(x=X, size = r, mu = r/alpha)
  s_zero <- ifelse(X == spike_val, spike_prob, 0)
  s_probs <- s_zero + (1-spike_prob)*probs
  -sum(fx*log(s_probs))
  #-sum(fx*log(probs))
}

## Gets the maximum likelihood estimator for a given
getMLE <- function(row) {
  s_val <- which.max(row[8:18]) - 1
  est <- mle(minuslogl = LL_NBD,
            start = list(r = .5, alpha = .5, spike_prob = .5),
            fixed = list(X = 0:10, fx = as.numeric(row[8:18]), spike_val = s_val),
            method = "L-BFGS-B",
            lower = c(.00001, .00001, .00001),
            upper = c(Inf, Inf, .99999))
  return(c(est@coef, s_val))
}

NBDbyTeam <- function(team) {
  alpha <- rep(0, 82)
  r <- rep(0, 82)
  spike <- rep(0, 82)
  spike_val <- rep(0, 82)
  ll <- rep(0, 82)
  pred_goals <- rep(0, 82)
  df <- goal_counts %>% filter(teamName==team)
  final_goals <- df[, 8:18]
  for (game in 1:nrow(df)) {
    #print(game)
    try({
      ml <- getMLE(df[game,])
      r[game] <- ml[1]
      alpha[game] <- ml[2]
      spike[game] <- ml[3]
      spike_val[game] <- ml[4]
      t_dist <- data.frame(goals=rnbinom(10000, size = ml[1], mu = ml[1]/ml[2])) %>%
        count(goals) %>% 
        mutate(n = n/10000) %>%
        mutate(n = ifelse(goals == ml[4], ml[3], 0) + (1- ml[3])*n)
      pred_goals[game] <- (t_dist %>% mutate(exp_goals = goals*n*82) %>% summarise(exp_goals = sum(exp_goals)))$exp_goals
      ll[game] <- LL_NBD(r = ml[1], alpha = ml[2], spike_prob = ml[3], spike_val = ml[4], X = 0:10, fx = df[nrow(df), 8:18])
      #print(ll[game])
    }
    , TRUE)
  }
  data.frame(alpha=alpha, r=r, spike = spike, spike_val = spike_val, ll = ll, pred_goals = pred_goals) %>% cbind(final_goals)
}

getDist <- function(nbd) {
  dist <- data.frame(goals=rnbinom(10000, size = nbd$r[82], mu = nbd$r[82]/nbd$alpha[82])) %>%
    count(goals) %>% 
    mutate(n = n/10000) %>%
    mutate(n = ifelse(goals == nbd[82, 4], nbd$spike[82], 0) + (1- nbd$spike[82])*n)
  return(dist)
}
###

Blues_nbd <- NBDbyTeam("Blues")
Blues_dist <- data.frame(goals=rnbinom(10000, size = Blues_nbd$r[82], mu = Blues_nbd$r[82]/Blues_nbd$alpha[82])) %>%
  count(goals) %>% 
  mutate(n = n/10000) %>%
  mutate(n = ifelse(goals == Blues_nbd$spike_val[82], Blues_nbd$spike[82], 0) + (1- Blues_nbd$spike[82])*n)

ggplot() + 
  geom_bar(aes(x = Blues_dist$goals, weight = Blues_dist$n), color = "red", fill = "red", alpha = .2) +
  geom_bar(aes(x = 0:10, weight = as.numeric(Blues_nbd[82, 7:17])/82), alpha = .6, color = "black")


## Capitals
Caps_nbd <- NBDbyTeam("Capitals")
Caps_dist <- data.frame(goals=rnbinom(10000, size = Caps_nbd$r[82], mu = Caps_nbd$r[82]/Caps_nbd$alpha[82])) %>%
  count(goals) %>% 
  mutate(n = n/10000) %>%
  mutate(n = ifelse(goals == Caps_nbd[82, 4], Caps_nbd$spike[82], 0) + (1- Caps_nbd$spike[82])*n)

ggplot() + 
  #geom_bar(aes(x = Caps_dist$goals, weight = Caps_dist$n), color = "red", fill = "red", alpha = .2) +
  geom_bar(aes(x = 0:10, weight = as.numeric(Caps_nbd[82, 7:17])/82), alpha = .6, color = "black")

## Golden Knights
Knights_nbd <- NBDbyTeam("Golden Knights")

Knights_dist <- data.frame(goals=rnbinom(10000, size = Knights_nbd$r[82], mu = Knights_nbd$r[82]/Knights_nbd$alpha[82])) %>%
  count(goals) %>% 
  mutate(n = n/10000) %>%
  mutate(n = ifelse(goals == Knights_nbd[82, 4], Knights_nbd$spike[82], 0) + (1- Knights_nbd$spike[82])*n)

ggplot() + 
  geom_bar(aes(x = Knights_dist$goals, weight = Knights_dist$n), color = "red", fill = "red", alpha = .2) +
  geom_bar(aes(x = 0:10, weight = as.numeric(Knights_nbd[82, 7:17])/82), alpha = .6, color = "black")

## Coyotes
Coyotes_nbd <- NBDbyTeam("Coyotes")
Coyotes_dist <- getDist(Coyotes_nbd)
ggplot() + 
  geom_bar(aes(x = Coyotes_dist$goals, weight = Coyotes_dist$n), color = "red", fill = "red", alpha = .2) +
  geom_bar(aes(x = 0:10, weight = as.numeric(Coyotes_nbd[82, 7:17])/82), alpha = .6, color = "black")

## All teams
teams <- goal_counts %>% distinct(teamName)

results_by_team <- data.frame(teamName = teams, 
                              mode_converge_game = rep(0, length(teams)), 
                              mode_coverge_value = rep(0, length(teams)),
                              ll_converge_game = rep(0, length(teams)),
                              ll_converge_value = rep(0, length(teams)))
for (t in as.character(teams$teamName)) {
  print(t)
  nbd <- NBDbyTeam(t)
  #dist <- getDist(nbd) 
  mode_converge <- nbd %>% 
    mutate(mode_change = ifelse(lag(spike_val) == spike_val, 0, 1),
           game_number = row_number()) %>%
    filter(mode_change == 1) %>%
    tail(1) %>%
    select(game_number, spike_val)
  ll_range <- nbd$ll[nbd$ll > 0][1] - nbd$ll[82]
  ll_converge <- nbd %>%
    mutate(game_number = row_number()) %>%
    filter(ll > 0) %>%
    filter(ll <= nbd$ll[82] + .01*ll_range) %>%
    head(1) %>%
    select(game_number, ll)
  results_by_team[results_by_team$teamName == t, 2:5] <- cbind(mode_converge, ll_converge)
}

library(xlsx)
setwd("~/Documents/MBA/Probability Models for Customer Analytics/")
write.xlsx(results_by_team, file = "results_by_team.xlsx")
