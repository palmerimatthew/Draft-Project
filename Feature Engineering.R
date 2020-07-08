require(tidyverse)
require(here)

Player_Details <- 
  read_csv(here('Data', 'Player_Detail.csv')) %>%
  rename(PlayerID = ID)
Junior_Stats <- 
  read_csv(here('Data', 'Player_Junior_Stats.csv'), guess_max = 40000) %>%
  mutate(Points_Game = TP/GP,
         SeasonID = seq(1, nrow(.)),
         Season_Start = as.numeric(gsub('-(.*)', '', Season))) %>%
  rename(PlayerID = ID) %>%
  select(SeasonID, PlayerID, Name, Season, Season_Start, Age:Points_Game) %>%
  filter(Season != '2019-20')
NHL_Stats <- 
  read_csv(here('Data', 'Player_NHL_Stats.csv'), guess_max = 8000) %>%
  rename(PlayerID = ID)




# Rankings
Rankings <- 
  Junior_Stats %>%
  mutate(Plus_10GP = GP >= 10) %>%
  group_by(Season, Team, League) %>%
  mutate(Within_Team_Rank = order(order(Points_Game, decreasing = T))) %>%
  ungroup() %>%
  group_by(Season, Team, League, Plus_10GP) %>%
  mutate(Plus_10GP_Within_Team_Rank = if_else(Plus_10GP,
                                              order(order(Points_Game, decreasing = T)),
                                              NA_integer_)) %>%
  ungroup() %>%
  group_by(Season, League) %>%
  mutate(Within_League_Rank = order(order(Points_Game, decreasing = T))) %>%
  ungroup() %>%
  group_by(Season, League, Plus_10GP) %>%
  mutate(Plus_10GP_Within_League_Rank = if_else(Plus_10GP,
                                                order(order(Points_Game, decreasing = T)),
                                                NA_integer_)) %>%
  left_join(Player_Details, by = 'PlayerID') %>%
  mutate(Defenseman = if_else(grepl('D', Position_Clean), "D", "F")) %>%
  ungroup() %>%
  group_by(Season, Team, League, Defenseman) %>%
  mutate(Within_Team_Position_Rank = order(order(Points_Game, decreasing = T))) %>%
  ungroup() %>%
  group_by(Season, Team, League, Defenseman, Plus_10GP) %>%
  mutate(Plus_10GP_Within_Team_Position_Rank = if_else(Plus_10GP,
                                                       order(order(Points_Game, decreasing = T)),
                                                       NA_integer_)) %>%
  ungroup() %>%
  group_by(Season, League, Defenseman) %>%
  mutate(Within_League_Position_Rank = order(order(Points_Game, decreasing = T))) %>%
  ungroup() %>%
  group_by(Season, League, Defenseman, Plus_10GP) %>%
  mutate(Plus_10GP_Within_League_Position_Rank = if_else(Plus_10GP,
                                                         order(order(Points_Game, decreasing = T)),
                                                         NA_integer_)) %>%
  ungroup() %>%
  select(SeasonID, Within_Team_Rank, Within_League_Rank, Within_Team_Position_Rank, Within_League_Position_Rank,
         Plus_10GP_Within_Team_Rank, Plus_10GP_Within_League_Rank, Plus_10GP_Within_Team_Position_Rank,
         Plus_10GP_Within_League_Position_Rank)
  