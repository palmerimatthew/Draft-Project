require(MASS)
require(tidyverse)
require(here)



Player_Details <- read_csv(here('Data', 'Player_Detail.csv')) %>%
  rename(PlayerID = ID)
Junior_Stats <- read_csv(here('Data', 'Player_Junior_Stats.csv'), guess_max = 40000) %>%
  mutate(Points_Game = TP/GP,
         SeasonID = seq(1, nrow(.)),
         Season_Start = as.numeric(gsub('-(.*)', '', Season))) %>%
  rename(PlayerID = ID) %>%
  select(SeasonID, PlayerID, Name, Season, Season_Start, Age:Points_Game)
NHL_Stats <- read_csv(here('Data', 'Player_NHL_Stats.csv'), guess_max = 8000) %>%
  rename(PlayerID = ID)
Rolled_up_Junior_Stats <- Junior_Stats %>%
  group_by(PlayerID, Name, Season, Season_Start, Age, League) %>%
  summarise(GP = sum(GP),
            G = sum(G),
            A = sum(A),
            TP = sum(TP),
            PIM = sum(PIM)) %>%
  mutate(Points_Game = TP/GP) %>%
  ungroup()


Comparison_Boostrap <- function(comparison, sample_length, reps) {
  data <- data.frame(sample_number = seq(1, reps), mean = numeric(reps))
  for (i in (1:reps)) {
    temp <- sample(comparison$diff, sample_length, replace = T)
    data[i,2] <- median(temp)
  }
  data
}


# Offsetting Years
Normal_Year <- Rolled_up_Junior_Stats %>%
  filter(GP > 25) %>%
  mutate(middle_age = Age+.5,
         for_join = paste(PlayerID, round(middle_age, 2))) %>%
  select(for_join, PlayerID, middle_age, League, Points_Game) %>%
  rename(First_League = League,
         First_Points_Game = Points_Game)

Next_Year <- Rolled_up_Junior_Stats %>%
  filter(GP > 25) %>%
  mutate(middle_age = Age-0.5,
         for_join = paste(PlayerID, round(middle_age, 2))) %>%
  select(for_join, League, Points_Game) %>%
  rename(Second_League = League,
         Second_Points_Game = Points_Game)


Yearly_Comparison <- Normal_Year %>%
  full_join(Next_Year, by = 'for_join')


#testing these year to year comparisons with WHL to AHL ----
WHL_to_AHL <- Yearly_Comparison %>%
  filter(First_League == 'WHL' & Second_League == 'AHL')
WHL_First <- Yearly_Comparison %>%
  filter(First_League == 'WHL')
AHL_Second <- Yearly_Comparison %>%
  filter(Second_League == 'AHL')

plot(density(WHL_First$First_Points_Game, kernel = 'gaussian'))
x <- seq(min(WHL_First$First_Points_Game, na.rm=T), max(WHL_First$First_Points_Game, na.rm=T), 0.01)
WHLtemp <- fitdistr(filter(WHL_First, First_Points_Game != 0)$First_Points_Game, 'gamma')
lines(x, dgamma(x, shape = WHLtemp$estimate[1], rate = WHLtemp$estimate[2]), col='red')

plot(density(AHL_Second$Second_Points_Game, kernel = 'gaussian'))
x <- seq(min(AHL_Second$Second_Points_Game, na.rm=T), max(AHL_Second$Second_Points_Game, na.rm=T), 0.01)
AHLtemp <- fitdistr(filter(AHL_Second, Second_Points_Game != 0)$Second_Points_Game, 'gamma')
lines(x, dgamma(x, shape = AHLtemp$estimate[1], rate = AHLtemp$estimate[2]), col='red')


#ecdf of each league
WHL_to_AHL <- WHL_to_AHL %>%
  mutate(WHL_Prob = pgamma(First_Points_Game, shape = WHLtemp$estimate[1], rate = WHLtemp$estimate[2]),
         AHL_Prob = pgamma(Second_Points_Game, shape = AHLtemp$estimate[1], rate = AHLtemp$estimate[2]),
         diff = WHL_Prob - AHL_Prob)

plot(density(WHL_to_AHL$diff, kernel = 'gaussian'))
#boostrap of mean diff
WHL_to_AHL %>%
  Comparison_Boostrap(25, 1000) %>%
  ggplot(aes(x = mean)) + geom_histogram() + theme_bw()


#testing with WHL to NHL ----
WHL_to_NHL <- Yearly_Comparison %>%
  filter(First_League == 'WHL' & Second_League == 'NHL')
WHL_First <- Yearly_Comparison %>%
  filter(First_League == 'WHL')
NHL_Second <- Yearly_Comparison %>%
  filter(Second_League == 'NHL')

plot(density(WHL_First$First_Points_Game, kernel = 'gaussian'))
x <- seq(min(WHL_First$First_Points_Game, na.rm=T), max(WHL_First$First_Points_Game, na.rm=T), 0.01)
WHLtemp <- fitdistr(filter(WHL_First, First_Points_Game != 0)$First_Points_Game, 'gamma')
lines(x, dgamma(x, shape = WHLtemp$estimate[1], rate = WHLtemp$estimate[2]), col='red')

plot(density(NHL_Second$Second_Points_Game, kernel = 'gaussian'))
x <- seq(min(NHL_Second$Second_Points_Game, na.rm=T), max(NHL_Second$Second_Points_Game, na.rm=T), 0.01)
NHLtemp <- fitdistr(filter(NHL_Second, Second_Points_Game != 0)$Second_Points_Game, 'gamma')
lines(x, dgamma(x, shape = NHLtemp$estimate[1], rate = NHLtemp$estimate[2]), col='red')


#ecdf of each league
WHL_to_NHL <- WHL_to_NHL %>%
  mutate(WHL_Prob = pgamma(First_Points_Game, shape = WHLtemp$estimate[1], rate = WHLtemp$estimate[2]),
         NHL_Prob = pgamma(Second_Points_Game, shape = NHLtemp$estimate[1], rate = NHLtemp$estimate[2]),
         diff = WHL_Prob - NHL_Prob)

plot(density(WHL_to_NHL$diff, kernel = 'gaussian'))
#boostrap of mean diff
WHL_to_NHL %>%
  Comparison_Boostrap(25, 1000) %>%
  ggplot(aes(x = mean)) + geom_histogram() + theme_bw()







