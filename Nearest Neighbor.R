require(here)
require(magrittr)
require(tidyverse)

Player_Details <- read_csv(here('Data', 'Player_Detail.csv'))
Junior_Stats <- read_csv(here('Data', 'Player_Junior_Stats.csv'), guess_max = 40000) %>%
  mutate(Points_Game = TP/GP)
NHL_Stats <- read_csv(here('Data', 'Player_NHL_Stats.csv'), guess_max = 8000)

NHL_under_27 <- NHL_Stats %>%
  filter(Age < 27) %>%
  group_by(ID, Name) %>%
  summarise(NHL_GP = sum(NHL_GP),
            NHL_G = sum(NHL_G),
            NHL_A = sum(NHL_A),
            NHL_TP = sum(NHL_TP),
            NHL_PIM = sum(NHL_PIM),
            NHL_S = sum(NHL_S),
            NHL_TSA = sum(na.omit(NHL_TSA)),
            NHL_OPS = sum(NHL_OPS),
            NHL_DPS = sum(NHL_DPS)) %>%
  mutate(NHL_PS = NHL_OPS + NHL_DPS)

# Using Eric Staal draft year as test sample ----

#Just Eric Staal's draft year
Eric_Staal <- Junior_Stats %>%
  filter(ID == 3656 &
           Age > 18 &
           Age < 19) %>%
  left_join(select(Player_Details, -Name), by = 'ID') %>%
  inner_join(select(NHL_under_27, -Name), by = 'ID')

#filtering to just desired league
League <- Junior_Stats %>%
  filter(League == Eric_Staal$League)

League_Player_Details <- Player_Details %>%
  filter(ID %in% League$ID)

#Distribution of variables for distance calculation

#Weight: Normal
plot(density(filter(League_Player_Details, !is.na(Weight))$Weight, kernel = 'gaussian'))
x <- seq(130, 280, 0.1)
lines(x, dnorm(x, 193, 15), col='red')

#Height: Normal
plot(density(filter(League_Player_Details, !is.na(Weight))$Height, kernel = 'gaussian'))
x <- seq(160, 210, 0.1)
lines(x, dnorm(x, 184.5, 4.5), col='red')

#Point Production: Gamma
plot(density(League$Points_Game, kernel = 'gaussian'))
x <- seq(0, 3.5, 0.01)
lines(x-.08, dgamma(x, 1.75, 3.2), col='red')

#Age: Interesting
plot(density(League$Age, kernel = 'gaussian'))
#these peaks are associated with birthdates in the early parts of the year. Malcolm Gladwell goes into 
# greater and better detail then I could about this phenomenon in his book 'Outliers', but the idea is
# that since a child's age as it relates to junior hockey eligibility is determined on December 31, 
# players that are born right after (in January) are going to be older and bigger, and thus look better, 
# so they will have a better chance of getting choosen to play on travel teams and improve more.



# Interesting Findings ----

## Birthdate trends in the CHL and NHL
CHL <- Junior_Stats %>%
  filter(League == 'QMJHL' |
           League == 'WHL' |
           League == 'OHL')

League_Player_Details <- Player_Details %>%
  filter(ID %in% CHL$ID)

NHL_Total <- NHL_under_27 %>%
  inner_join(Player_Details, by = 'ID')

NHL_no_USA_CAN <- NHL_under_27 %>%
  inner_join(Player_Details, by = 'ID') %>%
  .[!grepl('CAN', .$Birth_Country),] %>%
  .[!grepl('USA', .$Birth_Country),]

NHL_NA <- NHL_under_27 %>%
  inner_join(Player_Details, by = 'ID') %>%
  filter(!(ID %in% NHL_no_USA_CAN$ID))

a <- ggplot(League_Player_Details, aes(x = Birth_Month)) + geom_bar() + 
  labs(title = 'CHL') + theme_classic() + theme(plot.title = element_text(hjust=0.5))
b <- ggplot(NHL_Total, aes(x = Birth_Month)) + geom_bar() + 
  labs(title = 'NHL') + theme_classic() + theme(plot.title = element_text(hjust=0.5))
c <- ggplot(NHL_no_USA_CAN, aes(x = Birth_Month)) + geom_bar() + 
  labs(title = 'NHL no NA') + theme_classic() + theme(plot.title = element_text(hjust=0.5))
d <- ggplot(NHL_NA, aes(x = Birth_Month)) + geom_bar() + 
  labs(title = 'NHL just NA') + theme_classic() + theme(plot.title = element_text(hjust=0.5))

gridExtra::grid.arrange(d, c, ncol = 2)
