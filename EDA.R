require(here)
require(magrittr)
require(tidyverse)

Player_Details <- read_csv(here('Data', 'Player_Detail.csv'))
Junior_Stats <- read_csv(here('Data', 'Player_Junior_Stats.csv'), guess_max = 40000)
NHL_Stats <- read_csv(here('Data', 'Player_NHL_Stats.csv'), guess_max = 8000)

## Creating some useful tables

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


# Messing around with the data ----

QMJHL_comparison_18 <- Junior_Stats %>%
  filter(League == 'QMJHL' &
           Age >= 18 &
           Age <= 19) %>%
  left_join(NHL_under_27, by = 'ID') %>%
  replace(., is.na(.), 0)
