require(here)
require(magrittr)
require(tidyverse)
require(ggplot2)
require(gridExtra)

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

#Distribution of variables for distance calculation ----

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

#Goals:
plot(density(League$G, kernel = 'gaussian'))

#Assists:
plot(density(League$A, kernel = 'gaussian'))

#Age: Interesting
plot(density(League$Age, kernel = 'gaussian'))
#these peaks are associated with birthdates in the early parts of the year. Malcolm Gladwell goes into 
# greater and better detail then I could about this phenomenon in his book 'Outliers', but the idea is
# that since a child's age as it relates to junior hockey eligibility is determined on December 31, 
# players that are born right after (in January) are going to be older and bigger, and thus look better, 
# so they will have a better chance of getting choosen to play on travel teams and improve more.

# Scale Normalization techniques ----

normalization_calc <- function(df, new_point, method) {
  #No normalization is done
  if (method == 'None') {
    df <- mutate(df, distance = sqrt((x - new_point[1])^2 + (y - new_point[2])^2))
  } 
  #Normalized with the Standard Deviation
  else if (method == 'StDev') { 
    x_mean = mean(df$x)
    y_mean = mean(df$y)
    x_sd = sd(df$x)
    y_sd = sd(df$y)
    df <- mutate(df, x_adj = (x - x_mean)/x_sd,
                 y_adj = (y - y_mean)/y_sd,
                 distance = sqrt((x_adj - (new_point[1] - x_mean)/x_sd)^2 + 
                                 (y_adj - (new_point[2] - y_mean)/y_sd)^2)) %>%
      select(x, y, distance)
  } else if (method == 'MinMax') {
    x_min = min(df$x)
    x_max = max(df$x)
    y_min = min(df$y)
    y_max = max(df$y)
    df <- mutate(df, x_adj = (x - x_min)/(x_max - x_min),
                 y_adj = (y - y_min)/(y_max - y_min),
                 distance = sqrt((x_adj - (new_point[1] - x_min)/(x_max - x_min))^2 +
                                 (y_adj - (new_point[2] - y_min)/(y_max - y_min))^2)) %>%
      select(x, y, distance)
    
  } else if (method == 'ProbDenEmpir') {
    x_cdf = ecdf(df$x)
    y_cdf = ecdf(df$y)
    df <- mutate(df, x_dist = x_cdf(new_point[1]) - x_cdf(x),
                 y_dist = y_cdf(new_point[2]) - y_cdf(y),
                 distance = sqrt(x_dist^2 + y_dist^2)) %>%
      select(x, y, distance)
  } else if (method == 'ProbDenFit') {
    
  }
  #graphing resulting neighborhood
  df %>%
    rbind(data.frame(x = new_point[1], y = new_point[2], distance = 0)) %>%
    .[order(.$distance),] %>%
    mutate(rank = seq(0, nrow(.) - 1),
           type = case_when(rank == 0 ~ 'New Point',
                            rank <= k ~ 'Neighbor',
                            TRUE ~ 'Original'),
           type = as.factor(type),
           type = factor(type, levels = c('New Point', 'Neighbor', 'Original'))) %>%
    .[order(.$rank, decreasing = T),] %>%
    ggplot(aes(x = x, y = y, color = type)) + geom_point() + 
    scale_color_manual(values = c('Red', 'Blue', 'Gray')) + theme_classic()
}

## Simulation 1: Normal vs Gamma with similar scale

x <- rnorm(1000, 7.5, 1)
y <- rgamma(1000, 1, 1) + 5
df <- data.frame(x,y)
new_data_point <- c(rnorm(1, 7.5, 1), rgamma(1, 1, 1) + 5)
k <- 100


a <- df %>%
  normalization_calc(new_data_point, 'None') + 
  labs(title = 'No Normalization') + theme(plot.title = element_text(hjust=0.5))
b <- df %>%
  normalization_calc(new_data_point, method = 'StDev') +
  labs(title = 'Standard Deviation Normalization') + theme(plot.title = element_text(hjust=0.5))
c <- df %>%
  normalization_calc(new_data_point, method = 'MinMax') +
  labs(title = 'Min Max Normalization') + theme(plot.title = element_text(hjust=0.5))
d <- df %>%
  normalization_calc(new_data_point, method = 'ProbDenEmpir') +
  labs(title = 'Probability Density Normalization') + theme(plot.title = element_text(hjust=0.5))
grid.arrange(a,b,c,d, ncol = 2)

## Simulation 2: Normal vs Normal with different scale

x <- rnorm(1000, 100, 20)
y <- rnorm(1000, 5, 1)
df <- data.frame(x,y)
new_data_point <- c(rnorm(1, 100, 20), rnorm(1, 5, 1))
k <- 100

a <- df %>%
  normalization_calc(new_data_point, 'None') + 
  labs(title = 'No Normalization') + theme(plot.title = element_text(hjust=0.5))
b <- df %>%
  normalization_calc(new_data_point, method = 'StDev') +
  labs(title = 'Standard Deviation Normalization') + theme(plot.title = element_text(hjust=0.5))
c <- df %>%
  normalization_calc(new_data_point, method = 'MinMax') +
  labs(title = 'Min Max Normalization') + theme(plot.title = element_text(hjust=0.5))
d <- df %>%
  normalization_calc(new_data_point, method = 'ProbDenEmpir') +
  labs(title = 'Probability Density Normalization') + theme(plot.title = element_text(hjust=0.5))
grid.arrange(a,b,c,d, ncol = 2)

## Simulation 3: Normal vs Gamma with different scale

x <- rnorm(1000, 100, 20)
y <- rgamma(1000, 1, 1) + 5
df <- data.frame(x,y)
new_data_point <- c(rnorm(1, 100, 20), rgamma(1000, 1, 1) + 5)
k <- 100

a <- df %>%
  normalization_calc(new_data_point, 'None') + 
  labs(title = 'No Normalization') + theme(plot.title = element_text(hjust=0.5))
b <- df %>%
  normalization_calc(new_data_point, method = 'StDev') +
  labs(title = 'Standard Deviation Normalization') + theme(plot.title = element_text(hjust=0.5))
c <- df %>%
  normalization_calc(new_data_point, method = 'MinMax') +
  labs(title = 'Min Max Normalization') + theme(plot.title = element_text(hjust=0.5))
d <- df %>%
  normalization_calc(new_data_point, method = 'ProbDenEmpir') +
  labs(title = 'Probability Density Normalization') + theme(plot.title = element_text(hjust=0.5))
grid.arrange(a,b,c,d, ncol = 2)

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
