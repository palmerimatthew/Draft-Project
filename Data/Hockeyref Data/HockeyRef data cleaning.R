require(magrittr)
require(here)
require(devtools)
install_github('https://github.com/palmerimatthew/HockeyRefScraper')
require(HockeyRefScraper)
install_github('https://github.com/palmerimatthew/EPScraper')
require(EPScraper)
require(dplyr)
require(tidyr)
require(data.table)


# Drafted HockeyRef combination ----

combined <- data.frame()

for(year in 1980:2018) {
  temp <- read.csv(here('Data', 'Hockeyref Data', paste0('HockeyRef Draft ', year, '.csv')))
  combined <- smart_rbind(combined, temp)
}

fwrite(combined, here('Data', 'Hockeyref Data', 'Hockeyref drafted raw.csv'))


# Add EP ID to Hockeyref for joining

drafted_raw <- read.csv(here('Data', 'Hockeyref Data', 'Hockeyref drafted raw.csv'))

undrafted_raw <- read.csv(here('Data', 'Hockeyref Data', 'Hockeyref undrafted raw.csv'))

EP_players <- read.csv(here('Data', 'Player_Detail.csv'))



Matching <- data.frame()
Not_Matching <- data.frame()
problem_years <- numeric()
for(year in 1980:2018) {
  EP <- EP_players %>%
    filter(Draft_Year == year)
  HR <- read.csv(here('Data', 'Hockeyref Data', paste0('HockeyRef Draft ', year, '.csv')))
  
  if(nrow(EP) != length(unique(EP$Name))) {
    problem_years <- append(problem_years, year)
  } else {
    temp <- EP %>%
      select(ID, Name) %>%
      right_join(HR, by='Name')
    
    matched <- temp %>%
      filter(!is.na(ID))
    
    not_matched <- temp %>%
      filter(is.na(ID))
    
    Matching <- smart_rbind(Matching, matched)
    Not_Matching <- smart_rbind(Not_Matching, not_matched)
    
    # Making sure there is no duplication in the joins
    if(nrow(not_matched) + nrow(matched) > nrow(HR)) {
      break
    }
  }
}

## Fixing problem years

for(year in problem_years) {
  EP <- EP_players %>%
    filter(Draft_Year == year)
  HR <- read.csv(here('Data', 'Hockeyref Data', paste0('HockeyRef Draft ', year, '.csv')))
  
  problem_name <- EP %>%
    .[duplicated(.$Name),] %$%
    Name %>%
    as.character()

  for(name in problem_name) {
    EP <- filter(EP, Name != name)
  }
  
  temp <- EP %>%
    select(ID, Name) %>%
    right_join(HR, by='Name')
  
  matched <- temp %>%
    filter(!is.na(ID))
  
  not_matched <- temp %>%
    filter(is.na(ID))
  
  Matching <- smart_rbind(Matching, matched)
  Not_Matching <- smart_rbind(Not_Matching, not_matched)
}

## Fixing non-matching players

problem_players <- smart_rbind(undrafted_raw, Not_Matching)

for(i in 1275:nrow(problem_players)) {
  if(i %% 25 == 0) {
    print(i)
  }
  target_name <- problem_players %>%
    .[i,] %$%
    Name %>%
    as.character()
  
  desired_birth_years <- problem_players %>%
    .[i,] %$%
    Season %>%
    as.character() %>%
    gsub('-(.*)', '', .) %>%
    as.numeric() %>%
    subtract(problem_players[i,]$Age) %>%
    add(0.5)
  
  links <- EP_Name_Search(target_name, include_db = T)
  
  if(length(links) > 0) {
    potential_links <- links %>%
      separate(Birth_Date, c('Month', 'Day', 'Year'), sep = '/') %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(abs(Year - desired_birth_years) < 1)
  } else {
    potential_links <- data.frame()
  }
  
  if(nrow(potential_links) == 1) {
    problem_players[i,1] <- potential_links %>%
      .[1,1] %>%
      as.character() %>%
      gsub('(.*)/player/', '', .) %>%
      gsub('/(.*)', '', .) %>%
      as.numeric()
  }
}


## Leftovers likely need to hard-code
temp <- problem_players %>%
  filter(!is.na(ID))

Matching <- rbind(Matching, temp)

problem_players <- problem_players %>%
  filter(is.na(ID))

#Hard-coding
temp <- problem_players %>%
  .[-293,] %>%
  mutate(ID = case_when(Name == 'Matt Read' ~ 33243,
                        Name == 'Brian Gibbons' ~ 24396,
                        Name == 'Ryan Wilson' ~ 13843,
                        Name == 'Mike Johnson' ~ 5687,
                        Name == 'Francis Breault' ~ 73521,
                        Name == 'Rob Collins' ~ 13510,
                        Name == 'Sean Collins' ~ 11422,
                        Name == 'Jacob MacDonald' ~ 60064,
                        Name == 'Andrew Miller' ~ 17845,
                        Name == 'Stephane Richer' ~ 41492,
                        Name == 'Trevor Smith' ~ 11437,
                        Name == 'Mel Angelstad' ~ 69066,
                        Name == 'Alexandre Boikov' ~ 18449,
                        Name == 'Matt Fraser' ~ 37148,
                        Name == 'Erik Gustafsson' ~ 5545,
                        Name == 'Philip Holm' ~ 11070,
                        Name == 'Magnus Johansson' ~ 292,
                        Name == 'Dan Renouf' ~ 76892,
                        Name == 'Derek Smith' ~ 14185,
                        Name == 'Jason Williams' ~ 8713,
                        Name == 'Paul Gagne' ~ 27883,
                        Name == 'Dave Gans' ~ 38468,
                        Name == 'Viacheslav Fetisov' ~ 21368,
                        Name == 'Sylvain Cote' ~ 31444,
                        Name == 'Stephane Richer' ~ 22800,
                        Name == 'Michael Ware' ~ 72977,
                        Name == 'Bobby Babcock' ~ 78133,
                        Name == 'Evgeny Davydov' ~ 1628,
                        Name == 'Viacheslav Butsayev' ~ 1673,
                        Name == 'Viktor Gordiouk' ~ 20426,
                        Name == 'Sandis Ozolinsh' ~ 8690,
                        Name == 'Dmitry Yushkevich' ~ 18636,
                        Name == 'Alexander Andrievski' ~ 41140,
                        Name == 'Grigori Panteleev' ~ 1953,
                        Name == 'Victor Ignatjev' ~ 1838,
                        Name == 'Niklas Sundstrom' ~ 740,
                        Name == 'Stefan Bergkvist' ~ 706,
                        Name == 'Harijs Vitolinsh' ~ 2142,
                        Name == 'Ilja Byakin' ~ 1908,
                        Name == 'Pavel Torgaev' ~ 28713,
                        Name == 'Aki Berg' ~ 2667,
                        Name == 'Aleksey Morozov' ~ 11044,
                        Name == 'Sergey Gusev' ~ 21002,
                        Name == 'Yevgeny Shaldybin' ~ 18504,
                        Name == 'Alexandre Volchkov' ~ 19081,
                        Name == 'Eric Belanger' ~ 8758,
                        Name == 'Patrick Leahy' ~ 8991,
                        Name == 'Pavel Vorobiev' ~ 9561,
                        Name == 'Alex Frolov' ~ 8591,
                        Name == 'Dave Morisset' ~ 29020,
                        Name == 'Tomi Maki' ~ 3215,
                        Name == 'Maxim Kondratiev' ~ 9384,
                        Name == 'Dave Moss' ~ 8959,
                        Name == 'Oliver Ekman-Larsson' ~ 7693,
                        Name == 'Adam Almqvist' ~ 10378,
                        Name == 'J.C. Lipon' ~ 45487,
                        Name == 'Edward Lee' ~ 125386,
                        Name == 'David Sacco' ~ 51692,
                        Name == 'Alex Steen' ~ 460,
                        Name == 'Jarkko Immonen' ~ 3181,
                        TRUE ~ ID))
temp[271:275, 1] = 115880
temp[277:292, 1] = 21557



Matching <- rbind(Matching, temp)


## Cleaning Matching table ----
NHL_Player_Stats <- Matching %>%
  filter(Lg == 'NHL') %>%
  mutate(Season = as.character(Season),
         Tm = as.character(Tm),
         Lg = as.character(Lg),
         Awards = as.character(Awards),
         Awards = if_else(Awards == '', NA_character_, Awards)) %>%
  rename(Team = Tm,
         League = Lg,
         NHL_GP = GP,
         NHL_G = G,
         NHL_A = A,
         NHL_TP = PTS,
         NHL_PIM = PIM,
         NHL_S = S,
         NHL_TSA = TSA,
         NHL_TOI = TOI,
         NHL_FOW = FOW,
         NHL_FOL = FOL,
         NHL_BLK = BLK,
         NHL_TK = TK,
         NHL_GV = GV,
         NHL_CF = CF,
         NHL_CA = CA,
         NHL_FF = FF,
         NHL_FA = FA,
         NHL_oiGF = oiGF,
         NHL_oiGA = oiGA,
         NHL_PDO = PDO,
         NHL_TGF = TGF,
         NHL_PGF = PGF,
         NHL_TGA = TGA,
         NHL_PGA = PGA,
         NHL_PM = X...,
         NHL_OPS = OPS,
         NHL_DPS = DPS,
         NHL_PS = PS,
         NHL_xGF = xGF,
         NHL_xGA = xGA) %>%
  select(-ATOI)

fwrite(NHL_Player_Stats, here('Data', 'Player_NHL_Stats.csv'))
