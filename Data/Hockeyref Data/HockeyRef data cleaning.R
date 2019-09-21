require(magrittr)
require(here)
require(devtools)
install_github('https://github.com/palmerimatthew/HockeyRefScraper')
require(HockeyRefScraper)
install_github('https://github.com/palmerimatthew/EPScraper')
require(EPScraper)
require(dplyr)
require(tidyr)


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
  
  potential_links <- links %>%
    separate(Birth_Date, c('Month', 'Day', 'Year'), sep = '/') %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(abs(Year - desired_birth_years) < 1)
  
  if(nrow(potential_links) == 1) {
    problem_players[i,1] <- potential_links %>%
      .[1,1] %>%
      as.character() %>%
      gsub('(.*)/player/', '', .) %>%
      gsub('/(.*)', '', .) %>%
      as.numeric()
  }
}
