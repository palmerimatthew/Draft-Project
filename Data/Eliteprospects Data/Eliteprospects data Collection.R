require(data.table)
require(here)
require(tidyverse)
require(magrittr)
require(devtools)
install_github('https://github.com/palmerimatthew/EPScraper')
require(EPScraper)
require(wrapr)

## Drafts ----
base_url <- 'https://www.eliteprospects.com/draft/nhl-entry-draft/'

for(year in 1980:2019) {
  website <- paste0(base_url, year)
  temp <- draft_Scraper(website)
  fwrite(temp, here("Data", "Eliteprospects Data", paste0("Eliteprospects Draft ", year, '.csv')))
}

## undrafted players ----

#We want to pull undrafted data as well to not skew our results when we build out the model
#we will pull all of the players that played in each of these leagues, and then take 
# just the unique ones

Draft_League_List <- read.csv(here('Data', 'Eliteprospects Data', 'Eliteprospects Clean Drafted.csv')) %>%
  filter(Age >= 17 & Age <= 20) %>%
  mutate(Start_Year = as.numeric(gsub('-(.*)', '', Season))) %>%
  group_by(League) %>%
  summarise(count = length(unique(ID)),
            min_year = min(Start_Year),
            max_year = max(Start_Year)) %>%
  arrange(desc(count))

Leagues <- Draft_League_List %>%
  filter((max_year >= 2011 &
           count > 50)) %$%
  League %>%
  as.character() %>%
  c(., 'MJHL', 'Slovakia2', 'Division 2', 'VHL', 'USHS-WI', 'USHS-MI', 'DEL', 'WJC-20 D1A', 'WJC-18 D1A') %>%
  .[!. %in% c('NHL', 'Russian U17')]


defunct_leagues <- Draft_League_List %>%
  filter(max_year <= 2010 &
           count >= 105) %$%
  League %>%
  as.character()
#defunct league mappings:
## EJC18 -> WJC-18
## Russia -> KHL
## Russia2 -> VHL
## Russia3 -> No current equivalent
## OPJHL -> OJHL
## BCJHL -> No current equivalent
## Soviet -> KHL
## CJHL -> CCHL

league_list <- c(Leagues, defunct_leagues) %>%
  tolower()

base_url <- 'https://www.eliteprospects.com/league/'
for(league in league_list) {
  print(league)
  league <- gsub(' ', '-', league)
  undrafted_links <- character(0)
  for(year in 1990:2018) {
    links <- character(0)
    print(paste('  ', year))
    website <- paste0(base_url, league, '/stats/', year, '-', year+1)
    links <- EP_League_Links(website, T, 'u21')
    undrafted_links <- unique(c(undrafted_links, links))
  }
  let(alias = list(rname = gsub('-', '_', league)), expr = (rname = undrafted_links))
}



#Large number of undrafted players, so breaking this into segments of 2500 players
undrafted_links <- read.csv('~/Desktop/Github Repos/Draft-Project/Data/Eliteprospects Data/Eliteprospects Undrafted Links.csv') %$%
  links %>%
  as.character()

segments <- undrafted_links %>%
  length() %>%
  divide_by(2500) %>%
  floor() %>%
  add(1)

for(i in 1:segments) {
  links <- undrafted_links[(2500*(i-1)+1):(2500*i)]
  segment_df <- EP_Ind_Scraper(links[1])
  links <- links[-1]
  j <- 2
  for(website in links) {
    print(j)
    temp <- EP_Ind_Scraper(website)
    segment_df <- rbind(segment_df, temp)
    j <- j+1
  }
  fwrite(segment_df, here('Data', 'Eliteprospects Data', paste0('Eliteprospects Undrafted Segment ', i, '.csv')))
}






