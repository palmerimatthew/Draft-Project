require(data.table)
require(here)

## Drafts ----
base_str <- 'https://www.eliteprospects.com/draft/nhl-entry-draft/'

for(year in 1980:2019) {
  website <- paste0(base_str, year)
  temp <- draft_Scraper(website)
  fwrite(temp, here("Data", "Eliteprospects Data", paste0("Eliteprospects_Draft_", year, '.csv')))
}

## undrafted players ----

#We want to pull undrafted data as well to not skew our results when we build out the model
#we will initially pull in all player data (so there will be duplicates) 
leagues <- c('AHL', 'NCAA', 'USHL', 'USDP', 'USHS-MN', 'WHL', 'AJHL', 'BCHL', 'OHL', 'QMJHL', 'SHL', 
             'Allsvenskan', 'Division 1', 'J20 SuperElit', 'Liiga', 'Mestis', 'Jr. SM-Liiga',
             'NLA', 'DEL', 'KHL', 'VHL', 'MHL', 'Extraliga', 'Slovenia')

maybe_leagues <- c('USHS-Prep', 'USHS-MI', 'USHS-NY', 'USHS-MA', 'MJHL', 'CCHL', 'OJHL', 'J20 Elit',
                   'J18 Allsvenskan', 'J18 Elit', 'NLB')

defunct_leagues <- c()

for(League in leagues) {
  for(year in 1980:2019) {
    
  }
}
