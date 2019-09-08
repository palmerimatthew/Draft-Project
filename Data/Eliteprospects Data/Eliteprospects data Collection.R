require(data.table)
require(here)

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
leagues <- c('AHL', 'NCAA', 'USHL', 'USDP', 'USHS-MN', 'WHL', 'AJHL', 'BCHL', 'OHL', 'QMJHL', 'SHL', 
             'Allsvenskan', 'Division 1', 'J20 SuperElit', 'Liiga', 'Mestis', 'Jr. SM-Liiga',
             'NLA', 'DEL', 'KHL', 'VHL', 'MHL', 'Extraliga', 'Slovenia')

maybe_leagues <- c('USHS-Prep', 'USHS-MI', 'USHS-NY', 'USHS-MA', 'MJHL', 'CCHL', 'OJHL', 'J20 Elit',
                   'J18 Allsvenskan', 'J18 Elit', 'NLB')

defunct_leagues <- c()

base_url <- 'https://www.eliteprospects.com/league/'
undrafted_links <- character(0)
for(League in leagues) {
  for(year in 1980:2018) {
    website <- paste0(base_url, League, '/stats/', year, '-', year+1)
    links <- league_list_scraper(website)
    undrafted_links <- c(undrafted_links, links)
  }
}
undrafted_links <- unique(undrafted_links)
undrafted_df <- data.frame(undrafted_urls = undrafted_links)
fwrite(undrafted_df, here("Data", "Eliteprospects Data", paste0("Undrafted_Links.csv")))

#Large number of undrafted players, so breaking this into segments of 500 players
segments <- undrafted_links %>%
  length() %>%
  divide_by(500) %>%
  floor()









