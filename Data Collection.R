require(data.table)

#Eliteprospects scrapping ----


## Drafts
base_str <- 'https://www.eliteprospects.com/draft/nhl-entry-draft/'

for(year in 1980:2019) {
  website <- paste0(base_str, year)
  temp <- Ind_Scraper(website)
  fwrite(temp, '~/Data/Eliteprospects Data')
}