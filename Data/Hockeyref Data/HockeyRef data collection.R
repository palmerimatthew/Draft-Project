require(here)
require(data.table)

## Drafts ----
base_url <- 'https://www.hockey-reference.com/draft/NHL_'

for(x in 1980:2019) {
  website <- paste0(base_url, x, '_entry.html')
  temp <- Ref_Draft_Scraper(website)
  fwrite(temp, here("Data", "Hockeyref Data", paste0('HockeyRef Draft ', x, '.csv')))
}
