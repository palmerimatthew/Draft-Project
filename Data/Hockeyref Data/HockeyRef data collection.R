require(here)
require(data.table)
require(stringr)
require(data.table)

## Drafts ----
base_url <- 'https://www.hockey-reference.com/draft/NHL_'

for(x in 1980:2019) {
  website <- paste0(base_url, x, '_entry.html')
  temp <- Ref_Draft_Scraper(website)
  fwrite(temp, here("Data", "Hockeyref Data", paste0('HockeyRef Draft ', x, '.csv')))
}


# Undrafted ----

### Only need to grab drafted players

base_url <- 'https://www.hockey-reference.com/play-index/psl_finder.cgi?c2stat=&c4stat=&c2comp=gt&is_playoffs=N&order_by_asc=&birthyear_max=&birthyear_min=&c1comp=gt&year_min=1991&request=1&franch_id=&is_hof=&birth_country=&match=single&year_max=2019&c3comp=gt&season_end=1&is_active=&c3stat=&lg_id=NHL&order_by=goals&season_start=1&c1val=&threshhold=5&c3val=&c2val=&am_team_id=&handed=&rookie=N&pos=S&describe_only=&c1stat=&draft=none&c4val=&age_min=0&c4comp=gt&age_max=99&offset='

links <- character()
for(i in seq(0, 600, 200)) {
  website <- paste0(base_url, i)
  temp_links <- website %>%
    readLines() %>%
    paste(collapse = '\n') %>%
    str_match_all("<a href=\"(.*?)\"") %>%
    .[[1]] %>%
    .[,2] %>%
    .[grep('players', .)] %>%
    .[-(c(1,(length(.) - 15):length(.)))]
  links <- append(links, temp_links)
}
links <- paste0('https://www.hockey-reference.com', links)

undrafted <- Ref_Player_Scraper(links[1])
for(i in 2:length(links)) {
  temp <- Ref_Player_Scraper(links[i])
  undrafted <- smart_rbind(undrafted, temp)
}

fwrite(undrafted, here('Data', 'Hockeyref Data', 'Hockeyref undrafted raw.csv'))
