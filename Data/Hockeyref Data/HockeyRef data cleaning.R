require(magrittr)
require(here)
require(devtools)
install_github('https://github.com/palmerimatthew/HockeyRefScraper')
require(HockeyRefScraper)


# Drafted HockeyRef combination ----

combined <- data.frame()

for(year in 1980:2018) {
  temp <- read.csv(here('Data', 'Hockeyref Data', paste0('HockeyRef Draft ', year, '.csv')))
  combined <- smart_rbind(combined, temp)
}

