require(here)
require(wrapr)
require(data.table)

##Drafted ----

# Pulling in data from all drafts ----
for(year in 1980:2019) {
  df <- paste0('EP', year)
  file_directory <- here('Data', 'Eliteprospects Data', paste0('Eliteprospects Draft ', year, '.csv'))
  let(alias = list(rname = df), expr = (rname <- read.csv(file_directory)))
}


# Check for duplicated players (some players were drafted multiple times) ----
dup_test_df <- data.frame(Year = numeric(0), ID = numeric(0))

for(year in 1980:2019) {
  temp <- .GlobalEnv[[paste0('EP', year)]] %$%
    ID %>%
    unique()
  temp_df <- data.frame(Year = year, ID = temp)
  dup_test_df <- rbind(dup_test_df, temp_df)
}

dup_test_df <- dup_test_df %>%
  .[duplicated(.$ID),] %>%
  mutate(combined = paste0(Year, ID))

row_count <- rep(0, nrow(dup_test_df))

for(i in 1:nrow(dup_test_df)) {
  temp <- dup_test_df[i,]
  temp_df <- .GlobalEnv[[paste0('EP', temp$Year)]]
  row_count[i] <- temp_df %>%
    filter(ID == temp$ID) %>%
    nrow()
}

dup_test_df$row_count <- row_count

sum(dup_test_df$row_count)

# Combining all data together and removing duplicates ----
Combined <- EP1980 %>%
  mutate(Year = 1980)

for(Year in 1981:2019) {
  temp <- .GlobalEnv[[paste0('EP', Year)]] %>%
    mutate(Year = Year)
  Combined <- rbind(Combined, temp)
}

## The way this should work is that if a row has a filled in `Year.y` or `ID.y` (from the dup_test_df), then it 
##  Is a duplicated entry
Combined <- Combined %>%
  mutate(combined = paste0(Year, ID)) %>%
  left_join(dup_test_df, by='combined') %>%
  filter(is.na(Year.y)) %>%
  select(ID.x:X...) %>%
  rename(ID = ID.x)

#Total number of rows: 101250
#Number of duplicated rows: 853
#Combined data.frame row count: 100397

#Based on row counts, it seems like we have properly removed just the duplicated rows.
fwrite(Combined, here("Data", "Eliteprospects Data", paste0('Eliteprospects Clean Drafted.csv')))


