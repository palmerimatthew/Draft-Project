require(here)
require(wrapr)
require(data.table)
require(magrittr)
require(dplyr)
require(tidyr)

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
fwrite(Combined, here("Data", "Eliteprospects Data", 'Eliteprospects Clean Drafted.csv'))


# Combining undrafted eliteprospects data ----

# Combined_undrafted <- Undrafted %>%
#   filter(Drafted_Team == '' &
#            !grepl('G', Position) &
#            League != '' &
#            GP != '-' &
#            GP != '' &
#            GP != '0') %>%
#   mutate(Drafted_Team = NA_character_,
#          Birth_State = as.character(Birth_State),
#          Birth_State = if_else(Birth_State == '', NA_character_, Birth_State),
#          Birth_Country = as.character(Birth_Country),
#          Birth_Country = if_else(Birth_Country == '', NA_character_, Birth_Country),
#          Birth_City = as.character(Birth_City),
#          Birth_City = if_else(Birth_City == '', NA_character_, Birth_City),
#          Country = as.character(Country),
#          Country = if_else(Country == '', NA_character_, Country),
#          Team = as.character(Team),
#          Team = if_else(Team == '', NA_character_, Team))
# 
#   mutate(Position = as.character(Position),
#          Shoots = as.character(Shoots),
#          Shoots = case_when(Shoots == 'l' ~ 'L',
#                             Shoots == 'LR' ~ NA_character_,
#                             Shoots == 'RL' ~ NA_character_,
#                             Shoots == '' ~ NA_character_,
#                             Shoots == 'D' ~ NA_character_,
#                             Shoots == 'F' ~ NA_character_,
#                             TRUE ~ Shoots),
#          Drafted_Team = NA_character_,
#          Birth_State = as.character(Birth_State),
#          Birth_State = case_when(Birth_Country == 'CA USA' ~ 'CA', 
#                                  Birth_State == 'Ca' ~ 'CA',
#                                  Birth_State == '' ~ NA_character_,
#                                  TRUE ~ Birth_State),
#          Birth_Country = as.character(Birth_Country),
#          Birth_Country = case_when(Birth_Country == 'CA USA' ~ 'USA',
#                                    Birth_Country == 'US' ~ 'USA',
#                                    Birth_Country == 'USa' ~ 'USA',
#                                    Birth_Country == 'C' ~ 'CAN',
#                                    Birth_Country == 'Haiti' ~ 'HTI',
#                                    Birth_Country == 'HK' ~ 'HKG',
#                                    Birth_Country == 'S&gt;VK' ~ 'SVK',
#                                    Birth_Country == 'Swe' ~ 'SWE',
#                                    Birth_Country == '' ~ NA_character_,
#                                    TRUE ~ Birth_Country),
#          Birth_City = as.character(Birth_City),
#          Birth_City = if_else(Birth_City == '', NA_character_, Birth_City),
#          Country = as.character(Country),
#          Country = if_else(Country == '', NA_character_, Country),
#          Team = as.character(Team),
#          Team = if_else(Team == '', NA_character_, Team),
#          League = as.character(League),
#          GP = as.numeric(as.character(GP)))

# Seperating data into production data and player details ----

drafted_combined <- read.csv(here("Data", "Eliteprospects Data", 'Eliteprospects Clean Drafted.csv'))

undrafted_combined <- data.frame()

for(i in 1:58) {
  temp <- read.csv(here('Data', 'Eliteprospects Data', paste0('Eliteprospects Undrafted Segment ', i, '.csv')))
  undrafted_combined <- rbind(undrafted_combined, temp)
}

undrafted_combined <- filter(undrafted_combined, 
                             (Drafted_Team == '' | is.na(Drafted_Team)) &
                               !grepl('G', Position))

fwrite(undrafted_combined, here('Data', 'Eliteprospects Data', 'Eliteprospects Clean Undrafted.csv'))

combined <- rbind(drafted_combined, undrafted_combined) %>%
  filter(League != '' &
           GP != '-' &
           GP != '' &
           GP != '0' &
           ID != 5313 &     #This player swapped from goalie to player
           ID != 13927) %>% #This player swapped from goalie to player
  mutate(Name = as.character(Name))

combined_player_stats <- combined %>%
  select(ID, Name, Season:X...) %>%
  mutate(Season = as.character(Season),
         Team = as.character(Team),
         League = as.character(League),
         GP = as.numeric(as.character(GP)),
         G = as.numeric(as.character(G)),
         A = as.numeric(as.character(A)),
         TP = as.numeric(as.character(TP)),
         PIM = as.numeric(as.character(PIM)),
         X... = as.character(X...),
         X... = case_when(X... == '--33' ~ '-33',
                          X... == '--6' ~ '-6',
                          X... == '-0' ~ '0',
                          X... == '-2-1' ~ '-2',
                          X... == '-6su' ~ '-6',
                          X... == '1-' ~ '-1',
                          X... == '1-7' ~ '-17',
                          X... == '3&#2' ~ '32'))

combined_player_detail <- combined %>%
  select(ID:Weight) %>%
  .[!duplicated(.$ID),] %>%
  mutate(Position_Clean = Position) %>%
  rename(Position_Raw = Position) %>%
  separate(Position_Clean, )


f































Drafted <- read.csv(here("Data", "Eliteprospects Data", 'Eliteprospects Clean Drafted.csv'))

Undrafted <- read.csv(here('Data', 'Eliteprospects Data', 'Eliteprospects Clean Undrafted.csv'))

Combined <- rbind(Drafted, Undrafted)

fwrite(Combined, here('Data', 'Eliteprospects Data', 'Eliteprospects Clean Combined.csv'))