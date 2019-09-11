require(rvest)
require(stringr)
require(tidyverse)
require(XML)
require(lubridate)
require(magrittr)

draft_Scraper <- function(Data, Agerange = c(17, 25), draft.year = T, draft.pick = T, round = T, 
                         draft.elig = T, Agerel = "9/15", Goalie = F, position = T, shoots = T, 
                         Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-", "sv%", "GAA"),
                         place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, 
                         dbsep = T, drafted.team = T, reg.playoffs = 'R') {
  links <- paste(readLines(Data), collapse = "\n") %>%
    str_match_all("<a href=\"(.*?)\"") %>%
    extract2(1)  %>%
    .[-(1:300),2] %>%
    .[grep('player',.)]
  
  goalie_spots <- read_html(Data) %>%
    html_nodes("table") %>%
    html_table(header = T, fill = T) %>%
    extract2(2) %>%
    filter(!Team %in% paste('ROUND', c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) &
             Player != 'No selection was made') %>%
    separate(Player, c('Name', 'Position'), '\\(', fill = 'right') %$%
    Position %>%
    substr(1,1) %>%
    grep("G", .)
  
  player_links <- links[-goalie_spots]
  if (Goalie) {
    goalie_links <- links[goalie_spots]
  }
  player_template <- Ind_Scraper(player_links[1], Agerange, draft.year, draft.pick, round, draft.elig, Agerel, position, 
                                shoots, Stats, place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, drafted.team, reg.playoffs)
  
  player_data <- player_template %>%
    filter(Season == 'F')
  
  for(link in player_links) {
    temp <- Ind_Scraper(link, Agerange, draft.year, draft.pick, round, draft.elig, Agerel, position, 
                shoots, Stats, place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, drafted.team, reg.playoffs)
    player_data <- player_data %>%
      rbind(temp)
  }
  player_data
}

league_list_scraper <- function(Data, undrafted = T) {
  num_pages <- Data %>%
    readLines() %>%
    .[grep('table-pagination', .)+1] %>%
    .[1] %>%
    gsub(' players found', '', .) %>%
    gsub(' ', '', .) %>%
    as.numeric() %>%
    divide_by(100) %>%
    ceiling()
  
  all_links <- character(0)
  
  for(i in 1:num_pages) {
    website <- paste0(Data, '?page=', i)
    html <- website %>%
      readLines()
    links <- html %>%
      paste(collapse = '\n') %>%
      str_match_all("<a href=\"(.*?)\"") %>%
      .[[1]] %>%
      .[-(1:300),2] %>%
      .[grep('player', .)]
    
    first_goalie_link <- html %>%
      get_EP_table('AB', 'Undrafted') %$%
      Player %>%
      .[1] %>%
      as.character() %>%
      gsub(' ', '-', .) %>%
      tolower() %>%
      grep(links) %>%
      as.numeric()
    
    links <- links[-(first_goalie_link:length(links))]
    all_links <- c(all_links, links)
  }
  boolean <- rep(T, length(all_links))
  for(i in 1:length(all_links)) {
    boolean[i] <- !draft_boolean(all_links[i])
  }
  all_links[boolean]
}

draft_boolean <- function(website) {
  html <- website %>%
    readLines()
  
  information <- get_EP_Information(html)
  
  drafted <- information %>%
    grep('Drafted', .)
  
  length(drafted) != 0
}

NHL_boolean <- function(website) {
  html <- website %>%
    readLines()
  
  information <- get_EP_Information(html)
  
  #getting name
  Name <- information %>%
    .[(grep('plytitle', .)+1):(grep('plytitle', .) + 3)] %>%
    .[!grepl('<', .)] %>%
    trimws()
  
  #AB doesn't mean anything, we just want it to get through to the else at the end
  NHL <- get_EP_table(html, 'AB', 'Career') %>%
    .[,1:8] %>%
    select(League, GP) %>%
    filter(League == 'NHL') %$%
    GP %>%
    as.numeric()
  
  if(length(NHL) == 0) {
    character(0)
  } else if (NHL > 0) {
    Name
  } else {
    character(0)
  }
}

Ind_Scraper <- function(website, Agerange = c(17, 25), draft.year = T, draft.pick = T, round = T, 
                        draft.elig = T, Agerel = "9/15", position = T, shoots = T, 
                        Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-"),
                        place.birth = T, Pbsep = T, country = T, height = T, weight = T, date.birth = T, 
                        dbsep = T, drafted.team = T, reg.playoffs = 'R') {
  print(website)
  control <- T
  #Preliminary tables and configuring information ----
  html <- website %>%
    readLines()
  
  #information section
  information <- get_EP_Information(html)
  
  #Birthdate for age in table
  Birth_Date <- information %>%
    .[grep('Date of Birth', .) + 2] %>%
    str_split('<|>') %>%
    .[[1]] %>%
    .[3] %>%
    trimws()
  if(!grepl(',', Birth_Date)) {
    control <- F
  } else {
    Birth_Date <- mdy(Birth_Date)
  }
  
  stat_table <- get_EP_table(html, reg.playoffs) #Getting stats table
  
  if (length(stat_table) == 0 | !control) {
    control <- F
  } else {
    stat_table <- stat_table %>%
      mutate(Season = add_missing_season(as.character(S)), #filling in missing season data
             Age = exact_age(Season, Birth_Date, Agerel),
             age_at_draft = exact_age(Season, Birth_Date, "9/15")) %>% #Adding age to table
      select(Season, Age, age_at_draft, Team:`+/-`) %>%
      filter(Age >= Agerange[1] & Age <= Agerange[2])
  }
  if (control) {
    if(nrow(stat_table) == 0) {
      control <- F
    }
  }
  #Gathering desired information ----
  
  if(control){
    #This is in pounds
    if(weight) {
      Weight <- information %>%
        .[grep('>Weight <', .) + 2] %>%
        str_split('/') %>%
        .[[1]] %>%
        .[1] %>%
        gsub('lbs', '', .) %>%
        trimws()
      if(Weight == '-') {
        Weight <- NA
      } else {
        Weight <- as.numeric(Weight)
      }
      stat_table <- cbind(Weight, stat_table)
    }
    
    #This is in centimeters
    if(height) {
      Height <- information %>%
        .[grep('Height', .)[length(grep('Height', .))] + 2] %>%
        str_split('/') %>%
        .[[1]] %>%
        trimws()
      if(Height[1] == '-') {
        Height <- NA
      } else {
        Height <- Height %>%
          .[2] %>%
          gsub('cm', '', .) %>%
          as.numeric()
      }
      stat_table <- cbind(Height, stat_table)
    }
    
    #Country they would represent in an international country. This can be different from birth place
    if(country) {
      Country <- information %>%
        .[grep('Nation', .) + 2] %>%
        str_split('<|>') %>%
        .[[1]] %>%
        .[3] %>%
        trimws()
      stat_table <- cbind(Country, stat_table)
    }
    
    #Birthplace information
    if(place.birth) {
      Birth_Place <- information %>%
        .[grep('Place of Birth', .) + 2] %>%
        str_split('<|>') %>%
        extract2(1) %>%
        .[grep(',', .)] %>%
        trimws() %>%
        gsub('&#039;', '\'', .)
      
      if(identical(Birth_Place, character(0))) {
        Birth_Place <- NA
      }
      
      if(Pbsep) {
        if(identical(Birth_Place, character(0))) {
          Birth_City <- NA
          stat_table <- cbind(Birth_City, stat_table)
          Birth_State <- NA
          stat_table <- cbind(Birth_State, stat_table)
          Birth_Country <- NA
          stat_table <- cbind(Birth_Country, stat_table)
        } else {
          split_birth <- Birth_Place %>%
            str_split(', ') %>%
            extract2(1)
          
          if(length(split_birth) == 2) {
            Birth_Country <- split_birth[2]
            if(str_length(split_birth[1]) == 2 & split_birth[1] == toupper(split_birth[1])) {
              Birth_State <- split_birth[1]
              Birth_City <- NA
            } else {
              Birth_State <- NA
              Birth_City <- split_birth[1]
            }
          } else {
            Birth_City <- split_birth[1]
            Birth_State <- split_birth[2]
            Birth_Country <- split_birth[3]
          }
          
          stat_table <- cbind(Birth_City, stat_table)
          stat_table <- cbind(Birth_State, stat_table)
          stat_table <- cbind(Birth_Country, stat_table)
        }
      } else {
        stat_table <- cbind(Birth_Place, stat_table)
      }
    }
    
    #Birthdate information and seperation
    if(dbsep & date.birth) {
      Birth_Date <- Birth_Date %>%
        as.character() %>%
        str_split('-') %>%
        .[[1]]
      
      Birth_Day <- Birth_Date[3]
      stat_table <- cbind(Birth_Day, stat_table)
      
      Birth_Month <- Birth_Date[2]
      stat_table <- cbind(Birth_Month, stat_table)
      
      Birth_Year <- Birth_Date[1]
      stat_table <- cbind(Birth_Year, stat_table)
      
    } else if (date.birth) {
      stat_table <- cbind(Birth_Date, stat_table)
    }
    
    #draft pick information
    if(draft.year | draft.pick | round | drafted.team) {
      
      if(length(grep('Drafted', information)) == 0) {
        
        if (draft.year) {
          Draft_Year <- NA
          stat_table <- cbind(Draft_Year, stat_table)
        }
        if (draft.pick) {
          Draft_Pick <- NA
          stat_table <- cbind(Draft_Pick, stat_table)
        }
        if (round) {
          Round <- NA
          stat_table <- cbind(Round, stat_table)
        }
        if (drafted.team) {
          Drafted_Team <- NA
          stat_table <- cbind(Drafted_Team, stat_table)
        }
        
      } else {
        
        draft_statement <- information %>%
          .[grep('<div (.*) Drafted', .)[length(grep('<div (.*) Drafted', .))]+1] %>%
          str_split('>|<') %>%
          extract2(1) %>%
          .[grep('#', .)] %>%
          trimws() %>%
          str_split(' ') %>%
          extract2(1)
        
        Draft_Year <- draft_statement %>%
          .[1] %>%
          as.numeric()
        
        if (draft.pick) {
          Draft_Pick <- draft_statement %>%
            .[4] %>%
            gsub('#', '', .) %>%
            as.numeric()
          stat_table <- cbind(Draft_Pick, stat_table)
        }
        
        if (round) {
          Round <- draft_statement %>%
            .[3] %>%
            as.numeric()
          stat_table <- cbind(Round, stat_table)
        }
        
        if (draft.year) {
          stat_table <- cbind(Draft_Year, stat_table)
        }
        
        if (drafted.team) {
          Drafted_Team <- draft_statement %>%
            .[7:length(.)] %>%
            paste(collapse = ' ')
          stat_table <- cbind(Drafted_Team, stat_table)
        }
      }
    }
    
    #Shoot and Position information
    Shoots <- information %>%
      .[grep('Shoots', .) + 1] %>%
      str_split('<|>') %>%
      extract2(1) %>%
      .[length(.) - 2] %>%
      trimws()
    if (Shoots == '-') {
      Shoots <- NA
    }
    if(shoots) {
      stat_table <- cbind(Shoots, stat_table)
    }
    
    if(position) {
      
      Position <- information %>%
        .[grep('Position', .) + 2] %>%
        .[!grepl('<', .)] %>%
        trimws()
      
      #If a player has a position like LW/D, we want to preserve order (so this becomes LW/LD)
      if(grepl('D', Position) & !is.na(Shoots)) {
        #split position based on /
        temp <- Position %>%
          str_split('/') %>%
          .[[1]]
        #which entry has D
        num <- temp %>%
          grep('D', .) %>%
          as.numeric()
        #change this entry to include shooting side
        temp[num] <- paste0(Shoots, 'D')
        Position <- temp %>%
          paste(collapse = '/')
        #removing temporary variables
        rm(num)
        rm(temp)
      }
      stat_table <- cbind(Position, stat_table)
    }
    
    #getting name
    Name <- information %>%
      .[(grep('plytitle', .)+1):(grep('plytitle', .) + 3)] %>%
      .[!grepl('<', .)] %>%
      trimws()
    stat_table <- cbind(Name, stat_table)
    
    ID <- website %>%
      str_split('/') %>%
      .[[1]] %>%
      .[grep('player', .) + 1] %>%
      as.numeric()
    stat_table <- cbind(ID, stat_table)
    
    #returning table
    stat_table <- stat_table %>%
      select(-age_at_draft)
    
    stat_table
  }
}

get_EP_Information <- function(html) {
  
  right_start <- html %>%
    grep('plyr_details', .) %>%
    as.numeric()
  
  right_end <- html %>%
    grep('</section>', .) %>%
    .[. > right_start] %>%
    .[1] %>%
    as.numeric()
  
  html[right_start:right_end]
}

get_EP_table <- function(html, Season, Need = 'Stats') {
  
  if(Need == 'Stats') {
    right_start <- html %>%
      grep('<table(.*) player-stats', .) %>%
      .[1] %>%
      as.numeric()
  } else if(Need == 'Career') {
    right_start <- html %>%
      grep('<table(.*) total-player-stats', .) %>%
      as.numeric()
  } else if(Need == 'Undrafted') {
    right_start <- html %>%
      grep('<table(.*) goalie-stats', .) %>%
      as.numeric()
  }
  
  right_end <- html %>%
    grep('</table>', .) %>%
    .[. > right_start] %>%
    .[1] %>%
    as.numeric()
  
  full_table <- html %>%
    .[right_start:right_end] %>%
    paste(collapse = '\n') %>%
    readHTMLTable() %>%
    .[[1]]
  
  if (length(full_table) == 0) {
    full_table
  } else if (Season == 'R') {
    full_table %>%
      .[,-(10:ncol(.))]
    
  } else if (Season == 'P') {
    full_table %>%
      .[, -(4:10)]
    
  } else if (Season == 'RP') {
    regularseason_table <- full_table %>%
      .[,-(10:ncol(.))] %>%
      mutate(Regular_Playoffs = 'Regular')
    playoff_table <- full_table %>%
      .[, -(4:11)] %>%
      mutate(Regular_Playoffs = 'Playoffs')
    rbind(regularseason_table, playoff_table)
  } else {
    full_table
  }
  
}

add_missing_season <-function(column) {
  return <- column
  if(length(return) > 1) {
    for(val in 2:length(return)) {
      if(return[val] == '') {
        return[val] <- return[val-1]
      }
    }
  }
  return
}

exact_age <- function(Years, birthday, rel_date) {
  Years %>%
    gsub('-.*', '', .) %>%
    as.numeric() %>%
    add(1) %>%
    paste(rel_date, sep = '/') %>%
    as.Date('%Y/%m/%d') %>%
    relative_age(birthday, .)
}

relative_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age <- to_lt$year - from_lt$year
  age <- ifelse(to_lt$mon < from_lt$mon |
                  (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
                age - 1, age)
  from_lt$year <- from_lt$year + age[1]
  middle_age <- from_lt %>%
    interval(to_lt[1]) %>%
    as.period('days') %>%
    .$day %>%
    divide_by(365)
  
  age + middle_age
}

