require(rvest)
require(stringr)
require(magrittr)
require(dplyr)
require(tidyr)
require(XML)

draftScraper <- function(Data, draft = T, Agerange = c(17, 25), draft.year = T, draft.pick = T, round = T, 
                         draft.elig = T, Agerel = "9/15", Goalie = F, position = T, shoots = T, 
                         Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-", "sv%", "GAA"),
                         Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, 
                         dbsep = T, drafted.team = T, Season = 'R') {
  links <- paste(readLines(Data), collapse = "\n") %>%
    str_match_all("<a href=\"(.*?)\"") %>%
    extract2(1)  %>%
    .[-(1:300),2] %>%
    .[grep('player',.)]
  goalie_spots <- read_html(Data) %>%
    html_nodes("table") %>%
    html_table(header = T, fill = T) %>%
    extract2(2) %>%
    filter(!Team %in% paste('ROUND', c(1,2,3,4,5,6,7,8,9))) %>%
    separate(Player, c('Name', 'Position'), '\\(') %>%
    use_series(Position) %>%
    substr(1,1) %>%
    grep("G", .)
  player_links <- links[-goalie_spots]
  if (Goalie) {
    goalie_links <- links[goalie_spots]
  }
  player_template <- indScraper(playerLinks[1], Agerange, draft.year, draft.pick, round, draft.elig, Agerel, position, 
                         shoots, Stats, Place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, drafted.team, Season)
  
}


IndScraper <- function(website, Agerange = c(17, 25), draft.year = T, draft.pick = T, round = T, 
                       draft.elig = T, Agerel = "9/15", position = T, shoots = T, 
                       Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-"),
                       Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, 
                       dbsep = T, drafted.team = T, Season = 'R') {
  
  #Preliminary tables and configuring information ----
  html <- website %>%
    readLines()
  information <- get_EP_Information(html)
  stat_table <- get_EP_table(html, Season)
  
  #Gathering desired information ----
  
  #draft pick information
  if(draft.year | draft.pick | round |draft.elig | drafted.team) {
    
    if(length(grep('Drafted', information)) == 0) {
      
      if (draft.year) {Draft_Year <- NA}
      if (draft.pick) {SDraft_Pick <- NA}
      if (round) {Round <- NA}
      if (draft.elig) {Draft_Elig <- NA}
      if (drafted.team) {Drafted_Team <- NA}
      
    } else {
      
      draft_statement <- information %>%
        .[grep('Drafted', .)+1] %>%
        str_split('>|<') %>%
        extract2(1) %>%
        .[grep('#', .)] %>%
        trimws() %>%
        str_split(' ') %>%
        extract2(1)
      
      if (draft.year) {
        Draft_Year <- draft_statement %>%
          .[1] %>%
          as.numeric()
      }
      
      if (draft.pick) {
        Draft_Pick <- draft_statement %>%
          .[4] %>%
          gsub('#', '', .) %>%
          as.numeric()
      }
      
      if (round) {
        Round <- draft_statement %>%
          .[3] %>%
          as.numeric()
      }
      #Figure out how I want to do this
      if (draft.elig) {
        
      }
      
      if (drafted.team) {
        Drafted_Team <- draft_statement %>%
          .[7:length(.)] %>%
          paste(collapse = ' ')
      }
    }
  }
  
  #Shoot and Position information
  if(shoots) {
    Shoots <- information %>%
      .[grep('Shoots', .) + 1] %>%
      str_split('<|>') %>%
      extract2(1) %>%
      .[length(.) - 2] %>%
      trimws()
  }
  if(position) {
    Position <- information %>%
      .[grep('Position', .) + 2] %>%
      trimws()
    #Added shooting side for defensemen
  }
  
  #Birthplace information
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

get_EP_table <- function(html, Season) {
  
  right_start <- html %>%
    grep('<table', .) %>%
    .[2] %>%
    as.numeric()
  
  right_end <- html %>%
    grep('</table>', .) %>%
    .[. > right_start] %>%
    .[1] %>%
    as.numeric()
  
  full_table <- html %>%
    .[right_start:right_end] %>%
    paste(collapse = '\n') %>%
    readHTMLTable() %>%
    extract2(1)
  
  if (Season == 'R') {
    full_table %>%
      .[,-(10:ncol(.))]
    
  } else if (Season == 'P') {
    full_table %>%
      .[, -(4:10)]
    
  } else if (Season == 'RP') {
    regularseason_table <- full_table %>%
      .[,-(10:ncol(.))]
    playoff_table <- full_table %>%
      .[, -(4:10)]
    list(Regular = regularseason_table, Playoffs = playoff_table)
  }
  
}


