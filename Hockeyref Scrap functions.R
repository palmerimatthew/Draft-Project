library(rvest)
library(XML)
library(stringr)
library(tidyverse)

#skater stats codebook
###all = all available stats
###stateSep = seperate goals, assists, and points based on strength (PP, SH, EV)
###Cor = Corsi
###Fen = Fenwick
###oiS = On-Ice shooting and save percentage
###PDO = PDO
###IceTime = Ice time
###Awards = Awards with placement
###pmBreak = plus/minus breakdown (GF, GA, etc.)
###PS = HockeyRef's point shares metric
###xGF = expected Goals For percentage 


#goalie stats codebook
###QS - quality starts metric
###GSAA - goals saved above average
###Scoring - goals/assists/points/etc.
###Awards - Awards with placement


#Season codebook
###R = Regular season
###P = Playoffs
###RP = Regular season and playoffs (in seperate tables)

Ref_Draft_Scraper <- function(website, ages = c(17, 50), playerStats = "all", goalieStats = "all", Season = "R") {
  links <- website %>%
    readLines() %>%
    paste(collapse = '\n') %>%
    str_match_all("<a href=\"(.*?)\"") %>% #just grabbing hyperlinks in the html
    .[[1]] %>%
    .[,2] %>%
    .[grep('/players/.', .)] %>% #only want the hyperlinks that link to players
    paste0('https://www.hockey-reference.com', .)
  
  returnTable <- Player_Wrapper(links, ages, playerStats, goalieStats, Season)
  
  returnTable
}



RefPlayerScraper <- function(website, ages = c(17,50), Stats = "all", Season = "R", sepTeam = F) {
  tables <- getHockeyRefTables(website)
  if(Stats == "all") {
    Stats <- c("Cor", "Fen", "PDO", "oiS", "IceTime", "Awards", "pmBreak", "PS", "xGF")
  }
  
  #grabbing desired tables
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic(.*?)nhl_po", namesList)
    if (length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      
      #Constricting table to age limit
      playoffTable$Age <- as.numeric(levels(playoffTable$Age))[playoffTable$Age]
      playoffTable <- playoffTable[playoffTable$Age >= ages[1],]
      playoffTable <- playoffTable[playoffTable$Age <= ages[2],]
    } #if(length(playoffTable) == 0)
  } #if(Season == "P" | Season == "RP")
  if(Season == "R" | Season == "RP") {
    #grabbing wanted stats
    generalStats <- grep("stats_basic(.*?)nhl$", namesList)
    if(length(generalStats) != 0) {
      generalStats <- tables[[generalStats]]
      ev <- grep("EV", colnames(generalStats))
      
      #Based on stateSep inclusion in 'Stats'
      if("stateSep" %in% Stats) {
        colnames(generalStats)[ev[1]:(ev[1] + 3)] <- c("EVG", "PPG", "SHG", "GWG")
        colnames(generalStats)[ev[2]:(ev[2] + 2)] <- c("EVA", "PPA", "SHA")
        generalStats <- generalStats[,-((ev[1] - 5):(ev[1] - 3))]
      } else {
        generalStats <- generalStats[,-(ev[1]:(ev[2] + 2))]
      }
      
      #Based on Awards inclusion in 'Stats'
      if(!("Awards" %in% Stats)) {
        generalStats <- generalStats[,-ncol(generalStats)]
      }
      
      #Based on Ice Time inclusion in 'Stats'
      if(!("IceTime" %in% Stats)) {
        index <- grep("TOI", colnames(generalStats))
        generalStats <- generalStats[,-(index:(index + 1))]
      }
      generalStats <- removeDuplicateYears(generalStats, sepTeam)
      
      #Based on Corsi, Fenwick, or PDO inclusion in 'Stats'
      if("Cor" %in% Stats || "Fen" %in% Stats || "PDO" %in% Stats || "oiS" %in% Stats) {
        possessionTable <- grep("skaters_advanced", namesList)
        if(length(possessionTable) != 0) {
          possessionTable <- tables[[possessionTable]]
          #removing zone starts
          index <- grep("oZS", colnames(possessionTable))
          if (length(index) != 0) {
            possessionTable <- possessionTable[, -(index:(index + 1))]
          }
          #If Corsi is not wanted
          if(!("Cor" %in% Stats)) {
            index <- grep("CF$", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          #If Fenwick is not wanted
          if(!("Fen" %in% Stats)) {
            index <- grep("FF$", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          #If PDO is not wanted
          if(!("PDO" %in% Stats)) {
            index <- grep("PDO", colnames(possessionTable))
            possessionTable <- possessionTable[,-index]
          }
          #If On-Ice shooting and save percentage is not wanted
          if(!("oiS" %in% Stats)) {
            index <- grep("oiGF", colnames(possessionTable))
            possessionTable <- possessionTable[,-(index:(index + 3))]
          }
          possessionTable <- removeDuplicateYears(possessionTable, sepTeam)
          possessionTable <- possessionTable[, -c(1,4:6)]
          generalStats <- mergeTableHockeyRef(generalStats, possessionTable, sepTeam)
        }
      }
      
      #Based on breaking up plus/minus, Point-Shares, or expected GF% being in Stats
      if("pmBreak" %in% Stats || "PS" %in% Stats || "xGF" %in% Stats) {
        miscTable <- grep("stats_misc_plus_nhl", namesList)
        miscTable <-tables[[miscTable]]
        index <- grep("Att.", colnames(miscTable))
        if (length(index) != 0) {
          miscTable <- miscTable[,-(index:(index + 3))]
        }
        #Based on inclusion of plus/minus in Stats
        if ("pmBreak" %in% Stats) {
          index <- grep("+\\-", colnames(generalStats))
          generalStats <- generalStats[,-index]
        } else {
          index <- grep("TGF", colnames(miscTable))
          miscTable <- miscTable[,-(index:(index + 4))]
        }
        #If point-shares are not wanted
        if(!("PS" %in% Stats)) {
          index <- grep("OPS", colnames(miscTable))
          miscTable <- miscTable[,-(index:(index+2))]
        }
        #If Expected goals for is not wanted
        if(!("xGF" %in% Stats)) {
          index <- grep("xGF", colnames(miscTable))
          miscTable <- miscTable[-(index:(index + 1))]
        }
        index <- grep("E+", colnames(miscTable))
        if (length(index) != 0) {
          miscTable <- miscTable[,-index]
        }
        miscTable <- removeDuplicateYears(miscTable, sepTeam)
        ####################################
        #want to fix this to be more generic
        ####################################
        miscTable <- miscTable[,-c(1,4:16)]
        generalStats <- mergeTableHockeyRef(generalStats, miscTable, sepTeam)
      }
      #Constricting table to age limit
      generalStats$Age <- as.numeric(generalStats$Age)
      generalStats <- generalStats[,c(3,1,2,4:(ncol(generalStats)))]
      generalStats <- generalStats[generalStats$Age >= ages[1],]
      generalStats <- generalStats[generalStats$Age <= ages[2],]
    }
  } #if(Season == "R" | Season == "RP")
  
  #Constructing return structures
  if(Season == "RP") {
    list(Regular = generalStats, Playoff = playoffTable)
  } else if(Season == "R") {
    generalStats
  } else if (Season == "P") {
    playoffTable
  }
}

RefGoalieScraper <- function(website, ages = c(17,50), Stats, Season = "R", sepTeam = F) {
  if (Stats == "all") {
    Stats = c("QS", "GSAA", "Scoring", "Awards")
  }
  tables <- getHockeyRefTables(website)
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic_plus_nhl_po", namesList)
    if(length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      playoffTable <- playoffTable[,-c(5,10)]
      index <- grep("GA%", colnames(playoffTable))
      playoffTable <- playoffTable[,-index]
      index <- grep("GAA", colnames(playoffTable))
      if(length(index) == 2) {
        playoffTable <- playoffTable[,-index[2]]
      }
      if (!("QS" %in% Stats)) {
        index <- grep("QS", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 2))]
      }
      if (!("GSAA" %in% Stats)) {
        index <- grep("GSAA", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      if (!("Scoring" %in% Stats)) {
        index <- grep("G$", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 3))]
      }
      if (!("Awards" %in% Stats)) {
        index <- grep("Awards", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      playoffTable <- removeDuplicateYears(playoffTable, sepTeam)
    }
  }
  if(Season == "R" | Season == "RP") {
    generalStats <- grep("stats_basic_plus_nhl$", namesList)
    if(length(generalStats) != 0) {
      generalStats <- tables[[generalStats]]
      index <- grep("GAA", colnames(playoffTable))
      if(length(index) == 2) {
        playoffTable <- playoffTable[,-index[2]]
      }
      if (!("QS" %in% Stats)) {
        index <- grep("QS", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 2))]
      }
      if (!("GSAA" %in% Stats)) {
        index <- grep("GSAA", colnames(playoffTable))
        playoffTable <- playoffTable[,-((index - 1):index)]
      }
      if (!("Scoring" %in% Stats)) {
        index <- grep("G$", colnames(playoffTable))
        playoffTable <- playoffTable[,-(index:(index + 3))]
      }
      if (!("Awards" %in% Stats)) {
        index <- grep("Awards", colnames(playoffTable))
        playoffTable <- playoffTable[,-index]
      }
      generalStats <- removeDuplicateYears(generalStats, sepTeam)
    }
  }
  
  #Constructing return structures
  if(Season == "RP") {
    list(Regular = generalStats, Playoff = playoffTable)
  } else if(Season == "R") {
    generalStats
  } else if (Season == "P") {
    playoffTable
  }
}

changeToNumeric <- function(table) {
  i = 1
  while(i <= ncol(table)) {
    suppressWarnings(new <- as.numeric(levels(table[,i]))[table[,i]])
    if(!all(is.na(new))) {
      table[,i] <- new
    }
    i <- i + 1
  }
  table
}

getHockeyRefTables <- function(website) {
  html <- readLines(website)
  #Get lines with the start and ends of tables
  start <- grep("<table", html)
  end <- grep("</table>", html)
  matched <- paste(html[start[1]:end[1]], collapse = "\n")
  val <- 2
  while(val <= length(start)) {
    matched <- append(matched, paste(html[start[val]:end[val]], collapse = "\n"))
    val <- val + 1
  }
  #creating data.frame objects with the html tables
  readHTMLTable(matched)
}

removeDuplicateYears <- function(Table, boolean) {
  duplicate <- duplicated(Table$Age)
  if (boolean) {
    i = 1
    while(i < length(duplicate)) {
      if(!duplicate[i] && duplicate[i+1]) {
        duplicate[i] = T
      } else {
        duplicate[i] = F
      }
      i <- i + 1
    }
    duplicate[i] = F
  }
  Table[!duplicate,]
}

tableEquals <- function(table1, table2) {
  if(ncol(table1) != ncol(table2) || nrow(table1) != nrow(table2)) {
    return(FALSE)
  }
  i = 1
  while(i <= nrow(table1)) {
    j = 1
    while(j <= ncol(table1)) {
      if(is.na(table1[i,j]) && !is.na(table2[i,j])) {
        if (table2[i,j] != "") {
          return(FALSE)
        }
      } else if(!is.na(table1[i,j]) && is.na(table2[i,j])) {
        if (table1[i,j] != "") {
          return(FALSE)
        }
      } else if(!is.na(table1[i,j]) && !is.na(table2[i,j])) {
        if(table1[i,j] != table2[i,j]) {
          return(FALSE)
        }
      }
      j <- j + 1
    }
    i <- i + 1
  }
  TRUE
}

mergeTableHockeyRef <- function(Table1, Table2, boolean) {
  Table1 <- unite(Table1, Age, Tm, col = "forSort", sep = "-")
  try(Table2 <- unite(Table2, Age, Tm, col = "forSort", sep = "-"), silent = T)
  try(Table2 <- unite(Table2, Age, Team, col = "forSort", sep = "-"), silent = T)
  returnTable <- merge(x = Table1, y = Table2, by = "forSort", all.x = T)
  separate(returnTable, forSort, into = c("Age", "Tm"), sep = "-")
}

