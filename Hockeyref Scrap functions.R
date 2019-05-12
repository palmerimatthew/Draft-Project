library(rvest)
library(XML)
library(stringr)
library(tidyr)

#stats codebook
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

#Season codebook
###R = Regular season
###P = Playoffs
###RP = Regular season and playoffs (in seperate tables)

RefDraftScraper <- function(website, ages = c(17, 50), stats = "all", Season = "R") {
  Parse <- read_html(website)
  Nodes <- html_nodes(Parse, "table")
  #Getting table of drafted players
  draftTable <- html_table(Nodes, header = TRUE, fill = TRUE)[[1]]
  colnames(draftTable) <- draftTable[1,]
  
  #Removing any non-player rows
  draftTable[,1] <- as.numeric(draftTable[,1])
  naTest <- is.na(draftTable[,1])
  draftTable <- draftTable[!naTest,]
  
  #Goalie index to seperate players and goalies
  goalieIndex <- grep("G", draftTable$Pos)
  
  #Getting player links
  html <- paste(readLines(website), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][,2]
  Links <- grep("/players/", links)
  Links <- links[Links[1:nrow(draftTable)]]
  Links <- paste("https://www.hockey-reference.com", Links, sep = "")
  
  draftPlayerTable <- draftTable[-goalieIndex,]
  draftGoalieTable <- draftTable[goalieIndex,]
  playerLinks <- Links[-goalieIndex]
  goalieLinks <- Links[goalieIndex]
  
  returnTable <- list(Player = RefPlayerScraper(playerLinks[1], ages, stats, Season),
                      Goalie = RefGoalieScraper(goalieLinks[1], ages, Season))
  playerLinks <- playerLinks[-1]
  if (Season == "R" | Season == "P") {
    #Single table returned
    for(player in playerLinks) {
      playerTable <- RefPlayerScraper(player, ages, stats, Season)
      returnTable$Player <- rbind(returnTable$Player, playerTable)
    }
  } else {
    #Multiple tables returned
    for(player in playerLinks) {
      playerTable <- RefPlayerScraper(player, ages, stats, Season)
      returnTable$Player$Regular <- rbind(returnTable$Regular, playerTable$Regular)
      returnTable$Player$Playoff <- rbind(returnTable$Playoff, playerTable$Playoff)
    } #for
  } #if/else (Season == "R" | Season == "P")
  returnTable
}



RefPlayerScraper <- function(website, ages = c(17,50), Stats = "all", Season = "R") {
  tables <- getTables(website)
  if(Stats == "all") {
    Stats <- c("Cor", "Fen", "PDO", "oiS", "IceTime", "Awards", "pmBreak", "PS", "xGF")
  }
  
  #grabbing desired tables
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic_plus_nhl_po", namesList)
    if (length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      playoffTable <- removeDuplicateYears(playoffTable)
      
      #Constricting table to age limit
      playoffTable$Age <- as.numeric(levels(playoffTable$Age))[playoffTable$Age]
      playoffTable <- playoffTable[playoffTable$Age >= ages[1],]
      playoffTable <- playoffTable[playoffTable$Age <= ages[2],]
    } #if(length(playoffTable) == 0)
  } #if(Season == "P" | Season == "RP")
  if(Season == "R" | Season == "RP") {
    #grabbing wanted stats
    generalStats <- grep("stats_basic_plus_nhl$", namesList)
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
      generalStats <- removeDuplicateYears(generalStats)
      #Based on Corsi, Fenwick, or PDO inclusion in 'Stats'
      if("Cor" %in% Stats || "Fen" %in% Stats || "PDO" %in% Stats || "oiS" %in% Stats) {
        possessionTable <- grep("skaters_advanced", namesList)
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
        possessionTable <- removeDuplicateYears(possessionTable)
        possessionTable <- possessionTable[, -(2:6)]
        generalStats <- merge(x = generalStats, y = possessionTable, by = "Season", all.x = T)
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
        miscTable <- miscTable[,-(ncol(miscTable))]
        miscTable <- removeDuplicateYears(miscTable)
        ####################################
        #want to fix this to be more generic
        ####################################
        miscTable <- miscTable[,-(2:16)]
        generalStats <- merge(x = generalStats, y = miscTable, by = "Season", all.x = T)
      }
      #Constricting table to age limit
      generalStats$Age <- as.numeric(levels(generalStats$Age))[generalStats$Age]
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

RefGoalieScraper <- function(website, ages = c(17,50), Season = "R") {
  tables <- getTables(website)
  namesList <- names(tables)
  if(Season = "P" | Season = "RP") {
    playoffTable <- grep("stats_basic_plus_nhl_po", namesList)
    if(length(playoffTable) != 0) {
      playoffTable <- tables[[playoffTable]]
      playoffTable <- playoffTable[,-c(5,10)]
      
    }
  }
  if(Season = "R" | Season = "RP") {
    
  }
}

getTables <- function(website) {
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

removeDuplicateYears <- function(Table) {
  duplicate <- duplicated(Table$Age)
  Table[!duplicate,]
}
