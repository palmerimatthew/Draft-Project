library(rvest)
library(XML)
library(stringr)

#stats codebook
###all = all available stats
###stateSep = seperate goals, assists, and points based on strength (PP, SH, EV)
###Cor = Corsi
###Fen = Fenwick
###PDO = PDO
###ZS = even strength zone starts
###IceTime = Ice time
###Awards = Awards with placement
###pmBreak = plus/minus breakdown (GF, GA, etc.)
###PS = HockeyRef's point shares metric

#Season codebook
###R = Regular season
###P = Playoffs
###RP = Regular season and playoffs (in seperate tables)

RefDraftScraper <- function(website, ageLimit, byYear = F, Season = "R") {
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
  
  returnTable$Player <- RefPlayerScraper(playerLinks[1], ageLimit, byYear)
  playerLinks <- playerLinks[-1]
  if (Season == "R" | Season == "P") {
    #Single table returned
    for(player in playerLinks) {
      playerTable <- RefPlayerScraper(player, ageLimit, byYear, Season)
      returnTable$Player <- rbind(returnTable$Player, playerTable)
    }
  } else {
    #Multiple tables returned
    for(player in playerLinks) {
      playerTable <- RefPlayerScraper(player, ageLimit, byYear, Season)
      returnTable$Player$Regular <- rbind(returnTable$Regular, playerTable$Regular)
      returnTable$Player$Playoff <- rbind(returnTable$Playoff, playerTable$Playoff)
    } #for
  } #if/else (Season == "R" | Season == "P")
  returnTable
}



RefPlayerScraper <- function(website, ageLimit, byYear = F, stats, Season) {
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
  tables <- readHTMLTable(matched)
  
  #grabbing desired tables
  namesList <- names(tables)
  if(Season == "P" | Season == "RP") {
    playoffTable <- grep("stats_basic_plus_nhl_po", namesList)
    if (length(playoffTable) == 0) {
      remove(playoffTable)
    } else {
      playoffTable <- tables[[playoffTable]]
    } #if(length(playoffTable) == 0)
  } #if(Season == "P" | Season == "RP")
  if(Season == "R" | Season == "RP") {
    #grabbing wanted stats
    generalStats <- grep("stats_basic_plus_nhl$", namesList)
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
    if("Awards" %in% Stats) {
      
    }
  }
}