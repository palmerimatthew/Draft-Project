RefDraftScraper <- function(website, ageLimit, byYear = F) {
  Parse <- read_html(website)
  Nodes <- html_nodes(Parse, "table")
  draftTable <- html_table(Nodes, header = TRUE, fill = TRUE)[[1]]
  colnames(draftTable) <- draftTable[1,]
  draftTable[,1] <- as.numeric(draftTable[,1])
  naTest <- is.na(draftTable[,1])
  draftTable <- draftTable[!naTest,]
  goalieIndex <- grep("G", draftTable$Pos)
  
  html <- paste(readLines(website), collapse="\n")
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  links <- matched[[1]][,2]
  playerLinks <- grep("/players/", links)
  playerLinks <- links[playerLinks[1:nrow(draftTable)]]
  playerLinks <- paste("https://www.hockey-reference.com", playerLinks, sep = "")
  returnTable <- RefPlayerScraper(playerLinks[1], ageLimit, byYear)
  playerLinks <- playerLinks[-1]
  for(player in playerLinks) {
    playerTable <- RefPlayerScraper(player, ageLimit, byYear)
    returnTable <- rbind(returnTable, playerTable)
  }
  returnTable
}

RefPlayerScraper <- function(website, ageLimit, byYear = F) {
  Parse <- read_html(website)
  Nodes <- html_nodes(Parse, "div")
  nodesText <- html_text(Nodes)
  
}