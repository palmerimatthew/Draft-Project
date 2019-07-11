require(rvest)
require(stringr)
require(magrittr)
require(dplyr)
require(tidyr)

draftScraper <- function(Data, draft = T, Agerange = c(17, 25), draft.year = T, draft.pick = T, Round = T, 
                         Draft.Elig = T, Agerel = "9/15", Goalie = F, Position = T, Shoots = T, 
                         Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-", "sv%", "GAA"),
                         Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, dbsep = T, Drafted.team = T) {
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
  player_links <- links[!goalie_spots]
  if (Goalie) {
    goalie_links <- links[goalie_spots]
  }
  player_,template <- indScraper(playerLinks[1], Agerange, draft.year, draft.pick, Round, Draft.Elig, Agerel, Goalie, Position, 
                         Shoots, Stats, Place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, Drafted.team)
  
}








# Scraper <- function(Data, draft = T, Agerange = c(17, 25), draft.year = T, draft.pick = T, Round = T, 
#                     Draft.Elig = T, Agerel = "9/15", Goalie = F, Position = T, Shoots = T, 
#                     Stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-"),
#                     Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, dbsep = T, Drafted.team = T) {
#   html <- paste(readLines(Data), collapse="\n")
#   matched <- str_match_all(html, "<a href=\"(.*?)\"")
#   links <- matched[[1]][,2]
#   playerlinks <- grep("player", links)
#   playerlinks <- playerlinks[playerlinks > 300]
#   playerlinks <- links[playerlinks]
#   picknumber <- 1
#   finalpicknum <- 2
#   #finalpicknum <- length(playerlinks)
#   website <- playerlinks[picknumber]
#   fullpage <- SkaterIndScraper(website, Agerange, draft.year, draft.pick, Round, Draft.Elig, Agerel,
#                            Position, Shoots, Stats, Place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, Drafted.team)
#   picknumber <- picknumber + 1
#   while (picknumber <= finalpicknum) {
#     website <- playerlinks[picknumber]
#     playerpage <- SkaterIndScraper(website, Agerange, draft.year, draft.pick, Round, Draft.Elig, Agerel,
#                              Position, Shoots, Stats, Place.birth, Pbsep, Country, Height, Weight, date.birth, dbsep, Drafted.team)
#     fullpage <- rbind(fullpage, playerpage)
#     picknumber <- picknumber + 1
#   }
#   fullpage
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# SkaterIndScraper <- function(website, Agerange = c(17, 25), draft.year = T, draft.pick = T, Round = T,  
#                              Draft.Elig = T, Agerel = "9/15", Position = T, Shoots = T, 
#                              stats = c("S", "Team", "League", "GP", "G", "A", "TP", "PIM", "+/-"),
#                              Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, dbsep = T, Drafted.team = T) {
#   if (website != "Paste Website Here") {
#     daterel <- as.Date(Agerel, "%m/%d")
#     Parse <- read_html(website)
#     Nodes <- html_nodes(Parse, "table")
#     seasondata <- html_table(Nodes, header = TRUE, fill = TRUE)[[2]]
#     seasondata <- seasondata[,1:9]
#     infonodesmatched <- html_nodes(Parse, "li")
#     infonodes <- html_text(infonodesmatched)
#     dobnum <- as.numeric(grep("\\bDate of Birth\\b", infonodes))
#     dobnode <- infonodes[dobnum]
#     dobsplit <- str_split(dobnode, "\n")[[1]]
#     includedob <- as.numeric(grep("\\b \\b", dobsplit))
#     dob <- trimws(dobsplit[includedob[2]], "b")
#     dobtest <- as.Date(dob, "%b%d,%Y")
#     dobtest1 <- is.na(dob)
#     if (dobtest1) {
#       dob <- "NA"
#     }
#     Scolstart <- 1
#     Scolend <- as.numeric(nrow(seasondata))
#     if (is.na(dobtest)) {
#       age <- "NA"
#       while (Scolstart <= Scolend) {
#         Scurrent <- seasondata[Scolstart,1]
#         if (!grepl("-", Scurrent)) {
#           seasondata[Scolstart, 1] <- seasondata[Scolstart - 1, 1]
#         }
#         Scolstart <- Scolstart + 1
#       }
#       seasondata <- seasondata[, which(colnames(seasondata) %in% stats)]
#       seasondata <- cbind("Age" = age, seasondata)
#     } else {
#       dob <- as.Date(dob, "%b%d,%Y")
#       forage <- str_split(dob, "-")
#       forage <- as.Date(paste("2018", forage[[1]][2], forage[[1]][3], sep = "-"), "%Y-%m-%d")
#       agedif <- abs(as.numeric(difftime(forage, daterel, units = "days")))
#       agefrac <- agedif / 365
#       from_lt = as.POSIXlt(dob)
#       to_lt = as.POSIXlt(daterel)
#       ageyear <- to_lt$year - from_lt$year
#       ageyear <- ifelse(to_lt$mon < from_lt$mon |
#                           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
#                         ageyear - 1, ageyear)
#       age <- as.numeric(ageyear) + agefrac
#       agelist <- integer(Scolend)
#       while (Scolstart <= Scolend) {
#         Scurrent <- seasondata[Scolstart,1]
#         if (!grepl("-", Scurrent)) {
#           seasondata[Scolstart, 1] <- seasondata[Scolstart - 1, 1]
#         }
#         Scurrent1 <- str_split(seasondata[Scolstart, 1], "-")
#         Scurrent2 <- as.numeric(Scurrent1[[1]][1])
#         agelist[Scolstart] <- age + Scurrent2 - as.numeric(str_split(daterel, "-")[[1]][1]) 
#         Scolstart <- Scolstart + 1
#       }
#       seasondata <- seasondata[, which(colnames(seasondata) %in% stats)]
#       seasondata <- cbind("Age" = agelist, seasondata)
#     }
#     agestart <- Agerange[1]
#     ageend <- Agerange[2]
#     seasondata <- filter(seasondata, Age >= agestart & Age <= ageend)
#     forname <- paste(readLines(website), collapse="\n")
#     forname <- str_split(forname, "\n")
#     forname1 <- as.numeric(grep("<h1 class=\"plytitle\">", forname[[1]]))
#     forname <- forname[[1]][forname1:(forname1 + 3)]
#     name <- grepl("<", forname)
#     name <- trimws(forname[!name])
#     seasondata <- cbind("Name" = name, seasondata)
#     if (Place.birth == T) {
#       pobnum <- as.numeric(grep("\\bPlace of Birth\\b", infonodes))
#       if (length(pobnum) == 0) {
#         pob <- "NA"
#         if (Pbsep == T) {
#           seasondata <- cbind("Country" = pob, seasondata)
#           seasondata <- cbind("State/Province" = pob, seasondata)
#           seasondata <- cbind("City" = pob, seasondata)
#         } else {
#           seasondata <- cbind("Place.of.Birth" = pob, seasondata)
#         }
#       } else {
#         pobnode <- infonodes[pobnum]
#         pobnode <- str_split(pobnode, "\n")[[1]]
#         includepob <- as.numeric(grep("\\b \\b", pobnode))
#         pob <- trimws(pobnode[includepob[2]], "b")
#         if (Pbsep == T) {
#           splitpob <- str_split(pob, ", ")[[1]]
#           city <- splitpob[1]
#           if (length(splitpob) == 3) {
#             state <- splitpob[2]
#             country <- splitpob[3]
#             seasondata <- cbind("Country" = country, seasondata)
#             seasondata <- cbind("State/Province" = state, seasondata)
#           } else {
#             country <- splitpob[2]
#             seasondata <- cbind("Country" = country, seasondata)
#             seasondata <- cbind("State/Province" = "NA", seasondata)
#           }
#           seasondata <- cbind("City" = city, seasondata)
#         } else {
#           seasondata <- cbind("Place.of.Birth" = pob, seasondata)
#         }
#       }
#     }
#     if (Position == T) {
#       posnum <- as.numeric(grep("\\bPosition\\b", infonodes))
#       posnode <- infonodes[posnum]
#       posnode <- str_split(posnode, "\n")[[1]]
#       posnode <- gsub(" ", "", posnode)
#       posnode <- posnode[lapply(posnode, nchar) > 0]
#       pos <- posnode[2]
#       Dcheck <- grepl("D", pos)
#       if (Dcheck == T) {
#         shootnum <- as.numeric(grep("\\bShoots\\b", infonodes))
#         shootsnode <- infonodes[shootnum]
#         shootsnode <- str_split(shootsnode, "\n")[[1]]
#         includeshoot <- as.numeric(grep("\\b \\b", shootsnode))
#         shoot <- trimws(shootsnode[includeshoot[2]], "b")
#         if (is.na(shoot)) {
#           shoot <- "NA"
#         } else {
#           dcor <- str_split(pos, "/")[[1]]
#           dnum <- as.numeric(grep("D", dcor))
#           newd <- paste(shoot, dcor[dnum], sep = "")
#           dcor[dnum] <- newd
#           pos <- paste(dcor, collapse = "/")
#         }
#       }
#       if (Shoots == F) {
#         seasondata <- cbind("Position" = pos, seasondata)
#       }
#     }
#     if (Shoots == T & Position == T) {
#       if (Dcheck) {
#         seasondata <- cbind("Shoots" = shoot, seasondata)
#         seasondata <- cbind("Position" = pos, seasondata)
#       } else {
#         shootnum <- as.numeric(grep("\\bShoots\\b", infonodes))
#         shootsnode <- infonodes[shootnum]
#         shootsnode <- str_split(shootsnode, "\n")[[1]]
#         includeshoot <- as.numeric(grep("\\b \\b", shootsnode))
#         shoot <- trimws(shootsnode[includeshoot[2]], "b")
#         if (is.na(shoot)) {
#           shoot <- "NA"
#         }
#         seasondata <- cbind("Shoots" = shoot, seasondata)
#         seasondata <- cbind("Position" = pos, seasondata)
#       }
#     }
#     if (Shoots == T & Position == F) {
#       shootnum <- as.numeric(grep("\\bShoots\\b", infonodes))
#       shootsnode <- infonodes[shootnum]
#       shootsnode <- str_split(shootsnode, "\n")[[1]]
#       includeshoot <- as.numeric(grep("\\b \\b", shootsnode))
#       shoot <- trimws(shootsnode[includeshoot[2]], "b")
#       if (is.na(shoot)) {
#         shoot <- "NA"
#       }
#       seasondata <- cbind("Shoots" = shoot, seasondata)
#     }
#     if (draft.pick == T | Round == T | draft.year == T | Draft.Elig == T | Drafted.team == T) {
#       draftnum <- as.numeric(grep("\\bDrafted\\b", infonodes))
#       if (length(draftnum) == 0) {
#         if (draft.pick == T) {
#           seasondata <- cbind("Draft.Pick" = "NA", seasondata)
#         }
#         if (Round == T) {
#           seasondata <- cbind("Draft.Round" = "NA", seasondata)
#         }
#         if (draft.year == T) {
#           seasondata <- cbind("Draft.Year" = "NA", seasondata)
#         }
#         if (Drafted.team == T) {
#           seasondata <- cbind("Drafted.Team" = "NA", seasondata)
#         }
#         if (Draft.Elig == T) {
#           seasondata <- cbind("Draft.Elig" = 5, seasondata)
#         }
#       } else {
#         draftnode <- infonodes[draftnum]
#         draftnode <- str_split(draftnode, "\n")[[1]]
#         includedraft <- as.numeric(grep("\\b \\b", draftnode))
#         draft <- trimws(draftnode[includedraft[2]], "b")
#         draftsep <- str_split(draft, " ")[[1]]
#         if (draft.pick == T) {
#           picknum <- as.numeric(gsub("#", "", draftsep[4]))
#           seasondata <- cbind("Draft.Pick" = picknum, seasondata)
#         }
#         if (Round == T) {
#           roundnum <- as.numeric(draftsep[3])
#           seasondata <- cbind("Round" = roundnum, seasondata)
#         }
#         if (draft.year == T) {
#           yearnum <- as.numeric(draftsep[1])
#           seasondata <- cbind("Draft.Year" = yearnum, seasondata)
#         }
#         if (Drafted.team == T) {
#           draftteam <- paste(draftsep[7:length(draftsep)], collapse = " ")
#           seasondata <- cbind("Drafted.Team" = draftteam, seasondata)
#         }
#         if (Draft.Elig == T) {
#           if (age == "NA") {
#             seasondata <- cbind("Draft.Elig" = "NA", seasondata)
#           } else {
#             yearnum <- as.numeric(draftsep[1])
#             yeardif <- floor((age - 18))
#             origyear <- (2018 - yeardif)
#             eligdif <- (yearnum - origyear)
#             if (eligdif == 0) {
#               seasondata <- cbind("Draft.Elig" = 1, seasondata)
#             } else if (eligdif == 1) {
#               seasondata <- cbind("Drat.Elig" = 2, seasondata)
#             } else if (eligdif == 2) {
#               seasondata <- cbind("Draft.Elig" = 3, seasondata)
#             } else {
#               seasondata <- cbind("Draft.Elig" = 4, seasondata)
#             }
#           }
#         }
#       }
#     }
#     if (Country == T) {
#       counnum <- as.numeric(grep("\\bNation\\b", infonodes))
#       counnode <- infonodes[counnum]
#       counnode <- str_split(counnode, "\n")[[1]]
#       includecoun <- as.numeric(grep("\\b \\b", counnode))
#       coun <- trimws(counnode[includecoun], "b")
#       seasondata <- cbind("Rep.Country" = coun, seasondata)
#     }
#     if (Weight == T) {
#       weightnum <- as.numeric(grep("\\bWeight\\b", infonodes))
#       weightnode <- infonodes[weightnum]
#       weightnode <- str_split(weightnode, "\n")[[1]]
#       includeweight <- as.numeric(grep("\\b \\b", weightnode))
#       weight <- weightnode[includeweight[2]]
#       if (is.na(weight)){
#         weight <- "NA"
#       } else {
#         weight <- trimws(weight, "b")
#         weightdif <- str_split(weight, " ")[[1]]
#         weight <- as.numeric(weightdif[1])
#       }
#       seasondata <- cbind("Weight" = weight, seasondata)
#     }
#     if (Height == T) {
#       heightnum <- as.numeric(grep("\\bHeight\\b", infonodes))
#       heightnode <- infonodes[heightnum]
#       heightnode <- str_split(heightnode, "\n")[[1]]
#       includeheight <-as.numeric(grep("\\b \\b", heightnode))
#       height <- heightnode[includeheight[2]]
#       if (is.na(height)) {
#         height <- "NA"
#       } else {
#         height <- trimws(height, "b")
#         heightdif <- str_split(height, " ")[[1]]
#         height <- as.numeric(heightdif[3])
#       }
#     }
#     if (date.birth == T) {
#       if (dobtest1) {
#         if (dbsep == T) {
#           seasondata <- cbind(Birth.Year = dob, seasondata)
#           seasondata <- cbind(Birth.Month = dob, seasondata)
#           seasondata <- cbind(Birth.Day = dob, seasondata)
#         } else {
#           seasondata <- cbind(Birth.Date = dob, seasondata)
#         }
#       } else if (is.na(dobtest)) {
#         if (dbsep == T) {
#           seasondata <- cbind(Birth.Year = dob, seasondata)
#           seasondata <- cbind(Birth.Month = "NA", seasondata)
#           seasondata <- cbind(Birth.Day = "NA", seasondata)
#         } else {
#           seasondata <- cbind(Birth.Date = dob, seasondata)
#         }
#       } else {
#         if (dbsep == T) {
#           DBseper <- str_split(dob, "-")[[1]]
#           seasondata <- cbind(Birth.Year = DBseper[1], seasondata)
#           seasondata <- cbind(Birth.Month = DBseper[2], seasondata)
#           seasondata <- cbind(Birth.Day = DBseper[3], seasondata)
#         } else {
#           seasondata <- cbind(Birth.Date = dob, seasondata)
#         }
#       }
#     }
#     seasondata <- cbind("Name" = name, seasondata)
#     seasondata
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# GoalieIndScraper <- function(website, Agerange = c(17, 25), draft.year = T, draft.pick = T, Round = T,  
#                              Draft.Elig = T, Agerel = "9/15", Catches = T, 
#                              stats = c("S", "Team", "League", "GP", "GAA", "SVS%"),
#                              Place.birth = T, Pbsep = T, Country = T, Height = T, Weight = T, date.birth = T, dbsep = T, Drafted.team = T) {
#   daterel <- as.Date(Agerel, "%m/%d")
#   Parse <- read_html(website)
#   Nodes <- html_nodes(Parse, "table")
#   seasondata <- html_table(Nodes, header = TRUE, fill = TRUE)[[2]]
#   seasondata <- seasondata[,1:6]
#   infonodesmatched <- html_nodes(Parse, "li")
#   infonodes <- html_text(infonodesmatched)
#   dobnum <- as.numeric(grep("\\bDate of Birth\\b", infonodes))
#   dobnode <- infonodes[dobnum]
#   dobsplit <- str_split(dobnode, "\n")[[1]]
#   includedob <- as.numeric(grep("\\b \\b", dobsplit))
#   dob <- trimws(dobsplit[includedob[2]], "b")
#   dobtest <- as.Date(dob, "%b%d,%Y")
#   dobtest1 <- is.na(dob)
#   if (dobtest1) {
#     dob <- "NA"
#   }
#   Scolstart <- 1
#   Scolend <- as.numeric(nrow(seasondata))
#   if (is.na(dobtest)) {
#     age <- "NA"
#     while (Scolstart <= Scolend) {
#       Scurrent <- seasondata[Scolstart,1]
#       if (!grepl("-", Scurrent)) {
#         seasondata[Scolstart, 1] <- seasondata[Scolstart - 1, 1]
#       }
#       Scolstart <- Scolstart + 1
#     }
#     seasondata <- seasondata[, which(colnames(seasondata) %in% stats)]
#     seasondata <- cbind("Age" = age, seasondata)
#   } else {
#     dob <- as.Date(dob, "%b%d,%Y")
#     forage <- str_split(dob, "-")
#     forage <- as.Date(paste("2018", forage[[1]][2], forage[[1]][3], sep = "-"), "%Y-%m-%d")
#     agedif <- abs(as.numeric(difftime(forage, daterel, units = "days")))
#     agefrac <- agedif / 365
#     from_lt = as.POSIXlt(dob)
#     to_lt = as.POSIXlt(daterel)
#     ageyear <- to_lt$year - from_lt$year
#     ageyear <- ifelse(to_lt$mon < from_lt$mon |
#                         (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
#                       ageyear - 1, ageyear)
#     age <- as.numeric(ageyear) + agefrac
#     agelist <- integer(Scolend)
#     while (Scolstart <= Scolend) {
#       Scurrent <- seasondata[Scolstart,1]
#       if (!grepl("-", Scurrent)) {
#         seasondata[Scolstart, 1] <- seasondata[Scolstart - 1, 1]
#       }
#       Scurrent1 <- str_split(seasondata[Scolstart, 1], "-")
#       Scurrent2 <- as.numeric(Scurrent1[[1]][1])
#       agelist[Scolstart] <- age + Scurrent2 - as.numeric(str_split(daterel, "-")[[1]][1]) 
#       Scolstart <- Scolstart + 1
#     }
#     seasondata <- seasondata[, which(colnames(seasondata) %in% stats)]
#     seasondata <- cbind("Age" = agelist, seasondata)
#   }
#   agestart <- Agerange[1]
#   ageend <- Agerange[2]
#   seasondata <- filter(seasondata, Age >= agestart & Age <= ageend)
#   forname <- paste(readLines(website), collapse="\n")
#   forname <- str_split(forname, "\n")
#   forname1 <- as.numeric(grep("<h1 class=\"plytitle\">", forname[[1]]))
#   forname <- forname[[1]][forname1:(forname1 + 3)]
#   name <- grepl("<", forname)
#   name <- trimws(forname[!name])
#   seasondata <- cbind("Name" = name, seasondata)
#   if (Place.birth == T) {
#     pobnum <- as.numeric(grep("\\bPlace of Birth\\b", infonodes))
#     if (length(pobnum) == 0) {
#       pob <- "NA"
#       if (Pbsep == T) {
#         seasondata <- cbind("Country" = pob, seasondata)
#         seasondata <- cbind("State/Province" = pob, seasondata)
#         seasondata <- cbind("City" = pob, seasondata)
#       } else {
#         seasondata <- cbind("Place.of.Birth" = pob, seasondata)
#       }
#     } else {
#       pobnode <- infonodes[pobnum]
#       pobnode <- str_split(pobnode, "\n")[[1]]
#       includepob <- as.numeric(grep("\\b \\b", pobnode))
#       pob <- trimws(pobnode[includepob[2]], "b")
#       if (Pbsep == T) {
#         splitpob <- str_split(pob, ", ")[[1]]
#         city <- splitpob[1]
#         if (length(splitpob) == 3) {
#           state <- splitpob[2]
#           country <- splitpob[3]
#           seasondata <- cbind("Country" = country, seasondata)
#           seasondata <- cbind("State/Province" = state, seasondata)
#         } else {
#           country <- splitpob[2]
#           seasondata <- cbind("Country" = country, seasondata)
#           seasondata <- cbind("State/Province" = "NA", seasondata)
#         }
#         seasondata <- cbind("City" = city, seasondata)
#       } else {
#         seasondata <- cbind("Place.of.Birth" = pob, seasondata)
#       }
#     }
#   }
#   if (Catches == T) {
#     shootnum <- as.numeric(grep("\\bCatches\\b", infonodes))
#     shootsnode <- infonodes[shootnum]
#     shootsnode <- str_split(shootsnode, "\n")[[1]]
#     includeshoot <- as.numeric(grep("\\b \\b", shootsnode))
#     shoot <- trimws(shootsnode[includeshoot[2]], "b")
#     if (is.na(shoot)) {
#       shoot <- "NA"
#     }
#     seasondata <- cbind("Catches" = shoot, seasondata)
#   }
#   if (draft.pick == T | Round == T | draft.year == T | Draft.Elig == T | Drafted.team == T) {
#     draftnum <- as.numeric(grep("\\bDrafted\\b", infonodes))
#     if (length(draftnum) == 0) {
#       if (draft.pick == T) {
#         seasondata <- cbind("Draft.Pick" = "NA", seasondata)
#       }
#       if (Round == T) {
#         seasondata <- cbind("Draft.Round" = "NA", seasondata)
#       }
#       if (draft.year == T) {
#         seasondata <- cbind("Draft.Year" = "NA", seasondata)
#       }
#       if (Drafted.team == T) {
#         seasondata <- cbind("Drafted.Team" = "NA", seasondata)
#       }
#       if (Draft.Elig == T) {
#         seasondata <- cbind("Draft.Elig" = 5, seasondata)
#       }
#     } else {
#       draftnode <- infonodes[draftnum]
#       draftnode <- str_split(draftnode, "\n")[[1]]
#       includedraft <- as.numeric(grep("\\b \\b", draftnode))
#       draft <- trimws(draftnode[includedraft[2]], "b")
#       draftsep <- str_split(draft, " ")[[1]]
#       if (draft.pick == T) {
#         picknum <- as.numeric(gsub("#", "", draftsep[4]))
#         seasondata <- cbind("Draft.Pick" = picknum, seasondata)
#       }
#       if (Round == T) {
#         roundnum <- as.numeric(draftsep[3])
#         seasondata <- cbind("Round" = roundnum, seasondata)
#       }
#       if (draft.year == T) {
#         yearnum <- as.numeric(draftsep[1])
#         seasondata <- cbind("Draft.Year" = yearnum, seasondata)
#       }
#       if (Drafted.team == T) {
#         draftteam <- paste(draftsep[7:length(draftsep)], collapse = " ")
#         seasondata <- cbind("Drafted.Team" = draftteam, seasondata)
#       }
#       if (Draft.Elig == T) {
#         if (age == "NA") {
#           seasondata <- cbind("Draft.Elig" = "NA", seasondata)
#         } else {
#           yearnum <- as.numeric(draftsep[1])
#           yeardif <- floor((age - 18))
#           origyear <- (2018 - yeardif)
#           eligdif <- (yearnum - origyear)
#           if (eligdif == 0) {
#             seasondata <- cbind("Draft.Elig" = 1, seasondata)
#           } else if (eligdif == 1) {
#             seasondata <- cbind("Drat.Elig" = 2, seasondata)
#           } else if (eligdif == 2) {
#             seasondata <- cbind("Draft.Elig" = 3, seasondata)
#           } else {
#             seasondata <- cbind("Draft.Elig" = 4, seasondata)
#           }
#         }
#       }
#     }
#   }
#   if (Country == T) {
#     counnum <- as.numeric(grep("\\bNation\\b", infonodes))
#     counnode <- infonodes[counnum]
#     counnode <- str_split(counnode, "\n")[[1]]
#     includecoun <- as.numeric(grep("\\b \\b", counnode))
#     coun <- trimws(counnode[includecoun], "b")
#     seasondata <- cbind("Rep.Country" = coun, seasondata)
#   }
#   if (Weight == T) {
#     weightnum <- as.numeric(grep("\\bWeight\\b", infonodes))
#     weightnode <- infonodes[weightnum]
#     weightnode <- str_split(weightnode, "\n")[[1]]
#     includeweight <- as.numeric(grep("\\b \\b", weightnode))
#     weight <- weightnode[includeweight[2]]
#     if (is.na(weight)){
#       weight <- "NA"
#     } else {
#       weight <- trimws(weight, "b")
#       weightdif <- str_split(weight, " ")[[1]]
#       weight <- as.numeric(weightdif[1])
#     }
#     seasondata <- cbind("Weight" = weight, seasondata)
#   }
#   if (Height == T) {
#     heightnum <- as.numeric(grep("\\bHeight\\b", infonodes))
#     heightnode <- infonodes[heightnum]
#     heightnode <- str_split(heightnode, "\n")[[1]]
#     includeheight <-as.numeric(grep("\\b \\b", heightnode))
#     height <- heightnode[includeheight[2]]
#     if (is.na(height)) {
#       height <- "NA"
#     } else {
#       height <- trimws(height, "b")
#       heightdif <- str_split(height, " ")[[1]]
#       height <- as.numeric(heightdif[3])
#     }
#     seasondata <- cbind("Height" = height, seasondata)
#   }
#   if (date.birth == T) {
#     if (dobtest1) {
#       if (dbsep == T) {
#         seasondata <- cbind(Birth.Year = dob, seasondata)
#         seasondata <- cbind(Birth.Month = dob, seasondata)
#         seasondata <- cbind(Birth.Day = dob, seasondata)
#       } else {
#         seasondata <- cbind(Birth.Date = dob, seasondata)
#       }
#     } else if (is.na(dobtest)) {
#       if (dbsep == T) {
#         seasondata <- cbind(Birth.Year = dob, seasondata)
#         seasondata <- cbind(Birth.Month = "NA", seasondata)
#         seasondata <- cbind(Birth.Day = "NA", seasondata)
#       } else {
#         seasondata <- cbind(Birth.Date = dob, seasondata)
#       }
#     } else {
#       if (dbsep == T) {
#         DBseper <- str_split(dob, "-")[[1]]
#         seasondata <- cbind(Birth.Year = DBseper[1], seasondata)
#         seasondata <- cbind(Birth.Month = DBseper[2], seasondata)
#         seasondata <- cbind(Birth.Day = DBseper[3], seasondata)
#       } else {
#         seasondata <- cbind(Birth.Date = dob, seasondata)
#       }
#     }
#   }
#   seasondata <- cbind("Name" = name, seasondata)
#   seasondata
# }
