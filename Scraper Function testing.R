#####
# A #
#####
#Player with NHL games
testA1 <- tableEquals(read.csv("Test Expected Results/testA1.csv"), changeToNumeric(RefPlayerScraper("https://www.hockey-reference.com/players/n/niedesc01.html")))
testA2 <- tableEquals(read.csv("Test Expected Results/testA2.csv"), changeToNumeric(RefPlayerScraper("https://www.hockey-reference.com/players/p/perregi01.html")))
testA3 <- tableEquals(read.csv("Test Expected Results/testA3.csv"), changeToNumeric(RefPlayerScraper("https://www.hockey-reference.com/players/d/doansh01.html")))
A <- all(testA1, testA2, testA3)

#####
# B #
#####
#Player with NHL games playing for different teams in a season
testB1 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", sepTeam = F)
testB2 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/sillimi01.html", sepTeam = F)
testB3 <- RefPlayerScraper("https://www.hockey-reference.com/players/h/hagelca01.html", sepTeam = T)
testB4 <- RefPlayerScraper("https://www.hockey-reference.com/players/b/boylebr01.html", sepTeam = T)
B <- all(testB1, testB2, testB3, testB4)


#####
# C #
#####
#Playoff and Regular Season
testC1 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Season = "RP")
testC2 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Season = "RP")
testC3 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Season = "RP")


#####
# D #
#####
#Testing stat inclusions
testD1 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Stats = c("stateSep", "Cor", "Awards", "PS"))
testD2 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Stats = c("Fen", "PDO", "xGF"))
testD3 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Stats = c("oiS", "IceTime", "pmBreak"))


#####
# E #
#####
#Testing age difference
testE1 <- RefPlayerScraper("https://www.hockey-reference.com/players/c/crosbsi01.html", ages = c(20))
testE2 <- RefPlayerScraper("https://www.hockey-reference.com/players/o/ovechal01.html", ages = c(22, 24))


#####
# F #
#####
#Player without NHL games
testF1 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dicaige01.html")
testF2 <- RefPlayerScraper("https://www.hockey-reference.com/players/e/elfstma01.html")