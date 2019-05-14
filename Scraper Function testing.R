#####
# A #
#####
#Player with NHL games
test1 <- all(read.csv("Test Expected Results/test1.csv") == changeToNumeric(RefPlayerScraper("https://www.hockey-reference.com/players/n/niedesc01.html")))
test2 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/perregi01.html")
test3 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/doansh01.html")
A <- all(test1, test2, test3)

#####
# B #
#####
#Player with NHL games playing for different teams in a season
test4 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html")
test5 <- RefPlayerScraper("https://www.hockey-reference.com/players/h/hagelca01.html")
test6 <- RefPlayerScraper("https://www.hockey-reference.com/players/b/boylebr01.html")


#####
# C #
#####
#Playoff and Regular Season
test7 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Season = "RP")
test8 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Season = "RP")
test9 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Season = "RP")


#####
# D #
#####
#Testing stat inclusions
test10 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Stats = c("stateSep", "Cor", "Awards", "PS"))
test11 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Stats = c("Fen", "PDO", "xGF"))
test12 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Stats = c("oiS", "IceTime", "pmBreak"))


#####
# E #
#####
#Testing age difference
test13 <- RefPlayerScraper("https://www.hockey-reference.com/players/c/crosbsi01.html", ages = c(20))
test14 <- RefPlayerScraper("https://www.hockey-reference.com/players/o/ovechal01.html", ages = c(22, 24))


#####
# F #
#####
#Player without NHL games
test15 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dicaige01.html")
test16 <- RefPlayerScraper("https://www.hockey-reference.com/players/e/elfstma01.html")