#Player with NHL games
test1 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html")
test2 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html")
test3 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html")


#Player with NHL games playing for different teams in a season
test4 <- RefPlayerScraper("https://www.hockey-reference.com/players/h/hagelca01.html")
test5 <- RefPlayerScraper("https://www.hockey-reference.com/players/b/boylebr01.html")

#Playoff and Regular Season
test6 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Season = "RP")
test7 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Season = "RP")
test8 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Season = "RP")

#Testing stat inclusions
test9 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dahlira01.html", Stats = c("stateSep", "Cor", "Awards", "PS"))
test10 <- RefPlayerScraper("https://www.hockey-reference.com/players/s/smithcr01.html", Stats = c("Fen", "PDO", "xGF"))
test11 <- RefPlayerScraper("https://www.hockey-reference.com/players/p/petitmi01.html", Stats = c("oiS", "IceTime", "pmBreak"))

#Testing age difference
test12 <- RefPlayerScraper("https://www.hockey-reference.com/players/c/crosbsi01.html", ages = c(20))
test13 <- RefPlayerScraper("https://www.hockey-reference.com/players/o/ovechal01.html", ages = c(22, 24))

#Player without NHL games
test14 <- RefPlayerScraper("https://www.hockey-reference.com/players/d/dicaige01.html")
test15 <- RefPlayerScraper("https://www.hockey-reference.com/players/e/elfstma01.html")