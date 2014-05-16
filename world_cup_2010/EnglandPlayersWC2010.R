library(RColorBrewer)
setwd("~/projects/wal/r-learning/projects/world_cup_2010/")

wc_2010_players <- read.csv("../../datasets/guardian/world-cup-2010-players.csv", sep=',', header=TRUE,nrows=595)

wc_2010_players$Name <- paste(wc_2010_players$Player.Surname,wc_2010_players$Team,wc_2010_players$Position)
rownames(wc_2010_players) <- wc_2010_players$Name

# Extract England Players
wc_2010_players_england <- wc_2010_players[which(wc_2010_players$Team == 'England'),]

# Remove unneeded columns
wc_2010_players_england$Position <- NULL
wc_2010_players_england$Player.Surname <- NULL
wc_2010_players_england$Team <- NULL
wc_2010_players_england$Name <- NULL

#Order by Passes
wc_2010_players_england_ordered <- wc_2010_players_england[order(wc_2010_players_england$Total.Passes, decreasing=TRUE),]

# Create Matrix
wc_2010_players_england_matrix <- data.matrix(wc_2010_players_england_ordered)

# Set matrix column names
colnames(wc_2010_players_england_matrix)[1] <- 'Played'
colnames(wc_2010_players_england_matrix)[2] <- 'Shots'
colnames(wc_2010_players_england_matrix)[3] <- 'Passes'
colnames(wc_2010_players_england_matrix)[4] <- 'Tackles'
colnames(wc_2010_players_england_matrix)[5] <- 'Saves'

# Render the heat map
par(mar=c(5,8,4,2))
par(las=2)

wc_2010_players_england_heatmap <- heatmap(wc_2010_players_england_matrix, 
                                           Rowv=NA, 
                                           Colv=NA,
                                           margins=c(5,5),
                                           col = brewer.pal(20,'Greens'), 
                                           scale='column',                                            
                                           main="World Cup 2010 - England Players")
dev.print(file="EnglandPlayersWC2010.jpeg", device=jpeg, width=600) 
