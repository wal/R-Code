setwd("~/projects/wal/r-learning//projects/premier_league/")

results2012 <- read.csv("~/projects/wal/r-learning/datasets/premier-league/2012-2013.csv")

# Red cards received by home team
red_cards_received_at_home <- aggregate(results2012$HR, by=list(results2012$HomeTeam), FUN=sum)
# Red cards received by away team 
red_cards_received_away <- aggregate(results2012$AR, by=list(results2012$AwayTeam), FUN=sum)

names(red_cards_received_at_home)[1] <- 'Team'
names(red_cards_received_at_home)[2] <- 'ReceivedHome'

names(red_cards_received_away)[1] <- 'Team'
names(red_cards_received_away)[2] <- 'ReceivedAway'

# Red cards received opposition by home team
red_cards_opposition_at_home <- aggregate(results2012$AR, by=list(results2012$HomeTeam), FUN=sum)
# Red cards received opposition by away team
red_cards_opposition_away <- aggregate(results2012$HR, by=list(results2012$AwayTeam), FUN=sum)

names(red_cards_opposition_at_home)[1] <- 'Team'
names(red_cards_opposition_at_home)[2] <- 'OppositionHome'

names(red_cards_opposition_away)[1] <- 'Team'
names(red_cards_opposition_away)[2] <- 'OppositionAway'

# Merge data frames into single master data frame
red_cards_merged <- merge(red_cards_received_at_home,red_cards_received_away)
red_cards_merged <- merge(red_cards_merged, red_cards_opposition_at_home)
red_cards_merged <- merge(red_cards_merged, red_cards_opposition_away)

# Add totals columns
red_cards_merged$TotalRedCardsReceived <- red_cards_merged$ReceivedHome + red_cards_merged$ReceivedAway
red_cards_merged$TotalRedCardsOpposition <- red_cards_merged$OppositionHome + red_cards_merged$OppositionAway

rownames(red_cards_merged) <- red_cards_merged$Team

# Remove unnecessary columns
red_cards_merged$ReceivedHome <- NULL
red_cards_merged$ReceivedAway <- NULL
red_cards_merged$OppositionHome <- NULL
red_cards_merged$OppositionAway <- NULL
red_cards_merged$Team <- NULL

# Update column names
colnames(red_cards_merged)[1] <- 'Received'
colnames(red_cards_merged)[2] <- 'Opposition'

# Order by red cards received by opposition
red_cards_merged <- red_cards_merged[order(red_cards_merged$Opposition),]


# Generate barplot
par(mar=c(5,8,4,2))
par(las=2)
barplot(t(red_cards_merged),
        beside=TRUE,
        horiz=TRUE,
        cex.names=0.8,
        legend=colnames(red_cards_merged),
        col = c("firebrick2","dodgerblue3"),
        main = "EPL 2012 - Red Cards Received V's Opposition Received")

dev.print(file="2012_Red_Cards_received_v_opposition.jpeg", device=jpeg, width=2000, height=1200)
