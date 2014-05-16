setwd("~/projects/wal/r-learning/projects/premier_league/")
results2012 <- read.csv("../../datasets/premier-league/2012-2013.csv")

# Goals
aggregated_by_FTHG <- aggregate(results2012$FTHG, by=list(results2012$HomeTeam), FUN=sum)
aggregated_by_FTAG <- aggregate(results2012$FTAG, by=list(results2012$AwayTeam), FUN=sum)
names(aggregated_by_FTAG) <- c('Team', 'FTAG')
names(aggregated_by_FTHG) <- c('Team', 'FTHG')
goals <- merge(aggregated_by_FTHG, aggregated_by_FTAG)


# Shots 
aggregated_by_AS <- aggregate(results2012$AS, by=list(results2012$AwayTeam), FUN=sum)
aggregated_by_HS <- aggregate(results2012$HS, by=list(results2012$HomeTeam), FUN=sum)
names(aggregated_by_AS) <- c('Team', 'AS')
names(aggregated_by_HS) <- c('Team', 'HS')
shots <- merge(aggregated_by_HS, aggregated_by_AS)


# Shots on Target
aggregated_by_AST <- aggregate(results2012$AST, by=list(results2012$AwayTeam), FUN=sum)
aggregated_by_HST <- aggregate(results2012$HST, by=list(results2012$HomeTeam), FUN=sum)
names(aggregated_by_AST) <- c('Team', 'AST')
names(aggregated_by_HST) <- c('Team', 'HST')
shotsOnTarget<- merge(aggregated_by_AST, aggregated_by_HST)

# Corners
aggregated_by_AC <- aggregate(results2012$AC, by=list(results2012$AwayTeam), FUN=sum)
aggregated_by_HC <- aggregate(results2012$HC, by=list(results2012$HomeTeam), FUN=sum)
names(aggregated_by_HC) <- c('Team', 'HC')
names(aggregated_by_AC) <- c('Team' 'AC')
corners <- merge(aggregated_by_AC, aggregated_by_HC)

# Fouls
aggregated_by_AF <- aggregate(results2012$AF, by=list(results2012$AwayTeam), FUN=sum)
aggregated_by_HF <- aggregate(results2012$HF, by=list(results2012$HomeTeam), FUN=sum)
names(aggregated_by_HC) <- c('Team', 'HF')
names(aggregated_by_AC) <- c('Team' 'AF')
fouls <- merge(aggregated_by_HF, aggregated_by_AF)

# Yellow Cards
aggregated_by_AY <- aggregate(results2012$AY, by=list(results2012$AwayTeam), FUN=sum)
aggregated_by_HY <- aggregate(results2012$HY, by=list(results2012$HomeTeam), FUN=sum)
names(aggregated_by_HC) <- c('Team', 'HY')
names(aggregated_by_AC) <- c('Team' 'AY')
yellowCards <- merge(aggregated_by_AY, aggregated_by_HY)




# Merge aggregatd data frames together
merged <- 
  merge(
    merge(
      merge(
        merge(
          merge(aggregated_by_FTAG, aggregated_by_FTHG),
          aggregated_by_AST),
        aggregated_by_HST),
      aggregated_by_HC),
    aggregated_by_AC)

# Calculate totals columns
merged$TotalGoals <- merged$FTHG + merged$FTAG
merged$TotalShotsOnTarget <- merged$AST + merged$HST
merged$TotalCorners <- merged$HC + merged$AC

# Extract just the columns we need
goals_sot <- merged[,c('Team','TotalCorners','TotalShotsOnTarget','TotalGoals')]
rownames(goals_sot) <- goals_sot$Team
goals_sot$Team <- NULL

#Order by Total Corners
goals_sot <- goals_sot[order(goals_sot$TotalCorners),]




# Create matrix
goals_sot_matrix <- data.matrix(goals_sot)

# Generate heatmap
par(mar=c(5,8,4,2))
par(las=2)





library(RColorBrewer)
goals_sot_heatmap <- heatmap(goals_sot_matrix,
                             Rowv=NA, 
                             Colv=NA,
                             col = brewer.pal(9, 'Greens'),
                             scale='column',
                             main="Corners V's Shots on Target V's Goals - 2012")


