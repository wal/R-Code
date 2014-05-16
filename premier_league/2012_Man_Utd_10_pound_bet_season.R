setwd("~/projects/wal/r-learning/projects/premier_league/")
results2012 <- read.csv("../../datasets/premier-league/2012-2013.csv")


team = "Man United"

team_results <- results2012[which(results2012$HomeTeam == team | results2012$AwayTeam == team),c('Date', 'HomeTeam', 'AwayTeam', 'HTR', 'B365H', 'B365D', 'B365A')]
team_results$Total <- 0

total = 0
stake = 10  

for (i in 1:nrow(team_results)) {
  row <- team_results[i,]
  gain = 0
  
  if(row$HomeTeam == team && row$HTR == 'H') {
      gain = (stake * row$B365H)   
  } else if(row$AwayTeam == team && row$HTR == 'A') {
    gain = (stake * row$B365A)
  }
    
  total = total + gain
  team_results[i,'Stake'] = stake * i
  team_results[i,'Winnings'] = total
}

team_results$Date <- as.Date(team_results$Date, format="%d/%m/%y")

par(mar=c(5,8,4,2))
par(las=2)
plot(data=team_results,Stake ~ Date,type="l", main=paste(team, " - Weekly £10 win bet"))
lines(data=team_results, Winnings ~ Date, lty = 'dotted')
legend('topleft', legend = c("Stake", "Winnings"), lty = c('solid', 'dotted')) 

dev.print(file=paste("2012_",team,"weekly_10_pound_bet_season.jpeg"), device=jpeg, width=2000, height=1200)
