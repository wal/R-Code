setwd("~/projects/wal/r-learning/projects/premier_league/")
results2012 <- read.csv("../../datasets/premier-league/2012-2013.csv")


team = "Man United"

team_results <- results2012[which(results2012$HomeTeam == team | results2012$AwayTeam == team),c('Date', 'HomeTeam', 'AwayTeam', 'HTR', 'B365H', 'B365D', 'B365A')]

stake = 10
balance = 0
staked = 0  

for (i in 1:nrow(team_results)) {
  row <- team_results[i,]
  
  if (balance < stake) {
    staked = (staked + stake)
  }
  
  gain = 0
  
  if(row$HomeTeam == team && row$HTR == 'H') {
      gain = (stake * row$B365H) + stake   
  } else if(row$AwayTeam == team && row$HTR == 'A') {
    gain = (stake * row$B365A) + stake
  } 

  if(gain == 0 && balance > stake) {
    balance = balance - stake  
  } else {
    balance = balance + gain
  }
  
  team_results[i,'Staked'] = staked
  team_results[i,'Gain'] = gain
  team_results[i,'Balance'] = balance
}

team_results$Date <- as.Date(team_results$Date, format="%d/%m/%y")

par(mar=c(5,8,4,2))
par(las=2)
plot(data=team_results,Balance ~ Date,type="l", main=paste(team, " - Weekly £10 win bet - Balance"))
lines(data=team_results, Staked ~ Date, lty = 'dotted')
legend('topleft', legend = c("Balance", "Staked"), lty = c('solid', 'dotted')) 

image_file_name = gsub("\\s","_",paste("2012_",team,"weekly_10_pound_bet_season_balance.jpeg", sep=""))
csv_file_name = gsub("\\s","_",paste("2012_",team,"weekly_10_pound_bet_season_balance.csv", sep=""))
write.csv(team_results, csv_file_name)
dev.print(file=image_file_name, device=jpeg, width=2000, height=1200)

