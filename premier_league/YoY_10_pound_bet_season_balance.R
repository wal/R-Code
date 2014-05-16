setwd("~/projects/wal/r-learning/projects/premier_league/")
results2012 <- read.csv("../../datasets/premier-league/2012-2013.csv")
results2013 <- read.csv("../../datasets/premier-league/2013-2014.csv")

team = "Liverpool"

balance_analysis <- function(results) {
  team_results <- results[which(results$HomeTeam == team | results$AwayTeam == team),c('Date', 'HomeTeam', 'AwayTeam', 'HTR', 'B365H', 'B365D', 'B365A')]
  
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
  
  team_results
}

team_results_2012 = balance_analysis(results2012)
team_results_2013 = balance_analysis(results2013)

# Align dates
team_results_2013_adjusted = team_results_2013
team_results_2013_adjusted$Date = team_results_2013_adjusted$Date - 365

plot(data=team_results_2012,Balance ~ Date, type="l", main=paste(team, " - Weekly £10 win bet - Balance"), col="red", panel.first = grid())
lines(data=team_results_2013_adjusted, Balance ~ Date, col="blue")
legend('topleft', legend = c("2012 - Balance", "2013 - Balance"), lty="solid", col=c("red", "blue")) 


image_file_name = gsub("\\s","_",paste("YOY_",team,"weekly_10_pound_bet_season_balance.jpeg", sep=""))
csv_file_name = gsub("\\s","_",paste("YOY_",team,"weekly_10_pound_bet_season_balance.csv", sep=""))
write.csv(rbind(team_results_2012, team_results_2013), file=csv_file_name)
dev.print(file=image_file_name, device=jpeg, width=2000, height=1200)

