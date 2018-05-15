library(data.table)
library(tidyverse)

salary_data <- read_csv("data/Salaries.csv")

# Summarise data
dim(salary_data)
str(salary_data)
glimpse(salary_data)
summary(salary_data)


# Convert into a data.table
salary_data.DT <- as.data.table(salary_data)
class(salary_data.DT)

# indexing by range
salary_data.DT[1:10,]
salary_data.DT[1:3, 1:2]

# query with condition
dim(salary_data.DT)
dim(salary_data.DT[yearID > 2000])
dim(salary_data.DT[yearID > 2000 & lgID == "AL"])
dim(salary_data.DT[yearID > 2000 | yearID < 1999]) # Multiple conditions |&

# Sorting
head(salary_data.DT[order(salary)])
head(salary_data.DT[order(-salary)]) # descending
head(salary_data.DT[order(yearID, salary)]) # multiple


# Aggregation
mean(salary_data.DT$salary)
max(salary_data.DT$salary)
median(salary_data.DT$salary)

# Grouping
mean_salary_by_year <- salary_data.DT[, mean(salary), by=yearID]
mean_salary_by_year <- salary_data.DT[, 
                                list(avg_salary = mean(salary)), 
                                by=yearID] # Specify name for new column


salary_summary_by_year <- salary_data.DT[, 
                                   list(
                                     avg = mean(salary),
                                     max = max(salary),
                                     sd = sd(salary),
                                     iqr = IQR(salary)
                                   ), 
                                   by=yearID] # Specify multiple aggregating columns


melt(salary_summary_by_year, id.vars="yearID") %>%
  ggplot(aes(yearID, value, color = variable)) + geom_line()

# Summarise by multiple columns (year and team)
salary_summary_by_year_team <- salary_data.DT[, 
                                   list(
                                     avg = mean(salary),
                                     max = max(salary),
                                     sd = sd(salary),
                                     iqr = IQR(salary)
                                   ), 
                                   by=list(yearID, teamID)] # Specify multiple aggregating columns

melt(salary_summary_by_year_team, id.vars=c("yearID", "teamID")) %>%
  ggplot(aes(yearID, value, color = variable)) + 
    geom_line() +
    facet_wrap(~ teamID)


# Joining/Merging data

# Player details
player_data <- read_csv("http://dgrtwo.github.io/pages/lahman/Master.csv")
str(player_data)
dim(player_data)
glimpse(player_data)
summary(player_data)

player_data.DT <- as.data.table(player_data)
david_aardsma_player_data <- player_data.DT[playerID == "aardsda01",]


salary_player_merged <- merge(salary_data.DT, player_data.DT, by="playerID")
dim(salary_data.DT)
dim(player_data.DT)
dim(salary_player_merged)

# Create new columns
salary_player_merged[,name:= paste(nameFirst, nameLast)]
head(salary_player_merged$name)

batting_stats <- read_csv("http://dgrtwo.github.io/pages/lahman/Batting.csv")
dim(batting_stats)
glimpse(batting_stats)
batting_stats.DT <- as.data.table(batting_stats)

batting_salary.merged <- merge(batting_stats.DT, salary_data.DT, by=c("playerID", "yearID", "lgID", "teamID"))

dim(salary_data.DT)
dim(batting_stats.DT)
dim(batting_salary.merged)
sum(is.na(batting_salary.merged$salary))

# all.x = TRUE
batting_salary.merged.all_x <- merge(batting_stats.DT, 
                                            salary_data.DT,
                                            by=c("playerID", "yearID", "lgID", "teamID"), all.x = TRUE)
dim(batting_salary.merged.all_x)
sum(is.na(batting_salary.merged.all_x$salary))

# merge all 3 data sets

merged_data <- merge(batting_stats.DT, player_data.DT, by="playerID")
dim(batting_stats.DT)
dim(player_data.DT)
dim(merged_data)
head(merged_data)

# Question - Who are the top 10 career home run hitters ?
merged_data[, list(Total_HR = sum(HR)), by=c("playerID", "nameLast")][order(-Total_HR)][1:10]

#      playerID  nameLast Total_HR
#     _________ _________ ________
#  1: bondsba01     Bonds      762
#  2: aaronha01     Aaron      755
#  3:  ruthba01      Ruth      714
#  4:  mayswi01      Mays      660
#  5: rodrial01 Rodriguez      654
#  6: griffke02   Griffey      630
#  7: thomeji01     Thome      612
#  8:  sosasa01      Sosa      609
#  9: robinfr02  Robinson      586
# 10: mcgwima01   McGwire      583

# Question - What is the relationship between Hits and At Bats ?
batter_summary <- merged_data[,list(Hits = sum(H), 
                                    Runs = sum(R)),
                              by=c("playerID", "nameLast")]


