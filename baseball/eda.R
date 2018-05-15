library(tidyverse)
library(openintro)

data <- mlbBat10

# Code Book
#
# name Player name
# team Team abbreviation
# position Player position
# G Number of games
# AB Number of at bats
# R Number of runs
# H Number of hits
# 2B Number of doubles
# mlbBat10 103
# 3B Number of triples
# HR Number of home runs
# RBI Number of runs batted in
# TB Total bases, computed as 3*HR + 2*3B + 1*2B + H
# BB Number of walks
# SO Number of strikeouts
# SB Number of stolen bases
# CS Number of times caught stealing
# OBP On base percentage
# SLG Slugging percentage (TB / AB)
# AVG Batting average

glimpse(data)
table(data$position)
data_ab200 <- data %>% filter(AB > 200)

glimpse(data_ab200)
table(data_ab200$position)
data_ab200$position <- droplevels(data_ab200$position)
table(data_ab200$position)

data_ab200$position_summary <- plyr::revalue(data_ab200$position, replace = c("1B" = "IF", "2B" = "IF", "3B" = "IF", "SS" = "IF"))
table(data_ab200$position_summary)

t524 <- data_ab200 %>% 
  group_by(position_summary) %>% 
  summarise(n = n(),
            MeanOBP = round(mean(OBP), 4),
            SDOBP = round(sd(OBP), 4))

# Is OBP related to position ?

ggplot(data_ab200, aes(position_summary, OBP, fill = position_summary)) + 
  geom_boxplot() +
  theme_bw() +
  labs(y = "On base percentage", x = "Position") +
  guides(fill = FALSE)

ggplot(data_ab200, aes(sample = OBP, color = position_summary )) + 
  stat_qq() +
  theme_bw() +
  facet_wrap(~position_summary)


model.aov = aov(OBP ~ position_summary, data = data_ab200)
summary(model.aov)


# F-Distribution curves
ggplot(data = data.frame(x = seq(0, 6, length= 200)), aes(x = x)) +
  stat_function(fun = df, args = list(3, 323), geom = "area", fill = "purple", alpha = 0.5) +
  theme_bw() + 
  labs(x = "", y = "")

df_limit <- function(x){
  y <- df(x, 3, 323)
  y[x < 1.994] <- NA
  return(y)
}
p <- ggplot(data.frame(x = c(0, 6)), aes(x = x))
p + stat_function(fun = df_limit, geom = "area", fill = "purple", alpha = 0.4, n = 500) + 
  stat_function(fun = df, args = list(3, 323)) +
  theme_bw() +
  labs(x = "", y = "")




T5N <- data_ab200 %>%
  group_by(position) %>%
  summarise(n = n(), Mean = round(mean(OBP), 4), SD = round(sd(OBP), 4))
T5N


ggplot(data = data_ab200, aes(x = position, y = OBP, fill = position)) + 
  geom_boxplot() + 
  theme_bw() + 
  guides(fill = FALSE)