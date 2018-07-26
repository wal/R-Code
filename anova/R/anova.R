library(tidyverse)

data <- read_csv('anova/data/raw/CyclerCPK.csv')
glimpse(data)

# Question - How does the treatment vary across the time points ?


# Variables table
library(plyr)
library(tidyverse)
create_variables_table <- function(data) {
  types <- data %>% summarise_all(function(x) class(x)[[1]]) %>% gather(Name, class)
  unique_values <- data %>% summarise_all(function(x) length(unique(x))) %>% gather(Name, `Unique Values`)
  missing_count   <- data %>% summarise_all(function(x) sum(is.na(x))) %>% gather(Name, `Missing Data Count`)
  missing_percent <- data %>% summarise_all(function(x) round(mean(is.na(x)) * 100, 1)) %>% gather(Name, `Missing Data %`)
  
  join_all(list(types, unique_values, missing_count, missing_percent), by='Name', type='left')  
}
dim(data)
create_variables_table(data) %>% arrange(Name)


# CPK1 - Are there differences for the various treatments at the CPK1 timepoint ?
data$TRT <- factor(data$TRT, levels = c("N", "L", "M","H"))
ggplot(data, aes(TRT, CPK1)) + geom_boxplot()

aov <- aov(CPK1 ~ TRT, data)
summary(aov) # Do differences exist at all ? (p < 0.05)


# Where are the differences ?
TukeyHSD(aov, "TRT") # The Parwise differences high p-value means not different

tukey <- TukeyHSD(aov, "TRT")
plot(tukey) # Those bars away from 0 are significantly different

# Multiple comparision procedures
pairwise.t.test(data$CPK1, data$TRT)
pairwise.t.test(data$CPK1, data$TRT, p.adjust.method = "none")
pairwise.t.test(data$CPK1, data$TRT, p.adjust.method = "bonferroni")
pairwise.t.test(data$CPK1, data$TRT, p.adjust.method = "BH")
## Very different!


# ANOVA Assumption Testing
library(car)
leveneTest(aov) # Null is that we have constant variance across the groups (Homogenity of variance)

shapiro.test(aov$residuals) # Normality of residuals, p < 0.05 not normally distributed
hist(aov$residuals) # Bimodal - two groupings


# Chi-Square Test - check for homogeneity of variance
data <- read_csv('anova/data/raw/MIFish.csv')
glimpse(data)

data$Species <- factor(data$Species)
data$Location <- factor(data$Location)

table(data) # Frequency table - Distribution of fish per location

# Chi Square Test - 
chisq.test(table(data))



