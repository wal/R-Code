# EDA : To find patterns, reveal, structure, and make tentative model assessments

## Variables
# What are the variables, types, missing data, unique values

### Variables Table
create_variables_table <- function(data) {
  join_all(list(
    data %>% summarise_all(~ class(.)[[1]]) %>% gather(Name, class),
    data %>% summarise_all(~ length(unique(.))) %>% gather(Name, 'Unique Values'),
    data %>% summarise_all(~ sum(is.na(.))) %>% gather(Name, 'Missing Count'),
    data %>% summarise_all(~ mean(is.na(.))) %>% gather(Name, 'Missing %')
), by='Name', type='left')
}

create_variables_table(data) %>% arrange(Name)


## Variation
# Visualise Distribution of variables
# Investigate Typical Values
# Investigate Unusual Values / Outliers
# Investigate Missing values

## Covariation
# Categorical v Continuous
#  geom_freqpoly / geom_bar ..density..
#  boxplots
# Categorical v Categorical
#  geom_tile / geom_count
# Continuous v Continuous
#  geom_point
#  cut one into bins (cut_width) + boxplot

## Patterns
#  Is there a pattern / relationship ?
#  How strong ?
#  What direction ? - describe it ?
#  Could it be due to random chance ? 
#  Does it change when sub-groups are examined ?


# Simple count of values
table(train$Survived) # count of survivors
prop.table(table(train$Survived)) # proportion of survivors
# Proportions using two variables - total or rowwise
prop.table(table(train$Sex, train$Survived)) # Totals for all samples
prop.table(table(train$Sex, train$Survived), 1) # Off the rows 


# Counts & Proportions for groups
train %>% 
  group_by(Survived, Child, Sex) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Number of rows missing data
sum(complete.cases(diamonds)) / sum(!complete.cases(diamonds))

# % Missing data by column
train %>% summarise_all(function(x) mean(is.na(x) * 100))

# Percentages of missing data by column
test_data %>% 
  summarise_all(function(x) round(mean(is.na(x) * 100),1)) %>%
  select_if(function(x) sum(x) > 1) %>% 
  t() %>%
  as.tibble(rownames = "Variable") %>%
  rename(Missing=V1) %>%
  arrange(desc(Missing))

# Gather rows with missing data
missing_row <- train[!complete.cases(train),]


# Simple histogram
diamonds %>% count(cut)

# continuous variable histogram (cut into bins)
diamonds %>% count(cut_width(carat, 0.5)) # Bin of certain size
diamonds %>% count(cut_number(carat, 5)) # 5 equal bins

# Compare two categorical variables - geom_count (size of count)
ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))

# Data Explorer
install.packages('DataExplorer') 
library(DataExplorer)

## Plot Missing Values names(airquality)
plot_missing(airquality)
plot_histogram(airquality) # All Variables
plot_density(airquality) # All Variables
plot_correlation(airquality)
plot_bar(diamonds) 
create_report(airquality) # Generate report

# Keep/Discard numeric columns
numeric_cols <- iowa_data %>% purrr::keep(is.numeric)
non_numeric_cols <- iowa_data %>% purrr::discard(is.numeric)

# GGally
library(GGally)
ggpairs(diamonds)
ggscatmat(diamonds)

# Normally Distributed
ggplot(test_data, aes(lSalePrice)) + 
  geom_histogram(bins = 100) +
  geom_freqpoly(bins = 50)

ggplot(test_data, aes(sample = SalePrice)) + 
  geom_qq() +
  geom_qq_line()

# Correlation between all variables in handy df
library(reshape2)
library(reshape2)
melt(cor(test_data.numeric_variables)) %>% 
  filter(Var1 != Var2, (Var1 == "SalePrice" | Var2 == "SalePrice"), value > 0.5) %>%
  arrange(desc(value)) 
