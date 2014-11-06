library(psych)

data <- read.csv("insurance.csv", stringsAsFactors=TRUE)

str(data)

summary(data$charges)
hist(data$charges)
table(data$sex)
table(data$region)


pairs.panels(data[c("age", "bmi", "children", "charges")])

linear_model <- lm(data=data, charges ~ .)
summary(linear_model)

# Polynomial Model
data$age_squared <- data$age^2
data$bmi30 <- ifelse(data$bmi >= 30,1,0)

polynomial_model <- lm(data=data, charges ~ age + age_squared + children + bmi + sex + bmi30*smoker + region)
summary(polynomial_model)














