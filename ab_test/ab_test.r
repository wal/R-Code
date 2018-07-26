library(timeDate)
library(ggplot2)

# Choose parameters:
pA <- 0.06 # True click through rate for group A
pB <- 0.06 # True click through rate for group B
nA <- 500 # Number of cases for group A
nB <- 500 # Number of cases for group B
alpha <- 0.05 # Significance level

# Simulate data:
set.seed(47849)
data <- data.frame(group = rep(c("A", "B"), c(nA, nB)),
                   timestamp = sample(seq(as.timeDate('2016-06-02'),
                                          as.timeDate('2016-06-09'), by = 1), nA+nB),
                   clickedTrue = as.factor(c(rbinom(n = nA, size = 1, prob = pA),
                                             rbinom(n = nB, size = 1, prob = pB))))



head(data)
# Order data by timestamp
data <- data[order(data$GMT.x..i..), ]
levels(data$clickedTrue) <- c("0", "1")

# Compute current p-values after every observation:
pValues <- c()
index <- c()
for (i in 50:dim(data)[1]){
  
  presentData <- table(data$group[1:i], data$clickedTrue[1:i])
  
  if (all(rowSums(presentData) > 0)){
    pValues <- c(pValues, prop.test(presentData)$p.value)
    index <- c(index, i)}
}
results <- data.frame(index = index,
                      pValue = pValues)

# Plot the p-values:
ggplot(results, aes(x = index, y = pValue)) +
  geom_line() +
  geom_hline(aes(yintercept = alpha)) +
  scale_y_continuous(name = "p-value", limits = c(0,1)) +
  scale_x_continuous(name = "Observed data points") +
  theme(text = element_text(size=20))
