### LIBRARY CALL ###

library(ggplot2)
library(reshape2)    # To reshape date for ggplot
library(e1071) 
#library("rattle")

#######################################################################################################

### READING DATA ###
df <- read.csv("data/train.csv")
df.test  <- read.csv("data/test.csv")


df.test$Survived <- NA
df <- rbind(df, df.test)
df$Fare[is.na(df$Fare)] <- mean(df$Fare, na.rm=TRUE)

df$Embarked[df$Embarked == ""] <- "C"
extractTitle <- function(Names) {
  Title  <- sapply(Names, function(x) strsplit(as.character(x),'\\.|, ')[[1]][[2]])
  Title[is.element(Title, c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
  Title[is.element(Title, c("Lady", "Mme", "Ms", "Dona", "the Countess"))] <- "Mrs"
  Title[is.element(Title, c("Mlle"))] <- "Miss"
  
  Title <- factor(Title, levels=c("Mr","Master","Mrs","Miss"))
}

df$Title <- extractTitle(df$Name)
extractFamily <- function(Names) {
  Title  <- sapply(Names, function(x) strsplit(as.character(x),'\\.|, ')[[1]][[1]])
}

df$Family <- extractFamily(df$Name)
extractSurvivingRelatives <- function(x) {
  classify <- function(p) {
    if ( any(is.na(p$Survived) ) ) {
      class <- 'Unknown'
    } else if ( all(p$Survived == 0) ) {
      class <- 'None'
    } else if ( any(p$Survived == 1) ) {
      class <- 'Some'
    } 
    class
  }
  
  if ( nrow(x) == 1 ) {
    # Passenger travelling without family (survived maybe NA or not)
    RelatedSurvivors <- 'NoRelatedFamily'
  } else {
    ## I'd love to see how to vectorize this !
    RelatedSurvivors <- character(nrow(x))
    for ( i in 1:nrow(x) ) {
      RelatedSurvivors[i] <- classify(x[-i,])
    }
  }
  
  x$RelatedSurvivors <- factor(RelatedSurvivors, levels=c('NoRelatedFamily', 'Unknown', 'None', 'Some'))
  x
}
CFG <- lapply(unique(df$Family), function(x,D) df[df$Family == x,], D=df)
CFG.rs <- lapply(CFG, extractSurvivingRelatives)
CFG.rs <- Reduce(function(x,x0) rbind(x,x0), CFG.rs[-1], CFG.rs[1][[1]])
df <- CFG.rs

df$Pclass <- factor(df$Pclass, levels=1:3)
df$Embarked <- factor(df$Embarked)

df.withoutAge <- df[is.na(df$Age),]
df.withAge <- df[!is.na(df$Age),]
age.model <- lm(Age ~ Sex + Pclass + SibSp + Parch + Fare + Title, data=df.withAge)
ggplot(data=cbind(df.withAge,PredictedAge=fitted(age.model))) + 
  geom_point(aes(x=Age, y=PredictedAge, colour=Title)) +
  labs(title='Age imputation', x='Known age from data', y='Predicted age')
df.withoutAge$Age <- predict(age.model, newdata=df.withoutAge)

df <- rbind(df.withAge, df.withoutAge)

df.train <- df[!is.na(df$Survived),]
df.test <- df[is.na(df$Survived),]

df.train$Survived <- factor(df.train$Survived)
df.test$Survived <- NULL

F <- Survived ~ Sex + Pclass + poly(Age,3) + SibSp + Parch + Fare + Title + Embarked + RelatedSurvivors

obj <- tune(svm, F, data = df.train, 
            ranges = list(gamma = 2^(-8:-1), cost = 2^(-2:9)),
            tunecontrol = tune.control(sampling = "cross", cross=5)
)
gamma.opt <- obj$best.parameters$gamma
cost.opt <- obj$best.parameters$cost
trainAndCrossValidate <- function(df.train, df.cv) {  
  model <- svm(F, gamma=gamma.opt, cost=cost.opt, data=df.train)
  
  ## Apply to Training Data
  predictions <- predict(model, newdata=df.train)
  
  ## Statistics for training set
  accuracy.train <- length(which(df.train$Survived == predictions)) / nrow(df.train)
  
  ## Apply to cross-validation data
  predictions <- predict(model, newdata=df.cv)
  accuracy.cv <- length(which(df.cv$Survived == predictions)) / nrow(df.cv)
  
  return(list(accuracy.train=accuracy.train, accuracy.cv=accuracy.cv))
}

set.seed(19670122)
f <- sample(1:nrow(df.train), 200)
df.cv <- df.train[f,]
df.train <- df.train[-f,]

data.fraction <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
#data.fraction <- c(0.1,  1)
N <- length(data.fraction)
learningCurve <- data.frame(data.fraction, accuracy.train=rep(0,N), accuracy.cv=rep(0,N))

for ( i in 1:N ) {
  cat("Training on ", data.fraction[i] * 100, "% of data\n", sep="")
  results <- trainAndCrossValidate(df.train[1:round(nrow(df.train) * data.fraction[i]),], df.cv)
  cat("   Accuracy (Training): ", results$accuracy.train * 100, "%\n", sep="")
  cat("   Accuracy (cv):       ", results$accuracy.cv * 100, "%\n", sep="")
  learningCurve$accuracy.train[i] <- results$accuracy.train
  learningCurve$accuracy.cv[i] <- results$accuracy.cv
}
lc <- melt(learningCurve, id.vars="data.fraction")
ggplot(data=lc) + geom_line(aes(x=data.fraction, y=value, group=variable, colour=variable)) + 
  labs(y = 'Accuracy', x = 'Fraction of training set used', title = 'Learning Curves')
test_statistics <- function(labels, predictions) {
  ## Explicit factor to ensure coverage 0f {0,1} .. e.g. if all predictions = 1
  confusion_matrix <- table(data.frame(predicted=factor(predictions, levels=c(1,0)),
                                       actual=factor(labels, levels=c(1,0)) ))
  tp <- confusion_matrix[1,1]
  fp <- confusion_matrix[1,2]
  fn <- confusion_matrix[2,1]
  tn <- confusion_matrix[2,2]
  
  accuracy <- (tp + tn) / (tp + fp + fn + tn)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  F <- 2 * precision * recall / (precision + recall)
  
  # Return stats in a list
  list(confusion_matrix=confusion_matrix, accuracy=accuracy, precision=precision, recall=recall, F1=F)
}

model <- svm(F, gamma=gamma.opt, cost=cost.opt, data=df.train)
predictions <- predict(model, newdata=df.train)

stats <- test_statistics(df.train$Survived, predictions)

stats$confusion_matrix
model <- svm(F, gamma=gamma.opt, cost=cost.opt, data=df.cv)
predictions <- predict(model, newdata=df.cv)

stats <- test_statistics(df.cv$Survived, predictions)

stats$confusion_matrix
df$Survived <- factor(df$Survived)
model <- svm(F, data=df[!is.na(df$Survived),], gamma=gamma.opt, cost=cost.opt)

predictions <- predict(model, newdata=df.test)

write.table(data.frame(PassengerId=df.test$PassengerId, Survived=predictions), 
            file="test_predictions.csv", row.names=FALSE, sep=",")