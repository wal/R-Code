## Split into test/train
library(modelr)
splits <- resample_partition(iowa_data, c(test = 0.3, train = 0.7))
train_data <- splits$train
summary(train_data)
test_data <- splits$test
summary(test_data)


## Decision Tree
model <- rpart(Price ~ Rooms + Longtitude, data = melbourne_data)
plot(model, uniform=TRUE)
text(model, cex=.6)

## Random Forrest
model <- randomForest(Price ~ Rooms + Bathroom + Landsize + BuildingArea + Lattitude + Longtitude, data = splitData$train)

# get the mean average error for our new model, based on our test data
mae(model = fitRandomForest, data = splitData$test)

# Model Evaluation
library(modelr)
mae(model, data = test)
rsquare(model, data = test)
rmse(model, data = test)
qae(model, data = test)

## Loop to check the MSE for various tree depths
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
    
    # turn the predictors & target into a formula to pass to rpart()
    predictors <- paste(predictors, collapse="+")
    formula <- as.formula(paste(target,"~",predictors,sep = ""))
    
    # build our model
    model <- rpart(formula, data = training_data,
                   control = rpart.control(maxdepth = maxdepth))
    # get the mae
    mae <- mae(model, testing_data)
    return(mae)
}

target <- "Price"
predictors <-  c("Rooms","Bathroom","Landsize","BuildingArea",
                 "YearBuilt","Lattitude","Longtitude")


models_eval <- map_df(1:10, function(x) {
    mae <- get_mae(maxdepth = x, 
                   target = target, 
                   predictors = predictors, 
                   training_data = train_data, 
                   testing_data = test_data)
    tibble(MaxDepth = x, Error = mae)
})

## Plot showing MaxDepth v Error
ggplot(models_eval, aes(MaxDepth, Error)) + 
  geom_point() +
  geom_line()


## Caret
# Create a Confusion Matrix
Confusiox_matix <- confusionMatrix(submission_1$Survived, test$Survived, positive = "1", mode = "prec_recall")

accuracy <- confusion_matix$overall[['Accuracy']]

