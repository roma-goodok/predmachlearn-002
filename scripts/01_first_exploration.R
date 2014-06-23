


setwd("C:\\Data\\Coursera\\predmachlearn-002\\programm_assigment\\predmachlearn-002")

## load data:
pml <- read.csv("data\\pml-training.csv", na.strings = c("", "NA", "#DIV/0!"))


pml.submit <- read.csv("data\\pml-testing.csv", na.strings = c("", "NA", "#DIV/0!"))

### prepare data set for submition: set the same type for some features
pml.submit$new_window <- factor(pml.submit$new_window, levels(pml$new_window))
pml.submit$cvtd_timestamp <- factor(pml.submit$cvtd_timestamp,levels(pml$cvtd_timestamp))



##
##  Exploration and data cleaning
##

## explorations 
summary(pml)

## We can see, that we have a lot of column with moslty NA values
## find not sparsed columns, with at least 10% not NA values:
not.sparsed.features <- apply(pml, 2, function (x) { mean(is.na(x))}) < 0.1 
names(pml)[not.sparsed.features]

#check again count of NA for not.sparsed.features:
# it's ok if sum = 0
sum(apply(pml[, not.sparsed.features], 2, function (x) { mean(is.na(x))}))

#check that the dataset for submition this column also have all NA values.
# it's ok if sum = 0
sum(!is.na(pml.submit[,!not.sparsed.features]))

# in fact it not obligatiry to exlude all NA features for some algorithm (random forest,decision tree), we can just replace NA to some abnormal value 
# and get the same result, but it will takes some more time.

#looks again at the not sparsed data:
summary(pml[, not.sparsed.features])
head(pml[, not.sparsed.features])

# find than X, timestampls num windows are looks like leakage features. Let's validate it
leakageFeaturesInd <- c( 3,4,5,6, 7) # timestamps and others  are leackage  features. 
names(pml[, leakageFeaturesInd])



## to evaluate models create data partitions:
library(caret)
inTrain <- createDataPartition(y=pml$classe, p=0.7, list = FALSE)

## check have we all personalized data (user_name) both in the source data set and in the submition dataset
table(training$user_name, training$classe)
table(pml.submit$user_name)
# so we can use user_name column for submition


## TODO check leackage features

leakageFeaturesInd_WithClass <-  c(leakageFeaturesInd, c(160))
names(pml)[leakageFeaturesInd_WithClass]
training.leakage <- pml[inTrain,leakageFeaturesInd_WithClass]
testing.leakage <- pml[-inTrain,leakageFeaturesInd_WithClass]

dummies <- dummyVars( ~ ., data = rbind(pml[,leakageFeaturesInd], pml.submit[,leakageFeaturesInd]))


training.leakage <- data.frame(classe = training.leakage$classe, predict(dummies, newdata = training.leakage))
testing.leakage <- data.frame(classe = testing.leakage$classe, predict(dummies, newdata = testing.leakage))
pml.submit.leackage <- pml.submit[, leakageFeaturesInd]
pml.submit.leackage <- data.frame(predict(dummies, newdata = pml.submit.leackage ))
names(training.leakage)
names(testing.leakage)
names(pml.submit.leackage)

modFit.leakage <- train(as.factor(classe) ~., method="rf", ntree= 100, data = training.leakage, trControl=trainControl(method = "cv", number = 4,returnResamp="all", verboseIter=TRUE))


#rf.model.leakage <- randomForest(classe ~ ., data = training.leakage, ntree=100, do.trace=1, importance=TRUE)
rf.model.leakage  <- modFit.leakage$finalModel


predictions.leakage  <- predict(rf.model.leakage , newdata = testing.leakage); table(predictions.leakage )
confusionMatrix(predictions.leakage ,testing.leakage$classe)
prediction.submit.leakage <- predict(rf.model.leakage, newdata=pml.submit.leackage); prediction.submit.leakage 

## So, we have Accuracy = 99.95 % without using any measures from sensors, so it's seems we have 5 columns with information leakage. Exlude them. 
## And remember prediction for 20 rows for submitting to compare later with "honest" model result



## prepare cleared features vector, removing X column, leackage features and sparsed column
t <- rep(TRUE, dim(pml)[2])
t[leakageFeaturesInd] <- FALSE    # remove leakage features
t[1] <- FALSE                     # remove X column 

cleared.features <- t & not.sparsed.features

# result features vector :
table(cleared.features)
names(pml[,cleared.features])


##### Prepare training and testing data set:
training <- pml[inTrain, cleared.features]
testing <- pml[-inTrain, cleared.features]



cleared.features.withoutLast <- cleared.features;  cleared.features.withoutLast[160] <- FALSE;
# treat user_name factor
dummies <- dummyVars(~ ., data = rbind(pml[,cleared.features.withoutLast], pml.submit[,cleared.features.withoutLast]))

training <- data.frame(classe = training$classe, predict(dummies, newdata = training))
testing <- data.frame(classe = testing$classe, predict(dummies, newdata = testing))
names(training)

dim(training); dim(testing)


## we have not any suggestion about linearity, so use high accuracy method for non-linear data Random Forest
modFit <- train(as.factor(classe) ~., method="rf", ntree= 100, data = training, importance=TRUE, trControl=trainControl(method = "cv", number = 4,returnResamp="all", verboseIter=TRUE))
## it takes 4*2 execution of randomForest algorithm
modFit

rf.model <- modFit$finalModel;
rf.model$xNames


predictions <- predict(rf.model ,newdata=testing);    table(predictions)
confusionMatrix(predictions,testing$classe)

importance <- data.frame(rf.model$importance);  
importance.ordered <- importance[order(-importance$MeanDecreaseAccuracy),]

varImpPlot(rf.model, n.var=nrow(rf.model$importance), main="sensors importance")

## generate submitions:
pml.submit.transformed <- pml.submit[, cleared.features.withoutLast]
pml.submit.transformed <- data.frame(predict(dummies, newdata = pml.submit.transformed ))

prediction.submit.prepared <- predict(rf.model,newdata=pml.submit.transformed)
prediction.submit

# compare result with leakage prediction: 
prediction.submit.leakage 


###### generate submition files 

answers = as.character(prediction.submit)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)





