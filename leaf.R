library(corrplot)
library(randomForest)
library(caret)
library(gbm)
library(groupdata2)
library(dplyr)
library(ggplot2)
library(knitr) 
library(mltest)


csv <- read.csv("leaf.csv",header=F,sep = ",")
colnames(csv) <- c("Class","SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","Maximal IndentationDepth","Lobedness","AverageIntensity","AverageContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
data = subset(csv, select = -SpecimenNumber ) #il numero del campione non serve
classes <- c(seq(1,40))
classes %in% data$Class
data$Class=as.factor(data$Class)

# classes 16,17,18,19,20,21,37,38,39,40 are not present in the dataset, thus we cannot train the 
# classifier on these classes and these classes will be dropped from the possible outputs
# we are left with 30 possible species (classes)

set.seed(1) 
data_folds <- fold(data, k = 5, cat_col = "Class")

#------------------------RANDOM FOREST--------------------------------------

rf_acc <- array(NA, dim=5)
rf_tab <- table(append((1:15), (22:36))) - table(append((1:15), (22:36)))
rf_fpr <- array(0, dim = 30)
rf_fnr <- array(0, dim = 30)

for (fold in 1:5) {
  # for each fold, split train-set
  train_set <- data[data_folds$.folds != fold,]
  test_set <- data[data_folds$.folds == fold,]
  
  # x_test, y_test
  x_test<-test_set[2:15]
  y_test<-test_set[[1]]
  
  # setting seed
  set.seed(Sys.time())
  
  # preparing grid for our parameters
  rfGrid <- expand.grid(mtry = c(4:13))    # Usually 6~10
  
  # preparing fit procedure
  fitControl <- trainControl(method = "cv",
                             number = 5)
  
  # fitting RF with cv over train_set and tuning mtry
  rF_fit <- train(Class ~ .,
                  data = train_set,
                  method = "rf",
                  trControl = fitControl,
                  tuneGrid = rfGrid,
                  verbose = FALSE,
                  ntree = 1500,    # >= 1000 actually; after [Breiman, 1999]
                  minsplit = 2,
                  metric = "Accuracy")
  
  # predict over test set
  ypred <- predict(rF_fit, mtry=rF_fit$finalModel$mtry,  x_test)
  
  # accuracy
  rf_acc[fold] <-sum((y_test == ypred)/length(y_test))
  
  rf_tab <- rf_tab + table(y_test[(1:length(y_test))*(-((ypred == y_test) - 1))])
  
  
  rf_fpr<- rf_fpr + ml_test(ypred,y_test,output.as.table = FALSE)$FPR
  rf_fnr<- rf_fnr + ml_test(ypred,y_test,output.as.table = FALSE)$FNR
  
}

rf_fpr <- rf_fpr/5
rf_fnr <- rf_fnr/5
rf_FNR<-c(rf_fnr)
rf_FPR <- c(rf_fpr)
rf_final_acc <- mean(rf_acc)
rf_final_acc

Species <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)
rf_fpfn <- data.frame(Species, rf_FPR, rf_FNR)
rf_fpfn


#------------------------------------------------BOOSTING---------------------------------------------------

acc <- array(NA,dim = 5)

fpr <- array(0, dim = 30)
fnr <- array(0, dim = 30)

for (fold in 1:5){
  training_set <- data[data_folds$.folds != fold,]
  testing_set <- data[data_folds$.folds == fold,]
  
  xtest<- testing_set[2:15]
  ytest <- testing_set[[1]]

  set.seed(2)
  
  fitControl <- trainControl(method = "cv",number = 5, verboseIter = T)
  
  gbmGrid <- expand.grid(n.trees=100, interaction.depth=5, shrinkage=0.1, n.minobsinnode = 10)
  gbmFit <- train(Class ~., data = training_set, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, metric = "Accuracy")
  print(gbmFit)
  ypred <- predict(gbmFit, n.trees = gbmFit$finalModel$n.trees, interaction.depth=gbmFit$finalModel$interaction.depth, shrinkage=gbmFit$finalModel$shrinkage, n.minobsinnode=gbmFit$finalModel$n.minobsinnode, xtest)
  acc[fold] <- sum((ytest==ypred)/length(ytest))
  fpr<- fpr + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  fnr<- fnr + ml_test(ypred,ytest,output.as.table = FALSE)$FNR }

final_acc <- mean(acc)
final_acc

fpr <- fpr/5
fnr <- fnr/5
FNR<-c(fnr)
FPR <- c(fpr)
Species <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)
fpfn <- data.frame(Species, FPR, FNR)
fpfn

plot(varImp(gbmFit))






