library(corrplot)
library(randomForest)
library(caret)
library(gbm)
library(groupdata2)
library(dplyr)
library(ggplot2)
library(knitr) 
library(mltest)
library(rpart)
library(e1071)


csv <- read.csv("leaf.csv",header=F,sep = ",")
colnames(csv) <- c("Class","SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","MaximalIndentationDepth","Lobedness","AverageIntensity","AverageContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
data = subset(csv, select = -SpecimenNumber) #il numero del campione non serve
#data = subset(data, select = -Smoothness)
#data = subset(data, select = -AverageContrast)
#data = subset(data, select = -MaximalIndentationDepth)
classes <- c(seq(1,40))
classes %in% data$Class
data$Class=as.factor(data$Class)

corr_matrix <- cor(data)
corr_matrix
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# classes 16,17,18,19,20,21,37,38,39,40 are not present in the dataset, thus we cannot train the 
# classifier on these classes and these classes will be dropped from the possible outputs
# we are left with 30 possible species (classes)

set.seed(Sys.time()) 
data_folds <- fold(data, k = 5, cat_col = "Class")

#------------------------RANDOM FOREST--------------------------------------

rf_acc <- array(NA, dim=5)
rf_tab <- table(append((1:15), (22:36))) - table(append((1:15), (22:36)))
rf_fpr <- array(0, dim = 30)
rf_fnr <- array(0, dim = 30)

acc_svmr <- array(NA,dim = 5)
acc_svml <- array(NA,dim = 5)
acc_svmp <- array(NA,dim = 5)
fpr_svmr <- array(0, dim = 30)
fnr_svmr <- array(0, dim = 30)
fpr_svml <- array(0, dim = 30)
fnr_svml <- array(0, dim = 30)
fpr_svmp <- array(0, dim = 30)
fnr_svmp <- array(0, dim = 30)

t_acc <- array(NA,dim = 5)
t_fpr <- array(0, dim = 30)
t_fnr <- array(0, dim = 30)


for (fold in 1:5) {
  # for each fold, split train-set
  training_set <- data[data_folds$.folds != fold,]
  test_set <- data[data_folds$.folds == fold,]
  
  # x_test, y_test
  xtest<-test_set[2:15]
  ytest<-test_set[[1]]
  
  # setting seed
  set.seed(123)
  
  # preparing grid for our parameters
  rfGrid <- expand.grid(mtry = c(4:13))    # Usually 6~10
  
  # preparing fit procedure
  fitControl <- trainControl(method = "cv", number = 5, verboseIter = T)
  
  # fitting RF with cv over train_set and tuning mtry
  rF_fit <- train(Class ~ .,
                  data = training_set,
                  method = "rf",
                  trControl = fitControl,
                  tuneGrid = rfGrid,
                  verbose = FALSE,
                  ntree = 1500,    # >= 1000 actually; after [Breiman, 1999]
                  minsplit = 2,
                  metric = "Accuracy")
  
  # predict over test set
  ypred <- predict(rF_fit, mtry=rF_fit$finalModel$mtry,  xtest)
  
  # accuracy
  rf_acc[fold] <-sum((ytest == ypred)/length(ytest))
  
  rf_tab <- rf_tab + table(ytest[(1:length(ytest))*(-((ypred == ytest) - 1))])
  
  
  rf_fpr<- rf_fpr + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  rf_fnr<- rf_fnr + ml_test(ypred,ytest,output.as.table = FALSE)$FNR
  
  
  #SVM
  
  #radial SVM
  svmrGrid <- expand.grid(C = c(1, 1.5, 2, 2.5, 3, 3.5), sigma = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35))
  svmrFit <- train(Class ~., data = training_set, method = "svmRadial", trControl = fitControl, tuneGrid = svmrGrid, metric = "Accuracy")
  print(svmrFit)
  ypred <- predict(svmrFit, C = svmrFit$finalModel$C,sigma = svmrFit$finalModel$sigma, xtest)
  acc_svmr[fold] <- sum((ytest==ypred)/length(ytest))
  fpr_svmr<- fpr_svmr + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  fnr_svmr<- fnr_svmr + ml_test(ypred,ytest,output.as.table = FALSE)$FNR 
  
  #linear SVM
  
  svmlGrid <- expand.grid(C = c(1, 1.5, 2, 2.5, 3, 3.5))
  svmlFit <- train(Class ~., data = training_set, method = "svmLinear", trControl = fitControl, tuneGrid = svmlGrid, metric = "Accuracy")
  print(svmlFit)
  ypred <- predict(svmlFit, C = svmlFit$finalModel$C, xtest)
  sum((ytest==ypred)/length(ytest))
  acc_svml[fold] <- sum((ytest==ypred)/length(ytest))
  fpr_svml<- fpr_svml + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  fnr_svml<- fnr_svml + ml_test(ypred,ytest,output.as.table = FALSE)$FNR 
  
  #polynomial SVM
  
  svmpGrid <- expand.grid(C = c(1, 1.5, 2, 2.5, 3, 3.5), scale = c(0.01, 0.05, 0.1, 0.15, 0.2, 0.3), degree=c(1,2,3,4))
  svmpFit <- train(Class ~., data = training_set, method = "svmPoly", trControl = fitControl, tuneGrid = svmpGrid, metric = "Accuracy")
  print(svmpFit)
  ypred <- predict(svmpFit, C = svmpFit$finalModel$C,scale = svmpFit$finalModel$scale,degree = svmpFit$finalModel$degree, xtest)
  acc_svmp[fold] <- sum((ytest==ypred)/length(ytest))
  fpr_svmp<- fpr_svmp + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  fnr_svmp<- fnr_svmp + ml_test(ypred,ytest,output.as.table = FALSE)$FNR 
  
  #DECISION TREE
  
  treeGrid <- expand.grid(maxdepth=(1:30))
  treeFit <- train(Class ~., data = training_set, method = "rpart2", trControl = fitControl, tuneGrid = treeGrid, metric = "Accuracy", minsplit=2)
  print(treeFit)
  ypred <- predict(treeFit, xtest)
  t_acc[fold] <- sum((ytest==ypred)/length(ytest))
  t_fpr<- t_fpr + ml_test(ypred,ytest,output.as.table = FALSE)$FPR
  t_fnr<- t_fnr + ml_test(ypred,ytest,output.as.table = FALSE)$FNR
}

#RF
rf_fpr <- rf_fpr/5
rf_fnr <- rf_fnr/5
rf_FNR<-c(rf_fnr)
rf_FPR <- c(rf_fpr)
rf_final_acc <- mean(rf_acc)
rf_final_acc

Species <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)
rf_fpfn <- data.frame(Species, rf_FPR, rf_FNR)
rf_fpfn

plot(varImp(rF_fit))

#SVMR
fpr_svmr <- fpr_svmr/5
fnr_svmr <- fnr_svmr/5
FNR_svmr<-c(fnr_svmr)
FPR_svmr <- c(fnr_svmr)
svmr_final_acc <- mean(acc_svmr)
svmr_final_acc

fpfn_svmr <- data.frame(Species, FPR_svmr, FNR_svmr)
fpfn_svmr
#SVML
fpr_svml <- fpr_svml/5
fnr_svml <- fnr_svml/5
FNR_svml<-c(fnr_svml)
FPR_svml <- c(fpr_svml)
svml_final_acc <- mean(acc_svml)
svml_final_acc

fpfn_svml <- data.frame(Species, FPR_svml, FNR_svml)
fpfn_svml
#SVMP
fpr_svmp <- fpr_svmp/5
fnr_svmp <- fnr_svmp/5
FNR_svmp<-c(fnr_svmp)
FPR_svmp <- c(fpr_svmp)
svmp_final_acc <- mean(acc_svmp)
svmp_final_acc

fpfn_svmp <- data.frame(Species, FPR_svmp, FNR_svmp)
fpfn_svmp

#DT
t_final_acc <- mean(t_acc)
t_final_acc

t_fpr <- t_fpr/5
t_fnr <- t_fnr/5
t_FNR<-c(t_fnr)
t_FPR <- c(t_fpr)
fpfn_tree <- data.frame(Species, t_FPR, t_FNR)
fpfn_tree
plot(varImp(treeFit))






