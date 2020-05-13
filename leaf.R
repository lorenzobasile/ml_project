library(corrplot)
library(randomForest)
library(caret)
library(gbm)
library(groupdata2)
library(dplyr)
library(ggplot2)
library(knitr) 


csv <- read.csv("leaf.csv",header=F,sep = ",")
colnames(csv) <- c("Class","SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","Maximal IndentationDepth","Lobedness","AverageIntensity","AverageContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
data = subset(csv, select = -SpecimenNumber ) #il numero del campione non serve
classes <- c(seq(1,40))
classes %in% data$Class
data$Class=as.factor(data$Class)

# classes 16,17,18,19,20,21,37,38,39,40 are not present in the dataset, thus we cannot train the 
# classifier on these classes and these classes will be dropped from the possible outputs
# we are left with 30 possible species (classes)

# check if some predictors are redundant
corr_matrix <- cor(data)
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# defining training set and testing set
set.seed(1) 

#data <- data[sample(nrow(data)),]
folds <- fold(data, k = 5, cat_col = "Class")

rf.fit <- train(Class ~ ., 
                data=data, #per ora non facciamo tuning, quindi tutto train
                method = "rf",
                importance = TRUE,
                trControl = trainControl(method = "repeatedcv",number = 5,repeats = 5) #non compila
                tuneGrid = rfGrid,
                verbose = FALSE,
                # Additional parameters
                ntree = 1500,
                minsplit = 2,
                metric = "Accuracy") #non rmse per classificazione

rf.fit
rf.fit$finalModel
plot(varImp(rf.fit), top = 10)


#------------------------------------------------BOOSTING---------------------------------------------------

set.seed(123)
sdt_localacc <- 0.0
sdt_tab <- table(append((1:15), (22:36))) - table(append((1:15), (22:36)))



for (fold in 1:5){
  training_set <- data[data$.folds != fold,]
  testing_set <- data[data$.folds == fold,]

  xpassed <- testing_set[2:15]
  ytrue <- testing_set[[1]]
  ytrue <- factor(ytrue, levels = levels(data$Class))    
  dflen <- length(ytrue)
  
  fitControl <- trainControl(method = "cv",number = 5)
  
  
  set.seed(2)
  
  sdtGrid <- expand.grid(interaction.depth=seq(1,6,by=1),
                         n.trees=c(25,50,100,200),
                         shrinkage=c(0.01,0.05,0.1),
                         n.minobsinnode = 10)
  sdtfit <- train(Class ~., data = training_set, method = "gbm", trControl = fitControl, tuneGrid = sdtGrid, metric = "Accuracy")
  ypred <- max.col(predict(sdtfit$finalModel, xpassed))   
  sdt_localacc <- (sdt_localacc + sum((ytrue == ypred)/dflen))         
  sdt_tab <- sdt_tab + table(ytrue[(1:length(ytrue))*(-((ypred == ytrue) - 1))])}

  
sdt_localacc <- (sdt_localacc / (5))


