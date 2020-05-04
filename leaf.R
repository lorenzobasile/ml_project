library("corrplot")
library("randomForest")
library("caret")

data <- read.csv("leaf-2/leaf.csv",header=F,sep = ",")
colnames(data) <- c("Class","Specimen Number","Eccentricity","Aspect Ratio","Elongation","Solidity","Stochastic Convexity","Isoperimetric Factor","Maximal Indentation Depth","Lobedness","Average Intensity","Average Contrast","Smoothness","Third moment","Uniformity","Entropy")
classes <- c(seq(1,40))
classes %in% data$Class

# classes 16,17,18,19,20,21,37,38,39,40 are not present in the dataset, thus we cannot train the 
# classifier on these classes and these classes will be dropped from the possible outputs
# we are left with 30 possible species (classes)

# check if some predictors are redundant
corr_matrix <- cor(data)
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# defining training set and testing set
set.seed(123)

ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train <- data[ind==1,]
#...and test data, which is the 30% of iris
test <- data[ind==2,]

rf.fit <- train(Class~., 
                data=train, 
                method = "rf",     # Use the "random forest" algorithm
                importance = TRUE, # importance=TRUE allows to inspect variable importance
                trControl = trainControl(method = "repeatedcv", # Use cross-validation
                                         number = 5, # Use 10 folds for cross-validation
                                         repeats = 5)
)

rf.fit
rf.fit$finalModel
plot(varImp(rf.fit), top = 10)

rf<-rf.crossValidation()
