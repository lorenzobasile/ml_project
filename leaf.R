library("corrplot")
library("randomForest")
library("caret")

csv <- read.csv("leaf.csv",header=F,sep = ",")
colnames(csv) <- c("Class","SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","Maximal IndentationDepth","Lobedness","AverageIntensity","AverageContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
data = subset(csv, select = -SpecimenNumber ) #il numero del campione non serve
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

#rf<-rf.crossValidation() non serve?
