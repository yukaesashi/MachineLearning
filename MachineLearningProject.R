download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "~/train.csv", method="curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "~/test.csv", method="curl")

library(caret)

train <- read.table("~/train.csv", header=TRUE, sep=",")
test <- read.table("~/test.csv", header=TRUE, sep = ",")

trainNum <- train[,c(8:159)]

sapply(trainNum,summary)
## From here, it is obvious that the ones as factors can be thrown away

check <- as.data.frame(sapply(trainNum, class))
check1 <- subset(check, !sapply(trainNum, class)=="factor")
names <- rownames(check1)
trainNum1 <- trainNum[,names]

smallVar <- nearZeroVar(trainNum1)
trainNum2 <- trainNum1[,-smallVar]

valid <- function(x){length(na.omit(x))}
check2 <- as.data.frame(apply(trainNum2, 2, valid))
check2
check3 <- subset(check2, apply(trainNum2, 2, valid)=="19622")
names <- rownames(check3)
trainNum3 <- trainNum2[,names]

TRAIN <- cbind(train[,160], trainNum3)
names(TRAIN)[names(TRAIN)=="train[, 160]"] <- "classe"

modFit <- randomForest(classe~., data=TRAIN)

answers = as.vector(predict(modFit, test))

modFit