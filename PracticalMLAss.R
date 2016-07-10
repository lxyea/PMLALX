### Background
#Using devices such as JawboneUp, NikeFuelBand, and Fitbitit is now possible to collect a large amount of data about personal activity
#relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements
#about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that 
#people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.  
#In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked 
#to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website: 
#[http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) 
#(see the section on the Weight Lifting Exercise Dataset).   

### Preparing the data and R packages  

#### Load packages, set caching 
require(caret)
require(corrplot)

require(rpart)
require(rpart.plot)
require(randomForest)

require(knitr)
require(ggplot2)

knitr::opts_chunk$set(cache=TRUE)

#### Getting Data
# URL of the training and testing data
        train.url ="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        test.url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# file names
        train.name = "./data/pml_training.csv"
        test.name = "./data/pml_testing.csv"
# if directory does not exist, create new
        if (!file.exists("./data")) {dir.create("./data")}
# if files does not exist, download the files
        if (!file.exists(train.name)) {download.file(train.url, destfile=train.name)}
        if (!file.exists(test.name)) {download.file(test.url, destfile=test.name)}
# load the CSV files as data.frame 
        trainraw = read.csv("./data/pml_training.csv")
        testraw = read.csv("./data/pml_testing.csv")

###Data understanding:
        dim(trainraw)
        dim(testraw)
        names(trainraw)
        names(testraw)
        unique(trainraw$user_name)
#The raw training data has 19622 observations and 160 variables.
#1 index: Column `X` is row number
#1 user name variable: there are 6 user's, who are adelmo, carlitos, charles, eurico, jeremy, pedro
#3 time stamps: "raw_timestamp_part_1"     "raw_timestamp_part_2"     "cvtd_timestamp"
#2 window information: "new_window"               "num_window"
#1 target variable: "classe"
#152 sensor data variables. 
#The testing data has 20 observations and the same structure except the last variable is problem_id instead of classe.   


#### Data cleaning
        table(complete.cases(trainraw))  #seems there are huge number of un used columns
#remove the columns that contain NA values
        trainraw <- trainraw[, colSums(is.na(trainraw)) == 0]
        testraw <- testraw[, colSums(is.na(testraw)) == 0]
#given each observation as a combination of valid sensor data, can be classified as one type of motion, 
#here further cleansing the non accelerometer related measurements.
        classe <- trainraw$classe
        trainremove <- grepl("^X|timestamp|window", names(trainraw))
        trainraw <- trainraw[, !trainremove]
        traincleaned <- trainraw[, sapply(trainraw, is.numeric)]
        traincleaned$classe <- classe
        testremove <- grepl("^X|timestamp|window", names(testraw))
        testraw <- testraw[, !testremove]
        testcleaned <- testraw[, sapply(testraw, is.numeric)]

##slice the data  
        set.seed(58763)
        intrain <- createDataPartition(traincleaned$classe, p =.7, list = F)
        traindata <- traincleaned[intrain, ]
        testdata <- traincleaned[-intrain, ]

##Apply the random forest, use 6-fold cross vadlistion
        rfcntrl <- trainControl(method = "cv", 6)
        rfmodel <- train(classe ~., data = traindata, method = "rf", trControl= rfcntrl, ntree = 250)
        rfmodel

#review the performance of the model
        rfpredict <- predict(rfmodel, testdata)
        confusionMatrix(testdata$classe, rfpredict)
        accuracy <- postResample(rfpredict, testdata$classe)
        accuracy
#out of sample error
        oose <- 1- as.numeric(confusionMatrix(testdata$classe, rfpredict)$overall[1])
        oose


##Predicting the proiect Test Data set
        result <- predict(rfmodel, testcleaned[, -length(names(testcleaned))])
        result

##model visualization
        plotcorr <- cor(traindata[, -length(names(traindata))])
        corrplot(plotcorr, method="color")
        rftree <- rpart(classe ~ ., data=traindata, method="class")
        prp(rftree)

        
#submit the answers
        answers <- result
        pml_write_files <- function(x){
                n = length(x)
                for(i in 1:n){
                        filename = paste0("problem_results/problem_id_",i,".txt")
                        write.table(x[i], file=filename, quote=FALSE,
                                    row.names=FALSE, col.names=FALSE)
                }
        }
         pml_write_files(answers)
        