---
title: Practical Machine Learning Project - Human Activity Recognition Data Analysis Report
author: "by Larry YE"
output:
  html_document:
    fig_height: 9
    fig_width: 9
---

## Background  
Using devices such as JawboneUp, NikeFuelBand, and Fitbitit is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 
In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website: 
[http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset).

## Preparing the data and R packages  
```{r, cache = T}
require(caret)
require(corrplot)

require(rpart)
require(rpart.plot)
require(randomForest)

require(knitr)
require(ggplot2)

knitr::opts_chunk$set(cache=TRUE)
```
### Getting Data
```{r, cache = T}
        train.url ="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        test.url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        train.name = "./data/pml_training.csv"
        test.name = "./data/pml_testing.csv"
        if (!file.exists("./data")) {dir.create("./data")}
        if (!file.exists(train.name)) {download.file(train.url, destfile=train.name)}
        if (!file.exists(test.name)) {download.file(test.url, destfile=test.name)}
        trainraw = read.csv("./data/pml_training.csv")
        testraw = read.csv("./data/pml_testing.csv")
```  
### Data understanding
The raw training data has 19622 observations and 160 variables, in which:
- 1 index: Column `X` is row number
- 1 user name variable: there are 6 user's, who are adelmo, carlitos, charles, eurico, jeremy, pedro
- 3 time stamps: "raw_timestamp_part_1"     "raw_timestamp_part_2"     "cvtd_timestamp"
- 2 window information: "new_window"               "num_window"
- 1 target variable: "classe"
- 152 sensor data variables. 
The testing data has 20 observations and the same structure except the last variable is problem_id instead of classe.  
```{r, cache = T}
        dim(trainraw)
        dim(testraw)
        names(trainraw)
        names(testraw)
        unique(trainraw$user_name)
```


### Data cleaning
A generic data audit could be done to have a summary of the data. It seems that there are a lot variables are empty.
```{r, cache = T}
    table(complete.cases(trainraw))
```
Remove the columns that contain NA values only.
```{r, cache = T}
        trainraw <- trainraw[, colSums(is.na(trainraw)) == 0]
        testraw <- testraw[, colSums(is.na(testraw)) == 0]
```  
Given each observation as a combination of valid sensor data, can be classified as one type of motion, here further cleansing the non accelerometer related measurements
```{r, cache = T}
        classe <- trainraw$classe
        trainremove <- grepl("^X|timestamp|window", names(trainraw))
        trainraw <- trainraw[, !trainremove]
        traincleaned <- trainraw[, sapply(trainraw, is.numeric)]
        traincleaned$classe <- classe
        testremove <- grepl("^X|timestamp|window", names(testraw))
        testraw <- testraw[, !testremove]
        testcleaned <- testraw[, sapply(testraw, is.numeric)]
```
After the data cleansing, the input variables reduced from 159 to 53.

### Slice the data
Partition the raw training data set to 70% training and 30% testing data set.  
```{r, cache = T}
        set.seed(58763)
        intrain <- createDataPartition(traincleaned$classe, p =.7, list = F)
        traindata <- traincleaned[intrain, ]
        testdata <- traincleaned[-intrain, ]
```

## Classification modelling
Apply the **Random Forest** algorithm, use 6-fold cross validation. The **Random Forest** algorithm automatically selects important variables and it is robust to correlated covariates & outliers in general. A **6-fold cross validation** was applied in the algorithm.  
```{r, cache = T}
        rfcntrl <- trainControl(method = "cv", 6)
        rfmodel <- train(classe ~., data = traindata, method = "rf", trControl= rfcntrl, ntree = 250)
        rfmodel
```
review the performance of the model: 
```{r, cache = T}
        rfpredict <- predict(rfmodel, testdata)
        confusionMatrix(testdata$classe, rfpredict)
        accuracy <- postResample(rfpredict, testdata$classe)
        accuracy
```
The estimated accuracy of the model is 99.35% and the estimated out-of-sample error is 0.65%.
```{r, cache = T}
        oose <- 1- as.numeric(confusionMatrix(testdata$classe, rfpredict)$overall[1])
        oose
```


## Predicting on the proiect Test Data set
Apply the model to the testing data set downloaded from the data source. 
```{r, cache = T}
        result <- predict(rfmodel, testcleaned[, -length(names(testcleaned))])
        result
```  
