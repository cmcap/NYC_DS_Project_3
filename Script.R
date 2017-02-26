#actual script

# Load packages and data
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

setwd("~/Desktop/Kaggle Apartment Data")
test_data <- fromJSON("test.json")
train_data <- fromJSON("train.json")

vars <- setdiff(names(train_data), c("photos", "features"))


test_data <- map_at(test_data, vars, unlist) %>% tibble::as_tibble(.)
train_data <- map_at(train_data, vars, unlist) %>% tibble::as_tibble(.)

#create numfeatures and photos variables 
train_data$num_features = lapply(train_data$features, length) #the number of features per apartment
train_data$num_photos = lapply(train_data$photos, length) #num photos
test_data$num_features = lapply(test_data$features, length) #the number of features per apartment
test_data$num_photos = lapply(test_data$photos, length) #num photos

test_data$num_features = unlist(test_data$num_features)
train_data$num_features = unlist(train_data$num_features)
test_data$num_photos = unlist(test_data$num_photos)
train_data$num_photos = unlist(train_data$num_photos)

#manager_id maniuplation
temp = train_data %>% group_by(manager_id) %>% dplyr::summarise(Highcount = sum(interest_level == 'low'),
                                                  MidCount = sum(interest_level == 'medium'),
                                                  Lowcount = sum(interest_level == 'high'))

temp$total = temp$Highcount + temp$MidCount + temp$Lowcount
temp$highR = temp$Highcount/temp$total
temp$midR = temp$MidCount/temp$total
temp$lowR = temp$Lowcount/temp$total
temp = temp %>% dplyr::mutate(score = (2*highR) + midR)
temp2 = cbind('manager_id' = temp$manager_id, 'score' = temp$score)
temp2 = data.frame(temp2)
temp2$score = as.numeric(as.character(temp2$score))

train_data = left_join(train_data, temp2, by = "manager_id", copy = TRUE)
test_data = left_join(test_data, temp2, by = "manager_id", copy = TRUE)


#building id manipulation - currently not working 
temp = train_data %>% group_by(building_id) %>% dplyr::summarise(Highcount = sum(interest_level == 'low'),
                                                          MidCount = sum(interest_level == 'medium'),
                                                          Lowcount = sum(interest_level == 'high'))
temp$total = temp$Highcount + temp$MidCount + temp$Lowcount
temp$highR = temp$Highcount/temp$total
temp$midR = temp$MidCount/temp$total
temp$lowR = temp$Lowcount/temp$total
temp = temp %>% dplyr::mutate(score = (2*highR) + midR)
temp2 = cbind('building_id' = temp$building_id, 'building_score' = temp$score)
temp2 = data.frame(temp2)
temp2$building_score = as.numeric(as.character(temp2$building_score))

train_data = dplyr::left_join(train_data, temp2, by = "building_id", copy = TRUE)
test_data = left_join(test_data, temp2, by = "building_id", copy = TRUE)


#date formatting
#date formatting
train_data$created <- as.POSIXct(train_data$created, format = "%F %T")
test_data$created <- as.POSIXct(test_data$created, format = "%F %T")
train_data$month <- format(train_data$created, "%m")
test_data$month <- format(test_data$created, "%m")
#I'll get back to this

#number of descriptive words
head(train_data$description)
train_data$num_words = sapply(gregexpr("\\W+", train_data$description), length) + 1
test_data$num_words = sapply(gregexpr("\\W+", test_data$description), length) + 1

#important features words 
feat = train_data$features[train_data$interest_level == 'high']
num_unique <- data.frame(table(unlist(feat)))
num_unique[order(num_unique$Freq, decreasing = TRUE),][1:20,]
#hardwood floors, dishwasher, cats and dogs allowed, elevator, dishwasher, no fee

#collapse features
train_data$features <- lapply(train_data$features, paste, collapse = ' ')
test_data$features <- lapply(test_data$features, paste, collapse = ' ')

#elevators
test = lapply(train_data$features, grep, 'Elevator', fixed = TRUE) == 1
test[is.na(test)] <- 0
train_data$Elevator <- test
train_data$Elevator <- as.factor(train_data$Elevator)

test = lapply(test_data$features, grep, 'Elevator', fixed = TRUE) == 1
test[is.na(test)] <- 0
test_data$Elevator <- test
test_data$Elevator <- as.factor(test_data$Elevator)

#cats allowed
test = lapply(train_data$features, grep, 'Allowed', fixed = TRUE) == 1
test[is.na(test)] <- 0
train_data$Allowed <- test
train_data$Allowed <- as.factor(train_data$Elevator)

test = lapply(test_data$features, grep, 'Allowed', fixed = TRUE) == 1
test[is.na(test)] <- 0
test_data$Allowed <- test
test_data$Allowed <- as.factor(test_data$Elevator)

#hardwood floors
test = lapply(train_data$features, grep, 'Hardwood Floors', fixed = TRUE) == 1
test[is.na(test)] <- 0
train_data$floors <- test
train_data$floors <- as.factor(train_data$floors)

test = lapply(test_data$features, grep, 'Hardwood Floors', fixed = TRUE) == 1
test[is.na(test)] <- 0
test_data$floors <- test
test_data$floors <- as.factor(test_data$floors)

#no fee
test = lapply(train_data$features, grep, 'No Fee', fixed = TRUE) == 1
test[is.na(test)] <- 0
train_data$noFee <- test
train_data$noFee <- as.factor(train_data$noFee)

test = lapply(test_data$features, grep, 'No Fee', fixed = TRUE) == 1
test[is.na(test)] <- 0
test_data$noFee <- test
test_data$noFee <- as.factor(test_data$noFee)

#other feature manipulation
train_data$bathrooms[train_data$bathrooms > 5] <- 5
train_data$bathrooms = as.factor(train_data$bathrooms)

test_data$bathrooms[test_data$bathrooms > 5] <- 5
test_data$bathrooms = as.factor(test_data$bathrooms)

train_data$bedrooms[train_data$bedrooms > 5] <- 5
train_data$bedrooms = as.factor(train_data$bedrooms)

test_data$bedrooms[test_data$bedrooms > 5] <- 5
test_data$bedrooms = as.factor(test_data$bedrooms)

#just important features
train_data$important = train_data$Elevator + train_data$noFee + train_data$Allowed + train_data$floors
test_data$important = test_data$Elevator + test_data$noFee + test_data$Allowed + test_data$floors
train_data$important = as.factor(train_data$important)
test_data$important = as.factor(test_data$important)
#to test the accuracy
 
set.seed(0)
inds = sample(1:nrow(train_data), 0.75*nrow(train_data))
train <- train_data[inds, ]
test <- train_data[-inds, ]

# #tree models
# library(gbm)
library(tree)
# library(data.table)
# n.trees = seq(from = 100, to = 20000, by = 100)
# set.seed(0)
# boostInterest = gbm(interest_level ~ bathrooms + bedrooms +
#                       price + num_features + latitude + longitude + 
#                       num_photos, data = train_data,
#                     distribution = "multinomial",
#                     n.trees = 20000,
#                     interaction.depth = 5)
# 
# #prediction
# pred = predict(boostInterest, test_data, n.trees = n.trees, type = 'response')
# 
# #checking the accuracy if using subset
# library(caret)
# pred2 <- as.factor(colnames(pred[,,69])[max.col(pred[,,69])])
# confusionMatrix(pred2, test$interest_level)$overall
# 
# #choose the proper prediction and write to a dataframe 
# preds <- data.frame(pred[,,70])

testPreds <- data.frame(listing_id = test_data$listing_id, predictions[,c('high', 'medium', 'low')])
library(data.table)
fwrite(testPreds, "submission.csv")


# #h2o
# library(h2o)
# # 
# train[-c(4,,11)]
# # 
# vars2 <- setdiff(colnames(test_data), 'interest_level')
# # 
# h2o.init()
# train <- h2o::as.h2o(train[-c(4,7,12)])
# 
# set.seed(0)
# gbm1 <- h2o.gbm(x = varnames
#                 ,y = "interest_level"
#                 ,training_frame = train
#                 ,distribution = "multinomial"
#                 ,model_id = "gbm1"
#                 ,ntrees = 5000
#                 ,max_depth = 7
#                 ,stopping_rounds = 5
#                 ,stopping_metric = "logloss"
#                 ,stopping_tolerance = 0
# )

#_______________________________________________________________________#
#test models for ensemble
library(e1071)
y <- svm()

library(nnet)
x <- multinom(interest_level ~ building_score, train_data, na.action = na.pass)
#multinomial model
glm(interest_level ~ price, family = 'multinomial', data = train_data, )

#_______________________________________________________________________#
#caret stuff - preferred way

library(caret)
fitControl <- trainControl(method = "cv", 
                           number = 3, 
                           verboseIter = TRUE,
                           
                           ## Estimate class probabilities 		   	  
                           classProbs = TRUE,
                           summaryFunction=mnLogLoss)
                           

set.seed(0) 
gbmGrid <- expand.grid(interaction.depth = c(2,5), 	
                       n.trees = seq(from = 100, to = 2000, by = 100),
                       shrinkage = 0.1, 
                       n.minobsinnode = 10)

gbm <- train(interest_level ~ bathrooms + bathrooms +
               price + num_features + latitude + longitude +
               num_photos + score + num_words +
               important + created, data = train, method = "gbm", 
                 trControl = fitControl, 
                 verbose = TRUE,
                 # tuneLength = 1,
                 tuneGrid = gbmGrid, 
                 ## Specify which metric to optimize 
                 metric = "logLoss",
                 maximize = FALSE)

gbm$bestTune


fitCtrl <- trainControl(method = 'none')
gbmFinal <- train(interest_level ~ bathrooms + bathrooms +
                    price + num_features + latitude + longitude +
                    num_photos + score + num_words +
                    important, data = train_data, method = "gbm", 
             trControl = fitCtrl, 
             verbose = TRUE,
             # tuneLength = 1,
             tuneGrid = gbm$bestTune, 
             ## Specify which metric to optimize 
             metric = "logLoss",
             maximize = FALSE)


#variable importance
gbmImp <- varImp(gbm, scale = TRUE)
plot(gbmImp, top = 20)

trellis.par.set(caretTheme())
plot(gbm, metric = "logLoss")

#predictions from the final models
predicted <- predict(gbmFinal, test_data, type = 'prob', na.action = na.pass)
lm_predicted <- predict(x, test_data, type = 'prob', na.action = na.pass)

#replacing NA values in the linear model
lm_predicted[is.na(lm_predicted)] <- predicted[is.na(lm_predicted)]

predictions<-(lm_predicted + predicted)/2

gbm$resample$logLoss



postResample(pred = predicted, test$interest_level)
mnLogLoss(predicted, lev = c('high', 'medium', 'low'), model = 'cv')
