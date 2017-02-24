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
temp = train_data %>% group_by(manager_id) %>% summarise(Highcount = sum(interest_level == 'low'),
                                                  MidCount = sum(interest_level == 'medium'),
                                                  Lowcount = sum(interest_level == 'high'))

temp$total = temp$Highcount + temp$MidCount + temp$Lowcount
temp$highR = temp$Highcount/temp$total
temp$midR = temp$MidCount/temp$total
temp$lowR = temp$Lowcount/temp$total
temp = temp %>% mutate(score = (2*highR) + midR)
temp2 = cbind('manager_id' = temp$manager_id, 'score' = as.numeric(temp$score))
train_data = left_join(train_data, temp2, by = "manager_id", copy = TRUE)

test_data = left_join(test_data, temp2, by = "manager_id", copy = TRUE)

train_data$score = as.numeric(train_data$score)
test_data$score = as.numeric(test_data$score)

#building id manipulation - currently not working 
temp = train_data %>% group_by(building_id) %>% dplyr::summarise(Highcount = sum(interest_level == 'low'),
                                                          MidCount = sum(interest_level == 'medium'),
                                                          Lowcount = sum(interest_level == 'high'))
temp$total = temp$Highcount + temp$MidCount + temp$Lowcount
temp$highR = temp$Highcount/temp$total
temp$midR = temp$MidCount/temp$total
temp$lowR = temp$Lowcount/temp$total
temp = temp %>% dplyr::mutate(score = (2*highR) + midR)
temp2 = cbind('building_id' = temp$building_id, 'building_score' = as.numeric(temp$score))

train_data = dplyr::left_join(train_data, temp2, by = "building_id", copy = TRUE)
test_data = left_join(test_data, temp2, by = "building_id", copy = TRUE)

train_data$building_score = as.numeric(train_data$building_score)
test_data$building_score = as.numeric(test_data$building_score)


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

#other feature manipulation
train_data$bathrooms[train_data$bathrooms > 5] <- 5
train_data$bathrooms = as.factor(train_data$bathrooms)

test_data$bathrooms[test_data$bathrooms > 5] <- 5
test_data$bathrooms = as.factor(test_data$bathrooms)

train_data$bedrooms[train_data$bedrooms > 5] <- 5
train_data$bedrooms = as.factor(train_data$bedrooms)

test_data$bedrooms[test_data$bedrooms > 5] <- 5
test_data$bedrooms = as.factor(test_data$bedrooms)


#to test the accuracy
 
set.seed(0)
inds = sample(1:nrow(train_data), 0.75*nrow(train_data))
train <- train_data[inds, ]
test <- train_data[-inds, ]

# #tree models
# library(gbm)
# library(tree)
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

testPreds <- data.frame(listing_id = test_data$listing_id, predicted[,c('high', 'medium', 'low')])
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
#multinomial model
# glm(interest_level ~ price, family = 'multinomial', data = train_data)

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
gbmGrid <- expand.grid(interaction.depth = 3, 	
                       n.trees = 1300,
                       shrinkage = 0.1, 
                       n.minobsinnode = 10)

gbm <- train(interest_level ~ bathrooms +
               price + num_features + latitude + longitude +
               num_photos + score + num_words +
               building_score, data = train_data, method = "gbm", 
                 trControl = fitControl, 
                 verbose = TRUE,
                 # tuneLength = 1,
                 tuneGrid = gbmGrid, 
                 ## Specify which metric to optimize 
                 metric = "logLoss",
                 maximize = FALSE)

gbm$bestTune


fitCtrl <- trainControl(method = 'none')
gbmFinal <- train(interest_level ~ bathrooms + bedrooms +
               price + num_features + latitude + longitude +
               num_photos, data = train_data, method = "gbm", 
             trControl = fitCtrl, 
             verbose = TRUE,
             # tuneLength = 1,
             tuneGrid = gbm$bestTune, 
             ## Specify which metric to optimize 
             metric = "logLoss",
             maximize = FALSE)



gbmImp <- varImp(gbm, scale = TRUE)
plot(gbmImp, top = 20)

trellis.par.set(caretTheme())
plot(gbm, metric = "logLoss")

predicted <- predict(gbm, test_data, type = 'prob', na.action = na.pass)

gbm$resample$logLoss
