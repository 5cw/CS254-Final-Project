#for data manipulation
library(tidyverse)
#for paired plot
library(GGally)
#for dealing with duration
library(lubridate)
#for random forest
library(randomForest)
#for svm
library(e1071)

#path to the data on my machine
path <- "C:/Users/Admin/Dropbox/Machine Learning/Final_Project/tracks2.csv"
#path <- "C:/Users/jemer/Dropbox/Machine Learning/Final_Project/tracks2.csv"

#reading in data and removing spotify id column
tracks <- read.csv(path)

##############################################################################
#Visualizations

#creating tracks df with only numeric columns and plays
tracks_numeric <- tracks %>% 
  select(-artist, -track, -key, -time_signature, -mode, -duration)

#plotting pairwise scatterplots for all numeric predictors and plays
paired_plot <- ggpairs(tracks_numeric,
                       lower = list(continuous = wrap("points", 
                                                      alpha = 0.25,
                                                      size = 0.15)))

#############################################################################
#Random Forest


#have to convert key, mode, and time_sigmature to factor 
tracks <- tracks %>% 
  mutate(key = as.factor(key),
         mode = as.factor(mode),
         time_signature = as.factor(time_signature),
         liked = as.factor(liked)) %>% 
  #removing track name, artist name and plays
  select(-artist, -track, -plays)

#setting seed for reproducability
set.seed(1)

#randomly getting 80/20 split
indexes <- sample(2, nrow(tracks), replace = TRUE, prob = c(0.8, 0.2))
#training df
tracks_train <- tracks[indexes == 1,]
#test df
tracks_test <- tracks[indexes == 2,]

#running random forest. 500 trees. sqrt(12) = 3 candidate features at each split
rf <- randomForest(data = tracks_train,
                   x = select(tracks_train, -liked),
                   y = tracks_train$liked, 
                   xtest = select(tracks_test, -liked),
                   ytest = tracks_test$liked)

#rf precision
(rf_precision <- rf$test$confusion['1', '1']/sum(rf$test$confusion[,'1']))

#rf recall
(rf_recall <- rf$test$confusion['1', '1']/sum(rf$test$confusion['1',]))


####################################################################################
#SVM

#creating dummy variables
tracks <- tracks %>% 
  mutate(mode_major = ifelse(mode == "Major", 1, 0),
         liked = as.integer(liked)-1)

tracks <- dummy_cols(tracks, 
                     select_columns = c("time_signature", "key"))

#removing old columns
tracks <- tracks %>% 
  select(-time_signature, -key, -mode)


#randomly getting 80/20 split
indexes <- sample(2, nrow(tracks), replace = TRUE, prob = c(0.8, 0.2))
#training df
tracks_train <- tracks[indexes == 1,]
#test df
tracks_test <- tracks[indexes == 2,]

#getting vector of class weights 
class_weights <- c(0.25, 0.75)
names(class_weights) <- c("0", "1")

#fitting svm (default cost (C), tolerance, epsilon)
svm_fit <- svm(data = tracks_train, 
               x = select(tracks_train, -liked), 
               y = tracks_train$liked,
               type = "C",
               class.weights = class_weights)

#getting prediction of test set
svm_pred <- predict(svm_fit, select(tracks_test, -liked))

#precision
(svm_precision <- table(svm_pred, tracks_test$liked)['1', '1']/sum(table(svm_pred, tracks_test$liked)[,'1']))

#recall
(svm_recall <- table(svm_pred, tracks_test$liked)['1', '1']/sum(table(svm_pred, tracks_test$liked)['1',]))


