#download all necessary packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rlist)) install.packages("rlist", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(Rborist)
library(dplyr)
library(corrplot)
library(gridExtra)
library(dslabs)
library(rpart)

#choose cleveland data from heart disease data

heart_1 <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')

#set the names of each column to those provided in the dataset

names(heart_1) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
heart_1 <- as.tibble(heart_1)

#add a column to the heart data that is categorical

heart_1 <- heart_1 %>%
  mutate(age_cat = case_when(age >= 70 ~ '70+',
                             age >= 60 ~ '60-69',
                             age >= 50 ~ '50-59',
                             age >= 40 ~ '40-49',
                             age < 40 ~ '<40')) %>%
  mutate(num = case_when(num == 0 ~ 0, num > 0 ~ 1))

#separate data into a training set and a test set with a 90-10 split

set.seed(1984, sample.kind = "Rounding")
test_index <- createDataPartition(heart_1$num, p = 0.1, times = 1, list = FALSE)
heart_train <- heart_1[-test_index,]
heart_test <- heart_1[test_index,]

#RMSE function to find accuracy of prediction

RMSE <- function(predicted_ratings, actual_ratings){
  sqrt(mean((actual_ratings-predicted_ratings)^2,na.rm=T))
}

RMSEList <- list()

#plot the relationshipo between age and average severity of heart disease

heartageG <- heart_train %>% 
  group_by(age) %>% 
  summarize(avg = mean(num)) %>%
  ggplot(aes(age, avg)) +
  geom_point() +
  geom_smooth(method = "lm")

#graph the number of people with each severity of heart disease, coloring based on the age category
#and find the correlation between age and severity

heart_age <- heart_train %>%
  group_by(age) %>% 
  summarize(avg = mean(num)) %>%
  select(age, avg)
cor(heart_age$age, heart_age$avg)
heart_train %>% ggplot(aes(num, color = age_cat)) + geom_bar()

#graph the relationship between the rest bps and average severity

hearttrestG <- heart_train %>% 
  group_by(trestbps) %>% 
  summarize(avg = mean(num)) %>%
  ggplot(aes(trestbps, avg)) +
  geom_point() + 
  geom_smooth(method = "lm")

#graph relationship between cp and average severity of heart disease

heartcG <- heart_train %>% 
  group_by(cp) %>% 
  summarize(avg = mean(num)) %>%
  ggplot(aes(cp, avg)) + 
  geom_point() + 
  geom_smooth()

#see the difference in average severity based on sex

heart_train %>% 
  group_by(sex) %>% 
  summarize(avg = mean(num))

#see influence of blood sugar on data

heart_train %>% 
  group_by(fbs) %>% 
  summarize(avg = mean(num))

#see the relationship between restecg and average severity and find the correlation

heart_rest <- heart_train %>% 
  group_by(restecg) %>% 
  summarize(avg = mean(num)) %>% 
  na.omit()

cor(heart_rest$restecg, heart_rest$avg)

#see relationship between thalach and average severity

heartthalG <- heart_train %>% 
  group_by(thalach) %>% 
  summarize(avg = mean(num)) %>% 
  ggplot(aes(thalach, avg)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#see effect of exang based on category on severity

heart_train %>% 
  group_by(exang) %>% 
  summarize(avg = mean(num))

#see effect of oldpeak on average severity

heartoldG <- heart_train %>% 
  group_by(oldpeak) %>% 
  summarize(avg = mean(num)) %>% 
  ggplot(aes(oldpeak, avg)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#see effect of slope, ca, and thal categories on average severity

heart_train %>% 
  group_by(slope) %>% 
  summarize(avg = mean(num))

heart_train %>% 
  group_by(ca) %>% 
  summarize(avg = mean(num))

heart_train %>% 
  group_by(thal) %>% 
  summarize(avg = mean(num))

#create dataset without any NAs and find the correlation table/correlation plot
#deselect age_cat as it is not numerical

heart_train_2 <- na.omit(heart_train)
heart_corr <- cor(heart_train_2 %>% select(-age_cat))
corrplot(heart_corr)

#arrange five of the graphed variables to investigate which of them have strong relationship
#between the variable and average severity

grid.arrange(heartoldG, heartthalG, heartcG, hearttrestG, heartageG)

#find the mean of the severities to have a baseline predictor

mu <- mean(heart_train$num)

#adjust mean based on thalach effect

heart_thalach <- heart_train %>% 
  group_by(thalach) %>% 
  summarize(b_t = mean(num-mu))

#adjust mean based on thalach and age

heart_age <- heart_train %>%
  left_join(heart_thalach, by = 'thalach') %>%
  group_by(age) %>%
  summarize(b_a = mean(num - mu - b_t))

#adjust based on thalach, age, oldpeak

heart_oldpeak <- heart_train %>%
  left_join(heart_thalach, by = 'thalach') %>%
  left_join(heart_age, by = 'age') %>%
  group_by(oldpeak) %>%
  summarize(b_o = mean(num - mu - b_t - b_a))

#adjust based on thalach, age, oldpeak, cp

heart_cp <- heart_train %>%
  left_join(heart_thalach, by = 'thalach') %>%
  left_join(heart_age, by = 'age') %>%
  left_join(heart_oldpeak, by = 'oldpeak') %>%
  group_by(cp) %>%
  summarize(b_c = mean(num - mu - b_t - b_a - b_o))

#combine each of the four above adjustments with the validating dataset

heart_val_t <- heart_test %>% 
  left_join(heart_thalach, by = 'thalach') %>% 
  mutate(predicted = mu + b_t)

#any adjustment in the predictions that returns NA is replaced with the baseline predictor
heart_val_t[is.na(heart_val_t)] <- mu

heart_val_te <- heart_test %>% 
  left_join(heart_thalach, by = 'thalach') %>% 
  left_join(heart_age, by = 'age') %>% 
  mutate(predicted = mu + b_t + b_a)

heart_val_te[is.na(heart_val_te)] <- mu

heart_val_teo <- heart_test %>% 
  left_join(heart_thalach, by = 'thalach') %>% 
  left_join(heart_age, by = 'age') %>% 
  left_join(heart_oldpeak, by = 'oldpeak') %>%
  mutate(predicted = mu + b_t + b_a + b_o)

heart_val_teo[is.na(heart_val_teo)] <- mu

heart_val_teoc <- heart_test %>% 
  left_join(heart_thalach, by = 'thalach') %>% 
  left_join(heart_age, by = 'age') %>% 
  left_join(heart_oldpeak, by = 'oldpeak') %>%
  left_join(heart_cp, by = 'cp') %>%
  mutate(predicted = mu + b_t + b_a + b_o + b_c)

heart_val_teoc[is.na(heart_val_teoc)] <- mu

#find the RMSE of all four methods 
RMSEList[["mu"]] <- RMSE(mu, heart_test$num)
RMSEList[["t"]] <- RMSE(heart_val_t$predicted, heart_test$num)
RMSEList[["te"]] <- RMSE(heart_val_te$predicted, heart_test$num)
RMSEList[["teo"]] <- RMSE(heart_val_teo$predicted, heart_test$num)
RMSEList[["teoc"]] <-RMSE(heart_val_teoc$predicted, heart_test$num)

#repeat above method but with regularization, lambdas is tuning parameter

lambdas <- seq(0, 25, .5)

regRMSE <- sapply(lambdas, function(L) {
  
  #create four prediction adjustments, similar to above but incorporating lambda
  
  heart_thalach <- heart_train %>%
    group_by(thalach) %>%
    summarize(b_t = sum(num-mu)/n() + L)

  heart_age <- heart_train %>%
    left_join(heart_thalach, by = 'thalach') %>%
    group_by(age) %>%
    summarize(b_a = sum(num - mu - b_t)/n() + L)

  heart_oldpeak <- heart_train %>%
    left_join(heart_thalach, by = 'thalach') %>%
    left_join(heart_age, by = 'age') %>%
    group_by(oldpeak) %>%
    summarize(b_o = sum(num - mu - b_t - b_a)/n() + L)

  heart_cp <- heart_train %>%
    left_join(heart_thalach, by = 'thalach') %>%
    left_join(heart_age, by = 'age') %>%
    left_join(heart_oldpeak, by = 'oldpeak') %>%
    group_by(cp) %>%
    summarize(b_c = sum(num - mu - b_t - b_a - b_o)/n() + L)

  #join the predictors with the validation sets
  
  heart_val_t <- heart_test %>%
    left_join(heart_thalach, by = 'thalach') %>%
    mutate(predicted = mu + b_t)
  heart_val_t[is.na(heart_val_t)] <- mu

  heart_val_te <- heart_test %>%
    left_join(heart_thalach, by = 'thalach') %>%
    left_join(heart_age, by = 'age') %>%
    mutate(predicted = mu + b_t + b_a)
  heart_val_te[is.na(heart_val_te)] <- mu

  heart_val_teo <- heart_test %>%
    left_join(heart_thalach, by = 'thalach') %>%
    left_join(heart_age, by = 'age') %>%
    left_join(heart_oldpeak, by = 'oldpeak') %>%
    mutate(predicted = mu + b_t + b_a + b_o)
  heart_val_teo[is.na(heart_val_teo)] <- mu

  heart_val_teoc <- heart_test %>%
    left_join(heart_thalach, by = 'thalach') %>%
    left_join(heart_age, by = 'age') %>%
    left_join(heart_oldpeak, by = 'oldpeak') %>%
    left_join(heart_cp, by = 'cp') %>%
    mutate(predicted = mu + b_t + b_a + b_o + b_c)
  heart_val_teoc[is.na(heart_val_teoc)] <- mu

  #compare predictions and validation numbers
  
  RMSE(heart_val_t$predicted, heart_test$num)
  RMSE(heart_val_te$predicted, heart_test$num)
  RMSE(heart_val_teo$predicted, heart_test$num)
  RMSE(heart_val_teoc$predicted, heart_test$num)
})

RMSEList[["regRMSE"]] <- min(regRMSE)

#transform columns on both train and test datasets in order to use train() algorithms

heart_train_2$sex <- as.factor(heart_train_2$sex)
heart_train_2$cp <- as.factor(heart_train_2$cp)
heart_train_2$fbs <- as.factor(heart_train_2$fbs)
heart_train_2$restecg <- as.factor(heart_train_2$restecg)
heart_train_2$exang <- as.factor(heart_train_2$exang)
heart_train_2$slope <- as.factor(heart_train_2$slope)
heart_train_2$ca <- as.factor(heart_train_2$ca)
heart_train_2$thal <- as.factor(heart_train_2$thal)
heart_train_2$num <- as.factor(heart_train_2$num)

heart_test$sex <- as.factor(heart_test$sex)
heart_test$cp <- as.factor(heart_test$cp)
heart_test$fbs <- as.factor(heart_test$fbs)
heart_test$restecg <- as.factor(heart_test$restecg)
heart_test$exang <- as.factor(heart_test$exang)
heart_test$slope <- as.factor(heart_test$slope)
heart_test$ca <- as.factor(heart_test$ca)
heart_test$thal <- as.factor(heart_test$thal)
heart_test$num <- as.factor(heart_test$num)

heart_train_2 <- heart_train_2 %>% select(-age_cat)

heart_train_2 <- heart_train_2 %>% na.omit()

#for each model, train a fit and then use it to predict the test heart disease severity,
#using RMSE to judge accuracy

#glm model

fitglm <- train(num ~ ., model = "glm", data = heart_train_2)

RMSEList[["glm"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                          as.numeric(as.character(predict(fitglm, heart_test))))

#lda model

fitlda <- train(num ~ ., model = "lda", data = heart_train_2)
RMSEList[["lda"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                          as.numeric(as.character(predict(fitlda, heart_test))))

#naive_bayes model

fitbayes <- train(num ~ ., model = "naive_bayes", data = heart_train_2)

RMSEList[["naive_bayes"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                                  as.numeric(as.character(predict(fitbayes, heart_test))))

#svmLinear model

fitsvm <- train(num ~ ., model = "svmLinear", data = heart_train_2)

RMSEList[["svmLinear"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                                as.numeric(as.character(predict(fitsvm, heart_test))))

#knn model

fitknn <- train(num ~ ., model = "knn", data = heart_train_2)

RMSEList[["knn"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                          as.numeric(as.character(predict(fitknn, heart_test))))

#gamLoess model

fitgam <- train(num ~ ., model = "gamLoess", data = heart_train_2)

RMSEList[["gamLoess"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                               as.numeric(as.character(predict(fitgam, heart_test))))

#multinom model

fitmulti <- train(num ~ ., model = "multinom", data = heart_train_2)

RMSEList[["multinom"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                               as.numeric(as.character(predict(fitmulti, heart_test))))

#qda model

fitqda <- train(num ~ ., model = "qda", data = heart_train_2)

RMSEList[["qda"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                          as.numeric(as.character(predict(fitqda, heart_test))))

#rf model

fitrf <- train(num ~ ., model = "rf", data = heart_train_2)

RMSEList[["rf"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                         as.numeric(as.character(predict(fitrf, heart_test))))

#adaboost model

fitada <- train(num ~ ., model = "adaboost", data = heart_train_2)

RMSEList[["adaboost"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
                               as.numeric(as.character(predict(fitada, heart_test))))

#plot the accuracy of two of the methods

fitQdaPlot <- plot(fitqda)
fitAdaPlot <- plot(fitada)
grid.arrange(fitQdaPlot, fitAdaPlot)

#create an rpart fit and visualization, evaluating the fit for RMSE of prediction

fitrpart <- rpart(num ~ ., data = heart_train_2)
plot(fitrpart)
text(fitrpart)

RMSEList[["rpart"]] <- RMSE(as.numeric(as.character(heart_test$num)), 
     as.numeric(as.character(predict(fitrpart, heart_test))))

#create an Rborist fit and evaluate fit for RMSE of prediction

train_rborist <- train(num ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = heart_train_2)

RMSEList[["train_rborist"]] <- RMSE(as.numeric(as.character(predict(train_rborist, heart_test))), 
                          as.numeric(as.character(heart_test$num)))

#comparison list:
RMSEList