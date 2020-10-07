library(tidyverse)
library(caret)
library(data.table)

#---
# here try out the prediction using other method

names(edx_train)
edx_train_min <- edx_train %>% select(userId,movieId,rating,genres,yr_release,yr_rating)
edx_train_min <- edx_train %>% select(userId,movieId,rating)

edx_train <- edx_train %>% mutate(rating_y=rating/5)
edx_test <- edx_test %>% mutate(rating_y=rating/5)

edx_train <- edx_train %>% 
  mutate(yr_release=as.numeric(yr_release)) %>%
  mutate(yr_lapse=yr_rating-yr_release) %>%
  mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))

rm(edx,test_index,validation)

#logistic regression, 
rm(fit_glm)

fit_glm <- glm(rating_y~userId+movieId, data=edx_train,family = "binomial")   #execute times: 11min
#fit_glm <- train(rating~userId+movieId, data=edx_train,method = "glm")
predicted_glm <- predict(fit_glm, newdata = edx_test)
glm_rmse <- RMSE(predicted_glm,edx_test$rating_y)
### after 11 min, glm_rmse = 0.2636803 a surprising good result???

confusionMatrix(as.factor(edx_test$rating_y),as.factor(round(predicted_glm*5,0)))

table(as.factor(round(predicted_glm*5,2)))


#random forest
set.seed(9,sample.kind = "Rounding")
tuneGrid <- expand.grid(.mtry = c(3, 5, 7, 9))
fit_rf <- train(rating~userId+movieId, data=edx_train,
                method = "rf",
                tuneGrid = tuneGrid,
                importance = TRUE)
predicted_rf <- predict(fit_rf, newdata = edx_test)


#---------------------------

library(rpart)
library(rpart.plot)
edx_train_min <- edx_train %>% select(userId,movieId,rating,yr_release,yr_rating)

fit_rpart <- rpart(rating ~ . , data = edx_train_min, method = 'class')
rpart.plot(fit_rpart)
text(fit_rpart, pretty = 0)
summary(fit_rpart)
predicted_rpart <- predict(fit_rpart, newdata = edx_test, type = 'class')

RMSE(as.numeric(predicted_rpart)/2,edx_test$rating)    #rmse = 1.17432

ccc <- cbind(edx_test$rating,predicted_rpart)
ccc

