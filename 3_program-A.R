
#-------- INITIAL -------------
library(tidyverse)
library(caret)
library(data.table)



#-------------------------
# 0.0 data prequalification 
# 0.1 - Any NA?
anyNA(edx)
#-- No NA detected

#-------------------------
# 0.2 - To penalize any user with less than 10 rating 
n_distinct(edx$userId) #total 69878 unique user 

edx %>% group_by(userId) %>% summarise(n=n()) %>% arrange(n)
#-- userid 62516 has rated 10 movies, and others has more than 10.
#-- to decide penalize in later stage.

edx %>% group_by(userId) %>% summarise(n=n()) %>% filter(n<=20) %>% tally()
#-- there are total of 4966 users reviewd 10 to 20 movie. Need to watch up how this affect the overall result.

#-------------------------
# 0.3 - to penalize any movie with less than 10 rating
n_distinct(edx$movieId) #total 10677 movies

edx %>% group_by(movieId) %>% summarise(n=n()) %>% filter(n<=10) %>% tally()
#-- total 1139 movie has less than 10 rating

edx %>% group_by(movieId) %>% summarise(n=n()) %>% filter(n<=10) %>% 
  group_by(n) %>% summarise(count=n())
#-- Need to calculate what's the n value & what is use to regularize.
# n count
# <int> <int>
#   1     1   126
# 2     2   152
# 3     3   119
# 4     4   154
# 5     5   120
# 6     6   107
# 7     7    96
# 8     8    91
# 9     9    89
# 10    10    85


#----------------------------------
# 1.0 ADD year release & year rating
# 1.1 get year_release
edx_new <- edx %>% mutate(title=str_trim(title)) %>% 
  mutate(yr_release=substr(title,str_length(title)-4,str_length(title)-1))

# 1.2 get year_rating
library(lubridate)
edx_new <- edx_new %>% mutate(yr_rating=year(as_datetime(edx$timestamp)))

#----------------------------------
# 2.0 Create test data set
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx_new$rating,times = 1, p = 0.1, list = FALSE)
edx_test <- edx_new[test_index]
edx_train <- edx_new[-test_index]

temp <- edx_test
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(temp,removed)
count(edx_test)+count(edx_train)

#----------------------------------
# 3.0 let's get started
# As of the book --->

# as of 33.7.5 --> modeling movie effect
mu_hat <- mean(edx_train$rating)

movie_avgs <- edx_train %>% group_by(movieId) %>% summarise(movie_effect=mean(rating-mu_hat))
predicted_movie_effect <- mu_hat + edx_test %>%
  left_join(movie_avgs, by="movieId") %>%
  pull(movie_effect)
RMSE(predicted_movie_effect,edx_test$rating)
#RMSE = 0.9429615

# # as of 33.7.6 --> modeling user effects
# user_avgs <- edx_train %>% group_by(userId) %>% summarise(user_effect=mean(rating-mu_hat))
# predicted_user_effect <- mu_hat + edx_test %>%
#   left_join(user_avgs, by="userId") %>%
#   pull(user_effect)
# RMSE(predicted_user_effect,edx_test$rating)
# #RMSE = 0.977709

#as of 33.7.6 - movie then user effect
user_movie_avgs <- edx_train %>% left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_movie_effect = mean(rating - mu_hat - movie_effect))

predicted_user_movie_effect <- edx_test %>%
  left_join(movie_avgs,by="movieId") %>%
  left_join(user_movie_avgs, by="userId") %>%
  mutate(predicted_u_m = mu_hat + movie_effect + user_movie_effect) %>%
  pull(predicted_u_m)

RMSE(predicted_user_movie_effect, edx_test$rating)
#0.8646843



# --------------------------------------------------------------------
# as of 33.9 - modeling movie effect - regularised

# step 1 get lambda
lambdas <- seq(0,10,0.25)
mu_hat <- mean(edx_train$rating)

movie_sums <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(movie_sum=sum(rating-mu_hat), rating_count=n())

movie_rmses <- sapply(lambdas, function(lambda){
  predicted_movie_rating <- edx_test %>%
    left_join(movie_sums, by='movieId') %>%
    mutate(movie_effect = movie_sum/(rating_count+lambda)) %>%
    mutate(predicted = mu_hat + movie_effect) %>%
    pull(predicted)
  return(RMSE(predicted_movie_rating,edx_test$rating))
})

qplot(lambdas,movie_rmses)
lambdas[which.min(movie_rmses)]

# based on above plot, use lambdas = 1.5

# based on lambdas, the RMSE of movie effect with regularization is
movie_lambda=1.5

predicted_movie_rating <- edx_test %>%
  left_join(movie_sums, by='movieId') %>%
  mutate(movie_effect = movie_sum/(rating_count+movie_lambda)) %>%
  mutate(predicted = mu_hat + movie_effect) %>%
  pull(predicted)

RMSE(predicted_movie_rating, edx_test$rating)

# The new RMSE is now 0.942937
# comparing this regularised RMSE with non-regularised RMSE:
# -- Regularised Movie RMSE = 0.942937
# -- Non-regularised Movie RMSE = 0.9429615
# There is some slight improvement of 0.000245

# --------------------------------------------------------------------
# now regularised user effect

movie_reg_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(movie_effect=mean(sum(rating)/(n()+movie_lambda))-mu_hat)

user_movie_sums <- edx_train %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_movie_sum = sum(rating - mu_hat - movie_effect), rating_count=n())

lambdas <- seq(0,10,0.25)

user_movie_rmses <- sapply(lambdas, function(lambda){
  predicted_user_movie_rating <- edx_test %>%
    left_join(user_movie_sums, by='userId') %>%
    left_join(movie_reg_avgs, by="movieId") %>%
    mutate(user_effect = user_movie_sum/(rating_count+lambda)) %>%
    mutate(predicted = mu_hat + user_effect+movie_effect) %>%
    pull(predicted)
  return(RMSE(predicted_user_movie_rating,edx_test$rating))
})

qplot(lambdas,user_movie_rmses)
lambdas[which.min(user_movie_rmses)]

# based on above plot, use lambdas = 5.0

# based on lambdas, the RMSE of user moivie effect with regularization is
user_movie_lambda=5.0

predicted_user_movie_rating <- edx_test %>%
  left_join(user_movie_sums, by='userId') %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  mutate(user_effect = user_movie_sum/(rating_count+user_movie_lambda)) %>%
  mutate(predicted = mu_hat + user_effect+movie_effect) %>%
  pull(predicted)

RMSE(predicted_user_movie_effect, edx_test$rating)

# The new RMSE is now 0.8646843
# comparing this regularised RMSE with non-regularised RMSE:
# -- Regularised User Movie RMSE = 0.8646843
# -- Non-regularised User Movie RMSE = 0.8646843
# There is some slight improvement of 0