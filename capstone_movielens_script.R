###############################################################################
#                                                                             #
# title: 'HarvardX (PH125.9x) - Data Science: Capstone - MovieLens Project'   #
# author: "Wayne Koong Wah Yan"                                               #
# date: "10/13/2020"                                                           #
###############################################################################


###############################################################################
# STEP INITIAL: PREPARE ENVIRONMENT
library(tidyverse)
library(caret)
library(data.table)
library(scales)
library(R.utils)
library(formattable)
library(lubridate)

options(dplyr.summarise.inform = FALSE) #suppress summarise info
options(digits = 5)   #default to 5 decimal points
options(warn = -1)    #suppress warnings
options(scipen = 999) #always use regular numbers


###############################################################################
# STEP 1: PREPARE DATA, AS INSTRUCTED 
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(
  unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, 
                                  p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation, by = c("userId", "movieId"))
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###############################################################################
# STEP 2: PARSE DATA, ADD REQUIRED COLUMNS

# Prepare yr_release
edx <- edx %>% mutate(title=str_trim(title)) %>% 
  mutate(yr_release=as.numeric(substr(title,str_length(title)-4,
                                      str_length(title)-1)))

# Prepare yr_rating
edx <- edx %>% mutate(yr_rating=year(as_datetime(timestamp,origin="1970-01-01")))

# Prepare yr_lapse
edx <- edx %>% mutate(yr_lapse=yr_rating-yr_release) 

# Clean yr_lapse
edx <- edx %>% mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))



###############################################################################
# STEP 3: PREPARE TRAINING & TEST DATASET
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating,
                                  times = 1, p = 0.1, list = FALSE)
edx_test <- edx[test_index]
edx_train <- edx[-test_index]

temp <- edx_test
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test, by = c("userId", "movieId"))
edx_train <- rbind(edx_train, removed)

rm(temp,removed)


###############################################################################
# STEP 4a: GET THE MEAN
mu <- mean(edx_train$rating)
mu_rmse <- RMSE(edx_test$rating,mu)

result <- tibble(Method = "Project Target", RMSE = 0.86490)
result <- rbind(result, c(Method = "Mean", RMSE = round(mu_rmse,5)))
result %>% knitr::kable(caption="RMSE Result")


###############################################################################
# STEP 4b: REGULARISED MOVIEID
movie_lambdas <- seq(0,5,0.25)

movie_sums <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(movie_rating_sum=sum(rating-mu), movie_rating_count=n())

movie_rmses <- sapply(movie_lambdas, function(lambda){
  predicted_movie_rating <- edx_test %>%
    left_join(movie_sums, by='movieId') %>%
    mutate(movie_effect = movie_rating_sum/(movie_rating_count+lambda)) %>%
    mutate(predicted = movie_effect+mu) %>%
    pull(predicted)
  return(RMSE(predicted_movie_rating,edx_test$rating))
})

tibble(lambda = movie_lambdas, RMSE = movie_rmses) %>%
  ggplot(aes(x = lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Movies Penalized Effect") +
  theme_minimal()

rmse_movie <- min(movie_rmses)
lambda_movie <- movie_lambdas[which.min(movie_rmses)]
result <- rbind(result, c(Method = "Regularised Movie", 
                          RMSE = round(rmse_movie,5)))
result %>% knitr::kable(caption="RMSE Result")

###############################################################################
# STEP 4c: REGULARISED USERID & MOVIEID
user_movie_lambdas <- seq(0,10,0.25)

user_movie_rmses <- sapply(user_movie_lambdas, function(lambda){
  movie_sums <- edx_train %>%
    group_by(movieId) %>%
    summarise(movie_effect = sum(rating-mu) / (n()+lambda)) %>%
    select(movieId,movie_effect)
  
  user_sums <- edx_train %>%
    left_join(movie_sums, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_effect = sum(rating-mu-movie_effect) / (n()+lambda))
  
  predicted_rating <- edx_test %>%
    left_join(user_sums, by='userId') %>%
    left_join(movie_sums, by="movieId") %>%
    mutate(predicted = mu + user_effect + movie_effect) %>%
    pull(predicted)
  return(RMSE(predicted_rating,edx_test$rating))
})

tibble(lambda = user_movie_lambdas, RMSE = user_movie_rmses) %>%
  ggplot(aes(x = lambda, y = RMSE)) +
  geom_point() +
  ggtitle("User Movies Penalized Effect") +
  theme_minimal()

rmse_usermovie <- min(user_movie_rmses)
lambda_usermovie <- user_movie_lambdas[which.min(user_movie_rmses)]

result <- rbind(result, c(Method = "Regularised User Movie", 
                          RMSE = round(rmse_usermovie,5)))
result %>% knitr::kable(caption="RMSE Result")


###############################################################################
# STEP 4d: REGULARISED ALL VARIANTS, EXCEPT GERNE
all_in_lambdas <- seq(0,10,0.25)

all_in_rmse <- sapply(all_in_lambdas, function(lambda){
  movie_sums <- edx_train %>%
    group_by(movieId) %>%
    summarise(movie_effect = sum(rating-mu) / (n()+lambda)) %>%
    select(movieId,movie_effect)
  
  user_sums <- edx_train %>%
    inner_join(movie_sums, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_effect = sum(rating-mu-movie_effect) / (n()+lambda))
  
  yrrelease_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    group_by(yr_release) %>%
    summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / 
                (n()+lambda))
  
  rellapse_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    group_by(yr_lapse) %>%
    summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-
                                      yrrelease_effect) / (n()+lambda))
  
  yrrate_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    group_by(yr_rating) %>%
    summarise(yrrate_effect = 
                sum(rating-mu-movie_effect-user_effect-
                      yrrelease_effect-rellapse_effect) / 
                (n()+lambda))
  
  predicted_rating <- edx_test %>%
    inner_join(user_sums, by='userId') %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    inner_join(yrrate_sums, by="yr_rating") %>%
    mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + 
             rellapse_effect + yrrate_effect) %>%
    pull(predicted)
  return(RMSE(predicted_rating,edx_test$rating))
})
tibble(lambda = all_in_lambdas, RMSE = all_in_rmse) %>%
  ggplot(aes(x = lambda, y = RMSE)) +
  geom_point() +
  ggtitle("All Variant Penalized Effect") +
  theme_minimal()

rmse_all_in <- min(all_in_rmse)
lambda_all_in <- all_in_lambdas[which.min(all_in_rmse)]

result <- rbind(result, c(Method = "Regularised All Variants", 
                          RMSE = round(rmse_all_in,5)))
result %>% knitr::kable(caption="RMSE Result")




###############################################################################
# STEP 5: FINAL SCRIPT
final_lambda <- 5.0
movie_sums <- edx_train %>%
  group_by(movieId) %>%
  summarise(movie_effect = sum(rating-mu) / (n()+final_lambda)) %>%
  select(movieId,movie_effect)

user_sums <- edx_train %>%
  inner_join(movie_sums, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_effect = sum(rating-mu-movie_effect) / (n()+final_lambda))

yrrelease_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  group_by(yr_release) %>%
  summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / 
              (n()+final_lambda))

rellapse_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  group_by(yr_lapse) %>%
  summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-
                                    yrrelease_effect) / (n()+final_lambda))

yrrate_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  group_by(yr_rating) %>%
  summarise(yrrate_effect = 
              sum(rating-mu-movie_effect-user_effect-yrrelease_effect-
                    rellapse_effect) / (n()+final_lambda))

predicted_rating <- edx_test %>%
  inner_join(user_sums, by='userId') %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  inner_join(yrrate_sums, by="yr_rating") %>%
  mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + 
           rellapse_effect + yrrate_effect) %>%
  pull(predicted)




###############################################################################
# STEP 6: PARSE VALIDATION DATASET

# Prepare yr_release
validation <- validation %>% 
  mutate(title=str_trim(title)) %>% 
  mutate(yr_release=as.numeric(substr(title,str_length(title)-4,
                                      str_length(title)-1)))

# Prepare yr_rating
validation <- validation %>% 
  mutate(yr_rating=year(as_datetime(timestamp,origin="1970-01-01")))

# Prepare & clean yr_lapse
validation <- validation %>% 
  mutate(yr_lapse=yr_rating-yr_release) %>%
  mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))





###############################################################################
# STEP 7: PREDICT VALIDATION DATASET
final_lambda <- 5.0
mu <- mean(edx_train$rating)
movie_sums <- edx_train %>%
  group_by(movieId) %>%
  summarise(movie_effect = sum(rating-mu) / (n()+final_lambda)) %>%
  select(movieId,movie_effect)

user_sums <- edx_train %>%
  inner_join(movie_sums, by="movieId") %>%
  group_by(userId) %>%
  summarise(user_effect = sum(rating-mu-movie_effect) / (n()+final_lambda))

yrrelease_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  group_by(yr_release) %>%
  summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / 
              (n()+final_lambda))

rellapse_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  group_by(yr_lapse) %>%
  summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-
                                    yrrelease_effect) / (n()+final_lambda))

yrrate_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  group_by(yr_rating) %>%
  summarise(yrrate_effect = 
              sum(rating-mu-movie_effect-user_effect-yrrelease_effect-
                    rellapse_effect) / (n()+final_lambda))

predicted_rating <- validation %>%
  inner_join(user_sums, by='userId') %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  inner_join(yrrate_sums, by="yr_rating") %>%
  mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + 
           rellapse_effect + yrrate_effect) %>%
  pull(predicted)

validation_rmse <- RMSE(predicted_rating,validation$rating)




###############################################################################
# STEP FINAL: CLOSING
result <- rbind(result, c(Method = "Validation", RMSE = round(validation_rmse,5)))
result %>% knitr::kable(caption="RMSE Result")





