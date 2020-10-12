
# THIS SCRIPT CONTAINS PREDICTION USING REGULARISE

#----------------------------------
# 3.0 let's get started

# from the book, the assumption is based on 
# 1. just the average
# 2. movie effect
# 3. movie + user effect
# .. and follow by ...
# 4. Regularised Movie Effect
# 5. Regularised Movie + User Effect

# The model presented below is skipping the  2 & 3, with hupothesis as these value are itself bias.
# also, with 2, 3 & 4,5 the same effects are risk of double count, thus added unwanted weight to overall prediction.
# however the step 1 is necessary to use the standard value for ongoing analysist ?????


options(dplyr.summarise.inform = FALSE)

# --------------------------------------------------------------------
# get the mu
mu <- mean(edx_train$rating)
mu_rmse <- RMSE(edx_test$rating,mu)

# Naive RMSE is 1.060054
# this is the base RMSe we need to reduce to atleast less than 0.85
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# process no 1 is to get the model the effect by movie 
# as of 33.9 - modeling regularised movie effect

# step 1 get lambda (the penalty term)
# rm(movie_lambdas, movie_sums,movie_rmses,predicted_movie_rating)

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

qplot(movie_lambdas,movie_rmses)
movie_lambdas[which.min(movie_rmses)]

# based on movie_lambdas, the RMSE of movie effect with regularization is
movie_lambda=1.5
movie_rmse=0.942937
# The new RMSE is now 0.942937
# comparing naive rmse, there is some reduction of 0.1171167

# --------------------------------------------------------------------
# process no 2 is to model the user movie effect 
# as of 33.9 - modeling regularised movie effect

# --------------------------------------------------------------------
# now regularised user movie effect

options(dplyr.summarise.inform = FALSE)

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

qplot(user_movie_lambdas,user_movie_rmses)
user_movie_lambdas[which.min(user_movie_rmses)]

# based on above plot, use lambdas = 5.0

# based on lambdas, the RMSE of user moivie effect with regularization is
user_movie_lambda=5.0
user_movie_rmse = 0.8641362

# The new RMSE is now 0.8641362
# Commpare with regularise movie rmse of 0.942937 and naive mu of 1.060054
# There is significant improvement of 0.1959175

# --------------------------------------------------------------------
# process no 3 is to model the release_lapse user movie effect 
# as of 33.9 - modeling regularised release lapse effect

# --------------------------------------------------------------------
# now regularised user movie effects

#----------------- lets get the lambda


rellapse_user_movie_lambdas <- seq(0,10,0.25)

rellapse_user_movie_rmses <- sapply(rellapse_user_movie_lambdas, function(lambda){
  movie_sums <- edx_train %>%
    group_by(movieId) %>%
    summarise(movie_effect = sum(rating-mu) / (n()+lambda)) %>%
    select(movieId,movie_effect)
  
  user_sums <- edx_train %>%
    inner_join(movie_sums, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_effect = sum(rating-mu-movie_effect) / (n()+lambda))
  
  rellapse_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    group_by(yr_lapse) %>%
    summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect) / (n()+lambda))
  
  predicted_rating <- edx_test %>%
    inner_join(user_sums, by='userId') %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    mutate(predicted = mu + user_effect + movie_effect + rellapse_effect) %>%
    pull(predicted)
  return(RMSE(predicted_rating,edx_test$rating))
})

qplot(rellapse_user_movie_lambdas,rellapse_user_movie_rmses)
rellapse_user_movie_lambdas[which.min(rellapse_user_movie_rmses)]

# based on above plot, use lambdas = 5.0
# based on lambdas, the RMSE of year lapse user movie  effect with regularization is
# the rmse by using 5.25 is 0.8636809
rellapse_user_movie_lambda=5.25
rellapse_user_movie_rmse=0.8636809


# --------------------------------------------------------------------
# process no 4 is to model the yr_release user movie effect 
# as of 33.9 - modeling regularised yr release lapse effect


# --------------------------------------------------------------------
# 


#----------------- lets get the lambda

rellapse_yrrelease_user_movie_lambdas <- seq(0,10,0.25)

rellapse_yrrelease_user_movie_rmse <- sapply(rellapse_yrrelease_user_movie_lambdas, function(lambda){
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
    summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / (n()+lambda))
  
  rellapse_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    group_by(yr_lapse) %>%
    summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect) / (n()+lambda))
  
  yrrating_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    group_by(yr_rating) %>%
    summarise(yrrating_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect-rellapse_effect) / (n()+lambda))
  
  predicted_rating <- edx_test %>%
    inner_join(user_sums, by='userId') %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    inner_join(yrrating_sums, by="yr_rating") %>%
    mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + rellapse_effect + yrrating_effect) %>%
    pull(predicted)
  return(RMSE(predicted_rating,edx_test$rating))
})

qplot(rellapse_yrrelease_user_movie_lambdas,rellapse_yrrelease_user_movie_rmse)
rellapse_yrrelease_user_movie_lambdas[which.min(rellapse_yrrelease_user_movie_rmse)]

# based on above plot, use lambdas = 5.0
# based on lambdas, the RMSE of year lapse user movie  effect with regularization is
# the rmse by using 5.25 is 0.8635047
rellapse_yrrelease_user_movie_lambda=5.0
rellapse_yrrelease_user_movie_rmse=0.8635047


# --------------------------------------------------------------------
# process no 5 is to model the yr_rating  effect 
# as of 33.9 - modeling regularised yr rating  effect

# --------------------------------------------------------------------
# 


#----------------- all-IN

options(dplyr.summarise.inform = FALSE)

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
    summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / (n()+lambda))
  
  rellapse_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    group_by(yr_lapse) %>%
    summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect) / (n()+lambda))
  
  yrrate_sums <- edx_train %>%
    inner_join(user_sums, by="userId") %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    group_by(yr_rating) %>%
    summarise(yrrate_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect-rellapse_effect) / (n()+lambda))
  
  predicted_rating <- edx_test %>%
    inner_join(user_sums, by='userId') %>%
    inner_join(movie_sums, by="movieId") %>%
    inner_join(yrrelease_sums, by="yr_release") %>%
    inner_join(rellapse_sums, by="yr_lapse") %>%
    inner_join(yrrate_sums, by="yr_rating") %>%
    mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + rellapse_effect + yrrate_effect) %>%
    pull(predicted)
  return(RMSE(predicted_rating,edx_test$rating))
})

qplot(all_in_lambdas,all_in_rmse)
all_in_lambdas[which.min(all_in_rmse)]

# based on above plot, use lambdas = 5.0
# based on lambdas, the RMSE of year lapse user movie  effect with regularization is
all_in_lambdas=5.0
all_in_rmse=0.8634774

#---------------------- FINAL SCRIPTS

final_lambda <- 5.0
dataset_test <- validation

#--------- IF dataset is validation ------------
dataset_test <- dataset_test %>% mutate(title=str_trim(title)) %>% 
  mutate(yr_release=as.numeric(substr(title,str_length(title)-4,str_length(title)-1)))

dataset_test <- dataset_test %>% mutate(yr_rating=year(as_datetime(timestamp,origin="1970-01-01")))

dataset_test <- dataset_test %>% 
  mutate(yr_release=as.numeric(yr_release)) %>%
  mutate(yr_lapse=yr_rating-yr_release) %>%
  mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))
#-------------- END IF -------------------


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
  summarise(yrrelease_effect = sum(rating-mu-movie_effect-user_effect) / (n()+final_lambda))

rellapse_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  group_by(yr_lapse) %>%
  summarise(rellapse_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect) / (n()+final_lambda))

yrrate_sums <- edx_train %>%
  inner_join(user_sums, by="userId") %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  group_by(yr_rating) %>%
  summarise(yrrate_effect = sum(rating-mu-movie_effect-user_effect-yrrelease_effect-rellapse_effect) / (n()+final_lambda))

predicted_rating <- edx_test %>%
  inner_join(user_sums, by='userId') %>%
  inner_join(movie_sums, by="movieId") %>%
  inner_join(yrrelease_sums, by="yr_release") %>%
  inner_join(rellapse_sums, by="yr_lapse") %>%
  inner_join(yrrate_sums, by="yr_rating") %>%
  mutate(predicted = mu + user_effect + movie_effect + yrrelease_effect + rellapse_effect + yrrate_effect) %>%
  pull(predicted)

RMSE(predicted_rating,dataset_test$rating)
  
# ------------ validation RMSE = 0.8645436

#------------------ LETS reconstruct edx_train using both regularised effect to try out on other approach

# good reference
# https://rpubs.com/vsi/movielens