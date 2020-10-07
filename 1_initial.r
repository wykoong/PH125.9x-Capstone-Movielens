##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#---------


##########################################################

#-------- INITIAL -------------
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


#----------------------------------
# 1.0 ADD year release & year rating
# 1.1 get year_release
edx <- edx %>% mutate(title=str_trim(title)) %>% 
  mutate(yr_release=as.numeric(substr(title,str_length(title)-4,str_length(title)-1)))

# 1.2 get year_rating
edx <- edx %>% mutate(yr_rating=year(as_datetime(timestamp,origin="1970-01-01")))

#----------------------------------
# 2.0 Create test data set
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating,times = 1, p = 0.1, list = FALSE)
edx_test <- edx[test_index]
edx_train <- edx[-test_index]

temp <- edx_test
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(temp,removed)
count(edx_test)+count(edx_train)

edx_train <- edx_train %>% 
  mutate(yr_release=as.numeric(yr_release)) %>%
  mutate(yr_lapse=yr_rating-yr_release) %>%
  mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))

edx_test <- edx_test %>% 
  mutate(yr_release=as.numeric(yr_release)) %>%
  mutate(yr_lapse=yr_rating-yr_release) %>%
  mutate(yr_lapse=ifelse(yr_lapse<=0,0,yr_lapse))

rm(edx,test_index)



