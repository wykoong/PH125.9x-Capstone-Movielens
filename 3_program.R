

#----------------------------------
# 1.0 ADD year release
# get year_release
edx_new <- edx %>% mutate(title=str_trim(title)) %>% 
  mutate(yr_release=substr(title,str_length(title)-4,str_length(title)-1))

min(edx_new$yr_release)
max(edx_new$yr_release)

#----------------------------------
# 2.0 Create test data set
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx_new$rating,times = 1, p = 0.1, list = FALSE)
edx_test <- edx_new[test_index]
edx_train <- edx_new[-test_index]

temp <- edx_test
edx_test <- temp %>% 
  semi_join(edx_new, by = "movieId") %>%
  semi_join(edx_new, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(temp,removed)

#----------------------------------
# 3.0 let's get started
# As of the book --->





