
library(tidyverse)
library(caret)
library(data.table)


install.packages("rrecsys")
library(rrecsys)

names(edx_test)
edx_test_min <- edx_test %>% select(userId,movieId, rating, yr_release,yr_rating,yr_lapse)

recML <- defineData(as.matrix(edx_test), minimum = .5, maximum = 5, intScale = TRUE)
recML

?defineData

data(mlLatest100k)


dim(mlLatest100k)


mlLatest100k[1:10,1:10]
