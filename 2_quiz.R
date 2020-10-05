
#Q1
dim(edx)

#Q2
length(which(edx$rating==0))

length(which(edx$rating==3))


#Q3
length(unique(edx$movieId))
n_distinct(edx$movieId)

#Q4
n_distinct(edx$userId)

#Q5
unique(edx$genres)
sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))

#Q6
edx %>% group_by(movieId,title) %>% summarise(n=n()) %>% arrange(desc(n)) 
edx %>% filter(movieId == 296)

#Q7
edx %>% group_by(rating) %>% summarise(n=n()) %>% 
  mutate(round=(round(rating)==rating)) %>% group_by(round) %>% summarise(rn=sum(n))
  
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()





