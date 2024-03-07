nrow(edx)
ncol(edx)

# of ratings 0 or 3
edx %>% filter(rating %in% c(0,3)) %>% summarize(zero = sum(rating == 0),
                                                 three = sum(rating == 3))
# of movies
n_distinct(edx$movieId)
# of users
n_distinct(edx$userId)

# of dramas/comedies/thrillers/romances
edx %>% summarize(d = sum(str_detect(genres,"Drama")), 
                    c = sum(str_detect(genres,"Comedy")), 
                    t = sum(str_detect(genres,"Thriller")), 
                    r = sum(str_detect(genres,"Romance")))
#top # of ratings
edx %>% group_by(title) %>% summarise(n = n()) %>% arrange(desc(n))

#top ratings
edx %>% group_by(rating) %>%
  summarize(n = n()) %>% arrange(desc(n))

edx %>% group_by(rating) %>%
  summarize(n = n(), half = (rating == round(rating))) %>%
  group_by(half) %>% summarize(sum(n))
