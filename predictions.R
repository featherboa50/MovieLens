##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#-------------------------------#
# Save myself some time and save the imported data sets locally
library(readr)
library(tidyverse)
library(caret)

write_csv(edx, "./MainData.csv")
write_csv(final_holdout_test, "./FinalHoldout.csv")

#read the datasets
edx <- read.csv("./MainData.csv")
final_holdout_test <- read.csv("./FinalHoldout.csv")
#----------------------------#
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")

library(ggplot2)
#library(GGally)
#create test sets (bootstrap maybe?)
ind <- createDataPartition(edx$timestamp, times = 1, p = .2, list=FALSE)
train <- edx %>% slice(-ind)
test <- edx %>% slice(ind)


#basic model using avg rating for the movie,  rounded to nearest 0.5
#accuracy 0.2256885
avg_ratings_by_movie <- train %>% group_by(movieId) %>% summarize(avg = round(mean(rating)*2)/2)
inner_join(test,avg_ratings_by_movie, by = 'movieId') %>% na.omit() %>%
  summarise(mean(avg == rating))

#basic model using user's avg ratings
#accuracy 0.2049525
avg_ratings_by_user <- train %>% group_by(userId) %>% summarize(avg = round(mean(rating)*2)/2)
inner_join(test,avg_ratings_by_user, by = 'userId') %>% na.omit() %>%
  summarise(mean(avg == rating))


One is using Bayesian Average. Two is using bias or lambda that I learned in Chapter 33.7.5 or Section 6 of the Machine Learning.  I applied bias 3 different ways: userId, movieId and rating.  I did not use a matrix.  

#looking in general for connections, ignoring title and generes for time sake
#took about 20 min using ggally, no new info than what was in the course
#miniInd <- createDataPartition(train$timestamp, times = 1, p = .01, list=FALSE)
#miniset <- train %>% select(-c(title,genres)) %>% slice(miniInd)
#ggpairs(miniset)


#possibly split up generes into their own columns

#train models
#run predictions
#check RMSE
#crossvalidate
#pick best model and refit for all of edx
#final dataset check
#create report file and pdf

