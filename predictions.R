##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "https://cran.rstudio.com/")
if(!require(caret)) install.packages("caret", repos = "https://cran.rstudio.com/")
if(!require(kamila)) install.packages("kamila", repos = "https://cran.rstudio.com/")
if(!require(recipes)) install.packages("kamila", repos = "https://cran.rstudio.com/")

library(splitstackshape)
library(tidyverse)
library(caret)
library(ggplot2)
library(kamila)
library(readr)
library(recipes)
library(lubridate)

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

# ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
#                         stringsAsFactors = FALSE)
# had to adjust due to fixed() erroring out
ratings <- as.data.frame(str_split(read_lines(ratings_file), coll("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
#                        stringsAsFactors = FALSE)
movies <- as.data.frame(str_split(read_lines(movies_file), coll("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

#ADDED RELEASE YEAR and time since release to time of rating to both sets
movielens <- left_join(ratings, movies, by = "movieId") %>%
  mutate(release_year = as.numeric(str_sub(title,-5,-2)),
         years_since = ifelse(year(timestamp) - release_year <= 0, 0, year(timestamp) - release_year))

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

rm(dl, ratings, movies, test_index, temp, movielens, removed, movies_file, ratings_file)

# #-------------------------------#
# # Save myself some time and save the imported data sets locally
# library(readr)
# library(tidyverse)
# library(caret)
# 
# write_csv(edx, "./MainData.csv")
# write_csv(final_holdout_test, "./FinalHoldout.csv")
# 
# #read the datasets
 # edx <- read.csv("./MainData.csv")
# final_holdout_test <- read.csv("./FinalHoldout.csv")
# #--------------------------------------------------------------------#


set.seed(2006)
indexes <- split(1:nrow(edx), edx$userId) #creates sets for each user
test_ind <- sapply(indexes, function(ind) sample(ind, ceiling(length(ind)*.2))) %>%
  unlist(use.names = TRUE) %>% sort()  #takes 20% from each user for testing set

training <- edx[-test_ind,]
temp <- edx[test_ind,]

# Make sure same movieId in testing and training
testing <- temp %>% 
  semi_join(training, by = "movieId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, testing)
training <- rbind(training, removed)

# temp2 <- anti_join(training, testing, by = "movieId") 
# removed2 <- anti_join(temp2, training)
# testing <- rbind(testing, removed2)

# testing <- edx[test_ind,]
# training <- edx[-test_ind,]


#Years since release date for ones with over 100 ratings
training %>% group_by(years_since) %>% filter(n() > 100)%>%
  summarise(mean = mean(rating)) %>% ggplot(aes(years_since,mean)) +geom_point() +geom_smooth(span = 0.5, se = FALSE)


# 
# #making sure both training and test sets have the same movieids
# training <- training %>%
#   semi_join(testing, by = "movieId")
# testing <- testing %>%
#   semi_join(training, by = "movieId") 

#dummy coding, takes a few min for each
#training <- cSplit_e(training, "genres", "|", type = "character", fill = 0, drop = F)
#testing <- cSplit_e(testing, "genres", "|", type = "character", fill = 0, drop = F)

#root mean squared error
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#base model using the avg for all ratings
#3.512043
#RMSE 1.060369
mu <- training %>% summarize(m = mean(rating)) %>% pull(m)
mu
RMSE(testing$rating, mu)

#basic model using avg rating for the movie
#RMSE 0.94
avg_ratings_by_movie <- training %>% group_by(movieId) %>% 
  summarize(b_i = mean(rating) - mu,
            n = n(),
            sums = sum(rating - mu))
inner_join(testing,avg_ratings_by_movie, by = 'movieId') %>% 
  summarize(RMSE = RMSE(rating, b_i + mu))


#basic model using avg rating by user
#RMSE 0.98
avg_ratings_by_user <- training %>% group_by(userId) %>% 
  summarize(b_u = mean(rating) - mu,
            n = n(),
            sums = sum(rating - mu))
inner_join(testing, avg_ratings_by_user, by = 'userId') %>% 
  summarize(RMSE = RMSE(rating, b_u + mu)) 

#basic model using avg rating by genre groups
#RMSE 1.02
avg_ratings_by_genre <- training %>% group_by(genres) %>% 
  summarize(b_g = mean(rating) - mu,
            n = n(),
            sums = sum(rating - mu))
inner_join(testing, avg_ratings_by_genre, by = 'genres') %>% 
  summarize(RMSE = RMSE(rating, b_g + mu))


#basic model using avg rating by release year
# RSME 1.05
avg_ratings_by_release_year <- training %>%
  group_by(release_year) %>% 
  summarize(n = n(),
            sums = sum(rating - mu))


#movie + user bias
#RMSE 0.887
left_join(testing, avg_ratings_by_movie, by = "movieId") %>%
  left_join(avg_ratings_by_user, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>% 
  summarize(rmse = RMSE(rating, pred))


#finding the best lambda for smoothing movie effect
lambdas <- seq(0, 3, 0.05)
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_movie$b_i <- avg_ratings_by_movie$sums / (avg_ratings_by_movie$n + lambda)
  left_join(testing, avg_ratings_by_movie, by = "movieId") %>% mutate(pred = mu + b_i) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized movie effect
#best lambda 2
qplot(lambdas, rmses, geom = "line")
lambda_i <- lambdas[which.min(rmses)]
print(lambda_i)
avg_ratings_by_movie$b_i <- avg_ratings_by_movie$sums / (avg_ratings_by_movie$n + lambda_i)
training_reg <- left_join(training,avg_ratings_by_movie, by = "movieId") %>%
  select(-n,-sums)


#adjust b_u to new b_i
avg_ratings_by_user <- training_reg %>%
  mutate(x = rating - mu - b_i) %>%
  group_by(userId) %>% 
  summarize(b_u = mean(x),
            n = n(),
            sums = sum(x))

#reg movie + user effect effect
#RMSE 0.867
user_movie_model <- left_join(testing, avg_ratings_by_movie, by = "movieId")
#gc() #frees up memory for next command, takes a couple min
left_join(user_movie_model, avg_ratings_by_user, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>%
  summarize(rmse = RMSE(rating, pred))



lambdas <- seq(3, 7, 0.25)
#avg_ratings_by_user$b_u = 0
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_user$b_u <- avg_ratings_by_user$sums / (avg_ratings_by_user$n + lambda)
  left_join(testing, avg_ratings_by_user, by = "userId") %>%
    left_join(avg_ratings_by_movie, by = "movieId") %>%
    mutate(pred = mu + b_u + b_i) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized user effect
#best lambda 4.75
qplot(lambdas, rmses, geom = "line")
lambda_u <- lambdas[which.min(rmses)]
print(lambda_u)
avg_ratings_by_user$b_u <- avg_ratings_by_user$sums / (avg_ratings_by_user$n + lambda_u)
#RMSE 0.866698
left_join(user_movie_model, avg_ratings_by_user, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>%
  summarize(rmse = RMSE(rating, pred))

training_reg <- left_join(training_reg, avg_ratings_by_user, by = "userId") %>%
  select(-n,-sums)


#adj genres for bi&bu before regularization
avg_ratings_by_genre <- training_reg %>%
  mutate(x = rating - mu - b_i - b_u) %>%
  group_by(genres) %>% 
  summarize(n = n(),
            sums = sum(x))

lambdas <- seq(24, 28, 0.25)
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_genre$b_g <- avg_ratings_by_genre$sums / (avg_ratings_by_genre$n + lambda)
  left_join(testing, avg_ratings_by_genre, by = "genres") %>%
    left_join(avg_ratings_by_movie, by = "movieId") %>% 
    left_join(avg_ratings_by_user, by = "userId") %>%
    mutate(pred = mu + b_u + b_i + b_g) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized genre groups effect
qplot(lambdas, rmses, geom = "line")
lambda_g <- lambdas[which.min(rmses)]
print(lambda_g)
avg_ratings_by_genre$b_g <- avg_ratings_by_genre$sums / (avg_ratings_by_genre$n + lambda_g)
#RMSE 0.86
left_join(training_reg, avg_ratings_by_genre, by = "genres") %>% 
  mutate(pred = mu + b_i + b_u + b_g) %>%
  summarize(rmse = RMSE(rating, pred))

training_reg <- left_join(training_reg, avg_ratings_by_genre, by = "genres") %>%
  select(-n,-sums)



#adj genres for bi&bu before regularization
avg_ratings_by_release_year <- training_reg %>%
  mutate(x = rating - mu - b_i - b_u - b_g) %>%
  group_by(release_year) %>% 
  summarize(n = n(),
            sums = sum(x))

lambdas <- seq(-18, -14, .25)
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_release_year$b_r <- avg_ratings_by_release_year$sums / (avg_ratings_by_release_year$n + lambda)
  left_join(testing, avg_ratings_by_release_year, by = "release_year") %>%
    left_join(avg_ratings_by_movie, by = "movieId") %>% 
    left_join(avg_ratings_by_user, by = "userId") %>% 
    left_join(avg_ratings_by_genre, by = "genres") %>%
    mutate(pred = mu + b_u + b_i + b_g + b_r) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized release year effect
#best lambda -0.5
qplot(lambdas, rmses, geom = "line")
lambda_r <- lambdas[which.min(rmses)]
print(lambda_r)
avg_ratings_by_release_year$b_r <- avg_ratings_by_release_year$sums / (avg_ratings_by_release_year$n + lambda_r)
#RMSE 0.8554427
left_join(training_reg, avg_ratings_by_release_year, by = "release_year") %>% 
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
  summarize(rmse = RMSE(rating, pred))

training_reg <- left_join(training_reg, avg_ratings_by_release_year, by = "release_year") %>%
  select(-n,-sums)



#adj genres for bi&bu before regularization
avg_ratings_by_rating_time <- training_reg %>%
  mutate(x = rating - mu - b_i - b_u - b_g - b_r) %>%
  group_by(years_since) %>% 
  summarize(n = n(),
            sums = sum(x))

lambdas <- seq(99, 102, 0.25)
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_rating_time$b_y <- avg_ratings_by_rating_time$sums / (avg_ratings_by_rating_time$n + lambda)
  left_join(testing, avg_ratings_by_rating_time, by = "years_since") %>%
    left_join(avg_ratings_by_movie, by = "movieId") %>% 
    left_join(avg_ratings_by_user, by = "userId") %>% 
    left_join(avg_ratings_by_genre, by = "genres") %>%
    left_join(avg_ratings_by_release_year, by = "release_year") %>%
    mutate(pred = mu + b_u + b_i + b_g + b_r + b_y) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized release year effect
#best lambda -0.5
qplot(lambdas, rmses, geom = "line")
lambda_y <- lambdas[which.min(rmses)]
print(lambda_y)
avg_ratings_by_rating_time$b_y <- avg_ratings_by_rating_time$sums / (avg_ratings_by_rating_time$n + lambda_y)





#NEED TO REDO WITH THE FULL EDX set 
#----------------------final check--------------------------


x <- left_join(final_holdout_test, avg_ratings_by_movie, by = "movieId") %>%
  select(-n,-sums) %>%
  left_join(avg_ratings_by_user, by = "userId") %>%
  select(-n,-sums) %>%
  left_join(avg_ratings_by_genre, by = "genres") %>%
  select(-n,-sums) %>%
  left_join(avg_ratings_by_release_year, by  = "release_year") %>%
  select(-n,-sums) %>%
  left_join(avg_ratings_by_rating_time, by  = "years_since") %>%
  select(-n,-sums) %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r + b_y) 

x %>% summarize(RMSE(rating, pred))







#--------------HERE------------------------------

#dummy coding, takes a few min for each
dummy_training <- cSplit_e(training, "genres", "|", type = "character", fill = 0, drop = F)
dummy_testing <- cSplit_e(testing, "genres", "|", type = "character", fill = 0, drop = F)



#genre groups
avg_ratings_by_genre_group <- dummy_training %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating-mu),
            n=n(),
            sums = sum(rating-mu)) #%>% arrange(desc(n))
#regularizing
lambdas <- seq(2, 5, 0.05)
avg_ratings_by_genre_group$b_g_reg = 0
rmses <- sapply(lambdas, function(lambda){
  avg_ratings_by_genre_group$b_g_reg <- avg_ratings_by_genre_group$sums / (avg_ratings_by_genre_group$n + lambda)
  left_join(testing, avg_ratings_by_genre_group, by = "genres") %>% mutate(pred = mu + b_g_reg) %>% 
    summarize(rmse = RMSE(rating, pred)) %>%
    pull(rmse)
})
#regularized genre effect
#best lambda 3.75
qplot(lambdas, rmses, geom = "line")
lambda_g <- lambdas[which.min(rmses)]
print(lambda_g)
avg_ratings_by_genre_group <- avg_ratings_by_genre_group 
avg_ratings_by_genre_group$b_g_reg <- avg_ratings_by_genre_group$sums / (avg_ratings_by_genre_group$n + lambda_g)

combo_model <- left_join(user_movie_model, avg_ratings_by_user, by = "userId") %>% 
  left_join(avg_ratings_by_genre_group, by = "genres")

#-------------------------------------------------



#cooking with gas
my_recipe <- recipe(rating ~ ., data = training) %>%
  update_role(title, new_role = "ID") %>%
  update_role(genres, new_role = "ID")   #sets the title feature to be kept but not used for modeling
#  step_dummy(genres, one_hot = TRUE)  #One Hot encoding the genres
  
  my_recipe
  summary(my_recipe) 


set.seed(888)
#RMSE .917 9:10
start <- sys.time()
train_svm <- train(my_recipe, training,
                 method = "svmRadial", 
                 metric = "wRMSE",
                 maximize = FALSE,
                 tuneLength = 10)
end <- sys.time()
train_svm
#RMSE 1.09 example of overfitting
RMSE(predict(train_svm, testing), testing$rating)
print(format(start, %H:%M) & " " & format(end, %H:%M))


#RMSE .975
b <- 5
control <- trainControl(method = "cv", number = b, p = .9)
train_Loess <- train(my_recipe, training,
                 method = "gamLoess",
                 #tuneGrid = data.frame(k = seq(1,60,2)),
                 trControl = control)

train_Loess
RMSE(predict(train_Loess, minitest), minitest$rating)


my_recipe2 <- my_recipe %>%
  step_pca() 
train_Loess2 <- train(my_recipe2, minitrain,
                     method = "gamLoess",
                     tuneGrid = grid,
                     trControl = control)

train_Loess2
RMSE(predict(train_Loess2, minitest), minitest$rating)


fit.treebag <- train(full_recipe, training,
                      method = "treebag",
                     # tuneGrid = data.frame(k = seq(1,60,2)),
                      trControl = control)
fit.rf <- train(full_recipe,training, method="rf", trControl=control)
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
RMSE(predict(fit.rf, minitest), minitest$rating)


  
minitrain %>% mutate(pred = mu + b_i + b_u) %>% 
    summarize(rmse = RMSE(rating, pred))
#gower
ames_full <- minitrain %>%
  mutate_if(str_detect(names(.), 'Qual|Cond|QC|Qu'), as.numeric)

# One-hot encode --> retain only the features and not sale price
full_rank  <- caret::dummyVars(rating ~ . - title, data = minitrain_dummy, 
                               fullRank = TRUE)
ames_1hot <- predict(full_rank, minitrain_dummy)

# Scale data
ames_1hot_scaled <- scale(ames_1hot)

# New dimensions
dim(ames_1hot_scaled)
## [1] 2930  240
gower_dst <- daisy(minitrain, metric = "gower")

train_knn_2$bestTune
plot(train_knn_2)
RMSE(predict(train_knn_2, minitest), minitest$rating)

#One is using Bayesian Average. Two is using bias or lambda that I learned in Chapter 33.7.5 or Section 6 of the Machine Learning.  I applied bias 3 different ways: userId, movieId and rating.  I did not use a matrix.  







y <- select(train_set, movieId, userId, rating) %>%
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames
movie_map <- train_set %>% select(movieId, title) %>% distinct(movieId, .keep_all = TRUE)

naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse
#movie bias
b_i <- colMeans(y - mu, na.rm = TRUE)
qplot(b_i, bins = 10, color = I("black"))
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)
left_join(test_set, fit_movies, by = "movieId") %>% 
  mutate(pred = mu + b_i) %>% 
  summarize(rmse = RMSE(rating, pred))
#user bias
b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)
fit_users <- data.frame(userId = as.integer(rownames(y)), b_u = b_u)
#combining user and movie bias
left_join(test_set, fit_movies, by = "movieId") %>% 
  left_join(fit_users, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  summarize(rmse = RMSE(rating, pred))



#------------------
# # Kamila method
# # https://www.nature.com/articles/s41598-021-83340-8
# # https://cran.r-universe.dev/kamila/doc/manual.html
# # 
set.seed(52)
kamind <- createDataPartition(edx$userId, p = 0.7, list = FALSE)
kamtrain <- edx[kamind,]
kamtest <- edx[-kamind,]


kamtest <- kamtest %>%
  semi_join(kamtrain, by = "movieId") %>%
  mutate(release_year = as.numeric(str_sub(title,-5,-2)))
kamtrain <- kamtrain %>%
  semi_join(kamtest, by = "movieId") %>%
  mutate(release_year = as.numeric(str_sub(title,-5,-2)))

kamtrain_dummy <- cSplit_e(kamtrain, "genres", "|", type = "character", fill = 0, drop = F)
kamtest_dummy <- cSplit_e(kamtest, "genres", "|", type = "character", fill = 0, drop = F)

#continous variables
conInd <- c(4,7)
conVars <- kamtrain[,conInd]
#conVars <- data.frame(scale(conVars))
conTest <- kamtest[,conInd]

#categorical variables
catInd <- c(1,2,7:25) #dummy variables
catInd2 <- c(1,2,6) #genre groups in tact
catVarsFac <- kamtrain_dummy[,catInd]
catVarsFac[] <- lapply(catVarsFac, factor)
catVar2 <- data.frame(lapply(kamtrain[,catInd2],as.factor))
#catVarsDum <- dummyCodeFactorDf(catVar2)

catTestFac <- kamtrain_dummy[,catInd]
catTestFac[] <- lapply(catTestFac, factor)
catTest2 <- data.frame(lapply(kamtest[,catInd2],as.factor))
#catTestDum <- dummyCodeFactorDf(testcatVarsFac)




# KAMILA clustering 13 min
kamRes <- kamila(conVars, catVar2, numClust=3, numInit=10)
kamPred <- classifyKamila(kamRes, list(conTest, catTest2))
table(kamtest$rating, kamPred)

# Plot KAMILA results
plottingData <- cbind(
  conVars,
  catVar2,
  KamilaCluster = factor(kamRes$finalMemb))
plottingData$Bone.metastases <- ifelse(
  plottingData$Bone.metastases == '1', yes='Yes',no='No')

ggplot(
  plottingData,
  aes(
    x=logSpap,
    y=Index.of.tumour.stage.and.histolic.grade,
    color=ternarySurvival,
    shape=KamilaCluster)) + geom_point()

rm(kamtest, kamtest_dummy, kamtrain, kamtrain_dummy,
   testcatVarsFac, kamind, kamRes, catVarsFac, conTest, conVars)

# #-------------------


