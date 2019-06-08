###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind = "Rounding") # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################
# This concludes the code given in the excercise instructions #
###############################################################

# Checking out the data file
glimpse(edx)

### Turn genres into factors
edx <- mutate(edx, genres = as.factor(genres))
validation <- mutate(validation, genres = as.factor(genres))

### Testing to see that the genres were changed to factors
class(edx$genres)
class(validation$genres)


# Building the Recommendation System

## Creating the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Getting the mean rating and saving as mu
mu <- mean(edx$rating)
mu

## Buildng the basline predicting the average rating overall, regardless of movie or user.
predictions <- rep(mu, nrow(edx))

### Calculates the R mean squared error of the prediction
base_rmse <- RMSE(edx$rating, predictions)

### Adds the RMSE to the running total
rmse_results <- tibble(method = "Just the average", RMSE = base_rmse)

### prints out the running total table
rmse_results %>% knitr::kable()

### Get average rating delta from the mean, per movie
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

### Checks out the movie ratings by histogram
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"), main = "Average Movie Ratings")


### Adds the mu to the movie delta and predicts rating by movieId
predicted_ratings <- mu + edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

### Calculates the R mean squared error of the prediction
model_1_rmse <- RMSE(predicted_ratings, edx$rating)

### Adds the RMSE to the running total
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

### prints out the running total table
rmse_results %>% knitr::kable()

### checks out the average rating by user
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(x=b_u)) + 
  ggtitle("Distribution of Avg User Ratings") +
  geom_histogram(bins = 30, color = "black")


### Gets the user bias by subracting the modified average rating (the average rating considering the movie title)
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


#### Now it joins the two biases together, calculates the RMSE and adds the results to the table and displays the table.
predicted_ratings <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


## Checking final model against validation set
predicted_ratings_final <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred =(ifelse(mu + b_i + b_u <= 5, mu + b_i + b_u , 5))) %>%
  pull (pred)

### Calculating the RMSE and adding the result to the table
model_3_rmse <- RMSE(validation$rating, predicted_ratings_final)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Final Model against validation",  
                                     RMSE = model_3_rmse ))
## Final Results
rmse_results %>% knitr::kable()




