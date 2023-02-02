##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)

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


print(edx$genres[str_detect(edx$genres, "[A-Z][a-z]+|
+            [A-Z][a-z]+\\|+[A-Z][a-z]", negate = TRUE)])
print(edx$title[str_detect(edx$title, "\\s\\(\\d+\\)$", negate = TRUE)])

# temp <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
# edx <- edx[temp,]

# Genres 6, 7, and 8 collectively account for only 0.868% of data

# data.table method to separate genres column is 16.267 times faster than  
# analogous tidyverse method (separate)

setDT(edx)

# Separate genres and calculate number of genres given. Convert genres to 
# factors

edx[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") := 
      tstrsplit(edx$genres, "|", fixed=TRUE, fill = "None", keep = 1:5)]

edx[, n_genres := rowSums(.SD != "None"), .SDcols = 
      c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5")]

edx[, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5") :=
      list(factor(genre_1), factor(genre_2), factor(genre_3), factor(genre_4),
           factor(genre_5))]


# Extract time-stamp data
iso <- as_datetime(edx$timestamp)

edx[, c("ts_year", "ts_month", "ts_day", "ts_hour") := 
      list(year(iso), month(iso), day(iso), hour(iso))]


# Extract movie year from title
m_year <- str_match(edx$title, "\\s\\((\\d+)\\)$")[,2]

edx[, movie_year := year(mdy(paste("1-1-", m_year)))]


# Extract movie era from title
era_len <- 5
edx[, movie_era := floor(as.integer(m_year) / era_len) * era_len]


# Extract user group
group_size = 20
edx[, user_group := ceiling(userId / group_size)]


# Calculate mean bias, standard deviation, and number of reviews for
# all combinations of genre (taking into account genre order) 

lambda = 20
global_mean <- mean(edx$rating)
edx[, unbiased := rating - global_mean]


for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "n_genres",
             "genres", "movie_era", "ts_year", "ts_hour", "movieId", 
             "user_group", "userId")){
  
  edx[, paste0(col, "_nrev") := ceiling(length(unbiased) / 100), by = col]
  
  edx[, paste0(col, "_nrev_bias") := 
        sum(unbiased) / (length(unbiased) + lambda),
      by = eval(paste0(col, "_nrev"))]
  
  edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_nrev_bias")]
  
  edx[, paste0(col, "_bias") := sum(unbiased) / (length(unbiased) + lambda),
      by = col]
  
  edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
  edx[, paste0(col, "_nrev") := NULL]
}


# for(col in c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "n_genres",
#              "genres", "movie_era", "ts_year", "ts_hour", "movieId", 
#              "user_group", "userId")){
#   
#   edx[, paste0(col, "_bias") := 
#         sum(unbiased, na.rm = TRUE) / (sum(!is.na(unbiased)) + lambda),
#       by = col]
#   
#   edx[, unbiased := unbiased - .SD, .SDcols = paste0(col, "_bias")]
# }


# Calculate centered mean rating, standard deviation, and number of reviews for
# all combinations of genre (taking into account genre order) and user

# for(col in c("genres", "genre_1", "genre_2", "genre_3", "genre_4", "genre_5")){
#   edx[, paste0(col, c("_user_avg_rel", "_user_sdev", "_user_nrev")) 
#       := list(mean(rating), sd(rating), length(rating)), by = c("userId", col)]
# }


# Calculate mean rating, standard deviation, and number of reviews
# for each user across all genres

# edx[, c("user_avg_rel", "user_sdev", "user_nrev") := 
#       list(mean(rating), sd(rating), length(rating)), by = userId]


str(edx)
print(edx |> head(15))

rm(dl, ratings, movies, test_index, temp, movielens, removed, iso, m_year)

# temp <- t(edx[, lapply(.SD, function(v){c(sd(v), mean(v))}), .SDcols = colnames(edx[, unbiased:userId_bias])])
# colnames(temp) <- c("sd", "mean")
# temp2 <- temp
# temp2[, 2] <- log(abs(temp2[, 2]))
# as.data.frame(temp2) |> ggplot(aes(sd, mean, label = rownames(temp2))) + geom_point() + geom_text()
# temp
