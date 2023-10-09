# NMF troubleshooting

str(movie_ref_table)

# Simulate user input for a genre
genre_selected <- "Action"
# create movie recommendation template for user input
user_rated_movies <- movie_ref_table %>%
  filter(.data[[genre_selected]] == 1) %>% 
  arrange(desc(rating)) %>% 
  head(10) %>% 
  select(movieId, title) %>% 
  # create rating column and fill with 0
  mutate(rating = rep(0))

# Simulated user ratings for 10 movies
user_rated_movies$rating <- runif(10, 1, 5) 

user_rated_movies

user_rated_movies_transposed <- user_rated_movies %>%
  select(!title) %>% 
  pivot_wider(names_from = movieId, values_from = rating) %>% 
  mutate(userId = 999)

# check dimensions of rating_matrix
print(dim(rating_matrix))

# create named vector based on rating matrix with UserId 999
new_user_ratings <- setNames(c(999, rep(0, ncol(rating_matrix) -1)), names(rating_matrix))

# attach new_user_ratings vector as new row to rating_matrix.
rating_matrix_updated <- rbind(rating_matrix, new_user_ratings)

# for user-rated movieIDs replace 0s with user ratings
rating_matrix_updated[match(user_rated_movies_transposed$userId, rating_matrix_updated$userId), 
                      match(names(user_rated_movies_transposed), names(rating_matrix_updated))] <- user_rated_movies_transposed

# check if updated correctly
unique(new_user_ratings)
length(new_user_ratings)

print(paste("Length of new_user_ratings:", length(new_user_ratings)))
print(paste("Number of columns in rating_matrix:", ncol(rating_matrix)))





dim(rating_matrix)
dim(rating_matrix_updated)
max(rating_matrix$userId)


# update rating matrix by attaching ratings by the user
rating_matrix <- rbind(rating_matrix, new_user_ratings)

# create nmf-model
nmf_model <- nmf(rating_matrix, 2, method = "brunet", seed = 123)
# extract user-feature matrix
W <- basis(nmf_model)
# extract feature_movie matrix
H <- coef(nmf_model)

# reconstruct rating matrix
approx_rating_matrix <- W %*% H

# predict recommendations for the user
new_user_features <- new_user_ratings %*% solve(W)
# predict movie ratings by the user
new_user_predicted_ratings <- new_user_features %*% H 

print(length(new_user_predicted_ratings))

# Remove already-rated movies from recommendations
rated_movie_indices <- which(!is.na(new_user_ratings))

print(length(rated_movie_indices))

# assign 0 value so these movies will be on the bottom of list after sorting
new_user_predicted_ratings[rated_movie_indices] <- 0

print(length(new_user_predicted_ratings))

# select top 5-rated movie ids as predicted
recommended_movie_ids <- order(new_user_predicted_ratings, decreasing = TRUE)[1:5]

# retrieve movie title from reference table
recommended_movies <- movie_ref_table %>% 
  filter(movieId %in% recommended_movie_ids) %>% 
  pull(title)