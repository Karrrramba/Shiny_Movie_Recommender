# NMF troubleshooting

str(movie_ref_table)

# Simulate user input for a genre
genre_selected <- "Action"
user_rated_movies <- movie_ref_table %>%
  filter(.data[[genre_selected]] == 1) %>% 
  arrange(desc(rating)) %>% 
  head(10) %>% 
  select(movieId, title)

print(paste("Length of user_rated_movies$movieId:", length(user_rated_movies$movieId)))
print(paste("Length of user_rated_movies$title:", length(user_rated_movies$title)))
print(paste("Length of user_rated_movies$rating (if exists):", length(user_rated_movies$rating)))

str(user_rated_movies)

# check dimensions of rating_matrix
print(dim(rating_matrix))

# Simulated user ratings for 10 movies
simulated_user_input <- runif(10, 1, 5) 

new_user_ratings <- rep(0, ncol(rating_matrix))
new_user_ratings[user_rated_movies$movieId] <- simulated_user_input

print(paste("Length of new_user_ratings:", length(new_user_ratings)))

print(paste("Number of columns in rating_matrix:", ncol(rating_matrix)))
print(paste("Length of new_user_ratings:", length(new_user_ratings)))

str(rating_matrix)
str(new_user_ratings)

new_user_ratings[user_rated_movies$movieId]
