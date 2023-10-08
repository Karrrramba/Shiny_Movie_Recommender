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

# check dimensions of rating_matrix
print(dim(rating_matrix))

# 