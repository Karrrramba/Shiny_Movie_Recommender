# pivot into wide format and replace NAs with 0 values
rating_matrix <- ratings_df %>% 
  pivot_wider(names_from = movieId, values_from = rating) %>% 
  mutate(across(everything(), ~replace_na(., 0)))

# movies with mean rating
movies_rated <- rating_matrix %>% 
  pivot_longer(cols = !"userId", names_to = "movieId", values_to = "rating") %>% 
  group_by(movieId) %>% 
  summarise(rating = round(mean(rating, na.rm = TRUE), 1)) %>% 
  ungroup() %>% 
  mutate(movieId = as.integer(movieId))

# load movie annotations
movie_df <-  read.csv("C:/r_projects/shiny_movie_recommender/Shiny_Movie_Recommender/data/movies.csv")
movie_ref <- movie_df %>% 
  mutate(genres =  str_split(genres, pattern = "\\|")) %>% 
  left_join(movies_rated, by = "movieId") %>% 
  filter(!is.na(rating))


# extract the genres into a list for user selection
genre_list <- movie_ref %>% 
  pull(genres) %>% 
  flatten_chr() %>% 
  unique()

# remove "no genres listed"
genre_list <- genre_list[-20]

# load ratings 
ratings_df <- read.csv("C:/r_projects/shiny_movie_recommender/Shiny_Movie_Recommender/data/ratings.csv")
# remove timestamp and filter for movies with at least 10 valid values
ratings_df <- ratings_df %>% 
  select(-timestamp) %>% 
  group_by(movieId) %>% 
  filter(sum(!is.na(rating)) >= 10)%>% 
  ungroup()
