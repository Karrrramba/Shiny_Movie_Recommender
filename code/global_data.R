# dfs for basic recommender ----

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
movie_df <-  read.csv(".data/movies.csv")

movie_ref_table <- movie_df %>% 
  # transform genres into separate columns
  dummy_columns(select_columns = "genres", split = "\\|", 
                remove_selected_columns = TRUE) %>%
  rename_with(., ~ gsub("genres_", "", .), .cols = starts_with("genres")) %>%
  # remove all entries withou genre
  filter("(no genres listed)" != 1) %>% 
  select(!"(no genres listed)") %>% 
  # add movie ratings
  left_join(movies_rated, by = "movieId") %>% 
  # remove entries without ratings
  filter(!is.na(rating))

glimpse(movie_ref_table)
# extract the genres into a list for user selection
genre_list <- movie_ref %>% 
  pull(genres) %>% 
  flatten_chr() %>% 
  unique()


# remove "no genres listed"
genre_list <- genre_list[-20]

movie_ref %>% 
  rowwise() %>% 
  summarise(len = length(genres)) %>% 
  ungroup() %>% 
  max()
# create dummy variables for movie genres?

# load ratings 
ratings_df <- read.csv("./data/ratings.csv")
# remove timestamp and filter for movies with at least 10 valid values
ratings_df <- ratings_df %>% 
  select(-timestamp) %>% 
  group_by(movieId) %>% 
  filter(sum(!is.na(rating)) >= 10)%>% 
  ungroup()




