# Shiny_Movie_Recommender

Movie recommender system created in R/shiny.

This app uses the small [MovieLens dataset](https://grouplens.org/datasets/movielens/) (100 000 entries).

Breakdwon of the application:

User chooses a movie genre of interest.

The recommender system prompts the user to rate the five highest-rated movies from the genre.

User then has a choice of four recommender methods:

-   vector: based on cosine similarity

-   random: random choice from the chosen genre

-   nmf: based on Nonnegative Matrix Factorization algorithm

-   cluster: based on k-nearest neighbors clustering

Knwon issues:

-   App freezes and crashes after submitting.

Things to improve/implement:

-   random choice should not require user ratings

-   all other options should prompt user ratings

-   implement clickable word cloud of tags for movie filtering

-   implement publication year selection as filter
