# user interface ----
ui <- fluidPage(
  # select a movie genre
  selectInput("genre", "Select a movie genre", choices = genre_list), 
  actionButton("submit_genre", "Submit genre"),
  conditionalPanel(
    condition = "output.showRatingUI",
    uiOutput("rating_ui"),
    #  allow user to select recommender method
    selectInput("method", "Select a recommendation method", 
                choices = c("NMF", "Random", "Vector", "kNN")),
    actionButton("submit_recommendations", "Get recommendations"),
    # display movie recommendations
    tableOutput("recommendations")
  )
)

# server ----
server <- function(input, output, session) {
    
  showRatingUI <- reactiveVal(FALSE)
    
  output$showRatingUI <- reactive({
    showRatingUI()
  })
    
  observeEvent(input$submit_genre, {
    showRatingUI(TRUE)
  })
    
  outputOptions(output, "showRatingUI", suspendWhenHidden = FALSE)
    
  output$rating_ui <- renderUI({
    if(showRatingUI()) {
      genre_selected <- input$genre
      # select the five most-popular movies from the user-selected genre 
      popular_movies_to_rate <- movie_ref_table %>%
        # filter the respective genre-column based on input
        filter(.data[[genre_selected]] == 1) %>% 
        # sort based on ratings
        arrange(desc(rating)) %>%
        # extract top 10 movies
        head(10) %>%
        pull(title)
        
      movie_rating_inputs <- lapply(popular_movies_to_rate, function(movie) {
      # make movie titles HTML-conform by removing spaces
        sanitized_movie_id <- gsub("[^a-zA-Z0-9]", "_", movie)
        numericInput(sanitized_movie_id, label = movie, value = NA, min = 1.0, max = 5.0)
      })
        
      do.call(tagList, movie_rating_inputs)
    }
  })
  # generate recommendations after user prompt
  observeEvent(input$submit_recommendations, {
    
    genre_selected <- input$genre
    # store user input but exclude genre, recommendation method and buttons
    input_data <- sapply(names(input)[-c(11:14)], function(i) input[[i]])
    
    print(input_data)
    
    # retrieve the movies and 
    user_rated_movies <- movie_ref_table %>%
      # filter the respective genre-column based on input
      filter(.data[[genre_selected]] == 1) %>% 
      arrange(desc(rating)) %>% 
      head(10) %>% 
      select(movieId, title) %>% 
      # create rating column and fill with 0
      mutate(rating = rep(0))
    
    user_rated_movies_transposed <- user_rated_movies %>%
      select(!title) %>% 
      pivot_wider(names_from = movieId, values_from = rating) %>% 
      mutate(userId = 999)
    
    # set default rating to 0
    user_ratings <- tibble(title = user_rated_movies$title,
                           rating = 0)
    
    print(user_ratings)
    
    # update ratings according to user inputs
    # create a logical mask indicating which of the popular movies the user has rated
    user_rating_mask <- user_ratings$title %in% names(input_data)
    # override default rating with user rating
    user_ratings$rating[user_rating_mask] <- input_data
    
    # merge tables to attach movieID for recommender algorithms
    user_ratings <- user_ratings %>% 
      left_join(user_rated_movies, by = "title")
    
    print(user_ratings)
    
    # create named vector based on rating matrix with UserId 999
    new_user_ratings <- setNames(c(999, rep(0, ncol(rating_matrix) -1)), names(rating_matrix))
    
    # attach new_user_ratings vector as new row to rating_matrix.
    rating_matrix_updated <- rbind(rating_matrix, new_user_ratings)
    
    # for user-rated movieIDs replace 0s with user ratings
    rating_matrix_updated[match(user_rated_movies_transposed$userId, rating_matrix_updated$userId), 
                          match(names(user_rated_movies_transposed), names(rating_matrix_updated))] <- user_rated_movies_transposed
    
    
    # create empty vector for recommendations
    recommended_movies <- NULL
    
    # conditional recommendations ----
    
    # NMF-based recommendation ----
    if(input$method == "NMF") {
      # create empty vector for new user (NMF does not accept NAs)
      new_user_ratings <- rep(0, ncol(rating_matrix))
      new_user_ratings[user_rated_movies$movieId] <- input_data
      
      print(length(new_user_ratings))
      
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
      
      
    # Random recommendation
    } else if(input$method == "Random") {
      recommendations <- sample(colnames(rating_matrix), 5)
      
    # Cosine Similarity-based recommendation
    } else if(input$method == "Vector") {
      sim <- sim2(as.matrix(rating_matrix), method = "cosine")
      estimated_rating <- as.matrix(user_vector) %*% sim
      recommendations <- names(sort(estimated_rating, decreasing = TRUE))[1:5]
      
    # kNN -based recommendations
    } else if(input$method == "kNN") {
      sim <- sim2(as.matrix(rating_matrix), method = "euclidean")
      nn <- knn(sim, k = 5)
      estimated_rating <- rowMeans(as.matrix(nn), na.rm = TRUE)
      recommendations <- names(sort(estimated_rating, decreasing = TRUE))[1:5]
    }
    
    output$recommendations <- renderTable({
      data.frame(recommendations = recommended_movies)
    })
  })
}

shinyApp(ui = ui, server = server)
