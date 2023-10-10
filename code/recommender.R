# Definne user interface ----
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
  
  # make sanitized_titles available outside the renderUI block
  sanitized_titles <- reactiveVal()
  
  observeEvent(input$submit_genre, {
    showRatingUI(TRUE)
  })
    
  outputOptions(output, "showRatingUI", suspendWhenHidden = FALSE)
    
  output$rating_ui <- renderUI({
    if(showRatingUI()) {
      genre_selected <- input$genre
      # select the five most-popular movies from the user-selected genre 
      popular_movies <- movie_ref_table %>%
        # filter the respective genre-column based on input
        filter(.data[[genre_selected]] == 1) %>% 
        # sort based on ratings
        arrange(desc(rating)) %>%
        # extract top 10 movies
        head(10) %>%
        select(title, movieId)
      
      
      # remove special characters from titles
      san_titles <- setNames(popular_movies$title, 
                                   sapply(popular_movies$title, function(mov_title) gsub("[^a-zA-Z0-9]", "_", mov_title)))
      sanitized_titles(san_titles)
      
      movie_rating_inputs <- lapply(names(san_titles), function(san_title) {
        # map sanitized movie title to original title
        original_title <- san_titles[[san_title]]
        # input uses sanitized titles but GUI displays original titles
        numericInput(san_title, label = original_title, value = NA, min = 1.0, max = 5.0)
      })
      
      do.call(tagList, movie_rating_inputs)
    }
  })
  # Extract user inputs and attach user-ratings to the rating matrix----
  observeEvent(input$submit_recommendations, {
    
    genre_selected <- input$genre
    # extract movie titles and ratings from input
    input_data <- sapply(names(input)[-c(11:14)], function(i) input[[i]])
    
    original_titles <- sanitized_titles()[names(input_data)]
    
    # match original titles and ratings
    og_titles_rated <- tibble(san_title = names(input_data),
                              rating = as.numeric(input_data),
                              og_title = unname(original_titles))
    
    og_titles_rated <- og_titles_rated%>%
      select(-san_title) %>% 
      mutate(rating = replace_na(rating, 0))
    
    
    # 
    user_rated_movies <- movie_ref_table %>%
      # filter the respective genre-column based on input
      filter(.data[[genre_selected]] == 1) %>% 
      arrange(desc(rating)) %>% 
      head(10) %>% 
      select(movieId, title) %>% 
      # attach ratings by matching titles
      left_join(og_titles_rated, by = join_by(title == og_title)) %>% 
      select(!title) %>% 
      # transpose
      pivot_wider(names_from = movieId, values_from = rating) %>% 
      # create userId
      mutate(userId = 999)
    
    print("user_rated_movies")
    print(user_rated_movies)
    
    # Now we will create an empty vector, which we will populate with the user-given ratings and use to expand the rating matrix with.
    # create empty vector with all movieIds (just like the rating matrix) and rating = 0 
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
