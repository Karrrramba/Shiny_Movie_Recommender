# install.packages(c"(NMF", "proxy"))

library(tidyverse)
library(shiny)
library(proxy)
library(NMF)





# create user interface
ui <- fluidPage(
  # first select a genre
  selectInput("genre", "Select a movie genre", choices = genre_list), 
  actionButton("submit_genre", "Submit Genre"),
  conditionalPanel(
    condition = "output.showRatingUI",
    uiOutput("rating_ui"),
    selectInput("method", "Select a recommendation method", choices = c("nmf", "random", "vector", "cluster")),
    actionButton("submit_recommendations", "Get recommendations"),
    tableOutput("recommendations")
  )
)

# server <- function(input, output, session) {
#   output$rating_ui <- renderUI({
#     genre_selected <- input$genre
#     #  select popular movies in the selected genres
#     popular_movies <- movie_ref %>%
#       # filter movies for selected genre 
#       filter(map_lgl(genres, ~genre_selected%in% genres)) %>%
#       # select the top 5 highest-ranking
#       arrange(desc(rating)) %>% 
#       head(5) %>%
#       pull(title)
#     
#     movie_rating_inputs <- lapply(popular_movies, function(movie) {
#       numericInput(movie, label = movie, value = NA, min = 1.0, max = 5.0)
#     })
#     do.call(tagList, movie_rating_inputs)
#   })
#   
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
      popular_movies <- movie_ref %>%
        filter(map_lgl(genres, ~genre_selected %in% .)) %>%
        arrange(desc(rating)) %>% 
        head(5) %>%
        pull(title)
        
      # make movie titles HTML-conform by removing spaces
      movie_rating_inputs <- lapply(popular_movies, function(movie) {
        sanitized_movie_id <- gsub("[^a-zA-Z0-9]", "_", movie)
        numericInput(sanitized_movie_id, label = movie, value = NA, min = 1.0, max = 5.0)
      })
        
      do.call(tagList, movie_rating_inputs)
    }
  })
  observeEvent(input$submit_recommendatyions, {
    input_data <- sapply(names(input)[-1:-4], function(i) input[[i]])
    user_vector <- as.numeric(input_data)
    names(user_vector) <- names(input)[-1:-4]
    
    # create empty vecotr for recommendations
    recommendations <- NULL
    
    # conditional recommendations
    if(input$method == "nmf") {
      # NMF-based recommendation
      nmf_model <- nmf(rating_matrix, 2)
      W <- basis(nmf_model)
      H <- coef(nmf_model)
      new_user <- as.matrix(user_vector) %*% solve(W)
      estimated_rating <- new_user %*% H
      recommendations <- names(sort(estimated_rating, decreasing = TRUE))[1:5]
      
    } else if(input$method == "random") {
      # Random recommendation
      recommendations <- sample(colnames(rating_matrix), 5)
      
    } else if(input$method == "vector") {
      # Cosine Similarity- based recommendation
      sim <- sim2(as.matrix(rating_matrix), method = "cosine")
      estimated_rating <- as.matrix(user_vector) %*% sim
      recommendations <- names(sort(estimated_rating, decreasing = TRUE))[1:5]
      
    } else if(input$method == "cluster") {
      # kNN -based recommendations
      sim <- sim2(as.matrix(rating_matrix), method = "euclidean")
      nn <- knn(sim, k = 5)
      estimated_rating <- rowMeans(as.matrix(nn), na.rm = TRUE)
      recommendations <- names(sort(estimated_rating, decreasing = TRUE))[1:5]
    }
    
    output$recommendations <- renderTable({
      data.frame(Recommended_Movies = recommendations)
    })
  })
}

shinyApp(ui = ui, server = server)
