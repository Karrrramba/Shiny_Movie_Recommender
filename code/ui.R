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