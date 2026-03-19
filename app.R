library(shiny)

ui <- fluidPage(
  titlePanel("Text Mining App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a document")
    ),

    mainPanel(
      h4("Output will appear here")
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)