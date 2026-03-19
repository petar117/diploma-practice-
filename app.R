library(shiny)
library(tidyverse)
library(readtext)
library(tidytext)
library(textstem)
library(wordcloud2)
library(DT)

read_document <- function(filename) {
  readtext::readtext(filename)
}

custom_stopwords <- bind_rows(
  tidytext::stop_words,
  tibble(word = "chapter")
)

tokenize_text <- function(document) {
  tibble::tibble(text = document$text) |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::filter(stringr::str_detect(word, "^[a-z]+$")) |>
    dplyr::anti_join(custom_stopwords, by = "word") |>
    dplyr::mutate(word_lemma = textstem::lemmatize_words(word))
}

count_words <- function(tokens, word_form) {
  selected_column <- switch(
    word_form,
    original = "word",
    lemmatized = "word_lemma",
    "word_lemma"
  )

  tokens |>
    dplyr::count(.data[[selected_column]], sort = TRUE, name = "count") |>
    dplyr::rename(term = 1)
  }



ui <- fluidPage(
  titlePanel("Text Mining App"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Upload a file",
        accept = c(".txt", ".docx", ".pdf")
      ),
      selectInput(
        "word_form",
        "Analyze words as:",
        choices = c(
          "Original words" = "original",
          "Lemmatized words" = "lemmatized"
        ),
        selected = "lemmatized"
      )
    ),

    mainPanel(
      h4("Output will appear here"),
      verbatimTextOutput("file_info")
    )
  )
)

server <- function(input, output, session) {

  document <- reactive({
    req(input$file)
    read_document(input$file$datapath)
  })

  tokens <- reactive({
  req(document())
  tokenize_text(document())
  })

  counts <- reactive({
  req(tokens())
  req(input$word_form)

  count_words(tokens(), input$word_form)
  })

  output$file_info <- renderPrint({
  req(counts())
  head(counts(), 20)
  })

}

shinyApp(ui = ui, server = server)