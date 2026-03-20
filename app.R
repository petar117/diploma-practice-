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
        "Analyze words as",
        choices = c(
          "Original words" = "original",
          "Lemmatized words" = "lemmatized"
        ),
        selected = "lemmatized"
      ),
      sliderInput(
        "top_n",
        "Top N words",
        min = 5,
        max = 50,
        value = 10,
        step = 1
      ),
      sliderInput(
        "cloud_n",
        "Number of words in word cloud",
        min = 10,
        max = 100,
        value = 50,
        step = 10
      )
    ),

    mainPanel(
      h4("Output will appear here"),
      DTOutput("frequency_table"),
      wordcloud2Output("word_cloud", height = "600px")
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

  word_frequencies <- reactive({
  req(tokens())
  req(input$word_form)

  count_words(tokens(), input$word_form)
  })

  output$frequency_table <- renderDT({
  req(word_frequencies())

  datatable(
      word_frequencies(),
      options = list(pageLength = 10),
      colnames = c("Term", "Frequency")
    )
  })

  output$word_cloud <- renderWordcloud2({
  req(word_frequencies())
  req(input$cloud_n)

  cloud_data <- word_frequencies() |>
    dplyr::slice_head(n = input$cloud_n) |>
    dplyr::rename(word = term, freq = count)

  validate(
    need(nrow(cloud_data) > 0, "No words available for the word cloud.")
  )

  wordcloud2::wordcloud2(
    cloud_data,
    size = 0.8,
    color = "random-dark",
    backgroundColor = "#F5F5F5",
  )
})

}

shinyApp(ui = ui, server = server)