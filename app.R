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

prepare_stopwords <- function(stopword_input) {
  if (is.null(stopword_input) || !nzchar(stopword_input)) {
    return(tidytext::stop_words)
  }

  stopword_input |> 
    strsplit("\\r?\\n") |>
    unlist(use.names = FALSE) |> 
    stringr::str_trim() |>
    stringr::str_to_lower() -> extra_stopwords

  extra_stopwords <- extra_stopwords[nzchar(extra_stopwords)]

  dplyr::bind_rows(
    tidytext::stop_words,
    tibble::tibble(word = extra_stopwords)
  )
}

tokenize_text <- function(document, custom_stopwords) {
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
      ),
      textAreaInput(
        "custom_words",
        "Extra stopwords (one per line)",
        placeholder = "names\nproject\netc",
        rows = 5
      )
    ),

    mainPanel(
      #h4("Output will appear here"),
      #DTOutput("frequency_table"),
      wordcloud2Output("word_cloud", height = "600px"),
      plotOutput("bar_chart", height = "450px")
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
  tokenize_text(document(), prepare_stopwords(input$custom_words))
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
  
  output$bar_chart <- renderPlot({
    plot_data <- word_frequencies() |> 
      dplyr::slice_head(n = input$top_n) |> 
      mutate(term = reorder(term, count))

    ggplot2::ggplot(plot_data, aes(x = term, y = count)) +
      geom_col(fill = "#1f5c99") +
      coord_flip() +
      labs(
        title = "Most frequent terms",
        x = NULL,
        y = "Count"
      ) +
      theme_minimal(base_size = 13)
  })
  

}

shinyApp(ui = ui, server = server)