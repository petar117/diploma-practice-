library(shiny)
library(shinydashboard)
library(tidyverse)
library(readtext)
library(tidytext)
library(textstem)
library(wordcloud2)
library(DT)
library(topicmodels)

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

split_document <- function(document, segment_size) {
  words <- document$text |> 
    stringr::str_squish() |> 
    strsplit("\\s+") |> 
    unlist(use.names = FALSE)

  tibble::tibble(
    word = words,
    segment_id = paste0("Segment ", ceiling(seq_along(words) / segment_size))
  ) |> 
    dplyr::group_by(segment_id) |> 
    dplyr::summarise(
      text = paste(word, collapse = " "),
      .groups = "drop"
    )
}

tokenize_segments <- function(segments, custom_stopwords) {
  segments |> 
    tidytext::unnest_tokens(word, text) |> 
    dplyr::filter(stringr::str_detect(word, "^[a-z]+$")) |>
    dplyr::anti_join(custom_stopwords, by = "word") |>
    dplyr::mutate(word_lemma = textstem::lemmatize_words(word))
}

tokenize_bigrams <- function(segments, custom_stopwords) {
  segments |>
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
    tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |>
    dplyr::filter(
      stringr::str_detect(word1, "^[a-z]+$"),
      stringr::str_detect(word2, "^[a-z]+$")
    ) |>
    dplyr::filter(
      !word1 %in% custom_stopwords$word,
      !word2 %in% custom_stopwords$word
    ) |>
    dplyr::mutate(term = paste(word1, word2))
}

get_selected_column <- function(word_form) {
  switch(word_form,
    original = "word",
    lemmatized = "word_lemma",
    "word_lemma"
  )
}

count_words <- function(tokens, word_form) {
  selected_column <- get_selected_column(word_form)

  tokens |>
    dplyr::count(.data[[selected_column]], sort = TRUE, name = "count") |>
    dplyr::rename(term = 1)
}

count_bigrams <- function(bigrams) {
  bigrams |>
    dplyr::count(term, sort = TRUE, name = "count")
}

count_segment_terms <- function(tokens, word_form) {
  selected_column <- get_selected_column(word_form)

  tokens |>
    dplyr::count(segment_id, term = .data[[selected_column]], sort = FALSE, name = "count")
}

create_dtm <- function(document_segments) {
  tidytext::cast_dtm(
    document_segments,
    document = segment_id,
    term = term,
    value = count
  )
}

create_topics <- function(lda_model, terms_per_topic) {
  beta_matrix <- topicmodels::posterior(lda_model)$terms

  purrr::map_dfr(seq_len(nrow(beta_matrix)), function(topic_id) {
    topic_scores <- beta_matrix[topic_id, ]

    top_index <- order(topic_scores, decreasing = TRUE)[
      seq_len(min(terms_per_topic, length(topic_scores)))
    ]

    tibble::tibble(
      topic = paste("Topic", topic_id),
      term = names(topic_scores)[top_index],
      beta = round(as.numeric(topic_scores[top_index]), 4)
    )
  })
}

create_topic_summary <- function(topics) {
  topics |>
    dplyr::group_by(topic) |>
    dplyr::summarise(
      top_words = paste(term, collapse = ", "),
      .groups = "drop"
    )
}

get_segment_recommendation <- function(total_tokens) {
  if (total_tokens < 500) {
    return("80-120 words")
  }

  if (total_tokens < 1000) {
    return("120-180 words")
  }

  "180-250 words"
}

ui <- dashboardPage(
  dashboardHeader(title = "Text Mining App #1"),
  dashboardSidebar(
    width = 320,
    tags$div(
      style = "padding: 12px;",
      h4("Document"),
      fileInput(
        "file",
        "Upload a file",
        accept = c(".txt", ".docx", ".pdf")
      ),
      tags$hr(),
      h4("Preprocessing"),
      selectInput(
        "word_form",
        "Analyze words as",
        choices = c(
          "Original words" = "original",
          "Lemmatized words" = "lemmatized"
        ),
        selected = "lemmatized"
      ),
      selectInput(
        "token_type",
        "Analyze text as",
        choices = c(
          "Single words" = "unigram",
          "Two-word phrases" = "bigram"
        ),
        selected = "unigram"
      ),
      textAreaInput(
        "custom_words",
        "Extra stopwords",
        placeholder = "names\nproject\netc",
        rows = 4
      ),
      tags$hr(),
      conditionalPanel(
        condition = "input.main_tabs == 'Overview'",
        h4("Frequency Options"),
        sliderInput(
          "top_n",
          "Top N words",
          min = 5,
          max = 50,
          value = 10,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Word Cloud'",
        h4("Word Cloud Options"),
        sliderInput(
          "cloud_n",
          "Words in cloud",
          min = 10,
          max = 100,
          value = 50,
          step = 10
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Topic Modeling'",
        h4("Topic Modeling Options"),
        sliderInput(
          "segment_size",
          "Words per segment",
          min = 80,
          max = 320,
          value = 200,
          step = 20
        ),
        selectInput(
          "num_topics",
          "Number of topics",
          choices = 2:8,
          selected = 3
        ),
        sliderInput(
          "terms_per_topic",
          "Top words per topic",
          min = 4,
          max = 10,
          value = 6,
          step = 1
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
          margin-left: 320px;
          padding-top: 50px;
        }

        .main-header {
          position: fixed;
          width: 100%;
          top: 0;
          z-index: 1050;
        }

        .main-sidebar {
          position: fixed;
          height: 100vh;
          overflow-y: auto;
          padding-top: 50px;
        }

        .sticky-tabs-wrapper {
          position: relative;
          padding-top: 46px;
        }

        .sticky-tabs-wrapper > .tabbable > .nav-tabs {
          position: fixed;
          top: 50px;
          left: 320px;
          right: 0;
          z-index: 1040;
          background: #f4f6f9;
          border-bottom: 1px solid #d2d6de;
          padding: 0 15px;
          margin: 0;
        }

        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li {
          margin-bottom: -1px;
        }

        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li > a {
          background: transparent;
          border: none;
          border-bottom: 3px solid transparent;
          border-radius: 0;
          color: #3c8dbc;
          margin-right: 18px;
          padding: 14px 4px 10px 4px;
        }

        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li.active > a,
        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:focus,
        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li.active > a:hover {
          border: none;
          border-bottom: 3px solid #3c8dbc;
          background: transparent;
          color: #222d32;
        }

        .sticky-tabs-wrapper > .tabbable > .nav-tabs > li > a:hover {
          background: transparent;
          color: #222d32;
        }

        .small-box h3 {
          font-size: 28px;
        }

        .info-box-text {
          white-space: normal;
        }
      "))
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "sticky-tabs-wrapper",
          tabsetPanel(
            id = "main_tabs",
            tabPanel(
              "Overview",
              br(),
              fluidRow(
                valueBoxOutput("words_processed_box", width = 3),
                valueBoxOutput("unique_words_box", width = 3),
                valueBoxOutput("top_word_box", width = 3),
                valueBoxOutput("rare_words_box", width = 3)
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Most frequent terms",
                  status = "info",
                  solidHeader = FALSE,
                  height = "67vh",
                  plotOutput("bar_chart", height = "500px")
                ),
                box(
                  width = 6,
                  title = "Frequency table",
                  status = "info",
                  solidHeader = FALSE,
                  height = "67vh",
                  DTOutput("frequency_table")
                )
              )
            ),
            tabPanel(
              "Word Cloud",
              br(),
              box(
                width = 12,
                title = "Word Cloud",
                status = "info",
                solidHeader = FALSE,
                wordcloud2Output("word_cloud", height = "600px")
              )
            ),
            tabPanel(
              "Topic Modeling",
              br(),
              fluidRow(
                valueBoxOutput("usable_words_box", width = 3),
                valueBoxOutput("segment_size_box", width = 3),
                valueBoxOutput("estimated_segments_box", width = 3),
                valueBoxOutput("suggested_segment_size_box", width = 3)
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Topic Modeling Guidance",
                  status = "info",
                  solidHeader = FALSE,
                  p(textOutput("document_guidance")),
                  p(textOutput("topic_guidance"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Topic Summary",
                  status = "success",
                  solidHeader = FALSE,
                  DTOutput("topic_summary_table")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Top Terms per Topic",
                  status = "primary",
                  solidHeader = FALSE,
                  plotOutput("topic_terms_plot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Topic-Term Probabilities",
                  status = "warning",
                  solidHeader = FALSE,
                  DTOutput("topics_table")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  document <- reactive({
    req(input$file)
    read_document(input$file$datapath)
  })

  observeEvent(input$file, {
    updateTextAreaInput(session, "custom_words", value = "")
  })

  document_segments <- reactive({
    req(document())
    req(input$segment_size)
    split_document(document(), input$segment_size)
  })

  tokens <- reactive({
    req(document_segments())
    tokenize_segments(document_segments(), prepare_stopwords(input$custom_words))
  })

  bigrams <- reactive({
    req(document_segments())
    tokenize_bigrams(document_segments(), prepare_stopwords(input$custom_words))
  })

  word_frequencies <- reactive({
    if (input$token_type == "bigram") {
      req(bigrams())
      count_bigrams(bigrams())
    } 
    
    else {
      req(tokens())
      count_words(tokens(), input$word_form)
    }
  })

  segment_frequencies <- reactive({
    req(tokens())
    count_segment_terms(tokens(), input$word_form)
  })

  available_segments <- reactive({
    req(document_segments())
    req(input$segment_size)
    ceiling(nrow(tokens()) / input$segment_size)
  })

  observe({
    req(available_segments(), input$num_topics)

    max_topics_allowed <- min(8, max(2, available_segments()))
    topic_choices <- 2:max_topics_allowed
    selected_topics <- min(as.numeric(input$num_topics), max_topics_allowed)

    updateSelectInput(
      session,
      "num_topics",
      choices = topic_choices,
      selected = selected_topics
    )
  })

  dtm <- reactive({
    req(segment_frequencies())

    dtm_data <- create_dtm(segment_frequencies())
    dtm_dimensions <- dim(dtm_data)

    validate(
      need(
        dtm_dimensions[1] >= as.numeric(input$num_topics),
        "The number of segments must be greater or equal to the number of topics."
      ),
      need(
        dtm_dimensions[2] >= as.numeric(input$num_topics),
        "There are not enough distinct terms for that many topics."
      )
    )

    dtm_data
  })

  lda_model <- reactive({
    req(dtm())

    topicmodels::LDA(
      dtm(),
      k = as.numeric(input$num_topics),
      control = list(seed = 1234)
    )
  })

  topics <- reactive({
    req(lda_model())
    create_topics(lda_model(), input$terms_per_topic)
  })

  topic_summary <- reactive({
    req(topics())
    create_topic_summary(topics())
  })

  suggested_segment_size <- reactive({
    req(tokens())
    get_segment_recommendation(nrow(tokens()))
  })

  output$words_processed_box <- renderValueBox({
    req(tokens())

    valueBox(
      value = format(nrow(tokens()), big.mark = ","),
      subtitle = "Words after processing",
      icon = icon("font"),
      color = "light-blue"
    )
  })

  output$unique_words_box <- renderValueBox({
    req(word_frequencies())

    valueBox(
      value = nrow(word_frequencies()),
      subtitle = "Unique words",
      icon = icon("list"),
      color = "teal"
    )
  })

  output$top_word_box <- renderValueBox({
    req(word_frequencies())

    top_word <- word_frequencies() |> dplyr::slice_head(n = 1)

    valueBox(
      value = top_word$term,
      subtitle = paste("Most frequent (", top_word$count, ")", sep = ""),
      icon = icon("star"),
      color = "yellow"
    )
  })

  output$rare_words_box <- renderValueBox({
    req(word_frequencies())

    rare_words <- word_frequencies() |>
      dplyr::filter(count == 1)

    valueBox(
      value = nrow(rare_words),
      subtitle = "Rare words (appear once)",
      icon = icon("search"),
      color = "red"
    )
  })

  output$frequency_table <- renderDT({
    req(word_frequencies())

    datatable(
      word_frequencies() |> dplyr::slice_head(n = 100),
      options = list(
        pageLength = 10,
        scrollY = "43vh"
      ),
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
      backgroundColor = "#F5F5F5"
    )
  })

  output$bar_chart <- renderPlot({
    plot_data <- word_frequencies() |>
      dplyr::slice_head(n = input$top_n) |>
      dplyr::mutate(term = reorder(term, count))

    ggplot2::ggplot(plot_data, aes(x = term, y = count)) +
      geom_col(fill = "#1f5c99") +
      coord_flip() +
      labs(
        x = NULL,
        y = "Count"
      ) +
      theme_minimal(base_size = 13)
  })

  output$usable_words_box <- renderValueBox({
    req(tokens())

    shinydashboard::valueBox(
      value = format(nrow(tokens()), big.mark = ","),
      subtitle = "Usable words",
      icon = icon("font"),
      color = "light-blue"
    )
  })

  output$segment_size_box <- renderValueBox({
    req(input$segment_size)

    shinydashboard::valueBox(
      value = paste(input$segment_size, "words"),
      subtitle = "Segment size",
      icon = icon("grip-lines-vertical"),
      color = "navy"
    )
  })

  output$estimated_segments_box <- renderValueBox({
    req(available_segments())

    shinydashboard::valueBox(
      value = available_segments(),
      subtitle = "Estimated segments",
      icon = icon("copy"),
      color = "teal"
    )
  })

  output$suggested_segment_size_box <- renderValueBox({
    req(suggested_segment_size())

    shinydashboard::valueBox(
      value = suggested_segment_size(),
      subtitle = "Suggested segment size",
      icon = icon("lightbulb"),
      color = "purple"
    )
  })

  output$document_guidance <- renderText({
    req(tokens())

    total_tokens <- nrow(tokens())

    if (total_tokens < 500) {
      return(
        paste(
          "This document has fewer than 500 usable words, so topic modeling may be less stable.",
          "A longer document usually produces clearer topics.",
          "Recommended segment size:",
          suggested_segment_size(),
          "."
        )
      )
    }

    paste(
      "This document is long enough for topic modeling.",
      "Recommended segment size:",
      suggested_segment_size(),
      "."
    )
  })

  output$topic_guidance <- renderText({
    req(available_segments())

    segment_count <- available_segments()

    if (segment_count < 3) {
      return(
        "The current segment size creates very few segments, so the discovered topics may be broad or mixed. Try a smaller segment size for more detailed themes."
      )
    }

    if (segment_count <= 5) {
      return(
        paste(
          "The current settings produce",
          segment_count,
          "segments. This is usable, but a slightly smaller segment size may give more distinct topics."
        )
      )
    }

    if (segment_count <= 10) {
      return(
        paste(
          "The current settings produce",
          segment_count,
          "segments, which is a strong range for exploratory topic modeling."
        )
      )
    }

    paste(
      "The current settings produce",
      segment_count,
      "segments. This gives the model many text units, but the resulting topics may become a little more fragmented."
    )
  })

  output$topic_summary_table <- renderDT({
    req(topic_summary())

    datatable(
      topic_summary(),
      options = list(
        dom = "t",
        pageLength = 5
      ),
      rownames = FALSE,
      colnames = c("Topic", "Top words")
    )
  })

  output$topic_terms_plot <- renderPlot({
    req(topics())

    plot_data <- topics() |>
      dplyr::mutate(
        term = tidytext::reorder_within(term, beta, topic)
      )

    ggplot2::ggplot(plot_data, aes(x = term, y = beta, fill = topic)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      facet_wrap(~topic, scales = "free_y") +
      tidytext::scale_x_reordered() +
      labs(
        x = NULL,
        y = "Topic-term probability"
      ) +
      theme_minimal(base_size = 13)
  })

  output$topics_table <- renderDT({
    req(topics())

    datatable(
      topics(),
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Topic", "Term", "Importance")
    )
  })
}

shinyApp(ui = ui, server = server)
