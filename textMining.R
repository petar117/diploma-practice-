library(tidyverse)
library(dplyr)
library(readtext)
library(tibble)
library(tidytext)
library(textstem)

read_document <- function(filename) {
  readtext::readtext(filename)
}

doc1 <- read_document("disposition - Petar Todorovski.pdf")
doc2 <- read_document("book.pdf")
doc3 <- read_document("one.docx")

custom_stopwords <- bind_rows(
  tidytext::stop_words,
  tibble::tibble(word = "chapter")
)

tokenize_text <- function(doc) {
  tibble(text = doc$text) %>% # is it good practice? we already have df from readtext
    unnest_tokens(word, text) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    anti_join(custom_stopwords, by = "word") %>%
    mutate(word_lemma = lemmatize_words(word))
}
tokens <- tokenize_text(doc2)

# The function uses nonlematized words
word_count_1 <- function(tokens) {
  tokens %>%
    count(word, sort = TRUE)
}

# The function uses lematized words
word_count_2 <- function(tokens) {
  tokens %>%
    group_by(word_lemma) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
}

# Pull the word with most occurrences
word_count_1(tokens) %>%
  slice_head() %>%
  pull(word)

# Create a wordcloud with the top 100 words in the document
word_count_2(tokens) %>%
  slice_max(count, n = 100) %>%
  wordcloud2()

# Create a word frequency table
word_count_2(tokens) %>%
  datatable(
    options = list(pageLength = 10),
    colnames = c("Word", "Frequency")
  )

# Create a barchart showing the top 10 most common words
word_count_2(tokens) %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(
    x = reorder(word_lemma, -count),
    y = count,
    label = count
  )) +
  geom_bar(aes(fill = count),
    position = "identity",
    stat = "identity",
    show.legend = FALSE
  ) +
  labs(
    x = "Word",
    y = "Frequency"
  ) +
  geom_text(color = "black", vjust = -0.2, size = 4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
    axis.text.y = element_text(hjust = 1.5, lineheight = 20, size = 10)
  )