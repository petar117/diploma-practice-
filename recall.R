library(tidyverse)
library(gapminder)
library(dplyr)
library(readtext)
library(tidytext)
library(wordcloud2)

df <- tibble(
  name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  age = c(25, 30, 35, 40, 45),
  score = c(80, 85, 90, 95, 100) 
)

df
df %>% arrange(desc(score)) %>% head(3)
df %>% filter(age >= 35) %>% pull(name)
df %>% mutate(grade = case_when (
  score >= 92 ~ "A",
  score >= 82 ~"B",
  TRUE ~ "C"
))
df1 <- df %>% mutate(passed = case_when (
  score >= 90 ~ "PASS",
  TRUE ~ "FAIL"
)) %>% arrange(desc(score))

sentiment <- c("positive", "negative", "neutral", 
               "positive", "negative", "positive")

sentiment_f <- factor(sentiment, levels = c("negative", "neutral", "positive"), ordered = TRUE)

sentiment_f
levels(sentiment_f)
as.numeric(sentiment_f)

rating <- c("low", "medium", "high", "medium", "low")

rating_f <- factor(
  rating,
  levels = c("low", "medium", "high"),
  ordered = TRUE
)
rating_f > "medium"

words <- c("cat", "dog", "cat", "bird")
words_f <- factor(words)

words_f[1] <- "lion"
levels(words_f)
levels(words_f) <- c(levels(words_f), "lion")

words <- c("apple", "banana", "apple", "orange", 
           "banana", "apple", "orange", "banana")

words_f <- factor(words)

table(words_f)

words_f2 <- factor(words,
                   levels = c("apple", "banana", "orange", "grape"))
words[1] <- "grape" 
levels(words)
table(words_f2)


words_char <- c("apple", "banana", "apple", "orange",
                "banana", "apple", "orange", "banana")

words_f3 <- factor(words_char)

words_char <- c(words_char, "grape")
words_f3 <- c(words_f3, "grape")
levels(words_f3) <- c(levels(words_f3), "grape")
table(words_char)
table(words_f3)

x <-  seq(from = 1, to = 100, by = 5)
x
m <- matrix(1:12, nrow = 3, ncol = 4)
m[2,3]
m[1,1] = 100
m[2:3, 1:2]

my_list <- list(
  name = "Alice",
  age = 25,
  scores = c(90, 85, 88),
  matrix_data = m
)

my_list[[1]]
my_list[["scores"]][3]
my_list[["matrix_data"]][2,3]
my_list["age"] <-  30
my_list["city"] <- "New York"

student <- list(
  id = 101,
  grades = matrix(c(80,90,85,88), nrow=2),
  passed = TRUE
)
student[["grades"]][1,2] <- 95


A <- matrix(1:16, nrow = 4)
A[3,] <- 0
A[A>10] <- 999

A[A > 8]
A > 8
A[ , A[1, ] > 5]
A[ A[ ,1] > 2 , ]

airquality
library(tibble)
df <- as_tibble(airquality)
df
df[df$Day==3 & df$Month == 6,]
subset(df, Day == 3 & Month == 6)

library(dplyr)

words <- tibble(
  word = c("apple", "banana", "apple", "orange",
           "banana", "apple", "orange", "banana")
)

words
words %>% 
  group_by(word) %>% 
  summarise(count = n())
words %>% count(word)

words %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  slice_head(n=2)

words %>% 
  count(word) %>%
  slice_max(n, n=2)

library(dplyr)

docs <- tibble(
  doc_id = c(1,1,1,1,2,2,2,2,2,3,3,3),
  word = c("apple","banana","apple","orange",
           "banana","banana","apple","kiwi","kiwi",
           "apple","orange","orange")
)

docs %>% 
  count(doc_id, word) %>% 
  group_by(doc_id) %>%
  slice_max(n, n=1, with_ties = FALSE) %>% 
  ungroup()
docs %>% 
  group_by(doc_id, word) %>% 
  summarise(n = n(), .groups = "drop")

docs %>%
  group_by(doc_id) %>%
  summarise(
    total_words = n(),
    unique_words = n_distinct(word)
  )


docs %>% 
  group_by(doc_id) %>% 
  summarise(
    total_words = n(),
    unique_words = n_distinct(word),
    most_common_word = word[which.max(table(word))],
    .groups = "drop")
  
# This is example of reading .docx file and putting each sentence in separate row.
document1 <- read_docx("one.docx")
dc1 <- docx_summary(document1) %>% 
  dplyr::filter(text!="") %>% 
  select(text)

# This is example of reading any text file and putting each sentence in separate row.
doc <- readtext("one.docx")

dc <- tibble(text = doc$text) %>% 
  mutate(text = str_split(text, "\n")) %>% 
  tidyr::unnest(text) %>% 
  filter(text != "")

# This is example of reading any text file and putting each word in separate row.
# unnest_tokens basically does everything, it splits into words, lowers the case, removes punctuation, etc.

library(textstem)
# doc <- readtext("disposition - Petar Todorovski.pdf")
# doc <- readtext("one.docx")
doc <- readtext("book.pdf")


dc <- tibble(text = doc$text) %>% 
  unnest_tokens(word,text) %>%
  filter(str_detect(word, "^[a-z]+$")) %>% 
  # anti_join(stop_words, by = "word") %>%
  anti_join(
    bind_rows(stop_words, tibble(word = "chapter")),
    by = "word"
  ) %>% 
  mutate(word_lemma = lemmatize_words(word)) %>% 
  print(n=50)
  
dc %>% 
  group_by(word) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice_head() %>% 
  pull(word)
#or use shorter 
dc %>% 
  count(word_lemma, sort = TRUE) %>% 
  slice_max(n, n=100) %>%
  wordcloud2()


# I should let user decide on the beginning if he want to use lemmatization
# in the analysis, and based on that decision to select corresponding column 
# problem with words like "not good"