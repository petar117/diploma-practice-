library(tidyverse)
library(dplyr)
library(readtext)
library(tibble)
library(tidytext)
library(textstem)

read_document <- function(path) {
  readtext::readtext(path)
}

doc1 <- read_document("disposition - Petar Todorovski.pdf")
doc2 <- read_document("book.pdf")
doc3 <- read_document("one.docx")