#code from https://maraaverick.rbind.io/2017/10/goodreads-part-2/

library(tidyverse)
library(stringr)
library(rvest)

startUrl <- "https://www.goodreads.com/review/list/10388416?shelf=read"


# function to get book descriptions
getBookDescription <- function(bookLink) {
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_node("#descriptionContainer") %>%
    html_text() %>%
    trimws()
}

# function to get book genres
get_genres <- function(bookLink){
  url <- str_c("https://www.goodreads.com", bookLink)
  read_html(url) %>%
    html_nodes(".left .bookPageGenreLink") %>%
    html_text(trim = TRUE)
}

# function to get books
getBooks <- function(i) {
  cat(i, "\n")
  url <- str_c(startUrl, "?page=", i, "&shelf=read")
  
  html <- read_html(url)
  
  title <- html %>%
    html_nodes(".title a") %>%
    html_text(trim = TRUE) #%>%
  #discard(!str_detect(., "[A-Z0-9]"))
  
  author <- html %>%
    html_nodes(".author a") %>%
    html_text(trim = TRUE) %>%
    discard(str_detect(., "^\\("))
  
  bookLinks <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    discard(!str_detect(., "^/book/show")) %>%
    na.omit() %>%
    unique()
  
  bookDescription <- bookLinks %>%
    map_chr(getBookDescription)
  
  bookGenre <- bookLinks %>%
    map(get_genres)
  
  return(tibble(
    title = title,
    author = author,
    book_description = bookDescription,
    book_genres = bookGenre
  ))
}

# get books 
n_books <- 518
n_pages <- ceiling(n_books/30)
goodreads <- c(1:n_pages) %>%
  map_dfr(getBooks)

# save the output
saveRDS(goodreads, here::here("output", "goodreads_read.rds"))

#clean book descriptions
read_books_data <- goodreads

clean_book_descs <- read_books_data %>%
  mutate(clean_desc = str_replace_all(book_description, "[^a-zA-Z\\s]", " ")) %>%
  mutate(clean_desc = str_trim(clean_desc, side = "both")) %>%
  select(-book_genres)

library(tidytext)

descs_unnested <- clean_book_descs %>%
  unnest_tokens(word, clean_desc) %>%
  select(-book_description)

library(SnowballC)

descs_unnested <- descs_unnested %>%
  mutate(word_stem = wordStem(word, language="english"))

data("stop_words")

tidy_books <- descs_unnested %>%
  anti_join(stop_words, by = "word")

tidy_book_words <- tidy_books %>%
  count(word, sort = TRUE)

tidy_book_words

tidy_book_word_stems <- tidy_books %>%
  count(word_stem, sort = TRUE)

tidy_book_word_stems

data(sentiments)

word_sentiments <- tidy_book_words %>%
  inner_join(get_sentiments("afinn"), by = "word")

word_sentiments