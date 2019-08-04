#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("devtools", "httr", "tidyverse", "tidyRSS", "XML"), load_packages)

# install_github("Famguy/rgoodreads")
library(rgoodreads)

source("./R/functions.R")

#goodreads developer key
Sys.setenv(GOODREADS_KEY = "MSSsVeTYFafhHvSgadZXHg")

#it me
u <- user('10388416')

# 
# u_reviews <- tidyRSS::tidyfeed(as.character(u$reviews_rss_url)) 
# 
# u_reviews <- u_reviews %>%
#   mutate(book_id = str_replace_all(item_link, "https://www.goodreads.com/review/show/", "")) %>%
#   mutate(book_id = str_replace_all(book_id, "\\?utm_medium=api&utm_source=rss", ""))


#function to get the number of read books for a given user ID
readN <- function(id){
  temp <- goodreads_GET("review/list?v=2", id = id, shelf = "read")
  temp <- httr::content(temp, as = "parsed")
  temp <- xmlToList(xmlParse(temp))
  
  return(as.integer(temp$reviews$.attrs["total"]))
}

u_readN <- readN(id = '10388416')

#how many paginated api calls are required to get the full number of reviews
u_read_pageN <- ifelse(u_readN %% 200 == 0, u_readN %/% 200, u_readN %/% 200 + 1)

#all the review IDs are stored in this list
IDs <- list()
for(page in 1:u_read_pageN){
  temp <- goodreads_GET("review/list", v = 2, id = id, shelf = "read", per_page = "200", page = page)
  temp <- httr::content(temp, as = "parsed")
  temp <- xmlToList(xmlParse(temp))
  
  # for(i in 1:(length(temp$reviews)-1)){
  #   print(temp$reviews[i]$review$id)
  # }
  IDs <- c(IDs, unlist(sapply(temp$reviews[1:(length(temp$reviews)-1)], FUN = function(x) return(x$id)))) 
}

#convenience function to turn any nulls returned into NAs
nullifelse <- function(x){
  if(is.null(x)){
    return(NA)
  } else {
    return(x)
  }
}

#pull the stuff i care about for a single review
one_review <- function(review_id){
  temp <- goodreads_GET("review/show", id = review_id)
  temp <- httr::content(temp, as = "parsed")
  temp <- xmlToList(xmlParse(temp))
  
  read_at_date <- ifelse(!is.null(temp$review$read_at),
                         unlist(strsplit(temp$review$read_at, split = " "))[[length(unlist(strsplit(temp$review$read_at, split = " ")))]],
                         NA)

  out <- data.frame(rating = nullifelse(temp$review$rating),
                    avgrating = nullifelse(temp$review$book$average_rating),
                    read_at = read_at_date,
                    n_ratings = nullifelse(temp$review$book$ratings_count),
                    title = nullifelse(temp$review$book$title),
                    author = nullifelse(temp$review$book$authors$author$name),
                    link = nullifelse(temp$review$book$link),
                    year = nullifelse(temp$review$book$published))
  return(out)
}

#vectorized solution
test <- lapply(id_list, one_review)
test_df <- do.call(rbind, test)

reviews_df <- test_df %>%
  mutate(displacement = as.numeric(rating) - as.numeric(avgrating)) %>%
  arrange(desc(displacement))

#lots of missingness because of year read
ggplot(reviews_df) +
  geom_jitter(aes(y = displacement, x = lubridate::year(as.Date(read_at, format = "%Y")), 
                  color = rating)) +
  labs(x = "Year Read",
       y = "Distance from Average Rating",
       legend = "Rating") +
  theme(legend.position = "bottom")

#violin plot of rating vs. displacement
ggplot(reviews_df) +
  geom_violin(aes(y = displacement, x = rating, 
                  fill = rating)) +
  geom_hline(aes(yintercept = 0)) +
  scale_y_continuous(limits = c(-5, 2), breaks = seq(-5, 2)) +
  labs(x = "Rating",
       y = "Distance from Average Rating",
       title = "Goodreads Ratings' Displacement by Value",
       subtitle = paste0("Average Overall Goodreads Rating = ", 
                         formatC(mean(as.numeric(reviews_df$avgrating), na.rm = T), format = "f", digits = 2), "\n",
                         "Average Overall Personal Rating = ", 
                         formatC(mean(as.numeric(reviews_df$rating), na.rm = T), format = "f", digits = 2)),
       caption = paste0("N = ", nrow(reviews_df), " books read")) +
  theme(legend.position = "none")
