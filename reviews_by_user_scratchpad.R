#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("devtools", "httr", "tidyverse", "tidyRSS"), load_packages)

# install_github("Famguy/rgoodreads")
library(rgoodreads)

Sys.setenv(GOODREADS_KEY = "MSSsVeTYFafhHvSgadZXHg")

u <- user('10388416')

u_reviews <- tidyRSS::tidyfeed(as.character(u$reviews_rss_url)) 

u_reviews <- u_reviews %>%
  mutate(book_id = str_replace_all(item_link, "https://www.goodreads.com/review/show/", "")) %>%
  mutate(book_id = str_replace_all(book_id, "\\?utm_medium=api&utm_source=rss", ""))

rgoodreads::goodreads_GET("review/list", id = '10388416')

