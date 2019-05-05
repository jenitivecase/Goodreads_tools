#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("devtools", "httr", "tidyverse", "tidyRSS", "XML"), load_packages)

# install_github("Famguy/rgoodreads")
library(rgoodreads)

source("./R/functions.R")

Sys.setenv(GOODREADS_KEY = "MSSsVeTYFafhHvSgadZXHg")

u <- user('10388416')

u_reviews <- tidyRSS::tidyfeed(as.character(u$reviews_rss_url)) 

u_reviews <- u_reviews %>%
  mutate(book_id = str_replace_all(item_link, "https://www.goodreads.com/review/show/", "")) %>%
  mutate(book_id = str_replace_all(book_id, "\\?utm_medium=api&utm_source=rss", ""))

readN <- function(id){
  temp <- goodreads_GET("review/list?v=2", id = id, shelf = "read")
  temp2 <- httr::content(temp, as = "parsed")
  temp3 <- xmlToList(xmlParse(temp2))
  
  return(as.integer(temp3$reviews$.attrs["total"]))
}

u_readN <- readN(id = '10388416')

u_read_pageN <- ifelse(u_readN %% 200 == 0, u_readN %/% 200, u_readN %/% 200 + 1)

IDs <- list()
for(page in 1:u_read_pageN){
  temp <- goodreads_GET("review/list", v = 2, id = id, shelf = "read", per_page = "200", page = page)
  temp2 <- httr::content(temp, as = "parsed")
  temp3 <- xmlToList(xmlParse(temp2))
  
  for(i in 1:(length(temp3$reviews)-1)){
    print(temp3$reviews[i]$review$id)
  }
  IDs <- c(IDs, unlist(sapply(temp3$reviews[1:(length(temp3$reviews)-1)], FUN = function(x) return(x$id)))) 
}

