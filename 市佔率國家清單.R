# ¸ü¤J lib
if(!require(rvest)) install.packages("rvest")
if(!require(magrittr)) install.packages("magrittr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")

url <- "https://gs.statcounter.com/os-market-share/mobile/2021"
html <- read_html(url)

name_list <- html %>% html_nodes(xpath = "//ul[@id='all-regions']/li") %>% html_text() %>% list
name_list <- lapply(name_list , FUN = trimws)
name_list <- lapply(name_list , FUN = gsub , pattern = " " , replace = "-")
name_list <- lapply(name_list , FUN = tolower)

create_url <- function(country){
  url <- "https://gs.statcounter.com/os-market-share/mobile/"
  url <- paste(url , country , sep = "")
  url <- paste(url , "/2021" , sep = "")
  return(url)
}

url_list <- lapply(name_list, create_url)