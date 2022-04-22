# ¸ü¤J lib
if(!require(rvest)) install.packages("rvest")
if(!require(magrittr)) install.packages("magrittr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(rpart)) install.packages("rpart")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(data.table)) install.packages("data.table")


url <- as.list(url_list[[1]])
#country_vector <- as.vector(name_list)
get_html  <- lapply(url,read_html)
get_nodes <- lapply(get_html,html_nodes,xpath = "//table[@class = 'stats-snapshot']")
get_table <- lapply(get_nodes,html_table)

get_table <- Filter(function(x) length(x)>0,get_table)
get_table[[144]] <- NULL
get_table[[170]] <- NULL

get_market_share <- function() {
  return(do.call("rbind" , lapply(get_table , FUN = create_table )))
}

create_table <- function(get_table){
  df_market_share <- as.data.frame(get_table)
  title <- df_market_share[2,2]
  title <- as.character(title)   
  split <- strsplit(title," ",fixed = TRUE)
  country <- as.data.frame(split)[7,]
  iOS <- df_market_share[which(df_market_share$X1=="iOS"),2]
  row <- cbind(country,iOS)
  row <- as.data.frame(row)
  return(row)
}

market_share <- get_market_share()
write.csv(market_share, file = "market_share.csv")
