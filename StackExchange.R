# OAuth 2.0 - https://stackexchange.com/oauth/dialog?client_id=19733&scope=no_expiry&redirect_uri=https://stackexchange.com/oauth/login_success/
# key - SX945TWflISfN*DzZ*G53w((
# access_token - ixiOsNGGzzlmAyggyT1rRA))

key <- "SX945TWflISfN*DzZ*G53w(("
token <- "ixiOsNGGzzlmAyggyT1rRA))"

library(httr)
library(rlist)
library(jsonlite)
library(dplyr)

fromDate <- "2021-01-01"
fromDate <- as.numeric(as.POSIXct(fromDate, tz="UTC"))

toDate <- "2021-01-02"
toDate <- as.numeric(as.POSIXct(toDate, tz="UTC"))

url <- paste0("https://api.stackexchange.com/2.2/questions?key=", key, "&page=1&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=activity&access_token=", token, "&tagged=r&site=stackoverflow")
url <- GET(url)

http_type(url)
http_error(url)

jsonURLparsed <- content(url, as="parsed")
items <- jsonURLparsed$items
length(items)

modJson <- jsonURLparsed$items

len <- as.vector(1:length(modJson))

quesId <- lapply(len, function(x){modJson[[x]][["question_id"]]})
quesTitle <- lapply(len, function(x){modJson[[x]][["title"]]})
quesViewCount <- lapply(len, function(x){modJson[[x]][["view_count"]]})
quesAnswered <- lapply(len, function(x){modJson[[x]][["is_answered"]]})
quesLink <- lapply(len, function(x){modJson[[x]][["link"]]})

df <- data.frame(ID=unlist(quesId), Title=unlist(quesTitle), View_Count=unlist(quesViewCount), 
                 Answer=unlist(quesAnswered), Link=unlist(quesLink))


pg <- 1
url1 <- paste0("https://api.stackexchange.com/2.2/questions?key=", key, "&page=", pg, "&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=activity&access_token=", token, "&tagged=r&site=stackoverflow")
url1 <- GET(url1)

http_error(url1)

jsonURLparsed1111 <- content(url1, as="parsed")
temp_modjson <- jsonURLparsed1111$items
length(temp_modjson)



temp_df <- data.frame(ID=integer(), Title=character(), View_Count=integer(), 
                      Answer=logical(), Link=character())

pg <- 1

repeat{
  url1 <- paste0("https://api.stackexchange.com/2.2/questions?key=", key, "&page=", pg, "&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=activity&access_token=", token, "&tagged=r&site=stackoverflow")
  url1 <- GET(url1)
  jsonURLparsed1111 <- content(url1, as="parsed")
  temp_modjson <- jsonURLparsed1111$items
  
  if(length(temp_modjson)==0){
    break
  }
  
  len <- as.vector(1:length(temp_modjson))
  
  quesId <- lapply(len, function(x){temp_modjson[[x]][["question_id"]]})
  quesTitle <- lapply(len, function(x){temp_modjson[[x]][["title"]]})
  quesViewCount <- lapply(len, function(x){temp_modjson[[x]][["view_count"]]})
  quesAnswered <- lapply(len, function(x){temp_modjson[[x]][["is_answered"]]})
  quesLink <- lapply(len, function(x){temp_modjson[[x]][["link"]]})
  
  df <- data.frame(ID=unlist(quesId), Title=unlist(quesTitle), View_Count=unlist(quesViewCount), 
                   Answer=unlist(quesAnswered), Link=unlist(quesLink))
  
  temp_df <- rbind(temp_df, df)
  pg <- pg + 1
}





