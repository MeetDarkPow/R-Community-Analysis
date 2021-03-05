# OAuth 2.0 - https://stackexchange.com/oauth/dialog?client_id=19733&scope=no_expiry&redirect_uri=https://stackexchange.com/oauth/login_success/
# key - SX945TWflISfN*DzZ*G53w((
# access_token - ixiOsNGGzzlmAyggyT1rRA))

key <- "SX945TWflISfN*DzZ*G53w(("
token <- "ixiOsNGGzzlmAyggyT1rRA))"

library(httr)
library(rlist)
library(jsonlite)
library(dplyr)


# Questions related queries from `fromDate` till `toDate`
# 2 parameters - fromDate, toDate
# 1 default parameter - `pg` as page number
# pass fromDate or toDate as string parameters such as
# Ques_query("2021-01-01", "2021-01-02")

Ques_query <- function(fromDate, toDate, pg=1){
  
  fromDate <- as.numeric(as.POSIXct(fromDate, tz="UTC"))
  toDate <- as.numeric(as.POSIXct(toDate, tz="UTC"))
  
  ques_df <- data.frame(ID=integer(), Title=character(), View_Count=integer(), 
                        Answer=logical(), Link=character())
  repeat{
    
    wbpg <- paste0("https://api.stackexchange.com/2.2/questions?key=", key, "&page=", pg, "&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=activity&access_token=", token, "&tagged=r&site=stackoverflow")
    wbpg <- GET(wbpg)
    wbpg_jsonParsed <- content(wbpg, as="parsed")
    wbpg_items <- wbpg_jsonParsed$items
    
    if(length(wbpg_items)==0){
      break
    }
    
    len <- as.vector(1:length(wbpg_items))
    
    quesId <- lapply(len, function(x){wbpg_items[[x]][["question_id"]]})
    quesTitle <- lapply(len, function(x){wbpg_items[[x]][["title"]]})
    quesViewCount <- lapply(len, function(x){wbpg_items[[x]][["view_count"]]})
    quesAnswered <- lapply(len, function(x){wbpg_items[[x]][["is_answered"]]})
    quesLink <- lapply(len, function(x){wbpg_items[[x]][["link"]]})
    
    df <- data.frame(ID=unlist(quesId), Title=unlist(quesTitle), View_Count=unlist(quesViewCount), 
                     Answer=unlist(quesAnswered), Link=unlist(quesLink))
    
    ques_df <- rbind(ques_df, df)
    pg <- pg + 1
  }
  ques_df
}


# Answers related queries from `fromDate` till `toDate`
# 2 parameters - fromDate, toDate
# 1 default parameter - `pg` as page number
# pass fromDate or toDate as string parameters such as
# Ans_query("2021-01-01", "2021-01-02")

Ans_query <- function(fromDate, toDate, pg=1){
  
  fromDate <- as.numeric(as.POSIXct(fromDate, tz="UTC"))
  toDate <- as.numeric(as.POSIXct(toDate, tz="UTC"))
  
  ans_df <- data.frame(AnswerID=integer(), QuestionID=integer(), 
                       Accepted=logical(), Score=integer())
  repeat{
    
    wbpg <- paste0("https://api.stackexchange.com/2.2/answers?key=", key, "&page=", pg, "&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=activity&access_token=", token, "&tagged=r&site=stackoverflow")
    wbpg <- GET(wbpg)
    wbpg_jsonParsed <- content(wbpg, as="parsed")
    wbpg_items <- wbpg_jsonParsed$items
    
    if(length(wbpg_items)==0){
      break
    }
    
    len <- as.vector(1:length(wbpg_items))
    
    ansId <- lapply(len, function(x){wbpg_items[[x]][["answer_id"]]})
    quesId <- lapply(len, function(x){wbpg_items[[x]][["question_id"]]})
    ansAccepted <- lapply(len, function(x){wbpg_items[[x]][["is_accepted"]]})
    ansScore <- lapply(len, function(x){wbpg_items[[x]][["score"]]})
    
    df <- data.frame(AnswerID=unlist(ansId), QuestionID=unlist(quesId), 
                     Accepted=unlist(ansAccepted), Score=unlist(ansScore))
    
    ans_df <- rbind(ans_df, df)
    pg <- pg + 1
  }
  ans_df
}


# Comments related queries from `fromDate` till `toDate`
# 2 parameters - fromDate, toDate
# 1 default parameter - `pg` as page number
# pass fromDate or toDate as string parameters such as
# Comnt_query("2021-01-01", "2021-01-02")

library(lubridate)

Comnt_query <- function(fromDate, toDate, pg=1){
  
  fromDate <- as.numeric(as.POSIXct(fromDate, tz="UTC"))
  toDate <- as.numeric(as.POSIXct(toDate, tz="UTC"))
  
  cmt_df <- data.frame(CommentID=integer(), PostID=integer(), 
                       CreationDate=POSIXct(), Score=integer())
  repeat{
    
    wbpg <- paste0("https://api.stackexchange.com/2.2/comments?key=", key, "&page=", pg, "&pagesize=100&fromdate=", fromDate, "&todate=", toDate, "&order=desc&sort=votes&access_token=", token, "&tagged=r&site=stackoverflow")
    wbpg <- GET(wbpg)
    wbpg_jsonParsed <- content(wbpg, as="parsed")
    wbpg_items <- wbpg_jsonParsed$items
    
    if(length(wbpg_items)==0){
      break
    }
    
    len <- as.vector(1:length(wbpg_items))
    
    cmtId <- lapply(len, function(x){wbpg_items[[x]][["comment_id"]]})
    pId <- lapply(len, function(x){wbpg_items[[x]][["post_id"]]})
    createDate <- lapply(len, function(x){wbpg_items[[x]][["creation_date"]]})
    cmtScore <- lapply(len, function(x){wbpg_items[[x]][["score"]]})
    
    df <- data.frame(CommentID=unlist(cmtId), PostID=unlist(pId), 
                     CreationDate=as_datetime(unlist(createDate)), 
                     Score=unlist(cmtScore))
    
    cmt_df <- rbind(cmt_df, df)
    pg <- pg + 1
  }
  cmt_df
}

