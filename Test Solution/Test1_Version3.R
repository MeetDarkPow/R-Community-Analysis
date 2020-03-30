# FIRST FUNCTION

library(httr)
library(jsonlite)
library(gsubfn)
library(data.table)

dir.create("Year_Search")

monthExtract <- function(year, month, lastday_month){
  
  dir.create(paste0("Year_Search/Daywise_",year))
  
  # converting month to readable ISO date format
  if(month<10){
    month <- paste0("0", month)
  }
  
  # turning off the feature of R to turn multinomial variables,
  # dummy variables in regression models and produce nice cross tables
  options(stringsAsFactors = FALSE)
  
  for(day in 1:(lastday_month+1)){
    
    # converting day to readable ISO date format
    if(day<10){
      day <- paste0("0", day)
    }
    if(day == (lastday_month+1)){break}
    
    # counting total number of pages
    url <- "https://api.github.com"
    path <- paste0("search/repositories?q=language:R+created:",as.character(year),"-",as.character(month),
                   "-",as.character(day),"&per_page=100&sort=stars&order=desc")
    raw.result <- GET(url = url, path = path)
    this.raw.content <- rawToChar(raw.result$content)
    this.content <- fromJSON(this.raw.content)
    if((this.content$total_count)%%100 != 0){
      n <- as.integer(this.content$total_count/100) + 1
    } else {
      n <- this.content$total_count/100
    }
    
    Sys.sleep(time = 30)
    
    # data extraction
    # creating final list of all data extracted using API
    final_list <- vector(mode = "list", length = n)
    for (i in 1:n) {
      path_int <- paste0("search/repositories?q=language:R+created:",as.character(year),"-",as.character(month),"-",as.character(day),"&page=",as.character(i),"&per_page=100&sort=stars&order=desc")
      raw.result <- GET(url = url, path = path_int)
      this.raw.content <- rawToChar(raw.result$content)
      this.content <- fromJSON(this.raw.content)
      final_list[[i]] <- this.content
      Sys.sleep(time = 20)
    }
    
    # cleaning time format 
    # storing repo_name and created_date as data.frame
    datalist <- list()
    for(i in 1:n){
      repo_name <- final_list[[i]][["items"]][["name"]]
      date <- final_list[[i]][["items"]][["created_at"]]
      date <- as.Date(strapplyc(date, "\\d+-\\d+-\\d+", simplify = TRUE))
      repo_name_date <- data.frame(Repository = repo_name,Created = date)
      datalist[[i]] <- repo_name_date
    }
    big_data <- data.table::rbindlist(datalist)
    
    # arranging repos according to their dates
    big_data <- big_data[rev(order(big_data$Created, decreasing = TRUE)),]
    
    # saving file with their respective year-month
    saveRDS(big_data, paste0("Year_Search/Daywise_",year,"/",year,"_",month,"_",day,".rds"))
  }
}

# SECOND FUNCTION

library(tidyverse)
library(data.table)
library(jsonlite)

yearExtract <- function(year){
  
  setwd(paste0("Year_Search/Daywise_",year))
  
  # combining all .rds files and storing them as data.frame
  df <- list.files(pattern = ".rds") %>%
    map(readRDS) %>% 
    data.table::rbindlist()
  
  # writing .csv and .json file
  dir.create("CSV_File")
  write.csv(df, file = paste0("CSV_File/R_repos_",year,".csv"), row.names = FALSE)
  
  dir.create("JSON_File")
  exportJson <- toJSON(df)
  write_json(exportJson, paste0("JSON_File/R_repos_",year,".json"))
}
