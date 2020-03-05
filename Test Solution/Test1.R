library(httr)
library(jsonlite)
library(httpuv)
library(gsubfn)
library(data.table)

dir.create("Filter_Search")

repoExtract <- function(year){
  
  options(stringsAsFactors = FALSE)
  
  url <- "https://api.github.com"
  
  for(i in seq(1, 10, 1)){
    list_no <- paste("this.content", i, sep = "")
    path <- paste0("search/repositories?q=language:R&page=",as.character(i),"&per_page=100&sort=stars&order=desc")
    raw.result <- GET(url = url, path = path)
    this.raw.content <- rawToChar(raw.result$content)
    this.content <- fromJSON(this.raw.content)
    assign(list_no, this.content)
  }
  
  final <- list(this.content1, this.content2, this.content3, this.content4, this.content5,
                this.content6, this.content7, this.content8, this.content9, this.content10)
  
  datalist <- list()
  for(i in 1:10){
    repo_name <- final[[i]][["items"]][["name"]]
    date <- final[[i]][["items"]][["created_at"]]
    date <- as.Date(strapplyc(date, "\\d+-\\d+-\\d+", simplify = TRUE))
    repo_name_date <- data.frame(Repository = repo_name,
                                 Created = date)
    datalist[[i]] <- repo_name_date
  }
  
  big_data <- data.table::rbindlist(datalist)
  
  filter_repo <- subset(big_data, format(big_data$Created,"%Y")==year)
  filter_repo <- filter_repo[rev(order(filter_repo$Created, decreasing = TRUE)),]
  
  saveRDS(filter_repo, paste0("Filter_Search/filter_repo_",year,".rds"))
}

# SECOND FUNCTION

library(tidyverse)
library(data.table)
library(jsonlite)

repoSave <- function(csv = "", json = ""){
  
  setwd("Filter_Search")
  df <- list.files(pattern = ".rds") %>%
    map(readRDS) %>% 
    data.table::rbindlist()
  
  dir.create("CSV_File")
  write.csv(df, file = paste0("CSV_File/",as.character(csv),".csv"), row.names = FALSE)
  
  dir.create("JSON_File")
  exportJson <- toJSON(df)
  write_json(exportJson, paste0("JSON_File/",as.character(json),".json"))
}