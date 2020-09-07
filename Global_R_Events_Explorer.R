###  1. Global R Events Data from R Studio Education  ###

# RStudio.edu.events() function takes input 
# as year for extraction of data

library(rvest)

RStudio.edu.events <- function(year){
  Revents_wbpg <- read_html("https://education.rstudio.com/events/archive/")
  eve <- Revents_wbpg %>%
    html_nodes("td:nth-child(1)") %>%
    html_text()
  
  date <- Revents_wbpg %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()
  date <- gsub('\\s+','',date)
  
  description <- Revents_wbpg %>%
    html_nodes("td:nth-child(3)") %>%
    html_text()
  
  location <- Revents_wbpg %>%
    html_nodes("td:nth-child(4)") %>%
    html_text()
  
  event_url <- Revents_wbpg %>%
    html_nodes("td:nth-child(1) a") %>%
    html_attr('href')
  
  df <- data.frame(Events = eve, Date = date, 
                   Description = description, Location = location,
                   Year = year(as.Date(gsub(".*,","",date), format = "%Y")),
                   Link = event_url)
  event_details <- df[df$Year==year,]
  return(event_details)
}



###  3. Global R Events Data from Jumping Rivers  ###

# Rjumping_rivers_events() function takes year as input 
# for extraction of event details

library(rvest)
library(tidyr)

Rjumping_rivers_events <- function(year){
  wbpg <- read_html("https://github.com/jumpingrivers/meetingsR/blob/gh-pages/events.csv")
  raw_data <- wbpg %>%
    html_nodes(".js-file-line") %>%
    html_text()
  raw_data <- raw_data[-1]
  
  temp_df <- data.frame(x=raw_data)
  temp_df <- temp_df %>% separate(x, c("Year","Month","Date", 
                                       "Event", "Link","Location",
                                       "Country","NAs"), ",", extra = "merge")
  temp_df$Year <- year(as.Date(gsub("\\D","",temp_df$Year), format = "%Y"))
  temp_df$NAs <- NULL
  R_event_details <- temp_df[temp_df$Year==year,]
  return(R_event_details)
}

