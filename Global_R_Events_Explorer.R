###  1. Global R Events Data from R Studio Education  ###

# RStudio.edu.events() function takes input 
# as year for extraction of data

library(rvest)
library(lubridate)

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



###  2. Global R Events Data from Jumping Rivers  ###

# R_trainingC_jumping_Rivers() function takes input 
# as year for extraction of data

library(rvest)
library(lubridate)

R_trainingC_jumping_Rivers <- function(year){
  wbpg <- read_html("https://www.jumpingrivers.com/training/public/")
  title_event <- wbpg %>%
    html_nodes("strong") %>%
    html_text()
  
  link_event <- wbpg %>%
    html_nodes(".button--mid-blue") %>%
    html_attr('href')
  
  date_event <- c()
  description_event <- c()
  for(i in 1:length(link_event)){
    link_wbpg <- read_html(link_event[i])
    date_event[i] <- link_wbpg %>%
      html_nodes(".single-course-variation__meta-item:nth-child(3) .single-course-variation__meta-item-value") %>%
      html_text()
    description_event[i] <- link_wbpg %>%
      html_nodes(".single-course__description") %>%
      html_text()
  }
  
  df_event <- data.frame(Event = title_event, Date = date_event,
                         Description = description_event, URL = link_event,
                         Year = year(as.Date(gsub(".*, ","",date_event), format = "%Y")))
  event_table <- df_event[df_event$Year==year,]
  
  return(event_table)
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

