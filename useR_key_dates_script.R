# Reading useR archive data
archive_df <- read.csv("useR_key_dates.csv")

# Removing present Summary column
df <- archive_df[-4]

# Conference start date data-frame
conf_start_dates <- df[df$Action=="Conference opens",]

# Classification based on yearly data
year <- unique(df$Year)


# Function to compute - Weeks, Days
date_summary <- function(start_date, event_date){
  
  from <- start_date
  to <- event_date
  date_strings = c(from, to)
  datetimes = strptime(date_strings, format = "%d-%m-%Y")
  
  # days
  diff_in_days <- difftime(datetimes[2], datetimes[1], units = "days")
  # weeks
  diff_in_weeks <- floor(difftime(datetimes[2], datetimes[1], units = "weeks")) 
  smry <- paste0(diff_in_weeks, " Weeks, ", as.numeric(diff_in_days)%%7, " Days before the conference")
  smry
}


# Updating Summary Column
rowi <- 0
for(i in 1:length(year)){
  temp_df <- df[df$Year==year[i],]
  edate <- conf_start_dates$Date[i]
  n <- length(temp_df$Year)
  for(x in 1:n){
    sdate <- temp_df$Date[x]
    res <- date_summary(sdate, edate)
    df$Summary[rowi + x] <- res
  }
  df$Summary[rowi+n-1] <- str_replace(df$Summary[rowi+n-1], "before the conference", "conference started")
  res <- date_summary(edate, df$Date[rowi+n])
  res <- str_replace(res, "before the conference", "conference ended")
  df$Summary[rowi+n] <- res
  rowi <- rowi + n
}

View(df)
