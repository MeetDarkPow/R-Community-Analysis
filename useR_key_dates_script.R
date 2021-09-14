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
