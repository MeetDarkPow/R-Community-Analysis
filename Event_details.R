# TASK - 1

library(countrycode)
library(tibble)
sampleEvents <- readRDS("sampleEvents.rds")
country_code <- sampleEvents$group_country
country_name <- countrycode(country_code, "iso2c", "country.name")
sampleEvents <- add_column(sampleEvents, Country = country_name, .after = "group_country")

# TASK - 2

for (i in 1:nrow(sampleEvents)) {
  if(sampleEvents[i,"venue_name"] == "Online event" && !is.na(sampleEvents[i,"venue_name"])){
    sampleEvents[i, "name"] <- paste(sampleEvents[i, "name"], "[Virtual]")
  }
}

# TASK - 3

 for (i in 1:nrow(sampleEvents)) {
   sampleEvents[i, "name"] <- paste(sampleEvents[i, "name"], sampleEvents[i, "Country"], 
                                    sampleEvents[i, "group_region"], sep = ", ")
 }
