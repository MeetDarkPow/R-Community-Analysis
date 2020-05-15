### TASK - 1
library(xml2)
library(rvest)

CRAN_package_wbpg <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")

package_names <- CRAN_package_wbpg %>%
  html_nodes("a") %>%
  html_text()
package_names <- package_names[!package_names %in% LETTERS]

package_summary <- CRAN_package_wbpg %>%
  html_nodes("td+ td") %>%
  html_text()
package_summary <- gsub("[\r\n]", "", package_summary)

CRAN_package_data <- data.frame(
  Name = package_names, Summary = package_summary)

write.csv(CRAN_package_data, file = paste0("Pkg_byNames.csv"), row.names = F)

### TASK - 2

library(rvest)
library(ggplot2)

url = 'https://cran.r-project.org/web/packages/available_packages_by_date.html'

CRANpkg_date <- read_html(url)
tbls <- html_nodes(CRANpkg_date, "table") # since HTML is in table; no need to scrape td/tr elements
htmltable_date <- html_table(tbls[1], fill = TRUE)
pkgdata_date <- data.frame(htmltable_date[1])

pkgdata_date$Date <- as.Date(pkgdata_date$Date)

library(dplyr)
library(lubridate)

# updates by year
pkgdata_y <- pkgdata_date %>%
  mutate( pkgYear= year(Date)) %>%
  select (pkgYear) %>%
  group_by(pkgYear) %>%
  summarise(freq_pkgs = n())

pkgdata_y %>% 
  mutate(cumsum = cumsum(freq_pkgs)
         ,percY = freq_pkgs/cumsum(freq_pkgs)
         ,percC = cumsum(freq_pkgs)/sum(freq_pkgs))

# updates by month
pkgdata_ym <- pkgdata_date %>%
  mutate( pkgYear= year(Date)
          ,month_name = month(Date, label = FALSE)) %>%
  select (pkgYear,month_name) %>%
  group_by(pkgYear,month_name) %>%
  summarise(freq_pkgs = n())

pkgdata_ym2010 <- pkgdata_ym %>%
  filter(pkgYear > 2010 & pkgYear < 2020)

boxplot(pkgdata_ym2010$freq_pkgs~pkgdata_ym2010$month_name, 
        main="R Packages update over months", xlab = "Month", 
        ylab="Number of Packages")

# creating a dataframe which includes the following columns
# Package Name, First Release Date, Update frequency till today
rm(list = Filter(exists, c("packageNames")))
packageNames <- pkgdata_date$Package

df <- data.frame(Package=c("SAMPLE_NA")
                       ,firstRelease=c(as.Date("1999-08-17"))
                       ,nofUpdates=c(0))

for (i in 1:length(packageNames)){
  url1 <- 'https://cran.r-project.org/src/contrib/Archive/'
  pkgname <- packageNames[i]
  url2 <- paste0(url1,pkgname,'/')
  
  ifErrorPass <- tryCatch(read_html(url2), error=function(e) e) 
  if(inherits(ifErrorPass, "error")) next # if package does not have archive!!!
  
  cp <- read_html(url2)
  t2 <- html_nodes(cp, "table") 
  t2 <- html_table(t2[1], fill = TRUE)
  rm(list = Filter(exists, c("dd2")))
  dd2 <- data.frame(t2[1])
  dat <- dd2$Last.modified
  dat <- na.omit(as.Date(dat, format = '%Y-%m-%d'))
  firstRelease <- dat[order(format(as.Date(dat),"%Y%m%d"))[1]]
  numberOfUpdates <- length(dat) 
  df <- rbind(df,data.frame(Package=pkgname,firstRelease=as.Date(firstRelease, format='%Y-%m-%d'),nofUpdates=numberOfUpdates))
}

myData = df[df$firstRelease > '1999-08-17',]

# Using RWsearch for cran updates between 2 dates
library(RWsearch)

# last year
last_year <- crandb_fromto(from = Sys.Date()-365, to = Sys.Date()-1)
# last month
last_month <- crandb_fromto(from = Sys.Date()-30, to = Sys.Date()-1)