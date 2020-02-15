
### Names of packages AND respective summary available on https://cran.r-project.org/ ###

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

# PACKAGE DATA BETWEEN SPECIFIED DATES 
library(installr)
library(ggplot2)
library(data.table)

RStudio_CRAN_dir <- download_RStudio_CRAN_data(START = '2020-01-01',END = '2020-01-10', 
                                               log_folder="C:\\Users\\MeetDarkPow\\Desktop\\R_Community_Explorer\\Logs")

RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_dir)

dim(RStudio_CRAN_data)

pkg_list <- most_downloaded_packages(RStudio_CRAN_data)
pkg_list

lineplot_package_downloads(names(pkg_list),RStudio_CRAN_data)
par(mfrow=c(1,2))

barplot_package_users_per_day("ggplot",RStudio_CRAN_data)

barplot_package_users_per_day("Rcpp", RStudio_CRAN_data)

# yearly data
library(cranlogs)
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

total_downld <- cran_downloads(from = "2019-01-01", to = "2019-12-31")
setDT(total_downld)

total_downld[, `:=`(
  round_week = floor_date(date, "week"),
  round_month = floor_date(date, "month")
)]

total_downld[, .(total =sum(count))]

random_col <- function(n){
  sample(viridis::viridis(100), n)
}

total_downld[, .(count = sum(count)), round_week] %>%
  ggplot(aes(round_week, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "Packages downloads by Week on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Week classification", y="Number of downloads"
  ) + 
  theme_minimal()

total_downld[, .(count = sum(count)), round_month] %>%
  ggplot(aes(round_month, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "Packages downloads by Month on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Month classification", y="Number of downloads"
  ) + 
  theme_minimal()

# R DOWNLOADS

total_R <- cran_downloads("R", from = "2019-01-01", to = "2019-12-31")

setDT(total_R)

total_R[, `:=`(
  round_week_r = floor_date(date, "week" ),
  round_month_r = floor_date(date, "month" )
) ]

total_R[, .(total = sum(count))]

total_R[, .(count = sum(count)), round_week_r] %>%
  ggplot(aes(round_week_r, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "R downloads by Week on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Week classification", y="Number of downloads"
  ) + 
  theme_minimal()

total_R[, .(count = sum(count)), round_month_r] %>%
  ggplot(aes(round_month_r, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "R downloads by Month on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Month classification", y="Number of downloads"
  ) + 
  theme_minimal()

# r version downloads
total_R[, .(count = sum(count)), version][order(count, decreasing = TRUE)] %>%
  head(10) %>% 
  ggplot(aes(reorder(version, count), count)) +
  coord_flip() +
  geom_col(fill = random_col(1)) + 
  labs(
    title = "10 most downloaded R versions in 2019 on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "version", y="Number of downloads"
  ) + 
  theme_minimal()


# word-cloud of package keywords
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

data <- toString(package_summary)
data <- gsub('[[:punct:] ]+',' ',data)

docs <- VCorpus(VectorSource(data))

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=550, random.order=FALSE, rot.per=0.35, 
          colors=rev(colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]))

# Network-graph of package dependencies
library(miniCRAN)
library(igraph)

pkgdata <- pkgAvail(repos = c(CRAN="http://cran.revolutionanalytics.com"), 
                    type="source")
head(pkgdata[, c("Depends", "Suggests")])

tags <- c("data.table", "chron", "plyr")

set.seed(50)
plot(makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE), 
     legendPosEdge = c(-1, 1), legendPosVertex = c(1, 1), vertex.size=9)

# Most popular package - last-day, last-week, last-month
library(cranlogs)
library(cowplot)
mp_pkg_day <- cran_top_downloads(when = "last-day", count = 10)
mp_pkg_week <- cran_top_downloads(when = "last-week", count = 10)
mp_pkg_month <- cran_top_downloads(when = "last-month", count = 10)


day_plot <- ggplot(data=mp_pkg_day, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last day",
        x = "Packages", y = "Number of downloads")

week_plot <- ggplot(data=mp_pkg_week, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="#999999")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last week",
       x = "Packages", y = "Number of downloads")

month_plot <- ggplot(data=mp_pkg_month, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="#E69F00")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last month",
       x = "Packages", y = "Number of downloads")

plot_grid(day_plot, week_plot, month_plot, labels = "AUTO")



