# R-Community-Exploration
---
## Details of coding project

### A. CRAN Exploration

Package Dependencies : *rvest*, *installr*, *ggplot2*, *data.table*, *cranlogs*, *lubridate*, *magrittr*, *tm*, *wordcloud*, *RColorBrewer*, *SnowballC*, *miniCRAN*, *igraph*, *cowplot*, *NLP*, *xml2*. <br>

**All of the CRAN Exploration till now has been divided into Eight Sections**

* Web scraping - Names of packages and respective summary 
* Analysis of package statistics - Short duration data
* Analysis of package statistics - Yearly data
* Most popular package - last-day, last-week, last-month
* Statistics - R software downloaded
* Statistics - R version downloaded
* Word-cloud of popular package keywords
* Network-graph of package dependencies

Let's see a detailed view of these following sections :-

#### **Web scraping - Names of packages and respective summary**

Here's my idea: We know that, whenever a new package is added to R CRAN package list it will be available on the website (i.e https://cran.r-project.org/web/packages/). So we can directly web scrap the package name and their respective summary from this site. This results in our package list to be always updated. 
Presently we get a total of 15,389 packages available.

```
install.packages("rvest")
install.packages("xml2")

library(rvest)
library(xml2)

CRAN_package_wbpg <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
  
# storing and cleaning package names
package_names <- CRAN_package_wbpg %>%
  html_nodes("a") %>%
  html_text()
package_names <- package_names[!package_names %in% LETTERS]

# storing and cleaning package summary
package_summary <- CRAN_package_wbpg %>%
  html_nodes("td+ td") %>%
  html_text()
package_summary <- gsub("[\r\n]", "", package_summary)
  
# creating a data frame 
CRAN_package_data <- data.frame(
  Name = package_names, Summary = package_summary)
```
Output Image: <br>
<img src="Images/CRAN_pkg_data.png" height="50%" width="50%"> <br>

#### **Analysis of package statistics - Short duration data**

In this section we can study the day to day top package downloads from CRAN. The line plot using the function lineplot_package_downloads() produces a multiple time series plot for the top six packages. The bar plot is used to compare any two packages together for number of download analysis.

```
install.packages("installr")
install.packages("ggplot2")
install.packages("data.table")
install.packages("plyr")

library(installr)
library(ggplot2)
library(data.table)
library(plyr)

# set your working directory and package stats file will be stored in folder "Logs"
dir.create("Logs")
RStudio_CRAN_dir <- download_RStudio_CRAN_data(START = '2020-01-01',END = '2020-01-10', log_folder="Logs")

RStudio_CRAN_data <- read_RStudio_CRAN_data(RStudio_CRAN_dir)

dim(RStudio_CRAN_data)

# creating most downloaded package list
pkg_list <- most_downloaded_packages(RStudio_CRAN_data)
pkg_list

lineplot_package_downloads(names(pkg_list),RStudio_CRAN_data)

# bar plot for day to day package download stats comparison for "ggplot" and "Rcpp"
par(mfrow=c(1,2))
barplot_package_users_per_day("ggplot",RStudio_CRAN_data)
barplot_package_users_per_day("Rcpp", RStudio_CRAN_data)
```
Output Plots: <br>
(i) Line plot for top six package downloads from '2020-01-01' till '2020-01-10': <br>
<img src="Images/lineplot.png" height="70%" width="70%"> <br>
(ii) Bar plot for day to day package download statistics for "ggplot" and "Rcpp": <br>
<img src="Images/d2d_pkg_stat.png" height="100%" width="100%"> <br>

#### **Analysis of package statistics - Yearly data**

Here we have used {cranlogs} package to retrieve the data from the RStudio CRAN mirror. This section generates two graph plots based on week and month analysis.

```
install.packages("cranlogs")
install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("magrittr")

library(cranlogs)
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

# assigned a certain time period
total_downld <- cran_downloads(from = "2019-01-01", to = "2019-12-31")
# setting mode to data.table
setDT(total_downld)
# converting the date to week and month format
total_downld[, `:=`(
  round_week = floor_date(date, "week"),
  round_month = floor_date(date, "month")
)]

# total downloads
total_downld[, .(total =sum(count))]

# creating a random color generator for graph plot
random_col <- function(n){
  sample(viridis::viridis(100), n)
}

# Weekly download analysis 
total_downld[, .(count = sum(count)), round_week] %>%
  ggplot(aes(round_week, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "Packages downloads by Week on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Week classification", y="Number of downloads"
  ) + 
  theme_minimal()

# Monthly download analysis 
total_downld[, .(count = sum(count)), round_month] %>%
  ggplot(aes(round_month, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "Packages downloads by Month on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Month classification", y="Number of downloads"
  ) + 
  theme_minimal()
```
Output Plots: <br>
(i) Weekly package download analysis from '2019-01-01' till '2019-12-31': <br>
<img src="Images/pkg_downld_week.png" height="80%" width="80%"> <br>
(ii) Monthly package download analysis from '2019-01-01' till '2019-12-31': <br>
<img src="Images/pkg_downld_month.png" height="80%" width="80%"> <br>

#### **Most popular package - last-day, last-week, last-month** 

```
install.packages("cranlogs")
install.packages("cowplot")

library(cranlogs)
library(cowplot)

# storing number of download's for packages in last-day, last-week, last-month
mp_pkg_day <- cran_top_downloads(when = "last-day", count = 10)
mp_pkg_week <- cran_top_downloads(when = "last-week", count = 10)
mp_pkg_month <- cran_top_downloads(when = "last-month", count = 10)

# last-day plot
day_plot <- ggplot(data=mp_pkg_day, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last day",
        x = "Packages", y = "Number of downloads")

# last-week plot
week_plot <- ggplot(data=mp_pkg_week, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="#999999")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last week",
       x = "Packages", y = "Number of downloads")

# last-month plot
month_plot <- ggplot(data=mp_pkg_month, aes(x=package, y=count)) +
  geom_bar(stat="identity", fill="#E69F00")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Most popular packages downloaded on last month",
       x = "Packages", y = "Number of downloads")

# plotting graph together using library "cowplot" 
plot_grid(day_plot, week_plot, month_plot, labels = "AUTO")
```
Output plot: <br>
<img src="Images/mp_pkg_day_week_month.png" height="100%" width="100%"> <br>

#### **Statistics - R software downloaded**

This section will provide a look at the number of downloads for R.

```
install.packages("cranlogs")
install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("magrittr")

library(cranlogs)
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

# assigned a certain time period
total_R <- cran_downloads("R", from = "2019-01-01", to = "2019-12-31")
# setting mode to data.table
setDT(total_R)
# converting the date to week and month format
total_R[, `:=`(
  round_week_r = floor_date(date, "week" ),
  round_month_r = floor_date(date, "month" )
) ]

# total downloads
total_R[, .(total = sum(count))]

# Weekly download analysis 
total_R[, .(count = sum(count)), round_week_r] %>%
  ggplot(aes(round_week_r, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "R downloads by Week on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Week classification", y="Number of downloads"
  ) + 
  theme_minimal()

# Monthly download analysis 
total_R[, .(count = sum(count)), round_month_r] %>%
  ggplot(aes(round_month_r, count)) + 
  geom_col(fill = random_col(1)) + 
  labs(
    title = "R downloads by Month on RStudio CRAN mirror", 
    subtitle = "data via {cranlogs}", 
    x = "Month classification", y="Number of downloads"
  ) + 
  theme_minimal()
```
Output Plots: <br>
(i) Weekly package download analysis from '2019-01-01' till '2019-12-31': <br>
<img src="Images/r_downld_week.png" height="80%" width="80%"> <br>
(ii) Monthly package download analysis from '2019-01-01' till '2019-12-31': <br>
<img src="Images/r_downld_month.png" height="80%" width="80%"> <br>

#### **Statistics - R version downloaded**

Let’s have a look to the number of download by R version:

```
install.packages("cranlogs")
install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("magrittr")

library(cranlogs)
library(data.table)
library(lubridate)
library(ggplot2)
library(magrittr)

# R version plot analysis
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
```
Output plot: <br>
<img src="Images/r_version_downld.png" height="80%" width="80%"> <br>

#### **Word-cloud of popular package keywords**

Text mining methods allow us to highlight the most frequently used keywords in a paragraph of texts. One can create a word cloud, also referred as text cloud or tag cloud, which is a visual representation of text data.

```
install.packages("NLP")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")

library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

# converting data to string
data <- toString(package_summary)
data <- gsub('[[:punct:] ]+',' ',data)

# cleaning data
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

# creating a matrix for tabulation of word with respective frequency 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# plotiing word-cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=550, random.order=FALSE, rot.per=0.35, 
          colors=rev(colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]))
```
Output plot:
<img src="Images/word_cloud.png" height="80%" width="80%"> <br>

#### **Network-graph of package dependencies**

In this section, we will view an example for package dependencies for these 3 packages:
* "data.table"
* "chron"
* "plyr"

```
install.packages("miniCRAN")
install.packages("igraph")

library(miniCRAN)
library(igraph)

# retrieving the available packages on CRAN
pkgdata <- pkgAvail(repos = c(CRAN="http://cran.revolutionanalytics.com"), type="source")
head(pkgdata[, c("Depends", "Suggests")])

# packages to check dependencies on
tags <- c("data.table", "chron", "plyr")

# plotting network graph
set.seed(50)
plot(makeDepGraph(tags, includeBasePkgs=FALSE, suggests=TRUE, enhances=TRUE), 
     legendPosEdge = c(-1, 1), legendPosVertex = c(1, 1), vertex.size=9)
```
Output plot:
<img src="Images/Network_Package_Graph.png" height="80%" width="80%"> <br>

### B. Twitter Exploration

Package Dependencies : *rtweet*, *tidytext*, *ggplot2*, *dplyr*, *igraph*, *ggraph*, *widyr*, *tidyr*. <br>

**All of the Twitter Exploration till now has been divided into Five Sections**

* Searching and text mining twitter data
* Statistics for tweets between specified dates
* Twitter users - unique locations
* Count of unique words found in tweets
* Word network of tweets

Let's see a detailed view of these following sections :-

#### **Searching and text mining twitter data**

```
install.packages("rtweet")

library(rtweet)

# searching for #rstats tweets 
rstats_tweets <- search_tweets(q="#rstats", retryonratelimit = T)
head(rstats_tweets)

# cleaning tweets
# storing a cleaned tweet in a separate column named "stripped_text"
rstats_tweets$stripped_text <- gsub("http.*","",rstats_tweets$text) 
rstats_tweets$stripped_text <- gsub("https.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("#.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("@.*","",rstats_tweets$stripped_text)
```

#### **Statistics for tweets between specific dates**

```
install.packages("ggplot2")

library(ggplot2)

# past tweets date
rt <- as.Date(rstats_tweets$created_at)

# daily number of tweets
daily <- as.Date("2020-02-11")
daily_number_tweets <- sum(rt == daily, na.rm = TRUE)

# tweets between specific dates
since <- as.Date("2020-02-10")
until <- as.Date("2020-02-15")
weekend <- as.Date(since:until, origin="1970-01-01")
weekly_number_tweets <- c()
for(i in 1:length(weekend)){
  weekly_number_tweets <- c(weekly_number_tweets, sum(rt == weekend[i], na.rm = TRUE))
}
weekly_frame <- data.frame(weekend, weekly_number_tweets)

# ploting 
weekly_plot <- ggplot(data=weekly_frame, aes(x=weekend, y=weekly_number_tweets, group=1)) +
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3) +
  ggtitle("#rstats tweets between 2020-02-10 and 2020-02-15") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab("Days") + ylab("Number of Tweets")
```
Output plot: <br>
<img src="Images/rt_date_specific.png" height="50%" width="50%"> <br>

#### **Twitter users - unique locations**

```
install.packages("rtweet")
install.packages("ggplot2")

library(rtweet)
library(ggplot2)

# Name of account from where tweets were from
head(rstats_tweets$screen_name)
unique(rstats_tweets$screen_name)

# location from where tweets were from
users <- search_users("#rstats", n=500)

length(unique(users$location))

# ploting Twitter users - Unique Locations
users %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - Unique Locations ")

# ploting Twitter user's for tweeting #rstats in top 50 unique locations
users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(50) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter user's for tweeting #rstats in top 50 unique locations ")
```
Output Plots: <br>
(i) Twitter users - Unique Locations: <br>
<img src="Images/twitter_user_uniq_loc.png" height="100%" width="100%"> <br>
(ii) Twitter user's for tweeting #rstats in top 50 unique locations: <br>
<img src="Images/twitter_top_user_loc.png" height="100%" width="100%"> <br>

#### **Count of unique words found in tweets** 

```
install.packages("rtweet")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidytext")
install.packages("igraph")
install.packages("ggraph")

library(rtweet)   
library(ggplot2) 
library(dplyr)
library(tidytext) 
library(igraph)   
library(ggraph)

# selecting clean text
rstats_tweets_clean <- rstats_tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# ploting Count of unique words in tweets {Stop words present}
rstats_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) + 
  geom_col() +
  xlab(NULL)+
  coord_flip() +
  labs(x="count",
       y= "Unique words",
       title = "Count of unique words in tweets")

# searching what are stop_words 
data("stop_words")
head(stop_words)

# number of rows, text has before removal of stop words
nrow(rstats_tweets_clean)

# removing stop words
rstats_tweet_words <- rstats_tweets_clean %>%
  anti_join(stop_words)
 
# number of rows, text has after removal of stop words
nrow(rstats_tweet_words)

# WE CAN LITERALLY SEE THE DIFFERENCE IN NUMBER OF ROWS

# ploting Count of unique words in tweets {Stop words removed}
rstats_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")
```
Output Plots: <br>
(i) Count of unique words in tweets {Stop words present}: <br>
<img src="Images/countuniq_wstop_words.png" height="50%" width="50%"> <br>
(ii) Count of unique words found in tweets {Stop words removed}: <br>
<img src="Images/countuniq_stop_words.png" height="50%" width="50%"> <br>


#### **Word network of tweets**

```
install.packages("widyr")
install.packages("tidyr")

library(widyr)
library(tidyr)

# forming pair-words
rstats_tweets_paired_words <- rstats_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)
# count of pair-words
rstats_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

# separating pair-words into 2 columns
rstats_tweets_separated_words <- rstats_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

rstats_tweets_filtered <- rstats_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

rstats_words_counts <- rstats_tweets_filtered %>%
  count(word1, word2, sort = TRUE)
head(rstats_words_counts)

# ploting word network for #rstats
rstats_words_counts %>%
  filter(n >= 30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - rstats",
     subtitle = "Text mining twitter data ",
       x = "", y = "")
```
Output plot: <br>
<img src="Images/word_network.png" height="80%" width="80%"> <br>

### C. GitHub Exploration

| **CAUTION** | **!!! Work under progress !!!** |
|-------------|--------------------------------:|

---
  Thanks for reading!! - MEET BHATNAGAR


