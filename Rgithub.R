install.packages("httr")
install.packages("jsonlite")
install.packages("xml2")
install.packages("rvest")
install.packages("formattable")
install.packages("ggplot2")
install.packages("cowplot")

library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(formattable)
library(ggplot2)
library(cowplot)

# turning off the feature of R to turn multinomial variables,
# dummy variables in regression models and produce nice cross tables
options(stringsAsFactors = FALSE)

url <- "https://api.github.com"
path <- "search/repositories?q=language:R&per_page=100&sort=stars&order=desc"

# getting data
raw.result <- GET(url = url, path = path)

# checking status code for extracting data
raw.result$status_code

# raw data
this.raw.content <- rawToChar(raw.result$content)

# creating "list" from JSON file
this.content <- fromJSON(this.raw.content)
class(this.content)

# Most starred repos
repos_names <- this.content[["items"]][["name"]]
Star_count <- this.content[["items"]][["stargazers_count"]]

# Most forked repos
Fork_count <- this.content[["items"]][["forks_count"]]

# Most open_issues repos
Openissue_count <- this.content[["items"]][["open_issues_count"]]

# creating data-frame 
Rrepos_df <- data.frame(Repository = repos_names[1:10],
                 Star = Star_count[1:10],
                 Fork = Fork_count[1:10],
                 Open_Issue = Openissue_count[1:10])

# plotting

# plotting number of stars per repositories
star_plot <- ggplot(data=Rrepos_df, aes(x=Repository, y=Star)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Star), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Top 10 repositories with most number of stars",
       x = "Repositories", y = "Number of Stars")

# plotting number of forks per repositories
fork_plot <- ggplot(data=Rrepos_df, aes(x=Repository, y=Fork)) +
  geom_bar(stat="identity", fill="darkred")+
  geom_text(aes(label=Fork), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Top 10 repositories with most forks",
       x = "Repositories", y = "Number of Forked")

# plotting number of open issues per repositories
open_issues_plot <- ggplot(data=Rrepos_df, aes(x=Repository, y=Open_Issue)) +
  geom_bar(stat="identity", fill="darkgreen")+
  geom_text(aes(label=Open_Issue), vjust=1.2, color="white", size=3)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = "Top 10 repositories with most open issues",
       x = "Repositories", y = "Number of Open Issues")

# plotting three plots together
plot_grid(star_plot, fork_plot, open_issues_plot, labels = "AUTO")


# Trending repos per day/week/month


# today
r_trending_repos_today_wbpg <- read_html("https://github.com/trending/r?since=daily")

repo_names_today <- r_trending_repos_today_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_names_today <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_names_today, perl=TRUE)

stars_gained_today <- r_trending_repos_today_wbpg %>%
  html_nodes(".float-sm-right") %>%
  html_text()
stars_gained_today <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", stars_gained_today, perl=TRUE)

# week
r_trending_repos_week_wbpg <- read_html("https://github.com/trending/r?since=weekly")

repo_names_week <- r_trending_repos_week_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_names_week <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_names_week, perl=TRUE)

stars_gained_week <- r_trending_repos_week_wbpg %>%
  html_nodes(".float-sm-right") %>%
  html_text()
stars_gained_week <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", stars_gained_week, perl=TRUE)

# month
r_trending_repos_month_wbpg <- read_html("https://github.com/trending/r?since=monthly")

repo_names_month <- r_trending_repos_month_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_names_month <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_names_month, perl=TRUE)

stars_gained_month <- r_trending_repos_month_wbpg %>%
  html_nodes(".float-sm-right") %>%
  html_text()
stars_gained_month <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", stars_gained_month, perl=TRUE)

# plotting table for trending repos

today_df <- data.frame(repo_names_today, stars_gained_today)
formattable(today_df, align = c("l", rep("r", NCOL(today_df) - 1)))

week_df <- data.frame(repo_names_week, stars_gained_week)
formattable(week_df, align = c("l", rep("r", NCOL(week_df) - 1)))

month_df <- data.frame(repo_names_month, stars_gained_month)
formattable(month_df, align = c("l", rep("r", NCOL(month_df) - 1)))

# trending developers per day/week/month


# today
r_trending_developers_today_wbpg <- read_html("https://github.com/trending/developers?since=daily")

repo_author_name_today <- r_trending_developers_today_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_author_name_today <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_author_name_today, perl=TRUE)

popular_repo_name_today <- r_trending_developers_today_wbpg %>%
  html_nodes(".css-truncate-target") %>%
  html_text()
popular_repo_name_today <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", popular_repo_name_today, perl=TRUE)
popular_repo_name_today <- popular_repo_name_today[popular_repo_name_today != ""]

# week
r_trending_developers_week_wbpg <- read_html("https://github.com/trending/developers?since=weekly")

repo_author_name_week <- r_trending_developers_week_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_author_name_week <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_author_name_week, perl=TRUE)

popular_repo_name_week <- r_trending_developers_week_wbpg %>%
  html_nodes(".css-truncate-target") %>%
  html_text()
popular_repo_name_week <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", popular_repo_name_week, perl=TRUE)
popular_repo_name_week <- popular_repo_name_week[popular_repo_name_week != ""]

# month
r_trending_developers_month_wbpg <- read_html("https://github.com/trending/developers?since=monthly")

repo_author_name_month <- r_trending_developers_month_wbpg %>%
  html_nodes(".h3 a") %>%
  html_text()
repo_author_name_month <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", repo_author_name_month, perl=TRUE)

popular_repo_name_month <- r_trending_developers_month_wbpg %>%
  html_nodes(".css-truncate-target") %>%
  html_text()
popular_repo_name_month <- gsub("(?<=[\\s])\\s*|[\r\n]|^\\s+|\\s+$", "", popular_repo_name_month, perl=TRUE)
popular_repo_name_month <- popular_repo_name_month[popular_repo_name_month != ""]

# plotting table for trending developers

today_dev_df <- data.frame(repo_author_name_today, popular_repo_name_today)
formattable(today_dev_df, align = c("l", rep("r", NCOL(today_dev_df) - 1)))

week_dev_df <- data.frame(repo_author_name_week, popular_repo_name_week)
formattable(week_dev_df, align = c("l", rep("r", NCOL(week_dev_df) - 1)))

month_dev_df <- data.frame(repo_author_name_month, popular_repo_name_month)
formattable(month_dev_df, align = c("l", rep("r", NCOL(month_dev_df) - 1)))