### Twitter Exploration ###
api_key <- "2GeDrSFlkbzuM1IO8zgcY3FR3"
api_secret_key <- "Vk2SIjYjSLNzwWSNJJfHGbxNyiYouryIfmYvGi7ZPmaP6XFz8V"
access_token <- "1013338789580713984-MfcQumOgqWTZmovGHy2VvTli7t4jdG"
access_token_secret <- "JasINJyQ0aBcjDR5SxXT7xhiIPjRknQZVmjFTAKRFowIM"


token <- create_token(
  app = "RtweetsExploration",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## Page 1
# searching tweets
library(rtweet)
library(tidytext)
library(ggplot2)
library(dplyr)

rstats_tweets <- search_tweets(q="#rstats", retryonratelimit = T)
head(rstats_tweets)

rstats_tweets$stripped_text <- gsub("http.*","",rstats_tweets$text) 
rstats_tweets$stripped_text <- gsub("https.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("#.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("@.*","",rstats_tweets$stripped_text)

# Statistics for tweets between specified dates
rt <- as.Date(rstats_tweets$created_at)

# DAILY
daily <- as.Date("2020-02-11")
daily_number_tweets <- sum(rt == daily, na.rm = TRUE)

# WEEKLY
since <- as.Date("2020-02-07")
until <- as.Date("2020-02-13")
weekend <- as.Date(since:until, origin="1970-01-01")
weekly_number_tweets <- c()
for(i in 1:length(weekend)){
  weekly_number_tweets <- c(weekly_number_tweets, sum(rt == weekend[i], na.rm = TRUE))
}
weekly_frame <- data.frame(weekend, weekly_number_tweets)
weekly_plot <- ggplot(data=weekly_frame, aes(x=weekend, y=weekly_number_tweets, group=1)) +
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3) +
  ggtitle("#rstats tweets between 2020-02-07 and 2020-02-13") +
  xlab("Days") + ylab("Number of Tweets")

# Name of account from where tweets were from
head(rstats_tweets$screen_name)
unique(rstats_tweets$screen_name)

# Location from where tweets were from
users <- search_users("#rstats", n=500)

length(unique(users$location))

users %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - Unique Locations ")

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
       title = "Twitter user's for tweeting #rstats in top 20 unique locations ")

# text mining twitter data

library(rtweet)   # load twitter library 
library(ggplot2)  # plotting and pipes - tidyverse!
library(dplyr)

library(tidytext) # text mining library

library(igraph)   # plotting packages
library(ggraph)


rstats_tweets_clean <- rstats_tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

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

# stop_words 
data("stop_words")
head(stop_words)

nrow(rstats_tweets_clean)

rstats_tweet_words <- rstats_tweets_clean %>%
  anti_join(stop_words)

nrow(rstats_tweet_words)

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

library(widyr)

rstats_tweets_paired_words <- rstats_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

rstats_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

library(tidyr)
rstats_tweets_separated_words <- rstats_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

rstats_tweets_filtered <- rstats_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

rstats_words_counts <- rstats_tweets_filtered %>%
  count(word1, word2, sort = TRUE)
head(rstats_words_counts)

rstats_words_counts %>%
  filter(n >= 30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Climate Change",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
