install.packages("rtweet")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidytext")
install.packages("igraph")
install.packages("ggraph")
install.packages("widyr")
install.packages("tidyr")

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph) 
library(ggraph)
library(widyr)
library(tidyr)

# API Key - Vasln0HaAY8pkXu7J2xQkBX8c
# API key secret - h5ra23jvrY8nq7vkUBld1mXXYwjsKR5iyNoH2CATUNKO1rdzq3

# searching for #rstats tweets 
rstats_tweets <- search_tweets(q="#rstats", retryonratelimit = T)
head(rstats_tweets)

# cleaning tweets
# storing a cleaned tweet in a separate column named "stripped_text"
rstats_tweets$stripped_text <- gsub("http.*","",rstats_tweets$text) 
rstats_tweets$stripped_text <- gsub("https.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("#.*","",rstats_tweets$stripped_text)
rstats_tweets$stripped_text <- gsub("@.*","",rstats_tweets$stripped_text)

###################################################################################################################################

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
  xlab("Days") + ylab("Number of Tweets")

###################################################################################################################################

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
       title = "Twitter user's for tweeting #rstats in top 20 unique locations ")

###################################################################################################################################

# text mining twitter data

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

# number of rows, text has before removal of stop word
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

###################################################################################################################################

# Word Network

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
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Climate Change",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
###################################################################################################################################
