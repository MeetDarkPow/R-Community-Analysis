library(rtweet)

creds <- read.csv('twitter_creds.csv', fileEncoding = "UTF-8-BOM")

# store api keys
api_key <- as.character(creds$API_key)
api_secret_key <- as.character(creds$API_secret)
acc_token <- as.character(creds$Access_token)
acc_secret <- as.character(creds$Access_token_secret)

token <- create_token(
  app = "RtweetsExploration",
  consumer_key = api_key,
  consumer_secret = api_secret_key
)

rt <- search_tweets(q="#rstats", n=50, include_rts = FALSE)
