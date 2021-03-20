creds <- read.csv('twitter_creds.csv', fileEncoding = "UTF-8-BOM")

# store API keys
api_key <- as.character(creds$API_key)
api_secret_key <- as.character(creds$API_secret)
acc_token <- as.character(creds$Access_token)
acc_secret <- as.character(creds$Access_token_secret)

library(rtweet)

token <- create_token(
  app = "RtweetsExploration",
  consumer_key = api_key,
  consumer_secret = api_secret_key
)

rtweet_data <- search_tweets(q="#rstats", since = Sys.Date()-1, 
                             until = Sys.Date(), 
                             retryonratelimit = TRUE, include_rts = FALSE)

rtweet_df <- data.frame(User_ID=rtweet_data$user_id, Status_ID=rtweet_data$status_id,
                        Tweet_Date=rtweet_data$created_at, Tweet_Text=rtweet_data$text, 
                        Retweet_Count=rtweet_data$retweet_count, Likes = rtweet_data$favorite_count)

write.table(rtweet_df, file = "rstats_Tweet.csv", sep = ",",
            row.names = FALSE, col.names = !file.exists("rstats_Tweet.csv"), 
            append = TRUE)
