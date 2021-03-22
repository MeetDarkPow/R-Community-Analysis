# storing API keys
api_key <- "Vasln0HaAY8pkXu7J2xQkBX8c"
api_secret_key <- "h5ra23jvrY8nq7vkUBld1mXXYwjsKR5iyNoH2CATUNKO1rdzq3"
acc_token <- "1013338789580713984-2oRZj3Btzvgl0yZqJ8Wfr0GEmTzW34"
acc_secret <- "92oQXpMxvdNI7gwIAji2ZfEdN905eGudFoeNWd4sb6Wnn"

library(rtweet)

token <- create_token(
  app = "RtweetsExploration",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = acc_token,
  access_secret = acc_secret
)

if(!file.exists("Test_Solution_2021/rstats_data/rstats_Tweet.csv")){
  empty_df <- data.frame(User_ID = character(), Status_ID = character(),
                         Tweet_Date = character(), Tweet_Text = character(),
                         Retweet_Count = integer(), Likes = integer())
  write_as_csv(empty_df, file_name = "Test_Solution_2021/rstats_data/rstats_Tweet")
}

read_df <- read_twitter_csv("Test_Solution_2021/rstats_data/rstats_Tweet.csv")

# searching for required #rstats tweets
rtweet_data <- search_tweets(q="#rstats", since = Sys.Date()-1,
                             until = Sys.Date(),
                             retryonratelimit = TRUE, include_rts = FALSE)

# creating a structured data frame
rtweet_df <- data.frame(User_ID=rtweet_data$user_id, Status_ID=rtweet_data$status_id,
                        Tweet_Date=rtweet_data$created_at, Tweet_Text=rtweet_data$text,
                        Retweet_Count=rtweet_data$retweet_count, Likes = rtweet_data$favorite_count)

temp_df <- rbind(read_df, rtweet_df)

# saving #rstats tweets to a csv file
write_as_csv(temp_df, file_name = "Test_Solution_2021/rstats_data/rstats_Tweet")
