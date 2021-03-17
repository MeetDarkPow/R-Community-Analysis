library(rtweet)
library(data.table)

# takes system date as input
since = Sys.Date()-1

# searches yesterday tweets
rstats_tweets <- search_tweets(q="#rstats", since = since, until = (since + 1), retryonratelimit = T)

# filtering certain fields from rstats_tweets and storing as dataframe
rstats_df <- data.frame(User.Id = as.numeric(rstats_tweets$user_id), Tweet.Date = as.character(as.IDate(rstats_tweets$created_at)),
                        Time = as.character(as.ITime(rstats_tweets$created_at)), Tweet.Text = rstats_tweets$text, 
                        Retweet.Count = rstats_tweets$retweet_count)

# creating directory folder
dir.create("Tweet_rstats_File")

# checking and writing a .csv file
if(file.exists("Tweet_rstats_File/rstatsTweet.csv")){
  initial_df <- read.csv("Tweet_rstats_File/rstatsTweet.csv", stringsAsFactors = T)
  finaldf <- do.call("rbind", list(rstats_df, initial_df))
  write.csv(finaldf, file = paste0("Tweet_rstats_File/rstatsTweet.csv"), row.names = F)
} else {
  write.csv(rstats_df, file = paste0("Tweet_rstats_File/rstatsTweet.csv"), row.names = F)
}

# updating the archive of #rstats tweets daily in the CSV file
library(taskscheduleR)

### remember to change the path/directory of searching #rstats code according to your system

test2_script <- "C:\\Users\\MeetDarkPow\\Desktop\\R_Community_Explorer\\Test2.R"

# Scheduled to run every day at 6:00 AM
taskscheduler_create(
  taskname = "Rtweet_taskscheduler",
  rscript = test2_script,
  schedule = "DAILY",
  starttime = "06:00",
  startdate = format(Sys.time(), "%d/%m/%Y"),
  Rexe = file.path(Sys.getenv("R_Home"), "bin", "Rscript.exe")
)

# function to terminate the task schedular
taskscheduler_stop("Rtweet_taskscheduler")

# function to delete the task schedular
taskscheduler_delete("Rtweet_taskscheduler")
