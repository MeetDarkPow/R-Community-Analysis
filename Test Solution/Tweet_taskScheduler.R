library(taskscheduleR)

### remember to change the path of Test2.R file path according to your system

test2_script <- "C:\\Users\\MeetDarkPow\\Desktop\\R_Community_Explorer\\Test2.R"

# Schedulded to run every day at 6:00 AM
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
