# Cummulative Gender Data-Frame

library(dplyr)

load("useR2016/useR2016_s.RData")
gender_2016_df <- useR20162 %>% group_by(Q2) %>% summarise(count = n())
gender_2016_df <- data.frame(append(gender_2016_df, c(Year=2016), after = 0))
colnames(gender_2016_df) <- c('Year', 'Type', 'Count')

load("useR2017/useR2017_s.RData")
gender_2017_df <- useR20172 %>% group_by(Gender) %>% summarise(count = n())
gender_2017_df <- data.frame(append(gender_2017_df, c(Year=2017), after = 0))
colnames(gender_2017_df) <- c('Year', 'Type', 'Count')

gender_2018_df <- read.csv("useR2018/stats_from_local_organisers/genders.csv")
gender_2018_df <- data.frame(append(gender_2018_df, c(Year=2018), after = 0))
colnames(gender_2018_df) <- c('Year', 'Type', 'Count')

gender_2019_df <- read.csv("useR2019/stats_from_local_organisers/genders.csv", header = F)
gender_2019_df <- data.frame(append(gender_2019_df, c(Year=2019), after = 0))
colnames(gender_2019_df) <- c('Year', 'Type', 'Count')

load("useR2020/useR2020_s.RData")
gender_2020_df <- useR2020_s %>% group_by(`What is your gender?`) %>% summarise(count = n())
gender_2020_df <- data.frame(append(gender_2020_df, c(Year=2020), after = 0))
colnames(gender_2020_df) <- c('Year', 'Type', 'Count')

cumm_gender_df <- do.call("rbind", list(gender_2016_df, gender_2017_df, gender_2018_df,
                            gender_2019_df, gender_2020_df))


# Cummulative Levels Data-Frame


