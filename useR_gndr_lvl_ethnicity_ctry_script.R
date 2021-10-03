# Cumulative Gender Data-Frame

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


# Cumulative Levels Data-Frame

level_2016_df <- useR20162 %>% group_by(Q1) %>% summarise(count = n())
level_2016_df <- data.frame(append(level_2016_df, c(Year=2016), after = 0))
colnames(level_2016_df) <- c('Year', 'Level', 'Count')

level_2017_df <- useR20172 %>% group_by(EmploymentStatus) %>% summarise(count = n())
level_2017_df <- data.frame(append(level_2017_df, c(Year=2017), after = 0))
colnames(level_2017_df) <- c('Year', 'Level', 'Count')

level_2018_df <- read.csv("useR2018/stats_from_local_organisers/levels.csv")
level_2018_df <- data.frame(append(level_2018_df, c(Year=2018), after = 0))
colnames(level_2018_df) <- c('Year', 'Level', 'Count')

level_2019_df <- read.csv("useR2019/stats_from_local_organisers/levels.csv", header = F)
level_2019_df <- data.frame(append(level_2019_df, c(Year=2019), after = 0))
colnames(level_2019_df) <- c('Year', 'Level', 'Count')

level_2020_df <- useR2020_s %>% group_by(`What is your current employment status? If you are employed in multiple sectors or both work and study, select all that apply:`) %>% summarise(count = n())
level_2020_df <- data.frame(append(level_2020_df, c(Year=2020), after = 0))
colnames(level_2020_df) <- c('Year', 'Level', 'Count')

level_2021_df <- read.csv("useR2021/stats_from_local_organisers/levels.csv")
level_2021_df <- data.frame(append(level_2021_df, c(Year=2021), after = 0))
colnames(level_2021_df) <- c('Year', 'Level', 'Count')

cumm_level_df <- do.call("rbind", list(level_2016_df, level_2017_df, level_2018_df,
                                       level_2019_df, level_2020_df, level_2021_df))


# Cumulative Ethnicity Type Data-Frame

ethn_2016_df <- useR20162 %>% group_by(`survey_data$Q4`) %>% summarise(count = n())
ethn_2016_df <- data.frame(append(ethn_2016_df, c(Year=2016), after = 0))
colnames(ethn_2016_df) <- c('Year', 'Ethnicity', 'Count')

ethn_2017_df <- useR20172 %>% group_by(`useR2017_survey$EthnicGroup`) %>% summarise(count = n())
ethn_2017_df <- data.frame(append(ethn_2017_df, c(Year=2017), after = 0))
colnames(ethn_2017_df) <- c('Year', 'Ethnicity', 'Count')
ethn_2017_df <- ethn_2017_df[-c(1:4),]

load("useR2018/stats_from_local_organisers/useR2018_ethnicity.RData")
ethn_2018_df <- user2018 %>% group_by(`To what ethnic group(s) do you identify?`) %>% summarise(count = n())
ethn_2018_df <- data.frame(append(ethn_2018_df, c(Year=2018), after = 0))
colnames(ethn_2018_df) <- c('Year', 'Ethnicity', 'Count')

load("useR2020/useR2020_ethnicity.RData")
ethn_2020_df <- which_ethnicity %>% group_by(ethnicity_group) %>% summarise(count = n())
ethn_2020_df <- data.frame(append(ethn_2020_df, c(Year=2020), after = 0))
colnames(ethn_2020_df) <- c('Year', 'Ethnicity', 'Count')

cumm_ethnicity_df <- do.call("rbind", list(ethn_2016_df, ethn_2017_df,
                                           ethn_2018_df, ethn_2020_df))


# Cumulative Country Count Data-Frame

ctry_2016_df <- useR20162 %>% group_by(Q5) %>% summarise(count = n())

ctry_2017_df <- useR20172 %>% group_by(CurrentResidenceCountry) %>% summarise(count = n())

ctry_2018_df <- read.csv("useR2018/stats_from_local_organisers/countries.csv")

ctry_2019_df <- read.csv("useR2019/stats_from_local_organisers/countries.csv", header = F)

ctry_2020_df <- useR2020_s %>% group_by(`In what country do you currently reside?`) %>% summarise(count = n())

ctry_2021_df <- read.csv("useR2021/stats_from_local_organisers/countries.csv")

