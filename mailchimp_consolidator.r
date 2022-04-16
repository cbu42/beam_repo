# Loading libraries
library(tidyverse)
library(scales)
#Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# OUTPUT MUST HAVE
# week	
# total_recipients	
# bounces	
# unique_opens	
# open_rate	
# total_opens	
# date_last_open	
# unique_clicks	
# click_rate
# total_clicks	
# date_last_click	
# unsubs



# df with c(filename, title, 

#1. pull quarter and population from filename
#2. 

# OUTPUT VARIABLES
# initialize variables for output csv filename
df_pop <- ""
df_quarter <- ""
df_year <- ""

raw_path <- paste0(getwd(), "/raw_data/to_be_consolidated")
raw_file_names <- list.files(path=raw_path)
print(raw_file_names)

# cleaned_output <- data.frame(week=as.integer(), total_recipients=as.integer(),bounces= as.integer(),
#                              unique_opens = as.integer(),open_rate = as.double(),total_opens = as.integer(),date_last_open,
#                              unique_clicks, click_rate, total_clicks, date_last_click, unsubs) 
# 
# colnames(cleaned_output) <- c('week', 'total_recipients','bounces','unique_opens','open_rate','total_opens','date_last_open',
#                               'unique_clicks', 'click_rate', 'total_clicks', 'date_last_click', 'unsubs')


clean_week <- function(week_filename) {
  week_df <- read_csv(raw_file_names[1], col_names = c("a", "b", "c"))
  week_df <- week_df[1:20,]
  week_df$c <- NULL
  view(week_df)
  c_names <- pull(week_df, 'a')
  week_df <- t(week_df[,'b']) #we can leave a behind because we'll make that data the colnames
  colnames(week_df) <- c_names
  rm(c_names)
  #print(week_df)
  week_df <- as.data.frame(week_df)
  colnames(week_df)<-gsub(":","",as.character(colnames(week_df)))
  colnames(week_df)<-gsub(" ","_",as.character(colnames(week_df)))
  
  #week
  for (i in 1:10) { #indexes to 10 bc we have 10 weeks in the quarter
    if(grepl(paste0('Week ', as.character(i)),week_df$Title, fixed = TRUE)) {
      week_df$week <- i
    }
  }

  
  return(week_df)
}

test <- clean_week(raw_file_names[1])
view(test)


#### EXPERIMENT BEGIN
for (i in 1:length(raw_file_names)) {
  week <- clean_week(raw_file_names[i])
  cleaned_output <- rbind(cleaned_output,week)
}
cleaned_output$week = factor(cleaned_output$week, levels = c(1:10))

#### EXPERIMENT OVER

test <- raw_file_names[1]
print(test)
setwd(raw_path)
# a, b, and c are just dummy variables that won't matter soon
test <- read_csv(raw_file_names[1], col_names = c("a", "b", "c"))
test <- test[1:20,]
test$c <- NULL
view(test)
test2 <- t(test[,'b']) #we can leave a behind because we'll make that data the colnames
colnames(test2) <- pull(test, 'a')
view(test2)
# rm(test)
test2 <- as.data.frame(test2)
colnames(test2)<-gsub(":","",as.character(colnames(test2)))
colnames(test2)<-gsub(" ","_",as.character(colnames(test2)))
#if(test2$Title


#week
for (i in 1:10) { #indexes to 10 bc we have 10 weeks in the quarter
  if(grepl(paste0('Week ', as.character(i)),test2$Title, fixed = TRUE)) {
    test2$week <- i
  }
}
test2$week = factor(test2$week, levels = c(1:10))

## PULL GLOBAL VARIABLES

#year
df_year <- sub(".*\\'", '', test2$Title)
df_year

#population
df_pop <- case_when(grepl("Eng", test2$Title) ~ 'eng', 
                   grepl("H&S", test2$Title) ~ 'hs',
                   grepl("Frosh", test2$Title) ~'fs')
#quarter
df_quarter <- case_when(grepl("Autumn", test2$Title) ~ 'aut', 
                    grepl("Winter", test2$Title) ~ 'win',
                    grepl("Spring", test2$Title) ~'spr')


output_filename <- paste0(df_pop, '_', df_quarter, df_year)

#clean_week_dfs
cleaned_output <- rbind(hs_a20, hs_w21, hs_s21, eng_a20, eng_w21, eng_s21, fs_a20, fs_w21, fs_s21)

# 
# 
# 
# 
# 
# # Reading in data as csv files. 
# hs_a20 <- read_csv("clean_data/hs_aut20_csv.csv")
# hs_w21 <- read_csv("clean_data/hs_win21_csv.csv")
# hs_s21 <- read_csv("clean_data/hs_spr21_csv.csv")
# eng_a20 <- read_csv("clean_data/eng_aut20_csv.csv")
# eng_w21 <- read_csv("clean_data/eng_win21_csv.csv")
# eng_s21 <- read_csv("clean_data/eng_spr21_csv.csv")
# fs_a20 <- read_csv("clean_data/fs_aut20_csv.csv")
# fs_w21 <- read_csv("clean_data/fs_win21_csv.csv")
# fs_s21 <- read_csv("clean_data/fs_spr21_csv.csv")
# 
# # cleaning/categorizing
# #quarter
# hs_a20$quarter <- "Autumn"
# hs_w21$quarter <- "Winter"
# hs_s21$quarter <- "Spring"
# eng_a20$quarter <- "Autumn"
# eng_w21$quarter <- "Winter"
# eng_s21$quarter <- "Spring"
# fs_a20$quarter <- "Autumn"
# fs_w21$quarter <- "Winter"
# fs_s21$quarter <- "Spring"

# 
# 
# #term
# hs_a20$term <- "aut20"
# hs_w21$term <- "win21"
# hs_s21$term <- "spr21"
# eng_a20$term <- "aut20"
# eng_w21$term <- "win21"
# eng_s21$term <- "spr21"
# fs_a20$term <- "aut20"
# fs_w21$term <- "win21"
# fs_s21$term <- "spr21"
# #AY
# hs_a20$academic_year <- 2021
# hs_w21$academic_year <- 2021
# hs_s21$academic_year <- 2021
# eng_a20$academic_year <- 2021
# eng_w21$academic_year <- 2021
# eng_s21$academic_year <- 2021
# fs_a20$academic_year <- 2021
# fs_w21$academic_year <- 2021
# fs_s21$academic_year <- 2021
# 
# #population
# hs_a20$population <- "h&s"
# hs_w21$population <- "h&s"
# hs_s21$population <- "h&s"
# eng_a20$population <- "engineering"
# eng_w21$population <- "engineering"
# eng_s21$population <- "engineering"
# fs_a20$population <- "frosh/soph"
# fs_w21$population <- "frosh/soph"
# fs_s21$population <- "frosh/soph"
# 
# df <- rbind(hs_a20, hs_w21, hs_s21, eng_a20, eng_w21, eng_s21, fs_a20, fs_w21, fs_s21)
# 
# init_q_week <- function(df) {
#   q <- substring(df$quarter, 1, 1)
#   q_week <- paste0(rep(q),df$week)
# }
# df$q_week <- init_q_week(df)
# alevels <- paste0(rep('A',10), c(1:10))
# wlevels <- paste0(rep('W',10), c(1:10))
# slevels <- paste0(rep('S',10), c(1:10))
# qweek_levels <- append(alevels, append(wlevels, slevels, after = length(wlevels)), after = length(alevels))
# df$q_week = factor(df$q_week, levels = qweek_levels)
# 
# view(df)
# 
# #by pop
# eng_df <- subset(df, population=='engineering')
# fs_df <- subset(df, population=='frosh/soph')
# hs_df <- subset(df, population=='h&s')
# 
# #means
# mean(eng_df$open_rate) #.308
# mean(eng_df$click_rate) #.0565
# mean(hs_df$open_rate) #.333
# mean(hs_df$click_rate) #.0570
# mean(fs_df$open_rate) #0.4039751
# mean(fs_df$click_rate) #0.08682566
# 
# 
# 
# 
# 
# 
