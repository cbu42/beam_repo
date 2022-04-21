# Loading libraries
library(tidyverse)
#Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(paste0("This script (mailchimp_consolidator.r) is stored locally at: ", getwd()))
print(paste0("Setting working directory to: ", getwd()))
raw_path <- paste0(getwd(),"/raw_data/to_be_consolidated")
write_path <- paste0(getwd(),"/clean_data/")
raw_file_names <- list.files(path=raw_path)
print(raw_file_names)

# INITIALIZES GLOBAL VARIABLES
df <- read_csv(paste0(raw_path,"/", raw_file_names[1]), col_names = c("a", "b"))
df_pop <- ""
df_quarter <- ""
df_year <- ""
output_filename <- ""

## BUILDS FILENAME BASED ON NEWSLETTER TITLES
global_vars <- function(df) {
  df_year <- sub(".*\\'", '', df$Title)
  #population
  df_pop <- case_when(grepl("Eng", df$Title) ~ 'eng', 
                      grepl("H&S", df$Title) ~ 'hs',
                      grepl("Frosh", df$Title) ~'fs')
  #quarter
  df_quarter <- case_when(grepl("Autumn", df$Title) ~ 'aut', 
                          grepl("Winter", df$Title) ~ 'win',
                          grepl("Spring", df$Title) ~'spr')
  
  
  output_filename <- paste0(df_pop, '_', df_quarter, df_year, ".csv")
  return(output_filename)
}

## CLEANS A SINGLE WEEK OF DATA
clean_week <- function(filename) {
  week_df <- read_csv(paste0(raw_path,"/", filename), col_names = c("a", "b"))
  week_df <- week_df[1:20,]
  #view(week_df)
  c_names <- pull(week_df, 'a')
  week_df <- t(week_df[,'b']) #we can leave a behind because we'll make that data the colnames
  colnames(week_df) <- c_names
  rm(c_names)
  #print(week_df)
  week_df <- as.data.frame(week_df)
  colnames(week_df)<-gsub(":","",as.character(colnames(week_df)))
  colnames(week_df)<-gsub(" ","_",as.character(colnames(week_df)))
  for (i in 1:10) { #indexes to 10 bc we have 10 weeks in the quarter
    if(grepl(paste0('Week ', as.character(i)),week_df$Title, fixed = TRUE)) {
      week_df$week <- i
    }
  }
  return(week_df)
}

## BINDING FUNCTION
bind_weeks <- function(raw_file_names, df) {
  for (i in 1:(length(raw_file_names)-1)) {
    week <- clean_week(raw_file_names[i+1])
    #print(raw_file_names[i+1])
    #print(i)
    #cleaned_output <- rbind(cleaned_output,week)
    df <- rbind(df,week)
    i = i+1
  }
  return(df)
}

## CLEANS DF
clean_df <- function(df) {
  ### Drop un-needed columns
  drops <- c("Email_Campaign_Report", "Title", "Subject_Line", "Delivery_Date/Time", "Overall_Stats", "Successful_Deliveries",  
             "Times_Forwarded", "Forwarded_Opens", "Total_Abuse_Complaints", "Times_Liked_on_Facebook", "Clicks_by_URL")
  df <- df[ , !(names(df) %in% drops)]
  ### Manipulate data types
  # week
  df$week <- as.integer(df$week)
  df <- arrange(df, week) #sorts dataframe by week
  # total_recipients	
  #df$total_recipients <- sub(" \\(.*", '', df$Total_Recipients)
  df$total_recipients <- parse_number(df$Total_Recipients)
  df$Total_Recipients <- NULL
  # bounces	
  df$bounces <- parse_number(df$Bounces)
  df$Bounces <- NULL
  # unique_opens
  df$unique_opens <- parse_number(df$Recipients_Who_Opened)
  df$Recipients_Who_Opened <- NULL
  # open_rate
  df$open_rate <- (df$unique_opens)/(df$total_recipients)
  # total_opens
  df$total_opens <- parse_number(df$Total_Opens)
  df$Total_Opens <- NULL
  # date_last_open
  df$date_last_open <- as.Date.character(gsub(" .*", "", df$Last_Open_Date), format = "%m/%d/%y")
  df$Last_Open_Date <- NULL
  # unique_clicks
  df$unique_clicks <- parse_number(df$Recipients_Who_Clicked)
  df$Recipients_Who_Clicked <- NULL
  # click_rate
  df$click_rate <- (df$unique_clicks)/(df$unique_opens)
  # total_clicks
  df$total_clicks <- parse_number(df$Total_Clicks)
  df$Total_Clicks <- NULL
  # date_last_click	
  df$date_last_click <- as.Date.character(gsub(" .*", "", df$Last_Click_Date), format = "%m/%d/%y")
  df$Last_Click_Date <- NULL
  # unsubs
  df$unsubs <- parse_number(df$Total_Unsubs)
  df$Total_Unsubs <- NULL
  return(df)
}

## MAIN FUNCTION
main_function <- function(raw_file_names, df) {
  df <- df[1:20,]
  c_names <- pull(df, 'a')
  df <- t(df[,'b']) #we can leave a behind because we'll make that data the colnames
  colnames(df) <- c_names
  rm(c_names)
  df <- as.data.frame(df)
  #clean
  colnames(df)<-gsub(":","",as.character(colnames(df)))
  colnames(df)<-gsub(" ","_",as.character(colnames(df)))
  for (i in 1:10) { #indexes to 10 bc we have 10 weeks in the quarter
    if(grepl(paste0('Week ', as.character(i)),df$Title, fixed = TRUE)) {
      df$week <- i
    }
  }
  output_filename <-  global_vars(df)
  df <- bind_weeks(raw_file_names, df)
  df <- clean_df(df)
  write_csv(df, paste0(write_path, output_filename), append = FALSE, col_names = TRUE)
  print(paste0("Check ", write_path, " for new file ", output_filename))
}

## RUN THIS FUNCTION TO WRITE CSV
main_function(raw_file_names, df)

