# Loading libraries
library(tidyverse)
library(scales)
#Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(paste0("This script (mailchimp_consolidator.r) is stored locally at: ", getwd()))
print(paste0("Setting working directory to: ", getwd()))

raw_path <- paste0(getwd(),"/raw_data/to_be_consolidated")
write_path <- paste0(getwd(),"/clean_data/")
raw_file_names <- list.files(path=raw_path)
print(raw_file_names)


# OUTPUT VARIABLES
# initialize variables for output csv filename
df_pop <- ""
df_quarter <- ""
df_year <- ""

##Initial
test2 <- read_csv(paste0(raw_path,"/", raw_file_names[1]), col_names = c("a", "b"))
test2 <- test2[1:20,]
c_names <- pull(test2, 'a')
test2 <- t(test2[,'b']) #we can leave a behind because we'll make that data the colnames
colnames(test2) <- c_names
rm(c_names)
test2 <- as.data.frame(test2)
#clean
colnames(test2)<-gsub(":","",as.character(colnames(test2)))
colnames(test2)<-gsub(" ","_",as.character(colnames(test2)))
for (i in 1:10) { #indexes to 10 bc we have 10 weeks in the quarter
  if(grepl(paste0('Week ', as.character(i)),test2$Title, fixed = TRUE)) {
    test2$week <- i
  }
}

#global vars
#year
df_year <- sub(".*\\'", '', test2$Title)
#population
df_pop <- case_when(grepl("Eng", test2$Title) ~ 'eng', 
                    grepl("H&S", test2$Title) ~ 'hs',
                    grepl("Frosh", test2$Title) ~'fs')
#quarter
df_quarter <- case_when(grepl("Autumn", test2$Title) ~ 'aut', 
                        grepl("Winter", test2$Title) ~ 'win',
                        grepl("Spring", test2$Title) ~'spr')


output_filename <- paste0(df_pop, '_', df_quarter, df_year, ".csv")

#at this point, we have a transposed df with unpolished column names
#loops that add weekly data can proceed from here

test_clean <- function(filename) {
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

# MAIN FUNCTION
for (i in 1:(length(raw_file_names)-1)) {
  week <- test_clean(raw_file_names[i+1])
  #print(raw_file_names[i+1])
  #print(i)
  #cleaned_output <- rbind(cleaned_output,week)
  test2 <- rbind(test2,week)
  i = i+1
}

############ CLEANING
### Drop un-needed columns
drops <- c("Email_Campaign_Report", "Title", "Subject_Line", "Delivery_Date/Time", "Overall_Stats", "Successful_Deliveries",  
           "Times_Forwarded", "Forwarded_Opens", "Total_Abuse_Complaints", "Times_Liked_on_Facebook", "Clicks_by_URL")
test2 <- test2[ , !(names(test2) %in% drops)]

### Manipulate data types
# week
test2$week <- as.integer(test2$week)
test2 <- arrange(test2, week) #sorts dataframe by week
# total_recipients	
#test2$total_recipients <- sub(" \\(.*", '', test2$Total_Recipients)
test2$total_recipients <- parse_number(test2$Total_Recipients)
test2$Total_Recipients <- NULL
# bounces	
test2$bounces <- parse_number(test2$Bounces)
test2$Bounces <- NULL
# unique_opens
test2$unique_opens <- parse_number(test2$Recipients_Who_Opened)
test2$Recipients_Who_Opened <- NULL
# open_rate
test2$open_rate <- (test2$unique_opens)/(test2$total_recipients)
# total_opens
test2$total_opens <- parse_number(test2$Total_Opens)
test2$Total_Opens <- NULL
# date_last_open
test2$date_last_open <- as.Date.character(gsub(" .*", "", test2$Last_Open_Date), format = "%m/%d/%y")
test2$Last_Open_Date <- NULL
# unique_clicks
test2$unique_clicks <- parse_number(test2$Recipients_Who_Clicked)
test2$Recipients_Who_Clicked <- NULL
# click_rate
test2$click_rate <- (test2$unique_clicks)/(test2$unique_opens)
# total_clicks
test2$total_clicks <- parse_number(test2$Total_Clicks)
test2$Total_Clicks <- NULL
# date_last_click	
test2$date_last_click <- as.Date.character(gsub(" .*", "", test2$Last_Click_Date), format = "%m/%d/%y")
test2$Last_Click_Date <- NULL
# unsubs
test2$unsubs <- parse_number(test2$Total_Unsubs)
test2$Total_Unsubs <- NULL

write_csv(test2, paste0(write_path, output_filename), append = FALSE, col_names = TRUE)


