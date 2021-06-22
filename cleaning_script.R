#first create a dataframe
#import all files and add each comment to the bottom
library(tidyverse)
library(readr)
library(plyr)
library(lubridate)

#DATA CLEANING
#combine all comments into one data frame
setwd("C:/Users/karti/R/wsb/data_in")
comment_data <- ldply(list.files(), read.csv, header=TRUE)

#filter comment data to keep only relevant columns
comment_data <- comment_data[, -c((1:17),(19:23),(25:27),(29),(31:45))]

#shorten the name of a column using substring
comment_data$link_id <- substr(comment_data$link_id, 4, 9)

#renaming dataframe
colnames(comment_data)[4] <- "sub_id"
colnames(comment_data) [2] <-"created"

#cleaning submissions1
submissions <- read.csv("C:/Users/karti/R/wsb/submissions.csv")
colnames(submissions)[c(3,4)] <- c("sub_created", "sub_id")
submissions <- submissions[ , -c((1:2), 5, (7:9))]

#left_joining submissions to comment_Data
comment_data <- left_join(comment_data, submissions, by = 'sub_id')
comment_data$created <- as_datetime(comment_data$created)
comment_data$sub_created <- as_datetime(comment_data$sub_created)
write.csv(comment_data, "comment_data.csv")

