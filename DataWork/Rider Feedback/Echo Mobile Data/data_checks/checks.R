  #PSV Rider forms data checks
  
  # Clear memory
  rm(list=ls())
  
  # Install packages
  
  install.packages(c( "dplyr", "readr", "tidyverse"))
  library(dplyr)
  library(readr)
  library(tidyverse)
  library(ggplot2)
  library(lubridate)
  library(naniar)
  
  # File Path
  
  System <- Sys.getenv(x = NULL, unset = "")
  
  # MM file path
  User <- System["USER"]
  if (User == "meyhar") {
    git <- "~/Documents/GitHub/PSV-Rider-Feedback/DataWork/Rider Feedback/Echo Mobile Data/data_checks"
    dropbox <- "~/Dropbox/PSV Rider Feedback/Data"
    output <- "~/Dropbox/PSV Rider Feedback/Outputs"
  }
  
  # RM file path
  if (User == "") {
    folder <- ""
  }
  
  # Load Raw data
  echo_mobile <- readRDS(file.path(dropbox, "Rider Feedback", "Echo Mobile Data", "RawData", "echo_data.Rds"))
  
  # 1. Number of responses started per day
  countEntries <- function(timeStamps) {
    Dates <- as.Date(strftime(echo_mobile$start_date, "%Y-%m-%d"))
    allDates <- seq(from = min(Dates), to = max(Dates), by = "day")
    entry.count <- sapply(allDates, FUN = function(X) sum(Dates == X))
    data.frame(day = allDates, entry.count = entry.count)
  }

  entries_started <- countEntries(echo_mobile$start_date)
   
  # Plot number of entries per day
  
  entries_started <- subset(entries_started, day > as.Date("2020-06-25"))
  
 started_daily <-  ggplot(data=entries_started, aes(x=day, y=entry.count)) +
                   geom_bar(stat="identity", fill="steelblue")+
                   geom_text(aes(label=entry.count), vjust=-0.3, size=3.5)+
                   theme_minimal()
 
 started_daily + 
   xlab("Day") + ylab("Number of entries started")
 
 ggsave(filename = file.path(output, "Rider Feedback","Echo Mobile Data", "started_daily.png"))
 
 #2. Number of responses completed in a day
 # replace missing to NA
 subset_complete <- echo_mobile[complete.cases(echo_mobile$complete_date), ]
 
 countEntries <- function(timeStamps) {
   Dates <- as.Date(strftime(subset_complete$complete_date, "%Y-%m-%d"))
   allDates <- seq(from = min(Dates), to = max(Dates), by = "day")
   entry.count <- sapply(allDates, FUN = function(X) sum(Dates == X))
   data.frame(day = allDates, entry.count = entry.count)
 }
 
 entries_completed <- countEntries(subset_complete$complete_date)
 entries_completed <- subset( entries_completed, day > as.Date("2020-06-25"))

 completed_daily <-  ggplot(data=entries_completed, aes(x=day, y=entry.count)) +
   geom_bar(stat="identity", fill="steelblue")+
   geom_text(aes(label=entry.count), vjust=-0.3, size=3.5)+
   theme_minimal()
 
 completed_daily + 
   xlab("Day") + ylab("Number of entries completed")
 
 ggsave(filename = file.path(output, "Rider Feedback","Echo Mobile Data", "completed_daily.png"))
 