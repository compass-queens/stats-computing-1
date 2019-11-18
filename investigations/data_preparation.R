# This file contains modular code to prepare dataset for our analyses
suppressPackageStartupMessages(library(readr))      # to read csv file
suppressPackageStartupMessages(library(dplyr))      # pipes and data manipulation
suppressPackageStartupMessages(library(lubridate))  # to change date format

# This function can be improved, maybe could leverage some OOP?
get_data <- function(drop_cols=TRUE, rms_mean=TRUE, rm.types=TRUE, normalize=TRUE){
  # Read data, cast col names to lowercase and rename some for convenience
  df <- read_csv("../data/database.csv")
  colnames(df) <- c(
    'date', 'time', 'lat', 'lon', 'type', 'depth', 'depth_er', 'depth_ss', 
    'magnit', 'magnit_type', 'magnit_er', 'magnit_ss', 'azim', 'horiz_dist', 
    'horiz_er', 'rms', 'id', 'source', 'loc_source', 'magnit_source', 'status'
  )
  
  # Remove NA values in Time column, cast date to year/month/day and create y/m/d cols
  df <- df %>% filter(!is.na(time)) %>% as.data.frame()
  df <- df %>% mutate(date = as_date(parse_date_time(date, orders=c('mdy'))))
  df <- df %>% mutate(
    year = year(date), 
    month = factor(month(date)), 
    day = factor(day(date))
    )
  
  # Drop columns that have a high proportion of missing values
  if (drop_cols){
    df <- df %>% select(-c(
      depth_er, magnit_er, azim, horiz_er, 
      horiz_dist, magnit_type, magnit_ss, depth_ss,id)
    )
  }
  
  # Replace RMS missing values with mean
  if (rms_mean){
    df <- df %>% mutate(rms=replace(rms, is.na(rms), mean(rms, na.rm=TRUE)))
  }
  
  # Cast categorical variables to factors
  categ_var <- c('type', 'status', 'magnit_source', 'loc_source', 'source')
  df[, categ_var] <- lapply(df[, categ_var], factor)
  
  # There are few rock bursts and explosions, remove them
  if (rm.types){
    df <- df %>% filter(grepl('Earthquake|Nuclear', type)) 
  }
  
  # Divide the range of magniture into 4 categories
  df <- df %>% 
    mutate(class = factor(cut(
      df$magnit,
      breaks = c(5, 6, 7, 8, Inf),
      labels = c("moderate", "strong", "major", "great"),
      right  = FALSE)
    )
    )
  
  # Normalize Magnitude, Depth and RMS
  normalize <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
  }
  df[c('magnit','depth','rms')] <- lapply(df[c('magnit','depth','rms')], normalize)
  

  
  return(df)
}



