# !diagnostics off
# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, httr, jsonlite, zoo, reshape2, lubridate, doSNOW, excel.link)
library(foreach)
library(doParallel)


#Load bet slip parser
source('./data_preprocessing/twitter/parsing/bet_slip_parser.R')

#Load the functions to parse tweet extracts
source('./data_preprocessing/twitter/parsing/parse_tweet_extracts.R')

# Parse File Load ---------------------------------------------------------
#File path declaration
file_path <- './raw_data/twitter/'

#Read all files in bovada scrapes that are not found in parsed extracts
files_to_process <- 
  list(
    file_name = list.files(file_path),
    file_full_path = list.files(file_path, full.names = T)
  ) %>%
  invoke(cbind.data.frame, .) %>%
  mutate_all(., ~paste(.)) %>%
  filter(file_name %in% list.files('./preprocessed_data/twitter/parsed') == F)

# Parallel Core Initialization --------------------------------------------
#Load up all cores to complete this process
cl <- makeCluster(26, type="SOCK")

#Export the required objects to the other clusters
clusterExport(cl, list(
  "files_to_process"
))


clusterEvalQ(cl, {
#Package depdencies
  library(tidyverse)
  library(zoo)
  library(reshape2)
  library(lubridate)
  library(doSNOW)
  library(foreach)
  library(doParallel)
  
  
  
  #Load bet slip parser
  source('./data_preprocessing/twitter/parsing/bet_slip_parser.R')
  
  #Load the functions to parse tweet extracts
  source('./data_preprocessing/twitter/parsing/parse_tweet_extracts.R')
})

# Execution ---------------------------------------------------------------


#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster() #not to overload your computer
registerDoParallel(cl)

foreach(i=1:nrow(files_to_process), .combine=cbind) %dopar% {
  read.parse.store.tweet.extract(files_to_process$file_full_path[i]) #calling a function
}
#stop cluster
stopCluster(cl)


