# !diagnostics off
# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, httr, jsonlite, zoo, reshape2, lubridate, doSNOW, excel.link)
library(foreach)
library(doParallel)

#Load bet slip parser
source('./data_preprocessing/twitter/mapping/bet_slip_outcome_mapping.R')

#Source the competitor mapping file
source('./data_preprocessing/twitter/mapping/competitor_mapping.R')






map.parsed.slip.file <- function(file_name_path, mapping_data_list) {
  #Source the competitor mapping file
  source('./data_preprocessing/twitter/mapping/competitor_mapping.R')
  
  #Read in the parsed tweet bet slips
  tweet_df <- readRDS(file_name_path)
  
  if(is.null(tweet_df)) {
    mapped_df <-
      data.frame(status_id = NA)
  } else {
    tweet_df$timestamp <- as.POSIXct(strptime(tweet_df$timestamp, format = '%Y-%m-%d %H:%M:%S', tz = 'GMT'))
    
    mapped_df <-
  # 
  # #Generate the field mapping file
  # mapping_data_list <-
  #   generate.mapping.data.list()
  # 
  #Read in the parsed tweet bet slips
  tweet_df %>%
    filter(market %in% c('btts', 'team_total') == F) %>%
    mutate(condition = ifelse(outcome %in% c('over', 'under'), event, 'good')) %>%
    filter(condition != '', !is.na(condition), condition != 'NA') %>%
    mutate(condition = ifelse(market %in% c('spread', 'moneyline'), outcome, 'good')) %>%
    filter(condition != '') %>%
    mutate(condition = ifelse(market %in% c('spread', 'total'), handicap, 0)) %>%
    filter(!is.na(condition)) %>%
    as.data.frame %>%
    dplyr::select(-condition) %>%
    filter(status_id %in% c('1238153209383997441', '1238153209383997441', '1269277841276928003', '1269277841276928003', '1269672208428945408', '1269672208428945408', '1271910406311874560', '1271910406311874560', '1276628447436115969', '1276628447436115969', 
                            '1276628447436115969', '1276960590813069312', '1276960590813069312', '1276960590813069312', '1277719059346395136', '1331041612827414531', '1346291184105369601') == F) %>%
    filter(!is.na(outcome)|event!='') %>%
    mutate(odds = ifelse(is.na(odds), '-110', odds)) %>%
    # mutate(market = ifelse(str_detect(outcome, ' nrfi$'), 'total', paste(market)),
    #        period = ifelse(str_detect(outcome, ' nrfi$'), 'first_inning', paste(period)),
    #        handicap = ifelse(str_detect(outcome, ' nrfi$'), paste('0.5'), paste(handicap)),
    #        event = ifelse(str_detect(outcome, ' nrfi$'), str_replace_all(outcome, c(' nrfi$'='')), paste(event)),
    #        outcome = ifelse(str_detect(outcome, ' nrfi$'), paste('under'), paste(outcome))
    # ) %>%
    split(., .$status_id) %>%
    map(., ~map.bet.slip.outcomes(., mapping_data_list)) %>%
    invoke(plyr::rbind.fill, .)
  }
  
  return(mapped_df)
}


map.parsed.slip.file.save <- function(file_name_path, mapping_data_list, output_file_path) {
  #Generate the output_file_path if one is not provided
  if(missing(output_file_path)) {
    output_file_path <-
      file_name_path %>%
      str_split(., '[/]') %>%
      unlist %>%
      last() %>%
      paste0('./preprocessed_data/twitter/mapped/', .)
  }
  
  #Map the parsed tweets
  mapped_tweets <- map.parsed.slip.file(file_name_path, mapping_data_list)
  
  #Store
  saveRDS(mapped_tweets, output_file_path)
}













# Parse File Load ---------------------------------------------------------
#File path declaration
file_path <- './preprocessed_data/twitter/parsed/'

#Read all files in bovada scrapes that are not found in parsed extracts
files_to_process <- 
  list(
    file_name = list.files(file_path),
    file_full_path = list.files(file_path, full.names = T)
  ) %>%
  invoke(cbind.data.frame, .) %>%
  mutate_all(., ~paste(.)) %>%
  filter(file_name %in% list.files('./preprocessed_data/twitter/mapped/') == F)

#Generate the mapping data list
mapping_data_list <- generate.mapping.data.list()

# Parallel Core Initialization --------------------------------------------
#Load up all cores to complete this process
cl <- makeCluster(26, type="SOCK")

#Export the required objects to the other clusters
clusterExport(cl, list(
  "files_to_process", "mapping_data_list"
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
  source('./data_preprocessing/twitter/mapping/bet_slip_outcome_mapping.R')
  
  #Source the competitor mapping file
  source('./data_preprocessing/twitter/mapping/competitor_mapping.R')
  
})

# Execution ---------------------------------------------------------------
#setup parallel backend to use many processors
#cores=detectCores()
#cl <- makeCluster() #not to overload your computer
registerDoParallel(cl)

foreach(i=1:nrow(files_to_process), .combine=cbind) %dopar% {
  map.parsed.slip.file.save(file_name_path=files_to_process$file_full_path[i],
                            mapping_data_list=mapping_data_list)
}
#stop cluster
stopCluster(cl)







