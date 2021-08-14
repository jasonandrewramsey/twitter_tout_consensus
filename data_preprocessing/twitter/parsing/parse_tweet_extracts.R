# !diagnostics off

#Function that splits the twitter api output
#Into non/quote/retweet and preps
prep.twitter.output <- function(.data) {
  list(
    .data %>%
      filter(is_quote == T) %>%
      mutate(tweet_flag = 'quoted') %>%
      dplyr::select(tweet_flag, status_id = status_id, user_id = user_id, user = screen_name, timestamp=created_at, tweet=text),
    
    # .data %>%
    #   filter(is_retweet == T) %>%
    #   mutate(tweet_flag = 'retweet') %>%
    #   dplyr::select(tweet_flag, status_id = status_id, user_id = user_id, user = screen_name, timestamp=retweet_created_at, tweet=text),
    # 
    
    .data %>%
      filter(is_retweet == F,
             is_quote == F,
             !is.na(reply_to_user_id)) %>%
      mutate(tweet_flag = 'reply') %>%
      dplyr::select(tweet_flag, status_id = status_id, user_id = user_id, user = screen_name, timestamp=created_at, tweet=text),
    
    .data %>%
      filter(is_retweet == F,
             is_quote == F,
             is.na(reply_to_user_id)) %>%
      mutate(tweet_flag = 'base') %>%
      dplyr::select(tweet_flag, status_id = status_id, user_id = user_id, user = screen_name, timestamp=created_at, tweet=text)
    
  ) %>%
    invoke(rbind, .)
}

#Function that will parse an entire data frame
parse.entire.df <- function(tweet_df) {
  parsed_df <-
  lapply(1:nrow(tweet_df), function(x) {
    tryCatch({
      print(tweet_df$status_id[x])
      suppressWarnings(
        tweet_df[x,] %>%
        cbind.data.frame(., 
                         tweet_df$tweet[x] %>%
                           #str_replace_all(., c('[\\U0001F600-\\U0001F64F]|[\\U0001F300-\\U0001F5FF]|[\\U0001F680-\\U0001F6FF]|[\\U0001F1E0-\\U0001F1FF]'=' ')) %>%
                           parse.bet.slip(., quiet = T))
      )
    }, error = function(e) {})
  }) %>%
    plyr::compact() %>%
    invoke(plyr::rbind.fill, .)
  
  if(!is.null(parsed_df)) {
    parsed_df <-
      parsed_df %>%
      mutate(parse_timestamp = now())
  }
  return(parsed_df)
}

#Function to take a tweet extract file path / read / parse / store it
read.parse.store.tweet.extract <- function(file_full_path, output_file_path) {
  #Generate the output_file_path if one is not provided
  if(missing(output_file_path)) {
    output_file_path <-
      file_full_path %>%
      str_split(., '[/]') %>%
      unlist %>%
      last() %>%
      paste0('./preprocessed_data/twitter/parsed/', .)
  }
  
  #Read and prep for parse
  prepped_twitter_extract <-
  file_full_path %>%
  readRDS(.) %>%
    filter(status_id != '1391659022265241600') %>%
    prep.twitter.output() %>%
    arrange(timestamp) %>%
    mutate(tweet_file_path = file_full_path)
  
  #Parse
  parsed_twitter_extract <- parse.entire.df(prepped_twitter_extract)
  
  #Store
  saveRDS(parsed_twitter_extract, output_file_path)
}

# Job Runners -------------------------------------------------------------

parse.tweet.extracts <- function() {
  #Load bet slip parser
  source('./data_preprocessing/twitter/parsing/bet_slip_parser.R')
  
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
  
  #If there are no files to parse, return such
  if(nrow(files_to_process) == 0) {
    return(cat('\nno files to parse\n'))
  }
  
  #For each of the files -- read/structure/parse/store
  files_to_process %>%
    .$file_full_path %>%
    paste() %>%
    map(., ~read.parse.store.tweet.extract(.))
  
  return(cat('\nparse tweet extracts completed\n'))
}