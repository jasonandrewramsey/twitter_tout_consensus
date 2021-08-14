# !diagnostics off
# Data Load Functions -----------------------------------------------------

#Function to return all of the data elements required for mapping
generate.mapping.data.list <- function() {
  #Function to generate the reference tables required
  generate.bovada.reference.table.list <- function() {
    file_path <- './preprocessed_data/bovada/ref_data/'
    files_to_process <- 
      list(
        file_name = list.files(file_path),
        file_full_path = list.files(file_path, full.names = T)
      ) %>%
      invoke(cbind.data.frame, .) %>%
      mutate_all(., ~paste(.))
    
    #Create the ref data named list
    files_to_process$file_full_path %>%
      map(., ~readRDS(.)) %>%
      setNames(files_to_process %>% .$file_name %>% paste %>% str_replace_all(., c('.rds'='')))
  }
  
  mapping_data_list <-
    list(
      BOVADA_FIELD_MAPPING = readRDS('./data_preprocessing/ref_data/field_index_mapping.rds'),
      BOVADA_REF_DATA = generate.bovada.reference.table.list(),
      COMPETITOR_REGEX_TABLE = readRDS('./data_preprocessing/ref_data/competitor_name_regex.rds')
    )
  
  tennis_path_links <-
  mapping_data_list$BOVADA_REF_DATA$ref_outcomes %>%
    filter(grepl('tennis', path_link)) %>%
    dplyr::select(path_link) %>%
    distinct()
  
  mapping_data_list$BOVADA_REF_DATA$ref_competitors <-
  list(
    mapping_data_list$BOVADA_REF_DATA$ref_competitors %>%
      filter(grepl('tennis', path_link) == F),
    tennis_path_links %>%
      mutate(dummy = 1) %>%
      left_join(tennis_path_links %>%
                  mutate(dummy = 1),
                by = 'dummy',
                suffix = c('', '_copy')) %>%
      dplyr::select(-dummy) %>%
      left_join(
        mapping_data_list$BOVADA_REF_DATA$ref_competitors %>%
          filter(grepl('tennis', path_link)),
        by = 'path_link'
      ) %>%
      mutate(path_link = path_link_copy) %>%
      dplyr::select(-path_link_copy)
  ) %>%
    invoke(rbind, .)

  #Debatable when this normalization should occurr - I am doing it here for the time being
  #Must group by all keys in the bovada ref_outcome table and reduce to the most recent (or you could do it based on the tweet timestamp?)
  #Handicap/Odds change with each extract
  mapping_data_list$BOVADA_REF_DATA$ref_outcomes <-
    mapping_data_list$BOVADA_REF_DATA$ref_outcomes %>%
    group_by(path_id, path_link, event_id, competitor_id, displayGroup_id, displayGroup_description,
             market_key, market_description, period_description, period_abbreviation, outcome_id,
             outcome_description, outcome_type, team_total_flag) %>%
    arrange(desc(timestamp)) %>%
    summarise_all(first) %>%
    as.data.frame()
  
  mapping_data_list$BOVADA_REF_DATA$ref_events$event_startTime <- as.POSIXct(strptime(paste(mapping_data_list$BOVADA_REF_DATA$ref_events$event_startTime), format = '%Y-%m-%d %H:%M:%S', tz = "America/New_York"))
  
  return(mapping_data_list)
}

# Parsed Slip to Bovada Valid Sport Join ----------------------------------

#Function to map the fields based on event/outcome, market, sport, period
parsed.tweet.fields.to.bovada.key.fields <- 
  function(input_param_list, field_mapping_table_list) {
  #Identify the potential competitors
  parsed_competitor_names_table <- input_param_list$competitor_names
  
  #The initial join is sport to path link
  #If sport is not provided (NA) return all of the path links
  sport_to_path_link <-
  if(is.na(input_param_list$sport) | !input_param_list$sport %in% field_mapping_table_list$BOVADA_FIELD_MAPPING$sport$sport) {
    suppressWarnings(
      input_param_list[names(input_param_list)[names(input_param_list) %in% c('competitor_names') == F]] %>%
        invoke(cbind.data.frame, .) %>%
        mutate(dummy = 1) %>%
        inner_join(
          field_mapping_table_list$BOVADA_FIELD_MAPPING$sport %>%
            dplyr::select(-sport) %>%
            distinct() %>%
            mutate(dummy = 1),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy)
    )
  } else {
    suppressWarnings(
      input_param_list[names(input_param_list)[names(input_param_list) %in% c('competitor_names') == F]] %>%
        invoke(cbind.data.frame, .) %>%
        inner_join(
          field_mapping_table_list$BOVADA_FIELD_MAPPING$sport,
          by = 'sport'
        )
    )
  }

  #Perform joins on the input param information to the field mapping table lists
  field_listing_joins <-
    suppressWarnings(
      sport_to_path_link %>%
        inner_join(
          field_mapping_table_list$BOVADA_FIELD_MAPPING$period,
          by = c('path_link', 'period')
        ) %>%
        inner_join(
          field_mapping_table_list$BOVADA_FIELD_MAPPING$market,
          by = c('path_link', 'market')
        )
    )
  
  #If the competitors names came back with results -- perform an inner join
  initial_mapping_results <-
  if(nrow(parsed_competitor_names_table) > 0) {
    suppressWarnings(
    field_listing_joins %>%
      inner_join(
        parsed_competitor_names_table,
        by = c('path_link')
      )
    )
  } 
  #If the competitors name is empty (no mapping) -- perform a left join to maintain data structure
  else 
  {
    suppressWarnings(
    field_listing_joins %>%
      left_join(
        parsed_competitor_names_table,
        by = c('path_link')
      )
    )
  }
  
  #Add the initial mapping to the input params and return
  input_param_list$outcomes_in_scope <- initial_mapping_results
  return(input_param_list)
  }


# Outcome Reduction Functions ----------------------------------------------

#Reduce records on handicap
reduce.records.on.handicap <- function(.data) {
  idf <- .data
  
  #Reduce to the records that are less than 15% from the stated odds
  within_odds_range <- 
    idf %>%
    rowwise() %>%
    mutate(delta = abs(handicap-as.numeric(price_handicap))/abs(handicap)) %>%
    as.data.frame %>%
    filter(delta <= .15)
  
  #If no records fit this criteria, return the original data frame
  if(nrow(within_odds_range) == 0) { return(idf) }
  
  #Return the minimum distance record
  return(within_odds_range[which.min(within_odds_range$delta),])
}


#Function to reduce based on odds (moneyline)
reduce.records.on.moneyline <- function(.data) {
  #Function to convert american odds to numeric
  american_to_numeric_odds <- function(odds) {
    odds <- as.numeric(odds)
    if(odds<0) {
      ((abs(odds))+100)/(abs(odds))
    } else {
      1+(odds/100)
    }
  }
  #Reassign frame
  idf <- .data
  
  #If they only gave you -110, just return the frame, there is no information
  if(as.numeric(unique(idf$odds))) { return(idf) }
  
  #Reduce to the records that are less than 15% from the stated odds
  within_odds_range <- 
    idf %>%
    rowwise() %>%
    mutate(stated_price_numeric = american_to_numeric_odds(odds),
           delta = abs(stated_price_numeric-as.numeric(price_decimal))/stated_price_numeric) %>%
    as.data.frame %>%
    filter(delta <= .15)
  
  #If no records fit this criteria, return the original data frame
  if(nrow(within_odds_range) == 0) { return(idf) }
  
  #Return the minimum distance record
  return(within_odds_range[which.min(within_odds_range$delta),])
}

#Create a function that will filter records based on the stated outcome/competitor
reduce.records.on.outcome <- function(.data, mkt) {
  #Object reassignment
  idf <- .data
  
  #Assign the filtering field based on the market
  #This will be used to reduce to 1 outcome -- the bovada data is structured to show the data for both teams
  idf$filter_field <-
    if(mkt == 'total') {
      idf$outcome
    } else {
      idf$competitor_name
    }
  #Reduce based on the filter field
  idf %>%
    filter(tolower(filter_field) == tolower(outcome_description)) %>%
    distinct()
}

#Function that will reduce the frame for the most frequent event
reduce.most.frequent.event <- function(.data) {
  #Object reaassignment
  idf <- .data
  
  #Reduce the frame for the event(s) with the highest frequency
  event_mx_freq <-
    idf %>%
    group_by(event_id) %>%
    summarise(count = n()) %>%
    as.data.frame %>%
    .$count %>%
    max
  
  idf %>%
    group_by(event_id) %>%
    mutate(event_cnt = n()) %>%
    as.data.frame %>%
    filter(event_cnt == event_mx_freq)
}

#Sub function to determine if reduction is satisfied and the object can be returned
reduction.satisfied <- function(.data, mkt) {
  #Object reassignment
  idf <- .data
  
  #If there is only 1 row or if the frame is empty, all you can do is return it
  if(nrow(idf) < 2) { return(TRUE) }
  
  #If there is only 1 unique event and the market is 'total' -- return
  if(mkt == 'total' & length(unique(idf$event_id)) == 1 & nrow(idf) > 1) {
    return(
      TRUE
    )
  }
  return(FALSE)
}

#Function to reduce the many results returned by mapping
reduce.events.outcomes <- function(.data) {
  #Object reassignment
  idf <- .data
  
  #Initial Reduction is if there is no rows, return the frame
  if(nrow(idf) == 0) {
    return(
      data.frame(
        outcome_map_criteria = 'no_records',
               outcome_map_sufficient = FALSE
        )
    )
  }
  
  #Assign the market identifier
  mkt <- unique(idf$market)
  
  #Reduce records on outcome
  idf <- 
    idf %>%
    reduce.records.on.outcome(., mkt)
  
  #Check satisfied criteria
  if(idf %>% reduction.satisfied(., mkt)) {
    return(
      idf %>%
        summarise_all(first) %>%
        as.data.frame %>%
        mutate(outcome_map_criteria = 'number_of_outcomes',
               outcome_map_sufficient = TRUE)
    )
  }
  
  #Reduce on most frequent event
  idf <- 
    idf %>%
    reduce.most.frequent.event()
  
  #Check satisfied criteria
  if(idf %>% reduction.satisfied(., mkt)) {
    return(
      idf %>%
        summarise_all(first) %>%
        as.data.frame %>%
        mutate(outcome_map_criteria = 'number_of_events',
               outcome_map_sufficient = TRUE)
    )
  }
  
  #Try the specialized filtering
  idf <-
  if(mkt == 'moneyline') {
    idf %>% 
      reduce.records.on.moneyline()
  } else {
    idf %>%
      reduce.records.on.handicap()
  }
  
  #Check satisfied criteria
  if(idf %>% reduction.satisfied(., mkt)) {
    return(
      idf %>%
        summarise_all(first) %>%
        as.data.frame %>%
        mutate(outcome_map_criteria = 'handicap_odds',
               outcome_map_sufficient = TRUE)
    )
  }
  
  return(
    idf %>%
           mutate(outcome_map_criteria = 'none',
                  outcome_map_sufficient = FALSE)
         )
}


# Initial Pass Reduction --------------------------------------------------

#Run the initial mapping
initial.mapping.results <-
  function(
    input_param_list,
    mapping_data_list
  ) {
    #Perform the inner joins based on key fields/competitors
    mapping_dm <-
      suppressWarnings(
        mapping_data_list$BOVADA_REF_DATA$ref_events %>%
          mutate(time_diff = as.numeric(difftime(event_startTime, as.POSIXct(strptime(input_param_list$timestamp, format = '%Y-%m-%d %H:%M:%S', tz = 'GMT')), unit = "days"))) %>%
          filter(time_diff >= 0, time_diff <= 2) %>%
          dplyr::select(path_link, event_id, time_diff, event_startTime, normalized_event_description) %>%
          inner_join(
            mapping_data_list$BOVADA_REF_DATA$ref_events_competitors %>%
              dplyr::select(-timestamp),
            by = c('event_id')
          ) %>%
          inner_join(
            input_param_list$outcomes_in_scope %>%
              dplyr::select(path_link, competitor_id, handicap, odds, market, period_description, market_key, 
                            team_total_flag, displayGroup_id, outcome, competitor_name) %>%
              mutate(competitor_id = paste(competitor_id)),
            by = c('path_link', 'competitor_id')
          ) %>%
          inner_join(
            mapping_data_list$BOVADA_REF_DATA$ref_outcomes %>%
              dplyr::select(-timestamp),
            by = c('path_link', 'competitor_id', 'displayGroup_id', 'period_description', 'market_key', 'team_total_flag', 'event_id')
          ) %>%
          mutate_if(is.factor, ~paste(.)) %>%
          mutate_at(vars(price_handicap, price_decimal, handicap), ~as.numeric(.)) %>%
          reduce.events.outcomes()
      )
    
    #Add the mapping data model to the input param list and return
    input_param_list$mapping_dm <- mapping_dm
    return(
      input_param_list
    )
  }

# Secondary Pass Reduction ------------------------------------------------

secondary.pass.determination <- function(.data) {
  #Sub function to join slip number into the mapping dm and return
  sub_secondary_determ_funct <- function(.data) {
    idf <- .data
    
    idf[['mapping_dm']] %>%
      dplyr::select(outcome_map_criteria, outcome_map_sufficient) %>%
      mutate(slip_no = idf$slip_no)
  }
  
  df <-
    .data %>%
    map(., ~sub_secondary_determ_funct(.)) %>%
    invoke(rbind, .) %>%
    distinct()
  
  #If all outcomes are mapped sufficiently, return NULL
  if(!any(df$outcome_map_sufficient == F)) { return(NULL) }
  
  #If all outcomes are mapped insufficiently, with no records, return null
  if(sum(df$outcome_map_criteria == 'no_records')==nrow(df)) { return(NULL) }
  
  #Else, return the ones that need to be re-passed
  return(
    df %>%
      filter(outcome_map_sufficient == F,
             outcome_map_criteria != 'no_records') %>%
      .$slip_no %>%
      as.numeric()
  )
}

#Reduce records in the secondary portion by filtering for the slip's most common sport
secondary.reduce.outcomes <- function(.data) {
  #Object reassignment
  idf <- .data
  
  #Generate the secondary pass index
  secondary_pass_index <- secondary.pass.determination(idf)
  
  #If secondary pass index is NULL (length 0) return
  if(length(secondary_pass_index) == 0) { return(idf) }
  
  #Else, find the most common sport and attempt to map that way
  most_frequent_sport <-
    idf %>% 
    map(., ~.[['mapping_dm']]) %>%
    invoke(plyr::rbind.fill, .) %>% 
    filter(outcome_map_sufficient == T) %>%
    group_by(path_link) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    summarise_all(first) %>%
    as.data.frame %>%
    .$path_link
  
  for(i in 1:length(idf)) {
    #If a secondary pass is not required, return the object
    if(i %in% secondary_pass_index == F) { next }
    
    
    #If there are no good records, return the object
    #if(length(secondary_pass_index) < length(idf)) { 
    if(length(secondary_pass_index) > length(idf)) { 
      #If a secondary pass is required -- perform the sport reduction
      new_result <- 
        idf[[i]]$mapping_dm %>%
        filter(path_link == most_frequent_sport) %>%
        reduce.events.outcomes()
      
      #if satisfied, return the new result
      if(nrow(new_result) == 1) {
        idf[[i]]$mapping_dm <-
          new_result %>%
          mutate(outcome_map_criteria = paste0('secondary_sport_', outcome_map_criteria),
                 outcome_map_sufficient = TRUE)
        next
      }
    }
    
    #Else -- find the event/outcome with the closest timestamp and return
    idf[[i]]$mapping_dm <-
      idf[[i]]$mapping_dm %>%
      arrange(abs(time_diff)) %>%
      summarise_all(first) %>%
      as.data.frame %>%
      mutate(outcome_map_criteria = 'secondary_event_time_diff',
             outcome_map_sufficient = TRUE)
  }
  
  #Return the frame
  return(idf)
  
}

# Map Slip Aggregate Function ---------------------------------------------
map.bet.slip.outcomes <- function(.data, mapping_data_list) {
  print(unique(.data$status_id))
  .data %>% 
    #Split tweet by outcome
    split.parsed.tweet.by.outcome() %>%
    #Map parsed tweet input to valid competitors
    map(., ~return.parsed.competitor.names(input_param_list = ., 
                                           mapping_data_list$COMPETITOR_REGEX_TABLE, 
                                           mapping_data_list$BOVADA_REF_DATA$ref_competitors) %>%
          #Map parsed tweet input to valid bovada outcomes
          parsed.tweet.fields.to.bovada.key.fields(input_param_list = ., field_mapping_table_list = mapping_data_list) %>%
          #Reduce outcomes
          initial.mapping.results(input_param_list = ., mapping_data_list = mapping_data_list)) %>%
    #Reduce outcomes via secondary method if required
    secondary.reduce.outcomes(.) %>%
    map(., ~structure.list.to.df(.)) %>%
    invoke(plyr::rbind.fill, .)
}
# Generic Structuring Functions -------------------------------------------
#Function to structure the output
structure.list.to.df <-
  function(.data) {
    idf <- .data
    
    #Add original tweet info into the outcome frame
    original_tweet_info <- 
      idf[names(idf)[!(names(idf) %in% c('competitor_names', 'outcomes_in_scope', 'mapping_dm'))]] %>%
      do.call(cbind.data.frame, .) %>%
      mutate_all(., ~paste(.))
    
    #Add competitors
    competitor_info <-
      idf$competitor_names %>%
      summarise(
        competitor_mapping_string_value = paste(unique(string_value), collapse = '; '),
        competitor_mapping_competitor_name = paste(unique(competitor_name), collapse = '; ')
      )
    return(
      suppressWarnings(
        original_tweet_info %>%
          dplyr::select(-one_of(colnames(idf$mapping_dm)))
      ) %>%
        cbind.data.frame(
          ., 
          competitor_info
        ) %>%
        cbind.data.frame(
          ., 
          idf$mapping_dm
        )
      
    )
    
  }

#Single line data frame into a named list (names being the columns)
df.to.named.list <-
  function(.data) {
    .data %>%
      t %>%
      as.list() %>%
      setNames(names(.data))
  }


#Function that will take the bet slip, split by rows and run the mapping to bovada_key_fields
split.parsed.tweet.by.outcome <- 
  function(.data) {
    #Single line data frame into a named list (names being the columns)
    df.to.named.list <-
      function(.data) {
        .data %>%
          t %>%
          as.list() %>%
          setNames(names(.data))
      }
    
    .data %>%
      mutate(., slip_no = row_number()) %>%
      split(., .$slip_no) %>%
      map(., ~df.to.named.list(.))
  }


# Job Runners -------------------------------------------------------------

map.parsed.slip.file <- function(file_name_path) {
  #Source the competitor mapping file
  source('./data_preprocessing/twitter/mapping/competitor_mapping.R')
  
  #Generate the field mapping file
  mapping_data_list <-
    generate.mapping.data.list()
  
  #Read in the parsed tweet bet slips
  tweet_df <- readRDS(file_name_path)
  
  if(is.null(tweet_df)) {
    mapped_df <-
      data.frame(status_id = NA)
  } else {
    tweet_df$timestamp <- as.POSIXct(strptime(tweet_df$timestamp, format = '%Y-%m-%d %H:%M:%S', tz = 'GMT'))
      
    mapped_df <-
      tweet_df  %>%
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


map.parsed.slip.file.save <- function(file_name_path, output_file_path) {
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
  mapped_tweets <- map.parsed.slip.file(file_name_path)
  
  #Store
  saveRDS(mapped_tweets, output_file_path)
}


map.unmapped.tweets <- function() {
  #File path declaration
  file_path <- './preprocessed_data/twitter/parsed'
  
  #Read all files in bovada scrapes that are not found in parsed extracts
  files_to_process <- 
    list(
      file_name = list.files(file_path),
      file_full_path = list.files(file_path, full.names = T)
    ) %>%
    invoke(cbind.data.frame, .) %>%
    mutate_all(., ~paste(.)) %>%
    filter(file_name %in% list.files('./preprocessed_data/twitter/mapped') == F)
  
  #If there are no files to parse, return such
  if(nrow(files_to_process) == 0) {
    return(cat('\nno files to parse\n'))
  }
  
  #For each of the files -- read/structure/parse/store
  files_to_process %>%
    .$file_full_path %>%
    paste() %>%
    map(., ~map.parsed.slip.file.save(.))
  
  return(cat('\nparse tweet mapping completed\n'))
}