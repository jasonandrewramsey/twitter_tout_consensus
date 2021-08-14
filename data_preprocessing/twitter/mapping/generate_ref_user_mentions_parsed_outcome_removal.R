# !diagnostics off

#Function to generate and save the screen names to remove during mapping
generate.to.remove.screen.names <- function(extensive) {
  if(missing(extensive)) {
    extensive <- FALSE
  }
  
  #Optional sub-function for exploratory purposes
  #This will find all of the user names that actually map to competitors
  #This is used to create the excluded_from_removal list used above
  #IE: if someone says @Jets -1.5 --- we do not want to remove @Jets in that scenario, so their handle would be added to the excluded list
  find.valid.screen.names <- function(mentioned_screen_names_df) {
    #Source pre-requisites
    source('./data_preprocessing/twitter/mapping/competitor_mapping.R')
    source('./data_preprocessing/twitter/mapping/bet_slip_outcome_mapping.R')
    source('./data_scraping/twitter/establish_token_session.R')
    
    #Generate the mapping data list
    mapping_data_list <- generate.mapping.data.list()
    
    #Quick sub function for ocmpetitor mapping
    quick.df.map.comp <- function(.data) {
      .data %>%
        cbind.data.frame(., 
                         
                         lapply(1:nrow(.data), function(i) {
                           print(i)
                           comp_name <- map.competitor.names(.data$screen_name[i] %>% str_to_lower(), mapping_data_list$COMPETITOR_REGEX_TABLE, mapping_data_list$BOVADA_REF_DATA$ref_competitors)
                           if(nrow(comp_name) == 0) { data.frame(matching_comps = 0, competitor_name_mapped = NA, stringsAsFactors = F) } else {
                             comp_name %>%
                               summarise(
                                 matching_comps = length(unique(competitor_name)),
                                 competitor_name_mapped = paste(unique(competitor_name), collapse = '; ')
                               ) %>%
                               as.data.frame
                           }
                         }) %>%
                           invoke(rbind, .)
        )
    }
    
    #Generate the fields required to determine validity
    mentioned_screen_names_mapping_comps <-
      mentioned_screen_names_df %>%
      quick.df.map.comp() %>%
      filter(matching_comps > 0)
    
    #Find the new exclusion screen_names
    new_exclude_screen_names <-
      lookup_users(mentioned_screen_names_mapping_comps$screen_name) %>%
      filter(verified == T | followers_count > 5000) %>%
      dplyr::select(screen_name) %>%
      distinct()
    
    #Append to the old exclude_screen_names
    all_exclude_screen_names <-
      list(
        readRDS('./data_preprocessing/ref_data/valid_referenced_screen_names.rds'),
        new_exclude_screen_names
      ) %>%
      invoke(rbind, .) %>%
      distinct()
    
    #Save the exclude screen_names
    saveRDS(all_exclude_screen_names, './data_preprocessing/ref_data/valid_referenced_screen_names.rds')
    
    #Return the screen names to exclude
    mentioned_screen_names_df %>%
      filter(screen_name %in% all_exclude_screen_names$screen_name == F)
  }
  
  #Read the valid referenced screen names
  valid_screen_names <-
    readRDS('./data_preprocessing/ref_data/valid_referenced_screen_names.rds')
  
  #Folder path to dump new tweets/read old dumps
  folder_path <- './raw_data/twitter/'
  
  #Grab all information scraped/stored for each user (locally!)
  tweets <-
    list.files(folder_path, full.names = T) %>%
    map(., ~readRDS(.)) %>%
    invoke(plyr::rbind.fill, .)
  
  #Extract all screen names mentioned
  mentioned_screen_names <-
    list(
      tweets %>%
        dplyr::select(screen_name=reply_to_screen_name) %>%
        na.omit() %>%
        distinct(),
      tweets %>%
        dplyr::select(screen_name=retweet_screen_name) %>%
        na.omit() %>%
        distinct(),
      tweets %>%
        dplyr::select(screen_name=quoted_screen_name) %>%
        na.omit() %>%
        distinct(),
      tweets$mentions_screen_name %>%
        map(., ~invoke(c, .)) %>%
        invoke(c, .) %>%
        unique() %>%
        as.data.frame %>%
        setNames('screen_name') %>%
        na.omit(),
      tweets %>%
        dplyr::select(screen_name) %>%
        na.omit() %>%
        distinct()
    ) %>%
    invoke(rbind, .) %>%
    distinct() %>%
    filter(screen_name %in% valid_screen_names$screen_name == F) %>%
    mutate(effective_screen_name = paste0('@', str_to_lower(screen_name)))
  
  
  if(extensive == T) {
    mentioned_screen_names <- find.valid.screen.names(mentioned_screen_names)
  }
  
  saveRDS(mentioned_screen_names, './data_preprocessing/ref_data/to_exclude_screen_names.rds')
}

