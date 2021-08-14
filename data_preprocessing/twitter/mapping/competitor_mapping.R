# !diagnostics off
# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, zoo, reshape2, lubridate, english)

#Function to simply return the competitor name given a string
map.competitor.names <- function(string, competitor_regex_table, competitor_reference_table) {
  if(nchar(string) <= 1) {
    return(
      competitor_reference_table %>%
        filter(competitor_name %in% 'thisisadummyvaluestringdesignedtoreturnemptyrows') %>%
        dplyr::select(path_link, competitor_id, competitor_name) %>%
        mutate(string_value = character())
    )
  }
  
  binary_indexing <-
    string %>%
    str_replace_all(., c('[/_]'=' ', '[$]'='')) %>%
    str_trim() %>%
    str_detect(., competitor_regex_table$regex)
  
  #Filter the competitor reference table for the matching competitor names
  comp_table <-
    competitor_reference_table %>%
    filter(competitor_name %in% competitor_regex_table$competitor_name[binary_indexing]) %>%
    dplyr::select(path_link, competitor_id, competitor_name)
  
  #If there are records found -- return the data frame with the string value
  if(nrow(comp_table) > 0) {
    return(
      comp_table %>%
        mutate(string_value = string)
    )
  } else {
    return(
      comp_table %>%
        mutate(string_value = character())
    )
  }
}

#Function to break words into their ngrams (if applicable)
convert.to.ngrams <- function(string) {
  require(ngram)
  single.input.sub.function <- function(string) {
    tryCatch({
      get.ngrams(ngram(string, 2))
    }, error = function(e) {
      string
    })
  }
  list(
    string,
    string %>%
      map(., ~single.input.sub.function(.)) %>%
      invoke(c, .)
  ) %>%
    invoke(c, .) %>%
    unique()
}


conditional.sub.string.removal <- function(string_vector) {
  print(string_vector)
  sub.process.function <- function(sub_conditional_filtered_frame, sub_string_vector, position_index) {
    for(i in 1:nrow(sub_conditional_filtered_frame)) {
      pre_post <- sub_conditional_filtered_frame$pre_post[i]
      keep_qualifier <- sub_conditional_filtered_frame$keep_qualifier[i]
      assess_position <- ifelse(pre_post == 'post', position_index + 1, position_index - 1)
      print(list(pre_post, keep_qualifier, assess_position))
      if(assess_position == 0 | assess_position > length(sub_string_vector)) {
        print('exit1')
        sub_string_vector <- sub_string_vector[-position_index]
      } else {
        assess_word <- sub_string_vector[assess_position]
        ext <- str_extract(assess_word, keep_qualifier)
        if(
          !is.na(ext) & assess_word == ext
        ) {
          print('exit2')
          next
        } else {
          print('exit3')
          sub_string_vector <- sub_string_vector[-position_index]
        }
      }
    }
    return(sub_string_vector)
  }
  
  conditional_word_removal_index <- 
    data.frame(
      words = c('as', 'is', 'was', 'oh', 'or', 'am', 'no', 'at', 'but', 'will', 'oh', 'as'),
      pre_post = c('post', 'post', 'post', 'post', 'post', 'post', 'post', 'post', 'post', 'post', 'pre', 'post'),
      keep_qualifier = c('st', 'st', 'st', 'st', 'st', 'st', 'st', 'st', 'st', 'st', 'mia((mi)?)', 'roma'),
      stringsAsFactors = F
    )
  
  #Return if conditions are not met
  if(length(string_vector) == 1) { return(string_vector) }
  if(any(string_vector %in% conditional_word_removal_index$words) == F) { return(string_vector) }
  
  assess_word_index <- which(string_vector %in% conditional_word_removal_index$words)
  #Find which words are in question
  for(i in 1:length(assess_word_index)) {
    assess_word_index <- which(string_vector %in% conditional_word_removal_index$words)
    if(i > length(assess_word_index)) { break }
    pos <- assess_word_index[i]
    sub_frame <- conditional_word_removal_index[conditional_word_removal_index$words == string_vector[pos],]
    string_vector <- sub.process.function(sub_frame, string_vector, pos)
  }
  print(string_vector)
  return(string_vector)
}


#Function to iterate through string-preprocessing and splitting steps until a mapped competitor is found (if possible)
iterate.map.competitor.names <- function(string, competitor_regex_table, competitor_reference_table) {
  #Create the preprocessing index for each attempt
  split_value_index <- c('this_is_a_dummy_value_first_iteration_isnt_split',
                         '(?<!^)[@/]',
                         '[ ]|(?<![:alpha:])v(s?)')
  
  #Create an empty data frame that will serve as stoppage criteria
  mapping_competitor_info <- 
    data.frame(
      path_link = character(),
      competitor_id = character(),
      competitor_name = character(),
      string_value = character()
    )
  
  #Initialize the loop
  iteration <- 1
  
  #Search for competitor name by splitting and trying all strings -- stop once the data frame is not empty (a competitor has been found)
  while(nrow(mapping_competitor_info) == 0) {
    mapping_competitor_info <-  
    string %>%
      str_split(., split_value_index[iteration]) %>%
      unlist() %>%
      #conditional.sub.string.removal(.) %>%
      convert.to.ngrams(.) %>%
      map(., ~map.competitor.names(., competitor_regex_table, competitor_reference_table)) %>%
      invoke(rbind, .)
    iteration <- iteration + 1
    if(iteration > length(split_value_index)) { break }
  }
  
  return(mapping_competitor_info)
}

#Function to take in the raw fields (from the parsed tweet frame) and return the competitor names
return.parsed.competitor.names <- 
  function(input_param_list, competitor_regex_table, competitor_reference_table) {
  #Ensure everything provided is a character
  map_field_list <-
    input_param_list[c('event', 'outcome')] %>%
    map(., ~paste(.)) %>%
    melt() %>%
    as.data.frame %>%
    setNames(c('value', 'field')) %>%
    na.omit() %>%
    distinct() %>%
    filter(value %in% c('under', 'over', 'NA', '') == F) %>%
    rowwise() %>%
    mutate(value = str_replace_all(value %>% paste, c(' (over|under)'='')) %>% str_trim(),
           sort_key = ifelse(field == 'outcome', 0, 1)) %>%
    as.data.frame %>%
    arrange(sort_key)
  
  #Iterate through the available input fields -- output/event -- and map competitors
  #Stop once a map has been found or all have been iterated through
  competitor_names <- 
    lapply(1:nrow(map_field_list), function(i) {
    mapped_comps <- iterate.map.competitor.names(map_field_list$value[i], competitor_regex_table, competitor_reference_table)
    if(nrow(mapped_comps) > 0) {
      return(mapped_comps)
      break
    } else {
      return(
        data.frame(
          path_link = character(),
          competitor_id = character(),
          competitor_name = character(),
          string_value = character()
        )
      )
    }
  }) %>%
    invoke(rbind, .)
  
  #Add the competitors to the param list
  input_param_list$competitor_names <- competitor_names
  
  return(
    input_param_list
  )
}

#Function to trim mapped competitors based on path_links in the output
reduce.mapped.competitor.names.matchup <- function(.data) {
  if(nrow(.data) == 0) { return(.data) }
  if(length(unique(.data$string_value)) == 1) { return(.data) }
  if(length(unique(.data$path_link)) == 1) { return(.data) }
  
  #Reduce based on path links that have both competitors (if there is any)
  idf <-
    .data %>%
    mutate(total_string_unique_cnt = length(unique(string_value))) %>%
    group_by(path_link) %>%
    mutate(path_string_unique_cnt = length(unique(string_value))) %>%
    as.data.frame %>%
    filter(path_string_unique_cnt == total_string_unique_cnt)
  
  #If this reduces to 0 records -- return the original frame
  if(nrow(idf) == 0) { return(.data) }
  
  return(idf %>%
           dplyr::select(one_of(colnames(.data)))
  )
}