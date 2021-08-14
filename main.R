# !diagnostics off
options(dplyr.summarise.inform = FALSE)

# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rvest, httr, jsonlite, zoo, reshape2, lubridate, rtweet)



# Load Scripts ------------------------------------------------------------

# Parser
source('./data_preprocessing/twitter/parsing/bet_slip_parser.R')

#Mapping
source('./data_preprocessing/twitter/mapping/bet_slip_outcome_mapping.R')

#Source the competitor mapping script (Sub-Component of Mapping Process)
source('./data_preprocessing/twitter/mapping/competitor_mapping.R')

#Load the bovada (lines) reference data (Necessary for Outcome Mapping Process)
mapping_data_list <-
  generate.mapping.data.list()

# Usage -------------------------------------------------------------------

text <- 'Brewers ML'

# Poorly designed make-shift complete function
parse.bet.slip(text, quiet = F) %>%
  ### needs a timestamp to do outcome mapping, normally this is tweet timestamp
  ## and some other fields that are output by twitter api that i will just dummy here
  mutate(timestamp = now()-(60*1000), #### Here is a flaw with the mapping -- if you use today's date it will come back empty cause brewers have a double header
         status_id = 12345) %>%
  map.bet.slip.outcomes(., mapping_data_list)