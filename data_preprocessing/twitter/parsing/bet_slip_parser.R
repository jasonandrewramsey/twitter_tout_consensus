# !diagnostics off

# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, zoo, reshape2, english)


# Reference Data ----------------------------------------------------------

#Declaration of reference data
ref_sports <- c('bundesliga','england_pl','italy_series_a','spain_la_liga','france_ligue_one', 'england_fa_cup', 
                'college', 'ncaaf', 'ncaab', 'nba', 'nfl', 'ncaa', 'nhl', 'tennis', 'cbb', 'mlb', 'cfb', 'ipl ', 
                'mls', 'kbo', 'ufc')


# sport_unicode_transformation_index <-
#   data.frame(
#     character_unicode = c("\u26be", "\u26bd", "\U0001f3c0", "\U0001f3c8", "\U0001f3d2", "\U0001f3be", "\U0001f3d3", "\U0001f3c9", "\U0001f3f8"),
#     sport = c("mlb", "soccer", "basketball", "football", "hockey", "tennis", "tennis", "football", "tennis"),
#     stringsAsFactors = F
#   ) %>%
#   mutate(sport = paste0(' ', sport, ' '))
# 
# sport_unicode_transformation_index <-
#   sport_unicode_transformation_index$sport %>%
#   setNames(sport_unicode_transformation_index$character_unicode)



ref_periods <- c('fq', 'sq', 'tq', 'fq', 'fp', 'sp', 'tp', 'fh', 'sh', 'fh', 'ff')

#Weekdays
weekdays_regex <- c(paste0(c('mon', 'tue(s?)', 'wed((nes)?)', 'thur(s?)', 'fri', 'sat((ur)?)'), '((day)?)(s?)'), 'sunday')

#Known recap bag of words
recap_words <- c(weekdays_regex, "recap", "today", "week", "spreadsheet", "ytd", 'vip(s?)', 'yesterday', 'iamafuckingtout')
recap_bag_of_words_regex <- paste0('([l][0-9])|(?<![:alpha:])(', paste(recap_words, collapse = '|'), ')(?![:alpha:])')

#Known descriptive words to be removed in post processing
post_process_removal_words <- c('add', 'and', 'the', 'play', 'tonight', 
                                'card', 'also', 'have', 'from', 'earlier', 'lean', 
                                'pod', 'on', 'in', 'for', 'going', 'to', 'last',
                                'minute', 'day', 'dmd', 'model', 'lets', 'go', 'maxbet',
                                'winner', 'we', 'stay', 'hot', 'taking', 'premium',
                                'ush', 'push', 'bestbet', 'x', 'bought', 'halfpoint',
                                'fade', 'cash', 'bet', 'system', 'it', 'our')

post_process_removal_words_regex <- paste0('(?<![:alpha:])(', paste(post_process_removal_words, collapse = '|'), ')(?![:alpha:])')

# Tweet Pre Processing for Parse ------------------------------------------

#Function to do the initial standardization/cleaning of the tweet
initial_tweet_preprocess <- function(.data) {
  ## Removes HTTPS/URL strings from a string
  remove_https <- function(txt) { 
    split_text <- str_split(txt, " ")[[1]]
    detection_vector <- !str_detect(split_text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+")
    text <- paste(split_text[detection_vector], collapse = " ")
    return(text)
  }
  
  ## When Over/Under Notation is found -- standardizes the method in which the notation is used (over 123 | ov 123 | o 123 | o123 --> o123)
  totals_notation_regex <- function(txt) {
    str_replace_all(txt, c("\\s+"="", "over"="o", "ov"="o", "under"="u", "un"="u", '[/-]'='', '[()]'=''))
  }
  
  ## Replace word- with word -
  word_hyphen_regex <- function(txt) {
    str_replace_all(txt, c("-"=" -"))
  }
  
  ## When Over/Under Notation is found -- standardizes the method in which the notation is used (over 123 | ov 123 | o 123 | o123 --> o123)
  risk_to_win_descr_transformation <- function(txt) {
    #American Odds format function
    decimal_to_numeric_odds <- function(return) {
      if(return < 1) {
        paste0('-', round(100/return,0))
      } else {
        paste0('+', round(100*return,0))
      }
    }
    
    ext <- str_extract_all(txt,'([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})')[[1]]
    ext <- ext[which(nchar(ext) > 0)]

    return(
      list(
        ' ',
        paste0(ext[1], 'u'),
        if(as.numeric(ext[2])>0) { decimal_to_numeric_odds(((as.numeric(ext[1])+as.numeric(ext[2]))/as.numeric(ext[1]))-1) } else { '' },
        ' '
      ) %>%
        paste(., collapse = ' ')
    )
    
  }
  
  ## When Spread/Odds Notation is found -- standardizes the method in which the notation is used (+ 14 | +14 --> +14)
  odds_spread_hyphen_typo_regex <- function(txt) {
    #First break the punctuation away from the team/event
    txt <- str_replace_all(txt, c('[-]'=' -', '[+]'=' +'))
    #Then remove the space between the punctuation and the digits
    remove_space_between_digit_regex <- function(j) { str_replace_all(j, ' ', '')} 
    str_replace_all(txt, '[+-][\\s+]([\\d+]|[.])', remove_space_between_digit_regex)
  }
  
  ## When handicap notation is found -- standardizes the method which fractional numbers are referred (+1.5 | +1 1/2 | +1 3/6 --> +1.5)
  fractional_handicap_reference_regex <- function(txt) {
    #Remove the spaces
    txt <- str_replace_all(txt, c(' '=''))
    #Compute the fraction and replace
    replacement_index_frame <-
      str_match_all(txt, '[\\d+][/][\\d+]') %>%
      as.data.frame %>%
      setNames('org') %>%
      rowwise() %>%
      mutate(sub = as.numeric(str_split(org, '/')[[1]][1])/as.numeric(str_split(org, '/')[[1]][2]),
             sub = str_replace_all(sub, c('0.'='.'))) %>%
      as.data.frame
    
    str_replace_all(txt, setNames(replacement_index_frame$sub, replacement_index_frame$org))
  }
  
  ## When unit notation is denounced in dollar amounts -- standardize to units
  unit_dollar_reference_regex <- function(txt) {
    #Remove the dollar signs and replace with scalar
    txt <- str_replace_all(txt, c('[$]'='____', '[()]'=''))
    
    #Compute the unit and replace
    replacement_index_frame <-
      str_match_all(txt, '____\\d+[^[:alpha:]]') %>%
      as.data.frame %>%
      setNames('org') %>%
      rowwise() %>%
      mutate(sub = str_replace_all(org, c('[[:punct:]]'='', '[$]'='')),
             sub = paste0(' ', as.numeric(sub)/1000, 'u', ' '),
             sub = str_replace_all(sub, c('0.'='.')),
             org = paste(org)) %>%
      as.data.frame
    
    str_replace_all(txt, setNames(replacement_index_frame$sub, replacement_index_frame$org))
  }
  
  ## When unit/odds notation is announced in (risk/reward) format -- standardize it (1.1/1u --> -110 1u)
  risk_to_win_unit_transformation <- function(txt) {
    #American Odds format function
    decimal_to_numeric_odds <- function(return) {
      if(return < 1) {
        paste0('-', round(100/return,0))
      } else {
        paste0('+', round(100*return,0))
      }
    }
    
    #First remove the parenthesis due to potential regex problems
    txt <- str_replace_all(txt, c("[()]"=" "))
    
    #Compute the unit and the american odds
    replacement_index_frame <-
      txt %>%
      as.data.frame %>%
      setNames('org') %>%
      rowwise() %>%
      mutate(sub = str_replace_all(org, c('\n'="",'[(]'="", '[)]'="")),
             risk = str_split(sub, '[/]')[[1]][1],
             to_win = str_split(sub, '[/]')[[1]][2],
             return = decimal_to_numeric_odds(((as.numeric(gsub("u", "", to_win))+as.numeric(risk))/as.numeric(risk))-1),
             sub = paste(' ', return, to_win, sep = ' '))
    
    str_replace_all(txt, setNames(replacement_index_frame$sub, replacement_index_frame$org))
  }
  
  #Standardize unit notation (3 u | 3x | 3u | 3* | 3uABC -- > 3u)
  standardize_unit_notation <- function(txt) {
    txt %>%
      #Get rid of all spaces
      str_replace_all(., c(' '='')) %>%
      #Convert every notation to u
      str_replace_all(., c('[ux*]'='u')) %>%
      #Add padd at the end and beginning
      paste0(' ', ., ' ')
  }
  
  #Standardize unit notation (o3.5(3u) | o3.5 3u | o3.5 (3u) | o3.5 (3) -- > o3.5 3u)
  standardize_unit_notation_with_parenthesis <- function(txt) {
    txt %>%
      #Get rid of all () with spaces
      str_replace_all(., c('[()]'=' ', 'u'='')) %>%
      paste0(., "u") %>%
      str_replace_all(., c(' u'='u'))
  }
  
  #Identify/ Standardize Team Totals
  standardize_team_totals_notation <- function(txt) {
    str_replace_all(txt, c('tt'='team_total '))
  }
  
  #Standardize teaser notation
  #The regex used to detect teasers (found below on call) can handle three different declaration styles:
  #Point & Teaser & Number found (ie: 6.5 pt teaser| teaser 6.5 point | etc)
  #Teaser then number (ie: teaser +6.5)
  #Number then teaser (ie: +6.5 teaser)
  standardize_teaser_notation <- function(txt) {
    str_extract_all(txt, '[0-9]{1,2}(([\\.][0-9])?)') %>%
      unlist() %>%
      str_split(., '\\.') %>%
      unlist() %>%
      map(., ~paste(as.english(as.numeric(.)))) %>%
      invoke(c, .) %>%
      paste(., collapse = '.') %>%
      paste0(' ', ., '_pt_teaser ')
  }
  
  #Function to transform numeric prices into american odds
  numeric_quoted_odds_to_american <- function(txt) {
    clean_txt <- txt %>% str_replace_all(., c('\\s+'=' ')) %>% str_split(., ' ') %>% unlist()
    
    if(length(clean_txt) != 3) { return(txt)}
    
    first_cell_outcome <- str_detect(clean_txt[1], '^[-+ou]') == TRUE | clean_txt[1] == 'ml'
    last_cell_unit <- str_detect(clean_txt[3], '[u]$')
    
    american_odds <-
      if(first_cell_outcome == T & last_cell_unit == T) {
        num <- as.numeric(clean_txt[2])
        if(num > 2) {
          paste0('+', round((num-1),3)*100)
        } else {
          paste0('-', round(100/(num-1), 0))
        }
      }
    
    return(paste(clean_txt[1], american_odds, clean_txt[3], sep = ' '))
  }
  
  #Removing team hashtag identifiers like 'go jets go'
  remove_hashtag_go_team_go <- function(txt) {
    str_replace_all(txt, c('[#]go'=' ', 'go '=' '))
  }
  
  .data %>%
    str_replace_all(., c(
      # Emojis that signify the event has transpired
      '[\\U0001F926]'= '   iamafuckingtout   ',
      '[\\u274c]'= '   iamafuckingtout   ',
      '[\\U0001f4b0]'= '   iamafuckingtout   ',
      '[\\u2705]'= '   iamafuckingtout   ',
      '[\\U0001f17f]'= '   iamafuckingtout   ',
      '[\\U0001f5d1]'= '   iamafuckingtout   ',
      '[\\u2716]'= '   iamafuckingtout   ',
      '[\\u2714]' = '   iamafuckingtout   ',
      '[\\U0001f922]'= '   iamafuckingtout   ',
      '[\\U0001f4b5]'= '   iamafuckingtout   ',
      '[\\U0001f44e]'= '   iamafuckingtout   ',
      '[\\u274c]' = '   iamafuckingtout   ',
      '[\\U0001f911]' = '   iamafuckingtout   ',
      '[\\U0001f92e]' = '   iamafuckingtout   ',
      '[\\U0001f4a9]' = '   iamafuckingtout   ',
      '[\\U0001f973]' = '   iamafuckingtout   ',
      '[\\U0001f525]' = '   iamafuckingtout   ',
      '[\\U0001f6ad️]'= '  iamafuckingtout    ',
      '[\\U0001f3fc]'='   iamafuckingtout   ',
      '[\\u2620]'='   iamafuckingtout   ',
      '[\\U0001f6ae]'= '   iamafuckingtout   ',
      '[\\U0001f92c]'='   iamafuckingtout   ',
      '[\\U0001f494]'='   iamafuckingtout   ',
      '[\\U0001f480]'='   iamafuckingtout   ',
      '[\\U0001f407]'='   iamafuckingtout   ',
      '[\\u2764]'='   iamafuckingtout   ',
      '[\\U0001F3C6]'='   iamafuckingtout   ',
      '[\\U0001f94a]'='   iamafuckingtout   ',
      '[\\U0001f921]'='   iamafuckingtout   '
      )) %>%
    
    
    ### THIS IS TEMPORARY ###
    str_replace_all(., c('_lb_'='\n')) %>%
    
    #Send it to lowercase
    str_to_lower() %>%
    
    #Remove https
    remove_https() %>%
    
    #Simple string replacement -- remove emojis/perform simple string organization
    str_replace_all(., c(
      '’'='',
      #Remove emojis (unicode/utf-8/idkanythingaboutencoding)
      "[^\x01-\x7F]"=" ", 
      '[\\U0001F600-\\U0001F64F]|[\\U0001F300-\\U0001F5FF]|[\\U0001F680-\\U0001F6FF]|[\\U0001F1E0-\\U0001F1FF]'=' ',
      
      #Do some encoding/prep clean up of punctuation material
      "&amp;"="&",
      '\n{2}'='\n',
      '[\\^]'=' ', '[&]gt'='>',
      "[\\[]"=" ", "[\\]]"=" ", '\n'=' \n ',
      
      #Convert foreign methodology of referencing numerics (+1,5) with the american method (+1.5)
      '(\\d),(\\d)'='\\1.\\2',
      
      #Sporting hashtag transformations
      '(#?)mlb(playoffs|postseason|bets|bubble|picks|twitter)'='mlb', 
      '(#?)nhl(playoffs|postseason|bets|bubble|picks|twitter)'='nhl', 
      '(#?)nba(playoffs|postseason|bets|bubble|picks|twitter)'='nba',
      '(?<![:alpha:])([\\s+#]?)((t|s|m)nf|(monday|thursday|sunday)( ?)night( ?)football)([:]?)'=' nfl ', 
      'german(y?)( ?)bundesliga([2]?)'='bundesliga', 'bundesliga((soccer( ?)picks)?)'='bundesliga', 
      'bundesliga(?=[:alpha:])'='bundesliga ', 'bundesliga( ?)[1-3](?![[:digit:]|[:alpha:]|[:punct:]])'='bundesliga ',
      
      '(?<![:alpha:])e((ngl)?)((ish|and)?)( ?)premier( ?)league'='england_pl',
      '(?<![[:alpha:]|[:digit:]|[:punct:]])epl(?![[:alpha:]|[:digit:]|[:punct:]])'='england_pl',
      
      'italy( ?)serie(s?)([\\-]|[ ]|)(a?)(?![[:alpha:]|[:digit:]|[:punct:]])'='seriea', 
      'serie(s?)([\\-]|[ ]|)a(?![[:alpha:]|[:digit:]|[:punct:]])'='italy_series_a',
      
      '((spain)?)( ?)la([\\-]|[ ]|)liga'=' spain_la_liga', 'spain( ?)league(?![:alpha:])'= ' spain_la_liga',
      
      'france( ?)ligue( ?)([1]?)(?![[:alpha:]|[:digit:]|[:punct:]])'=' france_ligue_one ', 
      '(?<![[:alpha:]|[:digit:]|[:punct:]])ligue( ?)1(?![[:alpha:]|[:digit:]|[:punct:]])'=' france_ligue_one ',
      
      'engl(and|ish)( ?)fa([\\-]|[ ]|)((cup)?)'=' england_fa_cup',
      'engl(and|ish)( ?)cup'=' england_fa_cup',
      ' fa([\\-]|[ ]|)cup' = ' england_fa_cup',
      
      'u((nited)?)( ?)c((hampion)?)(s?)( ?)l((eague)?)( ?)((qual)?)((ifi)?)((er|cation)?)(s?)'='champions_league', 
      'champ((ion)?)(s?)( ?)lea((gue)?)( ?)((qual)?)((ifi)?)((er|cation)?)(s?)'='champions_league',
      
      '((australian)?)( ?)a(([\\-]|[ ]|)?)league'='a_league',
      
      '#collegefootball'='cfb', '#ipl2020'='ipl',
      'english premier league'='epl',
      '(premier league|england championship)'='soccer', 
      '(?<![:alpha:])([\\s+#]?)(atp|wta)([:]?)'=' tennis ',
      
      #Common tout wording removal
      '(#free(pick(s?)|play(s?))|#gamblingtwitter|#sportsbetting|#inplay|#stampit|#swampnation|#bettipsclub)'=' ', 
      '(?<![:alpha:])(surebet|maxbet|(#)?freeplay|bol|pod|max|tailing|adding|bought|record|bestbet|bodog)(?![:alpha:])'=' ',
      'asshole'=' ',
      
      #Fix spacing problems between key variables initially
      '(?<=( o| u))[-](?=[0-9])'='', '(?<=[:alpha:])[-]'=' -', '(?<=[:alpha:])[+]'=' +',
      
      #Fix half pointers
      ' 1/2u'=' 0.5u', '(?<=[ ])-1/2( |$)'='-0.5','(?<=[:digit:]) 1/2( |$)'='.5', '(?<![:digit:]) 1/2( |$)'='0.5', 
      
      #date fixes/touting record transform
      '(?<!([-+]|[()]|[\\.]|[0-9]))((([0-9]{1,3})[-/\\\\]([0-9]{1,3})(?![0-9]))((?![:alpha:])([-]?)([0-9]{1,3})?))(?![:alpha:])'='iamafuckingtout',
      '(?<![:alpha:])live(?![:alpha:])'='  iamafuckingtout  ',
      
      #Fix sport conversions to make it easier
      'cfb|college football'='ncaaf', 'cbb|college basketball'='ncaab',
      
      #One off - team name that has 'under' in it 
      'thunder'='okc', '49ers'='fortyniners', '76ers'='seventysixers', 'boston college'='bc', '(?<=[:alpha:])[ ]+st((ate|[\\.])?)(?![:alpha:])'='_st',
      
      #Odd one off fixes
      '[0-9]{1,2}(pm|p( ?)e(s?)t)'='', '(buy half point|half point|buy half|halfpoint)'=' ', ' total runs'=' ', ' runs'=' ', ' vs([\\.]?) '='/', ' v '='/', 
      #'u23'='LTTWENTYTHREEYRSOLD', 
      ' / '='/',
      '(?<=[:digit:])/(?=[:alpha:])'=' /',
      
      #Player Prop Nomenclature Standardization
      #NFL/Football
      '(?<![:alpha:])( ?)y(a?)(r?)d(s)(?![:alpha:])'='_yd',
      '(?<![:alpha:])( ?)t((ouch)?)d((own)?)(?![:alpha:])'='_td',
      '(?<![:alpha:])((to)?)( ?)score(s?)( ?)(a?)(?=( _td))'='anytime',
      '(?<![:alpha:])r(u?)sh((ing)?)(?![:alpha:])'='rush', 
      '(?<![:alpha:])p(a?)ss((ing)?)(?![:alpha:])'='pass', 
      '(?<![:alpha:])(reception(s?)|catch((es)?)|recps)(?![:alpha:])'='recv_rec',
      '(?<![:alpha:])comp((letion)?)(s?)(?![:alpha:])'='comp',
      '(?<![:alpha:])att((empt)?)(s?)(?![:alpha:])'='_att',
      '(?<![:alpha:])rece((i)?)(v?)((ing)?)(?![:alpha:])'='recv',
      '(?<![:alpha:])tot((al)?)( ?)_(?=(yd|td))'='tot_',
      '(?<=(rush|pass|recv|anytime))( ?)_(?=(yd|td))'='_',
      '(_td|_yd) (rush|pass|recv)'='\\2\\1',
      '(?<=(anytime_td))( ?)scorer'='',
      '1st( ?)(?=_td)'='first',
      '1st( ?)(score)'='first_score',
      '(on _td prop(s?) this)'='iamafuckingtout',
      #NBA/Basketball
      '3pt FG'='plyr_three_pt_fgs',
      '(?<![:alpha:])(3( ?)s)(?![:alpha:])'='plyr_three_pt_fgs',
      '(?<![:alpha:])(points|pts)(([ ]?)([+]?)([ ]?))(assist(s?)|asts)(?![:alpha:])'='plyr_pts_ast',
      '(?<![:alpha:])(assist(s?)|asts)(?![:alpha:])'='plyr_ast',
      '(?<![:alpha:])(rebound(s?)|reb|boards)(?![:alpha:])'='plyr_reb',
      '(?<![:alpha:])(points|pts)(?![:alpha:])'='plyr_pts',
      '(?<![:alpha:])(pra)(?![:alpha:])'='plyr_pts_reb_ast',
      #MLB
      '(?<![:alpha:])(k( ?)s)(?![:alpha:])'='plyr_strikeouts',
      #NHL
      '(?<![:alpha:])(sog)(?![:alpha:])'='plyr_shots_on_goal',
      
      #Bet outcome standardization
      '([ ]|[(])[p][k]([/]|[ ])'=' +0 ', '([ ]|[(])[e][v]([/]|[ ])'=' +100 ', '([ ]|[(])[r][l]([/]|[ ]|[)(])'=' -1.5 ', ' pl | rl '= ' -1.5 ',
      '([ ]|[(])[p][l]([/]|[ ]|[)])'=' -1.5 ', 'puck line'=' -1.5 ', '3-way'='', 'reg total'='', 'moneyline'='ml', 'team total'='tt',
      '[$]1k'='$1000', '(even money|(?<![:alpha:])(even)(?![:alpha:]))'='+100', '(?<![:alpha:])m((oney)?)( ?)l((ine)?)(?![:alpha:])'=' ml ',
      'both teams ((to)?)( ?)score'=' btts ', '(?<![:alpha:])( ?)alt((ernate)?)( ?)(?![:alpha:])'='alt_',
      
      #Bet period standardization
      "(first( ?)half|1((st)?)( ?)h((alf)?)(?!(ttp))|fh(?![:alpha:]))"=" fh ",
      "(second( ?)half|2((nd)?)( ?)h((alf)?)(?!(ttp)))"=' sh ', 
      '(1((st)?)( ?)q((ua)?)(r?)(t?)(e?)(r?)|f((irst)?)( ?)q((uarter)?)|q1)'=' fq ', 
      '(2((nd)?)( ?)q((ua)?)(r?)(t?)(e?)(r?)|second( ?)q((uarter)?)|q2)'=' sq ',
      '(3((rd)?)( ?)q((ua)?)(r?)(t?)(e?)(r?)|th(i?)rd( ?)q((uarter)?)|q3)'=' tq ', 
      '(4((th)?)( ?)q((ua)?)(r?)(t?)(e?)(r?)|fourth( ?)q((uarter)?)|q4)'=' fq ', 
      '(1((st)?)( ?)p(e?)(r?)((io)?)d|1p(?![m])|first( ?)period)'=' fp ', 
      '(2((nd)?)( ?)p(e?)(r?)((io)?)d|2p(?![m])|second( ?)period)'=' sp ',
      '(f((irst)?)( ?)f((ive)?)( ?)((inning)?)(s?))'=' ff ',
      '(?<!([:alpha:]|[0-9]))f5(?!([:alpha:]))'= ' ff ',
      '(?<!([:alpha:]|[0-9]))([1]((st)?)( ?)(5|f((ive))))'=' ff ',
      #|f5 innings|first 5|5 innings|f5
      '1st set'=' fp ',
      
      '(game|gm)[\\s+](?=[12])'=' gm', '1st game'='gm1', '2nd game'='gm2',
      
      #Unit standardization
      '(?<![:alpha:])unit(s?)(?![:alpha:])'='u'

      )) %>%
    #Remove https
    remove_https() %>%
    
    #Risk to win common talk
    str_replace_all(., '(risk)([\\s+]*)([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})([\\s+]*)((u|unit(s?))?)([\\s+]*)(to)([\\s+]*)(win)([\\s+]*)([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})([\\s+]*)((u|unit(s?))?)',
                    risk_to_win_descr_transformation) %>%
    
    #Standardize how Over/Unders are Referenced (Totals Terminology Standardization)
    str_replace_all(., '((([/]|)[ou]|[ou][vn]([d]|)[e][r]|[ou][vn])([\\s+]|)[\\d+|.|-|+])|([ou][/-]([0-9]|[//.]))|(over|under)([ ()-]{0,2})(?=[:digit:])', totals_notation_regex) %>%
    
    #Word hyphen clean up
    str_replace_all(., '[a-z][-]', word_hyphen_regex) %>%
    
    #Standardize how Odds/Spreads are Referenced (Spreads/Handicaps Typo Clean up)
    str_replace_all(., '([\\s+]|)[-+]([\\s+]|)([\\d+]|[.])', odds_spread_hyphen_typo_regex) %>%
    
    #Standardize Spreads/Handicaps Fractional Numeric Notation
    #str_replace_all(., '[-+ou]([\\d+]|[\\s+]|)([\\d+]|[\\s+]|)([\\d+]|[\\s+]|)([\\d+]|[\\s+]|)[\\d+][/][\\d+]', fractional_handicap_reference_regex) %>%
    
    #Standardize the unit notation -- Dollar Sign Referencing
    str_replace_all(., '[$]\\d+[^[:alpha:]]', unit_dollar_reference_regex) %>%
    
    #Standardize the unit/odds notation -- Risk/To Win Format standardization
    str_replace_all(., '([(]([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})[/]([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})[u])|([\n]|[ ]|)([(]|)([\\d+]|[.])([\\d+]|[.]|)([\\d+]|)[/]([\\d+]|[.])([\\d+]|[.]|)([\\d+]|)[u]([)]|)', risk_to_win_unit_transformation) %>%
    
    #Standardize the unit notation
    str_replace_all(., '([\\s+]([\\d+]|[.])([\\d+]|[.]|)([\\d+]|[.]|)([ ]|)([ux]))|([0-9][ ][u][^0-9])|([0-9][\\.][0-9][*])|[0-9][*]|[.]?[0-9][ ][u](?![:digit:])', standardize_unit_notation) %>%
    
    #Standardize teaser nomenclature
    str_replace_all(., '(teaser|^|[ ]+)([+]?)([0-9]{1,2})(([\\.]5)?)(([/-]|[ ]+)?)(p((oi)?)(n?)t(s?))(([ ]+)?)((teaser)?)|teaser(([ ]+)?)([+]?)([0-9]{1,2})(([\\.]5)?)|(([ ]+)?)([+]?)([0-9]{1,2})(([\\.]5)?)([ ]+)teaser', standardize_teaser_notation) %>%
    
    #Standardize the unit notation with parenthesis
    str_replace_all(., '[(]([0-9]|[\\.])([0-9]|[\\.]|)([0-9]|[.]|)([u]|)[)]', standardize_unit_notation_with_parenthesis) %>%
    
    #Standardize team totals
    str_replace_all(., '[\\s+][t][t]([\\s+]|[+-ou])', standardize_team_totals_notation) %>%
    
    #Numeric to american odds
    str_replace_all(., '(ml|[-+ou]([0-9]{1,2}(([\\.][0-9]{1,2})?)|[\\.][0-9]{1,2}))\\s+[0-9][\\.][0-9]{1,2}\\s+(\\w+)', numeric_quoted_odds_to_american) %>%
    
    #Hashtag team identifiers -- goTeamgo
    str_replace_all(., '#go(.*?) ', remove_hashtag_go_team_go) %>%
    
    #Last -- get rid of all remaining unnecessary punctuation
    str_replace_all(., c('[!?:#*]'='', '[()]'=' ', '[\\.]{3,}'=''))
}


# CARRY FORWARD / CARRYBACKWARD BET SLIP CRITERIA INFORMATION FUNCTIONS ---------------------------------------------------------

#Find / Implement carry back unit values (input a list)
#Takes each list of split tweets (/n) 
carry_unit_values <- function(.data) {
  idf <- .data
  #Find unit carry backward cases (if any)
  carry_values <-
    lapply(1:length(idf), function(x) {
      if(nchar(str_replace_all(idf[[x]]$slip, c('\\s+'='', '(allplays|all)?([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})(u)(each|live|plays)?'='')))==0) {
        str_extract_all(idf[[x]]$slip, '([0-9]{0,2}[\\.][0-9]{0,2}|[0-9]{0,2})(u )')[[1]]
      } else {
        NA
      }
    }) %>%
    invoke(c, .)
  
  #If there are no carrybackward values -- do nothing, else
  if(any(!is.na(carry_values))) {
    #Transform into a data frame for decisioning on carry forward/backward
    carry_values <-
      carry_values %>%
      as.data.frame %>%
      setNames('value') %>%
      mutate(
        index = row_number(),
        carry_fwd = na.locf(carry_values, na.rm = F),
        carry_bwd = na.locf(carry_values, fromLast = T, na.rm = F),
        carry_fwd = ifelse(is.na(carry_fwd), carry_bwd, carry_fwd),
        carry_bwd = ifelse(is.na(carry_bwd), carry_fwd, carry_bwd)
      )
    
    #Best logic i could find for figuring this out
    carry <-
      if(is.na(last(carry_values$value))) {
        carry_values$carry_fwd
      } else {
        carry_values$carry_bwd
      }
    
    #store the indexes of the carryback
    to_remove <- which(!is.na(carry_values$value))
    
    #Remove the indexes that were carried back
    carry <- carry[-to_remove]
    idf <- idf[-to_remove]
    
    #Append them
    for(x in 1:length(idf)) {
      if(!is.na(carry[x])) { 
        idf[[x]]$slip <- paste(idf[[x]]$slip, carry[x]) 
      }
      idf[[x]]$index <- x
    }
  }
  return(idf)
}

#Function to return word assignment -- WHAT IS THIS WORD TELLING US
get_word_assignment <- function(text, ref_sports, ref_periods) {
  if(text %in% ref_sports) {
    'sport'
  } else {
    if(text %in% ref_periods) {
      'period'
    } else {
      if(str_detect(text, '[-+][0-9]{3}')) {
        'odds'
      } else {
        if(str_detect(text, '[-+ou][0-9]|[-+ou][\\.][0-9]') & str_detect(text, '[0-9][u]') == F) {
          'handicap'
        } else {
          if(text %in% c('ml', 'team_total', 'btts') | str_detect(text, '_td')) {
            'market'
          } else { 
            if(str_detect(text, '[0-9][u]')) {
              'units'
            } else { 'null' }
          }
        }
      }
    }
  }
}

#Function to split vector by ' ' into a data frame with index
split_child_words <- function(text, ref_sports) {
  text %>% 
    .$slip %>%
    str_split(., " ") %>%
    as.data.frame %>%
    setNames('word') %>%
    mutate(word = str_replace_all(word, c('\\s+'=' '))) %>%
    filter(nchar(str_replace_all(word, c('\\s+'=''))) > 0) %>%
    mutate(word = ifelse(str_detect(word, paste(ref_sports, collapse = '|')), str_extract(word, paste(ref_sports, collapse = '|')), word)) %>%
    mutate(child_index = row_number())
}

american_to_decimal_odds <- function(string) {
  tryCatch({
    value <- as.numeric(paste(string))
    if(value > 100) {
      value/100
    } else {
      100/abs(value)
    }
  }, error = function(e) { 100000 })
}

#To win unit declaration handling
to_win_unit_declaration_handling <- function(.data) {
  idf <- .data
  
  #Detect if 'to win 123u' is found in the slip
  to_win_df <-
    idf %>%
    group_by(slip, slip_index) %>%
    summarise(
      odds_stated = 'odds' %in% assignment,
      units_stated = 'units' %in% assignment,
      max_child = max(child_index)
    ) %>%
    as.data.frame %>%
    mutate(to_win_unit_declaration_found = str_detect(slip, 'win[ ]{0,}[0-9]{0,4}([\\.]?)[0-9]{1,4}u')) %>%
    arrange(slip_index)
  
  #If it is not -- return the slip
  if(!any(to_win_df$to_win_unit_declaration_found)) { return(idf) }
  
  #If it is -- find the valid receivers of this unit declaration
  to_win_df <-
    to_win_df %>%
    mutate(unit_declaration = suppressWarnings(ifelse(to_win_unit_declaration_found == T, as.numeric(str_replace_all(slip, c('win'='', 'u'='', '[ ]{0,}'=''))), NA)),
           valid_receiver = (odds_stated == T & units_stated == F)) %>%
    filter(valid_receiver | to_win_unit_declaration_found)
  
  #If there are no valid receivers, return the original slip
  if(nrow(to_win_df) < 1 | !any(to_win_df$valid_receiver)) { return(idf) }
  
  #If there are valid receivers,
  #Do the math on the unit transofmraiton,
  #Append to original slip and return
  idf %>%
    rbind(., 
          to_win_df %>%
            mutate(add_unit = na.locf(unit_declaration, na.rm = F)) %>%
            filter(valid_receiver == T) %>%
            left_join(
              idf %>%
                filter(assignment == 'odds') %>%
                dplyr::select(slip_index, odds=word) %>%
                distinct(),
              by = 'slip_index'
            ) %>%
            rowwise() %>%
            mutate(word = paste0(add_unit/american_to_decimal_odds(odds), 'u')) %>%
            as.data.frame %>%
            mutate(child_index = max_child+1,
                   assignment = 'units',
                   scalar = 1) %>%
            dplyr::select(slip, slip_index, word, child_index, assignment, scalar)
    )
}


#Function to count the NAs before modifications
count_row_na <- function(.data) {
  list(
    .data,
    lapply(1:nrow(.data), function(i) {
      sum(is.na(.data[i,])) %>%
        as.data.frame %>%
        setNames('na_cnt')
    }) %>%
      invoke(rbind, .)
  ) %>%
    invoke(cbind.data.frame, .)
}

#Function to clean up outcome/event text
clean_event_outcome <- function(text) {
  split <- str_split(str_trim(str_replace_all(text, c('[*{}]'='', '#'='', '-'='/', '[\\.]'=' '))), '/')[[1]]
  
  lapply(1:length(split), function(i) {
    text_spl <- str_split(split[i], ' ')[[1]]
    text_spl <- text_spl[nchar(text_spl) > 0]
    #POSTPROCESS TEXT REMOVAL
    worthless_word <-
      (text_spl %in% c('free', 'add', 'and', 'the', 'play', 'tonight', 
                       'card', 'also', 'have', 'from', 'earlier', 'lean', 
                       'pod', 'on', 'in', 'for', 'going', 'to', 'last',
                       'minute', 'day', 'dmd', 'model', 'lets', 'go', 'maxbet',
                       'winner', 'we', 'stay', 'hot', 'taking', 'premium',
                       'ush', 'push', 'bestbet', 'x', 'bought', 'halfpoint',
                       'fade', 'already', 'that', 'than', 'only', 'my', 'tomorrow',
                       'who', 'are', 'night', 'more', 'like', 'get', 'easy',
                       'possible', 'coming', 'live', 'another', 'scores', 'purchase',
                       'now', 'other', 'just', 'cashes', 'cash', 'easy', 'bet',
                       'or', 'plays', 'system', 'days', 'it', 'odds', 'our', 'of',
                       'this', 'painful', 'seals', 'exactly', 'boom', 'one', 'had', 'then',
                       'draftkings', 'fanduel', 'early', 'sent', 'out'))
    #Digit or punct only
    digit_punct_only <- lapply(str_replace_all(text_spl,c('[0-9]{2,4}'='', '[+]'='')), nchar) %>% invoke(c, .) == 0
    
    paste(str_trim(text_spl[(worthless_word == F & digit_punct_only == F)]), collapse = ' ')
  }) %>%
    invoke(c, .) %>%
    paste(., collapse = '/')
}


#Function to carrybackward key assignment criteria via a premature linebreak
period_assignment_lb_carry_fwd <- function(.data) {
  #Data assignment
  idf <- .data
  
  #Conditions/Declarations that you will CARRY FORWARD
  ref_conditions <- c('period')
  
  #Additional assignments you will allow for it to be a valid carryforward (in addition to ref_conditions)
  valid_carryforward_conditions <- c(ref_conditions, 'sport')
  
  #Assignments that designate a row as carryforward eligible
  eligible_carryforward_receiver <- c('handicap', 'odds')
  
  #Filter for the conditions that appear in the dataset
  ref_conditions <- ref_conditions[ref_conditions %in% idf$assignment]
  eligible_carryforward_receiver <- eligible_carryforward_receiver[eligible_carryforward_receiver %in% idf$assignment]
  
  #This can't happen if there are no rows/units/odds
  if(max(idf$slip_index) > 1 & length(ref_conditions) > 0 & length(eligible_carryforward_receiver) > 0) {
    
    #Binary counts for each 'slip' assignment
    assignment_matrix <- 
      idf %>%
      dcast(., slip_index~assignment, value.var = 'assignment', fun.aggregate = length)
    
    #Find the valid carryforward rows
    valid_carryforward_rows <- assignment_matrix[,ref_conditions] == 1 & rowSums(assignment_matrix[,colnames(assignment_matrix)[!(colnames(assignment_matrix) %in% c(valid_carryforward_conditions, 'slip_index'))]]) == 0
    
    #Find the valid carryforward receivers
    valid_carryforward_receivers <- rowSums(assignment_matrix[,eligible_carryforward_receiver, drop = F]) > 1 & assignment_matrix[,ref_conditions] == 0
    
    if((sum(valid_carryforward_rows) > 0) & sum(valid_carryforward_receivers) > 0) {
      #Create the frame that will serve as the carry forward
      carryforward_frame <-
        data.frame(
          valid_carryforward_rows,
          valid_carryforward_receivers
        ) %>%
        mutate(slip_index = row_number()) %>%
        left_join(
          idf %>% filter(assignment == ref_conditions) %>% dplyr::select(slip_index, word),
          by = 'slip_index'
        ) %>%
        mutate(word = ifelse(!is.na(word), word, ifelse(valid_carryforward_receivers == FALSE, '', word)),
               word = na.locf(word, na.rm = F)) %>%
        filter(!is.na(word), valid_carryforward_rows == F, word != '') %>%
        dplyr::select(slip_index, word) %>%
        mutate(assignment = ref_conditions,
               scalar = 1)
      
      #Now append
      idf <- 
        list(
          idf,
          carryforward_frame %>%
            inner_join(
              idf %>%
                group_by(slip, slip_index) %>%
                summarise(child_index = max(child_index)+1) %>%
                as.data.frame,
              by = 'slip_index')
        ) %>%
        invoke(rbind, .) %>%
        arrange(slip_index, child_index)
    }
  }
  return(idf)
}

#Function to carrybackward key assignment criteria via a premature linebreak
premature_lb_carry_back <- function(.data) {
  #As of now, these conditions only apply for odds/units/periods
  ref_conditions <- c('units', 'odds', 'period')
  
  #Data assignment
  idf <- .data
  
  #Filter for the conditions that appear in the dataset
  ref_conditions <- ref_conditions[ref_conditions %in% idf$assignment]
  
  #This can't happen if there are no rows/units/odds
  if(max(idf$slip_index) > 1 & length(ref_conditions) > 0) {
    
    len_mat <- 
      idf %>%
      dcast(., slip_index~assignment, value.var = 'assignment', fun.aggregate = length)
    
    for(x in 2:nrow(len_mat)) { 
      for(z in 1:length(ref_conditions)) {
        #Condition that this line break only has odds/units assignments
        condition1 <- sum(len_mat[x, -1])==sum(len_mat[x, ref_conditions]) 
        
        if(condition1 == T) {
          #Condition that this line break only has 1 odd/1 unit/1 period assignment and the line previous has none of them
          condition2 <- len_mat[x,ref_conditions[z]] == 1 & len_mat[(x-1),ref_conditions[z]] == 0
          #If condition1 and 2 are both true, essentially remove the line break via indexing the parent frame
          if(condition2 == T) {
            pos_index <- match(paste(x, ref_conditions[z]), paste(idf$slip_index, idf$assignment))
            word_id <- 
              idf %>% filter(slip_index == (x-1)) %>% .$child_index %>% max
            
            idf$slip_index[pos_index] <- (x-1)
            idf$child_index[pos_index] <- word_id
          }
        }
      }
    }
  }
  
  return(idf)
}

#Function to structure bet slip
parse.bet.slip <- function(.data, quiet) {
  #Print initial stupid tweet
  if(quiet == F) { cat(paste(paste0('\n\n\nThe original tweet:'), gsub('_lb_', ' \n', .data), sep = '\n')) }
  
  #Clean the initial tweet!
  tweet_text <- 
    .data %>%
    initial_tweet_preprocess(.)
  
  #Print cleaned tweet
  if(quiet == F) { cat(paste(paste0('\n\n\nThe preprocessed tweet:'), tweet_text, sep = '\n')) }
  
  #Breaks the tweet by line breaks \n
  slip_parent_breaks <-
    tweet_text %>%
    str_split(., '\n') %>%
    as.data.frame %>%
    setNames('slip') %>%
    mutate(slip = str_replace_all(slip, c('\\s+'=' '))) %>%
    filter(slip != ' ') %>%
    mutate(slip_index = row_number()) %>%
    split(., .$slip_index) %>%
    carry_unit_values()
  

  #Breaks each line break tweet into words
  bet_slip_frame <-
    suppressWarnings(
    suppressWarnings(Map(cbind.data.frame, 
                         slip_parent_breaks,
                         slip_parent_breaks %>%
                           map(., ~split_child_words(., ref_sports))
    )) %>%
    invoke(rbind, .) %>%
    rowwise() %>%
    mutate(assignment = get_word_assignment(word, ref_sports, ref_periods)) %>%
    #Check against recap/other identifiers
    mutate(is_recap_flag = str_detect(word, recap_bag_of_words_regex),
           is_pp_flag = str_detect(word, post_process_removal_words_regex),
           is_bet_nomenclature = str_detect(word, 'game|g(m?)[0-9]|halfpoint'),
           scalar = 1) %>%
    #For weird format allowability (ie: Braves ML vs Brewers) we need to check if its valid
    group_by(slip_index) %>%
    mutate(
      check_flg = ifelse(
        #has more than 3 words in the slip
        max(child_index) >= 3 & child_index == 1 &
          assignment == 'null' & lead(assignment, 2) == 'null' &
          lead(assignment, 1) %in% c('market', 'handicap') & str_detect(lead(word, 1), '[ou][\\d+]') == F &
          lead(is_recap_flag, 2) == F & lead(is_pp_flag,2) == F & lead(is_bet_nomenclature,2) == F,
        'outcome', NA
      ),
      check_flg = ifelse(
        #has more than 3 words in the slip
        max(child_index) >= 3 & child_index == 3 &
          assignment == 'null' & lag(assignment, 2) == 'null' &
          lag(assignment, 1) %in% c('market', 'handicap') & str_detect(lag(word, 1), '[ou][\\d+]') == F &
          lag(is_recap_flag,2) == F & lag(is_pp_flag,2) == F & lag(is_bet_nomenclature,2) == F,
        'event', check_flg
      )) %>%
    as.data.frame %>%
    mutate(assignment = ifelse(is.na(check_flg), assignment, check_flg)) %>%
    dplyr::select(-check_flg, -is_recap_flag, -is_pp_flag, -is_bet_nomenclature)
    )
  
  
  bet_slip <-
    bet_slip_frame %>%
    period_assignment_lb_carry_fwd() %>%
    premature_lb_carry_back() %>% #Call the unit carryforward/backward
    to_win_unit_declaration_handling() %>%
    group_by(slip_index, assignment) %>%
    summarise(
      value = paste(unique(word), collapse = ' ')
    ) %>%
    full_join(
      lapply(1:max(bet_slip_frame$slip_index), function(i) {
        data.frame(assignment = c('sport', 'period', 'odds', 'handicap', 'units', 'market', 'event', 'outcome', 'null'), stringsAsFactors = F) %>%
          mutate(slip_index = i)
      }) %>% invoke(rbind, .),
      by = c('assignment', 'slip_index')
    ) %>%
    as.data.frame %>%
    dcast(slip_index~assignment, value.var = 'value') %>%
    count_row_na(.) %>%
    mutate(sport = na.locf(sport, na.rm = F),
           full_nonidentified_values = str_replace_all(paste(null, event, outcome, sep = ' '), c('(?<![:alpha:])NA(?![:alpha:])'='')),
           null = str_trim(str_replace_all(null, c('[*]'='')))) %>%
    filter(na_cnt != 8,
           nchar(str_replace_all(full_nonidentified_values, c('[[:digit:]]'='', '[[:punct:]]'='', '\\s+'='')))>0,
           str_detect(full_nonidentified_values, c(recap_bag_of_words_regex)) == F,
           str_detect(full_nonidentified_values, '%') == F,
           market == 'ml' | !is.na(odds) | !is.na(handicap) | !is.na(period) | !is.na(event) | !is.na(outcome)) %>%
    rowwise() %>%
    mutate(null = clean_event_outcome(null),
           null_len = length(str_split(null, ' ')[[1]]),
           handicap = str_split(handicap, ' ')[[1]] %>% first) %>%
    filter(null_len < 6) %>%
    as.data.frame %>%
    mutate(period = ifelse(is.na(period), 'match', period),
           odds = ifelse(is.na(odds), '-110', as.numeric(odds)),
           units = as.numeric(str_replace_all(units, c('[u]'=''))),
           event = ifelse(is.na(outcome) & is.na(event), event, full_nonidentified_values),
           market = ifelse(market == 'ml', 'moneyline', market),
           market = ifelse(is.na(market) & str_detect(handicap, '[ou]') == T, 'total', market),
           market = ifelse(is.na(market) & str_detect(handicap, '[+-]') == T, 'spread', market),
           market = ifelse(is.na(market), 'moneyline', market),
           outcome = ifelse(market != 'total', outcome, ifelse(str_detect(handicap, 'o'), 'over', 'under')),
           outcome = ifelse(market == 'team_total', paste(null, ifelse(str_detect(handicap, 'o'), 'over', 'under')), outcome),
           event = ifelse(market %in% c('team_total', 'spread', 'moneyline', 'btts'), event, null),
           outcome = ifelse(!is.na(outcome), outcome, ifelse(market %in% c('team_total', 'spread', 'moneyline', 'btts'), null, NA)),
           units = abs(as.numeric(ifelse(is.na(units), 1, str_replace_all(units, c('[*]'=''))))),
           handicap = as.numeric(str_trim(str_replace_all(handicap, c('[-+][0-9][\\.][0-9][0-9][u][ ]'='', '[-+][0-9][\\.][0-9][u][ ]'='','[-+][0-9][u][ ]'='', '[ou+]'=''))))
    ) %>%
    as.data.frame %>%
    rowwise() %>%
    mutate(outcome = str_replace_all(outcome, c('[\\.]'=''))) %>%
    as.data.frame %>%
    dplyr::select(sport, event, period, market, outcome, handicap, odds, units)
    
  if(quiet == F) { cat('\n\n\nThe bet slip:\n\n') }
  return(bet_slip)
}