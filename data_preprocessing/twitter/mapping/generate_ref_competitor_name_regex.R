# !diagnostics off

#Function to transform foreign letters to english
to.plain <- function(s) {
  # 1 character substitutions
  old1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
  new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions
  old2 <- c("œ", "ß", "æ", "ø")
  new2 <- c("oe", "ss", "ae", "oe")
  s2 <- s1
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
}


#Provide a vector of words and it will combine them for strings containing any of the individual words
return.word.combination.regex <- function(word_vector) {
  if(length(word_vector) == 1) { return(word_vector) }
  
  lapply(1:length(word_vector), function(index) {
    word_list <-
      list(
        words_before_index = if(index == 1) { '' } else { word_vector[0:(index-1)] },
        word_at_index <- word_vector[index],
        words_after_index = if(index == length(word_vector)) { '' } else { word_vector[(index+1):length(word_vector)] }
      )
    
    lapply(1:length(word_list), function(i) {
      if(paste(word_list[[i]], collapse = '') == '') {} else {
        if(i == 2) {
          paste(word_list[[i]], collapse = '( ?)')
        } else {
          paste(paste0('((', word_list[[i]], ')?)'), collapse = '( ?)')
        }
      }
    }) %>%
      plyr::compact() %>%
      invoke(c, .) %>%
      paste(., collapse = '( ?)')
  }) %>%
    paste(., collapse = '|')
}

#Function to determine class of the word inside a vector
competitor.word.generic <- function(word_vector, generic_words, complex_word_regex, non_optional_words_regex) {
  complex_word_regex_mod <- paste0('^', complex_word_regex, '$')
  lapply(1:length(word_vector), function(i) {
    if(word_vector[i] %in% generic_words) { return(list(binary = TRUE, word = paste0('((', word_vector[i], ')?)'))) }
    ref_complex_detection <- str_detect(word_vector[i], complex_word_regex_mod)
    if(any(ref_complex_detection)) { return(list(binary = TRUE, word = paste0('((', complex_word_regex[first(which(ref_complex_detection))], ')?)'))) }
    ref_non_optional_detection <- str_detect(non_optional_words_regex, paste0('^', word_vector[i], '$'))
    if(any(ref_non_optional_detection)) { return(list(binary = TRUE, word = word_vector[i])) }
    return(list(binary = FALSE, word = word_vector[i]))
  })
}

generate.competitor.regex <- function(competitor_name, path_link) {
  if(competitor_name == 'Southern U') { return(paste0('southern u')) }
  if(competitor_name == 'L.A. Clippers') { competitor_name <- 'Los Angeles Clippers' }
  
  competitor_name <- 
    competitor_name %>%
    str_replace_all(., c('( ?)St([\\.]?)( ?)(?![:alpha:])'=' Saint ', 'St$'='State', '^CS '='California State ', '^SE '='South Eastern ',
                         '^FC[ ]'= '((fc)?)( ?)', '^AC[ ]'='((ac)?)( ?)', '^Inter[ ]'='((inter)?)( ?)', '[ ]CF$'='( ?)((cf)?)',
                         '^SC[ ]'='((sc)?)( ?)'))

  #Generic/optional soccer competitor words
  generic_single_words <- c('fc', 'fk', 'cd', 'sv', 'deportivo', 'nk', 'al', 'sc', 'de', 'club', 'cf', 'union', 'sk', 'santa',
                            'lfc', 'tsv', 'ac', 'sao', 'moscow', 'ff', 'bk', 'la', 'ud', 'ik', 'town', 'hapoel', 'ifk', 'fsv',   'sp', 'vfb', 
                            'rovers', 'deportes', 'tus', 'afc', 'ec', 'inter', 'hnk', 'sokol', 'universidad', 
                            'shabab', 'ks', 'am', 'jk', 'zagreb', 'mfk', 'stal', 'tsg', 'praha', 'if', 'gks', 'fv', 'se', 'sd', 'county',
                            'borough', 'stade', 'i', 'ii', 'jr', 'the', 'ca', 'u')
  
  #Words that can (In the Database!) be used interchangably
  generic_complex_words <- c('sport((ing)?)', 'u(n?)(i?)t(e?)d', 'at(h?)letic(o?)', '((rot|blau)?)( ?)weiss', '(d?)el')
  
  #Words that are non optional
  non_optional_words <- c('state', 'saint', 'west((ern)?)', 'north((ern)?)', 'south((ern)?)', 'east((ern)?)', 'st')
  
  #Generate the list of words to word replacements
  word_replacement_list <-
    list(
      word = c('dallas', 'cowboys', 'miami', 'arizona', 'cincinnati', 'denver', 'chicago', 'detroit', 'jacksonville', 'las vegas',
               'cleveland', 'baltimore', 'carolina', 'green bay', 'buffalo', 'atlanta', 'indianapolis', 'houston', 'kansas city',
               'los angeles', 'minnesota', 'new england', 'new orleans', 'new york', 'philadelphia', 'pittsburgh', 'san francisco',
               'seattle', 'tampa bay', 'tennessee', 'washington', 'bills', 'ravens', 'cowboys', 'chiefs', 'jaguars', 'colts', 'rams',
               'packers', 'jets', 'texans', 'raiders', 'saints', 'eagles', 'giants', 'chargers', 'vikings', 'dolphins', 'patriots', '49ers',
               'seahawks', 'buccaneers', 'football team', 'cardinals', 'state', 'georgia', 'florida', 'colorado', 'tech', 'east((ern)?)', 'michigan',
               'carolina', 'central', 'west((ern)?)', 'indiana', 'kentucky', 'kent', 'ohio', 'maryland', 'massachusetts', 'mississippi', 'middle',
               'nevada', 'nebraska', 'north((ern)?)', 'new mexico', 'texas', 'oregon', 'san diego', 'san jose', 'temple', 'rutgers',
               'vanderbilt', 'virginia', 'wisconsin', 'wyoming', 'wake forest', 'utah', 'california', 'lafayette', 'toledo', 'san antonio',
               'south((ern)?)', 'alabama', 'syracuse', 'oklahoma', 'notre dame', 'purdue', 'memphis', 'louisville', 'louisiana', 'kansas', 'college',
               'arkansas', 'coastal', 'bowling green', 'illinois', 'charlotte', 'clemson', 'ball', 'xavier', 'villanova', 'weber', 'wofford',
               'wright state', 'youngstown', 'siena', 'sacred', 'sacramento', 'heart', 'rhode island', 'portland', 'marquette', 'penn', 'hampshire',
               'appalachian', 'boston', 'georgetown', 'gonzaga', 'akron', 'baylor', 'pepperdine', 'stanford', 'connecticut', 'dakota', 'liberty',
               'wichita', 'marys ca', 'auburn', 'creighton', 'rio grande valley', ' johns', 'wisconsin green bay', 'marshall', 'wisconsin milwaukee',
               'texas san antonio', 'the citadel', 'wizards', 'cavaliers', 'hornets', 'brooklyn', 'clippers', 'pelicans', 'nuggets', 'lakers',
               'toronto', 'raptors', 'celtics', 'timberwolves', 'oklahoma city', 'mavericks', 'knicks', 'trail blazers', '76ers', 'phoenix', 'orlando',
               'golden ', 'warriors', 'spurs', ' christian', 'joseph', 'md baltimore county', 'grambling state', 'boise state', 'nj tech',
               'morehead state', 'nicholls state', 'missouri', 'fresno state', 'cal irvine', 'pelicans', 'saint', 'mount', 'bonaventure',
               'louis', 'vegas', 'golden knights', 'avalanche', 'islanders', 'coyotes', 'capitals', 'edmonton', 'vancouver', 'canucks', 'lightning',
               'grizzlies', 'nashville', 'calgary', 'senators', 'columbus', 'blue', 'jackets', 'rangers', 'predators', 'penguins', 'ottawa',
               'kings', 'milwaukee', 'anaheim', 'manchester united', 'manchester city', 'maple leafs', 'sharks', 'montreal', 'winnipeg',
               'new jersey', 'devils', 'united', 'barcelona', 'valparaiso', 'juventus', 'villarreal', 'canadiens', 'bears', 'hurricanes', 'atletico ',
               'abilene', 'edwardsville', 'paris', 'angels', 'padres', 'yankees', 'diamondbacks', 'orioles', 'red sox', 'cubs', 'white sox',
               'indians', 'rockies', 'astros', 'royals', 'dodgers', 'marlins', 'brewers', 'twins', 'mets', 'athletics', 'phillies', 'pirates',
               'giants', 'mariners', 'rays', 'rangers', 'blue jays', 'nationals', 'oakland', 'vfb stuttgart', 'fsv mainz'),
      
      replacement = c('dal((las)?)', '((cow)?)boys', 'mia((mi)?)', 'a(r|z|i)((i|n|z)?)((zona)?)|((ari)?)zona', 
                      'cin((ci|cy|cinnat(t?)i)?)', 'den((ver)?)', 'ch(i?)((cago)?)', 'det((roit)?)', 'ja(x|cksonville)',
                      '(l?)((as)?)( ?)v((egas)?)', 'cle(v?)((eland)?)', 'bal(t?)((imore)?)', 'c((ar)?)((olina)?)', 'g((reen)?)( ?)b((ay)?)',
                      'buf(f?)((alo)?)', '((at)?)l((anta)?)', 'ind((y|ianapolis)?)', 'hou((ston)?)', 'k((ansas)?)( ?)c((ity)?)', 'l((os)?)( ?)a((ngeles)?)',
                      'm(i?)n(n?)((y|esota)?)', 'n((ew)?)( ?)e((ngland)?)', '(n((ew)?)( ?)o((rleans)?)|nola)', 'n((ew)?)( ?)y((ork)?)', 'phi(l?)((adelphia|ly)?)',
                      'pit(t?)(s?)((burgh)?)', 's((an)?)( ?)f((ran)?)((cisc)?)(o?)', 'sea((ttle)?)', 't((am)?)(p?)(a?)( ?)(b?)((ay)?)', 't((en)?)(n?)((essee)?)',
                      'w(a?)(s?)(h?)((ington)?)', 'bills((mafia)?)', 'ravens((flock)?)', '((cow)?)boys', '(kan|chiefs((kingdom)?))', 'jag((uar)?)s', 
                      'c(o?)lt(s?)', 'r((ams)?)', '(((go)?)pack((ers)?)((go)?)|lambo|gnb)', '((go)?)j((ets)?)((go)?)', 't(e?)x(a?)n(s?)', '(r((aiders)?)|(oak(land)?))', '(saints|r)', '((fly)?)ea((gles)?)((fly)?)',
                      'g((iants|men)?)', 'c((harge)?)(r?)(s?)', 'vik(ing|e)s', '(dolphins|fins)', '(pat((riot)?)(s?)|nwe)', '((forty)?)( ?)niners',
                      '((sea)?)ha(wk|kw)s', 'buc(c?)((aneer)?)(s?)', '(f((oo)?)((tball)?)( ?)t((eam)?)|((red)?)skins)', 'card((inal)?)(s?)', 's(t?)((ate)?)',
                      'g((eorgi)?)(a?)', 'f(l?)((orid)?)(a?)', 'c((ol)?)((orado)?)', 't(e?)((ch)?)', 'e((ast)?)((ern)?)', 'm(i?)((ch)?)((igan)?)', 'c((ar)?)((olina)?)',
                      'c((ent)?)((ral)?)', 'w((est)?)((ern)?)', 'i((nd)?)((iana|y)?)', 'k((en)?)((tuck)?)(y?)', 'k((ent)?)', 'o(h?)((io)?)', 'm((arylan)?)d', 'mass((achusetts)?)',
                      'm(i?)(s?)s((issippi)?)', 'mid((dle)?)', 'nev((ada)?)', 'neb((raska)?)', 'n((orth)?)((ern)?)', 'n((ew)?)( ?)m((ex)?)((ico)?)', 't(e?)(x?)((as)?)', 'o(r?)(e?)((gon)?)',
                      's((an)?)( ?)d((iego)?)', 's((an)?)( ?)j((ose)?)', 'tem((ple)?)', 'rut((gers)?)', 'vand(erbilt|y)', 'v((irgini)?)(a?)', 'wis(c?)(o?)((nsin)?)',
                      'wy(o?)((ming)?)', 'w((ake)?)( ?)(f?)((orest)?)', 'uta(h?)', 'c((al)?)((ifornia)?)', 'l((afayette)?)', 'tol((edo)?)', 's((an)?)( ?)a((ntonio)?)',
                      's(o?)((uth)?)((ern)?)', '(((ala)?)bama|ala)', '((syr)?)(a?)cuse', 'o(k?)((la)?)((homa)?)', 'n((o(tr|rt)e)?)( ?)d((ame)?)', 'pur((due)?)', 'mem(p?)((his)?)', 'lou((isville)?)',
                      'l((ouisi)?)(a?)((na)?)', 'k((an)?)((sas)?)', 'c((ollege)?)', 'a(r?)(k?)((ansas)?)', 'coast((al)?)', 'b((owling)?)( ?)g((reen)?)( ?)((s?)(t?)((ate)?)?)', 'ill((inois|y)?)',
                      'ch((a|o)?)(r?)((lotte)?)', 'clem((son)?)', 'b((all)?)', 'xav((ier)?)', '((villa)?)nova', 'web((er)?)', 'wof(f?)((ord)?)', 
                      'w(r?)((igh)?)(t?)( ?)(s?)(t?)((ate)?)', 'y((oungstown)?)', 'sien(a?)', 'sac((red)?)', 'sac((ramento)?)', 'h((ea)?)rt', 'r((hode)?)( ?)i((sland)?)', 'por(t?)((land)?)', 'marq((uette)?)',
                      'p((enn)?)', 'h((ampshire)?)', 'a((pp)?)((alachian)?)', 'b(o?)(s?)((ton)?)', 'g((eorge)?)((town)?)', '(gon|zags)(z?)((aga)?)', 'akr((on)?)', 'bay((lor)?)',
                      'pepp((er)?)((dine)?)', 'stan((ford)?)', 'conn((ecticut)?)', 'd((ak)?)((ota)?)', 'lib((erty)?)', 'wich((ita)?)', 'mar(y?)(s?)( ?)((ca)?)', 'au(b?)((urn)?)',
                      'crei((gh)?)((ton)?)', 'r((io)?)( ?)g((rande)?)( ?)v((alley)?)', '( ?)j((ohn)?)(s?)', '((wisc((onsin)?)( ?))?)( ?)g((reen)?)( ?)b((ay)?)',
                      'm(a?)rsh((all)?)', '((w((isc)?)((onsin)?))?)( ?)m((il)?)((waukee)?)', 't(e?)(x?)((as)?)( ?)s((an)?)( ?)a((ntonio)?)', '((the)?)( ?)citadel',
                      'wiz((ards)?)', 'cav((alier)?)(s?)', '(hornets|buzz( ?)city)', 'b((roo)?)(k|r)((ly)?)(n|k)', 'c((lip)?)((per)?)(s?)', 'p((el)?)((ican)?)(s?)', 'nug((get)?)(s?)',
                      'l((ake)?)((rs)?)', 'tor((onto)?)', '(rap((tor)?)(s?)|wethenorth)', 'c((eltic)?)(s?)', '(t?)( ?)((imber)?)wol((ves|fs)?)', 'o(k?)((lahoma)?)( ?)c((ity)?)', 'mav((erick)?)(s?)',
                      'k((nicks)?)', '((trail)?)( ?)blazers', '((seventy)?)sixers', 'ph(o?)((eni)?)(x?)', 'orl((ando)?)', 'g((olden)?)( ?)', 'w((arriors)?)', 's((purs)?)',
                      '( ?)chris((tain)?)', 'jo(s?)e((ph)?)', 'm(d?)( ?)b((altimore)?)( ?)c((ounty)?)', 'gramb((ling)?)( ?)(s?)(t?)((ate)?)', 'boise( ?)(s?)(t?)((ate)?)',
                      'nj( ?)(i?)((nstitute)?)( ?)t((ech)?)', 'morehead( ?)(s?)(t?)((ate)?)', 'nicholls( ?)(s?)(t?)((ate)?)', 'mi(ss|zz)((ou)?)((ri)?)', 'fresno( ?)(s?)(t?)((ate)?)',
                      'c((al)?)( ?)irv((ine)?)', 'pel((ican)?)s', 's((ain)?)t', 'm((oun)?)t', 'bon((aventure)?)', 'l((ou)?)((is)?)', 'v((egas)?)', '(g?)((olden)?)( ?)k((nights)?)', 'av((alanche|s)?)',
                      'i((sl(e|a))?)((nder)?)(s?)', '((co)?)yotes', 'cap((ital)?)(s?)', 'edm((onton)?)', 'van((couver)?)', '((ca)?)nucks', '(l((ightning)?)|((go)?)bolt(s?))',
                      'grizz((l(i?)es)?)', 'n(a?)sh((ville)?)', 'c((al)?)g((ar)?)y', 'sen((ator)?)s', 'c((olumbus)?)', 'b((lue)?)', 'j((ackets)?)', 'r((angers)?)', 'pred((ator)?)(s?)',
                      'pen((guin)?)(s?)', 'ott((awa)?)', 'k((ings)?)', 'mil((waukee)?)', 'ana((heim)?)', 'man((chester)?)( ?)(u?)((ni)?)(t?)(e?)(d?)', 'man((chester)?)( ?)((city)?)',
                      '(m?)((aple)?)( ?)leafs', 's((hark)?)(s?)', '(m((on)?)t((rea)?)l|habs)', 'w((inni)?)p(e?)g', 'n((ew)?)( ?)j((ersey)?)', 'd((evils)?)', '(u?)((ni)?)(t?)(e?)(d?)',
                      'barc((elon)?)a', 'valp((arais)?)o', 'juve((ntus)?)', 'villar(r?)eal', 'canadi(e|a)ns', '((da)?)( ?)bears', '((hurri)?)canes', '((at(h?)letico)?)( ?)',
                      'a(l?)b(((e|i)l)?)((ene)?)', 'e((dwardsville)?)', 'p((aris)?)', 'a((ngels)?)', '(p((ad)?)((re)?)(s?)|slam( ?)((diego)?))', '(y((ank)?)((ee)?)(s?)|nny)', 'd((iamond)?)( ?)b((acks)?)',
                      "o((riole|[']| )?)(s?)", '(r((ed)?)( ?)s((ox)?(s?))|rox)', '(cub((bie)?)(s?)|chc)', '(w((hite)?)( ?)s((ox)?)|c(h?)w(s?)|chi((sox)?))', '(indians|tribe)', 'r((ock)?)((ie|y)?)(s?)',
                      '(a?)stro(s?)', 'r((oy)?)((al)?)(s?)', 'd((odger)?)(s?)', '(m((ar)?)((lin)?)(s?)|fish)', 'b((rew)?)((er)?)(s?)', 'twin(s?)', 'm((et)?)((tropolitan)?)(s?)',
                      "(a((thletic| )?)(s?)|a[']s)", 'phi(l?)(l?)((ie|y)?)(s?)', 'p((ir)?)((ate)?)(s?)', 'g(i?)((ant)?)(s?)', 'm((ar)?)((iner)?)(s?)', '((devil)?)r((ay)?)(s?)',
                      'r((ang)?)((er)?)(s?)', '((b((lue)?))?)( ?)j((ay)?)(s?)', 'n((at)?)((ional)?)(s?)', 'oak((land)?)', '((vfb)?)( ?)stuttgart', '((fsv)?)( ?)mainz')
    ) %>%
    invoke(cbind.data.frame, .) %>%
    mutate_all(., ~paste(.)) %>%
    arrange(desc(nchar(word)))
  
  #Add the appendage information for college mascots/specific teamnames that are not found in the name
  append_specific_names <-
    list(
      competitor_name = c('Akron', 'Florida State', 'Clemson', 'LSU', 'Ohio State', 'Miami Florida', 'Minnesota', 'Oregon', 'Purdue', 'Wisconsin',
                          'USC', 'UCLA', 'Tennessee', 'TCU', 'Florida', 'Notre Dame', 'Oklahoma', 'NC State', 'Wisconsin', 'Tulane', 'Texas State',
                          'Texas', 'Tennessee', 'Stanford', 'Mississippi', 'South Carolina', 'Marshall', 'Utah', 'Seton Hall', 'San Diego State',
                          'Rutgers', 'Buffalo U', 'Washington', 'Kent State', 'Illinois', 'Kansas', 'Arkansas'),
      
      append_name = c('zips', '((semi)?)noles', 'tigers', 'tigers', 'buckeyes', '((hurri)?)canes|the( ?)u', '((golden)?)( ?)gophers', 'ducks', 'boiler((makers)?)', 'badgers',
                      'trojans', 'bruins', 'volunteers', 'hornfrogs', 'gators', '((fighting)?)irish', 'sooners', 'w((olf)?)pack|tuffy',
                      'badger(s?)', 'green wave', 'bobcats', '((long)?)horns', 'volunteer(s?)', 'card(inal(s?)|s)', 'ole( ?)miss', 'gamecock(s?)',
                      '((thundering)?)( ?)herd', 'utes', 'pirates', 'aztecs', 'scarlett knights', 'bulls', 'huskies', 'kent', '((the)?)( ?)((fighting)?)( ?)illini',
                      'jayhawks', 'razorbacks')
    )
  
  #Roman numerals|numbers|etc
  generic_addages <- c('[0-9]+')
  
  #Add the appendages
  to_append <- ''
  
  #Add the append
  if(competitor_name %in% append_specific_names$competitor_name) {
    to_append <- paste0('|', append_specific_names$append_name[match(competitor_name, append_specific_names$competitor_name)])
  }
  
  #Create a flag for if its a collegiate sport
  college_flag <- grepl('college', path_link)
  
  #Make the potential changes
  #Check if there is already a U in the name
  if(college_flag == T & str_detect(competitor_name, 'U') == F) {
    to_append_abbr <- str_extract_all(competitor_name %>% 
                                        str_replace_all(., c('The'='', ' CA$'='')), '([A-Z]|[&])') %>% unlist()
    if(length(to_append_abbr) > 1) {
      to_append <- paste0(to_append, '|(u?)', to_append_abbr %>% tolower %>% paste(., collapse = ''), '(u?)')
    }
    competitor_name <- paste0('(u?)', competitor_name, '(u?)')
  }
  
  #Find the regex component of the non optional words
  non_optional_words_regex_component <-
    non_optional_words %>% 
    map(., ~word_replacement_list$replacement[str_detect(., word_replacement_list$word)]) %>% 
    invoke(c, .) %>%
    str_replace_all(., c('[()?^$]'=''))
  
  #Start making modifications of the name (convert to regex)
  name <- 
    competitor_name %>%
    tolower() %>%
    str_replace_all(., c("[\\.]"='',
                         "[']"="((['\\\\s+])?)",
                         '[-/]'=' ',
                         '( ?)[&]( ?)'='( ?)([&]?)( ?)')) %>%
    str_trim() %>%
    str_replace_all(., c('\\s+'=' ')) %>%
    str_replace_all(., word_replacement_list$replacement %>% setNames(word_replacement_list$word)) %>%
    str_trim() %>%
    to.plain()
  
  #Split by spaces
  name_vector <- str_split(name, ' (?![)?])') %>% unlist
  
  #Find if the words are generic or not
  word_generic_index <- competitor.word.generic(name_vector, generic_single_words, c(generic_complex_words, generic_addages), non_optional_words_regex_component)
  
  #Extract the binary classifier portion of the word generic index
  word_is_generic_vector <- word_generic_index %>% map(1) %>% invoke(c, .)
  
  #Get total non-generic words
  total_non_generic_words <- sum(word_is_generic_vector)
  
  #If there are no generics, simply return the combinations
  if(total_non_generic_words == 0) {
    return(paste0('^(', return.word.combination.regex(name_vector), to_append, ')$'))
  }
  
  #If there is only one non generic, return it
  if((length(word_generic_index)-total_non_generic_words) == 1) {
    return(paste0('^(', paste(name_vector, collapse = '( ?)'), to_append, ')$'))
  }
  
  #Extract the word portion of the index
  word_vector_adj <- word_generic_index %>% map(2) %>% invoke(c, .)
  
  #If all words are generic, reassign one
  if(sum(word_is_generic_vector) == length(word_is_generic_vector)) {
    reassignment_index <- which.max(nchar(name_vector))
    word_is_generic_vector[reassignment_index] <- FALSE
    word_vector_adj[reassignment_index] <- name_vector[reassignment_index]
  }
  
  #Find first/last word in the vector is non-generic
  non_generic_word_index <-
    list(
      first = first(which(!word_is_generic_vector)),
      last = last(which(!word_is_generic_vector))
    )
  
  #Replace that vector position with the modified string
  word_vector_adj[non_generic_word_index$first] <- paste(word_vector_adj[1:non_generic_word_index$first], collapse = '( ?)')
  word_vector_adj[non_generic_word_index$last] <- paste(word_vector_adj[non_generic_word_index$last:(length(word_vector_adj))], collapse = '( ?)')
  
  if(non_generic_word_index$first > 1) { 
    word_vector_adj <- word_vector_adj[-(1:(non_generic_word_index$first-1))] 
    word_is_generic_vector <- word_is_generic_vector[-(1:(non_generic_word_index$first-1))]
  }
  if(non_generic_word_index$last < length(word_vector_adj)) { 
    word_vector_adj <- word_vector_adj[-((non_generic_word_index$last+1):length(word_vector_adj))]
    word_is_generic_vector <- word_is_generic_vector[-((non_generic_word_index$last+1):length(non_generic_word_index))]
  }
  
  if(sum(word_is_generic_vector) == 0) { return(paste0('^(', return.word.combination.regex(word_vector_adj), to_append, ')$')) }
  
  #Create the modified vector to pass through the regex builder
  modified_name_vector <-
    data.frame(
      word = word_vector_adj,
      flag = word_is_generic_vector,
      stringsAsFactors = F
    ) %>%
    mutate(absolute_index = row_number(),
           modified_index = ifelse(flag == TRUE, NA, absolute_index),
           modified_index_crry = na.locf(modified_index, na.rm = F, fromLast = T)) %>%
    group_by(modified_index_crry) %>%
    arrange(absolute_index) %>%
    summarise(word = paste(word, collapse = '( ?)')) %>%
    .$word %>%
    paste
  
  return(paste0('^(', return.word.combination.regex(modified_name_vector), to_append, ')$'))
}

#Function to perform the regex generation file
generate.bovada.competitor.regex.file <- function() {
  #Read in the competitors file, group by competitor name and collapse
  competitor_names <- 
    readRDS('./preprocessed_data/bovada/ref_data/ref_competitors.rds') %>%
    group_by(competitor_name) %>%
    summarise(path_link = paste(unique(path_link), collapse = ', ')) %>%
    as.data.frame %>%
    filter(competitor_name %in% c('Home Goals', 'Away Goals', 'Home Total Runs', 'Away Total Runs') == F)
  
  #Iterate through each competitor name and generate the regex field
  competitor_names$regex <- lapply(1:nrow(competitor_names), function(x) { 
    print(x) 
    generate.competitor.regex(competitor_names$competitor_name[x], competitor_names$path_link[x]) 
    }) %>% 
    invoke(c, .)
  
  #Dummy loop to test the validity of the regex
  error_catch <- NA
  for(i in 1:nrow(competitor_names)) {
    tryCatch({
      attempt_to_find_dummy <- str_detect('thisisadummystring', competitor_names$regex[i])
    }, error = function(e) {
      print('There was an error!')
      print(competitor_names[i,])
      error_catch <- 'error'
      break
    })
  }
  
  #Save if no error occurred
  if(is.na(error_catch)) {
    saveRDS(
      competitor_names %>%
        dplyr::select(competitor_name, regex) %>%
        distinct(),
      './data_preprocessing/ref_data/competitor_name_regex.rds'
    )
  }
}