#Function that generates the reference mapping tables -- ideally this would be read in eventually from a sourced file
generate.parse.field.mapping.tables <- function() {
  #The only sport provided that is one:many is when the user states 'college'
  #That sport can be mapped to multiple path_links
  sport_to_link <-
    list(
      list(
        sport = c('nfl', 'ncaab', 'ncaaf', 'ncaa', 'ncaa', 'nba', 'nhl', 'mlb'),
        
        path_link = c('/football/nfl', '/basketball/college-basketball', '/football/college-football', 
                      '/basketball/college-basketball', '/football/college-football', '/basketball/nba',
                      '/hockey/nhl', '/baseball/mlb')
      ) %>%
        invoke(cbind.data.frame, .) %>%
        mutate_all(., ~paste(.)),
      
      data.frame(
        sport = c('england_pl','italy_series_a','spain_la_liga','bundesliga','france_ligue_one','england_fa_cup', 'euro_cup', 'copa', 'concacaf', 
                  'mls', 'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'soccer',
                  'champions_league',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis',
                  'tennis'),
        path_link = c('/soccer/england-premier-league/premier-league', '/soccer/italy-serie-a/serie-a', '/soccer/spain-la-liga/la-liga', 
                      '/soccer/germany-bundesliga/bundesliga', '/soccer/france-ligue-1/ligue-1', '/soccer/europe/england/english-fa-cup/fa-cup',
                      '/soccer/uefa-euro-2020/euro-2020-matches', '/soccer/copa-america/copa-america', '/soccer/concacaf-gold-cup/gold-cup', 
                      '/soccer/major-league-soccer/mls', '/soccer/asia/australia/australia-a-league',
                      '/soccer/asia/japan/japan-j1-league',
                      '/soccer/brazil-serie-a/serie-a',
                      '/soccer/europe/austria/bundesliga',
                      '/soccer/europe/belarus/vysshaya-liga',
                      '/soccer/europe/belgium/eerste-klasse-a',
                      '/soccer/europe/denmark/superligaen',
                      '/soccer/europe/england/league-two',
                      '/soccer/europe/england/national-league',
                      '/soccer/europe/germany/bundesliga-2',
                      '/soccer/europe/hungary/liga-nb1',
                      '/soccer/europe/italy/serie-b',
                      '/soccer/europe/netherlands/dutch-eredivisie',
                      '/soccer/europe/norway/eliteserien',
                      '/soccer/europe/norway/obos-ligaen',
                      '/soccer/europe/romania/liga-1',
                      '/soccer/europe/russia/premier-league',
                      '/soccer/europe/slovakia/slovakia-corgon-liga',
                      '/soccer/europe/switzerland/super-league',
                      '/soccer/europe/turkey/super-lig',
                      '/soccer/north-america/costa-rica/primera-division',
                      '/soccer/south-america/chile/primera-division',
                      '/soccer/south-america/ecuador/serie-a',
                      '/soccer/uefa-champions-league/champions-league',
                      '/tennis/atp/acapulco',
                      '/tennis/atp/atp-cup',
                      '/tennis/atp/australian-open-men-s',
                      '/tennis/atp/barcelona',
                      '/tennis/atp/french-open-men-s',
                      '/tennis/atp/great-ocean-road-open',
                      '/tennis/atp/hamburg',
                      '/tennis/atp/madrid',
                      '/tennis/atp/miami',
                      '/tennis/atp/monte-carlo',
                      '/tennis/atp/murray-river-open',
                      '/tennis/atp/newport',
                      '/tennis/atp/rome',
                      '/tennis/atp/wimbledon',
                      '/tennis/wta/australian-open-women-s',
                      '/tennis/wta/budapest',
                      '/tennis/wta/charleston',
                      '/tennis/wta/dubai',
                      '/tennis/wta/french-open-women-s',
                      '/tennis/wta/gippsland-trophy',
                      '/tennis/wta/lausanne',
                      '/tennis/wta/madrid',
                      '/tennis/wta/miami',
                      '/tennis/wta/prague',
                      '/tennis/wta/rome',
                      '/tennis/wta/wimbledon',
                      '/tennis/wta/yarra-valley-classic',
                      '/tennis/atp/belgrade',
                      '/tennis/wta/istanbul',
                      '/tennis/wta/stuttgart',
                      '/tennis/challenger/rome',
                      '/tennis/challenger/salinas',
                      '/tennis/challenger/tallahassee',
                      '/tennis/itf-men/itf-turkey-f16-men-singles',
                      '/tennis/itf-men/itf-egypt-f16-men-singles',
                      '/tennis/itf-men/itf-france-f5-men-singles',
                      '/tennis/itf-women/itf-turkey-16a-women-singles',
                      '/tennis/itf-women/itf-tunisia-16a-women-singles',
                      '/tennis/itf-women/itf-portugal-2a-women-singles',
                      '/tennis/other-tennis/exhibition/utr-pro-tennis-series-canberra',
                      '/tennis/wta/charleston-2',
                      '/tennis/challenger/split-2',
                      '/tennis/challenger/orlando',
                      '/tennis/challenger/belgrade',
                      '/tennis/itf-men/itf-germany-f2-men-singles',
                      '/tennis/itf-men/itf-spain-f6-men-singles',
                      '/tennis/itf-men/itf-tunisia-f15-men-singles',
                      '/tennis/itf-women/itf-portugal-1a-women-singles',
                      '/tennis/itf-women/itf-turkey-15a-women-singles',
                      '/tennis/itf-women/itf-france-10a-women-singles',
                      '/tennis/atp/halle',
                      '/tennis/atp/queens',
                      '/tennis/wta/berlin',
                      '/tennis/wta/birmingham',
                      '/tennis/challenger/almaty-ii',
                      '/tennis/challenger/prostejov',
                      '/tennis/challenger/aix-en-provence',
                      '/tennis/challenger/forli',
                      '/tennis/challenger/nottingham-2',
                      '/tennis/itf-women/itf-france-15a-women-singles',
                      '/tennis/itf-women/itf-spain-08a-women-singles',
                      '/tennis/itf-women/itf-czech-republic-02a-women-singles',
                      '/tennis/itf-women/itf-sweden-03a-women-singles',
                      '/tennis/itf-women/itf-great-britain-01a-women-singles',
                      '/tennis/atp/munich',
                      '/tennis/atp/estoril',
                      '/tennis/challenger/rome-2',
                      '/tennis/challenger/ostrava',
                      '/tennis/challenger/salinas-2',
                      '/tennis/itf-women/itf-croatia-01a-women-singles',
                      '/tennis/challenger/little-rock',
                      '/tennis/challenger/biella-7',
                      '/tennis/itf-men/itf-dominican-republic-f1-men-singles',
                      '/tennis/itf-men/itf-finland-f1-men-singles',
                      '/tennis/itf-men/itf-turkey-f22-men-singles',
                      '/tennis/itf-women/itf-dominican-republic-01a-women-singles',
                      '/tennis/itf-women/itf-italy-02a-women-singles',
                      '/tennis/itf-women/itf-russia-03a-women-singles',
                      '/tennis/itf-women/itf-bosnia-herzegovina-01a-women-singles',
                      '/tennis/itf-women/itf-slovenia-02a-women-singles'),
        stringsAsFactors = F
      )
    ) %>%
    invoke(rbind, .)

  period_to_link <-
    list(
      #NFL/College football have identical periods available
      #College basketball does not have quarters
      list(
        path_link = c('/football/nfl', '/basketball/college-basketball', '/football/college-football', '/basketball/nba', '/hockey/nhl', '/baseball/mlb',
                      '/football/nfl', '/basketball/college-basketball', '/football/college-football', '/basketball/nba',
                      '/football/nfl', '/basketball/college-basketball', '/football/college-football', '/basketball/nba',
                      '/football/nfl', '/football/college-football', '/basketball/nba',
                      '/football/nfl', '/football/college-football', '/basketball/nba',
                      '/football/nfl', '/football/college-football', '/basketball/nba',
                      '/football/nfl', '/football/college-football', '/basketball/nba',
                      '/hockey/nhl', '/hockey/nhl', '/hockey/nhl',
                      '/baseball/mlb', '/baseball/mlb'),
        period = c('match', 'match', 'match', 'match', 'match', 'match',
                   'fh', 'fh', 'fh', 'fh',
                   'sh', 'sh', 'sh', 'sh',
                   'fq', 'fq', 'fq',
                   'sq', 'sq', 'sq',
                   'tq', 'tq', 'tq',
                   'frq', 'frq', 'frq',
                   'fp', 'sp', 'tp',
                   'ff', 'first_inning'),
        period_description = c('Match', 'Match', 'Match', 'Match', 'Match', 'Match',
                               'First Half', 'First Half', 'First Half', 'First Half',
                               'Second Half', 'Second Half', 'Second Half', 'Second Half',
                               '1st Quarter', '1st Quarter', '1st Quarter',
                               '2nd Quarter', '2nd Quarter', '2nd Quarter',
                               '3rd Quarter', '3rd Quarter', '3rd Quarter',
                               '4th Quarter', '4th Quarter', '4th Quarter',
                               '1st Period', '2nd Period', '3rd Period',
                               '5 Inning Line', '1st Inning')
      ) %>%
        invoke(cbind.data.frame, .) %>%
        mutate_all(., ~paste(.)),
      
      data.frame(
        period = c('match', 'fs', 'ss'),
        period_description = c('Match', '1st Set', '2nd Set'),
        dummy = rep(1, 3),
        stringsAsFactors = F
      ) %>%
        left_join(
          data.frame(
            path_link = c('/tennis/atp/acapulco',
                          '/tennis/atp/atp-cup',
                          '/tennis/atp/australian-open-men-s',
                          '/tennis/atp/barcelona',
                          '/tennis/atp/french-open-men-s',
                          '/tennis/atp/great-ocean-road-open',
                          '/tennis/atp/hamburg',
                          '/tennis/atp/madrid',
                          '/tennis/atp/miami',
                          '/tennis/atp/monte-carlo',
                          '/tennis/atp/murray-river-open',
                          '/tennis/atp/newport',
                          '/tennis/atp/rome',
                          '/tennis/atp/wimbledon',
                          '/tennis/wta/australian-open-women-s',
                          '/tennis/wta/budapest',
                          '/tennis/wta/charleston',
                          '/tennis/wta/dubai',
                          '/tennis/wta/french-open-women-s',
                          '/tennis/wta/gippsland-trophy',
                          '/tennis/wta/lausanne',
                          '/tennis/wta/madrid',
                          '/tennis/wta/miami',
                          '/tennis/wta/prague',
                          '/tennis/wta/rome',
                          '/tennis/wta/wimbledon',
                          '/tennis/wta/yarra-valley-classic',
                          '/tennis/atp/belgrade',
                          '/tennis/wta/istanbul',
                          '/tennis/wta/stuttgart',
                          '/tennis/challenger/rome',
                          '/tennis/challenger/salinas',
                          '/tennis/challenger/tallahassee',
                          '/tennis/itf-men/itf-turkey-f16-men-singles',
                          '/tennis/itf-men/itf-egypt-f16-men-singles',
                          '/tennis/itf-men/itf-france-f5-men-singles',
                          '/tennis/itf-women/itf-turkey-16a-women-singles',
                          '/tennis/itf-women/itf-tunisia-16a-women-singles',
                          '/tennis/itf-women/itf-portugal-2a-women-singles',
                          '/tennis/other-tennis/exhibition/utr-pro-tennis-series-canberra',
                          '/tennis/wta/charleston-2',
                          '/tennis/challenger/split-2',
                          '/tennis/challenger/orlando',
                          '/tennis/challenger/belgrade',
                          '/tennis/itf-men/itf-germany-f2-men-singles',
                          '/tennis/itf-men/itf-spain-f6-men-singles',
                          '/tennis/itf-men/itf-tunisia-f15-men-singles',
                          '/tennis/itf-women/itf-portugal-1a-women-singles',
                          '/tennis/itf-women/itf-turkey-15a-women-singles',
                          '/tennis/itf-women/itf-france-10a-women-singles',
                          '/tennis/atp/halle',
                          '/tennis/atp/queens',
                          '/tennis/wta/berlin',
                          '/tennis/wta/birmingham',
                          '/tennis/challenger/almaty-ii',
                          '/tennis/challenger/prostejov',
                          '/tennis/challenger/aix-en-provence',
                          '/tennis/challenger/forli',
                          '/tennis/challenger/nottingham-2',
                          '/tennis/itf-women/itf-france-15a-women-singles',
                          '/tennis/itf-women/itf-spain-08a-women-singles',
                          '/tennis/itf-women/itf-czech-republic-02a-women-singles',
                          '/tennis/itf-women/itf-sweden-03a-women-singles',
                          '/tennis/itf-women/itf-great-britain-01a-women-singles',
                          '/tennis/atp/munich',
                          '/tennis/atp/estoril',
                          '/tennis/challenger/rome-2',
                          '/tennis/challenger/ostrava',
                          '/tennis/challenger/salinas-2',
                          '/tennis/itf-women/itf-croatia-01a-women-singles',
                          '/tennis/challenger/little-rock',
                          '/tennis/challenger/biella-7',
                          '/tennis/itf-men/itf-dominican-republic-f1-men-singles',
                          '/tennis/itf-men/itf-finland-f1-men-singles',
                          '/tennis/itf-men/itf-turkey-f22-men-singles',
                          '/tennis/itf-women/itf-dominican-republic-01a-women-singles',
                          '/tennis/itf-women/itf-italy-02a-women-singles',
                          '/tennis/itf-women/itf-russia-03a-women-singles',
                          '/tennis/itf-women/itf-bosnia-herzegovina-01a-women-singles',
                          '/tennis/itf-women/itf-slovenia-02a-women-singles'),
            dummy = rep(1, 80),
            stringsAsFactors = F
          ),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy),
      
      
      
      
      #Soccer has many links and they are all the same, this is a modified version of above
      data.frame(
        period = c('match', 'fh', 'sh'),
        period_description = c('Regulation Time', 'First Half', 'Second Half'),
        dummy = rep(1, 3),
        stringsAsFactors = F
      ) %>%
        left_join(
          data.frame(
            path_link = c('/soccer/england-premier-league/premier-league', '/soccer/italy-serie-a/serie-a', '/soccer/spain-la-liga/la-liga', 
                          '/soccer/germany-bundesliga/bundesliga', '/soccer/france-ligue-1/ligue-1', '/soccer/europe/england/english-fa-cup/fa-cup',
                          '/soccer/uefa-euro-2020/euro-2020-matches', '/soccer/copa-america/copa-america', '/soccer/concacaf-gold-cup/gold-cup',
                          '/soccer/major-league-soccer/mls', '/soccer/asia/australia/australia-a-league',
                          '/soccer/asia/japan/japan-j1-league',
                          '/soccer/brazil-serie-a/serie-a',
                          '/soccer/europe/austria/bundesliga',
                          '/soccer/europe/belarus/vysshaya-liga',
                          '/soccer/europe/belgium/eerste-klasse-a',
                          '/soccer/europe/denmark/superligaen',
                          '/soccer/europe/england/league-two',
                          '/soccer/europe/england/national-league',
                          '/soccer/europe/germany/bundesliga-2',
                          '/soccer/europe/hungary/liga-nb1',
                          '/soccer/europe/italy/serie-b',
                          '/soccer/europe/netherlands/dutch-eredivisie',
                          '/soccer/europe/norway/eliteserien',
                          '/soccer/europe/norway/obos-ligaen',
                          '/soccer/europe/romania/liga-1',
                          '/soccer/europe/russia/premier-league',
                          '/soccer/europe/slovakia/slovakia-corgon-liga',
                          '/soccer/europe/switzerland/super-league',
                          '/soccer/europe/turkey/super-lig',
                          '/soccer/north-america/costa-rica/primera-division',
                          '/soccer/south-america/chile/primera-division',
                          '/soccer/south-america/ecuador/serie-a',
                          '/soccer/uefa-champions-league/champions-league'),
            dummy = rep(1, 34),
            stringsAsFactors = F
          ),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy)
    ) %>%
    invoke(rbind, .)

  
  #Market to displayGroups_id/markets_key gets a bit tricky
  #The id is dependent upon the sport, and the period
  #IE: moneyline for NFL/NCAAF is:
  #displayGroups_id = 100-41 | market_key = '2W-12'
  #for NCAAB it is:
  #displayGroups_id = 100-97 | market_key = '2W-12'
  #This is (probably) explained by path_sportCode
  market_to_keys <-
    list(
      #NFL/NCAAF
      data.frame(
        market = c('team_total', 'spread', 'moneyline', 'total'),
        displayGroup_id = c('100-86', '100-41', '100-41', '100-41'),
        market_key = c('2W-OU', '2W-HCAP', '2W-12', '2W-OU'),
        team_total_flag = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = F
      ) %>%
        mutate(dummy = 1) %>%
        left_join(
          data.frame(
            path_link = c('/football/nfl', '/football/college-football'),
            stringsAsFactors = F
          ) %>%
            mutate(dummy = 1),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy),
      
      #NCAAB
      data.frame(
        market = c('team_total', 'spread', 'moneyline', 'total'),
        displayGroup_id = c('100-101', '100-97', '100-97', '100-97'),
        market_key = c('2W-OU', '2W-HCAP', '2W-12', '2W-OU'),
        team_total_flag = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = F
      ) %>%
        mutate(dummy = 1) %>%
        left_join(
          data.frame(
            path_link = c('/basketball/nba', '/basketball/college-basketball'),
            stringsAsFactors = F
          ) %>%
            mutate(dummy = 1),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy),
      
      #NHL
      data.frame(
        path_link = c('/hockey/nhl', '/hockey/nhl', '/hockey/nhl', '/hockey/nhl'),
        market = c('team_total', 'spread', 'moneyline', 'total'),
        displayGroup_id = c('100-99999999', '100-128', '100-128', '100-128'),
        market_key = c('2W-XXXXX', '2W-HCAP', '2W-12', '2W-OU'),
        team_total_flag = c(TRUE, FALSE, FALSE, FALSE),
        stringsAsFactors = F
      ),
      
      # MLB
      data.frame(
        market = c('moneyline', 'total', 'spread'),
        displayGroup_id = rep('100-136', 3),
        market_key = c('2W-12', '2W-OU', '2W-HCAP'),
        team_total_flag = rep(FALSE, 3),
        path_link = rep('/baseball/mlb', 3),
        stringsAsFactors = F
      ),
      
      #International soccer
      data.frame(
        market = c('team_total', 'spread', 'moneyline', 'total'),
        displayGroup_id = c('100-99999998', '100-23', '100-23', '100-23'),
        market_key = c('2W-XXXXY', '2W-HCAP', '3W-1X2', '2W-OU'),
        team_total_flag = c(TRUE, FALSE, FALSE, FALSE),
        dummy = rep(1, 4),
        stringsAsFactors = F
      ) %>%
        left_join(
          data.frame(
            path_link = c('/soccer/england-premier-league/premier-league', '/soccer/italy-serie-a/serie-a', '/soccer/spain-la-liga/la-liga', 
                          '/soccer/germany-bundesliga/bundesliga', '/soccer/france-ligue-1/ligue-1', '/soccer/europe/england/english-fa-cup/fa-cup',
                          '/soccer/uefa-euro-2020/euro-2020-matches', '/soccer/copa-america/copa-america', '/soccer/concacaf-gold-cup/gold-cup',
                          '/soccer/major-league-soccer/mls', '/soccer/asia/australia/australia-a-league',
                          '/soccer/asia/japan/japan-j1-league',
                          '/soccer/brazil-serie-a/serie-a',
                          '/soccer/europe/austria/bundesliga',
                          '/soccer/europe/belarus/vysshaya-liga',
                          '/soccer/europe/belgium/eerste-klasse-a',
                          '/soccer/europe/denmark/superligaen',
                          '/soccer/europe/england/league-two',
                          '/soccer/europe/england/national-league',
                          '/soccer/europe/germany/bundesliga-2',
                          '/soccer/europe/hungary/liga-nb1',
                          '/soccer/europe/italy/serie-b',
                          '/soccer/europe/netherlands/dutch-eredivisie',
                          '/soccer/europe/norway/eliteserien',
                          '/soccer/europe/norway/obos-ligaen',
                          '/soccer/europe/romania/liga-1',
                          '/soccer/europe/russia/premier-league',
                          '/soccer/europe/slovakia/slovakia-corgon-liga',
                          '/soccer/europe/switzerland/super-league',
                          '/soccer/europe/turkey/super-lig',
                          '/soccer/north-america/costa-rica/primera-division',
                          '/soccer/south-america/chile/primera-division',
                          '/soccer/south-america/ecuador/serie-a',
                          '/soccer/uefa-champions-league/champions-league'),
            dummy = rep(1, 34),
            stringsAsFactors = F
          ),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy),
      
      #International tennis
      data.frame(
        market = c('spread', 'moneyline', 'total'),
        displayGroup_id = c('100-122', '100-122', '100-122'),
        market_key = c('2W-HCAP', '2W-12', '2W-OU'),
        team_total_flag = c(FALSE, FALSE, FALSE),
        dummy = rep(1, 3),
        stringsAsFactors = F
      ) %>%
        left_join(
          data.frame(
            path_link = c('/tennis/atp/acapulco',
                          '/tennis/atp/atp-cup',
                          '/tennis/atp/australian-open-men-s',
                          '/tennis/atp/barcelona',
                          '/tennis/atp/french-open-men-s',
                          '/tennis/atp/great-ocean-road-open',
                          '/tennis/atp/hamburg',
                          '/tennis/atp/madrid',
                          '/tennis/atp/miami',
                          '/tennis/atp/monte-carlo',
                          '/tennis/atp/murray-river-open',
                          '/tennis/atp/newport',
                          '/tennis/atp/rome',
                          '/tennis/atp/wimbledon',
                          '/tennis/wta/australian-open-women-s',
                          '/tennis/wta/budapest',
                          '/tennis/wta/charleston',
                          '/tennis/wta/dubai',
                          '/tennis/wta/french-open-women-s',
                          '/tennis/wta/gippsland-trophy',
                          '/tennis/wta/lausanne',
                          '/tennis/wta/madrid',
                          '/tennis/wta/miami',
                          '/tennis/wta/prague',
                          '/tennis/wta/rome',
                          '/tennis/wta/wimbledon',
                          '/tennis/wta/yarra-valley-classic',
                          '/tennis/atp/belgrade',
                          '/tennis/wta/istanbul',
                          '/tennis/wta/stuttgart',
                          '/tennis/challenger/rome',
                          '/tennis/challenger/salinas',
                          '/tennis/challenger/tallahassee',
                          '/tennis/itf-men/itf-turkey-f16-men-singles',
                          '/tennis/itf-men/itf-egypt-f16-men-singles',
                          '/tennis/itf-men/itf-france-f5-men-singles',
                          '/tennis/itf-women/itf-turkey-16a-women-singles',
                          '/tennis/itf-women/itf-tunisia-16a-women-singles',
                          '/tennis/itf-women/itf-portugal-2a-women-singles',
                          '/tennis/other-tennis/exhibition/utr-pro-tennis-series-canberra',
                          '/tennis/wta/charleston-2',
                          '/tennis/challenger/split-2',
                          '/tennis/challenger/orlando',
                          '/tennis/challenger/belgrade',
                          '/tennis/itf-men/itf-germany-f2-men-singles',
                          '/tennis/itf-men/itf-spain-f6-men-singles',
                          '/tennis/itf-men/itf-tunisia-f15-men-singles',
                          '/tennis/itf-women/itf-portugal-1a-women-singles',
                          '/tennis/itf-women/itf-turkey-15a-women-singles',
                          '/tennis/itf-women/itf-france-10a-women-singles',
                          '/tennis/atp/halle',
                          '/tennis/atp/queens',
                          '/tennis/wta/berlin',
                          '/tennis/wta/birmingham',
                          '/tennis/challenger/almaty-ii',
                          '/tennis/challenger/prostejov',
                          '/tennis/challenger/aix-en-provence',
                          '/tennis/challenger/forli',
                          '/tennis/challenger/nottingham-2',
                          '/tennis/itf-women/itf-france-15a-women-singles',
                          '/tennis/itf-women/itf-spain-08a-women-singles',
                          '/tennis/itf-women/itf-czech-republic-02a-women-singles',
                          '/tennis/itf-women/itf-sweden-03a-women-singles',
                          '/tennis/itf-women/itf-great-britain-01a-women-singles',
                          '/tennis/atp/munich',
                          '/tennis/atp/estoril',
                          '/tennis/challenger/rome-2',
                          '/tennis/challenger/ostrava',
                          '/tennis/challenger/salinas-2',
                          '/tennis/itf-women/itf-croatia-01a-women-singles',
                          '/tennis/challenger/little-rock',
                          '/tennis/challenger/biella-7',
                          '/tennis/itf-men/itf-dominican-republic-f1-men-singles',
                          '/tennis/itf-men/itf-finland-f1-men-singles',
                          '/tennis/itf-men/itf-turkey-f22-men-singles',
                          '/tennis/itf-women/itf-dominican-republic-01a-women-singles',
                          '/tennis/itf-women/itf-italy-02a-women-singles',
                          '/tennis/itf-women/itf-russia-03a-women-singles',
                          '/tennis/itf-women/itf-bosnia-herzegovina-01a-women-singles',
                          '/tennis/itf-women/itf-slovenia-02a-women-singles'),
            dummy = rep(1, 80),
            stringsAsFactors = F
          ),
          by = 'dummy'
        ) %>%
        dplyr::select(-dummy)
      
    ) %>%
    invoke(rbind, .)
  
  #Field mapping file
  field_mapping_list <-
    list(
      sport = sport_to_link,
      period = period_to_link,
      market = market_to_keys
    )
  
  #Store the file
  saveRDS(field_mapping_list, './data_preprocessing/ref_data/field_index_mapping.rds')
}