library(hoopR)
library(parallel)
library(tidyverse)
check_is_team = function(p, team1, team2){
  return(as.character(p) == as.character(team1) | as.character(p) == as.character(team2))
}

SEASON_NAME = 2024
get_pbp_game_sequence_data = function(game_i){
  pbp = hoopR::nba_data_pbp(game_id=game_i) %>% select(period, event_num, clock, description,offense_team_id,locX,locY,player1_id,player2_id,player3_id,home_score,away_score)
  pbp$event_num = as.numeric(pbp$event_num)
  pbp = pbp %>% arrange(event_num)
  players = hoopR::nba_boxscorescoringv2(game_id=game_i)$sqlPlayersScoring %>% select(TEAM_ID, PLAYER_ID) %>% rename(team_id = TEAM_ID, player_id = PLAYER_ID) %>% arrange(team_id) %>% unique()
  teams = players %>% select(team_id) %>% unique() %>% pull('team_id')
  team = players[1,'team_id'] %>% pull()
  player = players[1,'player_id'] %>% pull()
  all_shots = hoopR::nba_shotchartdetail(team_id = team, season=year_to_season(SEASON_NAME), player_id=player, game_id=game_i)$Shot_Chart_Detail
  for(i in 2:nrow(players)){
    team = players[i,'team_id'] %>% pull()
    player = players[i,'player_id'] %>% pull()
    tryCatch({
      player_shots = hoopR::nba_shotchartdetail(team_id = team, season=year_to_season(SEASON_NAME), player_id=player, game_id=game_i)$Shot_Chart_Detail
      all_shots = all_shots %>% rbind(player_shots)
    }, error=function(e){})
    Sys.sleep(1.5)
  }
  all_shots$GAME_EVENT_ID = as.numeric(all_shots$GAME_EVENT_ID)
  all_shots = all_shots %>% arrange(GAME_EVENT_ID) %>% rename(event_num = GAME_EVENT_ID, shot_loc_x = LOC_X, shot_loc_y = LOC_Y) %>% select(event_num,ACTION_TYPE,SHOT_ZONE_AREA, SHOT_ZONE_RANGE, SHOT_DISTANCE,shot_loc_x,shot_loc_y,SHOT_MADE_FLAG)
  colnames(all_shots) = tolower(colnames(all_shots))
  print(colnames(all_shots))
  pbp_w_shots = pbp %>% left_join(all_shots) %>% rename(poss_loc_x = locX, poss_loc_y = locY) 
  ## start building sequence data
  ## sequences start: defensive rebound, offensive rebound, steal, timeout
  ## sequences end: shot (make, miss), turnover
  
  ## init_action: defensive rebound, offensive rebound, steal, timeout
  ## end_action: shot (make or miss), turnover
  ## init_player_id: player responsible for start of sequence (i.e. rebounder, stealer, etc.)
  ## end_player_id: player responsible for ending possess
  ## init_play_loc_x: starting x location of sequence
  ## init_play_loc_y: starting y location of sequence
  ## end_play_loc_x: ending x location of sequence
  ## end_play_loc_y: ending y location of sequence
  ## shot_distance?:
  ## shot_zone?: 
  ## success?: 1 if make, 0 if not
  pbp_w_shots = pbp_w_shots %>% filter(player1_id > 0)
  sequences = tibble() %>% mutate(
    sequence_id = c(1),
    offense_team_id = c(0),
    init_action = c(''),
    end_action = c(''),
    success = as.numeric(c(0)),
    init_player_id = c(''),
    end_player_id = c(''),
    init_play_loc_x = as.numeric(c(0)),
    init_play_loc_y = as.numeric(c(0)),
    end_play_loc_x = as.numeric(c(0)),
    end_play_loc_y = as.numeric(c(0)),
    shot_distance = as.numeric(c(0)),
    shot_zone = c(''),
    shot_result = as.numeric(0),
    shot_type = as.character(''),
    shot_was_tpa = as.numeric(0),
  )
  curr_init_action = ''
  curr_team = ''
  curr_init_player_id = ''
  curr_init_play_loc_x = 0
  curr_init_play_loc_y = 0
  
  for(i in 1:nrow(pbp_w_shots)){
    tryCatch({
      if(i > 1){
        if(curr_init_action == ''){
          if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('rebound') | (sequences[nrow(sequences),] %>% pull(end_action) == 'shot' & sequences[nrow(sequences),] %>% pull(success) == 0)){
            curr_team = pbp_w_shots[i,] %>% pull('player1_id')
            if(!check_is_team(curr_team, teams[1], teams[2]))
            {
              curr_team = players %>% filter(player_id == curr_team) %>% pull('team_id')
            }
            curr_init_action = 'rebound'  
            curr_init_player_id = pbp_w_shots[i,] %>% dplyr::pull('player1_id')
          }
          else if(tolower(pbp_w_shots[i-1, 'description']) %>% str_detect('steal')){
            curr_init_action = 'steal'
            curr_init_player_id = pbp_w_shots[i-1,] %>% dplyr::pull('player3_id')
          }
          else if(tolower(pbp_w_shots[i-1, 'description']) %>% str_detect('turnover')){
            curr_init_action = 'violation'
            curr_init_player_id = pbp_w_shots[i,] %>% dplyr::pull('player1_id')
          }
          else if(tolower(pbp_w_shots[i-1, 'description']) %>% str_detect('timeout')){
            curr_init_action = 'timeout'
            curr_init_player_id = ''
          }
          else{
            curr_init_action = 'inbound'
            curr_init_player_id = pbp_w_shots[i,] %>% dplyr::pull('player1_id')
          }
          curr_init_play_loc_x = pbp_w_shots[i,] %>% dplyr::pull('poss_loc_x')
          curr_init_play_loc_y = pbp_w_shots[i,] %>% dplyr::pull('poss_loc_y')
        }
        else {
          end_success = 0
          end_loc_x = pbp_w_shots[i,] %>% pull('poss_loc_x')
          end_loc_y = pbp_w_shots[i,] %>% pull('poss_loc_y')
          end_shot_distance = -1
          end_shot_type = ''
          end_shot_zone = -1
          end_shot_result = -1
          end_shot_was_tpa = -1
          if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('shot')){
            end_action = 'shot'
            end_success = pbp_w_shots[i,] %>% pull('shot_made_flag')
            end_loc_x = pbp_w_shots[i,] %>% pull('shot_loc_x')
            end_loc_y = pbp_w_shots[i,] %>% pull('shot_loc_y')
            end_shot_distance = pbp_w_shots[i,] %>% pull('shot_distance')
            end_shot_zone = pbp_w_shots[i,] %>% pull('shot_zone_area')
            if(tolower(pbp_w_shots[i,] %>% pull('description')) %>% str_detect('3pt')){
              end_shot_was_tpa = 1
              if(end_success == 1){
                end_shot_result = 3
              }
              else {
                end_shot_result = 0
              }
            }
            else{
              end_shot_was_tpa = 0
              if(end_success == 1){
                end_shot_result = 2
              }
              else {
                end_shot_result = 0
              }
            }
            
            end_shot_type = tolower(pbp_w_shots[i,] %>% pull('action_type'))
          }
          else if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('turnover')){
            end_action = 'turnover'
          }
          end_player = pbp_w_shots[i,] %>% pull('player1_id')
          sequences = sequences %>% 
            add_row(sequence_id=nrow(sequences),
                    offense_team_id = as.double(curr_team),
                    init_action = curr_init_action,
                    end_action = end_action,
                    success=as.numeric(end_success),
                    init_player_id = as.character(curr_init_player_id),
                    end_player_id = as.character(end_player),
                    init_play_loc_x = as.numeric(curr_init_play_loc_x),
                    init_play_loc_y = as.numeric(curr_init_play_loc_y),
                    end_play_loc_x= as.numeric(end_loc_x),
                    end_play_loc_y = as.numeric(end_loc_y),
                    shot_distance = as.numeric(end_shot_distance),
                    shot_zone = as.character(end_shot_zone),
                    shot_result = as.numeric(end_shot_result),
                    shot_type = as.character(end_shot_type),
                    shot_was_tpa = as.numeric(end_shot_was_tpa))
          curr_init_action = '' 
        }
      }
      else{
        curr_init_action = 'tipoff'
        curr_init_play_loc_x = 0
        curr_init_play_loc_y = -80
        p = pbp_w_shots[i,] %>% pull(player1_id)
        curr_team = as.double(players[players$player_id == p,] %>% pull('team_id'))
        if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('shot')){
          end_action = 'shot'
          end_success = pbp_w_shots[i,] %>% pull('shot_made_flag')
          end_loc_x = pbp_w_shots[i,] %>% pull('shot_loc_x')
          end_loc_y = pbp_w_shots[i,] %>% pull('shot_loc_y')
          end_shot_distance = pbp_w_shots[i,] %>% pull('shot_distance')
          end_shot_zone = pbp_w_shots[i,] %>% pull('shot_zone_area')
          end_shot_result = 0
          end_shot_type = ''
          end_shot_was_tpa = -1
          if(tolower(pbp_w_shots[i,] %>% pull('description')) %>% str_detect('3pt')){
            end_shot_was_tpa = 1
            if(end_success == 1){
              end_shot_result = 3
            }
            else {
              end_shot_result = 0
            }
          }
          else{
            end_shot_was_tpa = 0
            if(end_success == 1){
              end_shot_result = 2
            }
            else {
              end_shot_result = 0
            }
          }
          
          end_shot_type = tolower(pbp_w_shots[i,] %>% pull('action_type'))
          end_player = pbp_w_shots[i,] %>% pull('player1_id')
          sequences = sequences %>% 
            add_row(sequence_id=nrow(sequences),
                    offense_team_id = as.double(curr_team),
                    init_action = curr_init_action,
                    end_action = end_action,
                    success=as.numeric(end_success),
                    init_player_id = as.character(curr_init_player_id),
                    end_player_id = as.character(end_player),
                    init_play_loc_x = as.numeric(curr_init_play_loc_x),
                    init_play_loc_y = as.numeric(curr_init_play_loc_y),
                    end_play_loc_x= as.numeric(end_loc_x),
                    end_play_loc_y = as.numeric(end_loc_y),
                    shot_distance = as.numeric(end_shot_distance),
                    shot_zone = as.character(end_shot_zone),
                    shot_result = as.numeric(end_shot_result),
                    shot_type = as.character(end_shot_type),
                    shot_was_tpa = as.numeric(end_shot_was_tpa))
          curr_init_action = ''
        }
        else if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('timeout')){
          end_action = 'timeout'
          end_success = 0
          end_loc_x = pbp_w_shots[i,] %>% pull('shot_loc_x')
          end_loc_y = pbp_w_shots[i,] %>% pull('shot_loc_y')
          end_shot_distance = 0
          end_shot_zone = ''
          end_shot_type = ''
          end_shot_result = 0
          end_shot_was_tpa = -1
          end_player = ''
          sequences = sequences %>% 
            add_row(sequence_id=nrow(sequences),
                    offense_team_id = as.double(curr_team),
                    init_action = curr_init_action,
                    end_action = end_action,
                    success=as.numeric(end_success),
                    init_player_id = as.character(curr_init_player_id),
                    end_player_id = as.character(end_player),
                    init_play_loc_x = as.numeric(curr_init_play_loc_x),
                    init_play_loc_y = as.numeric(curr_init_play_loc_y),
                    end_play_loc_x= as.numeric(end_loc_x),
                    end_play_loc_y = as.numeric(end_loc_y),
                    shot_distance = as.numeric(end_shot_distance),
                    shot_zone = as.character(end_shot_zone),
                    shot_result = as.numeric(end_shot_result),
                    shot_type = as.character(end_shot_type),
                    shot_was_tpa = as.numeric(end_shot_was_tpa))
          curr_init_action = ''
        }
        else if(tolower(pbp_w_shots[i, 'description']) %>% str_detect('turnover')){
          end_action = 'turnover'
          end_success = 0
          end_loc_x = pbp_w_shots[i,] %>% pull('poss_loc_x')
          end_loc_x = pbp_w_shots[i,] %>% pull('poss_loc_y')
          end_shot_distance = -1
          end_shot_zone = -1
          end_shot_type = ''
          end_shot_result = 0
          end_shot_was_tpa = -1
          end_player = pbp_w_shots[i,] %>% pull('player1_id')
          sequences = sequences %>% 
            add_row(sequence_id=nrow(sequences),
                    offense_team_id = curr_team,
                    init_action = curr_init_action,
                    end_action = end_action,
                    success=as.numeric(end_success),
                    init_player_id = as.character(curr_init_player_id),
                    end_player_id = as.character(end_player),
                    init_play_loc_x = as.numeric(curr_init_play_loc_x),
                    init_play_loc_y = as.numeric(curr_init_play_loc_y),
                    end_play_loc_x= as.numeric(end_loc_x),
                    end_play_loc_y = as.numeric(end_loc_y),
                    shot_distance = as.numeric(end_shot_distance),
                    shot_zone = as.character(end_shot_zone),
                    shot_result = as.numeric(end_shot_result),
                    shot_type = as.character(end_shot_type),
                    shot_was_tpa = as.numeric(end_shot_was_tpa))
          curr_init_action = ''
        }
      }}, error = function(e){
        curr_init_action = ''
        curr_team = ''
        curr_init_player_id = ''
        curr_init_play_loc_x = 0
        curr_init_play_loc_y = 0
      })
  }  
  
  return(sequences)
}

get_sequence_data_by_chunk = function(chunk){
  print('Worker started')
  sequences = get_pbp_game_sequence_data(game_i=chunk[1]) %>% mutate(game_id = chunk[1])
  for(i in 2:length(chunk)){
    tryCatch({
      s = get_pbp_game_sequence_data(game_i=chunk[i])
      sequences = sequences %>% rbind(s %>% mutate(game_id = chunk[i]))
      print(str_c("Done with game id ", as.character(chunk[i]), " ", as.character(i), " of ", as.character(length(chunk)), " (", as.character(i/length(chunk)), ")"))
      print(sequences %>% tail(5))
    }, error = function(e){})
  }
  return(sequences)
}

get_sequence_data_for_season = function(season){
  games = hoopR::nba_leaguegamelog(season=year_to_season(season))$LeagueGameLog 
  games = games %>% pull(GAME_ID) %>% unique()
  num_cores = detectCores()-1
  chunks = split(games, ceiling(seq_along(games)/num_cores))
  print('Beginning threading to process data')
  results = mclapply(chunks,get_sequence_data_by_chunk, mc.cores=num_cores)
  results = do.call(rbind, results)
  return(results)
}

main = function(){
  d = get_sequence_data_for_season(SEASON_NAME)
  d %>% write.csv('./SEQUENCES.csv')
}

main()

