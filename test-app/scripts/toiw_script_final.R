library(plyr)
library(dplyr)
library(httr)
library(lubridate)
library(mongolite)

USER = "root"
PASS = "heslo"
HOST = "localhost:27017"
URI = sprintf("mongodb://%s:%s@%s/", USER, PASS, HOST)

read_url <- function(url_link) {
  response <- GET(url_link)
  json_data <- content(response, "parsed")
  json_data
}

collect_data <- function(id, sub_to_replace, base){
  item_url <- sub(sub_to_replace, id, base)
  item_data <- read_url(item_url)
  item_data
}

convert_time <- function(time, hockey_period) {
  time <- paste(today, time)
  per <- parse_date_time(time, "y-m-d M:S")
  minute(per) <- minute(per) + case_when(hockey_period == 1 ~ 0,
                                         hockey_period == 2 ~ 20,
                                         hockey_period == 3 ~ 40)
  if(minute(per)[1] >= 60){
    minute(per) <- minute(per) %% 60
    hour(per) <- hour(per) + 1
  }
  res <- sprintf("%02d-%02d-%02d %02d:%02d:%02d", year(per), month(per), day(per), hour(per), minute(per), second(per))
  res
}

today <- "2024-02-08"
season <- "20232024"
schedule_base <- "https://api-web.nhle.com/v1/schedule/{current_date}"
schedule_url <- sub("\\{current_date\\}", today, schedule_base)

schedule_data = read_url(schedule_url)
game_week <- schedule_data$gameWeek

game_week_index <- function(game_week, today) {
  indexes <- which(sapply(game_week, function(item) item$date == today))
  return(indexes)
}

day_index <- game_week_index(game_week, today)
games_today <- game_week[[day_index]]$games
id_games <- lapply(games_today, function(item) item$id)

##############################################################################
# save shift data

shift_base <- "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={game_id}"
shift_data <- do.call("c", lapply(id_games, function(id){
  single_game <- collect_data(id, "\\{game_id\\}", shift_base)
  single_game$data
}))

df.shifts <- as.data.frame(do.call(rbind, shift_data))

df.shifts <- df.shifts %>% 
  mutate(fullName = paste(firstName, lastName)) %>% 
  mutate(startTime = ifelse(is.na(startTime), startTime, convert_time(startTime, period))) %>% 
  mutate(endTime = ifelse(is.na(endTime), endTime, convert_time(endTime, period)))

# conShiftTime <- mongo(collection = "shiftTime", url = URI)
# conShiftTime$drop()
# conShiftTime$insert(df.shifts)

# df.shifts <- conShiftTime$find()

##############################################################################
# save toiw data

calc_toiw <- function(model_player, df.model_player_shifts, df.names, df.all_shifts){
  df.toiw <- data.frame(playerId = numeric(),
                        playerName = character(),
                        playerTeam = character(),
                        otherPlayerId = numeric(),
                        otherPlayerName = character(),
                        otherPlayerTeam = character(),
                        hexValue = character(),
                        time = numeric())
  
  for(k in 1:length(df.names$playerId)){
    player_id <- df.names$playerId[k]
    player_name <- df.names$fullName[k]
    other_player_team <- df.names$teamAbbrev[k]
    hex <- df.names$hexValue[k]
    
    df.shifts_player <- df.all_shifts %>% 
      filter(as.numeric(playerId) == as.numeric(player_id) & !is.na(duration))
    
    times <- list()
    for(i in 1:nrow(df.shifts_player)){
      shift <- df.shifts_player[i,]
      tmp <- 0
      for(j in 1:nrow(df.model_player_shifts)){
        row <- df.model_player_shifts[j,]
        if(shift$startTime <= row$startTime && shift$endTime >= row$endTime)
          tmp <- tmp + as.numeric(difftime(row$endTime, row$startTime, units = "secs"))
        else if(row$startTime <= shift$startTime && row$endTime >= shift$endTime)
          tmp <- tmp + as.numeric(difftime(shift$endTime, shift$startTime, units = "secs"))
        else if(shift$startTime <= row$startTime && shift$endTime >= row$startTime && shift$endTime <= row$endTime)
          tmp <- tmp + as.numeric(difftime(shift$endTime, row$startTime, units = "secs"))
        else if(row$startTime <= shift$startTime && row$endTime >= shift$startTime && row$endTime <= shift$endTime)
          tmp <- tmp + as.numeric(difftime(row$endTime, shift$startTime, units = "secs"))
      }
      times <- append(times, tmp)
    }
    
    df.toiw[nrow(df.toiw)+1,] <- c(model_player$playerId, model_player$fullName,
                                   model_player$currentTeamAbbrev,
                                   player_id, player_name, other_player_team,
                                   hex, sum(unlist(times)))
  }
  return(df.toiw)
}

czechs_toiw <- function(df.players, df.all_shifts){
  df.nationality_toiw <- data.frame(playerId = numeric(),
                                    playerName = character(),
                                    otherPlayerId = numeric(),
                                    otherPlayerName = character(),
                                    time = numeric())
  for(i in 1:nrow(df.players)){
    elem <- df.players[i,]
    player_id <- elem$playerId
    player_name <- elem$fullName
    player_team <- elem$currentTeamAbbrev
    game_id <- elem$gameId
    
    df.shifts_tmp <- df.all_shifts %>% 
      filter(playerId == player_id) %>% 
      filter(duration != "NULL")
    
    df.others_tmp <- df.all_shifts %>%
      filter(gameId == game_id & duration != "NULL") %>%
      filter(playerId != player_id) %>% 
      select(playerId, fullName, teamAbbrev, hexValue) %>% 
      unique()
    
    df.nationality_toiw <- rbind(df.nationality_toiw,
                                 calc_toiw(elem, df.shifts_tmp, df.others_tmp, df.all_shifts))
  }
  return(df.nationality_toiw)
}

conPbp <- mongo(collection = "pbp", url = URI)
df.pbp <- conPbp$find()

df.czechs_in_action <- df.pbp %>% 
  # filter(position != "G") %>% 
  select(playerId, fullName, currentTeamAbbrev, gameId) %>% 
  unique()

df.toiw <- czechs_toiw(df.czechs_in_action, df.shifts)

other_players <- df.toiw %>% 
  select(otherPlayerId) %>% 
  unique()

player_info_base <- "https://api-web.nhle.com/v1/player/{player_id}/landing"
other_players_data <- lapply(other_players[[1]], function(id) collect_data(id, "\\{player_id\\}", player_info_base))

df.other_positions <- do.call(rbind.fill, lapply(other_players_data, function(it) as.data.frame(it)))
df.other_positions <- df.other_positions %>% 
  select(playerId, position) %>% 
  mutate(positionGroup = case_when(
    (position == "C" | position == "L" | position == "R") ~ "F",
    position == "D" ~ "D",
    position == "G" ~ "G"))

df.toiw <- df.toiw %>%
  left_join(df.other_positions, by = c("otherPlayerId" = "playerId"))

colnames(df.toiw)[colnames(df.toiw) %in% c("position", "positionGroup")] <- c("otherPlayerPosition", "otherPlayerPositionGroup")

df.toiw <- df.toiw %>% 
  filter(otherPlayerPosition != "G") %>% 
  mutate(timeInMin = time/60)

conToiw <- mongo(collection = "toiw", url = URI)
conToiw$drop()
conToiw$insert(df.toiw)
