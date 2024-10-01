library(dplyr)
library(httr)
library(tidyr)
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
# today <- "2023-10-10"
season <- "20232024"

##############################################################################
# read schedule

schedule_base <- "https://api-web.nhle.com/v1/schedule/{current_date}"
schedule_url <- sub("\\{current_date\\}", today, schedule_base)

schedule_data <- read_url(schedule_url)
game_week <- schedule_data$gameWeek

game_week_index <- function(game_week, today) {
  indexes <- which(sapply(game_week, function(item) item$date == today))
  return(indexes)
}

day_index <- game_week_index(game_week, today)
games_today <- game_week[[day_index]]$games
id_games <- lapply(games_today, function(item) cbind(item$id, item$homeTeam$abbrev))

##############################################################################
# read pbp data

pbp_base <- "https://api-web.nhle.com/v1/gamecenter/{game_id}/play-by-play"
pbp_data <- do.call(rbind, lapply(id_games, function(id) {
  res <- collect_data(id[1], "\\{game_id\\}", pbp_base)
  res <- cbind(res$id, res$plays, id[2])
  res
}))

colnames(pbp_data) <- c("gameId", "pbp", "homeTeamAbbrev")
df.pbp <- as.data.frame(pbp_data)
df.pbp <- df.pbp %>%
  unnest_wider(pbp, names_repair = "universal") %>% 
  filter(details != "NULL" & typeDescKey != "stoppage" & typeDescKey != "delayed-penalty") %>%
  unnest_wider(periodDescriptor, names_repair = "universal") %>% 
  unnest_wider(details, names_repair = "universal")

df.pbp <- df.pbp %>% 
  pivot_longer(
    c(playerId, losingPlayerId, winningPlayerId, hittingPlayerId, hitteePlayerId,
      committedByPlayerId, drawnByPlayerId, shootingPlayerId, goalieInNetId,
      blockingPlayerId, scoringPlayerId, assist1PlayerId, assist2PlayerId),
    names_to = "playerType",
    values_to = "playerId",
    values_drop_na = TRUE) %>%
  mutate(timeStamp = convert_time(timeInPeriod, number)) %>% 
  select(playerId, playerType, timeStamp, gameId, typeDescKey, xCoord, yCoord,
         shotType, descKey, duration, homeTeamDefendingSide, homeTeamAbbrev) %>% 
  filter((typeDescKey != "missed-shot" | playerType != "goalieInNetId")
         & playerType != "assist1PlayerId" & playerType != "assist2PlayerId")

shootingPlayerType <- "shootingPlayerId"
scoringPlayerType <- "scoringPlayerId"
goaliePlayerType <- "goalieInNetId"

df.pbp <- df.pbp %>% 
  mutate(shotResult = case_when(
    (typeDescKey == "blocked-shot" & playerType == shootingPlayerType) ~ "blocked",
    (typeDescKey == "missed-shot" & playerType == shootingPlayerType) ~ "missed",
    (typeDescKey == "shot-on-goal" & 
       (playerType == shootingPlayerType | playerType == goaliePlayerType)) ~ "on target",
    (typeDescKey == "goal" & 
       (playerType == scoringPlayerType | playerType == goaliePlayerType)) ~ "goal",
    TRUE ~ "regular")) %>% 
  mutate(typeDescKey = case_when(
    (typeDescKey == "blocked-shot" & playerType == shootingPlayerType) ~ "shot",
    (typeDescKey == "missed-shot") ~ "shot",
    (typeDescKey == "shot-on-goal" & 
       (playerType == shootingPlayerType | playerType == goaliePlayerType)) ~ "shot",
    (typeDescKey == "goal") ~ "shot",
    (typeDescKey == "penalty" & playerType == "committedByPlayerId") ~ "committedPenalty",
    (typeDescKey == "penalty" & playerType == "drawnByPlayerId") ~ "drawnPenalty",
    TRUE ~ as.character(typeDescKey))) %>% 
  mutate(playerType = sub("Id", "", playerType)) %>% 
  mutate(playerType = ifelse((typeDescKey == "committedPenalty" | typeDescKey == "drawnPenalty"), "player", playerType))

colnames(df.pbp)[colnames(df.pbp) %in% c("typeDescKey", "descKey", "duration")] <- c("event", "penalty.desc", "penalty.duration")

cols.int <- c("gameId")
cols.char <- c("homeTeamAbbrev")
df.pbp[cols.int] <- sapply(df.pbp[cols.int], as.integer)
df.pbp[cols.char] <- sapply(df.pbp[cols.char], as.character)

##############################################################################
# only czech players

conPlayers <- mongo(collection = "players", url = URI)
df.czech_players <- conPlayers$find()

df.pbp_czechs <- df.pbp %>% 
  inner_join(df.czech_players, by = c("playerId" = "playerId"))

df.pbp_czechs <- df.pbp_czechs %>% 
  select(playerId, playerType, timeStamp, gameId, event, xCoord, yCoord,
         shotType, penalty.desc, penalty.duration,
         homeTeamDefendingSide, currentTeamAbbrev, homeTeamAbbrev,
         shotResult, fullName, birthCountry)

df.pbp_czechs <- df.pbp_czechs %>% 
  mutate(eventLabel = case_when(
    event == "faceoff" ~ "vhazování",
    event == "hit" ~ "hit",
    event == "shot" ~ "střela",
    event == "giveaway" ~ "ztráta puku",
    event == "blocked-shot" ~ "zblokovaná střela",
    event == "takeaway" ~ "zisk puku",
    event == "committedPenalty" ~ "faul",
    event == "drawnPenalty" ~ "faul proti",
    TRUE ~ event
  )) %>%
  mutate(eventShape = case_when(
    event == "faceoff" ~ "16",
    event == "hit" ~ "10",
    event == "shot" ~ "16",
    event == "committedPenalty" ~ "18",
    event == "drawnPenalty" ~ "19",
    event == "giveaway" ~ "12",
    event == "blocked-shot" ~ "13",
    event == "takeaway" ~ "9",
    TRUE ~ event
  )) %>% 
  mutate(shotResultLabel = case_when(
    shotResult == "regular" ~ "regular",
    shotResult == "on target" ~ "na bránu",
    shotResult == "goal" ~ "gól",
    shotResult == "blocked" ~ "zblokovaná",
    shotResult == "missed" ~ "mimo bránu",
    TRUE ~ shotResult
  )) %>% 
  mutate(shotColor = case_when(
    shotResult == "regular" ~ "#000000",
    shotResult == "on target" ~ "#009E73",
    shotResult == "goal" ~ "#0072B2",
    shotResult == "blocked" ~ "#CC79A7",
    shotResult == "missed" ~ "#D55E00",
    TRUE ~ "#000000"
  )) %>% 
  mutate(yCoord = ifelse((event != "shot"
                         & ((homeTeamAbbrev == currentTeamAbbrev & homeTeamDefendingSide == "right")
                            | (homeTeamAbbrev != currentTeamAbbrev & homeTeamDefendingSide == "left")))
                         |  (event == "shot" & xCoord < 0),
                         -yCoord,  yCoord)) %>% 
  mutate(xCoord = case_when(event != "shot"
                         & ((homeTeamAbbrev == currentTeamAbbrev & homeTeamDefendingSide == "right")
                            | (homeTeamAbbrev != currentTeamAbbrev & homeTeamDefendingSide == "left")) ~ -xCoord,
                         event == "shot" ~ abs(xCoord),
                         TRUE ~ xCoord))

conPbp <- mongo(collection = "pbp", url = URI)
conPbp$drop()
conPbp$insert(df.pbp_czechs)
