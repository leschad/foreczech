library(plyr)
library(dplyr)
library(httr)
library(mongolite)
library(tidyr)

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

parse_player_data <- function(data){
  df.result <- data %>% 
    mutate(firstName = ifelse(is.na(firstName.default), default, firstName.default)) %>% 
    mutate(lastName = case_when(
      (is.na(firstName.default) & is.na(lastName.default)) ~ default.1,
      (!is.na(firstName.default) & is.na(lastName.default))~ default,
      TRUE ~ lastName.default)) %>% 
    mutate(birthCity = case_when(
      !is.na(birthCity.default) ~ birthCity.default,
      (is.na(firstName.default) & !is.na(lastName.default)) ~ default.1,
      (is.na(firstName.default) & is.na(lastName.default)) ~ default.2,
      (!is.na(firstName.default) & !is.na(lastName.default)) ~ default,
      (!is.na(firstName.default) & is.na(lastName.default)) ~ default.1)) %>% 
    mutate(birthStateProvince = case_when(
      (birthCountry != "CAN" & birthCountry != "USA") ~ as.character(NA),
      !is.na(birthStateProvince.default) ~ birthStateProvince.default,
      !is.na(default.3) ~ default.3,
      (is.na(default.3) & !is.na(default.2)) ~ default.2,
      (is.na(default.2) & !is.na(default.1)) ~ default.1,
      (is.na(default.1) & !is.na(default)) ~ default)) %>%
    mutate(positionGroup = case_when(
      (positionCode == "C" | positionCode == "L" | positionCode == "R") ~ "F",
      positionCode == "D" ~ "D",
      positionCode == "G" ~ "G"
    )) %>% 
    unite(fullName, c("firstName", "lastName") , sep = " ", remove = TRUE) %>%
    unite(fullNameAbbrev, c("fullName", "currentTeamAbbrev"), sep = ", ", remove = FALSE) %>% 
    select(id, fullName, fullNameAbbrev, birthCity, birthCountry,
           shootsCatches, sweaterNumber, positionCode, positionGroup,
           birthDate, heightInCentimeters, weightInKilograms,
           currentTeamAbbrev, currentTeamName)
  
  colnames(df.result)[colnames(df.result) == "id"] <- "playerId"
  colnames(df.result)[colnames(df.result) == "positionCode"] <- "position"
  
  df.result
}

season <- "20232024"
start_20232024 <- "2023-10-10"
today <- "2024-02-08"

##############################################################################
# collect teams

standings_url <- "https://api-web.nhle.com/v1/standings/now"
standings_data = read_url(standings_url)
standings <- as.data.frame(do.call(rbind.fill, lapply(standings_data$standings, function(it) as.data.frame(it))))
teams <- standings %>% 
  mutate(teamAbbrev = ifelse(!is.na(default.2), default.2, default.1)) %>% 
  select(teamAbbrev, teamName.default)

##############################################################################
# collect players

rosters_url <- "https://api-web.nhle.com/v1/roster/{team}/{season}"
rosters_20232024_url <- sub("\\{season\\}", season, rosters_url)

df.players <- do.call(rbind.fill, lapply(teams$teamAbbrev, function(abbrev) {
  teamData <- collect_data(abbrev, "\\{team\\}", rosters_20232024_url)
  roster <- c(teamData$forwards, teamData$defensemen, teamData$goalies)
  roster <- do.call(rbind.fill, lapply(roster, function(it) as.data.frame(it)))
  roster$currentTeamAbbrev <- abbrev
  return(roster)
}))

df.players <- df.players %>% 
  left_join(teams, by = c("currentTeamAbbrev" = "teamAbbrev"))
colnames(df.players)[colnames(df.players) == "teamName.default"] <- "currentTeamName"

df.players <- parse_player_data(df.players)

df.czech_players <- df.players %>% 
  filter(birthCountry == "CZE")

##############################################################################
# collect short trend

calcSec <- function(time){
  split <- strsplit(time, ":")
  minToSec <- as.numeric(split[[1]][1]) * 60
  sec <- as.numeric(split[[1]][2])
  return(minToSec + sec)
}

# skaters
df.skaters <- df.czech_players %>% 
  filter(position != "G")

log_url <- "https://api-web.nhle.com/v1/player/{player_id}/game-log/{season}/2"
log_20232024_url <- sub("\\{season\\}", season, log_url)

df.skaters <- do.call(rbind, lapply(df.skaters$playerId, function(id){
  log <- do.call(rbind.fill, lapply(collect_data(id, "\\{player_id\\}", log_20232024_url)$gameLog, function(it) as.data.frame(it)))
  if(is.null(log))
    return(data.frame(
      playerId = id
    ))
  num_cols <- c("goals", "assists", "points", "shots", "shifts")
  log <- log %>%
    select(teamAbbrev, opponentAbbrev, homeRoadFlag, gameDate, toi, goals, assists, points, shots, shifts) %>%
    mutate(toi = as.character(toi))
  log[num_cols] <- lapply(log[num_cols], as.numeric)
  log$toi <- sapply(log$toi, function(it) calcSec(it))
  
  test <- df.skaters %>% 
    filter(playerId == id)
  test$log_teamAbbrev <- list(rev(log$teamAbbrev))
  test$log_opponentAbbrev <- list(rev(log$opponentAbbrev))
  test$log_homeRoad <- list(rev(log$homeRoadFlag))
  test$log_gameDate <- list(rev(log$gameDate))
  test$log_toi <- list(rev(log$toi))
  test$log_goals <- list(rev(log$goals))
  test$log_assists <- list(rev(log$assists))
  test$log_points <- list(rev(log$points))
  test$log_shots <- list(rev(log$shots))
  test$log_shifts <- list(rev(log$shifts))
  test$log_n <- list(1:length(test$log_goals[[1]]))
  
  tmp <- unlist(test$log_toi) / 60
  test$log_toiMin <- list(tmp)
  
  test$log_shiftsPer <- list(round(unlist(test$log_toi) / unlist(test$log_shifts), digits = 0))
  
  test$log_goalsSum <- lapply(test$log_goals, cumsum)
  test$log_assistsSum <- lapply(test$log_assists, cumsum)
  test$log_pointsSum <- lapply(test$log_points, cumsum)
  test$log_shotsSum <- lapply(test$log_shots, cumsum)
  return(test)
}))

df.skaters <- df.skaters %>% 
  filter(!is.na(fullName))

# goalies
df.goalies <- df.czech_players %>% 
  filter(position == "G")

df.goalies <- do.call(rbind.fill, lapply(df.goalies$playerId, function(id){
  log <- do.call(rbind.fill, lapply(collect_data(id, "\\{player_id\\}", log_20232024_url)$gameLog, function(it) as.data.frame(it)))
  if(is.null(log))
    return(data.frame(
      playerId = id
    ))
  num_cols <- c("gamesStarted", "shotsAgainst", "goalsAgainst", "shutouts")
  log <- log %>%
    select(teamAbbrev, opponentAbbrev, homeRoadFlag, gameDate, toi, gamesStarted,
           shotsAgainst, goalsAgainst, shutouts, savePctg, decision) %>%
    mutate(toi = as.character(toi))
  log[num_cols] <- lapply(log[num_cols], as.numeric)
  log$toi <- sapply(log$toi, function(it) calcSec(it))
  
  test <- df.goalies %>% 
    filter(playerId == id)
  test$log_teamAbbrev <- list(rev(log$teamAbbrev))
  test$log_opponentAbbrev <- list(rev(log$opponentAbbrev))
  test$log_homeRoad <- list(rev(log$homeRoadFlag))
  test$log_gameDate <- list(rev(log$gameDate))
  test$log_toi <- list(rev(log$toi))
  test$log_shotsAgainst <- list(rev(log$shotsAgainst))
  test$log_goalsAgainst <- list(rev(log$goalsAgainst))
  test$log_shutouts <- list(rev(log$shutouts))
  test$log_savePctg <- list(rev(round(ifelse(is.na(log$savePctg), 1, log$savePctg) * 100, digits = 0)))
  test$log_decision <- list(rev(log$decision))
  test$log_n <- list(1:length(test$log_goalsAgainst[[1]]))
  
  tmp <- unlist(test$log_toi) / 60
  test$log_toiMin <- list(tmp)
  
  gamesStartedLabels <- sapply(rev(log$gamesStarted), function(it)
    ifelse(it == 1, return("Start"), return("Střídání")))
  test$log_gamesStarted <- list(gamesStartedLabels)
  
  test$log_shotsAgainstSum <- lapply(test$log_shotsAgainst, cumsum)
  test$log_goalsAgainstSum <- lapply(test$log_goalsAgainst, cumsum)
  
  return(test)
}))

df.goalies <- df.goalies %>% 
  filter(!is.na(fullName))

df.czech_players <- rbind.fill(df.skaters, df.goalies)

conPlayers <- mongo(collection = "players", url = URI)
conPlayers$drop()
conPlayers$insert(df.czech_players)
