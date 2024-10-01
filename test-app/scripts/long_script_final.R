library(plyr)
library(dplyr)
library(httr)
library(tidyr)
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

calcSec <- function(time){
  split <- strsplit(time, ":")
  minToSec <- as.numeric(split[[1]][1]) * 60
  sec <- as.numeric(split[[1]][2])
  return(minToSec + sec)
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
    select(id, fullName, fullNameAbbrev, birthCity, birthStateProvince, birthCountry,
           shootsCatches, sweaterNumber, positionCode, positionGroup,
           birthDate, heightInCentimeters, weightInKilograms, currentTeamAbbrev)
  
  colnames(df.result)[colnames(df.result) == "id"] <- "playerId"
  colnames(df.result)[colnames(df.result) == "positionCode"] <- "position"
  
  df.result
}

standings_season_url <- "https://api-web.nhle.com/v1/standings-season"

df.standings_season <- read_url(standings_season_url)

df.seasons <- do.call(rbind, lapply(df.standings_season$seasons, function(season) season[c("id", "standingsEnd")]))

df.selected_seasons <- df.seasons %>% 
  tail(1) %>% 
  as.data.frame()

con <- mongo(collection = "longterm", url = URI)

standings_url <- "https://api-web.nhle.com/v1/standings/{date}"

roster_season_base <- "https://api-web.nhle.com/v1/roster/{team}/{season}"

df.players_season <- do.call(rbind, lapply(df.selected_seasons$id, function(seasonId){
  q <- sprintf('{"seasonId": "%s"}', as.character(seasonId))
  df.existing <- con$find(query = q)
  if(nrow(df.existing) > 0){
    con$remove(q)
  }
  
  seasonEnd <- df.selected_seasons %>% 
    filter(id == seasonId) %>% 
    select(standingsEnd) %>%
    unname()
  seasonEnd <- as.vector(seasonEnd[[1]])[[1]]
  
  list_standings <- collect_data(seasonEnd, "\\{date\\}", standings_url)
  
  teams <- sapply(list_standings$standings, function(team) team$teamAbbrev$default)
  
  roster_season_url <- sub("\\{season\\}", seasonId, roster_season_base)
  
  df.players <- do.call(rbind.fill, lapply(teams, function(team){
    team_data <- collect_data(team, "\\{team\\}", roster_season_url)
    df.roster <- c(team_data$forwards, team_data$defensemen, team_data$goalies)
    df.roster <- do.call(rbind.fill, lapply(df.roster, function(it) as.data.frame(it)))
    df.roster$currentTeamAbbrev <- team
    
    return(df.roster)
  }))
  
  df.players <- parse_player_data(df.players)
  
  skater_id <- df.players %>% 
    filter(position != "G") %>% 
    filter(birthCountry == "CZE") %>% 
    select(playerId)
  
  goalie_id <- df.players %>% 
    filter(position == "G") %>% 
    filter(birthCountry == "CZE") %>% 
    select(playerId)
  
  result <- data.frame(seasonId = seasonId,
                       endDate = seasonEnd)
  result$skaters <- list(skater_id)
  result$goalies <- list(goalie_id)
  result$playerCount <- length(skater_id[[1]]) + length(goalie_id[[1]])
  
  return(result)
}))

rownames(df.players_season) <- NULL

df.players_season$seasonId <- as.character(df.players_season$seasonId)

df.players_season <- df.players_season %>% 
  mutate(season = as.numeric(substr(seasonId, 1, 4)))

##############################################################################

log_url <- "https://api-web.nhle.com/v1/player/{player_id}/game-log/{season}/2"

df.result <- do.call(rbind, lapply(df.players_season$seasonId, function(id){
  season_data <- df.players_season %>% 
    filter(seasonId == id)
  skaters <- season_data$skaters[[1]]
  season <- season_data$seasonId[[1]]
  
  log_season_url <- sub("\\{season\\}", season, log_url)
  
  tmp_skaters <- do.call(rbind, lapply(skaters$playerId, function(id){
    log <- do.call(rbind.fill, lapply(collect_data(id, "\\{player_id\\}", log_season_url)$gameLog, function(it) as.data.frame(it)))
    if(!is.null(log)){
      num_cols <- c("goals", "points")
      log <- log %>%
        select(toi, goals, points) %>%
        mutate(toi = as.character(toi))
      log[num_cols] <- lapply(log[num_cols], as.numeric)
      log$toi <- sapply(log$toi, function(it) calcSec(it))
    
      result <- data.frame(toi_sum = sum(log$toi),
                         toi_avg = mean(log$toi),
                         goals = sum(log$goals),
                         points = sum(log$points))
    
      return(result)
    }
  }))
  
  season_data$toi_sum_display <- round(sum(tmp_skaters$toi_sum)/60, digits = 0)
  season_data$toi_sum <- season_data$toi_sum_display/1000
  season_data$toi_avg_display <- mean(tmp_skaters$toi_avg)
  season_data$toi_avg <- season_data$toi_avg_display/60
  season_data$goals_sum <- sum(tmp_skaters$goals)
  season_data$goals_avg <- mean(tmp_skaters$goals)
  season_data$points_sum <- sum(tmp_skaters$points)
  season_data$points_avg <- mean(tmp_skaters$points)
  
  return(season_data)
}))

con$insert(df.result)

###############################################################

# id <- "20232024"
# 
# season_data <- df.players_season %>% 
#   filter(seasonId == id)
# skaters <- season_data$skaters[[1]]
# season <- season_data$seasonId[[1]]
# 
# log_season_url <- sub("\\{season\\}", season, log_url)
# 
# id <- skaters$playerId[2]
# 
# tmp_skaters <- do.call(rbind, lapply(skaters$playerId, function(id){
#   log <- do.call(rbind.fill, lapply(collect_data(id, "\\{player_id\\}", log_season_url)$gameLog, function(it) as.data.frame(it)))
#   if(!is.null(log)){
#     num_cols <- c("goals", "points")
#     log <- log %>%
#       select(toi, goals, points) %>%
#       mutate(toi = as.character(toi))
#     log[num_cols] <- lapply(log[num_cols], as.numeric)
#     log$toi <- sapply(log$toi, function(it) calcSec(it))
#     
#     result <- data.frame(toi_sum = sum(log$toi),
#                          toi_avg = mean(log$toi),
#                          goals = sum(log$goals),
#                          points = sum(log$points))
#     
#     return(result)
#   }
# }))
# 
# season_data$toi_sum_display <- round(sum(tmp_skaters$toi_sum)/60, digits = 0)
# season_data$toi_sum <- season_data$toi_sum_display/1000
# season_data$toi_avg_display <- mean(tmp_skaters$toi_avg)
# season_data$toi_avg <- season_data$toi_avg_display/60
# season_data$goals_sum <- sum(tmp_skaters$goals)
# season_data$goals_avg <- mean(tmp_skaters$goals)
# season_data$points_sum <- sum(tmp_skaters$points)
# season_data$points_avg <- mean(tmp_skaters$points)
# 
# return(season_data)