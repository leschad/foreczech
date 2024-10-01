format_season <- function(season){
  year1 <- substr(season, 1, 4)
  year2 <- substr(season, 5, 8)
  
  return(paste0(year1, "/", year2))
}

timeInMinSec <- function(time) {
  min <- time %/% 60
  sec <- round(time %% 60, digits = 0)
  res <- paste(min, "min", sec, "s")
  return(res)
}

parseDate <- function(dateStamp) {
  per <- ymd(dateStamp)
  res <- paste0(day(per), ".", month(per), ".", year(per))
  return(res)
}

parseSaveRatio <- function(goals, shots) {
  res <- paste(shots - goals, "/", shots)
  return(res)
}

calculateMins <- function(timeInSec) {
  timeInMin <- timeInSec %/% 60
  return(timeInMin)
}

leftoverSeconds <- function(timeInSec) {
  leftoverSec <- timeInSec %% 60
  return(leftoverSec)
}

get_previous_date <- function(){
  current_date <- Sys.Date()
  previous_date <- current_date - 1
  res <- format(previous_date, "%Y-%m-%d")
  return(res)
}

read_url <- function(url_link) {
  response <- GET(url_link)
  json_data <- content(response, "parsed")
  json_data
}

get_current_seasonId <- function(){
  standings_season_url <- "https://api-web.nhle.com/v1/standings-season"
  df.standings_season <- read_url(standings_season_url)
  df.seasons <- do.call(rbind, lapply(df.standings_season$seasons, function(season) season[c("id", "standingsEnd")]))
  
  res <- df.seasons %>%
    as.data.frame() %>% 
    select(id) %>% 
    tail(1) %>% 
    unlist() %>% 
    unname()
  
  return(as.character(res))
}