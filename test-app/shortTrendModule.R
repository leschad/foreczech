USER = "root"
PASS = "heslo"
HOST = "localhost:27017"

URI = sprintf("mongodb://%s:%s@%s/", USER, PASS, HOST)

shortTrendUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Bruslaři"),
    selectInput(ns("skater"),
                "Zvol bruslaře:",
                c()),
    radioGroupButtons(ns("stat_skater"),
                "Zvol statistiku:",
                c("Čas na ledě",
                  "Góly",
                  "Asistence",
                  "Body",
                  "Střely",
                  "Průměrný čas na střídání"),
                selected = "Čas na ledě",
                status = "primary"),
    plotlyOutput(ns("short_skater")),
    titlePanel("Brankáři"),
    selectInput(ns("goalie"),
                "Zvol brankáře:",
                c()),
    radioGroupButtons(ns("stat_goalie"),
                      "Zvol statistiku:",
                      c("Čas na ledě",
                        "Úspěšnost zákroků",
                        "Střely proti",
                        "Obdržené góly"),
                      selected = "Čas na ledě",
                      status = "primary"),
    plotlyOutput(ns("short_goalie"))
  )
}

shortTrendModule <- function(input, output, session) {
  
  season <- get_current_seasonId()
  players_collection <- paste0("players_", season)
  
  con <- mongo(collection = players_collection, url = URI)
  df.all <- con$find()
  
  df.skaters <- df.all %>% 
    filter(position != "G")
  
  df.goalies <- df.all %>% 
    filter(position == "G")
  
  observe({
    updateSelectInput(session,
                      inputId = "skater", 
                      label = "Zvol hráče:",
                      choices = df.skaters$fullName,
                      selected = df.skaters$fullName[1])
  })
  
  skaterStatInput <- reactive({
    switch (input$stat_skater,
            "Čas na ledě" = "log_toiMin",
            "Góly" = "log_goalsSum",
            "Asistence" = "log_assistsSum",
            "Body" = "log_pointsSum",
            "Střely" = "log_shotsSum",
            "Průměrný čas na střídání" = "log_shiftsPer"
    )})
  
  observe({
    updateSelectInput(session,
                      inputId = "goalie", 
                      label = "Zvol brankáře:",
                      choices = df.goalies$fullName,
                      selected = df.goalies$fullName[1])
  })
  
  goalieStatInput <- reactive({
    switch (input$stat_goalie,
            "Čas na ledě" = "log_toiMin",
            "Úspěšnost zákroků" = "log_savePctg",
            "Střely proti" = "log_shotsAgainstSum",
            "Obdržené góly" = "log_goalsAgainstSum"
    )})
  
  goalieStatIsGoalsAgainst <- reactive({
    return(goalieStatInput() == "log_goalsAgainstSum")
  })
  
  goalieStatIsToiOrSavePctg <- reactive({
    return(goalieStatInput() == "log_toiMin"
           || goalieStatInput() == "log_savePctg")
  })
  
  output$short_skater <- renderPlotly({
    
    fig <- plot_ly()
    
    for (i in 1:nrow(df.skaters)) {
      skater <- df.skaters[i,]
      if (skater$fullName == input$skater) {
        color <- "#D55E00"
        opacity <- 1
      } else {
        color <- "grey"
        opacity <- 0.3
      }
      
      stat_val <- skater[[skaterStatInput()]]
      stat_val <- stat_val %>% 
        unlist()
      
      game_count <- skater$log_n %>% 
        unlist()
      
      fig <- fig %>%
        add_trace(x = game_count,
                  y = stat_val,
                  type = 'scatter',
                  mode = 'lines',
                  name = skater$fullName,
                  hoverinfo = 'none',
                  line = list(color = color),
                  opacity = opacity)
      
      if (skater$fullName == input$skater){
        hovercontent <- if(skaterStatInput() == "log_toiMin"){
          timeInMinSec(unlist(skater[["log_toi"]]))
        }else if(skaterStatInput() == "log_shiftsPer"){
            paste(stat_val, "s")
        }else if(skaterStatInput() == "log_shotsSum"){
          paste(unlist(skater[["log_shots"]]), "v zápase /", unlist(skater[["log_shotsSum"]]), "celkem")
        }else{
          stat_val
        }

        xval <- tail(game_count, 1)
        yval <- tail(stat_val, 1)

        fig <- fig %>% add_trace(
          x = game_count,
          y = stat_val,
          type = 'scatter',
          mode = 'markers',
          marker = list(color="#D55E00"),
          hoverinfo = 'text',
          hovertext = paste0("Zápas ", game_count, " - ", parseDate(unlist(skater[["log_gameDate"]])), "<br>",
                             unlist(skater[["log_teamAbbrev"]]), ifelse(unlist(skater[["log_homeRoad"]]) == "H", " vs ", " @ "), unlist(skater[["log_opponentAbbrev"]]),
                             "<br>", hovercontent)
        ) %>% add_annotations(
          x = xval,
          y = yval,
          xref = "x",
          yref = "y",
          text = skater$fullName,
          font = list(weight = "bold", size = 14),
          xanchor = 'left',
          showarrow = FALSE
        )
      }
    }
    
    fig <- fig %>% 
      layout(xaxis = list(title = "Zápasy", rangemode = "tozero"),
             yaxis = list(title = case_when(
               skaterStatInput() == "log_toiMin" ~ paste(input$stat_skater, "(v minutách)"),
               skaterStatInput() == "log_shiftsPer" ~ paste(input$stat_skater, "(ve vteřinách)"),
               TRUE ~ input$stat_skater),
               rangemode = "tozero"),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
    return(fig)
  })
  
  output$short_goalie <- renderPlotly({
    
    fig <- plot_ly()
    
    for (i in 1:nrow(df.goalies)) {
      goalie <- df.goalies[i,]
      if (goalie$fullName == input$goalie) {
        color <- "#D55E00"
        opacity <- 1
      } else {
        color <- "grey"
        opacity <- 0.3
      }
      
      stat_val <- goalie[[goalieStatInput()]]
      stat_val <- stat_val %>% 
        unlist()
      
      game_count <- goalie$log_n %>% 
        unlist()
      
      fig <- fig %>%
        add_trace(x = game_count,
                  y = stat_val,
                  type = 'scatter',
                  mode = 'lines',
                  name = goalie$fullName,
                  hoverinfo = 'none',
                  line = list(color = color),
                  opacity = opacity)
      
      if (goalie$fullName == input$goalie){
        hovercontent <- if(goalieStatInput() == "log_toiMin"){
          paste(unlist(goalie[["log_gamesStarted"]]), "|", timeInMinSec(unlist(goalie[["log_toi"]])))
        }else if(goalieStatInput() == "log_savePctg"){
          paste(unlist(goalie[["log_gamesStarted"]]), "|", stat_val, "% -", parseSaveRatio(unlist(goalie[["log_goalsAgainst"]]), unlist(goalie[["log_shotsAgainst"]])))
        }else if(goalieStatInput() == "log_shotsAgainstSum"){
          paste(unlist(goalie[["log_shotsAgainst"]]), "v zápase /", unlist(goalie[["log_shotsAgainstSum"]]), "celkem")
        }else if(goalieStatInput() == "log_goalsAgainstSum"){
          paste(unlist(goalie[["log_goalsAgainst"]]), "v zápase /", unlist(goalie[["log_goalsAgainstSum"]]), "celkem")
        }else{
          stat_val
        }
        
        xval <- tail(game_count, 1)
        yval <- tail(stat_val, 1)
        
        fig <- fig %>% add_trace(
          x = game_count,
          y = stat_val,
          type = 'scatter',
          mode = 'markers',
          marker = list(color=case_when(
            (goalieStatIsGoalsAgainst() & unlist(goalie[["log_shutouts"]]) == 1) ~ "#009E73",
            goalieStatIsToiOrSavePctg() & unlist(goalie[["log_gamesStarted"]]) == "Střídání" ~ "#0072B2",
            TRUE ~ "#D55E00")),
          hoverinfo = 'text',
          hovertext = paste0("Zápas ", game_count, " - ", parseDate(unlist(goalie[["log_gameDate"]])), "<br>",
                             unlist(goalie[["log_teamAbbrev"]]), ifelse(unlist(goalie[["log_homeRoad"]]) == "H", " vs ", " @ "), unlist(goalie[["log_opponentAbbrev"]]),
                             "<br>", hovercontent)
        ) %>% add_annotations(
          x = xval,
          y = yval,
          xref = "x",
          yref = "y",
          text = goalie$fullName,
          font = list(weight = "bold", size = 14),
          xanchor = 'left',
          showarrow = FALSE
        )
        
        if(goalieStatIsGoalsAgainst()){
          line_place <- which(unlist(goalie$log_shutouts) == 1)
        }else if(goalieStatIsToiOrSavePctg()){
          line_place <- which(unlist(goalie$log_gamesStarted) == "Střídání")
        }else{
          line_place <- c()
        }
        
        line_list <- list()
        for(j in line_place){
          line_list[[j]] <- list(type="line", fillcolor=ifelse(goalieStatIsGoalsAgainst(), "#009E73", "#0072B2"),
                                line = list(color=ifelse(goalieStatIsGoalsAgainst(), "#009E73", "#0072B2"), dash="dot"),
                                x0 = j,
                                x1 = j,
                                xref = "x",
                                y0 = 0,
                                y1 = 1,
                                yref = "paper")
        }
        
        fig <- fig %>% 
          layout(shapes = line_list)
      }
    }
    
    fig <- fig %>% 
      layout(xaxis = list(title = "Zápasy"),
             yaxis = list(title = case_when(
               goalieStatInput() == "log_toiMin" ~ paste(input$stat_goalie, "(v minutách)"),
               goalieStatInput() == "log_savePctg" ~ paste(input$stat_goalie, "(v %)"),
               TRUE ~ input$stat_goalie)), 
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
    
    return(fig)
  })
}