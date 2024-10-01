USER = "root"
PASS = "heslo"
HOST = "localhost:27017"

URI = sprintf("mongodb://%s:%s@%s/", USER, PASS, HOST)

longTrendUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioGroupButtons(ns("stat"),
                "Zvol statistiku:",
                c("Počet hráčů",
                  "Čas na ledě",
                  "Góly",
                  "Body"),
                selected = "Počet hráčů",
                status = "primary"),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'Počet hráčů'", ns("stat")),
      materialSwitch(ns("switch"),
                     label = "Součtové hodnoty",
                     value = FALSE,
                     right = TRUE,
                     status = "primary")
    ),
    sliderTextInput(ns("range"),
                    "Vyber sezóny:",
                    choices = c("0","1"),
                    selected = c("0","1"),
                    force_edges = TRUE),
    plotlyOutput(ns("long"))
  )
}

longTrendModule <- function(input, output, session) {
  con <- mongo(collection = "longterm", url = URI)
  df.longterm <- con$find()
  
  statInput <- reactive({
    switch (input$stat,
            "Počet hráčů" = "playerCount",
            "Čas na ledě" = "toi",
            "Góly" = "goals",
            "Body" = "points"
    )})
  
  sliderRange <- reactive({
    cbind(as.integer(input$range[1]), as.integer(input$range[2]))
  })
  
  observe({
    updateSliderTextInput(session,
                          inputId = "range", 
                          label = "Vyber sezóny:",
                          choices = as.character(unlist(df.longterm$season)),
                          selected = c(as.character(head(df.longterm$season, 1)), as.character(tail(df.longterm$season, 1))))
  })
  
  output$long <- renderPlotly({
    fig <- plot_ly()
    
    statistic <- statInput()
    
    low_season <- sliderRange()[1]
    high_season <- sliderRange()[2]
    df.display <- df.longterm %>% 
      filter(season >= low_season & season <= high_season)
    
    if(input$switch == FALSE && statistic != "playerCount"){
      statistic <- paste0(statistic, "_avg")
    }else if(input$switch == TRUE && statistic != "playerCount"){
      statistic <- paste0(statistic, "_sum")
    }
    
    stat_val <- df.display[[statistic]]
    
    hovercontent <- if(statistic == "toi_avg"){
      timeInMinSec(df.display[["toi_avg_display"]])
    }else if(statistic == "goals_avg" || statistic == "points_avg"){
      round(stat_val, digits = 3)
    }else if(statistic == "toi_sum"){
      paste(df.display[["toi_sum_display"]], "min")
    }else{
      stat_val
    }
    
    ytitle <- if(statistic == "toi_avg"){
      "Průměrný čas na zápas (v minutách)"
    }else if(statistic == "toi_sum"){
      "Celkový čas (v tis. minut)"
    }else if((statInput() == "goals" || statInput() == "points") && input$switch == FALSE){
      paste("Průměrné", ifelse(statInput() == "goals", "góly", "body"), "na hráče")
    }else{
      input$stat
    }
    
    fig <- fig %>%
      add_trace(x = df.display$season,
                y = stat_val,
                type = 'scatter',
                mode = 'lines+markers',
                hoverinfo = 'text',
                hovertext = paste("Sezóna", format_season(df.display$seasonId), "|", hovercontent),
                line = list(color = "#0072B2"),
                marker = list(color = "#0072B2")) %>% 
      layout(xaxis = list(title = "Sezóna"),
             yaxis = list(title = ytitle, rangemode = "tozero"),
             showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    
    return(fig)
  })
}