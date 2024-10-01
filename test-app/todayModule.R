USER = "root"
PASS = "heslo"
HOST = "localhost:27017"

URI = sprintf("mongodb://%s:%s@%s/", USER, PASS, HOST)

todayModuleUI <- function(id){
  ns <- NS(id)
  
  tagList(
    chooseSliderSkin("Flat", color = "#0072B2"),
    tags$h2(textOutput(ns("title"))),
    sidebarLayout(
      sidebarPanel(
        dateInput(ns("date"),
                  label = "Zvol den:",
                  min = "2023-10-10",
                  max = get_previous_date(),
                  value = get_previous_date(),
                  weekstart = 1,
                  language = "cs",
                  format = "d. m. yyyy"),
        conditionalPanel(condition = sprintf("output['%s'] == false", ns("empty")),
                         radioGroupButtons(ns("player"),
                                           choices = c(""),
                                           status = "primary",
                                           direction = "vertical",
                                           width = "100%"))
      ),
      mainPanel(
        htmlOutput(ns("playerInfo")),
        tags$hr(),
        conditionalPanel(
          condition = sprintf("output['%s'] == false", ns("empty")),
          tags$h3("Přehled střel"),
          plotOutput(ns("shotRinkPlot"), width = "100%"),
          tags$h3("Akce na ledě"),
          checkboxGroupInput(
            ns("eventType"),
            "Zvol typ akce:",
            choices = "",
          ),
          actionButton(
            ns("clearButton"),
            "Odstranit zaškrtnuté",
          ),
          plotOutput(ns("eventRinkPlot"), width = "100%"),
          tags$h3("Čas na ledě spolu s ostatními hráči"),
          radioGroupButtons(ns("playerGroup"),
                            "Zvol skupinu hráčů:",
                            choices = c("Všichni", "Spoluhráči", "Soupeři"),
                            selected = "Všichni",
                            status = "primary"),
          sliderInput(ns("playerCount"),
                      "Zvol počet hráčů:",
                      min = 1,
                      max = 10,
                      value = 10,
                      step = 1,
                      ticks = FALSE),
          plotlyOutput(ns("icetimePlot"))
        )
      )
    )
  )
}

todayModule <- function(input, output, session){
  season <- get_current_seasonId()
  today <- reactive({input$date})
  
  players_collection <- paste0("players_", season)
  
  
  con <- mongo(collection = players_collection, url = URI)
  df.players <- con$find()
  
  pbp_data <- reactive({
    pbp_collection <- paste0("pbp_", today())
    conPbp <- mongo(collection = pbp_collection, url = URI)
    res <- conPbp$find()
    return(res)
  })

  toiw_data <- reactive({
    toiw_collection <- paste0("toiw_", today())
    conToiw <- mongo(collection = toiw_collection, url = URI)
    res <- conToiw$find()
    return(res)
  })
  
  output$title <- renderText({
    per <- parse_date_time(today(), "y-m-d")
    return(paste0("Statistiky ze dne ", day(per), ". ", month(per), ". ", year(per)))
  })
  
  observe( {
    if(nrow(pbp_data()) != 0){
      unique_skaters <- pbp_data() %>%
        select(fullName) %>% 
        unique() %>% 
        unname() %>% 
        unlist() %>% 
        sort()
    }else {
      unique_skaters <- c()
    }
    
    updateRadioGroupButtons(session,
                            inputId = "player",
                            choices = unique_skaters,
                            selected = unique_skaters[1],
                            status = "primary")
  })
  
  output$playerInfo <- renderUI({
    if(nrow(pbp_data()) == 0){
      res <- "<font size='6'>Dnes se žádný český hráč neúčastnil zápasu</font>"
    } else{
      player <- df.players %>% 
        filter(fullName == input$player)
      
      res <- paste0("<font size='6'>", input$player, "</font>", "<br>",
                  ifelse(is.na(player$position), "", paste0("<font size='4'>", unlist(player$position)[1], " | ",
                  unlist(player$currentTeamName)[1], "</font>")))
    }
    return(HTML(res))
  })
  
  output$empty <- reactive({
    return(nrow(pbp_data()) == 0)
  })
  outputOptions(output, 'empty', suspendWhenHidden=FALSE)
  
  output$shotRinkPlot <- renderPlot({
    
    if(nrow(pbp_data()) == 0){
      return()
    }
    
    player <- df.players %>% 
      filter(fullName == input$player)
    
    df.shotPlot <- pbp_data() %>%
      filter(event == "shot") %>% 
      filter(if (unlist(player$position) == "G") playerType == "goalieInNet"
             else playerType != "goalieInNet")
    
    color_values <- df.shotPlot$shotColor
    colors_names <- df.shotPlot$shotResultLabel
    names(color_values) <- colors_names
    
    color_values <- color_values %>% 
      unique()
    
    geom_hockey(league = "NHL", display_range = "ozone") +
      geom_point(
        data = df.shotPlot,
        mapping = aes(
          x = xCoord,
          y = yCoord,
          color = shotResultLabel
        ),
        alpha = ifelse(df.shotPlot$fullName == input$player, 1, 0.2),
        size = 5
      ) +
      scale_color_manual(values = color_values) +
      labs(color = "Typ střely") +
      theme(
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14)
      )
  })
  
  observeEvent(ignoreInit = TRUE,
    input$player, {
      events <- pbp_data() %>% 
        filter(fullName == input$player) %>% 
        filter(event != "shot" & event != "faceoff")
      
      event_labels <- events %>% 
        select(eventLabel) %>% 
        unique() %>% 
        unname() %>% 
        unlist()
      
      event_values <- events %>% 
        select(event) %>% 
        unique() %>% 
        unname() %>% 
        unlist()
      
      updateCheckboxGroupInput(
        session,
        inputId = "eventType",
        label = "Zvol typ akce:",
        choiceNames = event_labels,
        choiceValues = event_values
      )
    }
  )
  
  observeEvent(ignoreInit = TRUE,
    input$clearButton, {
      events <- pbp_data() %>% 
        filter(fullName == input$player) %>% 
        filter(event != "shot" & event != "faceoff")
      
      event_labels <- events %>% 
        select(eventLabel) %>% 
        unique() %>% 
        unname() %>% 
        unlist()
      
      event_values <- events %>% 
        select(event) %>% 
        unique() %>% 
        unname() %>% 
        unlist()
      
      updateCheckboxGroupInput(
        session,
        inputId = "eventType",
        label = "Zvol typ akce:",
        choiceNames = event_labels,
        choiceValues = event_values,
        selected = NULL
      )
    }
  )
  
  output$eventRinkPlot <- renderPlot({
    
    if(nrow(pbp_data()) == 0){
      return()
    }
    
    df.eventPlot <- pbp_data() %>%
      filter(event != "shot" & event != "faceoff") %>%
      filter(if(!is.null(input$eventType)) event %in% input$eventType else TRUE)
    
    shape_values <- df.eventPlot$eventShape %>% 
      unique()
    
    geom_hockey(league = "NHL", display_range = "in_bounds_only") +
      geom_point(
        data = df.eventPlot,
        mapping = aes(
          x = xCoord,
          y = yCoord,
          shape = eventLabel
        ),
        alpha = ifelse(df.eventPlot$fullName == input$player, 1, 0.2),
        size = 5
      ) +
      scale_shape_manual(values = as.numeric(shape_values), name = "Typ akce") +
      theme(
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.position = "top"
      )
  },
  width = 650
  )
  
  playerGroupInput <- reactive({
    switch(input$playerGroup,
           "Všichni" = "all",
           "Spoluhráči" = "mates",
           "Soupeři" = "opps")
  })
  
  observeEvent(ignoreInit = TRUE, list(playerGroupInput(), input$player), {
    max_value <- toiw_data() %>%
      filter(case_when(
        playerGroupInput() == "all" ~  playerName == input$player,
        playerGroupInput() == "mates" ~ playerName == input$player & playerTeam == otherPlayerTeam,
        playerGroupInput() == "opps" ~ playerName == input$player & playerTeam != otherPlayerTeam
      )) %>%
      select(otherPlayerName) %>%
      nrow()
    
    updateSliderInput(session,
                      inputId = "playerCount",
                      "Zvol počet hráčů:",
                      min = ifelse(max_value > 5, 5, 1),
                      max = max_value,
                      value = ifelse(max_value > 10, 10, max_value),
                      step = 1)
  })
  
  playerCountInput <- reactive({input$playerCount})
  
  output$icetimePlot <- renderPlotly({
    
    if(nrow(toiw_data()) == 0){
      return()
    }
    
    df.toiw_display <- toiw_data() %>%
      filter(case_when(
        playerGroupInput() == "all" ~  playerName == input$player,
        playerGroupInput() == "mates" ~ playerName == input$player & playerTeam == otherPlayerTeam,
        playerGroupInput() == "opps" ~ playerName == input$player & playerTeam != otherPlayerTeam
      )) %>% 
      arrange(desc(time)) %>% 
      head(playerCountInput())
    
    plot_ly(df.toiw_display, x = ~reorder(otherPlayerName, -time), y = ~timeInMin, type = 'bar',
            marker = list(
              color = ~hexValue,
              pattern = list(
                shape = ~ifelse(playerTeam == otherPlayerTeam, "", "\\"),
                size = 13,
                solidity = 0.8)),
            hoverinfo = 'text',
            hovertext = ~paste0(otherPlayerName, "<br>",
                               otherPlayerPosition, " | ", otherPlayerTeam,
                               "<br>Čas: ", calculateMins(time), " min ",
                               leftoverSeconds(time), " s")) %>%
      layout(
        xaxis = list(title = "", tickangle = 315, tickfont = list(size = 13),
                     showticklabels = ifelse(playerCountInput() <= 20, TRUE, FALSE)),
        yaxis = list(title = "Čas (v minutách)", titlefont = list(size = 14)),
        barmode = 'group') %>% 
      config(displayModeBar = FALSE)
  })
}