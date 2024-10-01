library(shiny)
library(shinyWidgets)
library(dplyr)
library(httr)
library(ggplot2)
library(sportyR)
library(lubridate)
library(plotly)
library(mongolite)
source("functions.R")
source("todayModule.R")
source("shortTrendModule.R")
source("longTrendModule.R")

ui <- navbarPage("Fore-Czech",
  
  tabPanel("Dnešní dění",
           todayModuleUI("today")),
  
  tabPanel("Krátkodobé trendy",
           shortTrendUI("short")),
  
  tabPanel("Dlouhodobé trendy",
           longTrendUI("long"))

)

server <- function(input, output) {
  
  callModule(todayModule, "today")
  
  callModule(shortTrendModule, "short")
  
  callModule(longTrendModule, "long")
}

shinyApp(ui = ui, server = server)
