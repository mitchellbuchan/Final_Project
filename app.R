library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(shiny)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Interactive Map of Presidential Elections from 1976-2016"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "repper",
                  label = "Adjust Republican Vote Totals by Percentage (100 displays real to life):",
                  min = 0,
                  max = 200,
                  value = 100),
    sliderInput(inputId = "demper",
                label = "Adjust Democrat Vote Totals by Percentage (100 displays real to life):",
                min = 0,
                max = 200,
                value = 100),
    selectInput(inputId = "year",
                label = "Choose which Election Year to Display:",
                choices = seq(1976,2016,4),
                selected = 2016)
                
    
   )
   ,
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("selected_year"),
      textOutput("electoral_count"),
      # Output: Histogram ----
      leafletOutput("electionplot")
    )
)
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$electionplot <- renderPlot({
    electoralvals = c(9,3,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,
                      10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
    eyear = input$year
    setwd("C:/Users/Mitchell/Downloads/finproject")
    load("1976-2016-president.RData")
    statemap = readOGR("us-states.json")
    elections = x
    elections = mutate(elections, percentage = candidatevotes/totalvotes)
    elections_democrats = filter(elections, party == 'democrat')
    elections_republican = filter(elections, party == 'republican')
    elections = merge(elections_democrats,elections_republican, by = c('state', 'year'))
    elections = filter(elections, year == eyear)
    elections = distinct(elections, state, .keep_all = TRUE)
    elections = mutate(elections, percentage.x = percentage.x * (input$demper / 100))
    elections = mutate(elections, percentage.y = percentage.y * (input$repper / 100))
    elections = cbind(elections, electoralvals)
    elections = mutate(elections, win.R = case_when(
      percentage.y > percentage.x ~ 1,
      percentage.y < percentage.x ~ 0
    ))
    elections = mutate(elections, win.D = case_when(
      percentage.y > percentage.x ~ 0,
      percentage.y < percentage.x ~ 1
    ))
    elections$win.D = as.factor(elections$win.D)
    palette = c('blue', 'red')
    
    statemap = merge(statemap,elections, by.x = 'name', by.y = 'state', all.x = TRUE, all.y = TRUE)
    #Creating and formating label 
    factpal <- colorFactor(palette = palette, statemap$win.D)
    state_label = paste0("<strong>Democratic Candidate (Percentage):</strong>", 
                         statemap$candidate.x, "(", statemap$percentage.x, '%', ")",
                         "<br><strong>Republican Candidate (Percentage):</strong>", 
                         statemap$candidate.y, "(", statemap$percentage.y, '%', ")")
    elecmap = leaflet(data=statemap) %>% 
      addTiles() %>% 
      addPolygons(label = state_label,highlightOptions = highlightOptions(color = "white",weight = 2,bringToFront = TRUE),
                  color = ~factpal(win.D))
  })
  elecmap
  output$selected_year = renderText({paste("You have selected the", input$year, 'map')})
  output$electoral_count = renderText({paste('Republican Electoral Count:', sum(elections$win.R * elections$electoralvals),
                                            "<br> Democrat Electoral Count:", sum(elections$win.R * elections$electoralvals) )})

}
shinyApp(ui = ui, server = server)
