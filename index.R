################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

# ******************************************************************************
# Ensure "sewanee_weather.rds" & "utilities.rds" are in your working directory
# ******************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))

# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail

utilities2 <- utilities %>% 
  select(building, type, sq_ft, capacity, built, year, month, gallons, gal_per_day)

# View(utilities)
#View(split_creek)
#View(sewanee_temp)

################################################################################
################################################################################
################################################################################
################################################################################


# Define UI for application that explore sewanee water data

ui <- fluidPage(
  titlePanel('Sewanee Utilities Data'),
  helpText('This dashboard can be used to explore data on water use in University of the South campus buildings.'),
  br(),
  p("This dashboard is in connection to the United Nation's SDG #6 which is related to water and sanitation.
    Conserving water so that it is a safe and accessible to all is of the utmost importance to ensure sustainble development around the globe.
    Tracking our own water use data is one small step in the process of having the data needed to recognize trends of where we are in line and also straying from this goal as a community."),
  tabsetPanel(
    tabPanel(h5('Water Use'),
             
             #set up a number input widget to choose year
             column(4, numericInput(
               inputId = 'year',
               label = 'Select year',
               min = min(utilities$year),
               max = max(utilities$year),
               value = min(utilities$year))),
             
             #set up a canned options widget to choose building
             column(4, uiOutput('building'),
                    #selectInput(inputId = 'building',
                    #            label = 'Select Buildings',
                    #           multiple = TRUE,
                    #          choices = unique(utilities$building),
                    #         selected = 'Ayres Hall'),
                    
                    #set up a radio buttons widget to choose list sorting
                    radioButtons(inputId = 'rank',
                                 label = 'List buildings...',
                                 choices = c('By newest', 'By oldest', 'By capacity', 'Alphabetically'),
                                 selected = 'By newest',
                                 inline = TRUE)),
             
             #set up a radio buttons widget to choose y variable
             column(4, radioButtons(inputId = 'yvar',
                                    label = 'Select Variable',
                                    choices = c('gallons', 'gallons per day' = 'gal_per_day'),
                                    selected = 'gallons',
                                    inline = TRUE)),
             br(),
             br(),
             
             #here's the figure
             fluidRow(column(1),
                      column(10, plotOutput("waterplot")),
                      column(1))
    ),
    #here's the data table
    tabPanel(h5('Data viewer'),
             fluidRow(column(12, DTOutput('dt1'),)))
    
    
  )
)

###########################################################################
###########################################################################
###########################################################################


# Define server logic required to show widgets, figure, and table
server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$utilities <- utilities
  
  observe({
    if(! is.null(input$building)){
      rv$utilities <- utilities %>% filter(building %in% input$building,
                                           year == input$year)
    }
  })
  
  output$building <- renderUI({
    
    (building <- utilities %>% pull(building) %>% unique %>% sort)
    
    
    if(input$rank == 'By capacity'){
      # Rank building by capacity 
      building <- utilities %>%
        arrange(desc(capacity)) %>%
        pull(building) %>% unique
      print(building)
      
    }
    
    if(input$rank == 'By newest'){
      # Rank them by construction date
      building <- utilities %>%
        arrange(desc(built)) %>%
        pull(building) %>% unique
      print(building)
      
    }
    
    if(input$rank == 'By oldest'){
      # Rank them by construction date
      building <- utilities %>%
        arrange((built)) %>%
        pull(building) %>% unique
      print(building)
      
    }
    
    #Logic for canned building options widget
    selectInput(inputId = 'building',
                label = 'Select Buildings',
                multiple = TRUE,
                choices = building,
                selected = 'Ayres Hall')
    
    
  })
  
  #Logic for plot output
  output$waterplot <- renderPlot({
    print(input$building)
    
    
    ggplot(rv$utilities,
           
           #utilities %>% filter(building %in% input$building,
           # year == input$year),
           
           #Using aes_string allows for the dynamic change of input$yvar for the graph y-axis
           aes_string(
             x = 'month',
             y = input$yvar,
             color = 'building')) +
      geom_path(linewidth = 2) +
      theme_bw() +
      labs(title = "Water use is driven by building capacity and moderness",
           caption = "This figure is designed to show comparisons in water use data amongst university buildings in a given year. Notice how during the summer, (months 6-8), there is a considerable drop off in water use in many buildings \n given that they are often at lower capacities as students leave for this period.",
           x = "Month",
           y = "Water Use (gallons)") +
      theme(plot.caption = element_text(size = 11))
  })
  
  #Logic for data table
  output$dt1 <- renderDT({ rv$utilities })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

