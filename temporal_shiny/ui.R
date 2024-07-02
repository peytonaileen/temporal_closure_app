library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(fishmethods)
library(gdata)
library(lunar)
library(lubridate)
library(here)
library(shinyWidgets)

#Create sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Temporal Closure Tool", tabName = "temp_close"),
    menuItem("Blank", tabName = "blank")
  )
)

#Create body 
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "temp_close",
            h2("Temporal closures tool"), 
            
            box(width = 12, 
                plotOutput(outputId = "annual_take"),
                pickerInput(
                  inputId = "closure_type", 
                  label = "Seasonal Closure Type", 
                  choices = c("Monthly closure",
                              "Moon Quarter Closure",
                              "Moon Phase Closure")
                )
            ),
              
 
            tabBox(width =12, 
              side = "left", height = "250px",
              selected = "Monthly Closure",
              tabPanel("Monthly Closure", 
                       sliderTextInput(
                         inputId = "Id096",
                         label = "Choose a range:", 
                         choices = month.abb,
                         selected = month.abb[c(4, 8)]
                       )
                       ),
              tabPanel("Moon Quarter Closure", "Tab content 2"),
              tabPanel("Moon Phase Closure", "Note that when side=right, the tab order is reversed.")
        
            )
            
      
    ),
    
    tabItem(tabName = "blank",
            h2("Blank tab")
    )
  )
)




# Put page together 
dashboardPage(
  
  dashboardHeader(title = "Temporal Closures Tool"),
  sidebar, #object called sidebar
  body #object body
)

