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
            box(
              sliderTextInput(
                inputId = "Id096",
                label = "Choose a range:", 
                choices = month.abb,
                selected = month.abb[c(4, 8)]
              ),
              plotOutput(outputId = "annual_take"),   
            )
      
    ),
    
    tabItem(tabName = "blank",
            h2("Blank tab")
    )
  )
)




# Put page together 
dashboardPage(
  
  dashboardHeader(title = "Temporal Closures - FishKit"),
  sidebar, #object called sidebar
  body #object body
)

