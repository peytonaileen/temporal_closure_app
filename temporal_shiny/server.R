
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

# Define server logic required to draw a histogram
function(input, output, session) {

  #----------------------------------
  # Read in data 
  #----------------------------------
  
  # Sample annual catch data
  sample_catch <- read_csv(here::here("data", "Sample_fish_moon_data.csv")) %>% 
    clean_names() %>% 
    mutate(date = mdy(date), 
           month = month(date, label = TRUE, abbr = FALSE), 
           lunar_phase4 = lunar.phase(date, name=T), 
           lunar_phase8 = lunar.phase(date, name=8))
  
  output$annual_take <- renderPlot({
    
    ggplot(sample_catch, aes(x= date, y = total_fish_caught))+
      geom_col()
    
  })
  
  # #Projected take historgram
  # output$percentDecrease <- renderPlot({
  #   req(sampleBagLimit())
  #   
  #   
  #   ridge_decreased<- data.frame(Historical = c(sampleBagLimit()$original_bag_vector),
  #                                Projected = c(sampleBagLimit()$new_bag_vector)) %>% 
  #     pivot_longer(cols = 1:2 ,
  #                  names_to = "scenario", 
  #                  values_to = "take")
  #   
  #   
  #   ridge_decreased %>% 
  #     ggplot(aes(x = take, fill = scenario))+
  #     geom_histogram( binwidth = 1, alpha = 0.5, color="#e9ecef")+
  #     theme_bw()+
  #     scale_fill_manual(values=c("#66CDAA", "orange"))+
  #     scale_x_continuous(limits=c(0,max(ridge_decreased$take)))+
  #     theme(legend.position = "none", 
  #           panel.spacing = unit(0.1, "lines"), 
  #           text = element_text(size = 15), 
  #           plot.caption = element_text(hjust = 0.5, face = "italic"))+
  #     labs( title = "Projected Distribution of Take Under Sample Bag Limit",
  #           y = "Frequency", 
  #           x = "Take per Trip (Number of Fish)"
  #           #caption = "The historical take is reflective of the distribution of take associated with the current selection. The projected take is \n reflective of the current selection under the sample bag limit. This projection assumes that all fishers previously \n taking more than the suggested bag limit, would now take the maximum amount of fish allowed."
  #     ) +
  #     facet_wrap(~scenario)
  #   
  #   
  # })

}
