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

# Sample annual catch data
sample_catch <- read_csv(here::here("data", "Sample_fish_moon_data.csv")) %>% 
  clean_names() %>% 
  mutate(date = mdy(date), 
         month = month(date, label = TRUE, abbr = FALSE), 
         lunar_phase4 = lunar.phase(date, name=T), 
         lunar_phase8 = lunar.phase(date, name=8))

ggplot(sample_catch, aes(x= date, y = total_fish_caught))+
  geom_col(alpha = 0.9, fill="#69b3a2",width=0.5)+
  theme_minimal()+
  labs(x = "Date", 
       y = "Total Fish Caught", 
       title = "Fish Caught Throughout the Year")

monthly_grouped_catch <- sample_catch %>% 
  group_by(month) %>% 
  summarize(total_fish_caught= sum(total_fish_caught))

ggplot(monthly_grouped_catch, aes(x= month, y = total_fish_caught))+
  geom_col(alpha = 0.9, fill="#69b3a2",width=0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(x = "Month", 
       y = "Total Fish Caught", 
       title = "Fish Caught Throughout the Year")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

moon_quarters_grouped_catch <- sample_catch %>% 
  group_by(lunar_phase4) %>% 
  summarize(total_fish_caught= sum(total_fish_caught))

ggplot(moon_quarters_grouped_catch, aes(x= lunar_phase4, y = total_fish_caught))+
  geom_col(alpha = 0.9, fill="#69b3a2",width=0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(x = "Moon Quarters", 
       y = "Total Fish Caught", 
       title = "Fish Caught by Moon Quarter")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))


moon_phases8_grouped_catch <- sample_catch %>% 
  group_by(lunar_phase8) %>% 
  summarize(total_fish_caught= sum(total_fish_caught))

ggplot(moon_phases8_grouped_catch, aes(x= lunar_phase8, y = total_fish_caught))+
  geom_col(alpha = 0.9, fill="#69b3a2",width=0.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  labs(x = "Moon Quarters", 
       y = "Total Fish Caught", 
       title = "Fish Caught by Moon Quarter")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

