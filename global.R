#
# Created by: https://github.com/chocolatmint
#

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(lubridate)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)

options(scipen = 9999)
options(shiny.maxRequestSize=200*1024^2)

load(here::here("world_map2_project.rda"))

spotify <- read.csv("data_input/universal_top_spotify_songs.csv")

spotify <- spotify %>%
  select(name,artists,daily_rank,country,snapshot_date,popularity) %>%
  filter(!is.na(country) & country != "",
         !is.na(name) & name != "",
         !is.na(artists) & artists != "") %>% 
  mutate(
    snapshot_date = ymd(snapshot_date))

country_diff <- setdiff(world_map2$code_2, spotify$country)

global <- world_map2 %>% 
  filter(!code_2 %in% country_diff) %>% 
  select(-long, -lat, -group, -order, -code_num, -form_name) %>% 
  distinct(code_2, code_3, .keep_all = TRUE)

world_map <- world_map2 %>% 
  select(-long, -lat, -group, -order, -code_num, -form_name) %>% 
  distinct(code_2, code_3, .keep_all = TRUE)
