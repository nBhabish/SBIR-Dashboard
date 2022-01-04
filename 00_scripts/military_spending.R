
# Loading Libraries ----

library(tidyverse)
library(tidyquant)
library(fs)
library(readxl)
library(janitor)
library(shinyWidgets)
library(DataExplorer)
library(lubridate)
library(ggplotly)
library(flexdashboard)
library(shiny)
library(timetk)

# Reading in the data ----

military_tbl <- readxl::read_xlsx("00_data/bbr_dashboard.xlsx")

# Cleaning the column names -----

military_tbl %>% 
  janitor::clean_names() %>% 
  colnames()

# Formatting the Dataframe ----

military_tbl_formatted <- military_tbl %>% 
  janitor::clean_names() %>% 
  rename(no_of_phase_one_awards     = "number_of_phase_i_awards",
         total_phase_one_obligation = "total_phase_i_obligation",
         no_of_phase_two_awards     = "number_of_phase_ii_awards",
         total_phase_two_obligation = "total_phase_ii_obligation",
         date                       = "year") %>% 
  select(date, state, organization, everything()) %>% 
  mutate(organization = as.factor(organization)) 

# Changing the dbl date to date data type ----

military_tbl_formatted <- military_tbl_formatted %>% 
  mutate(date = as.Date(paste0(date, origin = "-01-01"))) %>% 
  mutate(date = as.factor(year(date)))


# Identifying extra states on military_formatted_tbl

military_tbl_formatted <- military_tbl_formatted %>% 
  filter(state != "AS") 


write_csv(military_tbl_formatted, "00_data/military_spending_formatted.csv")

military_tbl_formatted %>% 
  group_by(date, organization) %>% 
  filter(date == 2018) %>% 
  summarise(total_obligation = sum(total_obligation)) %>% 
  ggplot(aes(organization, total_obligation))+
  geom_col(position = position_dodge(preserve = "single"))+
  theme_tq()+
  coord_flip()

ggplotly(g)


# Phase One Obligation Plot

g <- military_tbl_formatted %>% 
  group_by(date, organization) %>% 
  summarise(total_phase_one_obligation = sum(total_phase_one_obligation)) %>% 
  ungroup() %>% 
  ggplot(aes(organization, total_phase_one_obligation, fill = as.factor(date)))+
  geom_col(position = position_dodge(preserve = "single"))+
  coord_flip()+
  theme_tq()+
  scale_fill_tq()

ggplotly(g)


# Phase Two Obligation Plot

g2 <- military_tbl_formatted %>% 
  group_by(date, organization) %>% 
  summarise(total_phase_two_obligation = sum(total_phase_two_obligation)) %>% 
  ungroup() %>% 
  mutate(label_text = str_glue("Year: {date}
                               Organization: {organization}
                               Awards: {scales::dollar(total_phase_two_obligation)}")) %>% 
  ggplot(aes(organization, total_phase_two_obligation, fill = as.factor(date)))+
  geom_col(position = position_dodge(preserve = "single"), aes(text = label_text))+
  coord_flip()+
  theme_tq()+
  scale_fill_tq()

ggplotly(g2, tooltip = "text")

  

