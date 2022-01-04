# Libraries ----

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)

# Reading in the data ----

military_tbl_formatted <- read_csv("00_data/military_spending_formatted.csv")

#  UI ----

ui <- navbarPage(
  
  title = "Military Spending Dashboard",
  inverse = TRUE,
  collapsible = TRUE,
  
  theme = shinytheme("paper"),
  
  # JS ----
  
  shinyjs::useShinyjs(),
  
  div(
    class = "container",
    id    = "header",
    h1(class = "page-header", h3("Federal Research and Development Spending Analyzer")),
    p(class = "lead", "Placeholder for some text",
      a(class = "btn btn-primary", href = "#", target = "_blank", "Learn more about it here")),
  ),
  
  
  # Application UI ----
  
  
  # * Input Section -----
  
  tabPanel(
    title = "Overall Analysis",
    
    div(
      class = "container",
      id    = "application_ui",
      column(width = 4,
             wellPanel(
               div(
                 id = "main_input",
                 pickerInput(inputId = "picker_year",
                             label   = "Year",
                             choices = c(2011, 2012, 2013, 2014),
                             multiple = FALSE,
                             selected = 2011,
                             option   = pickerOptions(
                               actionsBox = FALSE,
                               liveSearch = TRUE,
                               size       = 5
                             )
                 ),
                 pickerInput(inputId = "pikcer_organization",
                             label   = "Organizations",
                             choices = c("A", "B", "C", "D", "E", "F"),
                             multiple = FALSE,
                             options = pickerOptions(
                               actionsBox = FALSE,
                               liveSearch = TRUE,
                               size       = 4
                             )
                 )
               ),
               div(
                 id = "input_buttons",
                 actionButton(inputId = "analyze", label = "Apply", icon = icon("download")),
                 div(
                   class = "pull-right",
                   actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
                 )
               )
             )
      ),
      column(width = 8,
             div(
               class = "panel",
               div(class = "panel-header",
                   style = "padding: 20px;",
                   h5("Panel Header")),
               div(class = "panel-body",
                   style = "padding: 20px;",
                   h5("Map goes here"))
             )
      )
    ),
    
    div(
      class = "container",
      id    = "About the project",
      column(
        width = 12,
        div(
          class = "panel",
          div(class = "panel-header", h5("About SGP"),
              style = "padding: 20px;"),
          div(class = "panel-body",
              p(class = "lead", 
                "Something goes here properly"))
          
        )
      )
    )
  ),
  tabPanel(title = "Insights on Phase Awards",
           
           div(
             class = "container",
             id    = "application_ui",
             column(width = 4,
                    wellPanel(
                      div(
                        id = "main_input",
                        pickerInput(inputId = "picker_year",
                                    label   = "Year",
                                    choices = c(2011, 2012, 2013, 2014),
                                    multiple = FALSE,
                                    selected = 2011,
                                    option   = pickerOptions(
                                      actionsBox = FALSE,
                                      liveSearch = TRUE,
                                      size       = 5
                                    )
                        ),
                        pickerInput(inputId = "pikcer_organization",
                                    label   = "Organizations",
                                    choices = c("A", "B", "C", "D", "E", "F"),
                                    multiple = FALSE,
                                    options = pickerOptions(
                                      actionsBox = FALSE,
                                      liveSearch = TRUE,
                                      size       = 4
                                    )
                        )
                      ),
                      div(
                        id = "input_buttons",
                        actionButton(inputId = "analyze", label = "Apply", icon = icon("download")),
                        div(
                          class = "pull-right",
                          actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
                        )
                      )
                    )
             ),
             column(width = 8,
                    div(
                      class = "panel",
                      div(class = "panel-header",
                          style = "padding: 20px;",
                          h4("Panel Header")),
                      div(class = "panel-body",
                          style = "padding: 20px;",
                          plotlyOutput(outputId = "plotly_1"))
                    )
             )
           )
        )
  
  
  
)

# Server ----

server <- function(input, output, session){
  
  military_map_plot_tbl <- reactive({
    
    military_reactive_tbl() %>% 
      group_by(state) %>% 
      summarise(total_obligation = sum(total_obligation)) %>% 
      ungroup() %>% 
      mutate(label_text = str_glue("State: {state}
                               Total Obligation: {scales::dollar(total_obligation)}"))
    
  })
  
  output$plotly_1 <- renderPlotly({
    military_map_plot_tbl() %>% 
      plot_geo(locationmode = "USA-states") %>% 
      add_trace(z         = ~total_obligation,
                locations = ~state,
                color     = ~total_obligation, 
                text      = ~label_text,
                colors    = "Greens") %>% 
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes  = TRUE,
          lakecolor  = toRGB("white")
        )
      )
    
  })
  
  
}


shinyApp(ui = ui, server = server)