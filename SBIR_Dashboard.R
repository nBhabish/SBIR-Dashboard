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


# Quick Function ----

reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

#  UI ----

ui <- navbarPage(
  
  title = "Small Business Innovation Research Awards",
  inverse = FALSE,
  collapsible = TRUE,
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("united")),
    # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # JS ----
  
  shinyjs::useShinyjs(),
  
  # Application UI ----
  
  # * Input Section -----
  
  tabPanel(
    
    title = "State Analysis",
    
    div(
      class = "container",
      id    = "header",
      h1(class = "page-header", "Federal Research and Development", tags$small("Spending Dashboard")),
    ),
    
    # div(
    #   class = "container",
    #   id = "header",
    #   h1(class = "page-header", "Stock Analyzer", tags$small("by Business Science")),
    #   p(class = "lead", "This is the first mini-project completed in our", 
    #     a(href = "https://www.business-science.io/", target = "_blank", "Expert Shiny Applications Course (DS4B 202-R)"))
    # ),
    
    div(
      class = "container",
      id    = "application_ui",
      column(width = 4,
             wellPanel(
               div(
                 id = "main_input",
                 pickerInput(inputId = "picker_year_1",
                             label   = "Year",
                             choices = unique(military_tbl_formatted$date),
                             multiple = FALSE,
                             selected = 2010,
                             option   = pickerOptions(
                               actionsBox = FALSE,
                               liveSearch = TRUE,
                               size       = 5
                             )
                 ),
                 pickerInput(inputId = "picker_organization_1",
                             label   = "Organizations",
                             choices = unique(military_tbl_formatted$organization),
                             selected = "Department of Agriculture",
                             multiple = FALSE,
                             options = pickerOptions(
                               actionsBox = FALSE,
                               liveSearch = TRUE,
                               size       = 5
                             )
                 )
               ),
               div(
                 id = "input_buttons",
                 actionButton(inputId = "apply", label = "Apply", icon = icon("play")),
                 div(
                   class = "pull-right",
                   actionButton(inputId = "reset", label = "Reset", icon = icon("sync"))
                 )
               )
             ),
             
             div(
               class = "well",
               h4("About the SBIR"),
               id = "lorem_ipsum",
               p( 
                 tags$small("The Small Business Innovation Research (SBIR) programs is a competitive program that encourages small 
                 businesses to engage in Federal Research/Research and Development (R/R&D) with the
                 potential for commercialization. Through a competitive awards-based program, SBIR awards 
                 enable small businesses to explore their technological potential and provide the incentive to profit from its commercialization.")),
             )
             
      ),
      column(width = 8,
             div(
               class = "panel",
               div(class = "panel-header",
                   style = "padding: 20px;",
                   h3("Spending Across States")),
               div(class = "panel-body",
                   style = "padding: 20px;",
                   plotlyOutput(outputId = "plotly_1"),
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
          p(class = "lead", "",
            a(class = "btn btn-primary btn-sm", href = "https://www.sbir.gov/about", target = "_blank", "About SBIR"))
          ),
        )
      )
    )
  ),
  tabPanel(
    
    div(
      class = "container",
      id    = "header",
      h1(class = "page-header", "Federal Research and Development", tags$small("Spending Dashboard")),
    ),
    
    title = "Insights on Phase Awards",
           
           div(
             class = "container",
             id    = "application_ui",
             column(width = 4,
                    wellPanel(
                      div(
                        id = "main_input",
                        pickerInput(inputId = "picker_year_2",
                                    label   = "Year",
                                    choices = unique(military_tbl_formatted$date),
                                    multiple = TRUE,
                                    selected = 2010,
                                    option   = pickerOptions(
                                      actionsBox = FALSE,
                                      liveSearch = TRUE,
                                      size       = 5
                                    )
                        )
                      ),
                      div(
                        id = "input_buttons",
                        actionButton(inputId = "apply_1", label = "Apply", icon = icon("play")),
                        div(
                          class = "pull-right",
                          actionButton(inputId = "reset_1", label = "Reset", icon = icon("sync"))
                        )
                      )
                    ),
                    div(
                      class = "well",
                      h4("About the SBIR"),
                      id = "lorem_ipsum",
                      p( 
                        tags$small("The Small Business Innovation Research (SBIR) programs is a competitive program that encourages small 
                 businesses to engage in Federal Research/Research and Development (R/R&D) with the
                 potential for commercialization. Through a competitive awards-based program, SBIR awards 
                 enable small businesses to explore their technological potential and provide the incentive to profit from its commercialization.")),
                    ),
                    div(
                      class = "container",
                      id    = "About the project",
                      column(
                        width = 12,
                        div(
                          class = "panel",
                          p(class = "lead", "",
                            a(class = "btn btn-primary btn-sm", href = "https://www.sbir.gov/about", target = "_blank", "About SBIR"))
                        ),
                      )
                    )
             ),
             
             column(width = 8,
                    div(
                      class = "panel",
                      div(class = "panel-body",
                          style = "padding: 20px;",
                          tabsetPanel(
                            type = "pills",
                            
                            tabPanel(
                              title = "Phase One Awards",
                              plotlyOutput(outputId = "plotly_phase_1")),
                            
                            tabPanel(
                              title = "Phase Two Awards",
                              plotlyOutput(outputId = "plotly_phase_2")
                            )
                            )
                          )
                          
                    )             
              ),
             
           )
        )
  
  
  
)

# Server ----

server <- function(input, output, session){
  
  observeEvent(eventExpr = input$reset, handlerExpr = {
    
    
    # TabPanel 1 -----
    
    updatePickerInput(session = session,
                      inputId = "picker_year_1",
                      selected = c("2010"))
    
    
    updatePickerInput(session = session, 
                      inputId = "picker_organization_1",
                      selected = "Department of Agriculture")
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply")
    })
    
  })
  
  military_reactive_tbl <- eventReactive(eventExpr = input$apply, valueExpr = {

    military_tbl_formatted %>%

      filter(date %in% input$picker_year_1) %>%

      filter(organization %in% input$picker_organization_1)

  }, ignoreNULL = FALSE)


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
                colors    = "Greens",
                span = I(0)) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes  = TRUE,
          lakecolor  = toRGB("white")
        )
      )

  })
  
  # TabPanel 2 ----
  
  observeEvent(eventExpr = input$reset_1, handlerExpr = {
    
    updatePickerInput(session = session,
                      inputId = "picker_year_2",
                      selected = 2010)
    
    shinyjs::delay(ms = 100, expr = {
      shinyjs::click(id = "apply_1")
    })
    
  })
  
  military_phase_plots <- eventReactive(eventExpr = input$apply_1,valueExpr = {
    
    military_tbl_formatted %>% 
      mutate(date = as.factor(date)) %>% 
      filter(date %in% input$picker_year_2)
    
  }, ignoreNULL = FALSE)
  
  # Phase 1 Plot ----
  
  output$plotly_phase_1 <- renderPlotly({
    
    g1 <- military_phase_plots() %>% 
      group_by(date, organization) %>% 
      summarise(total_phase_one_obligation = sum(total_phase_one_obligation)) %>% 
      ungroup() %>% 
      mutate(label_text = str_glue("Year: {date}
                                Organization: {organization}
                                Awards: {scales::dollar(total_phase_one_obligation)}")) %>% 
      ggplot(aes(organization, total_phase_one_obligation, fill = date))+
      geom_col(position = position_dodge(preserve = "single"), aes(text = label_text))+
      coord_flip()+
      theme_tq()+
      scale_fill_tq()+
      labs(x = "",
           y = "",
           fill = "")+
      scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, accuracy = 1, prefix = "$", suffix = "M"))
    
    
    ggplotly(g1, tooltip = "text") %>% 
      reverse_legend_labels()
    
  })
  
  # Phase 2 Plot ----
  
  output$plotly_phase_2 <- renderPlotly({
    
    g2 <- military_phase_plots() %>% 
      group_by(date, organization) %>% 
      summarise(total_phase_two_obligation = sum(total_phase_two_obligation)) %>% 
      ungroup() %>% 
      mutate(label_text = str_glue("Year: {date}
                               Organization: {organization}
                               Awards: {scales::dollar(total_phase_two_obligation)}")) %>% 
      ggplot(aes(organization, total_phase_two_obligation, fill = date))+
      geom_col(position = position_dodge(preserve = "single"), aes(text = label_text))+
      coord_flip()+
      theme_tq()+
      scale_fill_tq()+
      labs(x = "",
           y = "",
           fill = "")+
      scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, accuracy = 1, prefix = "$", suffix = "M"))
    
    ggplotly(g2, tooltip = "text") %>% 
      reverse_legend_labels()
    
  })
  
}


shinyApp(ui = ui, server = server)