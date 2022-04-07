

# Libraries ---------------------------------------------------------------



library(shiny) # Web Application Framework for R
library(shinyWidgets) # Custom Inputs Widgets for Shiny
library(shinythemes) # Themes for Shiny
library(shinyjs) # Easily Improve the User Experience of Your Shiny Apps in Seconds

library(plotly) # Create Interactive Web Graphics via 'plotly.js'
library(tidyquant) # Tidy Quantitative Financial Analysis
library(tidyverse) # Easily Install and Load the 'Tidyverse'


library(bslib) # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
library(thematic) # Unified and Automatic 'Theming' of 'ggplot2', 'lattice', and 'base' R Graphics


library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(sf) # Simple Features for R




# Theming -----------------------------------------------------------------



shinyOptions(bootstrapLib = TRUE)
thematic::thematic_shiny(font = "auto")

my_theme <- bslib::bs_theme(
  version    = 4,
  bootswatch = "united",
  fg         = "black",
  bg         = "white",
  primary    = "#E41C38",
  secondary  = "#E41C38",
  base_font  = font_google("Harmattan"),
  heading_font = font_google("Harmattan"),
  font_scale  = 0.95
)


# Loading Data ------------------------------------------------------------

military_tbl_formatted <-
  read_csv("00_data/military_spending_formatted.csv")


us_shp_file <-
  read_sf("00_data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp") %>%
  janitor::clean_names()


# Function for reversing legend -------------------------------------------

reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}


# USER INTERFACE ----------------------------------------------------------

ui <- navbarPage(
  title = "Small Business Innovation Research Awards",
  inverse = FALSE,
  collapsible = TRUE,
  theme = my_theme,
  
  tags$body(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  
  
  shinyjs::useShinyjs(),
  
  # Tab Panel 1 -------------------------------------------------------------
  
  
  tabPanel(
    title = "State Analysis",
    
    div(
      class = "container",
      id    = "header",
      h1(
        class = "page-header",
        "Federal Research and Development",
        tags$small("Spending Dashboard")
      )
    ),
    
    div(class = "container",
        id    = "application_ui",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              # * Input Buttons ---------------------------------------------------------
              
              
              div(
                id = "main_input",
                pickerInput(
                  inputId = "picker_year_1",
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
                pickerInput(
                  inputId = "picker_organization_1",
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
              
              
              # * Apply and Reset Buttons -----------------------------------------------
              
              
              div(
                id = "input_buttons",
                actionButton(
                  inputId = "apply",
                  label = "Apply",
                  icon = icon("play")
                ),
                div(
                  class = "pull-right",
                  actionButton(
                    inputId = "reset",
                    label = "Reset",
                    icon = icon("sync")
                  )
                )
              )
            ),
            
            br(),
            br(),
            
            div(
              class = "well",
              h4("About the SBIR"),
              id = "lorem_ipsum",
              p(
                tags$small(
                  "The Small Business Innovation Research (SBIR) programs is a competitive program that encourages small
                 businesses to engage in Federal Research/Research and Development (R/R&D) with the
                 potential for commercialization. Through a competitive awards-based program, SBIR awards
                 enable small businesses to explore their technological potential and provide the incentive to profit from its commercialization.",
                  a(
                    class = "btn btn-primary btn-sm",
                    href = "https://www.sbir.gov/about",
                    target = "_blank",
                    "Learn More"
                  )
                )
              )
            )
            
          ),
          column(width = 9,
                 div(
                   class = "panel",
                   div(
                     class = "panel-header",
                     style = "padding: 20px;",
                     h3("Spending Across States")
                   ),
                   div(
                     class = "panel-body",
                     style = "padding: 20px;",
                     leafletOutput(outputId = "us_map")
                   )
                 ))
        ))
  ),
  
  
  # Tab Panel 2 -------------------------------------------------------------
  
  
  tabPanel(
    div(
      class = "container",
      id    = "header",
      h1(
        class = "page-header",
        "Federal Research and Development",
        tags$small("Spending Dashboard")
      )
    ),
    
    title = "Insights on Phase Awards",
    
    
    
    # * Input Buttons ---------------------------------------------------------
    
    
    div(class = "container",
        id    = "application_ui",
        fluidRow(
          column(
            width = 3,
            wellPanel(div(
              id = "main_input",
              pickerInput(
                inputId = "picker_year_2",
                label   = "Year",
                choices = unique(military_tbl_formatted$date),
                multiple = TRUE,
                selected = 2010,
                option   = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size       = 5
                )
              ),
              
              
              # * Apply and Reset Buttons -----------------------------------------------
              
              
              div(
                id = "input_buttons",
                actionButton(
                  inputId = "apply_1",
                  label = "Apply",
                  icon = icon("play")
                ),
                div(
                  class = "pull-right",
                  actionButton(
                    inputId = "reset_1",
                    label = "Reset",
                    icon = icon("sync")
                  )
                )
              )
            )),
            
            
            
            # * Line Breaks -----------------------------------------------------------
            
            
            
            
            br(),
            br(),
            
            
            
            # * About The Project Panel-Box -------------------------------------------
            
            
            div(
              class = "well",
              h4("About the SBIR"),
              id = "lorem_ipsum",
              p(
                tags$small(
                  "The Small Business Innovation Research (SBIR) programs is a competitive program that encourages small
                 businesses to engage in Federal Research/Research and Development (R/R&D) with the
                 potential for commercialization. Through a competitive awards-based program, SBIR awards
                 enable small businesses to explore their technological potential and provide the incentive to profit from its commercialization.",
                  a(
                    class = "btn btn-primary btn-sm",
                    href = "https://www.sbir.gov/about",
                    target = "_blank",
                    "Learn More"
                  )
                )
              )
            )
          ),
          column(width = 9,
                 div(
                   class = "panel",
                   div(
                     class = "panel-body",
                     style = "padding: 20px;",
                     tabsetPanel(
                       type = "pills",
                       
                       tabPanel(title = "Phase One Awards",
                                plotlyOutput(outputId = "plotly_phase_1")),
                       
                       tabPanel(title = "Phase Two Awards",
                                plotlyOutput(outputId = "plotly_phase_2"))
                     )
                   )
                   
                 ))
        ))
  )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer()
  
  
  # Tab Panel 1  ------------------------------------------------------------
  
  
  # * Reset btn functionality -----------------------------------------------
  
  
  
  observeEvent(eventExpr = input$reset, handlerExpr = {
    updatePickerInput(
      session = session,
      inputId = "picker_year_1",
      selected = c("2010")
    )
    
    
    updatePickerInput(session = session,
                      inputId = "picker_organization_1",
                      selected = "Department of Agriculture")
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply")
    })
    
  })
  
  
  
  
  # * Reactive output tbl after input filters -------------------------------
  
  
  
  military_reactive_tbl <-
    eventReactive(
      eventExpr = input$apply,
      valueExpr = {
        military_tbl_formatted %>%
          
          filter(date %in% input$picker_year_1) %>%
          
          filter(organization %in% input$picker_organization_1)
        
      },
      ignoreNULL = FALSE
    )
  
  # Leaflet starts here -----------------------------------------------------
  
  
  
  
  military_tbl_fitered <- eventReactive(
    eventExpr = input$apply,
    valueExpr = {
      military_tbl_formatted %>%
        filter(organization %in% input$picker_organization_1,
               date %in% input$picker_year_1)
      
    },
    ignoreNULL = FALSE
  )
  
  
  # dataset for leaflet -----------------------------------------------------
  
  us_shp_1 <- reactive({
    us_shp_file %>%
      left_join(military_tbl_fitered(),
                by = c("stusps" = "state")) %>%
      mutate(total_obligation_label = scales::dollar(total_obligation))
  })
  
  
  
  # leaflet popup -----------------------------------------------------------
  
  mappopup <- reactive({
    paste(
      "State:",
      us_shp_1()$stusps,
      "<br>",
      "Obligation",
      ":",
      us_shp_1()$total_obligation_label,
      "<br>"
    )
  })
  
  
  
  # leaflet server ----------------------------------------------------------
  
  output$us_map <- renderLeaflet({
    leaflet(us_shp_file) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$CartoDB.DarkMatter)
  })
  
  
  
  # Observing leafletProxy --------------------------------------------------
  
  
  observe({
    
    mappopup_1 <- mappopup()
    
    pal <- colorQuantile("Reds", domain = us_shp_1()$total_obligation, n = 10, na.color = "#bdbdbd")
    
    leafletProxy("us_map") %>%
      addPolygons(
        data = us_shp_1(),
        weight = 1,
        smoothFactor = 0.5,
        color = "white",
        fillColor = pal(us_shp_1()$total_obligation),
        # opacity = 1.0,
        fillOpacity = 1.5,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = mappopup()
      )
  })
  
  # leaflet ends here -------------------------------------------------------
  
  
  
  # Tab Panel 2 -------------------------------------------------------------
  
  
  # * Reset btn functionality -----------------------------------------------
  
  observeEvent(eventExpr = input$reset_1, handlerExpr = {
    updatePickerInput(session = session,
                      inputId = "picker_year_2",
                      selected = 2010)
    
    shinyjs::delay(ms = 100, expr = {
      shinyjs::click(id = "apply_1")
    })
    
  })
  
  
  # * Reactive output for phase plots ---------------------------------------
  
  
  
  military_phase_plots <-
    eventReactive(
      eventExpr = input$apply_1,
      valueExpr = {
        military_tbl_formatted %>%
          mutate(date = as.factor(date)) %>%
          filter(date %in% input$picker_year_2)
        
      },
      ignoreNULL = FALSE
    )
  
  
  # * Phase Plot 1 ----------------------------------------------------------
  
  
  
  output$plotly_phase_1 <- renderPlotly({
    g1 <- military_phase_plots() %>%
      group_by(date, organization) %>%
      summarise(total_phase_one_obligation = sum(total_phase_one_obligation)) %>%
      ungroup() %>%
      mutate(
        label_text = str_glue(
          "Year: {date}
                                Organization: {organization}
                                Awards: {scales::dollar(total_phase_one_obligation)}"
        )
      ) %>%
      ggplot(aes(organization, total_phase_one_obligation, fill = date)) +
      geom_col(position = position_dodge(preserve = "single"), aes(text = label_text)) +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "",
           fill = "") +
      scale_y_continuous(labels = scales::dollar_format(
        scale = 1e-6,
        accuracy = 1,
        prefix = "$",
        suffix = "M"
      ))
    
    
    ggplotly(g1, tooltip = "text") %>%
      reverse_legend_labels()
    
  })
  
  
  # * Phase Plot 2 ----------------------------------------------------------
  
  
  
  output$plotly_phase_2 <- renderPlotly({
    g2 <- military_phase_plots() %>%
      group_by(date, organization) %>%
      summarise(total_phase_two_obligation = sum(total_phase_two_obligation)) %>%
      ungroup() %>%
      mutate(
        label_text = str_glue(
          "Year: {date}
                               Organization: {organization}
                               Awards: {scales::dollar(total_phase_two_obligation)}"
        )
      ) %>%
      ggplot(aes(organization, total_phase_two_obligation, fill = date)) +
      geom_col(position = position_dodge(preserve = "single"), aes(text = label_text)) +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "",
           fill = "") +
      scale_y_continuous(labels = scales::dollar_format(
        scale = 1e-6,
        accuracy = 1,
        prefix = "$",
        suffix = "M"
      ))
    
    ggplotly(g2, tooltip = "text") %>%
      reverse_legend_labels()
    
  })
  
}


shinyApp(ui = ui, server = server)
