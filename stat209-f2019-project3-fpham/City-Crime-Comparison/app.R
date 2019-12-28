#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)

# Load Data, filter out computed National averages
ucr_crime <- read.csv("ucr_crime_1975_2015.csv", stringsAsFactors = FALSE)
ucr_crime <- ucr_crime %>% 
  filter(department_name != "National")

# Get cities for selection controls
cities <- as.list(unique(ucr_crime$department_name))

# Compute Average Statistics
ucr_avg_crime <- ucr_crime %>% 
  group_by(year) %>% 
  summarise(avg_violent_crime = mean(violent_crime, na.rm = TRUE), avg_homs = mean(homs_sum, na.rm = TRUE), 
            avg_rape = mean(rape_sum, na.rm = TRUE),avg_rob = mean(rob_sum, na.rm = TRUE),  
            avg_agg_ass = mean(agg_ass_sum, na.rm = TRUE), avg_violent_100k = mean(violent_per_100k, na.rm = TRUE),
            avg_homs_100k = mean(homs_per_100k, na.rm = TRUE), avg_rape_100k = mean(rape_per_100k, na.rm = TRUE),
            avg_robs_100k = mean(rob_per_100k, na.rm = TRUE), avg_agg_ass_100k = mean(agg_ass_per_100k, na.rm = TRUE))


# Define UI for the application
ui <- fluidPage(
  titlePanel("Crime Rate Analyzer",
             windowTitle = "US Cities Violent Crime"

  ),
  sidebarLayout(
    sidebarPanel(
      # Add in text stating the data we have
      helpText("Violent crime data for 68 US police jurisdictions."
              ),
      # Create city selection widget
      selectInput("cityInput", label = h3("Select Cities"),
                  choices = cities,
                  multiple = TRUE,
                  selectize = TRUE),
      # Create time frame selection widget
      sliderInput("yearInput", label = h3("Year Range"), step = 5,
                  min = 1975, max = 2015, value = c(1975,2015),
                  sep = ""
      )
    ),
    mainPanel(
      
      # A question to lead the user into engaging with the interface
      h3("Which city do you think has the highest increase in crime rate?"),
      helpText("The average of all the cities is the dotted grey line"
      ),
      
      # Create tabs for the different categories of violent crime 
      tabsetPanel( id = 'tabs', selected = 'violent_crime-violent_per_100k-avg_violent_crime-avg_violent_100k',
        tabPanel("Total", value = 'violent_crime-violent_per_100k-avg_violent_crime-avg_violent_100k', 
                 plotOutput("raw_graph1"), plotOutput("normalized_graph1")),
        tabPanel("Homicide", value = 'homs_sum-homs_per_100k-avg_homs-avg_homs_100k', 
                 plotOutput("raw_graph2"), plotOutput("normalized_graph2")),
        tabPanel("Rape",value = 'rape_sum-rape_per_100k-avg_rape-avg_rape_100k', 
                 plotOutput("raw_graph3"), plotOutput("normalized_graph3")),
        tabPanel("Robbery", value = 'rob_sum-rob_per_100k-avg_rape-avg_rape_100k',
                 plotOutput("raw_graph4"), plotOutput("normalized_graph4")),
        tabPanel("Aggravated Assualt", value = 'agg_ass_sum-agg_ass_per_100k-avg_agg_ass-avg_agg_ass_100k', 
                 plotOutput("raw_graph5"), plotOutput("normalized_graph5"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Create y labels for graphs on different tab panels
  y_labels <- list('violent_crime' = 'Total Violent Crime', 'violent_per_100k' = 'Violent Crime per 100k', 
                   'homs_sum' = 'Total Homicides', 'homs_per_100k' = 'Homicides per 100k',
                   'rape_sum' = 'Total Rapes', 'rape_per_100k' = 'Rapes per 100k',
                   'rob_sum' = 'Total Robberies', 'rob_per_100k' = 'Robberies per 100k',
                   'agg_ass_sum' = 'Total Aggrevated Assualt', 'agg_ass_per_100k' = 'Aggrevated Assualt per 100k')
  # Get variables to filter data set depending on the selected tab
  variables <- reactive(
    str_split(input$tabs, '-')
  )
  # Filter data for selected cities and year
  ucr_crime_filtered <- reactive(
    ucr_crime %>%
        filter(between(year, input$yearInput[1], input$yearInput[2]),
               department_name %in% input$cityInput)
  )
  # Filter average data for the selected years
  ucr_avg_crime_filtered <- reactive(
    ucr_avg_crime %>%
      filter(between(year, input$yearInput[1], input$yearInput[2]))
  )
  # Make raw data plot
  output$raw_graph1 <- output$raw_graph2 <- output$raw_graph3 <- output$raw_graph4 <- output$raw_graph5 <- renderPlot(
      ggplot() +
        geom_line(data = ucr_avg_crime_filtered(), aes_string(x='year', y = variables()[[1]][3]),
                  linetype = "dashed", color = "grey") +
        geom_line(data = ucr_crime_filtered(),
                  aes_string(x='year', y = variables()[[1]][1], group = 'department_name', color = 'department_name'), size = 1.5) +
        ggtitle("Raw Crime Numbers") +
        xlab('Year') +
        ylab(y_labels[variables()[[1]][1]]) +
        labs(colour = "City") +
        scale_color_brewer(palette = "Spectral", type = "div") +
        theme_bw() + 
        theme(legend.title = element_text(size = 12),
              legend.text = element_text(size = 11),
              axis.text = element_text(size = 11),
              plot.title = element_text(size = 13)
              ) 
  )
  # Make normalized data plot
  output$normalized_graph1 <- output$normalized_graph2 <- output$normalized_graph3 <- output$normalized_graph4 <- output$normalized_graph5 <-  renderPlot(
      ggplot() +
        geom_line(data = ucr_avg_crime_filtered(), aes_string(x='year', y = variables()[[1]][4]),
                  linetype = "dashed", color = "grey") +
        geom_line(data = ucr_crime_filtered(), aes_string(x='year', 
                                                              y = variables()[[1]][2], group = 'department_name', 
                                                              color = 'department_name'), size = 1.5) +
        ggtitle("Normalized Crime Numbers") +
        xlab('Year') +
        ylab(y_labels[variables()[[1]][2]]) +
        labs(colour = "City") +
        scale_color_brewer(palette = "Spectral", type = "div") +
        theme_bw() + 
        theme(legend.title = element_text(size = 12),
              legend.text = element_text(size = 11),
              axis.text = element_text(size = 11),
              plot.title = element_text(size = 13)
             )
  )
}

# Run the application
shinyApp(ui = ui, server = server)