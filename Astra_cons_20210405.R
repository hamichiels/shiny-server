# Load packages

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(rsconnect)
library(readxl)
library(reshape)

# Load data
#OPM: zorg ervoor dat deze Shiny-app in een folder staat waar ook een subfolder "data" te vinden is, met de dataset erin
#De naam vd dataset moet ook zonder spaties ertussen!
trend_data <- read_xls("data/Astradata.xls"
                       , sheet=1
                       , range="A6:G2000"
                       , col_names=c("date","odo","odo_diff","vol","speed","cons","cons_BC"))

trend_data <- as.data.frame(trend_data)

#Alle records eruit waarvoor geen volume werd berekend:
trend_data <- trend_data[which(!is.na(trend_data$vol)),]
#Melt dataframe voor selectie van KPI in UI-deel hieronder:
trend_data_melted <- melt.data.frame(trend_data,
                   id.vars=c(1),
                   measure.vars=c(2:7),
                   variable_name="variable",
                   na.rm=FALSE
                     )
#trend_description <- read_csv("data/trend_description.csv")



# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Fuel consumption"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("KPI"),
                                choices = unique(trend_data_melted$variable),
                                selected = "cons"),

                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = min(trend_data_melted$date), end = max(trend_data_melted$date),
                                   min = min(trend_data_melted$date), max = max(trend_data_melted$date)),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px")
                  # , textOutput(outputId = "desc"),
                  #  tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    trend_data_melted %>%
      filter(
        variable == input$type,
        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
        ))
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_trends()$date, y = selected_trends()$value, type = "l",
         xlab = "Date", ylab = "KPI value", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$value, f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  # Pull in description of trend
  # output$desc <- renderText({
  #   trend_text <- filter(trend_description, type == input$type) %>% pull(text)
  #   paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  # })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


#TODO from here:
# App runnen lukt lokaal, MAAR: 
# app publishen naar shinyapps lukt wel, maar kan daar niet geopend worden (geeft error), zie:
# https://support.rstudio.com/hc/en-us/articles/229848967-Why-does-my-app-work-locally-but-not-on-shinyapps-io-
