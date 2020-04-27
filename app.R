# Importing necessary libraries
library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(tidyr)
library(reshape)
library(plotrix)
library(scales)
library(shinydashboard)

# Local Path on my machine
# setwd("E:\\Academics\\Data science Monash\\Semester 1\\FIT 5147 Exploration\\Project_Visualisation\\Work")

# Read data
excel_data <- read.csv("Data.csv")
top3_data <- read.csv("Top_3.csv")

# Melt data based on 'Country' and transform the value column into numeric data
melt_data_with_region <- melt(excel_data, id=c("Country"))
melt_data<-melt_data_with_region[!(melt_data_with_region$variable=="Region"),]
melt_data$value <- as.numeric(melt_data$value)


# Shiny: UI
ui <- fluidPage(
  
  # Create Panels
  tabsetPanel(id = "inTabset",
              
    # First Panel  
    tabPanel(title = "Basic Comparison", value = "panel1",
             
             # Show text
             column(12, align="center", tags$pre(h1("Happiness Report!"))),
             
             # Show drop down menus in single row
             fluidRow(
               column(6, align="center",selectInput("first_country_input", "Select a primary country:", 
                                    choices = c(levels(melt_data$Country)))),
               column(6, align="center",selectInput("second_country_input", "Select country/region to compare:", 
                                    choices = c(levels(melt_data$Country)),
                                    selected = "Argentina"))
             ),
             
             # Show graph of above 2 selected countries
             plotOutput("first_fold", height = "300px"),
             
             # New lines
             br(),br(),
             
             # Next button to go to next tab
             column(12, align="center",actionButton("first_fold_button",label = "Next"))
    ),
    
    # Second Panel
    tabPanel(title = "Factor Analysis", value = "panel2",
             br(),
             
             # Show drop down list and bar graph
             fluidRow(
               column(6, align="center", selectInput("second_factor_input", 
                                                     "Select the factor to compare in the same region: ", 
                                                     choices = c(levels(melt_data$variable))))
               ),  
             
             column(5, align="center", textOutput("graph_info")),
             br(),
             
             # Show pie chart with information text
             fluidRow(
               column(6,  plotOutput("second_fold_bar", height = "400px", width = "500px")),
               column(6, plotOutput("second_fold_pie", height = "400px", width = "800px"))
             ),
             
             br(),
             # Add next and previous buttons
             column(6, align="center",actionButton("second_fold_button_prev",label = "Previous")),
             column(6, align="center",actionButton("second_fold_button_next",label = "Next"))
    ),
    
    # Third Panel
    tabPanel(title = "News", value = "panel3",
             
             # Adding UI for graph and search results
             fluidRow(
               column(12, plotOutput("third_fold_graph"), height = "600px"),
               column(12, tags$pre(h4("Scroll down for more information for this country..."))),
               column(12, htmlOutput("third_fold_news")))
             
    )
    

  )
)

# Shiny: server
server <- function(input, output, session) {
  
  # Button reactions in each page/fold
  observeEvent(input$first_fold_button, {
    updateTabsetPanel(session, "inTabset", selected = "panel2")
  })
  observeEvent(input$second_fold_button_prev, {
    updateTabsetPanel(session, "inTabset", selected = "panel1")
  })
  observeEvent(input$second_fold_button_next, {
    updateTabsetPanel(session, "inTabset", selected = "panel3")
  })
  
  # filtering data as per country selected in drop down lists
  first_country_input_data <- reactive({
    melt_data %>% filter(Country == input$first_country_input)
    
  })
  second_country_input_data <- reactive({
    melt_data %>% filter(Country == input$second_country_input)
  })
  
  # Show graph in first page
  output$first_fold <- renderPlot({
    df <- rbind(first_country_input_data(), second_country_input_data())
    
    ggplot(df,aes(x = variable, y = value, fill = Country)) + 
      geom_bar(position = "fill", stat = "identity") +
      scale_y_continuous(name="Share", labels = percent_format()) +
      xlab("Factors")
  })
  
  # Show texts in second page
  output$graph_info <- renderPrint({
    cat("Comparison of ", input$second_factor_input," in same region as ", input$first_country_input)
  })
  
  output$pie_info <- renderPrint({
    cat("Distribution of each factor in", input$first_country_input)
  })
  
  # Pie chart
  output$second_fold_pie <- renderPlot({
    df<-first_country_input_data()[!(first_country_input_data()$variable %in% c("Population", "GDP_per_Capita")),]
    
    pie(df$value,labels = paste(df$variable, df$value, sep = " - "),
        col=rainbow(length(df$value)), 
        radius = 1,
        main=input$first_country_input)
  })
  
  # Merge data for flipped bar chart
  second_factor_input_data <- reactive({
    melt_data_with_region %>% filter(variable == input$second_factor_input)
  })
  
  # flipped bar chart
  output$second_fold_bar <- renderPlot({
    
    reg <- excel_data[excel_data$Country == input$first_country_input, "Region"]
    required_countries <- excel_data[excel_data$Region == reg, "Country"]
    
    df2<-second_factor_input_data()[second_factor_input_data()$Country %in% required_countries,]
    
    ggplot(df2, aes(x = Country, y = value)) +
      geom_bar(stat = "identity", fill = "#FF6666") +
      geom_text(aes(label=value))  +
      xlab("Country") +
      coord_flip()
  })
  
  # Show text above group bar chart in third page
  output$graph_info_2 <- renderPrint({
    cat("Comparison of ", input$first_country_input," with the top average of top 3 countries in the world")
  })
  
  # Merge data
  third_country_input_data <- reactive({
    rbind(first_country_input_data(), top3_data)
  })
  
  # Group bar chart in third page
  output$third_fold_graph <- renderPlot({
    df3 <- rbind(first_country_input_data(), top3_data)
    # df <- third_country_input_data()
    df3 <- df3[!(df3$variable == "Region"),]
    
    ggplot(df3, aes(factor(variable), value, fill = Country)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + 
      # scale_x_discrete(labels = abbreviate) +
      theme(axis.text.x=element_text(angle = 90,vjust = 0)) +
      ggtitle(paste("Comparison of", 
                    input$first_country_input,
                    "with the top average of top 3 countries in the world",
                    sep = " ")) +
      labs(xlab("Factors"), ylab("Value")) +
      scale_fill_discrete(labels=c(input$first_country_input, "Average of top 3 countries"))

  })
  
  # Had created a map code which was working but removed as was not able to integrate D3 map here.
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng=145.715 , lat =-16.091, zoom=2) %>%             # Set default view of the map
      addTiles()
  })
  
  # Show Google results for primary country
  output$third_fold_news<-renderUI({
    link <- paste("https://www.google.com/search?q=",input$first_country_input, sep = "")
    HTML(readLines(link))
  })
}

shinyApp(ui, server)



