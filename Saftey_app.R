library(shiny)
library(bslib)
library(tidyverse)

Results <- read_csv("data/processed/Nuclear_Metrics_Scores.csv")

ui <- page_fluid(
  selectInput(
    inputId = "date_select",
    label = "Select Date:",
    choices = unique(Results$date),
    selected = "2007-01-01"
  ),
  radioButtons(
    inputId = "stat",
    label = "Select Statistic:",
    choices = c(
      "Overall Safety Score",
      "Power Generated",
      "Power Gernerated vs Safety Score"
    ),
    selected = "Overall Safety Score"
  ),
  plotOutput("safety_plot")
)

server <- function(input, output, session) {
  output$safety_plot <- renderPlot({
    filtered_data <- Results %>%
      filter(date == input$date_select)

    ggplot(
      filtered_data,
      aes(x = state_of_location, y = input$stat)
    ) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Overall Safety Scores for", input$date_select),
        x = "State of Location",
        y = "Overall Safety Score"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)
