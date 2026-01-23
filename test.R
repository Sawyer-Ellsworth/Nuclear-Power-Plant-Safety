library(shiny)
library(bslib)
library(tidyverse)
library(glue)

# Ensure this file path is correct relative to your app.R or project root
Results <- read_csv("data/processed/Nuclear_Metrics_Scores.csv")

ui <- page_fluid(
  titlePanel("Nuclear Site Metrics"),

  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        inputId = "date_select",
        label = "Select Date:",
        choices = unique(Results$date),
        selected = "2007-01-01"
      ),
      radioButtons(
        inputId = "stat",
        label = "Select Statistic to Plot:",
        choices = c(
          "Overall Safety Score" = "overall_safety_score",
          "Power Generated" = "power_generated",
          "Power Generated vs Safety Score" = "power_vs_safety"
        ),
        # FIX: Value must match one of the choice values, not the label
        selected = "overall_safety_score"
      ),
      hr(), # Visual separator
      selectInput(
        inputId = "sort_by",
        label = "Sort Graphs By:",
        choices = c(
          "State/Site Name" = "state_of_location",
          "Overall Safety Score" = "overall_safety_score",
          "Power Generated" = "power_generated"
        ),
        selected = "state_of_location"
      ),
      radioButtons(
        inputId = "sort_order",
        label = "Sort Order:",
        choices = c(
          "Ascending (A-Z / Low-High)" = "asc",
          "Descending (Z-A / High-Low)" = "desc"
        ),
        selected = "asc"
      )
    ),

    # Main content area
    card(
      card_header("Metric Visualization"),
      plotOutput("safety_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # 1. Reactive Data Block: Handles Filtering AND Sorting
  sorted_data <- reactive({
    req(input$date_select) # Wait until date is selected

    # First, filter by date
    df <- Results |> filter(date == input$date_select)

    # Second, apply sorting to the Factor Levels of 'state_of_location'
    # In ggplot + coord_flip, the LAST factor level appears at the TOP of the Y-axis.

    is_desc <- input$sort_order == "desc"

    if (input$sort_by == "state_of_location") {
      # Alphabetical Sort
      if (is_desc) {
        # Descending (Z-A): Z should be at Top (Last Level) -> Standard Sort
        df <- df |>
          mutate(
            state_of_location = factor(
              state_of_location,
              levels = sort(unique(state_of_location))
            )
          )
      } else {
        # Ascending (A-Z): A should be at Top (Last Level) -> Reverse Sort
        df <- df |>
          mutate(
            state_of_location = factor(
              state_of_location,
              levels = sort(unique(state_of_location), decreasing = TRUE)
            )
          )
      }
    } else {
      # Numeric Sort (Power or Safety)
      # We use fct_reorder to sort sites based on the chosen numeric column

      if (is_desc) {
        # Descending (Highest on Top): Highest Value -> Last Level -> .desc=FALSE (default)
        df <- df |>
          mutate(
            state_of_location = fct_reorder(
              state_of_location,
              .data[[input$sort_by]]
            )
          )
      } else {
        # Ascending (Lowest on Top): Lowest Value -> Last Level -> .desc=TRUE
        df <- df |>
          mutate(
            state_of_location = fct_reorder(
              state_of_location,
              .data[[input$sort_by]],
              .desc = TRUE
            )
          )
      }
    }

    return(df)
  })

  output$safety_plot <- renderPlot({
    # Use the pre-sorted data
    df <- sorted_data()

    # Pretty title generator
    label_map <- c(
      "overall_safety_score" = "Overall Safety Score",
      "power_generated" = "Power Generated"
    )

    if (input$stat == "power_vs_safety") {
      # PLOT 1: Power vs Safety (Specific Visualization)
      ggplot(
        df,
        aes(
          x = state_of_location,
          y = overall_safety_score,
          fill = power_generated
        )
      ) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(
          low = "green",
          high = "red",
          na.value = "grey50",
          name = "Power Gen"
        ) +
        labs(
          title = paste(
            "Safety Score (Bar) colored by Power (Fill) -",
            input$date_select
          ),
          subtitle = paste("Sorted by:", input$sort_by),
          x = "Site / State",
          y = "Overall Safety Score"
        ) +
        theme_minimal(base_size = 14)
    } else {
      # PLOT 2: Generic (Either Power OR Safety)

      # Determine label safely
      y_label <- if (!is.null(label_map[input$stat])) {
        label_map[input$stat]
      } else {
        tools::toTitleCase(gsub("_", " ", input$stat))
      }

      ggplot(df, aes(x = state_of_location, y = .data[[input$stat]])) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(
          title = paste(y_label, "for", input$date_select),
          subtitle = paste("Sorted by:", input$sort_by),
          x = "Site / State",
          y = y_label
        ) +
        theme_minimal(base_size = 14)
    }
  })
}

shinyApp(ui, server)
