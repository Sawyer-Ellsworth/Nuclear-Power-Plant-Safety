#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| standalone: true
#| viewerHeight: 650
library(shiny)
library(bslib)

library(munsell) # shinylive does not run without this

library(dplyr)
library(ggplot2) #tidyverse packages to make shiny run smoother
library(forcats)

#Used to Uploaded directly from github as local csvs don't work with shinylive
url <- "https://raw.githubusercontent.com/Sawyer-Ellsworth/Nuclear-Power-Plant-Safety/refs/heads/main/data/processed/Nuclear_Metrics_Scores.csv"
Results <- read.csv(url)
Results$date <- as.character(Results$date)

safe_num <- function(x) as.numeric(gsub(",", "", as.character(x)))

cor_by_date <- Results |>
  mutate(
    date = as.Date(date),
    power_generated = safe_num(power_generated),
    overall_safety_score = as.numeric(overall_safety_score)
  ) |>
  group_by(date) |>
  summarize(
    n = sum(complete.cases(power_generated, overall_safety_score)),
    correlation = if (n >= 2) cor(power_generated, overall_safety_score, use = "complete.obs") else NA_real_,
    direction = case_when(
      is.na(correlation) ~ NA_character_,
      correlation > 0 ~ "positive",
      correlation < 0 ~ "negative",
      TRUE ~ "zero"
    ),
    .groups = "drop"
  ) |>
  arrange(date)
ui <- page_fluid(
  titlePanel("Nuclear Site Metrics"),

  layout_sidebar(
    sidebar = sidebar(
      # Select date for sorting
      selectInput( 
        inputId = "date_select",
        label = "Select Date:",
        choices = unique(Results$date),
        selected = "2007-01-01"
      ),
      # Select statistic to plot for ggplot
      radioButtons( 
        inputId = "stat",
        label = "Select Statistic to Plot:",
        choices = c(
          "Overall Safety Score" = "overall_safety_score",
          "Power Generated" = "power_generated",
          "Power Generated vs Safety Score" = "power_vs_safety",
          "Safety vs Power Correlation" = "safety_power_corr",
          "Monthly correlation trend" = "corr_trend"
        ),
        selected = "safety_power_corr"
      ),
      selectInput( # Select variable to sort by
        inputId = "sort_by",
        label = "Sort Graphs By:",
        choices = c(
          "Site Name" = "state_of_location",
          "Overall Safety Score" = "overall_safety_score",
          "Power Generated" = "power_generated"
        ),
        selected = "state_of_location"
      ),
      # Select sort order
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

    # Plot area
    card(
      card_header("Metric Visualization"),
      plotOutput("safety_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {

  sorted_data <- reactive({
  req(input$date_select)

  df <- Results |>
    filter(date == input$date_select)

  is_desc <- input$sort_order == "desc"

  if (input$sort_by == "state_of_location") {
    # Character sort
    df <- df |>
      mutate(
        state_of_location = factor(
          state_of_location,
          levels = sort(unique(state_of_location), decreasing = is_desc)
        )
      )
  } else {
    # Numeric sort
    df <- df |>
      mutate(
        state_of_location = fct_reorder(
          state_of_location,
          .data[[input$sort_by]],
          .desc = is_desc,
          .fun = sum
        )
      )
  }

  df
})

output$safety_plot <- renderPlot({
  df <- sorted_data()

  label_map <- c(
    "overall_safety_score" = "Overall Safety Score",
    "power_generated" = "Power Generated"
  )

  if (input$stat == "power_vs_safety") {
    ggplot(df, aes(
      x = state_of_location,
      y = overall_safety_score,
      fill = power_generated
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(
        low = "green",
        high = "red",
        name = "Power Generated (MWh)"
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
      theme_minimal(base_size = 12)

  } else if (input$stat == "safety_power_corr") {
    df <- df |>
      mutate(
        power_generated = as.numeric(gsub(",", "", as.character(power_generated))),
        overall_safety_score = as.numeric(overall_safety_score)
      ) |>
      filter(!is.na(power_generated), !is.na(overall_safety_score))

    if (nrow(df) < 2 || dplyr::n_distinct(df$power_generated) < 2) {
      return(
        ggplot(df, aes(x = power_generated, y = overall_safety_score)) +
          geom_point(color = "steelblue") +
          labs(
            title = paste("Safety vs Power -", input$date_select),
            subtitle = "Not enough data to fit a line",
            x = "Power Generated (MWh)",
            y = "Overall Safety Score"
          ) +
          theme_minimal(base_size = 12)
      )
    }

  } else if (input$stat == "corr_trend") {
    ggplot(cor_by_date, aes(x = date, y = correlation) 
      geom_hline(yintercept = 0, color = "grey70") +
      geom_line(linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_manual(
        values = c(positive = "steelblue", negative = "firebrick", zero = "grey40")
      ) +
      labs(
        title = "Monthly correlation: Safety vs Power",
        subtitle = "Correlation computed across sites within each month",
        x = NULL,
        y = "Correlation"
      ) +
      theme_minimal(base_size = 12)

  } else {
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
      theme_minimal(base_size = 12)
  }
})

}

shinyApp(ui, server)
#
#
#
#
#
