library(here)
library(shiny)
library(tidyverse)
library(lubridate)
load(here("data/complete_data.rda"))

final_combined_data |>
  select(1:5) |>
  drop_na() |>
  group_by(president) |>
  mutate(
    months_in_office = interval(min(date), date) %/% months(1)
  ) |>
  print(n = Inf)

# UI
ui <- fluidPage(
  titlePanel("U.S. Presidential Approval Ratings"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "president",
        label = "Choose a President:",
        choices = rev(unique(final_combined_data$president)[
          !is.na(unique(final_combined_data$president))
        ]),
        selected = "Trump (2nd Term)",
        selectize = FALSE
      )
    ),

    mainPanel(
      tabsetPanel(
        fluidRow("Approval Ratings", column(12, plotOutput("approval_plot"))),
        fluidRow(
          "Economic Indicators",
          selectInput(
            inputId = "econ_var",
            label = "Select Economic Indicator:",
            choices = econ_labels
          ),
          column(10, plotOutput("econ_plot"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Filter data by selected president
  pres_data <- reactive({
    final_combined_data |> filter(president == input$president)
  })

  # Approval rating plot
  output$approval_plot <- renderPlot({
    pres_data() |>
      ggplot(aes(x = date)) +
      geom_line(aes(y = approval_rating, color = "Approval"), size = 1) +
      geom_line(aes(y = disapproval_rating, color = "Disapproval"), size = 1) +
      geom_line(aes(y = unsure_rating, color = "Unsure"), size = 1) +
      scale_color_manual(values = c("#61D04F", "#DF536B", "#2297E6")) +
      labs(y = "Rating (%)", x = "Date", color = "Legend") +
      theme_minimal()
  })

  # Economic indicator plot
  output$econ_plot <- renderPlot({
    var <- input$econ_var
    friendly_label <- names(econ_labels)[econ_labels == var]
    pres_data() |>
      ggplot(aes(x = date, y = .data[[var]])) +
      geom_line(size = 1) +
      labs(y = friendly_label, x = "Date") +
      theme_minimal()
  })
}

# Run app
shinyApp(ui, server)
