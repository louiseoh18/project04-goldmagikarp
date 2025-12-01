library(shiny)
library(tidyverse)
library(lubridate)
load(here("data/complete_data.rda"))

econ_labels <- c(
  "Unemployment Rate" = "unemployment_rate",
  "Consumer Price Index" = "consumer_p_index",
  "Total Jobs" = "total_nonfarm_jobs",
  "Average Hourly Earnings" = "avg_hourly_earnings",
  "Real GDP" = "Real_GDP",
  "Disposable Income" = "Disposable_Income",
  "Corporate Profits" = "Corporate_Profits",
  "Healthcare Price Index" = "Healthcare_Price_Index",
  "Government Spending" = "Gov_Spending",
  "Personal Savings Rate" = "Personal_Saving_Rate"
)

# UI
ui <- fluidPage(
  titlePanel("U.S. Presidential Approval Ratings"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "president",
        label = "Choose a President:",
        choices = unique(final_combined_data$president),
        selected = "Franklin D. Roosevelt"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Approval Ratings",
                 plotOutput("approval_plot")
        ),
        tabPanel("Economic Indicators",
                 selectInput(
                   inputId = "econ_var",
                   label = "Select Economic Indicator:",
                   choices = econ_labels
                 ),
                 plotOutput("econ_plot") 
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
      geom_line(aes(y = approval_rating, color = "Approval")) +
      geom_line(aes(y = disapproval_rating, color = "Disapproval")) +
      geom_line(aes(y = unsure_rating, color = "Unsure")) +
      labs(y = "Rating (%)", x = "Date", color = "Legend") +
      theme_minimal()
  })
  
  # Economic indicator plot
  output$econ_plot <- renderPlot({
    var <- input$econ_var
    friendly_label <- names(econ_labels)[econ_labels == var]
    pres_data() |>
      ggplot(aes(x = date, y = .data[[var]])) +
      geom_line(color = "steelblue") +
      labs(y = friendly_label, x = "Date") +
      theme_minimal()
  })
}

# Run app
shinyApp(ui, server)
