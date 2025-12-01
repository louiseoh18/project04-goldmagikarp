
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(here)
library(zoo)

tryCatch({
  rf_model <- readRDS(here("data/rf_approval_model.rds"))
  trump_history <- readRDS(here("data/trump_history.rds"))
}, error = function(e) {
  stop()
})

last_actual_row <- tail(trump_history, 1)
start_date <- last_actual_row$date
start_approval <- last_actual_row$approval_rating

last_3_approvals <- tail(trump_history$approval_rating, 3)
if(length(last_3_approvals) < 3) {
  last_3_approvals <- rep(start_approval, 3)
}

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Presidential Approval Forecaster: Trump 2nd Term"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4, 
      h4("Economic Trajectory (2026-2027)"),
      p("Define the start and end points. The model simulates the trend between them."),
      hr(),
      
      h5("1. Unemployment Rate (%)"),
      splitLayout(
        numericInput("unemp_start", "Start (Jan '26)", value = 4.5, step = 0.1),
        numericInput("unemp_end", "End (Dec '27)", value = 5.0, step = 0.1)
      ),
      
      h5("2. Inflation Rate (YoY %)"),
      splitLayout(
        numericInput("inf_start", "Start", value = 3.0, step = 0.1),
        numericInput("inf_end", "End", value = 2.5, step = 0.1)
      ),
      
      h5("3. GDP Growth (YoY %)"),
      splitLayout(
        numericInput("gdp_start", "Start", value = 2.5, step = 0.1),
        numericInput("gdp_end", "End", value = 1.8, step = 0.1)
      ),
      
      hr(),
      
      h5("Secondary Factors (Avg)"),
      sliderInput("wages", "Wage Growth (%):", min = 0, max = 6, value = 3.5, step = 0.1),
      sliderInput("healthcare", "Healthcare Inflation (%):", min = 0, max = 10, value = 6.0, step = 0.5),
      sliderInput("jobs", "Job Growth (%):", min = -2, max = 4, value = 1.0, step = 0.1),
      sliderInput("market", "Corp. Profit Growth (%):", min = -10, max = 15, value = 4.0, step = 1),
      
      hr(),
      actionButton("reset", "Reset Scenarios", class = "btn-secondary", width = "100%")
    ),
    
    mainPanel(
      width = 8,
      plotOutput("forecastPlot", height = "500px"),
      br(),
      fluidRow(
        column(6,
               div(class = "alert alert-info",
                   h4("Forecasted Approval (Dec 2027)"),
                   h1(textOutput("final_score")),
                   span(textOutput("change_score"), style = "font-size: 1.2em; color: gray;")
               )
        ),
        column(6,
               wellPanel(
                 h5("How this works:"),
                 tags$ul(
                   tags$li("The model linearly interpolates the economic values between your Start and End points."),
                   tags$li("It recursively predicts approval month-by-month, carrying 'inertia' forward."),
                   tags$li("The result shows how Trump's popularity might evolve if the economy cools down vs. heats up.")
                 )
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateNumericInput(session, "unemp_start", value = 4.5)
    updateNumericInput(session, "unemp_end", value = 5.0)
    updateNumericInput(session, "inf_start", value = 3.0)
    updateNumericInput(session, "inf_end", value = 2.5)
    updateNumericInput(session, "gdp_start", value = 2.5)
    updateNumericInput(session, "gdp_end", value = 1.8)
  })
  
  forecast_data <- reactive({
    future_dates <- seq(start_date + months(1), as.Date("2027-12-01"), by = "month")
    n_months <- length(future_dates)
    
    unemp_seq <- seq(input$unemp_start, input$unemp_end, length.out = n_months)
    inf_seq   <- seq(input$inf_start, input$inf_end, length.out = n_months)
    gdp_seq   <- seq(input$gdp_start, input$gdp_end, length.out = n_months)
    
    df <- data.frame(
      date = future_dates,
      president = "Trump (2nd Term)", 
      Party = "Republican",
      Year_Index = year(future_dates),
      Months_in_Office = seq(11, 11 + n_months - 1),
      Honeymoon = 0,
      
      Unemployment      = unemp_seq,
      Inflation_Rate    = inf_seq,
      GDP_Growth        = gdp_seq,
      
      Wage_Growth       = rep(input$wages, n_months),
      Healthcare_Infl   = rep(input$healthcare, n_months),
      Job_Growth        = rep(input$jobs, n_months),
      Profit_Growth     = rep(input$market, n_months),
      
      Income_Growth     = rep(2.0, n_months), 
      Gov_Spend_Growth  = rep(1.0, n_months),
      Savings_Rate      = rep(4.0, n_months),
      
      Approval_Lag3     = NA,
      approval_rating   = NA
    )
    
    approval_buffer <- last_3_approvals
    
    for(i in 1:n_months) {
      df$Approval_Lag3[i] <- approval_buffer[1]
      pred_val <- predict(rf_model, newdata = df[i, ])
      df$approval_rating[i] <- pred_val
      approval_buffer <- c(approval_buffer[-1], pred_val)
    }
    
    first_pred <- df$approval_rating[1]
    anchor_gap <- start_approval - first_pred
    df$approval_rating <- df$approval_rating + anchor_gap
    
    df$Type <- "Forecast"
    return(df)
  })
  
  output$forecastPlot <- renderPlot({
    fc <- forecast_data()
    hist_plot <- trump_history %>% mutate(Type = "Actual History")
    bridge <- last_actual_row
    bridge$Type <- "Forecast" 
    
    plot_df <- bind_rows(hist_plot, bridge, fc %>% select(date, approval_rating, Type))
    
    ggplot(plot_df, aes(x = date, y = approval_rating, color = Type)) +
      geom_line(size = 1.5) +
      geom_point(data = filter(plot_df, Type == "Actual History"), size = 3, alpha = 0.8) +
      
      geom_vline(xintercept = start_date, linetype = "dotted", color = "gray50") +
      
      scale_color_manual(values = c("Actual History" = "black", "Forecast" = "#D55E00")) +

      labs(y = "Approval Rating (%)", x = "Year") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(25, 60))
  })
  
  output$final_score <- renderText({
    val <- tail(forecast_data()$approval_rating, 1)
    paste0(round(val, 1), "%")
  })
  
  output$change_score <- renderText({
    start_val <- start_approval
    end_val <- tail(forecast_data()$approval_rating, 1)
    diff <- end_val - start_val
    sign <- ifelse(diff >= 0, "+", "")
    paste0("(", sign, round(diff, 1), "% from today)")
  })
}

shinyApp(ui = ui, server = server)