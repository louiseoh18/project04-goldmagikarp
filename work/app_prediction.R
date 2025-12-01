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
  stop("Data files not found")
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
      width = 3,
      h4("Economic Scenarios (2026-2027)"),
      p(""),
      hr(),
      
      sliderInput("inflation", "Inflation Rate:", min = 0, max = 25, value = 3.0, step = 0.1),
      sliderInput("unemployment", "Unemployment Rate:", min = 3, max = 20, value = 4.5, step = 0.1),
      sliderInput("gdp", "GDP Growth:", min = -10, max = 10, value = 2.0, step = 0.1),
      
      hr(),
      h5("Secondary Indicators"),
      sliderInput("wages", "Wage Growth (%):", min = 0, max = 8, value = 3.5, step = 0.1),
      sliderInput("healthcare", "Healthcare Inflation (%):", min = 0, max = 15, value = 6.0, step = 0.5),
      sliderInput("jobs", "Job Growth (%):", min = -2, max = 5, value = 1.0, step = 0.1),
      sliderInput("market", "Corp. Profit Growth (%):", min = -10, max = 20, value = 4.0, step = 1),
      
      hr(),
      actionButton("reset", "Reset to Baseline", class = "btn-secondary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      plotOutput("forecastPlot", height = "500px"),
      br(),
      fluidRow(
        column(4, 
               div(class = "alert alert-info",
                   h4("Predicted Approval (Dec 2027)"),
                   h2(textOutput("final_score"))
               )
        ),
        column(8,
               wellPanel(
                 h5("Model Logic:"),
                 p("This forecast uses a **Recursive Random Forest**. It predicts next month's approval based on:"),
                 tags$ul(
                   tags$li("The economic settings you chose on the left."),
                   tags$li("The 'Inertia' of the previous 3 months (Approval Lag)."),
                   tags$li("The Time in Office.")
                 ),
                 p("The forecast is automatically anchored to the last actual poll to ensure a seamless visual transition from history.")
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSliderInput(session, "inflation", value = 3.0)
    updateSliderInput(session, "unemployment", value = 4.5)
    updateSliderInput(session, "gdp", value = 2.0)
    updateSliderInput(session, "wages", value = 3.5)
    updateSliderInput(session, "healthcare", value = 6.0)
    updateSliderInput(session, "jobs", value = 1.0)
    updateSliderInput(session, "market", value = 4.0)
  })
  
  forecast_data <- reactive({
    future_dates <- seq(start_date + months(1), as.Date("2027-12-01"), by = "month")
    n_months <- length(future_dates)
    
    df <- data.frame(
      date = future_dates,
      president = "Trump (2nd Term)", 
      
      Party = "Republican",
      Year_Index = year(future_dates),
  
      Months_in_Office = seq(11, 11 + n_months - 1),
      Honeymoon = 0,
      
      Unemployment      = input$unemployment,
      Inflation_Rate    = input$inflation,
      GDP_Growth        = input$gdp,
      Wage_Growth       = input$wages,
      Healthcare_Infl   = input$healthcare,
      Job_Growth        = input$jobs,
      Profit_Growth     = input$market,
      
      Income_Growth     = 2.0, 
      Gov_Spend_Growth  = 1.0,
      Savings_Rate      = 4.0,
      
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
    
    ggplot(plot_df, aes(x = date, y = approval_rating, color = Type, linetype = Type)) +
      geom_line(size = 1.5) +
      geom_point(size = 3, alpha = 0.8) +
      
      geom_vline(xintercept = start_date, linetype = "dotted", color = "gray50") +
      
      scale_color_manual(values = c("Actual History" = "black", "Forecast" = "#D55E00")) +
      scale_linetype_manual(values = c("Actual History" = "solid", "Forecast" = "dashed")) +
      
      labs(y = "Approval Rating (%)", x = "Year") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(25, 60))
  })
  
  output$final_score <- renderText({
    val <- tail(forecast_data()$approval_rating, 1)
    paste0(round(val, 1), "%")
  })
}

shinyApp(ui = ui, server = server)