library(here)
library(shiny)
library(tidyverse)
library(lubridate)
load(here("data/complete_data.rda"))

approval_comparison_data <-
  final_combined_data |>
  drop_na(1:5) |>
  group_by(president) |>
  mutate(
    months_in_office = case_when(
      month(min(date)) == 1 ~ interval(min(date), date) %/% months(1),
      month(min(date)) == 2 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 1), # presi's missing first month
      month(min(date)) == 8 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 8), # Ford
      month(min(date)) == 6 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 4), # Truman
      month(min(date)) == 12 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 11), # LBJ
      month(min(date)) == 7 ~ interval(min(date), date) %/%
        months(1) +
        (month(min(date)) - 1), # FDR 3rd term
    ),
    president = case_when(
      str_detect(president, "Trump") ~ "Donald Trump",
      TRUE ~ president
    ),
    months_in_office = case_when(
      date < "2025-01-01" ~ months_in_office,
      president == "Donald Trump" & date >= "2025-01-01" ~ months_in_office +
        49,
      president == "Joe Biden" & date == "2025-01-01" ~ 48
    )
  ) |>
  select(1:5, 17)

# notes to add:
# FDR only has approval data for his third term
# Trump combined (added a month)
# going based off day 01

# UI
ui <- fluidPage(
  titlePanel("Comparison of U.S. Presidential Approval Ratings"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "president",
        label = "Choose Presidents",
        choices = replace(
          rev(unique(approval_comparison_data$president)),
          c(1, 2),
          rev(unique(approval_comparison_data$president))[c(2, 1)]
        ),
        selected = c("Donald Trump", "Joe Biden"),
        multiple = TRUE,
        selectize = FALSE
      )
    ),

    mainPanel(plotOutput("approval_plot"))
  )
)

# Server
server <- function(input, output, session) {
  # Filter data by selected president
  pres_data <- reactive({
    approval_comparison_data |> filter(president %in% input$president)
  })

  # Approval rating plot
  output$approval_plot <- renderPlot({
    pres_data() |>
      ggplot(aes(x = months_in_office, linetype = president)) +
      geom_line(aes(y = approval_rating, color = "Approval"), size = 1) +
      geom_line(aes(y = disapproval_rating, color = "Disapproval"), size = 1) +
      geom_line(aes(y = unsure_rating, color = "Unsure"), size = 1) +
      scale_color_manual(values = c("#61D04F", "#DF536B", "#2297E6")) +
      scale_linetype_manual(
        values = c(
          "solid",
          "dashed",
          "dotted",
          "dotdash",
          "longdash",
          "twodash",
          "12",
          "21",
          "13",
          "31",
          "14",
          "41",
          "1122",
          "2211",
          "F2"
        )
      ) +
      labs(y = "Rating (%)", x = "Time in Office (months)", color = "Legend") +
      theme_minimal() +
      ggtitle("Approval Ratings for Presidents") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

# Run app
shinyApp(ui, server)
