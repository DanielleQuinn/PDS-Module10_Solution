# ---- Module 10 Assignment Solution ----
# Load packages and import data
library(shiny)
library(dplyr)
library(ggplot2)
income <- read.csv("income.csv")

# ---- Assignment Code: ui object ----
# Define the ui object
ui <- fluidPage(
  # App Title
  titlePanel("Module 10 Assignment"),
  # Page Layout
  sidebarLayout(
    # Side Panel
    sidebarPanel(
      # Interactive piece 1: inputID = "subset_income"
      selectInput(inputId = "subset_income", label = "Select Income Type:", 
                  choices = c("<=50K", ">50K")),
      # Interactive piece 2: inputId = "set_yaxis"
      selectInput(inputId = "set_yaxis", label = "Set y-axis to:", 
                  choices = c("hours_per_week", "capital_loss")),
      # Interactive piece 3: inputId = "subset_occupation"
      checkboxGroupInput(inputId = "subset_occupation", label = "Include Occupations:", 
                         choices = unique(income$occupation),
                         selected = unique(income$occupation))),
    # Main panel
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

# ---- Instructions: server function ----
# 1. Subset the income data to only include records where:
# - capital loss is greater than 0
# - income category is the input selected above ("subset_income"), and 
# - occupation is in the input selected above ("subset_occupation")

# 2. Render a boxplot with occupation on the x axis and the input selected above ("set_yaxis") on the y axis

# ---- Assignment Code: server function ----
# Define server function
server <- function(input, output) {
  # Create a reactive subset of the data
  create_subset <- reactive(income %>%
                              filter(capital_loss > 0 &
                                       income == input$subset_income &
                                       occupation %in% input$subset_occupation))
  
  # Render Plot
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                               # Boxplot of x = occupation, y = defined by input
                               geom_boxplot(aes_string(x = "occupation", y = input$set_yaxis)) +
                               theme_bw(18) +
                               theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

# ---- Run App ----
shinyApp(ui, server)