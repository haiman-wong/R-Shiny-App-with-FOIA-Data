library(ranger)
library(caret)
library(data.table)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

#Link: https://www.kaggle.com/doj/foia-requests/metadata

setwd("/Users/haimanwong/Desktop")
read_csv("requests.csv") -> data
data

#Data Prelim Exploration
nrow(data)
ncol(data)

View(data)

#Cleaning and tidying the data (pre-processing):
data %>%
  drop_na() -> data_tidied
data_tidied

View(data_tidied)

#Data Exploration
dim(data_tidied)
summary(data_tidied)

#Shiny App 
ui <- fluidPage(
  tags$h2("STAT-613 Data Science: Shiny App Final Project", style = "color:brown"),
  h3("By: Haiman Wong", style = "color:brown"),
  h4("August 10th, 2021", style = "color:brown"),
  setBackgroundColor(
    color = c("antiquewhite", "darksalmon"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  titlePanel("FOIA Data"),
  selectInput("var1", "Variable 1", choices = names(data_tidied)),
  selectInput("var2", "Variable 2", choices = names(data_tidied)),
  plotOutput(outputId = "scatterplot"),
  checkboxGroupInput("vars3", "Which FOIA data variable combinations yield the scatterplots with the best fitted lines?", choices = data_tidied),
  plotOutput(outputId = "histogram"),
  plotOutput(outputId = "histogram2"),
  checkboxGroupInput("vars4", "For which FOIA data variables is it possible to create a histogram for?", choices = data_tidied),
  textOutput("text"),
  verbatimTextOutput("code"),
  plotOutput(outputId = "boxplot"),
  checkboxGroupInput("vars5", "Which FOIA data variables yield the boxplots that best represents the dataset?", choices = data_tidied),
  plotOutput(outputId = "boxplot2"),
  checkboxGroupInput("vars6", "When x = Agency, which FOIA data variables yield the most informative side-by-side boxplots?", choices = data_tidied),
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data_tidied, aes(x = .data[[input$var1]], y = .data[[input$var2]])) +
      geom_point(color = "saddlebrown") +
      geom_smooth(method = lm, color = "salmon1", se = FALSE) +
      ggtitle("FOIA Scatter Plot")
  })
  output$histogram <- renderPlot({
    ggplot(data_tidied, aes(x = .data[[input$var1]])) +
      geom_histogram(fill = "indianred3", color = "indianred4") +
      ggtitle("FOIA Histogram 1")
  })
  output$histogram2 <- renderPlot({
    ggplot(data_tidied, aes(x = .data[[input$var2]])) +
      geom_histogram(fill = "indianred2", color = "indianred4") +
      ggtitle("FOIA Histogram 2")
  })
  output$text <- renderText({
    "FOIA summaries"
  })
  output$code <- renderPrint({
    summary(data_tidied)
  })
  output$boxplot <- renderPlot({
    ggplot(data_tidied, mapping = aes(x =.data[[input$var1]])) +
      geom_boxplot(fill = "indianred") +
      ggtitle("FOIA Boxplot 1")
  })
  output$boxplot2 <- renderPlot({
    ggplot(data_tidied, mapping = aes(x = Agency, y =.data[[input$var2]])) +
      geom_boxplot(fill = "indianred") +
      ggtitle("FOIA Boxplot 2")
  })
}
shinyApp(ui = ui, server = server)

