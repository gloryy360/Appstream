#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/gadek/OneDrive/Bureau/M1 IFRI/iris_dataset/a08a1080b88344b0c8a7-0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv")

ui <- fluidPage(
  titlePanel("Exploration de la base de données Iris"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Options de visualisation"),
      checkboxGroupInput("columns", "Choisissez les colonnes à afficher",
                         choices = colnames(data), selected = colnames(data)),
      selectInput("chart_type", "Choisissez le type de graphique",
                  choices = c("Histogramme", "Pairplot", "Boxplot")),
      conditionalPanel(
        condition = "input.chart_type == 'Histogramme'",
        selectInput("hist_column", "Choisissez une colonne",
                    choices = names(data)[sapply(data, is.numeric)])
      ),
      conditionalPanel(
        condition = "input.chart_type == 'Boxplot'",
        selectInput("box_column", "Choisissez une colonne",
                    choices = names(data)[sapply(data, is.numeric)])
      ),
      checkboxGroupInput("species_filter", "Choisissez les espèces à afficher",
                         choices = unique(data$species), selected = unique(data$species))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Aperçu des données", tableOutput("data_table")),
        tabPanel("Visualisation", plotOutput("plot")),
        tabPanel("Données filtrées", tableOutput("filtered_data"))
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  
  output$data_table <- renderTable({
    data %>% select(all_of(input$columns))
  })
  
  output$plot <- renderPlot({
    req(input$chart_type)
    
    if (input$chart_type == "Histogramme") {
      ggplot(data, aes_string(input$hist_column)) +
        geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
        theme_minimal()
    } else if (input$chart_type == "Pairplot") {
      pairs(data[, sapply(data, is.numeric)], col = as.factor(data$species))
    } else if (input$chart_type == "Boxplot") {
      ggplot(data, aes(x = species, y = .data[[input$box_column]], fill = species)) +
        geom_boxplot() +
        theme_minimal()
    }
  })
  
  output$filtered_data <- renderTable({
    data %>% filter(species %in% input$species_filter)
  })
}


shinyApp(ui = ui, server = server)

