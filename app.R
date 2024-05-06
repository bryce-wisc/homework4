library(tidyverse)
library(dplyr)
library(ggrepel)
library(shiny)
library(tidymodels)
library(tidytext)
library(plotly)
library(viridis)
library(stats)
library(RColorBrewer)
library(colorspace)
data = read_csv("https://raw.githubusercontent.com/bryce-wisc/homework4/main/nba_team_stats_00_to_23.csv")
columns_to_exclude <- c("win_percentage", "teamstatspk", "Min","games_played")
data <- data %>%
  select(-one_of(columns_to_exclude))
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
data_filter <- select(data, where(is.numeric))
data_filter_s <- scale(data_filter)

pca_recipe = recipe(~ ., data = data_filter_s) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_predictors(), num_comp = 5)

pca_prep = prep(pca_recipe)

pca_results = tidy(pca_prep, 2)

ui = fluidPage(
  titlePanel("NBA Wins PCA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "X-Axis PC:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5")),
      selectInput("y_axis", "Y-Axis PC:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC2"),
      selectInput("z_axis", "Z Axis PC:", choices = c("PC1", "PC2", "PC3", "PC4", "PC5"), selected = "PC3"),
    ),
    mainPanel(
      plotlyOutput("pca_plot"),
      plotOutput("PCA", height = "800px"),
    )
  )
)


server = function(input, output) {
  
  output$PCA <- renderPlot({
    # Filter PCA results for the top 5 principal components
    pca_top5 <- pca_results %>%
      filter(component %in% paste0("PC", 1:5))
    
    # Plot PCA results using ggplot
    ggplot(pca_top5, aes(x = value, y = terms, fill = component)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ component, scales = "free_y", nrow = 3) +
      scale_fill_viridis_d(option = "C", direction = -1) + 
      labs(x = "Principal Component Value", y = NULL) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 7, hjust = 0)) 
  })
  output$pca_plot = renderPlotly({
    pca_top5 = pca_results %>%
      filter(component %in% c(input$x_axis, input$y_axis, input$z_axis))
    pca_plot_data = pca_top5 %>%
      pivot_wider(names_from = component, values_from = value)
    plot_ly(data = pca_plot_data, x = ~get(input$x_axis), y = ~get(input$y_axis),z = ~get(input$z_axis),color = ~terms) %>%
      add_markers() %>%
      layout(xaxis = list(title = input$x_axis), yaxis = list(title = input$y_axis), zaxis = list(title = input$z_axis))
  })
}

shinyApp(ui, server)