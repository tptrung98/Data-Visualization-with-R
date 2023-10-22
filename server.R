# Load libraries
library(shiny)
library(tidyverse)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(fill = "bisque",color = "black") +  # histogram geom
        labs(title = paste("Trend of", input$continuous_variable, "in", input$country), y = "Total of People") +  # labels
        facet_wrap(~prediction) +     # facet by prediction
        theme(plot.title = element_text(face="bold", color = "blue", size=18))
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot(fill = "bisque") +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(title = paste("Trend of", input$continuous_variable, "in", input$country)) +  # labels
        facet_wrap(~prediction) +  # facet by prediction
        theme(plot.title = element_text(face="bold", color = "blue", size=18))
    }
    
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      labs(title = paste("Trend of", input$categorical_variable, "in", input$country), y = "Total of People") +  # labels
      theme(axis.text.x = element_text(angle = 45), legend.position = "bottom", plot.title = element_text(face="bold", color = "blue", size=18)) +   # modify theme to change text angle and legend position
      scale_color_binned()
    
    if (input$is_stacked == TRUE) {
      p + geom_bar(aes(fill = prediction))  # add bar geom and use prediction as fill
    }
    else if (input$categorical_variable == "education") {
      p + geom_bar(aes(fill = education)) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)   # facet by prediction
    }
    else if (input$categorical_variable == "workclass") {
      p + geom_bar(aes(fill = workclass)) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)   # facet by prediction
    }
    else{
      p + geom_bar(aes(fill = sex)) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)   # facet by prediction
    }
  })
  
})