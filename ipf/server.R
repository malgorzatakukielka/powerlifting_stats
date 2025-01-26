library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
observeEvent(input$sex, {
  #pull weightclasses depending on a gender
  weightclass_filtered <- ipf %>% 
    filter(Sex == input$sex) %>% 
    pull(WeightClass) %>% 
    unique() %>% 
    sort()
  
  #sort as numeric (implemented to make male categories >100kg not appear on top of the list)
  
  numeric_weight_class <- gsub(" kg|\\+", "", weightclass_filtered)
  numeric_weight_class <- as.numeric(numeric_weight_class)
  sorted_indices <- order(numeric_weight_class)
  sorted_weight_class_values <- weightclass_filtered[sorted_indices]
  
  updatePickerInput(session, inputId = "weightclass", 
                    choices = list("None selected" = "",
                                   "Weight Class" = sorted_weight_class_values))
})
 
  
  output$summary <- renderDataTable({
    # Sprawdzamy, czy płeć została wybrana
    if (input$sex == "") {
      return(data.frame(
        Message = "Please select a gender to display the data."
      ))
    }
    
    # Filtrujemy dane w zależności od wartości wejściowych
    filtered_data <- ipf %>%
      filter(
        Sex == input$sex,  # Teraz wymaga wyboru płci
        (input$weightclass == "" | WeightClass == input$weightclass),
        (input$Ageclass == "" | AgeClass == input$Ageclass)
      ) %>%
      group_by(Name, WeightClass, AgeClass) %>%  # Grupujemy po zawodniku, kat. wagowej i wiekowej
      slice_max(TotalKg, n = 1) %>%  # Pobieramy najwyższy wynik TotalKg
      arrange(desc(TotalKg)) %>%
      select("Athlete" = Name, "Weight Class" = WeightClass, 
             "Age Class" = AgeClass, "Best Total" = TotalKg) %>%
      head(5)
    
    datatable(
      filtered_data,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        ordering = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$summary2 <- renderDataTable({
    # Sprawdzamy, czy płeć została wybrana
    if (input$sex == "") {
      return(data.frame(
        Message = "Please select a gender to display the data."
      ))
    }
    
    # Filtrujemy dane w zależności od wartości wejściowych
    filtered_data <- ipf %>%
      filter(
        Sex == input$sex,  # Teraz wymaga wyboru płci
        (input$weightclass == "" | WeightClass == input$weightclass),
        (input$Ageclass == "" | AgeClass == input$Ageclass)
      ) %>%
      group_by(Name, WeightClass, AgeClass) %>%  # Grupujemy po zawodniku, kat. wagowej i wiekowej
      slice_max(Goodlift, n = 1) %>%  # Pobieramy najwyższy wynik Goodlift
      arrange(desc(Goodlift)) %>%
      select("Athlete" = Name, "Weight Class" = WeightClass, 
             "Age Class" = AgeClass, "Best GL Score" = Goodlift, 
             "Best Total" = TotalKg) %>%
      head(5)
    
    datatable(
      filtered_data,
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        ordering = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  output$weightclass_dist <- renderPlotly({
    if (is.null(input$sex) || input$sex == "") {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "Please select a gender to view the plot.",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
    
    # Filter based on gender
    filtered_data_weight <- ipf %>%
      filter(Sex == input$sex, 
             input$Ageclass == "" | AgeClass == input$Ageclass)
    
    # plot
    plot1 <- filtered_data_weight %>%
      ggplot(aes(x = factor(WeightClass, levels = sorted_weight_class_values))) +
      geom_bar(fill = "steelblue") +
      labs(
        x = "Weight Class",
        y = "Count",
        title = paste(
          "Weight Class Distribution for", input$sex, "athletes",
          if (input$Ageclass != "") paste("in Age Class:", input$Ageclass) 
          else "in all Age Classes")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # to plotly
    ggplotly(plot1)
  })
  
  output$ageclass_dist <- renderPlotly({
    if (is.null(input$sex) || input$sex == "") {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "Please select a gender to view the plot.",
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
    
    # Filter based on gender
    filtered_data_age <- ipf %>%
      filter(Sex == input$sex, 
             input$weightclass == "" | WeightClass == input$weightclass)
    
    # plot
    plot2 <- filtered_data_age %>%
      ggplot(aes(x = factor(AgeClass))) +
      geom_bar(fill = "steelblue") +
      labs(
        x = "Age Class",
        y = "Count",
        title = paste(
          "Age Class Distribution for", input$sex, "athletes",
          if (input$weightclass != "") paste("in Weight Class:", input$weightclass) 
          else "in all Weight Classes")
        ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # to plotly
    ggplotly(plot2)
    
  })

}
