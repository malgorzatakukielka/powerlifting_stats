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
  
  observeEvent(input$weightclass, {
    print(input$weightclass)
  })
 
  
  output$summary <- renderDataTable({
    # Filtrujemy dane w zależności od wartości wejściowych
    filtered_data <- ipf %>%
      filter(
        # Płeć: Jeśli input$sex jest pusty, to nie filtrujemy po płci
        (input$sex == "" | Sex == input$sex),
        (input$weightclass == "" | WeightClass == input$weightclass),
        (input$Ageclass == "" | AgeClass == input$Ageclass)
      ) %>% 
      group_by(Name, WeightClass, AgeClass) %>%  # Grupowanie po zawodniku, kat. wagowej i wiekowej
      slice_max(TotalKg, n = 1) %>%  # Pobieranie najlepszego wyniku (max TotalKg) dla każdej grupy
      arrange(desc(TotalKg)) %>%  # Sortowanie po najlepszym wyniku
      select("Athlete" = Name, "Weight Class" = WeightClass, 
             "Age Class" = AgeClass, "Best Total" = TotalKg) %>%
      head(5)  # Zwrócenie tylko 5 najlepszych rekordów
    
    # Zwracamy przefiltrowane dane
   print(filtered_data)
  })

}
