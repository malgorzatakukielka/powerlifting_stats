library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

ipf <- read.csv("~/powerlifting_stats/ipf.csv")

ipf$Sex <- factor(ipf$Sex)
ipf$Event <- factor(ipf$Event)
ipf$Equipment <- factor(ipf$Equipment)
ipf$AgeClass <- factor(ipf$AgeClass)
ipf$WeightClassKg <- factor(ipf$WeightClassKg)


ipf$WeightClass <- factor(ipf$WeightClass)
# Unikalne wartości z WeightClass
weight_class_values <- unique(ipf$WeightClass)

# Usuwamy jednostki "kg" i " +" oraz tworzymy numeryczne wersje
numeric_weight_class <- gsub(" kg|\\+", "", weight_class_values)
numeric_weight_class <- as.numeric(numeric_weight_class)
sorted_indices <- order(numeric_weight_class)
sorted_weight_class_values <- weight_class_values[sorted_indices]
sorted_weight_class_values

# # Posortowanie WeightClass
sorted_weight_class <- sort(unique(ipf$WeightClass),
                            by = function(x) convert_weight_class(x))

dashboardPage(
  dashboardHeader(title = "Powerlifting Statistics", titleWidth = 300),
  dashboardSidebar(width = 300,
    radioButtons("sex", label = "Choose a gender:",
                 choices = unique(ipf$Sex),
                 selected = unique(ipf$Sex)[1]),
    pickerInput("weightclass", label = "Choose a Weight Class:",
                choices = list("None Selected" = "", 
                               "Weight Class" = sorted_weight_class_values)
                ),
    pickerInput("Ageclass", label = "Choose an age class:",
                choices =  list("None Selected" = "", 
                                 "AgeClass" = sort(unique(ipf$AgeClass))))
    
    # pickerInput("Equipment", label = "Choose an equipment category:",
    #             choices = list("None Selected" = "", 
    #                            "Equipment" = setNames(c("Raw", "Single-ply", "Wraps"), 
    #                                                   c("Raw: Bare knees or knee sleeves.", 
    #                                                     "Single-ply: Equipped, single-ply suits.", 
    #                                                     "Wraps: Knee wraps were allowed."))))
    ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Summary Table", width = 12,
        dataTableOutput("summary")
    )
  )
)
)