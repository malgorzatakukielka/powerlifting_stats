library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)

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
                   sidebarMenu(
                  # Navigation Section Header
                   h4("Navigation"),
                   menuItem("Athletes statistics", tabName = "athletes", icon = icon("medal")),
                   menuItem("Performance Analysis", tabName = "performance_analysis", icon = icon("dumbbell"))),
    # Selection Section Header
    h4("Selection"),

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
    ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    ## First window ##
    tabItems(
      tabItem(
        tabName = "athletes",
    fluidRow(
      box(
        title = "Summary Table - Best Total", width = 6,
        dataTableOutput("summary")),
        box(
          title = "Summary Table - Best GL Score", width = 6,
          dataTableOutput("summary2")
    )
  ),
  fluidRow(
    box(
      title = "Weight Class Distribution", width = 6,
      plotlyOutput("weightclass_dist")),
    box(
      title = "Age Class Distribution", width = 6,
      plotlyOutput("ageclass_dist"))
    )
    ),
  
  ## Second Window ##
  tabItem(
    tabName = "performance_analysis",
    fluidRow(
      box(
        title = "Title - TBC",
        width = 8, height = "600px",
        plotlyOutput("boxplots",
                     height = "550px")
        ),
      box(
        title = "Title - TBC",
        width = 4, height = "600px",
        plotlyOutput("boxplot2",
                     height = "550px")
      )
      ))
  )
  )
)
