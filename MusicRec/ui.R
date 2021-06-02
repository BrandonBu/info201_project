
library("dplyr")
library("shiny")

first_page <- tabPanel(
    "About us",
        mainPanel(
            tags$img(src = "Logo.jpg", height = 100, width = 100), 
            h3("App Introduction"), 
            tags$p("This application help visualize the data of different factors that 
                   could possibly have correlation with the choice of various genre of music. "),
            tags$a(href="https://www.kaggle.com/boltmaud/musics-depending-on-demographic-data/data?select=rules.json", "Click here to view data source!")
        )
)

second_page <- tabPanel(
    "Table",
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "area_ind",
                label = "please adjust the area of county",
                min = range(midwest$area)[1],
                max = range(midwest$area)[2],
                value = range(midwest$area)[1]
            ),
            radioButtons(
                inputId = "state_ind",
                label = "please select a state",
                choices = unique(midwest$state)
            )
        ),
        
        mainPanel(
            h3("Population of Different Indices Based on State"),
            plotOutput(outputId = "pop_forth")
        )
    )
)

third_page <- tabPanel(
    "Summary",
        mainPanel(
            h3("Summary"),
        )
    
)

ui <- navbarPage(title = span("Music Recommendation", style = "default_bg: #DEEBF7; color: black"),
    first_page,
    second_page,
    third_page
)