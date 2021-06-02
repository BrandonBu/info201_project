library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")

music <- read.csv("data/responses.csv", stringsAsFactors=FALSE)

first_page <- tabPanel(
    "About us",
        mainPanel(
            tags$img(src = "Logo.jpg", height = 100, width = 100), 
            h3("App Introduction"), 
            tags$p("This application helps visualize the data of different factors that could possibly have correlation with the choice of various genre of music. These include: gender, education level, and time spends on internet everyday. In the second page, Table & Plots, there are some widgets that users could interact with to find out the elements that play a crucial role in users' music tastes."),
            tags$a(href="https://www.kaggle.com/boltmaud/musics-depending-on-demographic-data/data?select=rules.json", "Click here to view data source!")
        )
)

second_page <- tabPanel(
    "Table",
    sidebarLayout(
        sidebarPanel(
            uiOutput("gender"),
            uiOutput("education"),
            uiOutput("internet")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("music_pie"),
            plotOutput("genre_bar"),
            textOutput("pie_message")
        )
    )
)

third_page <- tabPanel(
    "Plot",
      
    
)

fourth_page <- tabPanel(
    "Summary",
    mainPanel(
        h3("Summary"),
    )
)

ui <- navbarPage(title = span("Music Recommendation", style = "default_bg: #DEEBF7; color: black"),
    first_page,
    second_page,
    third_page,
    fourth_page
)