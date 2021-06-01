library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")

first_page <- tabPanel(
    "About us",
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "area_index",
                label = "please adjust the area of county",
                min = range(midwest$area)[1],
                max = range(midwest$area)[2],
                value = range(midwest$area)[1]
            ),
            radioButtons(
                inputId = "state_index",
                label = "please select a state",
                choices = unique(midwest$state)
            )
        ),
        
        mainPanel(
            h3("App Introduction"),
            img(src = "Logo.png", height = 80, width = 80),
            p("This application "),
            plotOutput(outputId = "pop_second")
        )
    )
)

second_page <- tabPanel(
    "Other Population Indices Info",
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

ui <- navbarPage(
    "Music Recommendation",
    first_page,
    second_page
)