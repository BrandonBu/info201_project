library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")

music <- read.csv("data/responses.csv", stringsAsFactors=FALSE)
rawData <- read.csv("data/responses.csv")
data <- select(rawData, 141, 145, 2, 133, 147, (3:19))

first_page <- tabPanel(
  "About us",
  mainPanel(
    tags$span(img(src = "Logo.jpg", height = 100, width = 100), tags$span(tags$h3("App Introduction"))), 
    tags$p("This application helps visualize the data of different factors that could possibly have correlation with the choice of various genre of music. These include: gender, education level, and time spends on internet everyday. In the second page, Table & Plots, there are some widgets that users could interact with to find out the elements that play a crucial role in users' music tastes."),
    tags$a(href="https://www.kaggle.com/boltmaud/musics-depending-on-demographic-data/data?select=rules.json", "Click here to view data source!"),
    h3("Group Member Description"),
    tags$span(img(src = "song1.png", height = 200, width = 150), tags$span(h5("Brandon Bu")), tags$span(tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Dancing with your ghost"))),
    tags$img(src = "song2.png", height = 200, width = 150),
    h5("Mina Gao"),
    tags$p("Female, Education Level: college, Internet usage: most of the day, Favorite song: Good Day - Surfaces"),
    tags$img(src = "song3.png", height = 200, width = 150),
    h5("Zihan Lin"),
    tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Leave the door open (Jazz, pop)"),
    tags$img(src = "song4.png", height = 200, width = 150),
    h5("Vivian Yu"),
    tags$p("Female, Education Level: college, Internet usage: most of the day, Favorite song: 2002, If I can't have you, 10000 hours... (basically pop songs, bossa nova, Jazz)"),
  )
)

second_page <- tabPanel(
  "Plot",
  sidebarLayout(
    sidebarPanel(
      uiOutput("Gender"),
      uiOutput("Education"),
      uiOutput("Internet")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Genre analysis",
                 textOutput("genre_description"),
                 plotOutput("genre_bar"),
                 htmlOutput("bar_message")),
        tabPanel("Pie",
                 textOutput("pie_description"),
                 plotOutput("music_pie"),
                 textOutput("pie_message"))
      )
    )
  )
)

third_page <- tabPanel(
  "Table",
  fluidRow(
    column(4,
           selectInput("age",
                       "Age:",
                       c("All",
                         unique(as.character(data$Age))))
    ),
    column(4,
           selectInput("gender",
                       "Gender:",
                       c("All",
                         unique(as.character(data$Gender))))
    ),
    column(4,
           selectInput("speed",
                       "Tempo preference(1-5):",
                       c("All",
                         unique(as.character(data$Slow.songs.or.fast.songs))))
    ),
    column(4,
           selectInput("internet",
                       "Hours spend on internet:",
                       c("All",
                         unique(as.character(data$Internet.usage))))
    ),
    column(4,
           selectInput("education",
                       "Education level:",
                       c("All",
                         unique(as.character(data$Education))))
    )
  ),
  textOutput("table_description"),
  DT::dataTableOutput("table"),
)

fourth_page <- tabPanel(
  "Summary",
  mainPanel(
    tags$img(src = "summary1.png", height = 400, width = 1200), 
    tags$img(src = "summary2.png", height = 400, width = 1200), 
    h6("According to the results from our table, we realized that regardless of the gender and internet usage, college students like rock music the most. As for our data quality, 
      the sample size of our data is about 1000 and that there are only a few missing data. However, there are some bias because the gender column only includes females and males.")
  )
)

ui <- navbarPage(title = span("Music Preference Analysis", style = "default_bg: #DEEBF7; color: black"),
                 first_page,
                 second_page,
                 third_page,
                 fourth_page
)