
library("dplyr")
library("shiny")

music <- read.csv("data/responses.csv", stringsAsFactors=FALSE)
rawData <- read.csv("data/responses.csv")
data <- select(rawData, 141, 145, 2, 133, 147, (3:19))

first_page <- tabPanel(
  "About us",
  mainPanel(
    tags$img(src = "Logo.jpg", height = 100, width = 100), 
    h3("App Introduction"), 
    tags$p("This application helps visualize the data of different factors that could possibly have correlation with the choice of various genre of music. These include: gender, education level, and time spends on internet everyday. In the second page, Table & Plots, there are some widgets that users could interact with to find out the elements that play a crucial role in users' music tastes."),
    tags$a(href="https://www.kaggle.com/boltmaud/musics-depending-on-demographic-data/data?select=rules.json", "Click here to view data source!"),
    h3("Group Member Description"),
    h5("Brandon Bu"),
    tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Dancing with your ghost"),
    h5("Mina Gao"),
    tags$p("Female, Education Level: college, Internet usage: most of the day, Favorite song: Good Day - Surfaces"),
    h5("Zihan Lin"),
    tags$p("Male, Education Level: college, Internet usage: most of the day, Favorite song: Leave the door open (Jazz, pop)"),
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
                 plotOutput("genre_bar")),
        tabPanel("Pie",
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
  DT::dataTableOutput("table")
)

fourth_page <- tabPanel(
  "Summary",
  mainPanel(
    h3("Summary")
  )
)

ui <- navbarPage(title = span("Music Recommendation", style = "default_bg: #DEEBF7; color: black"),
                 first_page,
                 second_page,
                 third_page,
                 fourth_page
)