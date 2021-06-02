library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)

music <- read.csv("data/responses.csv", stringsAsFactors=FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    music$interest <- ifelse(music$Music == 5, "Like", "Dislike")
    
    sample <- reactive({
        music %>%
            filter(Gender %in% input$Gender) %>%
            filter(Education %in% input$Education) %>%
            filter(Internet.usage %in% input$Internet)
    })
    
    output$music_pie <- renderPlot({
        data <- sample() %>%
            group_by(interest) %>%
            summarise(observation = n())
        if(nrow(data) != 0) {
            ggplot(data, aes(x = "", y = observation, fill = interest)) +
                geom_bar(stat='identity', width = 1) + 
                coord_polar("y", start=0) +
                theme_void()
        }
    })
    
    output$pie_description <- renderText({
        paste0("This is a pie chart that will tell you how people with the similar demograpic to you enjoy listening to music. 
               People choose 5 in music means they enjoy very much. If there are more people like music, then we say you are
               more likely to enjoy music.
               If you are master or currently primary, it is probable that there is not enough data to get the trend")
    })
    
    output$pie_message <- renderText({
        data <- sample() %>%
            group_by(interest) %>%
            summarise(observation = n())
        if(nrow(data) == 0) {
            paste0("There are not enough data")
        } else if(nrow(data) == 1) {
            if(data[1,1] == "Like") {
                paste0("People like you enjoy listening music")
            } else {
                paste0("People like you do not enjoy listening music")
            }
        } else if (data[1,2] < data[2,2]) {
            paste0("You are more likely enjoying listening music")
        } else {
            paste0("You are less likely enjoying listening music")
        }
    })
    
    
    output$genre_bar <- renderPlot({
        genre_data <- sample()
        
        dance <- genre_data$Dance
        folk <- genre_data$Folk
        country <- genre_data$Country
        classical <- genre_data$Classical.music
        musical <- genre_data$Musical
        pop <- genre_data$Pop
        rock <- genre_data$Rock
        metal <- genre_data$Metal.or.Hardrock
        punk <- genre_data$Punk
        hiphop <- genre_data$Hiphop..Rap
        reggae <- genre_data$Reggae..Ska
        jazz <- genre_data$Swing..Jazz
        rocknroll <- genre_data$Rock.n.roll
        alter <- genre_data$Alternative
        latino <- genre_data$Latino
        techno <- genre_data$Techno..Trance
        opera <- genre_data$Opera
        
        df <- data.frame(dance, folk, country, classical, musical, pop, rock, metal, punk, hiphop, reggae, 
                         jazz, rocknroll, alter, latino, techno, opera)
        
        genres <- c("dance", "folk", "country", "classical", "musical", "pop", "rock", "metal", "punk", "hiphop", 
                    "reggae", "jazz", "rocknroll", "alter", "latino", "techno", "opera")
        values <- c(sum(df$dance, na.rm = T), sum(df$folk, na.rm = T), sum(df$country, na.rm = T), sum(df$classical, na.rm = T), sum(df$musical, na.rm = T), 
                    sum(df$pop, na.rm = T), sum(df$rock, na.rm = T), sum(df$metal, na.rm = T), sum(df$punk, na.rm = T), sum(df$hiphop, na.rm = T), 
                    sum(df$reggae, na.rm = T), sum(df$jazz, na.rm = T), sum(df$rocknroll, na.rm = T), sum(df$alter, na.rm = T), sum(df$latino, na.rm = T), 
                    sum(df$techno, na.rm = T), sum(df$opera, na.rm = T))
        
        final_df <- data.frame(genres, values)
        
        ggplot(final_df, aes(x = genres, y = values)) + geom_bar(stat='identity', fill = "orange1")
    })
    
    output$Gender <- renderUI({
        radioButtons("Gender", label = "Gender",
                     choices = list("male", "female"),
                     selected = "male")
    })
    
    output$Education <- renderUI({
        selectInput("Education", label = "What is your Education level",
                    choices = as.list(unique(music$Education)),
                    selected = "secondary school")
    })
    
    output$Internet <- renderUI({
        selectInput("Internet", label = "How long do you spend on internet",
                    choices = as.list(unique(music$Internet.usage)),
                    selected = "few hours a day")
    })
    
    output$table_description <- renderText({
        paste0("The dataset contains the music preference of a group of people whose age between 15 - 30. 
          They rated 12 different music generals with their opinions; 1 stands for 'don't enjoy at all'
          and 5 stands for 'enjoy very much'. With the table, users can visualize the data for specific
          ages, genders, tempo preferences, hours spend on the internet, and education levels.")
    })
    
    
    output$table <- DT::renderDataTable(DT::datatable({
        rawData <- read.csv("data/responses.csv")
        table_data <- select(rawData, 141, 145, 2, 133, 147, (3:19))
        if (input$age != "All") {
            table_data <- table_data[table_data$Age == input$age,]
        }
        if (input$gender != "All") {
            table_data <- table_data[table_data$Gender == input$gender,]
        }
        if (input$speed != "All") {
            table_data <- table_data[table_data$Slow.songs.or.fast.songs == input$speed,]
        }
        if (input$internet != "All") {
            table_data <- table_data[table_data$Internet.usage == input$internet,]
        }
        if (input$education != "All") {
            table_data <- table_data[table_data$Education == input$education,]
        }
        table_data
    }))
})
