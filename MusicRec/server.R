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
            filter(Gender %in% input$gender) %>%
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
    
    output$gender <- renderUI({
        radioButtons("gender", label = "Gender",
                     choices = as.list(unique(music$Gender)),
                     selected = "male")
    })
    
    output$education <- renderUI({
        selectInput("Education", label = "What is your Education level",
                    choices = as.list(unique(music$Education)),
                    selected = "secondary school")
    })
    
    output$internet <- renderUI({
        selectInput("Internet", label = "How long do you spend on internet",
                    choices = as.list(unique(music$Internet.usage)),
                    selected = "few hours a day")
    })
    
    output$music_pie <- renderPlot({
        data <- sample() %>%
            group_by(interest) %>%
            summarise(observation = n())
        ggplot(data, aes(x = "", y = observation, fill = interest)) +
            geom_bar(stat='identity', width = 1) + 
            coord_polar("y", start=0) +
            theme_void()
    })
    
    output$gender <- renderUI({
        radioButtons("gender", label = "Gender",
                     choices = list("male", "female"),
                     selected = "male")
    })
    
    output$education <- renderUI({
        selectInput("Education", label = "What is your Education level",
                    choices = as.list(unique(music$Education)),
                    selected = "secondary school")
    })
    
    output$internet <- renderUI({
        selectInput("Internet", label = "How long do you spend on internet",
                    choices = as.list(unique(music$Internet.usage)),
                    selected = "few hours a day")
    })
    
    
})
