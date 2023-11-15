#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

### data sets

load("taylor_all_songs.rda")
load("taylor_albums.rda")
load("taylor_album_songs.rda")

billboard_albums <- read_csv("billboard_albums.csv")

billboard_albums[8,2] <- "folklore"
billboard_albums[9,2] <- "evermore"

billboard_songs <- read_csv("billboard_songs.csv")

taylor_album_focus <- taylor_all_songs %>%
  full_join(taylor_albums, by = c("album_name", "ep", "album_release")) %>%
  inner_join(billboard_albums, by = "album_name") %>%
  filter(!(is.na(album_name))) %>%
  filter(!(album_name %in% c("The Taylor Swift Holiday Collection", "Beautiful Eyes"))) %>%
  mutate(album_name = as.character(album_name)) %>%
  group_by(album_name) %>%
  mutate(album_duration = sum(duration_ms, na.rm = T)/60000,
         album_danceability = mean(danceability, na.rm = T),
         album_energy = mean(energy, na.rm = T),
         album_loudness = mean(loudness, na.rm = T),
         album_speechiness = mean(speechiness, na.rm = T),
         album_acousticness = mean(acousticness, na.rm = T),
         album_instrumentalness = mean(instrumentalness, na.rm = T),
         album_liveness = mean(liveness, na.rm = T),
         album_valence = mean(valence, na.rm = T),
         album_tempo = mean(tempo, na.rm = T),
         album_track_number = max(track_number),
         album_metacritic_score = metacritic_score,
         album_user_score = user_score,
         album_peak_pos = peak_pos,
         album_weeks_charting = weeks_charting
  ) %>%
  ungroup()


taylor_song_focus <- taylor_all_songs %>%
  # full_join(billboard_songs, by = c("track_name" = "name")) %>%
  select(c(1,3:5, 11:29)) %>%
  mutate(duration_ms = duration_ms/60000)

colnames(taylor_song_focus)[18] <- 'duration_min'


taylor_albums_summaries <- taylor_album_focus %>%
  group_by(album_name) %>% summarize(
    danceability = mean(danceability, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    speechiness = mean(speechiness, na.rm = TRUE),
    acousticness = mean(acousticness, na.rm = TRUE),
    liveness = mean(liveness, na.rm = TRUE),
    valence = mean(danceability, na.rm = TRUE)
  ) %>% pivot_longer(danceability:valence, names_to = "variable_name", values_to = "value")

taylor_palette <- c("Taylor Swift" ="lightseagreen","The Taylor Swift Holiday Collection"="forestgreen", 
                    "Beautiful Eyes" ="darkorange", "Fearless" ="darkgoldenrod1",
                    "Speak Now" ="mediumorchid", "Red" ="red", 
                    "1989" ="cornflowerblue", "reputation" ="black", 
                    "Lover" ="hotpink", "folklore" ="ivory4",
                    "evermore" ="darkorange3","Fearless (Taylor's Version)"="darkgoldenrod3",
                    "Red (Taylor's Version)" ="red3", "Midnights" ="navyblue",
                    "Speak Now (Taylor's Version)" ="mediumorchid4",
                    "1989 (Taylor's Version)" ="deepskyblue", "NA"="gray99")

ts_colors <- function(x) {
  cols <- c(x)
  taylor_palette[cols]
}
###


album_vars <- taylor_album_focus %>% select(starts_with("album_"), -c(album_name))
song_vars <- taylor_song_focus %>% select(-c(23))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(tagList("Taylor Swift",
                       img(src = "Taylor_Swift.jpg", width = "40px", height = "40px"),
                       img(src = "Fearless.jpg", width = "40px", height = "40px"),
                       img(src = "Speak_Now.jpg", width = "40px", height = "40px"),
                       img(src = "Red.jpg", width = "40px", height = "40px"),
                       img(src = "1989.jpg", width = "40px", height = "40px"),
                       img(src = "reputation.jpg", width = "40px", height = "40px"),
                       img(src = "Lover.jpg", width = "40px", height = "40px"),
                       img(src = "folklore.jpg", width = "40px", height = "40px"),
                       img(src = "evermore.jpg", width = "40px", height = "40px"),
                       img(src = "Fearless_Taylor's_Version.jpg", width = "40px", height = "40px"),
                       img(src = "Red_Taylor's_Version.jpg", width = "40px", height = "40px"),
                       img(src = "Midnights.jpg", width = "40px", height = "40px"),
                       img(src = "Speak_Now_Taylor's_Version.jpg", width = "40px", height = "40px"),
                       img(src = "1989_Taylor's_Version.jpg", width = "40px", height = "40px")
                       )),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Album Analysis",
                          inputPanel(selectInput("album_xaxis",
                                      "X Variable",
                                      choices = colnames(album_vars)),
                                      # choiceNames = str_sub(colname(album_vars)),
                          selectInput("album_yaxis", 
                                      "Y Variable",
                                      choices = colnames(album_vars),
                                      selected = colnames(album_vars)[2]),
                          selectInput("include_albums",
                                      "Albums",
                                      choices = unique(album_vars$album_name),
                                      multiple = T), 
                                      # selected = colnames(album_vars[2])),
                          checkboxInput("exclude_tvs", "Exclude Taylor's Versions", value = F)),
                   plotOutput("albumPlot")
                 ),
        tabPanel("Song Analysis",
                          inputPanel(selectInput("song_xaxis",
                                      "X Variable",
                                      choices = colnames(song_vars)),
                          selectInput("song_yaxis", 
                                      "Y Variable",
                                      choices = colnames(song_vars),
                                      selected = colnames(song_vars)[2])),
                   plotOutput("songPlot")
                 ),
        tabPanel("Two Album Comparison",
                 fluidRow(
                   column(2, 
                          selectInput(inputId = "album1", "First Album", 
                                      taylor_albums_summaries$album_name),
                                      # selected = taylor_albums_summaries[1,1]),
                          imageOutput("leftalbum")
                   ),
                   column(8, plotOutput("barPlot")),
                   column(2, selectInput(inputId = "album2", "Second Album", choices = taylor_albums_summaries$album_name),
                          imageOutput("rightalbum")
                   )
                 )
        )
          
            )
        )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  taylor_album_plot <- reactive({
    if (input$exclude_tvs == T){
      return (taylor_album_focus %>% filter(!str_detect(album_name, "Taylor's Version")))
    }
    else {
      return (taylor_album_focus)
    }
  })

    output$albumPlot <- renderPlot({
      taylor_album_plot() %>%
        ggplot(aes_string(x=input$album_xaxis, y=input$album_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release)), size = 7) +
        scale_color_manual(values = ts_colors(unique(taylor_album_plot()$album_name))) +
        labs(color = "Album") +
        theme_bw()
    })
    
    output$songPlot <- renderPlot({
      taylor_song_focus %>%
        ggplot(aes_string(x=input$song_xaxis, y=input$song_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release))) +
        scale_color_manual(values = ts_colors(unique(taylor_song_focus$album_name))) +
        labs(color = "Album") +
        theme_bw()
    })
    
    
    filename1 <- reactive({
      normalizePath(file.path(
        "www", paste(str_replace_all(str_replace_all(str_replace_all(input$album1,"\\s","_"),
                                              "\\(", ""), "\\)", ""), ".jpg", sep="")))
    })
    
    filename2 <- reactive({
      normalizePath(file.path(
        "www", 
        paste(str_replace_all(str_replace_all(str_replace_all(input$album2,"\\s","_"), 
                                              "\\(", ""), "\\)", ""), ".jpg", sep="")))
    })
    
    filtered_taylor_albums_summaries <- reactive(
      taylor_albums_summaries %>% filter(album_name == input$album1 | album_name == input$album2)
    )
    
    output$leftalbum <- renderImage({
      list(src = filename1(), width = "100", height = "100")
    }, deleteFile = FALSE)
    
    output$rightalbum <- renderImage({
      list(src = filename2(), width = "100", height = "100")
    }, deleteFile = FALSE)
    
    output$barPlot <- renderPlot({
      ggplot(filtered_taylor_albums_summaries(), 
             aes(fill=album_name, y=value, x=variable_name)) + 
        geom_bar(position="dodge", stat="identity") +
        labs(fill = "Album") +
        scale_fill_manual(values = ts_colors(unique(filtered_taylor_albums_summaries()$album_name))) +
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
