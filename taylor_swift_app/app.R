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

album_vars <- taylor_album_focus %>% select(starts_with("album_"), -c(album_name))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taylor Swift"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Album Analysis",
                 fluidRow(
                   column(3, 
                          selectInput("album_xaxis",
                                      "X Variable",
                                      choices = colnames(album_vars)),
                          selectInput("album_yaxis", 
                                      "Y Variable",
                                      choices = colnames(album_vars),
                                      selected = colnames(album_vars)[2])
                          ),
                   column(9, plotOutput("albumPlot")
                          ))
                 ),
        tabPanel("Song Analysis",
                 fluidRow(
                   column(3,
                          selectInput("song_xaxis",
                                      "X Variable",
                                      choices = colnames(taylor_all_songs)),
                          selectInput("song_yaxis", 
                                      "Y Variable",
                                      choices = colnames(taylor_all_songs),
                                      selected = colnames(taylor_all_songs)[2])
                          ),
                   column(8, plotOutput("songPlot")
                   )
                 )
          
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
### data sets
  taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
  taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
  taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')
  
  taylor_album_focus <- taylor_all_songs %>%
    # full_join(taylor_albums, by = c("album_name", "ep", "album_release")) %>%
    filter(!(is.na(album_name))) %>%
    filter(!(album_name %in% c("The Taylor Swift Holiday Collection", "Beautiful Eyes"))) %>%
    mutate(album_name = as.character(album_name)) %>%
    group_by(album_name) %>%
    mutate(album_length_min = sum(duration_ms, na.rm = T)/60000,
           album_danceability = sum(danceability, na.rm = T)/max(track_number),
           album_energy = sum(energy, na.rm = T)/max(track_number),
           album_loudness = sum(loudness, na.rm = T)/max(track_number),
           album_speechiness = sum(speechiness, na.rm = T)/max(track_number),
           album_acousticness = sum(acousticness, na.rm = T)/max(track_number),
           album_instrumentalness = sum(instrumentalness, na.rm = T)/max(track_number),
           album_liveness = sum(liveness, na.rm = T)/max(track_number),
           album_valence = sum(valence, na.rm = T)/max(track_number),
           album_tempo = sum(tempo, na.rm = T)/max(track_number),
           album_track_number = max(track_number),
           # album_metacritic_score = metacritic_score,
           # album_user_score = user_score
    ) %>%
    ungroup()
  
  all_album_palette <- c("lightseagreen", "darkgoldenrod1", "mediumorchid", 
                         "red", "cornflowerblue", "black", "hotpink",  "ivory4", 
                         "darkorange3", "darkgoldenrod3" ,"red3","navyblue")
  
  all_songs_palette <- c("lightseagreen", "forestgreen", "darkorange","darkgoldenrod1", "mediumorchid", 
                         "red", "cornflowerblue", "black", "hotpink",  "ivory4", 
                         "darkorange3", "darkgoldenrod3" ,"red3","navyblue", "gray99")
###

    output$albumPlot <- renderPlot({
      taylor_album_focus %>%
        ggplot(aes_string(x=input$album_xaxis, y=input$album_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release)), size = 5) +
        scale_color_manual(values = all_album_palette) +
        labs(color = "Album")
    })
    
    output$songPlot <- renderPlot({
      taylor_all_songs %>%
        ggplot(aes_string(x=input$song_xaxis, y=input$song_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release))) +
        scale_color_manual(values = all_songs_palette) +
        labs(color = "Album")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
