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

billboard_albums <- read_csv(
  "/Users/mariahloehr/Stat220/Assignments/final_project/billboard_albums.csv")

billboard_albums[8,2] <- "folklore"
billboard_albums[9,2] <- "evermore"

billboard_songs <- read_csv(
  "/Users/mariahloehr/Stat220/Assignments/final_project/billboard_songs.csv")

taylor_album_focus <- taylor_all_songs %>%
  full_join(taylor_albums, by = c("album_name", "ep", "album_release")) %>%
  inner_join(billboard_albums, by = "album_name") %>%
  filter(!(is.na(album_name))) %>%
  filter(!(album_name %in% c("The Taylor Swift Holiday Collection", "Beautiful Eyes"))) %>%
  mutate(album_name = as.character(album_name)) %>%
  group_by(album_name) %>%
  mutate(album_duration = sum(duration_ms, na.rm = T)/60000,
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
         album_metacritic_score = metacritic_score,
         album_user_score = user_score,
         album_peak_pos = peak_pos,
         album_weeks_charting = weeks_charting
  ) %>%
  ungroup()



all_album_palette <- c("lightseagreen", "darkgoldenrod1", "mediumorchid", 
                       "red", "lightskyblue", "black", "hotpink",  "ivory4",
                       "darkorange3", "darkgoldenrod3" ,"red3","navyblue", 
                       "mediumorchid4", "deepskyblue")

all_songs_palette <- c("lightseagreen", "forestgreen", "darkorange","darkgoldenrod1", "mediumorchid", 
                       "red", "cornflowerblue", "black", "hotpink",  "ivory4", 
                       "darkorange3", "darkgoldenrod3" ,"red3","navyblue", 
                       "mediumorchid4", "deepskyblue", "gray99")
###


album_vars <- taylor_album_focus %>% select(starts_with("album_"), -c(album_name))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taylor Swift"),

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
                          checkboxGroupInput("include_albums",
                                      "Albums",
                                      choices = album_vars$album_name)),
                   plotOutput("albumPlot")
                 ),
        tabPanel("Song Analysis",
                          inputPanel(selectInput("song_xaxis",
                                      "X Variable",
                                      choices = colnames(taylor_all_songs)),
                          selectInput("song_yaxis", 
                                      "Y Variable",
                                      choices = colnames(taylor_all_songs),
                                      selected = colnames(taylor_all_songs)[2])),
                   plotOutput("songPlot")
                 )
          
            )
        )


# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$albumPlot <- renderPlot({
      taylor_album_focus %>%
        ggplot(aes_string(x=input$album_xaxis, y=input$album_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release)), size = 7) +
        scale_color_manual(values = all_album_palette) +
        labs(color = "Album") +
        theme_bw()
    })
    
    output$songPlot <- renderPlot({
      taylor_all_songs %>%
        ggplot(aes_string(x=input$song_xaxis, y=input$song_yaxis)) +
        geom_point(aes(color = fct_reorder(album_name, album_release))) +
        scale_color_manual(values = all_songs_palette) +
        labs(color = "Album") +
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
