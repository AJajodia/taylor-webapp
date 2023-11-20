#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(tidyverse)
library(plotly)

### load data sets

load("taylor_all_songs.rda")
load("taylor_albums.rda")

billboard_albums <- read_csv("billboard_albums.csv")

#make album names consistent across datasets
billboard_albums[8, 2] <- "folklore"
billboard_albums[9, 2] <- "evermore"

billboard_songs <- read_csv("billboard_songs.csv")

#get count of explicit songs per album for album analysis
explicit_df <- taylor_all_songs %>%
  group_by(album_name) %>%
  count(explicit) %>%
  filter(explicit == TRUE) %>%
  select(album_name, n)

colnames(explicit_df)[2] <- 'album_explicit_songs'

#create album_focus dataset by taking individual song values and creating new columns for  overall album metrics (i.e. total album duration)
taylor_album_focus <- taylor_all_songs %>%
  full_join(explicit_df, by = "album_name") %>%
  full_join(taylor_albums, by = c("album_name", "ep", "album_release")) %>%
  inner_join(billboard_albums, by = "album_name") %>%
  filter(!(is.na(album_name))) %>%
  filter(!(
    album_name %in% c("The Taylor Swift Holiday Collection", "Beautiful Eyes")
  )) %>%
  mutate(album_name = as.character(album_name)) %>%
  group_by(album_name) %>%
  mutate(
    album_duration = sum(duration_ms, na.rm = T) / 60000,
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
    album_peak_position = peak_pos,
    album_weeks_charting = weeks_charting,
    album_explicit_songs = coalesce(album_explicit_songs, 0)
  ) %>%
  ungroup()

#make song_focus dataset by excluding columns that aren't of interest to song analysis (i.e. whether the song is from an EP)
taylor_song_focus <- taylor_all_songs %>%
  select(c(1, 3:5, 11:29)) %>%
  mutate(duration_ms = duration_ms / 60000)

colnames(taylor_song_focus)[18] <- 'duration_min'

#create dataset for two album comparison with consistent overall album metrics that can be compared to each other (scales are comparable)
taylor_albums_summaries <- taylor_album_focus %>%
  group_by(album_name) %>% summarize(
    danceability = mean(danceability, na.rm = TRUE),
    energy = mean(energy, na.rm = TRUE),
    speechiness = mean(speechiness, na.rm = TRUE),
    acousticness = mean(acousticness, na.rm = TRUE),
    liveness = mean(liveness, na.rm = TRUE),
    valence = mean(danceability, na.rm = TRUE)
  ) %>% pivot_longer(danceability:valence,
                     names_to = "variable_name",
                     values_to = "value")

#create color palette vector that assigns each album to specific color
taylor_palette <-
  c(
    "Taylor Swift" = "lightseagreen",
    "The Taylor Swift Holiday Collection" = "forestgreen",
    "Beautiful Eyes" = "darkorange",
    "Fearless" = "darkgoldenrod1",
    "Speak Now" = "mediumorchid",
    "Red" = "red",
    "1989" = "cornflowerblue",
    "reputation" = "black",
    "Lover" = "hotpink",
    "folklore" = "ivory4",
    "evermore" = "darkorange3",
    "Fearless (Taylor's Version)" = "darkgoldenrod3",
    "Red (Taylor's Version)" = "red3",
    "Midnights" = "navyblue",
    "Speak Now (Taylor's Version)" = "mediumorchid4",
    "1989 (Taylor's Version)" = "deepskyblue",
    "NA" = "gray99"
  )

#function to extract specified color for each album, in order to have consistent album coloring across plots
ts_colors <- function(x) {
  cols <- c(x)
  taylor_palette[cols]
}
###

#create vectors with desired variables for album and song analysis
album_vars <-
  taylor_album_focus %>% select(starts_with("album_"),-c(album_name))
song_vars <- taylor_song_focus %>% select(-c(23))

#### UI
ui <- fluidPage(# Application title
  titlePanel(
    #title and then album cover images in order of release
    tagList(
      "Taylor Swift",
      img(
        src = "Taylor_Swift.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Fearless.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Speak_Now.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Red.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "1989.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "reputation.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Lover.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "folklore.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "evermore.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Fearless_Taylor's_Version.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Red_Taylor's_Version.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Midnights.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "Speak_Now_Taylor's_Version.jpg",
        width = "40px",
        height = "40px"
      ),
      img(
        src = "1989_Taylor's_Version.jpg",
        width = "40px",
        height = "40px"
      )
    )
  ),
  
  # Sidebar with a slider input for number of bins
  tabsetPanel(
    #create four tabs, intro and then one for each visualization
    tabPanel("Introduction",
             #create columns for text and image
             fluidRow(column(
               6,
               p(
                 "
                     This app utilizes Spotify API data (from the Taylor
                     package for R) and  Billboard chart data to provide a
                     comprehensive method for analyzing the individual
                     characteristics and charting success of Taylor Swift's
                     complete discography. With the aid of the tools included in
                     this app, we aim to understand how Taylor Swift has evolved
                     as an artist over time, using
                     both musical metrics (tempo, energy, loudness, etc.) and
                     chart success.
                     "
               )
             ),
             column(
               6, img(src = "headshot.jpg"), p("Image from Getty Images")
             ))),
    #album analysis panel
    tabPanel(
      "Album Analysis",
      #text description
      p(
        "This visualization compares all Taylor Swift albums based on song characteristics from
        Spotify's API and charting data scraped from Billboard Music Charts.  The
      plot is interactive, allowing the user to zoom and hover over points to
      identify the album and its plotted characteristics. It is particularly
    interesting to note how Swift's artistry differs and evolves over her albums.
    For instance, when comparing the acousticness of her albums and the number of explicit songs
    on the album (excluding re-recorded Taylor's Versions) we see Swift's earlier albums are clumped together in the
    right corner, and her latest non-rerecored albums seperated out, indicating an evoltuion
    and experimentation in Swift's sound in recent years and indicating how her songwriting has
    matured. We are also able to look out Swift's popularity and critical acclaim accross her albums.
    When comparing album release and metacritic score, for instance, we see a clear upward trend in
    with Swift's latest release receiving the hightest metacritic score."
      ), 
    inputPanel(
      #choose x_axis variable from column names
      selectInput("album_xaxis",
                  "X Variable",
                  choices = colnames(album_vars)),
      # choiceNames = str_sub(colname(album_vars)),
      selectInput(
        #choose y_axis variable, default selection second column
        "album_yaxis",
        "Y Variable",
        choices = colnames(album_vars),
        selected = colnames(album_vars)[2]
      ),
      #decide if user wants to exclude re-recorded albums
      checkboxInput("exclude_tvs", "Exclude Taylor's Versions", value = F)
    ),
    #plot output
    plotlyOutput("albumPlot")
    ),
    #song analysis panel
    tabPanel(
      "Song Analysis",
      #text description
      p(
        "This visualization compares all of Taylor Swift's released songs based on song
      characteristics pulled from Spotify's API, colored by the Album or EP
      the song belong to.
      shows two albums in a direct comparison
      based on song characteristics pulled from Spotify's API. This tool allows
      for a granular sample of the entire corpus, allowing every song to be seen
      plotted by their different characteristics and colored by their album. The
      plot is interactive, allowing the user to zoom and hover over points to
      identify the song and its plotted characteristics."
      ),
      inputPanel(
        #select x_axis variable from column names
        selectInput("song_xaxis",
                    "X Variable",
                    choices = unique(colnames(song_vars)),
        selected = "album_release"),
        #select y_axis variable, default selection second column name
        selectInput(
          "song_yaxis",
          "Y Variable",
          choices = unique(colnames(song_vars)),
          selected = "danceability"
        )
      ),
      #plot output
      plotlyOutput("songPlot")
    ),
    #pale for two album comparison
    tabPanel(
      "Two Album Comparison",
      #text description
      p(
        "This visualization shows two albums in a direct comparison
      based on song characteristics pulled from Spotify's API. Though this tool
      allows for the comparison of any two albums, it highlights differences
      between Taylor's regular albums and \"Taylor's Version\" re-releases. It
      also characterizes what a Taylor Swift \"era\" might mean in terms of its
      unique song qualities and focuses."
      ),
      #create columns for each album selection and bar plot
      fluidRow(
        column(
          2,
          #select first album to compare from album names in dataset, default selection 1989
          selectInput(
            inputId = "album1",
            "First Album",
            choices = unique(taylor_albums_summaries$album_name),
            selected = "1989"
          ),
          #output cover image of selected album
          imageOutput("leftalbum")
        ),
        #plot comparison barchart in middle of page
        column(8, plotOutput("barPlot")),
        column(
          2,
          #select second album to compare, default selection 1989 (Taylor's Version)
          selectInput(
            inputId = "album2",
            "Second Album",
            choices = unique(taylor_albums_summaries$album_name),
            selected = "1989 (Taylor's Version)"
          ),
          #output album cover image
          imageOutput("rightalbum")
        )
      )
    ),
    p("Album covers courtesy of taylorpictures.net")
  )
  )


# SERVER
server <- function(input, output) {
  #create dataset used in album analysis plot based on whether re-recordings are included
  taylor_album_plot <- reactive({
    if (input$exclude_tvs == T) {
      return (taylor_album_focus %>% filter(!str_detect(album_name, "Taylor's Version")))
    }
    else {
      return (taylor_album_focus)
    }
  })
  
  output$albumPlot <- renderPlotly({
    #use reactive dataset assigned above
    p1 <- taylor_album_plot() %>%
      #x and y axis decided by UI input
      ggplot(aes_string(x = input$album_xaxis, y = input$album_yaxis)) +
      geom_point(aes(
        #color by album in order of release
        color = fct_reorder(album_name, album_release),
        #text to be displayed for each point in interactive plot, x variable, y variable and album name
        text = paste(
          paste(str_to_title(
            str_replace_all(input$album_xaxis, "_", " ")
          )),
          ": ",
          get(req(input$album_xaxis)),
          "<br>",
          paste(str_to_title(
            str_replace_all(input$album_yaxis, "_", " ")
          )),
          ": ",
          get(req(input$album_yaxis)),
          "<br><b>Album:</b> ",
          fct_reorder(album_name, album_release)
        )
      ), size = 7) +
      #color palette based on albums included in plot
      scale_color_manual(values = ts_colors(unique(taylor_album_plot()$album_name))) +
      #string manipulation to make labels not in coded snake case
      labs(
        color = "Album",
        x = paste(str_to_title(
          str_replace_all(input$album_xaxis, "_", " ")
        )),
        y = paste(str_to_title(
          str_replace_all(input$album_yaxis, "_", " ")
        ))
      ) +
      theme_bw()
    
    #make ggplot interactive, tooltip is information we want to dispaly as specified above
    ggplotly(p1, tooltip = "text")
  })
  
  output$songPlot <- renderPlotly({
    #use song_focus dataset
    p2 <- taylor_song_focus %>%
      #get x and y variables from UI input
      ggplot(aes_string(x = input$song_xaxis, y = input$song_yaxis)) +
      #color by album in order of release and get text to be displayed when made interactive
      geom_point(aes(
        color = fct_reorder(album_name, album_release),
        text = paste(
          paste(str_to_title(
            str_replace_all(input$song_xaxis, "_", " ")
          )),
          ": ",
          get(req(input$song_xaxis)),
          "<br>",
          paste(str_to_title(
            str_replace_all(input$song_yaxis, "_", " ")
          )),
          ": ",
          get(req(input$song_yaxis)),
          "<br><b>Album:</b> ",
          fct_reorder(album_name, album_release),
          "<br><b>Song Title:</b> ",
          track_name
        )
      )) +
      #get color palette based on albums in visualization
      scale_color_manual(values = ts_colors(unique(taylor_song_focus$album_name))) +
      #labels not in snake case
      labs(
        color = "Album",
        x = paste(str_to_title(
          str_replace_all(input$song_xaxis, "_", " ")
        )),
        y = paste(str_to_title(
          str_replace_all(input$song_yaxis, "_", " ")
        ))
      ) +
      theme_bw()
    
    #make plot interactive
    ggplotly(p2, tooltip = "text")
  })
  
  # make reactive filename to access image 1
  filename1 <- reactive({
    # normalize path
    normalizePath(file.path("www", paste(
      # replace "(" and ")" with "_"
      str_replace_all(str_replace_all(
        # replace " " with ""
        str_replace_all(input$album1, "\\s", "_"),
        "\\(", ""
      ), "\\)", ""), ".jpg", sep = ""
    )))
  })
  
  # make reactive filename to access image 2
  filename2 <- reactive({
    # normalize path
    normalizePath(file.path("www",
                            paste(
                              # replace "(" and ")" with "_"
                              str_replace_all(str_replace_all(
                                # replace " " with ""
                                str_replace_all(input$album2, "\\s", "_"),
                                "\\(", ""
                              ), "\\)", ""), ".jpg", sep = ""
                            )))
  })
  
  # filter albums
  filtered_taylor_albums_summaries <- reactive(
    taylor_albums_summaries %>% filter(album_name == input$album1 |
                                         album_name == input$album2)
  )
  
  # render left album image
  output$leftalbum <- renderImage({
    list(src = filename1(),
         width = "100",
         height = "100")
    # don't delete file after rendering
  }, deleteFile = FALSE)
  
  # render left album image
  output$rightalbum <- renderImage({
    list(src = filename2(),
         width = "100",
         height = "100")
    # don't delete file after rendering
  }, deleteFile = FALSE)
  
  # render album comparison plot
  output$barPlot <- renderPlot({
    ggplot(
      filtered_taylor_albums_summaries(),
      aes(fill = album_name, y = value, x = variable_name)
    ) +
      # geom_bar grouped by album characteristics and then by album
      geom_bar(position = "dodge", stat = "identity") +
      labs(fill = "Album",
           x = "Album Characteristics",
           y = "Value") +
      # apply Taylor Swift album colors
      scale_fill_manual(values = ts_colors(unique(
        filtered_taylor_albums_summaries()$album_name
      ))) +
      # black and white
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
