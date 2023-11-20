# Taylor Swift App

This project is an app which allows users to investigate Taylor Swift's discography over time.

Updated versions of R data files with newer songs can be found as part of the [Taylor package for R](https://github.com/wjakethompson/taylor).

Accessible at [this URL](https://ajajodia.shinyapps.io/taylor_swift_app/) using shinyapps.io to host.

## Installation

Required packages

``` r
install.packages(c("shiny", "tidyverse", "plotly"))
```

## Usage

Run from /taylor_swift_app/app.R

## File List

Technical_Report.md - markdown file containing technical information, background, and methodology for this project

mariah_ts_ideas.Rmd - ideas file for Mariah Loehr

anu_ts_ideas.Rmd - ideas file for Anu Jajodia

taylor_albums.rda - R data object containing album names and features

taylor_all_songs.rda - R data object containing all of Taylor's songs and Spotify API features

billboard.Rmd - file containing scraping process for obtaining Billboard chart data

billboard_albums.csv - CSV file containing scraped album information from Billboard

billboard_songs.csv - CSV file containing scraped song information from Billboard

final_project.pdf - PDF file containing instructions given by Claire Kelling as part of STAT220 at Carleton College

album_thumbnails - folder containing Taylor image displayed on homepage and album thumbnails as JPG files

taylor_swift_app - folder containing Shiny app deployment files

### taylor_swift_app

app.R - Shiny app part of the current release of this product

billboard_albums.csv - repeated above

billboard_songs.csv - repeated above

taylor_albums.rda - repeated above

taylor_all_songs.rda - repeated above

rsconnect - folder containing shinyapps.io deployment files

www - folder containing images for app.R
