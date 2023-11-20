### Introduction

Our goal with this Shiny app was to create three different visualizations to analyze Taylor Swift's discography across albums, individual songs, and direct album comparisons, to understand how Taylor Swift can be characterized as an artist and how this characterization has changed over time.

### Data

We began by obtaining Spotify API data from the Taylor library in R and album charting data from web scraping the Billboard website. For our analysis we merged two of the Spotify API datasets (taylor_all_songs and taylor_albums) and the Billboard dataset. We then created three datasets, one for each respective analysis. To create the album focus dataset, we created columns that took the individual song data and gave a metric for the entire album, for instance calculating the duration of the whole album or the average song tempo per album. For the song focus dataset, we simply excluded the columns we were not interested in, such as artist (it is almost always Taylor Swift) and EP. For the album comparison dataset, the features were limited to Spotify's calculated characteristics (example: energy) to keep the scale consistent and also ensure that they could all be on a single bar chart.

### Colors

Taylor Swift also has iconic colors that are associated with each of her albums, and to honor this we wanted to color by album consistently across all our plots. To accomplish this, we created a vector that assigned each of Swift's albums to a color, and then created a function that extracts the color codes from this vector by the name of the album. This function is then used to create a color palette for each plot.

### App

We then created our Shiny app. We used a tabset layout to create four tabs. The introduction tab has an image and a text explanation of our overall website and main questions.

The album analysis tab includes a text description and analysis starting-point, an input panel, and a main panel with the scatterplot. The input panel has two select input options that allow the user to choose which album centric variables they wish to analyze in the plot. There is also a button excluding the re-recorded Taylor's Versions from analysis. In response to this button, we used reactivity to create the dataset used when rendering the plot, so the dataset excludes Taylor's Versions albums from the plot when indicated. We also used ggplotly to make the plot interactive for clarity, specifically to label albums. We learned to use tooltip to get the interactive information to appear how we wanted.

The song analysis panel was created similarly. We have a text description, an input panel with the variables for the x and y axis, and a main panel with an interactive scatterplot.

The two album comparison uses columns to create a three pane spread, with the albums on either side of their bar chart. We decided not to make the ggplot bar chart interactive because there isn't much description beyond what the labels say. The select boxes use reactive elements for the names of the two different albums, which are converted with regex to filename formats (e.g. "1989 Taylor's Version" becomes "1989_Taylor's_Version") in order to get the images corresponding to the album names. Simultaneously, the data is also filtered in a reactive element to choose only the albums of interest and pivoted such that columns are the names of albums and variables are the observations.
