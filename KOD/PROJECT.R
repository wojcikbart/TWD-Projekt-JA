library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(plotly)
library(lubridate)
library(spotifyr)
library(tidyr)
library(shinyWidgets)
library(shinyjs)
library(jsonlite)
library(fmsb)

####   Wczytanie Danych   ####

Songs <- fromJSON("./dane/Songs.json")

SH <- fromJSON("./dane/SpotifyExtendedAll.json")

minutesPerWeek <- fromJSON("./dane/minutesPerWeek.json")

playlist <- fromJSON("./dane/playlistData.json")


Songs <- fromJSON("../dane/Songs.json")

SH <- fromJSON("../dane/SpotifyExtendedAll.json")

minutesPerWeek <- fromJSON("../dane/minutesPerWeek.json")

playlist <- fromJSON("../dane/playlistData.json")

####   Style   ####

HTML_styles <- '
      * {
        font-family: "Gotham";
      }

      .main-header {
        position: fixed;
        width: 100%;
      }

      .skin-green .main-header .logo, .skin-green .main-header .navbar{
        background-color: #000;
      }

      body {
        color: #FFFFFF;
        background-color: #000;
      }
      
      .content {
        margin-top: 50px;
        min-height: calc(88vh - 50px);
        margin-bottom: 12vh;
        background-color: #000;
        padding: 0;
      }
      
      .body-panel {
        min-height: calc(88vh - 50px);
        border: 6px solid #000;
        border-radius: 15px;
        background-color: #121212;
      }
      
      .content .wrapped {
        margin-bottom: 12vh;
        background-color: #121212;
      }
      
      .sidebar {
        display: block !important;
        width: 40vh;
        height: calc(88vh - 50px);
        padding: 0px;
        background-color: #000;
        position: fixed;
      }

      .sidebar-panel {
        display: block;
        width: 100%;
        height: 100%;
        flex-direction: column;
        justify-content: 
        background-color: #000;
        border: 1px solid #000;
      }

      .sidebar-panel-upper{
        display: flex;
        height: calc(20% - 10.5px);
        background-color: #121212;
        border-radius: 10px;
        margin: 7px;
        flex-direction: column;
        justify-content: center;
      }

      .sidebar-panel-lower {
        display: flex;
        height: calc(80% - 10.5px);
        background-color: #121212;
        border-radius: 10px;
        margin: 7px;
        flex-direction: column;
        justify-content: top;
      }

      html {
        font-size: 16px;
        color: #FFFFFF !important;
      }
      
      * {
          font-family: "Gotham";
          letter-spacing: -0.35px;
      }

      .top-text {
        display: inline-block;
        margin-left: 20px;
        margin-bottom: 55px;
      }

      .text-fav {
        margin-left:30px;
        font-weight:bold;
      }
      
      .pretty .state label {
        color: white;
        height: 36%;
        font-size: 18px;
        top: 0;
        margin-bottom: 18%;
        margin-top: 18%;
        font-weight:bold;
      }

      .pretty .state label:before {
        top: 10px;
      }

      .pretty .state label:after {
        top:10px;
      }

      .control-label{
        font-weight:600;
        margin-bottom: 20px;
      }
      
      .footer {
        display: flex;
        justify-content: space-between;
        background-color: #000;
        flex-direction: row;
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 12vh;
        padding: 0 2vh;
        left: 0;
        z-index: 1000;
      }
      
      .footer-left-panel {
        width: 30%;
        display: flex;
        justify-content: left;
        height: 100%;
        align-items: center;
      }
             
      .our-cover {
        width: 8.5vh;
        height: 8.5vh;
        border-radius: 10%;
        margin-right: 2vh;
      }
      
      .footer-center-panel {
        margin-top: 2vh;
        width: 33%;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 2000;
      }

      .icons {
        width: 50%;
        display: flex;
        justify-content: center;
        flex-direction: row;
      }
      
      .slider {
        margin-top: -4vh;
        width: 100%;
      }
      
      .slider .irs-handle, .slider2 .irs-handle {
        background-color: white !important;
        height: 13px;
        width: 13px;
        top: 22px;
      }

      .slider .irs-bar, .slider2 .irs-bar{
        top: 25px;
        height: 8px;
        background: #1DB954;
      }

      .slider .irs-from, .slider .irs-to, .slider .irs-single {
        color: #909090;
        text-shadow: none;
        background-color:#000;
        border-radius: 25px;
        font-size: 10px;
      }

      .slider .irs-min, .slider .irs-max {
        color: #909090;
        text-shadow: none;
        background-color:#000;
        border-radius: 25px;
        font-size: 10px;
      }

      .slider .irs-min {
        left: -37px;
        top: 22px;
      }

      .slider .irs-max {
        right: -37px;
        top: 22px
      }

      .footer-right-panel {
        display: flex;
        width: 30%;
        justify-content: right;
        flex-direction: row;
        height: 100%;
        align-items: center;
        padding-right: 20px;
      }

      .slider2 {
        width: 30%;
        border-radius: 10%;
      }

      .slider2 .irs-min, .slider2 .irs-max, .slider2 .irs-from, .slider2 .irs-to, .slider2 .irs-single {
        display: none;
      }

      .icons2 {
        display: flex;
        width: 40%;
        flex-direction: row;
      }
      '


####   UI   ####

ui <- dashboardPage(
  title = "Jestem Akustyczny",
  skin = "green",
  dashboardHeader(
    title = span("projekt - JA"),
    titleWidth = '40vh'),
  dashboardSidebar(
    width = '40vh',
    div(
      class = 'sidebar',
      div(
        class = "sidebar-panel",
        div(
          class = "sidebar-panel-upper",
          prettyRadioButtons(
            inputId = "year",
            label = NULL,
            thick = TRUE,
            fill = TRUE,
            shape = "round",
            animation = "jelly",
            choices = c(2022,2023),
            selected = 2022,
            status = "default")),
        div(
          class = "sidebar-panel-lower",
          sidebarMenu(
            id = "tabs",
            h3("Features", style = "margin-left: 5%; font-size: 35px; font-weight: bold;"),
            menuItem("  Wrapped", tabName = "wrapped"),
            menuItem("  Compatibility", tabName = "compatibility"),
            menuItem("  Playlist", tabName = "playlist"),
            menuItem("  Summary", tabName = "summary")))))),
  dashboardBody(
    tags$head(tags$style(HTML_styles)),
    div(
      class = 'body-panel',
      tabItems(
        tabItem(
          tabName = "wrapped",
          fluidPage(
            # Tutaj kod dla wrapped
            uiOutput("wrapped_title"),
            fluidRow(
              h3("Top Artists", class = "text-fav"),
              plotlyOutput("topArtists"),
              h3("Artist's Songs Mean Features", class = "text-fav"),
              plotOutput("clickP"),
              h3("Top Tracks by this artist", class = "text-fav"),
              tableOutput("clickT")),
            fluidRow(
              h3("Top Tracks", class = "text-fav"),
              plotlyOutput("topSongs")),
            fluidRow(
              selectInput(
                inputId = "parameter",
                label = "Parameter:",
                choices = c(
                  "Danceability" = "danceability",
                  "Energy" = "energy",
                  "Liveness" = "liveness",
                  "Speechiness" = "speechiness",
                  "Valence" = "valence",
                  "Instrumentalness" = "instrumentalness",
                  "Acousticness" = "acousticness"
                )),
              plotOutput("violin")),
            fluidRow(h3("Average minutes listened per day of the week", class = "text-fav"),
                     plotlyOutput("minutesPerDayOfWeek")),
            fluidRow(h3("Average listening through the day", class = "text-fav"),
                     plotlyOutput("listeningThroughDay")))),
        tabItem(
          tabName = "playlist",
          fluidPage(
            checkboxGroupInput("selected_people",
                               "Wybierz osoby:",
                               choices = unique(playlist$Who),
                               selected = "Filip"),
            sliderInput("song_count_slider", "Liczba piosenek:", min = 10, max = 50, value = 15),
            uiOutput("playlist_title"),
            uiOutput("song_list_output"))),
        tabItem(
          tabName = "summary",
          fluidPage(
            # Tutaj kod do podsumowania
            uiOutput("summary_title"))))),
    div(
      div(
        class = "footer-left-panel",
        uiOutput("our_cover"),
        uiOutput("left_caption"),
        uiOutput("heart")),
      div(
        class = "footer-center-panel",
        div(
          icon("shuffle", style = "margin: auto; font-size: 2.5vh; color: #909090"),
          actionButton(inputId = "backward", label = "", icon = icon("backward-step"), style = "z-index:4000; cursor:pointer; margin: 0 0 auto; border-radius: 5px; border: none; font-size: 2.5vh; color: #909090; background-color: #000;", class = "btn-xs"),
          icon("pause-circle", style = "margin: auto; font-size: 3.5vh;", class = "fas"),
          actionButton(inputId = "forward", label = "", icon = icon("forward-step"), style = "z-index:4000; cursor:pointer; margin: 0 0 auto; border-radius: 5px; border:none; font-size: 2.5vh; color: #909090; background-color: #000;", class = "btn-xs"),
          icon("repeat", style = "margin: auto; font-size: 2.5vh; color: #909090"),
          class = "icons"),
        div(
          class = "slider",
          sliderInput(
            "Months",
            "",
            min = 1,
            max = 12,
            value = c(1,12),
            width = '100%',
            ticks = FALSE))),
      div(
        class = "footer-right-panel",
        div(
          class = "icons2",
          icon("square-poll-vertical", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("microphone", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("bars", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("speaker-deck", style = "margin: auto; font-size: 2vh; color: #909090"),
          icon("volume-low", style = "margin: auto; font-size: 2vh; color: #909090"),),
        div(
          class = "slider2",
          sliderTextInput(
            "user",
            NULL,
            choices = c("Karolina", "Bartek", "Filip"),
            selected = "Karolina"))),
      class = "footer")))


####   Server   ####


server = function(input, output, session) {
  
  ####   Praca na Danych   ####
  #### MINUTES PER WEEK ####
  mPWfiltered <- reactive({
    minutesPerWeek %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      group_by(dayOfWeek) %>% 
      summarise(count = sum(count), time = sum(time)) %>% 
      mutate(time = time / count)
  })
  
  output$minutesPerDayOfWeek <- renderPlotly({
    plot_ly(mPWfiltered(),
            x = ~dayOfWeek,
            y = ~time,
            type = "bar",
            marker = list(color = '#1DB954')) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        xaxis = list(
          categoryorder = "array",
          categoryarray = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
        ),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  #### LISTENING THROUGH THE DAY ####
  lTDfiltered <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      mutate(ts = ymd_hms(ts),
             hour = hour(ts)) %>% 
      group_by(hour) %>%
      summarise(time = sum(ms_played)/ (60 * 1000)) %>%
      group_by(hour) %>% 
      summarise(time = mean(time)) %>% 
      arrange(hour)
  })
  
  output$listeningThroughDay <- renderPlotly({
    plot_ly(lTDfiltered(),
            x = ~hour,
            y = ~time,
            type = "bar",
            marker = list(color = '#1DB954')) %>%
      animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
      layout(
        xaxis = list(
          title = "Hour",
          tickmode = "linear",
          tick0 = 0,    
          dtick = 1
        ),
        yaxis = list(title = "Time)"),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  #### TOP ARTISTS #### 
  
  SHfilteredArtists <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(time = sum(ms_played) / 60000) %>% 
      arrange(-time) %>% 
      head(10) %>% 
      na.omit()
  })
  
  output$topArtists <- renderPlotly({
    plot_ly(SHfilteredArtists(),
            x = ~time,
            y = ~reorder(master_metadata_album_artist_name, time),
            type = "bar",
            marker = list(color = '#1DB954'),
            orientation = 'h') %>%
      layout(
        xaxis = list(title = "Minutes Listened"),
        yaxis = list(title = list(text = "Artist")),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE) %>% 
      event_register('plotly_click')
  })
  
  SHfilteredArtistsSongs <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      group_by(master_metadata_album_artist_name, master_metadata_track_name) %>% 
      summarise(time = sum(ms_played) / 60000) %>% 
      arrange(-time) %>% 
      na.omit()
  })
  
  ArtistFeaturesfiltered <- reactive({
    Songs %>% 
      filter(person == input$user) %>% 
      group_by(master_metadata_album_artist_name) %>% 
      select(master_metadata_album_artist_name, danceability:valence) %>% 
      summarise(across(danceability:valence, mean)) %>% 
      select(-c(mode, key, loudness)) %>% 
      na.omit()
  })
  
  output$clickT <- renderTable({
    selected_artist <- SHfilteredArtists()$master_metadata_album_artist_name[1]
    selected_data <- event_data("plotly_click")
    if (!is.null(selected_data)) {
      selected_artist <- selected_data$y
    }
    selected_songs <- SHfilteredArtistsSongs() %>%
      filter(master_metadata_album_artist_name == selected_artist) %>% 
      head(10) %>% 
      mutate(lp = seq_along(master_metadata_track_name)) %>% 
      select(lp, everything())
    colnames(selected_songs) <- c("  ", "Artist", "Track", "Minutes Listened")
    return(selected_songs)
  })
  
  output$clickP <- renderPlot({
    selected_artist <- SHfilteredArtists()$master_metadata_album_artist_name[1]
    selected_data <- event_data("plotly_click")
    if (!is.null(selected_data)) {
      selected_artist <- selected_data$y
    }
    selected <- ArtistFeaturesfiltered() %>% 
      filter(master_metadata_album_artist_name == selected_artist) %>% 
      select(danceability:valence)
    selected <- rbind(0, 1, selected) 
    par(bg = "#121212", col = "white", family = "Gotham")
    rc <- radarchart(selected, 
                     axistype = 1, 
                     pcol = '#1db954',
                     pfcol = alpha('#1db954', 0.3),
                     plwd = 2,
                     plty = 1,
                     cglty = 2,
                     axislabcol = "white",
                     cglcol = "#b3b3b3",
                     col = "#b3b3b3",
                     caxislabels = c(0, 0.25, 0.5, 0.75, 1),
                     title = selected_artist)
    return(rc)
  })
  
  
  #### TOP SONGS ####
  
  SHfilteredSongs <- reactive({
    SH %>% 
      filter(person == input$user, year == input$year, as.numeric(month) >= input$Months[1] & as.numeric(month) <= input$Months[2]) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(time = sum(ms_played) / 60000) %>% 
      arrange(-time) %>% 
      head(10) %>% 
      na.omit()
  })
  
  output$topSongs <- renderPlotly({
    plot_ly(SHfilteredSongs(),
            x = ~time,
            y = ~reorder(master_metadata_track_name, time),
            type = "bar",
            marker = list(color = '#1DB954'),
            orientation = 'h') %>%
      layout(
        xaxis = list(title = "Minutes Listened"),
        yaxis = list(title = list(text = "Track Name")),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        bargap = 0.1,
        font = list(color = 'white', family = "Gotham")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #### VIOLIN FEATURES ####
  output$violin <- renderPlot({
    SongsFeaturesfiltered <- Songs %>% 
      select(danceability:valence, person) %>% 
      select(-c(mode, key, loudness)) %>% 
      filter(person == input$user) %>% 
      na.omit()
    
    # par(bg = "#121212", col = "white", family = "Gotham")
    gv <- ggplot(SongsFeaturesfiltered, aes(x = person, y = !!sym(input$parameter))) +
      geom_violin(fill = "#1DB954", color = "#1DB954", alpha = 0.7) +
      coord_flip() +
      labs(title = paste("Distribution of songs by", input$parameter), y = input$parameter) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, colour = "white", family = "Gotham"),
        axis.text.x = element_text(colour = "white", size = 12, family = "Gotham"),         
        axis.text.y = element_text(colour = "white", size = 12, family = "Gotham"),         
        text = element_text(size = 13, colour = "white", family = "Gotham"),        
        legend.title = element_blank(),        
        panel.background = element_rect(fill = "#121212"),       
        plot.background = element_rect(fill = "#121212"),         
        legend.background = element_rect(fill = "#121212")     
      )
    return(gv)
  })
  
  
  
  ######   PLAYLIST     #######
  
  observe({
    # Update the selected values in checkboxGroupInput based on the selected user
    updateCheckboxGroupInput(session, "selected_people", selected = input$user)
  })
  
  output$song_list_output <- renderUI({
    
    playlist_data <- playlist %>%
      filter(Who %in% input$selected_people) %>%
      group_by(artistName, trackName) %>%
      summarise(
        people = n(),
        avg_count = round(mean(count)),
        avg_time = mean(time),
        image = coalesce(first(image), first(na.omit(image)), "default_value"),
        .groups = "drop"
      ) %>%
      arrange(-people, -avg_count, -avg_time)
    
    selected_songs <- playlist_data[1:input$song_count_slider, c("artistName", "trackName", "people", "avg_count", "image")]
    
    formatted_songs <- paste0(
      "<div class='song-item' data-toggle='tooltip' data-placement='top' title='Listened ",
      round(selected_songs$avg_count, 2),
      " times on average, by ",
      selected_songs$people,
      " people'>",
      "<div style='display: flex; align-items: center;'>",  # Flex container
      "<img src='", selected_songs$image, "' style='width: 7.5vh; height: 7.5vh; margin-right: 1.5vh; border-radius: 5px;'>",
      "<div style='text-align: left;'>",  # Nested div for text, align left
      "<span style='font-weight: bold;'>", seq_along(selected_songs$trackName), ".</span> ",
      paste(selected_songs$artistName, selected_songs$trackName, sep = " - "),
      "</div>",
      "</div>",
      "</div>"
    )
    
    formatted_songs <- paste0("<div style='font-family: Gotham, sans-serif; color: #FFFFFF; cursor: pointer; font-size: 18px;'>", formatted_songs, "</div>")
    
    HTML(formatted_songs)
  })
  
  ####   Animacja   ####
  
  change_year <- function(year) {
    updateCheckboxInput(session, inputId = "year", value = year)
  }
  
  observeEvent(input$backward,{
    new <- case_when(
      input$user == 'Karolina' ~ 'Filip',
      input$user == 'Bartek' ~ 'Karolina',
      input$user == 'Filip' ~ 'Bartek')
    updateSliderTextInput(
      session,
      "user",
      selected = new
    )
  })
  
  observeEvent(input$forward,{
    new <- case_when(
      input$user == 'Karolina' ~ 'Bartek',
      input$user == 'Bartek' ~ 'Filip',
      input$user == 'Filip' ~ 'Karolina')
    updateSliderTextInput(
      session,
      "user",
      selected = new
    )
  })
  
  
  ####   Nagłówki, opisy ####
  
  output$wrapped_title <- renderUI({
    div(
      h1(
        paste0("Custom Wrapped for: ", input$user),
        class = "text-fav"),
      h3(
        paste0("from ", input$Months[1], "-", input$year, " to ", input$Months[2], '-', input$year),
        class = "text-fav",
        style = "margin-top:-5px;"))
  })
  
  
  output$compatibility_title <- renderUI({
    div(h1("Compatibility"),
        class = "text-fav")
  })
  
  
  output$playlist_title <- renderUI({
    div(h1("Playlist"),
        class = "text-fav")
  })
  
  output$summary_title <- renderUI({
    div(h1("Summary"),
        class = "text-fav")
  })
  
  
  ####   Footer   ####
  
  output$left_caption <- renderUI({
    user_caption <- case_when(
      input$user == 'Karolina' ~ 'karo',
      input$user == 'Bartek' ~ 'KochamAK',
      input$user == 'Filip' ~ 'FylypO')
    div(
      h4(paste0(user_caption), style = "height: 100%; margin: 5px;"),
      h5(paste0(input$user), style = "height: 100%; margin: 5px; color: #909090;")
    )
  })
  
  
  output$heart <- renderUI({
    tags$a(style = "z-index:100; cursor:pointer; margin-left: 10px; color: white;",
           href = "https://www.youtube.com/watch?v=ISoCbWBqsYI",
           target="_blank",
           icon(id = "heart", "heart")
    ) 
  })
  
  output$our_cover <- renderUI({
    user_image <- case_when(
      input$user == 'Karolina' ~ 'https://i.pinimg.com/originals/7d/53/38/7d5338d2a67464493b745c9a417aebf7.jpg',
      input$user == 'Bartek' ~ 'https://media.npr.org/assets/img/2015/09/23/ap_836720500193-13f1674f764e5180cf9f3349cfef258d181f2b32-s1100-c50.jpg',
      input$user == 'Filip' ~ 'https://i1.sndcdn.com/artworks-6vjkU2fWoGjyNCCs-toATmQ-t500x500.jpg')
    img(
      src = user_image,
      class = "our-cover",
      height = "8.5vh",
      width = "8.5vh"
    )
  })
  
  
}


shinyApp(ui, server)
