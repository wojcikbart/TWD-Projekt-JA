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

####   Wczytanie Danych   ####

Songs <- fromJSON("../dane/Songs.json")

SH <- fromJSON("../dane/SpotifyExtendedAll.json")

minutesPerWeek <- fromJSON("../dane/minutesPerWeek.json")

playlist <- fromJSON("../dane/playlistData.json")


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
          font-family: "Gotham", sans-serif;
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
            uiOutput("wrapped_title"))),
        tabItem(
          tabName = "compatibility",
          fluidPage(
            # Tutaj kod dla compatibility
            uiOutput("compatibility_title"))),
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
