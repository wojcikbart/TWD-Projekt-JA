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

####   Wczytanie Danych   ####

bartekSongs <- fromJSON("./dane/extendedDane/extendedBartek.json")
karolinaSongs <- fromJSON("./dane/extendedDane/extendedKarolina.json")
filipSongs <- fromJSON("./dane/extendedDane/extendedFilip.json")

bartekSH2023 <- fromJSON("./dane/SpotifyExtendedBartek/Streaming_History_Audio_2023.json")

karolinaSH2023 <- fromJSON("./dane/SpotifyExtendedKarolina/SpotifyExtendedKarolina2023")
karolinaSH2022 <- fromJSON("./dane/SpotifyExtendedKarolina/SpotifyExtendedKarolina2022")

filipSH2023 <- fromJSON("./dane/SpotifyExtendedFilip/SpotifyExtendedFilip2023")
filipSH2022 <- fromJSON("./dane/SpotifyExtendedFilip/SpotifyExtendedFilip2022")

minutesPerWeek <- fromJSON("./dane/minutesPerWeek.json")

bartekArtists <- fromJSON("./dane/SpotifyExtendedBartek/bartekArtists.json")
karolinaArtists <- fromJSON("./dane/SpotifyExtendedKarolina/karolinaArtists.json")
filipArtists <- fromJSON("./dane/SpotifyExtendedFilip/filipArtists.json")

####   Style   ####

HTML_styles <- '
      * {
        font-family: "Gotham";
      }
      .content {
        min-height: 1000px;
      }

      body {
        color: #FFFFFF;
        background-color: #242424;
      }

      .main-sidebar .sidebar .sidebar-menu {
        position: fixed;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        width: 290px;
        height: 81%;
        background-color: transparent;
        margin: 10 auto;
        border: 2px solid #FFF;
      }
      
      .sidebar-panel{
        position: fixed;
        width: 290px;
        height: 100%;
        justify-content: space-evenly;
        align-items: center;
        background-color: #000;
        border: 2px solid #FFFFFF
        margin: 10px auto;
      }

      .sidebar-panel .sidebar-panel-upper{
        width: 100%;
        height: 50%;
        justify-content: center;
        align-items: center;
        background-color: #202020;
        margin: 0 auto;
        border: 2px solid #FFF;
      }

      .sidebar-panel .sidebar-panel-lower {
        width: 100%;
        justify-content: space-evenly;
        align-items: center;
        height: 50%;
        background-color: #202020;
        margin: 0 auto;
        border: 2px solid #FFF;
      }


      html {
        font-size: 16px;
        color: #FFFFFF !important;
      }
      .content-wrapper .content {
          background-color: #121212;
          margin-bottom: 100px;
          
      }
      .content{
        margin-bottom: 100px;
      }
      
      * {
          font-family: "Open Sans", sans-serif;
          letter-spacing: -0.35px;
      }

      skin-blue .main-sidebar {
        background-color: #040404;
      }
      .logo {
        background-color:  #040404 !important;
        position:fixed;
      }
      .navbar {
        background-color:  #040404 !important;
      }
      #sidebarCollapsed {
        background-color: #040404;
      }

      .skin-blue .main-sidebar .sidebar .sidebar-menu  a{
        border-radius: 5px;
        border-color: transparent;
      }


      .top-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
        width: 90%;
        box-shadow: 10px 5px 5px black;
        object-fit: cover;
        height: 162px;
        width: 162px;
      }


      .top-text {
        display: inline-block;
        margin-left: 20px;
        margin-bottom: 55px;
      }



      .col-sm-6 {
      width: 20%;
      min-width: 220px;
      }


      .text-fav {
        margin-left:30px;
        font-weight:bold;

      }
      
      .pretty .state label {
        color: #b8c7ce;
        height:20px;

      }


      }
      .shiny-input-checkboxgroup label~.shiny-options-group, .shiny-input-radiogroup label~.shiny-options-group {
      margin-top:20px;

      }


  .pretty .state label {
    top:0;
    margin-bottom:10px;
    margin-top:10px;

  }
  .pretty .state label:hover {
    top:0;
    margin-bottom:10px;
    margin-top:10px;
    color: #FFFFFF;
  }



  .pretty .state label:before {
  top:10px;}
  .pretty .state label:after {
  top:10px;}



      .control-label{
      font-weight:600;
      margin-bottom: 20px;
      }
      
      .footer{
        display: flex;
        justify-content: center;
        flex-direction: row;
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #000000;
        height: 12%;
        padding: 0 15px;
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
        width: 70px;
        height: 70px;
        border-radius: 10%;
        margin-right: 10px;
      }
      
      .footer-center-panel {
        margin: auto;
        width: 33%;
        display: flex;
        flex-direction: column;
        z-index: 2000;
      }

      .icons{
        width: 50%;
        margin-top: 3%;
        margin-bottom:0;
        margin-left: auto;
        margin-right: auto;
        display: flex;
        justify-content: center;
        flex-direction: row;
      }
      
      .slider {
        margin-top: -7%;
        margin-bottom: auto;
        width: 100%;
        z-index: 2000;
        border-radius: 10px;
        
      }
      
      .slider .irs-handle {
        background-color: white !important;
        height: 20px;
        width: 20px;
        top: 20px;
      }

      .slider .irs-bar {
        top: 25px;
        height: 8px;
        background: #1DB954;
      }

      .slider .irs-from, .slider .irs-to, .slider .irs-single{
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
        width: 30%;
        justify-content: right;
        flex-direction: row;
        height: 100%;
        justify-content: left;
        align-items: center;
        border: 5px solid #404040;
      }
      '


####   Sraka   ####


####   UI   ####

ui <- dashboardPage(
  title = "Jestem Akustyczny",
  skin = "green",
  dashboardHeader(
    title = span("projekt - JA")),
  dashboardSidebar(
    width = '290px',
    div(
      class = "sidebar-panel",
      div(
        div(
          class = "sidebar-panel-upper",
          h3("Choose year", style = "margin-left: 3px"),
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
        class = "sidebar-panel-lower",
        sidebarMenu(
          id = "tabs",
          h3("Features", style = "margin-left: 3px;"),
          menuItem("  Wrapped", tabName = "wrapped", icon = icon('music')),
          menuItem("  Compatibility", tabName = "compatibility", icon = icon('link')),
          menuItem("  Playlist", tabName = "playlist", icon = icon('headphones')),
          menuItem("  Summary", tabName = "summary", icon = icon('music')))))),
  dashboardBody(
    tags$head(tags$style(HTML(HTML_styles))),
    tabItems(
      tabItem(
        tabName = "wrapped",
        fluidPage(
          # Tutaj kod dla wrapped
          uiOutput("wrapped_title")),
          fluidRow(h3("Average minutes listened per day of the week by month", class = "text-fav"),
                 plotlyOutput("minutesPerDayOfWeek"))),
      tabItem(
        tabName = "compatibility",
        fluidPage(
          # Tutaj kod dla compatibility
          uiOutput("compatibility_title"))),
      tabItem(
        tabName = "playlist",
        fluidPage(
          # Tutaj kod dla playlist
          uiOutput("playlist_title"))),
      tabItem(
        tabName = "summary",
        fluidPage(
          # Tutaj kod do podsumowania
          uiOutput("summary_title")))),
    div(
      div(
        class = "footer-left-panel",
        uiOutput("our_cover"),
        uiOutput("left_caption"),
        uiOutput("heart")),
      div(
        class = "footer-center-panel",
        div(
          icon("shuffle", style = "margin: auto;"),
          icon("backward",  id = "backward", style = "z-index:4000; cursor:pointer; margin: auto; padding: 10px; border-radius: 5px;"),
          icon("pause-circle", style = "margin: auto; font-size: 2em;", class = "fas"),
          icon("forward",  id = "forward", style = "z-index:4000; cursor:pointer; margin: auto; padding: 10px; border-radius: 5px;"),
          icon("repeat", style = "margin: auto;"),
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
        sliderTextInput(
          "user",
          NULL,
          choices = c("Karolina", "Bartek", "Filip"),
          selected = "Karolina")),
      class = "footer")))


####   Server   ####


server = function(input, output, session) {
  
  ####   Praca na Danych   ####
  # minutes per day of week
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
  
  
  ####   Animacja   ####
  
  change_year <- function(year) {
    updateCheckboxInput(session, inputId = "year", value = year)
  }
  
  
  observeEvent(input$forward, {
    val <- input$Months
    val[2] <- val[2] + 1
    updateSliderInput(
      session,
      "Months",
      value = val,
      min = 1,
      max = 12
    )
  })
  
  observeEvent(input$backward, {
    val <- input$Months
    val[1] <- val[1] - 1
    updateSliderInput(
      session,
      "Months",
      value = val,
      min = 1,
      max = 12
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
    div(
      h4(paste0(input$user), style = "height: 100%; margin: 5px;"),
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
    img(
      src = paste0(input$user, ".jpg"),
      class = "our-cover",
      height = "70px",
      width = "70px"
    )
  })
  
  
}


shinyApp(ui, server)
