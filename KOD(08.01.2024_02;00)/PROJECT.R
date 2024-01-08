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
          font-family: "Open Sans", sans-serif;
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
        justify-content: center;
        background-color: #000;
        flex-direction: row;
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 12vh;
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
        align-items: center;
        justify-content: center
        z-index: 2000;
      }

      .icons {
        width: 50%;
        margin-top: 3%;
        display: flex;
        justify-content: center;
        flex-direction: row;
      }
      
      .slider {
        margin-top: -9%;
        width: 100%;
        z-index: 2000;
        border-radius: 10px;
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

      .slider .irs-from, .slider .irs-to, .slider .irs-single, .slider2 .irs-from, .slider2 .irs-to, .slider2 .irs-single {
        color: #909090;
        text-shadow: none;
        background-color:#000;
        border-radius: 25px;
        font-size: 10px;
      }

      .slider .irs-min, .slider .irs-max, .slider2 .irs-min, .slider2 .irs-max {
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

      .icons2 {
        display: flex;
        width: 40%;
        flex-direction: row;
      }
      '


####   Sraka   ####


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
            # Tutaj kod dla playlist
            uiOutput("playlist_title"))),
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
          icon("shuffle", style = "margin: auto; font-size: 1.5em; color: #909090"),
          icon("backward-step",  id = "backward", style = "z-index:4000; cursor:pointer; margin: auto; padding: 10px; border-radius: 5px; font-size: 1.5em; color: #909090"),
          icon("pause-circle", style = "margin: auto; font-size: 2em;", class = "fas"),
          icon("forward-step",  id = "forward", style = "z-index:4000; cursor:pointer; margin: auto; padding: 10px; border-radius: 5px; font-size: 1.5em; color: #909090"),
          icon("repeat", style = "margin: auto; font-size: 1.5em; color: #909090"),
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
          icon("square-poll-vertical", style = "margin: auto; font-size: 1.2em; color: #909090"),
          icon("microphone", style = "margin: auto; font-size: 1.2em; color: #909090"),
          icon("bars", style = "margin: auto; font-size: 1.2em; color: #909090"),
          icon("speaker-deck", style = "margin: auto; font-size: 1.2em; color: #909090"),
          icon("volume-low", style = "margin: auto; font-size: 1.2em; color: #909090"),),
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
      src = paste0('C:/Users/szlin/Desktop/Programowanie/TWD/PROJEKT 2/KOD(07.01.2024_01;00)/',input$user, ".jpg"),
      class = "our-cover",
      height = "70px",
      width = "70px"
    )
  })
  
  
}


shinyApp(ui, server)
