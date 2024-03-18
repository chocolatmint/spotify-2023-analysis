#
# Created by: https://github.com/chocolatmint
#

library(shinydashboard)
library(shinyjs)

dashboardPage(
  skin="purple",
  # ---- Title ---- 
  dashboardHeader(
    title="Spotify Analysis"
  ),
  
  # ---- Menu ---- 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="homePage", icon = icon("house")),
      menuItem("Overview", tabName="overviewPage", icon = icon("music")),
      menuItem("Artists' Popularity Analysis", tabName = "artistsPopularity", icon = icon("user")),
      menuItem("Artists' Rank Analysis", tabName = "artistsRank", icon = icon("globe")),
      menuItem("Data Set", tabName="dataSetPage", icon = icon("server"))
    )
  ),
  
  # ---- Body ---- 
  dashboardBody(
    tabItems(
      # ---- Home ----
      tabItem(
        tabName="homePage",
        fluidRow(
          box(
            width = 12,
            column(
              12,
              HTML('<div style="text-align:center"><h4>WELCOME TO SPOTIFY ANALYSIS DASHBOARD</h4></div>'),
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            column(
              12,
              HTML('<div style="text-align: justify"><p><b>BACKGROUND</b></p></div>'),
              HTML('<div style="text-align: justify"><p>Spotify is a digital music, podcast, and video service that gives you access to millions of songs and other content from creators all over the world. Millions of people from around the globe listen to Spotify daily, making it a great choice for analyzing data.</p></div>'),
              HTML('<div style="text-align: justify"><p><b>WHAT</b></p></div>'),
              HTML('<div style="text-align: justify"><p>This dashboard will analyze Spotify\'s top 50 daily charts for each country between 18th October 2023 to 26th January 2024.</p></div>'),
              HTML('<div style="text-align: justify"><p><b>HOPE</b></p></div>'),
              HTML('<div style="text-align: justify"><p>With this dashboard, I hope the dashboard\'s reader could gain insight into current songs and artists\' trends globally and also in each country.</p></div>'),
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            column(
              12,
              HTML('<div style="text-align: center"><h5>NOTICE</h5></div>'),
              HTML('<div style="text-align: center"><p>For optimal performance, we recommend using Firefox browser to open this dashboard.</p></div>'),
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            HTML('<div style="text-align: center"><a href="https://www.kaggle.com/datasets/asaniczka/top-spotify-songs-in-73-countries-daily-updated" target="_blank">Data set source</a href></div>')
          ),
          box(
            width = 6,
            HTML('<div style="text-align: center"><a href="https://github.com/chocolatmint" target="_blank">Let\'s get acquainted</a href></div>')
          )
        )
      ),
      
      # ---- Overview ----
      tabItem(
        tabName="overviewPage",
        fluidRow(
          infoBox(
            "TOTAL SONGS",
            value=length(unique(spotify$name)),
            icon=icon("music"),
            width=4,
            color="light-blue",
          ),
          #Total Artists
          infoBox(
            "TOTAL ARTISTS",
            value=length(unique(spotify$artists)),
            icon=icon("microphone"),
            width=4,
            color="blue"
          ),
          #Total Countries
          infoBox(
            "TOTAL COUNTRIES",
            value=length(unique(spotify$country)),
            icon=icon("flag"),
            width=4,
            color="purple"
          )
        ),
        fluidRow(
          shinyjs::useShinyjs(), 
          box(
            width=4,
            height = 100,
            radioButtons(
              inputId="overviewMode",
              label=h4("Choose Overview Mode"),
              choices = c("Global"="global",
                          "Country"="country"),
              selected = "global",
              inline = TRUE,
            ),
          ),
          box(
            width=8,
            height = 100,
            selectInput(
              inputId = "country",
              label = h4("Choose Country"),
              choices = unique(global$country),
            ),
          )
        ),
        fluidRow(
          box(
            width = 12,
            conditionalPanel(
              condition = "input.overviewMode === 'global'",
              plotlyOutput(outputId = "globalPlot"
              )
            ),
            conditionalPanel(
              condition = "input.overviewMode === 'country'",
              plotlyOutput(outputId = "countryPlot"
              )
            )
          ),
        ),
      ),
      
      # ---- Artists Popularity ----
      tabItem(
        tabName = "artistsPopularity",
        fluidRow(
          box(
            width=6,
            height = 100,
            selectizeInput(
              inputId = "artist_name",
              label = h4("Enter Artist Name"), 
              choices = "",
              multiple = FALSE,
              selected = NULL,
              options = list(
                create = TRUE,  
                maxOptions = 1
              )
            )
          ),
          box(
            width = 6,
            height = 100,
            dateInput(
              inputId = "date", 
              label = h4("Choose Date"),
              value = NULL,
              format = 'dd-mm-yyyy',
              min = NULL, 
              max = NULL
            )
          ),
        ),
        fluidRow(
          plotlyOutput(outputId = "artistsPlot")
        )
      ),
      
      # ---- Artists Rank ----
      tabItem(
        tabName = "artistsRank",
        fluidRow(
          box(
            width=6,
            height = 100,
            selectizeInput(
              inputId = "artist_name2",
              label = h4("Enter Artist Name"), 
              choices = "",
              multiple = FALSE,
              selected = NULL,
              options = list(
                create = TRUE,  
                maxOptions = 1
              )
            )
          ),
          box(
            width = 6,
            height = 100,
            selectInput(
              inputId = "countryDropdown",
              label = h4("Choose Country"),
              choices = unique(global$country),
            )
          ),
        ),
        fluidRow(
          plotlyOutput(outputId = "rankPlot")
        )
      ),
      
      # ---- Data Set ----
      tabItem(
        tabName = "dataSetPage",
        fluidRow(
          box(
            width = 12,
            title = "Dataset Spotify Analysis",
            dataTableOutput(outputId = "dataset")
          )
        )
      )
    )
  )
)