#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

header <- dashboardHeader(title = "TMDB Movies")

sidebar <- dashboardSidebar(sidebarUserPanel("Vlad Lee", image = 'NYCDSA.png'),
  sidebarMenu(
    menuItem("Home Page", tabName = "homepage", icon = icon("house")),
    menuItem("Explore The Data", tabName = "explore_data", icon = icon("database"),
             badgeColor = "green"),
    menuItem("Top 10 Genres", tabName = "top10genres", icon = icon("medal")),
    menuItem("Movies by Year and Genre", tabName = "by_year_genre",icon = icon("film")),
    menuItem("Ratings", tabName = "ratings", icon = icon("star")),
    menuItem("About", tabName = "about", icon = icon("circle-info"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    tabItem(tabName = "homepage",
            h2("Welcome to the TMDB Movies App!:"),
            box(
              title = "Background",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              HTML("
              <p>The purpose of this app is to allow users to analyze trends in movies. The data used
              in this app includes all movies listed on TMDB from 1900 to 2024. The dataset is available
              through <a href='https://www.kaggle.com/datasets/asaniczka/tmdb-movies-dataset-2023-930k-movies' 
              target='_blank'>Kaggle</a></p>
              "
              )
            )
    ),
    tabItem(tabName = "explore_data",
            fluidRow(
              column(4, 
                     selectInput("lang", "Language:", sort(c("All",
                                                             unique(as.character(movies_shiny$main_spoken_lang)))))),
              column(4, 
                     selectInput("genre", "Genre:", c("All",
                                                      sort(unique(as.character(movies_shiny$main_genre)))))),
              column(4,
                     selectInput("prod_country", "Production Country:", sort(c("All",
                                                                          unique(as.character(movies_shiny$main_prod_country)))))),
              column(4, 
                     dateRangeInput("dateRange", 
                                    "Release Date Range (yyyy-mm-dd):",
                                    start = min(movies_shiny$release_date), 
                                    end = max(movies_shiny$release_date),
                                    min = min(movies_shiny$release_date),
                                    max = max(movies_shiny$release_date),
                                    format = "yyyy-mm-dd")
              ),
              fluidRow(
                column(12, actionButton("reset_filters", "Reset Filters"))
              ),
              fluidRow(
                column(12, DT::dataTableOutput("table"))
              )
            )
    ),
    tabItem(tabName = "top10genres",
            fluidRow(
              column(12,
                     tabsetPanel(
                                tabPanel("By Budget", plotOutput('top20bybudget')),
                                tabPanel("By Revenue", plotOutput('top20byrevenue')),
                                tabPanel("By Profit", plotOutput('top20byprofit')),
                                tabPanel("By Number of Movies", plotOutput('top20bycount'))
                     )
              )
            )
    ),
    
    tabItem(tabName = "by_year_genre",
            tabBox(
              width = 12,
              id = "chart_tabs",
              tabPanel("By Year", 
                       fluidRow(
                         column(4, selectInput("year", "Year:", sort(c("All", unique(movies_shiny$Year))),
                                               selected = 2024)),
                         column(12, plotOutput("bar_plot_by_year"))
                       )),
              tabPanel("By Genre", 
                       fluidRow(
                         column(4, selectInput("genre2", "Genre:", c("All", sort(unique(as.character(movies_shiny$main_genre)))),
                                               selected = "All")),
                         column(12, plotOutput("bar_plot_by_genre"))
                       ))
            )
    ),
    tabItem(tabName = "ratings",
            tabBox(
              width = 12,
              id = "chart_tabs",
              tabPanel("Distribution", plotOutput('ratings_dist')),
              tabPanel("Summary Statistics", DT::DTOutput("summary_table")),
              tabPanel("Ratings Heatmap", plotOutput('ratings_heatmap')),
              tabPanel("Correlation Matrix", DT::DTOutput("corr_matrix"))
            )
    ),
    tabItem(tabName = "about",
            h2("About me:"),
            box(
              title = "Information",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              HTML("
              <p>My name is Vlad Lee. I am an economic consultant and a student at NYC Data Science
              Academy. Feel free to check out my profile pages and github!</p>
              <p><a href='www.linkedin.com/in/vlad-lee' target='_blank'>LinkedIn</a></p>
              <p><a href='https://www.nera.com/experts/l/vladislav-lee.html?lang=en' target='_blank'>NERA</a></P>
              <p><a href='https://github.com/bomimyhomie/TMDB-Movies-Data-Analysis' target='_blank'>GitHub</a>.</p>
              <p>For questions or feedback, contact the author at 
              <a href='mailto:Vlad7984@gmail.com'>vlad7984@gmail.com</a>.</p>
              "
            )
            )
        )
  )
)


dashboardPage(header, sidebar, body)
