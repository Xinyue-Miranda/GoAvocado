library(tidyverse)
library(shiny)
library(shinydashboard)

recipe <- read.csv("data/avocado_recipe.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(-X) %>%
  janitor::clean_names()

feature <- c(
  "New", "Popular", "Quick & Easy", "Heart-Healthy",
  "Guacamoles & Dips", "Fall", "Summer"
)
health <- c(
  "Vegan", "Vegetarian", "Meals Under 500 Calories", "Snacks Under 200 Calories", "Dairy-Free",
  "Egg-Free", "Soy-Free", "Gluten-Free", "Grain-Free", "Fiber-Rich", "Good Source of Protein",
  "No Added Sugars", "Low Sodium"
)

ui <- dashboardPage(
  dashboardHeader(title = "Avocado Recipe"),
  
  dashboardSidebar(
    hr(),
    sidebarMenu(
      id = "tabs",
      menuItem("Search", tabName = "search", icon = icon("search"), selected = TRUE),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Codes",
               icon = icon("file-text-o"),
               menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
               menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
      ),
      menuItem("About", tabName = "about", icon = icon("question"))
    ),
    hr()
  ),
  
  dashboardBody(
    tabItems(
      # TAB -- SEARCH
      tabItem(
        tabName = "search",
        fluidRow(
          column(
            width = 4,
            
            tabBox(
              width = NULL,
              tabPanel(
                h5("Calories and Time"),
                sliderInput("calories", "Calories Range",
                            min = min(recipe$calories, na.rm = TRUE),
                            max = max(recipe$calories, na.rm = TRUE),
                            value = c(200, 1200)
                ),
                sliderInput("time", "Cooking Time Range",
                            min = min(recipe$time_minutes, na.rm = TRUE),
                            max = max(recipe$time_minutes, na.rm = TRUE),
                            value = c(0, 100)
                )
              ),
              tabPanel(
                h5("Feature labels"),
                checkboxGroupInput("feature", label = "feature", choices = feature)
              ),
              tabPanel(
                h5("Health labels"),
                checkboxGroupInput("health", label = "health", choices = health)
              )
            ),
            
            # Run button
            actionButton("run", "Go Avocado!")
          ),
          
          # Main panel
          box(
            title = "Filter Results", solidHeader = TRUE,
            uiOutput("selections"),
            br(),
            uiOutput("recipes")
          )
        )
      ),
      
      # TAB -- TABLE
      tabItem(
        tabName = "table",
        box(
          width = NULL, solidHeader = TRUE, title = "Table",
          tableOutput("table")
        )
      ),
      
      # TAB -- CODE
      tabItem(
        tabName = "ui",
        box(
          width = NULL, solidHeader = TRUE, title = "ui.R",
          pre(includeText("dashboard_file/ui.R"))
        )
      ),
      tabItem(
        tabName = "server",
        box(
          width = NULL, solidHeader = TRUE, title = "server.R",
          pre(includeText("dashboard_file/server.R"))
        )
      ),
      
      # TAB -- README
      tabItem(
        tabName = "about",
        includeMarkdown("dashboard_file/about.Rmd")
      )
    )
  ),
  skin = "green"
)