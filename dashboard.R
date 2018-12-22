library(tidyverse)
library(stringr)
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
      menuItem("Readme", tabName = "readme", icon = icon("question"))
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
            width = 5,
            
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
          p("Viewing more information for the filtered recipes in a table."),
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
        tabName = "readme",
        includeMarkdown("dashboard_file/readme.Rmd")
      )
    )
  ),
  skin = "green"
)



server <- function(input, output, session) {

  # model dialog
  recipeInfo <- function() {
    modalDialog(
      h4("Name"),
      textOutput("title"),
      h4("Introduction"),
      textOutput("introduction"),
      h4("Labels"),
      textOutput("characteristics"),
      h4("Cooking Time (in minutes)"),
      textOutput("time"),
      h4("Calories"),
      textOutput("calories"),
      br(),
      htmlOutput("image_url"),
      br(),
      uiOutput("url"),
      easyClose = TRUE
    )
  }

  # Get filter result
  search <- eventReactive(
    input$run, {
      print("Filter Avocado Recipes...")
      df <- recipe %>%
        filter(calories >= input$calories[1],
               calories <= input$calories[2],
               time_minutes >= input$time[1],
               time_minutes <= input$time[2]) 
      
      temp <- NULL
      if (length(input$feature) != 0) {
        for (i in 1:length(input$feature)) {
          temp <- cbind(temp, str_detect(df$characteristics, input$feature[i]))
        }
      }
      if (length(input$health) != 0) {
        for (i in 1:length(input$health)) {
          temp <- cbind(temp, str_detect(df$characteristics, input$health[i]))
        }
      }
      if (length(input$feature) != 0 | length(input$health) != 0) {
        temp <- rowSums(temp)
        df <- df %>%
          mutate(logictest = temp) %>%
          filter(logictest == (length(input$feature) + length(input$health))) %>%
          select(-logictest)
      }
      df
    }
  )


  output$table <- renderTable(
    search() %>%
      dplyr::select(-image_url)
  )


  # Get filter inputs
  get_selections <- eventReactive(
    input$run,
    list(
      paste(input$calories[1], "~", input$calories[2]),
      paste(input$time[1], "~", input$time[2]),
      input$health,
      input$feature
    )
  )

  # Show filter selections
  observeEvent(
    get_selections(),
    output$selections <- renderUI(
      fluidPage(
        h4("Filter Selections:"),
        renderText(c("Calories Range:", get_selections()[[1]])),
        renderText(c("Time Range:", get_selections()[[2]])),
        renderText(c("Health Labels:", paste(get_selections()[[3]], collapse = ", "))),
        renderText(c("Feature Labels:", paste(get_selections()[[4]], collapse = ", ")))
      )
    )
  )


  observeEvent(
    search(), {

      # Get current existing observers
      state <- reactiveValues(
        observers = list()
      )

      # Destroy existing observers
      for (i in seq_along(state$observers)) {
        state$observers[[i]]$destroy()
      }

      # Create new UI elements (fluidpage of fluidrows for links)
      output$recipes <- renderUI(
        fluidPage(
          h4("Recipes:"),
          if (nrow(search()) == 0) {
            renderText("Woops! No results matched!")
          } else {
            renderText(paste("We found", nrow(search()), "avocado recipes for you!"))
          },
          br(),br(),
          map(
            seq_len(search() %>% nrow()),
            function(i) fluidRow(
                actionLink(paste0("link", i), search()[["title"]][i])
              )
          )
        )
      )

      # Reset and create new observers for each of our links
      state$observers <- map(
        seq_len(search() %>% nrow()),
        function(i) {
          label <- paste0("link", i)
          observeEvent(
            input[[label]], # correspond to the fluidRows
            {
              cat("You clicked link ", i, "!\n", sep = "")

              # Text
              output$title <- renderText(search()[["title"]][i])
              output$introduction <- renderText(search()[["introduction"]][i])
              output$characteristics <- renderText(search()[["characteristics"]][i])
              output$time <- renderText(search()[["time_minutes"]][i])
              output$calories <- renderText(search()[["calories"]][i])

              # Image
              src <- search()[["image_url"]][i]
              if (!is.na(src)) {
                output$image_url <- renderText({
                  c('<img src="', src, '" width="450">')
                })
              } else {
                output$image <- NULL
              }

              # URL
              url <- a("Read it on loveonetoday website", href = search()[["url"]][i])
              output$url <- renderUI(tagList("Web URL:", url))

              showModal(recipeInfo())
            },
            ignoreInit = TRUE
          )
        }
      )
    }
  )
}

shinyApp(ui, server)
