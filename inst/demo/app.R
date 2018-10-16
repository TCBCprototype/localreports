library(shiny)
library(shinydashboard)
library(localreports)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      selectInput("species", "Species:",
             c(sort(unlist(lapply(atrisk, "[[", "species_name"))))),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Charts", icon = icon("bar-chart-o"),
        menuSubItem("Sub-item 1", tabName = "subitem1"),
        menuSubItem("Sub-item 2", tabName = "subitem2")
      )
    ),
    textOutput("res")
  ),
  dashboardBody(
    uiOutput("speciesTitle"),
    h2("Range map"),
    p("Map of known samples from Tennessee (using information from GBIF) as black dots, with predicted range suitability mapped with bioclim)."),
  #  textOutput("BHL"),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", width=500, height = 250))
    ),
    h2("OneZoom tree"),
    p("Placement of this species on a phylogenetic tree. Zoom out to see its relatives, click on the name for more information on this species"),
    htmlOutput("onezoom")
    # tags$iframe(
    #                     seamless = "seamless",
    #                     src = "https://forecast.io/embed/#lat=42.3583&lon=-71.0603&name=Downtown Boston",
    #                     height = 800, width = 1400
    # )
  )
)

server <- function(input, output, session) {
  chosen_species <- reactive({which(unlist(lapply(atrisk, "[[", "species_name"))==input$species)})
  species_name <- reactive({atrisk[[chosen_species()]]$species_name})
  output$speciesTitle <- renderUI({h1(species_name())})
#  output$BHL <- reactive({HTML((paste0("Publications about this species may be available <a href='https://www.biodiversitylibrary.org/search?searchTerm=", tolower(gsub(" ", "+", species_name())), "&stype=F#/titles'>here</a>")))})
  output$plot1 <- renderPlot({
    localreports::lr_prediction_plot(atrisk[[chosen_species()]]$prediction)
  })
  onezoom_url <- reactive({
    isolate(paste0("http://www.onezoom.org/life/@", gsub(" ", "_", species_name())))
  })
  output$onezoom <- renderUI({
    tags$iframe(id = "onezoom", src = onezoom_url(), width = 700, height=700)
  })
}

shinyApp(ui, server)
