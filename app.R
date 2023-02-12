# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize=15*1024^2)
library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Weak Points in Lineup"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      htmlOutput("loadLegnaFile"),
      fileInput(
        "mutable",
        "Make sure to use the newest version (e.g. mu_prevs_401.csv)",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      selectInput("lineup1", "Lineup Deck 1",
                  choices = "Upload Legna's file first!"),
      selectInput("lineup2", "Lineup Deck 2",
                  choices = "Upload Legna's file first!"),
      selectInput("lineup3", "Lineup Deck 3",
                  choices = "Upload Legna's file first!")
    ),
    # Show a plot of the generated distribution
    mainPanel(h5("If there is no data shown there are not enough matches < 40"), tableOutput('weakPoints'))),
  hr(),
  htmlOutput("questions"),
  br(),
  print("This app was first developed by Rono and i am continuing it."),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$questions <- renderUI({
    HTML("Questions or suggestions?<a href=https://github.com/JayJayBinks/lor-lineup-weakpoints target=_blank>See Github page (Link)</a>")
  })
  output$loadLegnaFile <- renderUI({
    HTML("<h4><a href=https://github.com/MaouLegna/llorr-website/tree/main/static/data target=_blank>Load MU table from Legna (Link)</a></h4>")
  })
  
  
 
  WP <- reactive({
    inFile <- input$mutable
    if (is.null(inFile))
      return(NULL)
    
    MU <- read.csv(inFile$datapath)
    
    lineup <- c(input$lineup1, input$lineup2, input$lineup3)
    
    relevantMUs <-
      MU %>% filter((archetype_1 == lineup[1] |
                      archetype_1 == lineup[2] | archetype_1 == lineup[3]) & mu_n > 40)
  
    weakPoints <-
      relevantMUs %>% group_by(archetype_2) %>% summarise(
        WeakDecks = paste(archetype_1[which(mu_wr < 0.5)],collapse = ' / '),
        WeakPoint = sum(mu_wr < 0.5),
        AverageWR = mean(mu_wr)
      ) %>% filter(WeakPoint > 1) %>% arrange(desc(WeakPoint))
    
    return(weakPoints)
  })
  observeEvent(input$mutable, {
    inFile <- input$mutable
    MU <- read.csv(inFile$datapath)
    updateSelectInput(session,
                      "lineup1",
                      label = "Select",
                      choices = unique(MU$archetype_1))
    updateSelectInput(session,
                      "lineup2",
                      label = "Select",
                      choices = unique(MU$archetype_1))
    updateSelectInput(session,
                      "lineup3",
                      label = "Select",
                      choices = unique(MU$archetype_1))
  })
  output$weakPoints <- renderTable({
    WP()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
