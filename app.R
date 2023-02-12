# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Weak Points in Lineup"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "mutable",
        "Load MU table from Legna's website (https://www.llorr-stats.com/static/mu.html#diamond)",
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
    mainPanel(tableOutput('weakPoints'))),
  hr(),
  print("Questions or suggestions? Ping me at Discord! Rono#8604")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  WP <- reactive({
    inFile <- input$mutable
    if (is.null(inFile))
      return(NULL)
    
    MU <- read.csv(inFile$datapath)
    
    lineup <- c(input$lineup1, input$lineup2, input$lineup3)
    
    relevantMUs <-
      MU %>% filter(playerDeck == lineup[1] |
                      playerDeck == lineup[2] | playerDeck == lineup[3])
    weakPoints <-
      relevantMUs %>% group_by(opponentDeck) %>% summarise(
        WeakDecks = paste(playerDeck[which(muWR < 0.5)],collapse = ' / '),
        WeakPoint = sum(muWR < 0.5),
        AverageWR = mean(muWR)
      ) %>% filter(WeakPoint > 1)
    
    return(weakPoints)
  })
  observeEvent(input$mutable, {
    inFile <- input$mutable
    MU <- read.csv(inFile$datapath)
    updateSelectInput(session,
                      "lineup1",
                      label = "Select",
                      choices = unique(MU$playerDeck))
    updateSelectInput(session,
                      "lineup2",
                      label = "Select",
                      choices = unique(MU$playerDeck))
    updateSelectInput(session,
                      "lineup3",
                      label = "Select",
                      choices = unique(MU$playerDeck))
  })
  output$weakPoints <- renderTable({
    WP()
  })
}

# Run the application
shinyApp(ui = ui, server = server)