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
      selectizeInput("lineup1", "Lineup Deck 1",
                  choices = "Wait until Mu Tale downloaded..."),
      selectizeInput("lineup2", "Lineup Deck 2",
                  choices = "Wait until Mu Tale downloaded..."),
      selectizeInput("lineup3", "Lineup Deck 3",
                  choices = "Wait until Mu Tale downloaded...")
    ),
    # Show a plot of the generated distribution
    # If there is no data shown there are not enough matches (MatchupsCount < 10)
    mainPanel(h5("Pay attention to the MatchupsCount, few matches mean insignifcant data."), tableOutput('weakPoints'))),
  hr(),
  htmlOutput("questions"),
  br(),
  print("This app was first developed by Rono and i am continuing it."),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$questions <- renderUI({
    HTML("Questions or suggestions? Create an issue at the <a href=https://github.com/JayJayBinks/lor-lineup-weakpoints target=_blank>Github page (Link)</a>")
  })
  print("123")
  MU <- read.csv("https://raw.githubusercontent.com/MaouLegna/llorr-website/main/static/data/mu_prevs_401.csv")
  updateSelectizeInput(session,
                       "lineup1",
                       label = "Select",
                       server = TRUE,
                       choices = unique(MU$archetype_1))
  updateSelectizeInput(session,
                       "lineup2",
                       label = "Select",
                       server = TRUE,
                       choices = unique(MU$archetype_1))
  updateSelectizeInput(session,
                       "lineup3",
                       label = "Select",
                       server = TRUE,
                       choices = unique(MU$archetype_1))
 
  WP <- reactive({
    lineup <- c(input$lineup1, input$lineup2, input$lineup3)
    
    relevantMUs <-
      MU %>% filter((archetype_1 == lineup[1] |
                      archetype_1 == lineup[2] | archetype_1 == lineup[3]))
  
    weakPoints <-
      relevantMUs %>% group_by(archetype_2) %>% summarise(
        WeakDecks = paste(archetype_1[which(mu_wr < 0.5)],collapse = ' / '),
        WeakPoint = sum(mu_wr < 0.5),
        AverageWR = mean(mu_wr),
        MatchupsCount = sum(mu_n)
      ) %>% filter(WeakPoint > 1) %>% filter(MatchupsCount >= 5) %>% arrange(desc(MatchupsCount))
    
    return(weakPoints)
  })
  output$weakPoints <- renderTable({
    WP()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
