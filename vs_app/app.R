#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(visNetwork)

library(igraph)
karate <- read_graph(here("data","karate.gml"), format="gml")

karate <- set.vertex.attribute(karate, "name", 
                               value=c("Chloe", "Emily", "Aaliyah", "Emma", 
                                       "Jennifer", "Olivia", "Hannah",
                                       "Jessica", "Sarah", "Lily", "Charlotte", 
                                       "Elizabeth", "Abigail", "Rebecca",
                                       "Samantha", "Jacob", "Muhammad", "Shawn", 
                                       "Aaron", "Daniel", "Jonah", "Alex", 
                                       "Michael", "James", "Ryan", "Jordan", 
                                       "Alexander", "Ali", "Tyler", "Kevin", 
                                       "Jack", "Ethan", "Luke", "Harry"))

deg <- igraph::degree(karate)

kInt <- toVisNetworkData(karate)

nodes <- kInt$nodes
nodes$label <- rownames(nodes)
nodes$id <- rownames(nodes)

edges <- kInt$edges



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("VisNetwork Example"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slide_me", "Try Sliding Me", 
                        min = 1, max=17, value = c(1,17))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("network",height = "500px", width="600px")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$network <- renderVisNetwork({
        # minimal example
        visNetwork(nodes, edges, height="600px", width="100%") %>%
            visOptions(highlightNearest = TRUE) %>%
            visInteraction(navigationButtons = TRUE)
    })
    
    observe({
        nodes_selection <- names(deg[deg >= min(input$slide_me) & deg <= max(input$slide_me)])
        print(names(nodes_selection))
        print(nodes_selection)
        visNetworkProxy("network") %>%
            visSelectNodes(id = nodes_selection,highlightEdges = TRUE,clickEvent = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
