#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

ccbio <- read_csv("create_shiny_data/IA_ccbio-map.csv")

dd_county <- ccbio %>% select(subregion) %>% distinct() %>% pull() %>% str_to_title()

dd_dop <- ccbio %>% select(dop2) %>% distinct() %>% pull()
dd_dot <- ccbio %>% select(DOY2) %>% distinct() %>% pull()


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    title = "Iowa Cover Crop Biomass Explorer",
    
    plotOutput('plot'),
    
    hr(),
    
    fluidRow(
        column(4,
               selectizeInput('county', 'Choose a county:', dd_county)
        ),
        column(4,
               selectizeInput('dop', 'Choose a planting date:', dd_dop)
        ),
        column(4,
               selectizeInput('dot', 'Choose a termination date:', dd_dot)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    dataset <- reactive({
        ccbio %>% 
            filter(dop2 == input$dop,
                   DOY2 == input$dot)
    })
    
    
    output$plot <- renderPlot({
        
        ggplot(dataset(), aes(long, lat, fill = ccbio)) + 
            geom_polygon(aes(x=long, y = lat, group = group)) + 
            # geom_polygon(data = state_poly, 
            #              aes(x=long, y = lat, group = group), 
            #              fill = alpha("white",0.1),
            #              colour = "black", lwd = 0.5) + 
            facet_grid(.~prob) + 
            scale_fill_viridis_c() +
            guides(fill = guide_colorbar(title.position = "top",
                                         title.hjust = 0.5,
                                         barwidth = 20,
                                         barheight = 0.8)) +
            theme(legend.position = "top")
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
