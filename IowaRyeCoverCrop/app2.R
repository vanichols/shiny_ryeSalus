#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggthemes)
library(shiny)
library(tidyverse)
library(maps)
library(shinythemes)

ccbio <- read_csv("create_shiny_data/IA_ccbio-map.csv") %>% 
    mutate(subregion = str_to_title(subregion),
           prob_nice = case_when(
               grepl("20", prob) ~ "20% Probability",
               grepl("50", prob) ~ "50% Probability",
               grepl("80", prob) ~ "80% Probability"),
           ccbio_lbs = round(ccbio * 2.2/2.47, 0)
           )

#--dropdowns
dd_county <- ccbio %>% select(subregion) %>% distinct() %>% pull() 
dd_dop <- ccbio %>% select(dop2) %>% distinct() %>% pull()
dd_dot <- ccbio %>% select(DOY2) %>% distinct() %>% pull()



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    
    # Application title
    navbarPage("Iowa Cover Crop Biomass Explorer",
    
    tabPanel(
        "Rye Cover Crop Biomass Production",
       
        plotOutput('plot'),
        
        hr(),
        
        fluidRow(
            column(4,
                   selectizeInput('county', 'Choose a county:', dd_county),
                   br(),
                   selectizeInput('dop', 'Choose a planting date:', dd_dop),
                   br(),
                   selectizeInput('dot', 'Choose a termination date:', dd_dot),
            ))        
        ),
    tabPanel(
        "About",
        fluidRow(
            column(6,
                   includeMarkdown("about.md")
            )
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    dataset1 <- reactive({
        ccbio %>% 
            filter(dop2 == input$dop,
                   DOY2 == input$dot)
    })
    
    dataset2 <- reactive({
        ccbio %>% 
            filter(dop2 == input$dop,
                   DOY2 == input$dot,
                   subregion == input$county)
    })
    
    dataset3 <- reactive({
        ccbio %>% 
            filter(dop2 == input$dop,
                   DOY2 == input$dot,
                   subregion == input$county) %>% 
            group_by(prob_nice) %>% 
            summarise(long = mean(long),
                      lat = mean(lat),
                      ccbio_lbs = mean(ccbio_lbs, na.rm = T)) %>% 
            distinct()
        
    })
    
    
   

    output$plot <- renderPlot({
        
      
        
        ggplot() + 
            #geom_polygon(data = dataset1(), aes(x=long, y = lat, group = group, fill = ccbio)) + 
            geom_polygon(data = dataset1(), aes(x=long, y = lat, group = group), fill = "gray80", color = "white", lwd = 0.5) + 
            geom_polygon(data = dataset2(),
                         aes(x=long, y = lat, group = group, fill = ccbio_lbs),
                         color = "red",
                         #fill = NA,
                         lwd = 1.5) +
            geom_label(data = dataset3(), 
                       aes(x = long, y = lat+0.5, label = paste(ccbio_lbs, "lbs/ac")),
                       size = 10) +
            facet_grid(.~prob_nice) + 
            scale_fill_viridis_c() +
            scale_fill_gradient(low = "lightgreen", high = "green4") +
            ggthemes::theme_few() + 
            theme(legend.position = "top",
                  panel.background = element_rect(fill = "white"),
                  aspect.ratio = 0.8,
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  strip.text = element_text(size = rel(1.5), face = "bold"),
                  plot.background = element_rect(fill = "transparent", colour = NA),
                  panel.grid = element_blank(),
                  panel.border = element_blank()) +
            guides(fill = F)
            # guides(fill = guide_colorbar(title.position = "top",
            #                              title.hjust = 0.5,
            #                              barwidth = 20,
            #                              barheight = 0.8))  
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
