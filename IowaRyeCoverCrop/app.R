# Have different planting dates faceted
# Use ggridges to show distributions

library(ggthemes)
library(shiny)
library(tidyverse)
library(maps)
library(shinythemes)


# #--for running through script
# ccbio <- 
#   read_csv("create_shiny_data/IA_ccbio-map-raws.csv") %>% 
#     mutate(subregion = str_to_title(subregion),
#            ccbio_lbs = round(ccbio_kgha * 2.2/2.47, 0),
#            ccbio_lbs = ifelse(ccbio_lbs <0, 0, ccbio_lbs),
#            dop_nice = factor(dop, labels = c("Sep-15", "Oct-7", "Nov-1")),
#            dot_nice = factor(dot, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15", "Jul-1")),
#            dot_nicerev = fct_rev(dot_nice)
#            ) 
#ccbio

#--for running with 'run app'
ccbio <- 
  read_rds("IA_ccbio-map-raws.rds") %>% 
  mutate(subregion = str_to_title(subregion),
         ccbio_lbs = round(ccbio_kgha * 2.2/2.47, 0),
         ccbio_lbs = ifelse(ccbio_lbs <0, 0, ccbio_lbs),
         dop_nice = factor(dop, labels = c("Sep-15 (Early)", "Oct-7 (Expected)", "Nov-1 (Late)")),
         dot_nice = factor(dot, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15", "Jul-1")),
         dot_nicerev = fct_rev(dot_nice)
  ) 

county_map <- 
  map_data("county") %>% 
  as_tibble() %>% 
  filter(region == "iowa") %>% 
  mutate(subregion = str_to_title(subregion))
  

#--dropdowns
dd_county <- ccbio %>% select(subregion) %>% distinct() %>% pull() 


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    
    # Application title
    navbarPage("Iowa Cover Crop Biomass Explorer",
    
    tabPanel(
        "Rye Cover Crop Biomass Production",
       
        
        fluidRow(
          column(4,
                 selectizeInput(inputId = "county", 
                                label = "Choose a county:",
                                selected = "Story",
                                choices = dd_county),
                 includeMarkdown("desc.md")),
          column(8, 
                 plotOutput('fig_map', width = 400, height = 350)),
          # column(4, 
          #        includeMarkdown("desc.md"))
          # 
          
        ), 
        
        plotOutput('fig_dens', height = 500),
        
        hr()
        ),
    tabPanel(
        "About",
        fluidRow(
            column(12,
                   includeMarkdown("about.md")
            )
    )
)
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
  #--which county to highlight on map
    dataset1 <- reactive({
        county_map %>% 
            filter(subregion == input$county)
    })
    
    #--which county to display data fro
    dataset2 <- reactive({
      ccbio %>% 
        filter(subregion == input$county)
    })

    #--map showing highlighted county
    output$fig_map <- renderPlot({
        
      
        ggplot() + 
            geom_polygon(data = county_map, aes(x=long, y = lat, group = group), 
                         fill = "gray80", color = "white", lwd = 0.5) + 
            geom_polygon(data = dataset1(),
                         aes(x=long, y = lat, group = group),
                         color = "black", # "mediumseagreen", #seagreen springgreen4
                         fill = "gold1",
                         lwd = 1.5) +
        ggthemes::theme_few() + 
        coord_cartesian() + 
        theme(panel.background = element_rect(fill = "white"),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
              aspect.ratio = 0.7,
                  plot.background = element_rect(fill = "transparent", colour = NA),
                  panel.grid = element_blank(),
                  panel.border = element_blank()) 
        
        
    })
    
    output$fig_dens <- renderPlot({
      
      
      ggplot(data = dataset2(), aes(x = ccbio_lbs, y = dot_nicerev, fill = stat(x))) + 
        geom_density_ridges_gradient(scale = 2) +
        geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
        scale_fill_viridis_c(option = "C") +
        facet_grid(.~dop_nice, scales = "free") + 
        labs(x = "Cover Crop Biomass (lbs/ac)",
             y = "Termination Date") +
        guides(fill = F) +
        theme(axis.title = element_text(size = rel(2.5)),
              axis.text.x = element_text(size = rel(2.3)),
              axis.text.y = element_text(size = rel(2.5)),
              strip.text = element_text(size = rel(2.5), face = "bold")
              ) +
        theme_bw()
      
      
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
