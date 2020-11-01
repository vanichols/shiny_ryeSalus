# Have different planting dates faceted
# have multiple states
# Use ggridges to show distributions (maybe not...)

library(ggridges)
library(ggthemes)
library(shiny)
library(tidyverse)
library(maps)
library(shinythemes)
library(extrafont)
library(ggrepel)



#--for summarised data
#ccbio_raw <- read_csv("MidwestRyeCoverCrop/ccbio_map.csv")
ccbio_raw <- read_csv("ccbio_map.csv")
  
ccbio_sum <- 
  ccbio_raw %>% 
  mutate(region = str_to_title(region),
         subregion = str_to_title(subregion),
         prob_nice = case_when(
           grepl("20", prob) ~ "In An Unfavorable Year",
           grepl("50", prob) ~ "In An Average Year",
           grepl("80", prob) ~ "In A Favorable Year"),
         prob_nice = factor(prob_nice, levels = c("In A Favorable Year", 
                                                  "In An Average Year",
                                                  "In An Unfavorable Year")),
         prob_nice = fct_rev(prob_nice),
         ccbio_lbs = round(ccbio * 2.2/2.47, -1)
  )

#--why is illinois adams have decimals?
ccbio_sum %>% 
  filter(region == "Illinois",
         subregion == "Adams") ->a

#--dropdowns
dd_dop_sum <- ccbio_sum %>% select(dop2) %>% distinct() %>% pull()
dd_dot_sum <- ccbio_sum %>% select(DOY2) %>% distinct() %>% pull()

dd_county_sum <- ccbio_sum %>% select(subregion) %>% distinct() %>% pull() 
dd_state_sum <- ccbio_sum %>% select(region) %>% distinct() %>% pull() 



#--------------------------------------------
#--map
county_map <- 
  map_data("county") %>% 
  as_tibble() %>% 
  filter(region %in% c("iowa", "illinois", "indiana", 
                       "minnesota", "michigan", "wisconsin", "ohio")) %>% 
  mutate(subregion = str_to_title(subregion))

state_map <- 
  map_data("state") %>% 
  as_tibble() %>% 
  filter(region %in% c("iowa", "illinois", "indiana", 
                       "minnesota", "michigan", "wisconsin", "ohio")) %>% 
  mutate(region = str_to_title(region))



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    
    # Application title
    navbarPage("Midwest Cover Crop Biomass Explorer",
    
               #--start tab
    tabPanel(
        "Data Summaries",
       
        h1("How much cover crop biomass can I grow?"),
        h4("These values are based on model predictions using 30 years of historial weather data"),
        h4("For more information please see the About tab"), 
        fluidRow(
          #--first bit
          column(3,
                 includeMarkdown("desc_sum.md"),
                   br(),
                 selectizeInput(inputId = "state_sum", 
                                label = "Choose a state:",
                                selected = "Iowa",
                                choices = dd_state_sum),  
                 selectizeInput(inputId = "county_sum", 
                                label = "Choose a county:",
                                selected = "Adair",
                                choices = dd_county_sum),
                 selectizeInput(inputId = "dop_sum",
                                label = "Choose a planting date:",
                                selected = "Oct-7",
                                choices = dd_dop_sum),
                 selectizeInput(inputId = "dot_sum",
                                label = "Choose a termination date:",
                                selected = "Apr-15",
                                choices = dd_dot_sum),
                 img(src = "popcan2.png", height = 300, width = 200),
                 includeMarkdown("popcan.md")),
          column(9,
        
        #h1("Cover Crop Biomass Production (lbs/ac)"),
        plotOutput('fig_sum', width = '100%', height = '100%'),
        
        ))),
    #--end tab
    
    #--start tab
    tabPanel(
        "About",
        fluidRow(
            column(12,
                   includeMarkdown("about.md")
            )
    )
    )
    #--end tab

)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    #-------------------summaries---------------------
    
    #--puts counties in selected state
    dataset1 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum,
               region == input$state_sum)
    })
    
    #--highlights the selected county
    dataset2 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum,
               region == input$state_sum,
               subregion == input$county_sum)
    })
    
    #--displays mean values for that county(?)
    dataset3 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum,
               region == input$state_sum,
               subregion == input$county_sum) %>% 
        group_by(prob_nice) %>% 
        summarise(long = mean(long),
                  lat = mean(lat),
                  ccbio_lbs = round(mean(ccbio_lbs, na.rm = T), -1)) %>% 
        distinct()
      
    })
    
    
    
    output$fig_sum <- renderPlot({
      
      
      
      ggplot() + 
        geom_polygon(data = state_map, aes(x = long, y = lat, group = group), 
                     fill = "gray80", color = "white", lwd = 0.2) +
        geom_polygon(data = dataset1(), 
                     aes(x=long, y = lat, group = group), 
                     fill = "gray80", color = "white", lwd = 0.5) + 
        geom_polygon(data = dataset2(),
                     aes(x = long, y = lat, group = group),
                     color = "black", # "mediumseagreen", #seagreen springgreen4
                     fill = "gold1",
                     lwd = 1.5) +
      
        #--bad year
        geom_label(data = dataset3() %>% filter(prob_nice == "In An Unfavorable Year"),
                  aes(label = paste0(ccbio_lbs, " lbs/ac"),
                      #y = lat + 1,
                      #size = ccbio_lbs
                      ),
                  x = -94, 
                  y = 49.2,
                  size = 10,
                  color = "red") +
        geom_text(data = dataset3() %>% filter(prob_nice == "In An Unfavorable Year"),
                  label = "Unfavorable Year:",
                   x = -94, 
                   y = 50,
                   size = 10,
                   color = "red") +
        #--avg year
        geom_label(data = dataset3() %>% filter(prob_nice == "In An Average Year"),
                  aes(label = paste0(ccbio_lbs, " lbs/ac"),
                      #y = lat + 1,
                      #size = ccbio_lbs
                      ),
                  x = -89, 
                  y = 49.2,
                  size = 10,
                  color = "black") +
        geom_text(data = dataset3() %>% filter(prob_nice == "In An Average Year"),
                   label = "Average Year:",
                   x = -89, 
                   y = 50,
                   size = 10,
                   color = "black") +
      #--good year
      geom_label(data = dataset3() %>% filter(prob_nice == "In A Favorable Year"),
                aes(label = paste0(ccbio_lbs, " lbs/ac"),
                    #y = lat + 1,
                    #size = ccbio_lbs
                    ),
                x = -84, 
                y = 49.2,
                size = 10,
                color = "blue") +
        geom_text(data = dataset3() %>% filter(prob_nice == "In A Favorable Year"),
                   label = "Favorable Year:",
                   x = -84, 
                   y = 50,
                   size = 10,
                   color = "blue") +
        scale_size_continuous(range = c(5, 12)) +
        ggthemes::theme_few() + 
        theme(legend.position = "left",
              panel.background = element_rect(fill = "white"),
              aspect.ratio = 1,
              legend.text = element_text(size = rel(1.5)),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              strip.text = element_text(size = rel(1.5), face = "bold"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm") #--bigger margin on R and L
              ) +
      #  coord_quickmap(clip = "off") +
        coord_fixed(#ratio = 1.8, #xlim = NULL, ylim = NULL, #expand = TRUE, 
                    clip = "off") +
        guides(size = F) 
      
      
    }, height = 800, width = 800)
    
    # Refresh county choices for a state selection (not working)
    observe({

      mystate <- input$state_sum

      newCounties <- 
        ccbio_sum %>%
        filter(region == mystate) %>%
        select(subregion) %>% distinct() %>% pull()

      updateSelectizeInput(session, 
                   inputId = "county_sum", 
                   label = "Choose a county:",
                   choices = newCounties,
                   selected = newCounties[1])

    })

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
