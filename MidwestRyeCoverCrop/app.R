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
library(ggforce)



#--for summarised data
#ccbio_raw <- read_csv("MidwestRyeCoverCrop/ccbio_quants-map.csv")
ccbio_raw <- read_csv("ccbio_quants-map.csv")

ccbio_sum <- 
  ccbio_raw %>% 
  mutate(region = str_to_title(region),
         subregion = str_to_title(subregion),
         prob_nice = case_when(
           grepl("20", prob) ~ "Unfavorable Year",
           grepl("50", prob) ~ "Average Year",
           grepl("80", prob) ~ "Favorable Year"),
         prob_nice = factor(prob_nice, levels = c("Favorable Year", 
                                                  "Average Year",
                                                  "Unfavorable Year")),
         prob_nice = fct_rev(prob_nice),
         ccbio_lbs = round(ccbio * 2.2/2.47, -2)
  )


#--dropdowns
dd_dop_sum <- ccbio_sum %>% select(dop2) %>% distinct() %>% pull()
dd_dot_sum <- ccbio_sum %>% select(DOY2) %>% distinct() %>% pull()

dd_county_sum <- ccbio_sum %>% select(subregion) %>% distinct() %>% pull() 
dd_state_sum <- ccbio_sum %>% select(region) %>% distinct() %>% pull() 

##--practice fig

ccbio_sum %>% 
  filter(region == "Iowa", subregion == "Story", dop == 260) %>% 
  filter(prob_nice != "Average Year") %>% 
  select(region, subregion, dop2, DOY2, prob_nice, ccbio_lbs) %>%
  distinct() %>% 
  pivot_wider(names_from = prob_nice, values_from = ccbio_lbs) %>% 
  mutate(DOY2 = fct_rev(fct_inorder(DOY2))) %>% 
  ggplot() + 
  geom_point(aes(x = `Unfavorable Year`, y = DOY2), color = "red3") +
  geom_point(aes(x = `Favorable Year`, y = DOY2), color = "blue4") +
  geom_segment(aes(x = `Unfavorable Year`, xend = `Favorable Year`, 
                   y = DOY2, yend = DOY2))


ccbio_sum %>% 
  filter(region == "Iowa", subregion == "Story", dop == 260) %>% 
  filter(prob_nice != "Average Year") %>% 
  select(region, subregion, dop2, DOY2, prob_nice, ccbio_lbs) %>%
  distinct() %>% 
  pivot_wider(names_from = prob_nice, values_from = ccbio_lbs) %>% 
  mutate(DOY2 = fct_rev(fct_inorder(DOY2))) %>% 
  ggplot() + 
  geom_link(aes(x = `Unfavorable Year`, xend = `Favorable Year`, 
                y = DOY2, yend = DOY2, 
                colour = stat(index)), lineend = "round", size = 5
            ) +
  scale_colour_gradient(low = "red", high = "blue4") 
  

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
               fluidRow(h1("How much rye biomass can I grow?")),
               fluidRow(
                 column(3,
                        includeMarkdown("desc_sum.md"),
                        selectizeInput(inputId = "state_sum", 
                                       label = "Choose a state:",
                                       selected = "Iowa",
                                       choices = dd_state_sum),
                        selectizeInput(inputId = "county_sum", 
                                       label = "Choose a county:",
                                       selected = "Adair",
                                       choices = dd_county_sum),
                        selectizeInput(inputId = "dop_sum",
                                       label = "Choose a rye planting date:",
                                       selected = "Oct-7",
                                       choices = dd_dop_sum)
                        # selectizeInput(inputId = "dot_sum",
                        #                label = "Choose a rye termination date:",
                        #                selected = "Apr-15",
                        #                choices = dd_dot_sum)
                        ),
                 column(4,
                        plotOutput('fig_map', height = '60%', width = '100%')),
                 column(5,
                        plotOutput('fig_sum', height = '60%', width = '100%'))
               )
             ),
             #--end tab
             
             #--start tab
             tabPanel(
               "About",
               fluidRow(
                 column(12,
                        includeMarkdown("about.md")
                 )
               )
             ),
             #--end tab
             #--start tab
             tabPanel(
               "Pop Can",
               fluidRow(
                 column(12,
                        includeMarkdown("popcan.md"),
                        img(src = "popcan2.png", height = 700, width = 500))
                 )
               )
             #--end tab
             
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #-------------------summaries---------------------
  
  #--puts counties in selected state
  dmap1 <- reactive({
    ccbio_sum %>% 
      filter(dop2 == input$dop_sum,
             #DOY2 == input$dot_sum,
             region == input$state_sum) %>% 
      distinct()
  })
  
  #--highlights the selected county
  dmap2 <- reactive({
    ccbio_sum %>% 
      filter(dop2 == input$dop_sum,
             #DOY2 == input$dot_sum,
             region == input$state_sum,
             subregion == input$county_sum) %>% 
      distinct()
  })
  
  
  output$fig_map <- renderPlot({
    
    
    
    ggplot() + 
      geom_polygon(data = state_map, aes(x = long, y = lat, group = group), 
                   fill = "gray80", color = "white", lwd = 0.2) +
      geom_polygon(data = dmap1(), 
                   aes(x=long, y = lat, group = group), 
                   fill = "gray80", color = "white", lwd = 0.5) + 
      geom_polygon(data = dmap2(),
                   aes(x = long, y = lat, group = group),
                   color = "black", # "mediumseagreen", #seagreen springgreen4
                   fill = "gold1",
                   lwd = 1.5) +
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
        clip = "on") +
      guides(size = F) 
    
    
  }, height = 500, width = 500)
  
  
  
  dsum1 <- reactive({
    ccbio_sum %>% 
      filter(dop2 == input$dop_sum,
             region == input$state_sum,
             subregion == input$county_sum) %>% 
      filter(prob_nice != "Average Year") %>% 
      select(region, subregion, dop2, DOY2, prob_nice, ccbio_lbs) %>%
      distinct() %>% 
      pivot_wider(names_from = prob_nice, values_from = ccbio_lbs) %>% 
      mutate(DOY2 = fct_rev(fct_inorder(DOY2))) 
    
  })
  
 
  output$fig_sum <- renderPlot({
    
    ggplot(data = dsum1()) +
      geom_link(aes(x = `Unfavorable Year`, xend = `Favorable Year`, 
                         y = DOY2, yend = DOY2, 
                         colour = stat(index)), lineend = "round", size = 10) +
      scale_colour_gradient(low = "red", high = "blue4") +
      geom_vline(xintercept = 2000, linetype = "dashed") +
      guides(color = F) +
      labs(y = "Rye Termination Date",
           x = "Rye Cover Crop Biomass (lbs/ac)") + 
      theme_bw() +
      theme(axis.text = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(2))) 
    
  }, height = 500, width = 500)
  
  # Refresh county choices for a state selection (not working?)
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
