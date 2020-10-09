# Have different planting dates faceted
# Use ggridges to show distributions

library(ggridges)
library(ggthemes)
library(shiny)
library(tidyverse)
library(maps)
library(shinythemes)
library(extrafont)
library(ggrepel)

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


#--------------------------------------------
#--for data distributions
ccbio_dist <- 
  read_rds("IA_ccbio-map-raws.rds") %>% 
  filter(dot < 183) %>% #--jul-1 not realistic
  mutate(subregion = str_to_title(subregion),
         ccbio_lbs = round(ccbio_kgha * 2.2/2.47, 0),
         ccbio_lbs = ifelse(ccbio_lbs < 0, 0, ccbio_lbs),
         dop_nice = factor(dop, labels = c("Planted Sep-15", "Planted Oct-7", "Planted Nov-1")),
         dot_nice = factor(dot, labels = c("Apr-1", "Apr-15", "May-1", "May-15", "Jun-1", "Jun-15")),
         dot_nicerev = fct_rev(dot_nice)
  ) 

#--dropdowns
dd_county_dist <- ccbio_dist %>% select(subregion) %>% distinct() %>% pull() 


#--------------------------------------------
#--for summarised data
ccbio_sum <- read_csv("IA_ccbio-map.csv") %>% 
  mutate(subregion = str_to_title(subregion),
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

#--dropdowns
dd_county_sum <- ccbio_sum %>% select(subregion) %>% distinct() %>% pull() 
dd_dop_sum <- ccbio_sum %>% select(dop2) %>% distinct() %>% pull()
dd_dot_sum <- ccbio_sum %>% select(DOY2) %>% distinct() %>% pull()

#--------------------------------------------
#--map
county_map <- 
  map_data("county") %>% 
  as_tibble() %>% 
  filter(region == "iowa") %>% 
  mutate(subregion = str_to_title(subregion))
  


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    
    # Application title
    navbarPage("Iowa Cover Crop Biomass Explorer",
    
               #--start tab
    tabPanel(
        "Data Summaries",
       
        fluidRow(
          #--first bit
          column(9,
                 fluidRow(
                   column(12,
                          includeMarkdown("desc_sum.md")),
                   br(),
                   fluidRow(
                     br(),
                     column(3,
                            selectizeInput(inputId = "county_sum", 
                                label = "Choose a county:",
                                selected = "Story",
                                choices = dd_county_sum)),
                     column(3,
                 selectizeInput(inputId = "dop_sum",
                                label = "Choose a planting date:",
                                selected = "Oct-7",
                                choices = dd_dop_sum)),
                 column(3,
                 selectizeInput(inputId = "dot_sum",
                                label = "Choose a termination date:",
                                selected = "Apr-15",
                                choices = dd_dot_sum))
                 ))),
          column(3, 
                 fluidRow(
            column(12, 
                 img(src = "popcan2.png", height = 300, width = 200)),
            column(12,
                   includeMarkdown("popcan.md"))))
          ), 

        
        h1("Cover Crop Biomass Production (lbs/ac)"),
        h2("Values shown for:"),
        h4("- unfavorable (red) years"),
        h4("- average (black) years"),
        h4("- favorable (blue) years"),
        plotOutput('fig_sum')
                
        # fluidRow(
        #   column(6,
        #          plotOutput('fig_sum')),
        #   column(6, 
        #          br(),
        #          br(),
        #          br(),
        #          h3("Cover Crop Biomass Production (lbs/ac)"),
        #          tableOutput('table_sum'))
        # )
        
        ),
    #--end tab
    
    #--start tab
    tabPanel(
      "Data Distributions",
      
      
      fluidRow(
        column(4,
               selectizeInput(inputId = "county_dist", 
                              label = "Choose a county:",
                              selected = "Story",
                              choices = dd_county_dist),
               includeMarkdown("desc.md")),
        column(4, 
               plotOutput('fig_map_dist', width = 400, height = 350)),
        column(4, 
               fluidRow(
                 column(12, 
                        img(src = "popcan2.png", height = 300, width = 200)),
                 column(12,
                        includeMarkdown("popcan.md"))))
        
        
      ),
      plotOutput("fig_dist")
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
    )
    #--end tab

)
)


# Define server logic required to draw a histogram
server <- function(input, output) {


    #-------------------distributions---------------------
  
  #--map------
    
    dataset_map_dist <- reactive({
      county_map %>% 
        filter(subregion == input$county_dist)
    })
    
    
    output$fig_map_dist <- renderPlot({
        
      
        ggplot() + 
            geom_polygon(data = county_map, aes(x = long, y = lat, group = group), 
                         fill = "gray80", color = "white", lwd = 0.5) + 
            geom_polygon(data = dataset_map_dist(),
                         aes(x = long, y = lat, group = group),
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
    
    #---ggridges fig------
    
    dataset_dist <- reactive({
      ccbio_dist %>% 
        filter(subregion == input$county_dist)
    })
    
    
    output$fig_dist <- renderPlot({
      
      
      ggplot(data = dataset_dist(), aes(x = ccbio_lbs, 
                                    y = dot_nicerev, 
                                    fill = stat(x), 
                                    height = ..density..)) + 
        geom_density_ridges_gradient(stat = "density", 
                                     trim = TRUE, 
                                     #scale = 2
                                     ) +
        geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
        scale_fill_viridis_c(option = "C") +
        facet_grid(.~dop_nice, scales = "free") + 
        labs(x = "Cover Crop Biomass (lbs/ac)",
             y = "Termination Date") +
        guides(fill = F) +
        theme(#axis.title = element_text(size = 14),
              #axis.text.x = element_text(size = rel(2.3)),
              #axis.text.y = element_text(size = rel(2.5)),
              #strip.text = element_text(size = rel(2.5), face = "bold"),
              text = element_text(family="Arial")) +
        theme_bw()
      
      
      
    }, res = 130)
    
    
    #-------------------summaries---------------------
    
    
    dataset1 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum)
    })
    
    dataset2 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum,
               subregion == input$county_sum)
    })
    
    dataset3 <- reactive({
      ccbio_sum %>% 
        filter(dop2 == input$dop_sum,
               DOY2 == input$dot_sum,
               subregion == input$county_sum) %>% 
        group_by(prob_nice) %>% 
        summarise(long = mean(long),
                  lat = mean(lat),
                  ccbio_lbs = mean(ccbio_lbs, na.rm = T)) %>% 
        distinct()
      
    })
    
    
    
    output$fig_sum <- renderPlot({
      
      
      
      ggplot() + 
        geom_polygon(data = dataset1(), aes(x=long, y = lat, group = group), fill = "gray80", color = "white", lwd = 0.5) + 
        geom_polygon(data = dataset2(),
                     aes(x = long, y = lat, group = group),
                     color = "black", # "mediumseagreen", #seagreen springgreen4
                     fill = "gold1",
                     lwd = 1.5) +
      
        #--bad year
        geom_label(data = dataset3() %>% filter(prob_nice == "In An Unfavorable Year"),
                  aes(label = paste0(ccbio_lbs, " lbs/ac"),
                      #size = ccbio_lbs
                      ),
                  x = -93.8, 
                  y = 43,
                  size = 10,
                  color = "red") +
        #--avg year
        geom_label(data = dataset3() %>% filter(prob_nice == "In An Average Year"),
                  aes(label = paste0(ccbio_lbs, " lbs/ac"),
                      #size = ccbio_lbs
                      ),
                  x = -93.8, 
                  y = 42.5,
                  size = 10,
                  color = "black") +
      #--good year
      geom_label(data = dataset3() %>% filter(prob_nice == "In A Favorable Year"),
                aes(label = paste0(ccbio_lbs, " lbs/ac"),
                    #size = ccbio_lbs
                    ),
                x = -93.8, 
                y = 42,
                size = 10,
                color = "blue") +
        # geom_label_repel(data = dataset3(), 
        #            aes(x = long, y = lat, 
        #                fill = prob_nice, 
        #                label = paste0(ccbio_lbs, " lbs/ac")),
        #            size = 10,
        #            box.padding = 1,
        #            point.padding = NA,
        #            segement.color = NA) +
        # scale_fill_manual(values = c("red", "yellow", "green4")) +
        scale_size_continuous(range = c(5, 12)) +
        ggthemes::theme_few() + 
        theme(legend.position = "left",
              panel.background = element_rect(fill = "white"),
              aspect.ratio = 0.8,
              legend.text = element_text(size = rel(1.5)),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              strip.text = element_text(size = rel(1.5), face = "bold"),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.grid = element_blank(),
              panel.border = element_blank()) +
        coord_quickmap() +
        guides(size = F) 
      
      
    })
    
    output$table_sum <-
      renderTable({
        head(dataset3())
      },
      digits = 0,
      align = "c",
      width = '100%', height = '80%', res = 130)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
