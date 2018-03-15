

library(shiny)
library(tidyverse)
library(DT)
library(ggmap)


# Read in pre-processed data
property_data <- read_rds("property_data.rds")
property_info <- read_rds("property_info.rds")

#New
my_map_roadmap_10 <- read_rds("my_map_roadmap_10.rds")





ui <- fluidPage(
   
   # Application title
   titlePanel("My Big Title"),
   
   # Sidebar 
   sidebarLayout(
     
      sidebarPanel(
        selectizeInput(inputId = "my_prop", 
                       label = "Choose a Subject Property:", 
                       choices = property_info$Name)
        
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "area_plot", brush = "prop_brush", width = "800px", height = "800px"),
         br(),
         dataTableOutput(outputId = "prop_datatable")
      )
      
   )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  # Save in case
   # output$area_plot <- renderPlot({
   #   ggplot(data = my_property_info(),
   #          aes(x = Longitude, y = Latitude, color = Submarket)) +
   #     geom_point(data = my_property_info() %>% filter(Name == Subject_Property), size = 10) +
   #     geom_point() +
   #     theme(axis.text = element_blank(), axis.ticks = element_blank()) +
   #     guides(color = FALSE) +
   #     coord_map() 
   # })
  
  
  output$area_plot <- renderPlot({
    ggmap(my_map_roadmap_10, extent = "panel",
          base_layer = ggplot(data = my_property_info(), 
                              aes(x = Longitude, 
                                  y = Latitude, 
                                  color = Submarket))) +
      geom_point(data = my_property_info() %>% filter(Name == Subject_Property), size = 8, color = "black", alpha = 0.4) +
      geom_point(alpha = 0.8) +
      guides(color = FALSE)
  })
   
   
   
   output$prop_datatable <- renderDataTable({
     brushedPoints(my_property_info(), 
                   brush = input$prop_brush)[,c("Subject_Property",  "Name", "Longitude", 
                                                "Latitude", "Address", 
                                                "Submarket")]
     })
   
   
   
   
   my_property_info <- reactive({
     
     my_property_info <- property_info
     
     my_property_info$Subject_Property <- input$my_prop 
     
     my_property_info
     
   })
   
   
   
   
   
}



# Run the application 
shinyApp(ui = ui, server = server)

