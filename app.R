library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(maps)
#library(raster)


#Import Data
SchoolLoc <- read.csv("School_Locations.csv")




# Define UI
ui <- fluidPage(

#Navbar structure for UI
  navbarPage("Calgary School Locations", theme = shinytheme("cerulean"),
             tabPanel("School Finder", fluid = TRUE, icon = icon("globe-americas"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Filters"),
                          fluidRow(column(3,

                                          checkboxGroupInput(inputId = "GradeFinder",
                                                             label = "Select Grade(s):",
                                                             choices = c("Elementary"="Elementary",
                                                                         "ECS"="ECS",
                                                                         "Junior High"="Junior High",
                                                                         "Senior High"="Senior High"
                                                                         ),
                                                             selected = "Elementary"),

                                          checkboxGroupInput(inputId = "CityFinder",
                                                             label = "City(s):",
                                                             choices = c("Calgary"),
                                                             selected = "Calgary")
                          ),
                          column(6, offset = 2,
                                 checkboxGroupInput(inputId = "BoardFinder",
                                                    label = "Board(s):",
                                                    choices = c("The Calgary School Division" = "The Calgary School Division","The Calgary Roman Catholic Separate School Division"="The Calgary Roman Catholic Separate School Division"),
                                                    selected = "The Calgary School Division")
                          )),
                          hr(),
                          helpText("Data Souce: Open Data City of Calgary"),
                          hr(),

                        ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,


                            )),

                          withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                          )),
                          hr(),
                          fluidRow(column(7,
                                          helpText("Tip: Click on locations to show attribute table")

                          ),
                          column(width = 2, offset = 2, conditionalPanel(
                            condition = "output.schoolstableFinder",
                            actionButton(inputId = "FinderClear", label = "Clear Table")))),
                          br(),
                          fluidRow(
                          withSpinner(dataTableOutput(outputId = "schoolstableFinder"))))
                      )
             ),


)
)

# Define server
server <- function(input, output, session) {

  #School Finder



  SchoolLoc_finder <- reactive({
    req(input$CityFinder)
    req(input$BoardFinder)
    req(input$GradeFinder)
    filter(SchoolLoc, CITY %in% input$CityFinder) %>%
      filter(BOARD %in% input$BoardFinder) %>%
      filter(GRADES %in% input$GradeFinder) %>%
      dplyr::mutate(Entries = n())

  })


  world <- map_data("world")
  canada <- subset(world, region %in% c("Canada"))
  #canada <- getData("GADM",country="CAN",level=1)
  #alberta <- subset(canada, NAME_1 %in% c("Alberta"))


  output$scatterplotFinder <- renderPlot({
    input$GradeFinder
    input$CityFinder
    input$BoardFinder

    isolate({
      if (length(SchoolLoc_finder()$NAME) == 0) {
        ggplot() +
          geom_polygon(data = canada, aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No schools found as per selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 15))
      } else {
        ggplot() +
          geom_polygon(data = canada, aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          geom_point(data = SchoolLoc_finder(), aes(x = long, y = lat), alpha = 0.5) +
          theme_void() +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15)) +
          theme(plot.background = element_rect(
            color = "white"
          ))

      }
    })
  })

  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(SchoolLoc), nrow = 1))
    names(user_clickFinder$DT) <- colnames(SchoolLoc)
  })

  observeEvent(input$click_plotFinder, {
    add_row <-     nearPoints(SchoolLoc_finder(), input$click_plotFinder, xvar = "long", yvar = "lat", threshold = 5)
    user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  })

  brushFinder <- reactive({
    req(length(user_clickFinder$DT) > 1)
    user_clickFinder$DT
  })

  observeEvent({
    input$FinderClear
  },{
    user_clickFinder$DT <- NULL
  })

  output$schoolstableFinder<-DT::renderDataTable({

    DT::datatable(unique(brushFinder()[,c("NAME", "BOARD", "ADDRESS", "GRADE", "CITY")]),
                  colnames = c("Sort" = "NAME", "NAME" = "NAME", "BOARD" = "BOARD", "ADDRESS" = "ADDRESS", "GRADE" = "GRADE","CITY" = "CITY"),
                  rownames = FALSE,
                  options = list(order = list(9, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=c(9)),
                                                   list(className = "dt-center", targets = 1:7),
                                                   list(classname = "dt-right", targets = 8))
                  ))

  })


}
# Run the application
shinyApp(ui = ui, server = server)

