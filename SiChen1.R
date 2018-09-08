#Si Chen
#Assignment 1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
require(RColorBrewer)

# datasets: ToothGrowth
#A data frame with 60 observations on 3 variables.
#description:The response is the length of odontoblasts in 60 guinea pigs. 
# Each animal received one of three dose levels of vitamin C by one of two delivery methods, orange juice or ascorbic acid 
# len	numeric	Tooth length
# supp	factor	Supplement type (VC or OJ).
# dose	numeric	Dose in milligrams/day

tg <- ToothGrowth
melttg <- melt(tg, id = "supp")
melttg$supp <- as.factor(melttg$supp)
pdf(NULL)

#input1
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("The Effect of Vitamin C on Tooth Growth", 
             theme = shinytheme("united"),
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          # These are both based off of the supp column feel free switch it up.
                          selectInput("char_select",
                                      "Supplement type:",
                                      choices = levels(melttg$supp),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected = c("VC", "OJ")),
                          #input 2
                          radioButtons("supp", "Please select the supplement type:", choices = c("OJ","VC"))
                        ),
                        # Output plot
                        mainPanel(
                          plotlyOutput("plot")
                        )
                      )
             ),
             # Data Table
             tabPanel("Table",
                      fluidPage(DT::dataTableOutput("table"))
             )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlotly({
    dat <- subset(melttg, supp %in% input$char_select)
    ggplot(data = dat, aes(x = supp, y = as.numeric(value), fill = supp)) + geom_point(stat = "identity")+
      ggtitle("Value distribution of different supplement type") +xlab("Supplement Type") + ylab("Value")+ 
      scale_fill_brewer(palette="Set2")
                                                       
  })
  output$table <- DT::renderDataTable({
    subset(tg, supp %in% input$char_select, select = c(supp, len,dose))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)