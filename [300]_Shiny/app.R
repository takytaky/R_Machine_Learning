# R Shiny Random Forest ####

library(shiny)
library(randomForest)






# 데이터 9:1 분리 ####

set.seed(2045)

idx <- sample(2, 
              size = nrow(iris), 
              replace = TRUE, 
              prob = c(0.9, 0.1))

TR <- iris[idx == 1, ]

TE <- iris[idx == 2, ]








# ui Script ####

ui <- fluidPage(
  
  titlePanel("RandomForest"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("input_ntree",
                  "Number of ntree:",
                  min = 500,
                  max = 2500,
                  value = 1000,
                  step = 100),
      
      sliderInput("input_mtry",
                  "Number of mtry:",
                  min = 1,
                  max = 4,
                  value = 3,
                  step = 1)
    ),
    
    mainPanel(
      verbatimTextOutput("result_print")
    )
  )
)


# server Script ####

server <- function(input, output) {
  
  output$result_print <- renderPrint({
    
    Model_rfs <- randomForest(Species ~ ., 
                              data = TR, 
                              ntree = input$input_ntree, 
                              mtry = input$input_mtry)

    Model_rfs
    
  })
}



shinyApp(ui = ui, server = server)