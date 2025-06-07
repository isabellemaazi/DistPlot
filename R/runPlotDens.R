#'runPlotDens Function
#'
#'This function will open an interactive Shiny App where the user can choose many parameters to plot the normal and/or Student-t Distributions.
#'
#' @return Interactive Shiny App
#' @export
#'
#' @import shiny
#' @import ggplot2
#' @import stats
runPlotDens <- function() {
  ui <- fluidPage(
    titlePanel("Normal vs Student"),

    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "mean",
          label="Mean - Normal",
          min=-5,max=5,value=0,step=0.5
        ),
        sliderInput(
          inputId = "sd",
          label="Standard Deviation ('sd') - Normal",
          min=0.1,max=3,value=1,step=0.1
        ),
        sliderInput(
          inputId = "df",
          label="Degree of freedom ('df') - Student",
          min=1, max=50, value=1, step=1
        ),
        sliderInput(
          inputId = "range",
          label = "Interval (a,b):",
          min = -5, max = 5, value = c(-1,1), step = 0.5,  animate = animationOptions(loop = TRUE)
        ),
        radioButtons(
          inputId = "type",
          label="Output",
          choices = c("Overlay","Normal","Student"),
          selected = "Overlay"
        )
      ),
      mainPanel(plotOutput("plot")),
    )
  )
  server <- function(input, output) {
    a <- reactive({input$range[1]})
    b <- reactive({input$range[2]})
    mean <- reactive({input$mean})
    sd <- reactive({input$sd})
    df <- reactive({input$df})
    type <- reactive({input$type})
    output$plot <- renderPlot({PlotDens(a(),b(),mean(),sd(),df(),type())})
  }
  runApp(shinyApp(ui, server))
}


