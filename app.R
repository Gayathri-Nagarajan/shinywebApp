library(shiny)
library(tidyverse)
library(corrplot)
library(cpp11)

# Define UI for random distribution app ----
ui <- fluidPage(
    
   

    # App title ----
    titlePanel("Welcome to my Page showing Datasets in R"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(


            # br() element to introduce extra vertical spacing ----
            br(),
            h3("Welcome user to my page.These datasets exist in R", style = "color:blue"),
            br(),
            strong("1.Select a dataset and Select number of records you want to view from the dataset"),
            br(),
            strong("2.select the x axis"),
            br(),
            strong("3.select the y axis"),
            br(),
            strong("4.Navigate the tabsets to the left to view the data"),
            br(),

            selectInput("var",
                        label = "Choose a dataset to display",
                        choices = list("Iris",
                                       "mtcars",
                                       "ToothGrowth",
                                       "PlantGrowth"),
                        selected = "Iris"),

            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10),

            selectInput("x_label",
                        label="Select X axis",
                        choices = "",
                        selected=""),
            selectInput("y_label",
                        label="Select Y axis",
                        choices = "",
                        selected="")
            
        ),
      
        # Main panel for displaying outputs ----
        mainPanel(

                      # Output: Tabset w/ plot, summary, and table ----
                #      img(src = "rstudio.png", height = 100, width = 300),
                      tabsetPanel(type = "tabs",
                        tabPanel(strong("Correlation Plot"), plotOutput("plot")),
                        tabPanel(strong("Summary"), verbatimTextOutput("summary")),
                        tabPanel(strong("Table"), tableOutput("table")),
                        tabPanel(strong("My dynamic Plot"), plotOutput("plot1"))
                       
            )

        )
    ),
    
    fluidRow(
        column(10,
               br(),
               strong("Documentation to use this page:"),
               br(),
               br(),
               p("This page is a simple application built to display 4 simple data sets in R",style = "color:violet"),
               br(),
               p("The first tab of correlation plots display the correlation each column in the table has with the others",style = "color:violet"),
               br(),
               p("The second tab displays the summary like mean, median values of each column in the table",style = "color:violet"),
               br(),
               p("The third tab displays the observation rows of data in the table as selected above",style = "color:violet"),
               br(),
               p("The last tab displays the plot of selected x and y axis",style = "color:violet"),
               br()
        ),
        column(10, offset = 6,
               h3(strong("***ENJOY THIS PAGE***"
               ))
        )
             
    )
    
)

# Define server logic for random distribution app ----
server <- function(session,input, output) {

    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
         switch(input$var,
                       Iris = iris,
                       mtcars = mtcars,
                       ToothGrowth = ToothGrowth,
                       PlantGrowth = PlantGrowth,
                       iris)


        })

      observeEvent(
        input$var,
        updateSelectInput(session, "x_label", "Select X axis",
                          choices = names(d()),
                          selected=names(d()[1])))


     observeEvent(
        input$var,
        updateSelectInput(session, "y_label", "Select Y axis",
                          choices = names(d()),
                          selected=names(d()[2])))


    # Generate a correlation plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$plot <- renderPlot({

        plot(d())


    })

    # Generate a summary of the data ----
    output$summary <- renderPrint({

                        summary(d())
    })

    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        head(d(), n = input$obs)
           })

    output$plot1 <- renderPlot({
  
        gg <-
            ggplot(d(), aes_string(x = input$x_label, y = input$y_label))
        gg <- gg + geom_point()
        gg

    })

}

# Create Shiny app ----
shinyApp(ui, server)
