#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(ggplot2)
library(shinythemes)

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"), titlePanel("Regresi Linier"),
                navbarPage("Let's get started",
                           tabPanel("Dataset",
                                    sidebarLayout(
                                        sidebarPanel(
                                            # Input: Select a file ----
                                            fileInput("file1", "Choose CSV File",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv")),
                                            
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # Input: Checkbox if file has header ----
                                            checkboxInput("header", "Header", TRUE),
                                            
                                            # Input: Select separator ----
                                            radioButtons("sep", "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ","),
                                            
                                            # Input: Select quotes ----
                                            radioButtons("quote", "Quote",
                                                         choices = c(None = "",
                                                                     "Double Quote" = '"',
                                                                     "Single Quote" = "'"),
                                                         selected = '"'),
                                            
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # Input: Select number of rows to display ----
                                            radioButtons("disp", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")
                                        ),
                                        mainPanel(
                                            tableOutput("contents")
                                        )
                                    )
                           ),
                           tabPanel("Summary",
                                    verbatimTextOutput("summ")
                           ),
                           tabPanel("Regresi",
                                    sidebarLayout(
                                        sidebarPanel(
                                            textInput("x","Enter Variabel Independen"),
                                            textInput("y","Enter Variabel Dependen")
                                        ),
                                        mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Model Summary", verbatimTextOutput("summary")),
                                                        tabPanel("Scatterplot", plotOutput("scatterplot")),
                                                        tabPanel("Distribution",
                                                                 fluidRow(
                                                                     column(6, plotOutput("distribution1")),
                                                                     column(6, plotOutput("distribution2")))
                                                                 
                                                        )
                                            )   
                                        )
                                    )
                           )
                )
)

# Define server logic to read selected file ----
server <- function(input, output,session) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    output$summ<-renderPrint({
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        summary(df)
    })
    
    #Regression output
    output$summary <-renderPrint(
        {
            req(input$file1)
            
            #when reading semicolon separated files,
            #having a comma separator causes 'read.csv' to error
            tryCatch({
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                #return a safeError if a parsing error occur
                stop(safeError(e))
            }
            )
            tryCatch(
                {
                    fit <- lm(df[,input$y] ~ df[,input$x])   
                    names(fit$coefficients) <- c("Intercept", input$x)
                    summary(fit)
                },
                error = function(e) {
                    #return a safeError if a parsing error occurs
                    print("Masukkan nama variabel dengan benar")
                }
                
                
            )
        })
    
    #scatterplot output
    output$scatterplot <- renderPlot({
        req(input$file1)
        
        #when reading semicolon separated files,
        #having a comma separator causes 'read.csv' to error
        tryCatch(
            {
                df <- read.csv(file=input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                plot(df[,input$x], df[,input$y], main="Scatterplot",
                     xlab=input$x, ylab=input$y, pch=19, col = "green", cex = 1)
                abline(lm(df[,input$y] ~ df[,input$x]), col="red")
                lines(lowess(df[,input$x] ~ df[,input$y]), col="blue")
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                print("")
            }
        )
    })
    
    #Histogram output var 1
    output$distribution1 <- renderPlot({
        req(input$file1)
        
        #when reading semicolon separated files,
        #having a comma separator causes 'read.csv' to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
                
            }
        )
        
        tryCatch(
            {
                hist(df[,input$y], main="", xlab=input$y)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                print("")
            }
        )
        
    }
    )
    
    #Histogram output var 2
    output$distribution2 <- renderPlot({
        req(input$file1)
        
        #when reading semicolon separated files,
        #having a comma separator causes 'read.csv' to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
                
            }
        )
        
        tryCatch(
            {
                hist(df[,input$x], main="", xlab=input$x)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                print("")
            }
        )
        
    }
    )
    
}


# Run the application 
shinyApp(ui = ui, server = server)

