library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(ggthemes)
library(shinythemes)
library(plotly)
library(readxl)
library(car)
library(RCurl)
library(rsconnect)

inflation1 <- read.csv("https://raw.githubusercontent.com/Junying123/test_inflation/main/inflation1.csv")


interest <- read.csv("https://raw.githubusercontent.com/Junying123/test_inflation/main/interest.csv")



urate <- read.csv("https://raw.githubusercontent.com/Junying123/test_inflation/main/urate.csv")

x <- read.csv("https://raw.githubusercontent.com/Junying123/test_inflation/main/covid-19.csv")

interest <- arrange(interest,desc(Year))
intOnly <- select(interest,Deposit_interest_rate) 
data <- bind_cols(inflation1,intOnly)
data <- select(data,-AnnualChange)
y <- select(x,Date,New.Case,Cumulative.Case,Recovered)
t <- select(data,Year,Deposit_interest_rate)

ui <- navbarPage("Interest Analyzer",
                 theme = shinytheme("cerulean"),
                 tabPanel(
                   "Main",
                   # App title ----
                   titlePanel(div(
                     windowTitle = "Interest Analytical Dashboard",
                     img(src = "Interest Analytical Dashbord.png", 
                         width = "100%", height="340", class = "bg"),
                   )),
                   
                   tags$br(),
                   
                   tabsetPanel(
                     type = "tabs",
                     
                     #page1            
                     
                     tabPanel("Overview",
                              
                              #summary
                              
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  p(h3("3 factor that affect Interest Rate:" 
                                  )),
                                  p(h5("  Inflation Rate,  Unemployment Rate, 
                                    ")),
                                  p(h5("Covid-19 Case")),
                                  tags$br(),
                                  
                                ),
                                mainPanel(
                                  p(h2("Summary:")),
                                  p(h3("Malysia Interest Rate (1966-2020)")),
                                  
                                  verbatimTextOutput("summary3"),
                                  p(h3("Malysia Inflation Rate (1960-2020)")),
                                  
                                  verbatimTextOutput("summary1"),
                                  p(h3("Malysia Unemployment Rate (2019-2021)")),
                                  
                                  verbatimTextOutput("summary2"),
                                  p(h3("Malysia Covid-19 Cases (2020-2021)")),
                                  helpText("This is the summary of the daily new cases starting from March of 2020 to April of 2021.
                            From the Summary you can tell Malaysia did experience from 1 cases per day to 5728 cases per day."),
                                  verbatimTextOutput("summary"),
                                  br(),
                                  
                                  tags$br(),
                                  tags$br()
                                )
                              ),
                              tags$hr(),
                              
                              #int
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Interest Rate in %"),
                                  tags$br(),
                                  checkboxGroupInput("num3", "Select the variables:",
                                                     names(t),selected = names(t)
                                  )
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Table 1",dataTableOutput("datahead3")),
                                  ),
                                  tags$br(),
                                  tags$br()
                                )
                              ),
                              tags$hr(),
                              
                              #inf
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Inflation Rate in %"),
                                  tags$br(),
                                  checkboxGroupInput("num", "Select the variables:",
                                                     names(inflation1),selected = names(inflation1)
                                  )
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Table 1",dataTableOutput("datahead")),
                                  ),
                                  tags$br(),
                                  tags$br()
                                )
                              ),
                              tags$hr(),
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Unemployment Rate in %"),
                                  tags$br(),
                                  checkboxGroupInput("num1", "Select the variables:",
                                                     names(urate),selected = names(urate)
                                  )
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Table 2",dataTableOutput("datahead1")),
                                  ),
                                  tags$br(),
                                  tags$br()
                                )
                              ),
                              tags$hr(),
                              
                              #covid
                              
                              sidebarLayout(
                                sidebarPanel(
                                  h3("Covid-19 case stat"),
                                  tags$br(),
                                  checkboxGroupInput("num2", "Select the variables:",
                                                     names(y),selected = names(y)
                                  )
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    type = "tabs",
                                    tabPanel("Table 3",dataTableOutput("datahead2")),
                                  ),
                                  tags$br(),
                                  tags$br()
                                )
                              ),
                              tags$hr(),
                              
                              
                     ),   
                     
                     #page2
                     
                     tabPanel("Plot",
                              tabsetPanel( type = "tabs",
                                           
                                           tabPanel("page 1",       
                                                    
                                                    #int & inf 
                                                    
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Compare frequency of Inflation and Interest"),
                                                        tags$br(),
                                                        selectInput(
                                                          "var",
                                                          "Graph to show:",
                                                          c("Interest Rate", "Inflation Rate"),
                                                          selected = "Interest Rate"
                                                        ),
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Frequency of Inflation and Interest"),
                                                        plotlyOutput(outputId = "Plot"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(),
                                                    
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Compare trend of Inflation and Interest"),
                                                        tags$br(),
                                                        selectInput(
                                                          "tre",
                                                          "Graph to show:",
                                                          c("Interest Rate", "Inflation Rate"),
                                                          selected = "Interest Rate"
                                                        ),
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Description Analysis of Inflation and Interest"),
                                                        plotlyOutput(outputId = "TPlot"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(),
                                                    
                                                    
                                                    
                                                    #int & urate
                                                    
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Check the Unemployment Rate"),
                                                        sliderInput(inputId = "bins",
                                                                    label = "Number of frequency of unemployment Rate:",
                                                                    min = 1,
                                                                    max = 20,
                                                                    value = 10),
                                                        tags$br(),
                                                        
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Frequency of Unemployment Rate"),
                                                        plotOutput(outputId = "displot"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(),
                                                    
                                                    
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Check the trend of Unemployment Rate"),
                                                        
                                                        tags$br(),
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Line Graph of Unemployment Rate"),
                                                        plotlyOutput(outputId = "plot1"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(), 
                                           ),
                                           
                                           
                                           tabPanel("page 2",
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Check the trend of Covid Case"),
                                                        
                                                        tags$br(),
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Graph of Cumulative Cases"),
                                                        plotOutput(outputId = "hist"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(),
                                                    
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        h3("Check the trend of Covid New Cases"),
                                                        
                                                        tags$br(),
                                                        tags$hr()
                                                      ),
                                                      
                                                      mainPanel(
                                                        h3("Graph of New Cases"),
                                                        plotOutput(outputId = "bar"),
                                                        tags$br(),
                                                        tags$br()
                                                      )
                                                    ),
                                                    tags$hr(),
                                           )
                              )
                     ),
                     
                     tabPanel("Calculation",
                              sidebarLayout(
                                sidebarPanel(
                                  helpText("This app calculates simple interest
                     based on your inputs."),
                                  br(),
                                  numericInput("num_principal",
                                               label = h6("Enter the principal amount (in $)"),
                                               value = 1000),
                                  br(),
                                  sliderInput("slider_intrate",
                                              label = h6("Choose the yearly interest rate (in %)"),
                                              min = 0, max = 20, value = 5),
                                  
                                  br(),
                                  sliderInput("slider_num",
                                              label = h6("Choose the number of time periods"),
                                              min = 0, max = 50, value = 5),
                                  selectInput("select_time",
                                              label = h6(""),
                                              choices = list("Years" = 1,
                                                             "Quarters" = 2,
                                                             "Months" =3),
                                              selected = 1
                                  ),
                                  br(),
                                  br(),
                                  actionButton("action_Calc", label = "Refresh & Calculate"),
                                  tags$hr()
                                ),
                                
                                mainPanel(
                                  tabPanel("Output",
                                           p(h5("Your entered values:")),
                                           textOutput("text_principal"),
                                           textOutput("text_intrate"),
                                           textOutput("text_num"),
                                           textOutput("text_time"),
                                           br(),
                                           p(h5("Calculated values:")),
                                           textOutput("text_int"),
                                           textOutput("text_amt")
                                  )
                                )
                                
                              ),
                              tags$hr(),
                     )
                   )            
                 ),
                 tabPanel("Documentation",
                          p(h4("Simple Interest Calculator:")),
                          br(),
                          helpText("This application calculates simple interest
                                     and total amount, i.e. principal plus interest."),
                          HTML("<u><b>Equation for calculation: </b></u>
                                <br> <br>
                                <b> A = P + I = P(1 + rt) ; R = r * 100 </b>
                                <br>
                                where: <br>
                                A = Total amount (Principal + Interest) <br>
                                P = Principal amount <br>
                                I = Interest amount <br>
                                r = Rate of interest per year, in decimal; r=R/100 <br>
                                t = Time period invested in years/quarters/months
                          ")                
                 )             
)


server <- function(input, output) {
  
  #page1  
  
  output$summary<-renderPrint({summary(x$New.Case)})
  
  output$summary3<-renderPrint({summary(data$Deposit_interest_rate)})
  
  output$summary1<-renderPrint({summary(data$Inflation_Rate)})
  
  output$summary2<-renderPrint({summary(urate$rate)})
  
  
  output$datahead3 <- DT::renderDataTable({
    DT::datatable(t[,input$num3,drop=FALSE])
  })
  
  output$datahead <- DT::renderDataTable({
    DT::datatable(inflation1[,input$num,drop=FALSE])
  })
  
  output$datahead1 <- DT::renderDataTable({
    DT::datatable(urate[,input$num1,drop=FALSE])
  })
  
  
  output$datahead2 <- DT::renderDataTable({
    DT::datatable(y[,input$num2,drop=FALSE])
  })
  
  #page2
  
  output$Plot <- renderPlotly({
    num_val = ifelse(input$var == 'Interest Rate', 'Deposit_interest_rate',
                     ifelse(input$var == 'Inflation Rate', 'Inflation_Rate'))
    
    ggplot(data = data, aes(x = data[[num_val]]))+
      geom_histogram(stat = "bin", fill = 'steelblue3', color = 'lightgrey')+
      
      labs(title = sprintf('Histogram plot of the %s', num_val),
           x = sprintf('%s', input$num),y = 'Frequency')+
      stat_bin(geom = 'text',
               aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
               vjust = -0.6)
  })
  
  output$TPlot <- renderPlotly({
    tre_val = ifelse(input$tre == 'Interest Rate', 'Deposit_interest_rate',
                     ifelse(input$tre == 'Inflation Rate', 'Inflation_Rate'))
    
    ggplot(data = data) +
      aes(x =Year , y =data[[tre_val]]) +
      geom_point(colour = "#0c4c8a") +geom_smooth(method=lm)+
      labs(title = sprintf('Scatter plot of %s', tre_val),
           x = sprintf('%s', input$tre),y = 'Rate')+
      theme_minimal()
    
  })
  
  output$plot1 <- renderPlotly({
    
    # plot(urate,
    #      type="o",xaxt="n")
    # mnnn <-   c("Jan'19","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan'20","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan'21","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    # axis(1, at=1:36, labels=mnnn) 
    ggplot(data=urate, aes(x=Date, y=rate, group=1)) +
      geom_line()+
      geom_point()
  })
  
  output$displot <- renderPlot({
    
    x <- urate$rate
    bins <- seq(min(x),max(x),length.out=input$bins+1)
    
    hist(x,breaks=bins,col="#75AADB", border="white",xlab = "Unemployment Rate",
         main = "Histogram of unemployment rate" )
    
  })
  
  output$hist<-renderPlot(plot(y$Cumulative.Case,type = "h", main = "Cumulative Cases from 2020-2021",
                               
                               xlab = "Number of Days",
                               ylab = "Number of cases", border="blue",col="pink"))
  
  
  output$bar<-renderPlot(plot(y$New.Case,type = "h", main = "Number of New Cases from 2020-2021",
                              xlab = "Number of Days",
                              ylab = "Number of new cases", border="brown",col="orange"))
  
  #page3
  
  values <- reactiveValues()
  # Calculate the interest and amount    
  observe({
    input$action_Calc
    values$int <- isolate({
      input$num_principal * input$slider_intrate *
        recode(input$select_time, "1 = '1'; 2 = '0.25'; 
                                   3 = '0.08333333'")/100 * input$slider_num  
    })
    values$amt <- isolate(input$num_principal) + values$int
  })
  
  # Display values entered
  output$text_principal <- renderText({
    input$action_Calc
    paste("Principal amount: [$]", isolate(input$num_principal))
  })
  
  output$text_intrate <- renderText({
    input$action_Calc
    paste("Interest rate: ", isolate(input$slider_intrate), 
          " % per year")
  })
  
  output$text_num <- renderText({
    input$action_Calc
    paste("Time period ", isolate(input$slider_num),
          recode(isolate(input$select_time),
                 "1 = 'Years'; 2 = 'Quarters';3 = 'Months'")
    )
  })
  
  # Display calculated values
  
  output$text_int <- renderText({
    if(input$action_Calc == 0) ""
    else
      paste("Simple Interest [$]:", values$int)
  })
  
  output$text_amt <- renderText({
    if(input$action_Calc == 0) ""
    else 
      paste("Total Amount, i.e. Principal plus Interest [$]:", values$amt)
  })
}


shinyApp(ui = ui, server = server)
