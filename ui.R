library(shiny)
library(DT)
library(shinydashboard)
library(visdat)
library(ggplot2)
library(vcd)
library(gridGraphics)
library(PerformanceAnalytics)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(tabplot)
library(plotly)
library(ECharts2Shiny)
library(grid)

dat <- read.csv("Ass1Data.csv")
domChoices <- c("l","f","r","t","i","p")
numerical_data <- dat[,-c(2:14)]
cat_data <- dat[,c(2,3,5,6,7,8,9,10,11,12,13,14)]
timeseries <- as.Date(dat$Date,"%Y-%m-%d")

shinyUI(fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Shiny App"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("dashboard",tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Table",tabName = "Table", icon = icon("table")),
        # menuSubItem("Table1",tabName = "table1_sub"),
        # menuSubItem("Table2"),
        menuItem("Summary statistics",tabName = "Summary",icon = icon("info")),
        menuItem("Visualization",tabName = "Plot",icon = icon("chart-bar"))
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow( withTags({
                  div(class="header", checked=NA,
                      
                      img(src="Avi_PP.JPG",height=200, widh=200) ,
                      h3("My first shiny apps"),
                      br(),
                      img(src="dashboard_image.png",height=500,width=1000)
                  )
                  
                  #   div(h1("hello"))
                  
                }))
                
                
                
        ),
        
        tabItem(tabName = "Table",
                titlePanel("Data Tables"),
                
                #Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    checkboxInput("rownames", "Show row names", value=T),
                    checkboxInput("order", "Column ordering", value=T),
                    selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                    selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                    selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
                  ),
                  
                  # #   # Show a plot of the generated distribution
                  mainPanel(
                    
                    DT::dataTableOutput("tableX"),
                  #  browser(),
                    tableOutput("SelRows")       
                  )
                )
        ),
        
        
        tabItem(tabName = "Summary",
                fluidRow(
                  tabBox(width='100',
                         tabPanel("Summary of data",
                    h1("Summary statistics of dataset"),
                    tabPanel(
                    tags$b("It gives the information about number of rows and columns respectively"),
                    verbatimTextOutput("SummaryStatistics"))
                         ),
                    
                    tabPanel("Type of columns",
                             sliderInput("threshold", label = "Threshold Value", min = 0, max = 1, step = 0.1, value = 0.2),
                             h4("Continuous and Discontinuous variables of the data"),
                             tags$b("No unexpected continuous variables and no unexpected non-continuous variable"),
                             verbatimTextOutput("col_identify")
                             
                    )
                    
                )
                  )
       
        ),
        
        tabItem(tabName = "Plot",
                fluidRow(
                tabBox( width = '100',
                  tabPanel(title= "Missing Data",
                           plotOutput("MissingData"),
                           p("The graph shows there is missing value in the dataset"),
                           h4("Missing data in categorical Variables"),
                           plotOutput("Missingvalcat"),
                           br(),
                           h4("Missing data in numerical Variables"),
                           plotOutput("Missingval")
                           
                           ),
      
                  tabPanel("Factors",
                           tabsetPanel(
                             tabPanel("summary",
                                      h4("Summary Statistics of categorical variables"),
                                      verbatimTextOutput("Summarycat")
                                            
                             ),
                             
                             tabPanel("Factors of categorical data",
                                      h4("Graphical display of categorical variables."),
                                      selectizeInput("DiscreteV", label="Show variables:", choices=colnames(cat_data[2:12]), multiple=TRUE, selected=colnames(cat_data)[2:3]),
                                      plotOutput("Factors")
                                      
                             )
                           )
                  ),
                  
                
                  tabPanel(title="Correlation",
                           h4("Correlation between numerical variables"),
                           selectizeInput("CorrelationVariable", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)[1:3]),
                           tags$b("Check dataset is center or scale"),
                           checkboxInput("center", label = "center dataset",value = FALSE),
                           checkboxInput("scale", label = "scale dataset",value = FALSE),
                           plotOutput("Correlation"),
                           p("This chart will identify the correlation between different variable")),
                  
                  tabPanel("Time Series",
                           h3("Heading"),
                           tabsetPanel(
                             tabPanel("Summary of Numerical data",
                                      h4("Summary Statistics of numerical variables"),
                                      verbatimTextOutput("Summarynum")
                                      ),
                             tabPanel("Visualisation",
                                      h4("Time Series plot of all the Numerical variables"),
                                      selectizeInput("Variablesnum1", label="Show variables:", choices=colnames(numerical_data), multiple=FALSE, selected=colnames(numerical_data)[2]),
                                      
                                      plotOutput("timeSeries1"),
                                      tags$b("Lot of variations are observed in the graphs when trying to plot the numerical values with respect to time. In some variables there is sudden drop after year 2001"),
                                      br(),
                                      h4("Rising Order Plot"),
                                      sliderInput("Threshold", label = "Continuity threshold", min = 0, max = 1, step = 0.1, value = 0.5),
                                      selectizeInput("Variablesnum2", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)[2]),
                                
                                      tags$b("Check dataset is center or scale"),
                                      checkboxInput("centerRisingOrder", label = "center dataset",value = FALSE),
                                      checkboxInput("scaleRisingOrder", label = "scale dataset",value = FALSE),
                                      tags$b("Data is sorted in ascending order and plotted"),
                                      plotOutput("Rising"),
                                      h4("Natural Order Plot"),
                                      selectizeInput("Variablesnum3", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)[2]),
                                      tags$b("Check dataset is center or scale"),
                                      checkboxInput("centerNaturalOrder", label = "center dataset",value = FALSE),
                                      checkboxInput("scaleNaturalOrder", label = "scale dataset",value = FALSE),
                                      tags$b("Plotting the data without sorting"),
                                      plotOutput("Natural")
                             )
                           )
                  ),
                  
                  tabPanel("Box Plot",
                           h4("Box plot of numerical variables"),
                           selectizeInput("VariablesB", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)),
                           tags$b("Check dataset is center or scale"),
                           checkboxInput("centerB", label = "center dataset",value = FALSE),
                           checkboxInput("scaleB", label = "scale dataset",value = FALSE),
                           checkboxInput("outliers", label = "Show outliers", value = TRUE),
                           sliderInput("range", label = "Interquartile range", min = 0, max = 10, step = 0.1, value = 1.5),
                           plotOutput("boxplot"),
                           tags$b("Novelties are seen for some of the variables far away from interquartile range."),
                           tags$b("Even after changing the range some of novelties are still observed in the box plot")
                           
                     ),
                           
                  tabPanel("Homogeneity",
                           h4("Plot to display data homogeneity"),
                           selectizeInput("VariablesH", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)),
                           tags$b("Check dataset is center or scale"),
                           checkboxInput("centerH", label = "center dataset",value = FALSE),
                           checkboxInput("scaleH", label = "scale dataset",value = TRUE),
                           tags$b("In homogeneity chart I don’t see much difference.	The first half look a different to the second half but not significantly"),
                           plotOutput("Homogeneity"),
                           h4("Plot to display data homogeneity using tabplot"),
                           tags$b("In this graph I can find ID and Date column have different pattern of data."),
                          # browser(),
                           plotOutput("HomogeneityTabplot")
                  
                           
                           ),
                  
                  tabPanel("Heatmap",
                           h4("Heatmap chart"),
                  #          selectizeInput("VariablesP", label="Show variables:", choices=colnames(numerical_data), multiple=TRUE, selected=colnames(numerical_data)),
                           tags$b("Check dataset is center or scale"),
                           checkboxInput("centerHeatmap", label = "center dataset",value = FALSE),
                          checkboxInput("scaleHeatmap", label = "scale dataset",value = FALSE),
                           plotlyOutput("heatmap")
                        
                  )
                  
                )
                )
                
        )

      )
      
      
    )
  )
  
))
