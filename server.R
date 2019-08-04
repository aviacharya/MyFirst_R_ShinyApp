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
ext <- list(Responsive = TRUE)
numerical_data <- dat[,-c(2:14)]
cat_data <- dat[,c(2,3,5,6,7,8,9,10,11,12,13,14)]
timeseries <- as.Date(dat$Date,"%Y-%m-%d")
shinyServer(function(input, output, session) {
  # https://datatables.net/reference/option/dom
  
  # https://datatables.net/extensions/index
  
  
  
  getContinuousData <- reactive({
    continuous <- c()
    for (col in colnames(dat)) {
      myCol <- na.omit(dat[,col])
      ratio <- length(unique(myCol)) / length(myCol)
      if (ratio > input$Threshold & is.numeric(myCol)) {
        continuous <- c(continuous, col)
      }
    }
    req(length(continuous) > 0)
    dat[, continuous, drop = FALSE]
  })
  
  
  getDisContinuousData <- reactive({
    d <- dat
    discontinuous <- c()
    for (col in colnames(d)) {
      myCol <- na.omit(d[,col])
      ratio <- length(unique(myCol)) / length(myCol)
      if (ratio < input$Threshold) {
        discontinuous <- c(discontinuous, col)
        if (!is.factor(myCol)) {  # Convert to factor if it is not a factor
          d[,col] <- as.factor(d[,col])
        }
      }
    }
    req(length(discontinuous) > 0)
    d[, discontinuous, drop = FALSE]
  })
  
  
  output$tableX <- DT::renderDataTable({
    DT::datatable(data = dat,
                  rownames = input$rownames,
                  selection = input$selection,
                  filter = list(position = input$filter),
                  options = list(searching = TRUE,
                                 pageLength = 20,
                                 lengthMenu = c(20, 50, 100),
                                 dom = paste(input$dom, collapse = ""),
                                 ordering = input$order
                  )
                  #,extensions = ext
    )  %>%
      formatStyle(columns = c("Y"), backgroundColor = "lightblue")  %>%
           #formatCurrency(c(2), '$') %>%
           #formatPercentage(3, 2) %>%
      formatRound(c("Y"), 3)
  })
  
  output$SelRows <- renderTable({
    req(input$tableX_rows_selected)
    print(mt[input$tableX_rows_selected,"ID"])
  })
  
  output$SummaryStatistics <- renderPrint(
    {
      
      str(dat)
    }
  )
  
  output$Summarynum <- renderPrint({
    summary(as.data.frame(numerical_data))
  })
  
  output$Summarycat <- renderPrint({
    summary(cat_data)
  })
  
  output$col_identify <- renderPrint({
    d_raw <- dat
    lines <- "Variables in dat:"
    for (col in colnames(d_raw)) {
      xline <- paste0("  ", col, " -",paste(collapse=",", class(d_raw[,col])),"- ")
      ratio <- length(unique(d_raw[,col])) / nrow(d_raw)
      if ( ratio == 1) {
        xline <- paste0(xline, "is continuous, ratio = ", round(ratio,1))
      } 
      else if (ratio > input$threshold) {
        xline <- paste0(xline, "is as good as continuous, ratio = ", round(ratio,3))
      } else {
        xline <- paste0(xline, "is not continuous, ratio = ", round(ratio,3))
      }
      lines <- c(lines, xline)
    }
    cat(paste(lines, collapse = "\n"))
  })
  
  
  output$MissingData <- renderPlot(
    vis_dat(dat) +
      ggtitle("Missing Value Distribution")
  )
  
  output$Missingvalcat <- renderPlot({
    vis_miss(cat_data, sort_miss=FALSE)
  })
  
  output$Missingval <- renderPlot({
    vis_miss(numerical_data, sort_miss=FALSE)
  })
  
  
  output$Factors <- renderPlot({
     req(length(input$DiscreteV) > 0)
     d <- getDisContinuousData()
     req(all(input$DiscreteV %in% colnames(d)))
     formula <- as.formula(paste("~",paste(input$DiscreteV, collapse = " + ")))
     mosaic(formula, data = d,
            main = "Discontinuous variables", shade = TRUE, legend = TRUE)
    
    # df <- dat
    # 
    # # k5 <- as.factor(input$measure)
    # # k6 <- as.factor(input$measure1)
    # form <- as.formula(paste(collapse = "+", "~", input$DiscreteV))
    # mosaicplot(form, data = df, legend = TRUE, shade = TRUE)
    
    
  })
  
  
  output$Correlation <- renderPlot({
   
    numdat3 <- numerical_data %>%
      select(input$CorrelationVariable)
    numdat3 <- scale(numdat3, input$center, scale=input$scale)
    chart.Correlation(numdat3)
    
 } )
  
  
  
  output$timeSeries1 <- renderPlot({
    numdat1 <- dat %>%
      mutate(timeseries) 
    plot_data <- numdat1 %>%
      select(timeseries, input$Variablesnum1)
    #plot_data <- scale(x = plot_data, center=input$center, scale=input$scale)  
    p <-  ggplot(plot_data, aes(x= plot_data[,"timeseries"], y=plot_data[, input$Variablesnum1])) +
      geom_line(color = "blue")
    p + ggtitle("Time VS Numerical Data")+
      xlab("Time") + ylab("Numerical Data")
  })
  
  
  output$Rising <- renderPlot({
    data1 <- getContinuousData() %>%
      select(input$Variablesnum2)
    for (col in 1:ncol(data1)) {
      data1[,col] <- data1[order(data1[,col]),col] #sort each column in ascending order
    }
    # scale so they can be graphed with a y shared axis
    data1 <- scale(x = data1, center = input$centerRisingOrder, scale=input$scaleRisingOrder)  
    mypalette <- rainbow(ncol(data1))
    matplot(y = data1, type = "l", xlab = "Observations", ylab="Values", lty = 1, lwd=1, col = mypalette, main="Rising Order chart")
    legend(legend = colnames(data1), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(data1)^0.3))
  })
  
  
  output$Natural <- renderPlot({
    data5 <- getContinuousData() %>%
      select(input$Variablesnum3)
    # Normalise so they can share a common y axis
    numData <- scale(x= data5, center = input$centerNaturalOrder, scale=input$scaleNaturalOrder) 
    mypalette <- rainbow(ncol(data5))
    matplot(numData, type = "l", xlab = "Observations", col = alpha(rainbow(ncol(numData)), 1), main="Natural Order chart")
    legend(legend = colnames(data5), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(data5)^0.3))
  })
  
  getContinuousData <- reactive({
    continuous <- c()
    for (col in colnames(dat)) {
      myCol <- na.omit(dat[,col])
      ratio <- length(unique(myCol)) / length(myCol)
      if (ratio > input$Threshold & is.numeric(myCol)) {
        continuous <- c(continuous, col)
      }
    }
    req(length(continuous) > 0)
    dat[, continuous, drop = FALSE]
  })
  
  
  getDisContinuousData <- reactive({
    d <- dat
    discontinuous <- c()
    for (col in colnames(d)) {
      myCol <- na.omit(d[,col])
      ratio <- length(unique(myCol)) / length(myCol)
      if (ratio < input$Threshold) {
        discontinuous <- c(discontinuous, col)
        if (!is.factor(myCol)) {  # Convert to factor if it is not a factor
          d[,col] <- as.factor(d[,col])
        }
      }
    }
    req(length(discontinuous) > 0)
    d[, discontinuous, drop = FALSE]
  })
  
  output$boxplot <- renderPlot({
    numdat <- numerical_data %>%
      select(input$VariablesB)
    numdat <- scale(numdat, center=input$centerB, scale=input$scaleB)
    boxplot(x=numdat, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
            horizontal = FALSE, outline=input$outliers, 
            col=brewer.pal(n = dim(numdat)[2], name = "RdYlGn"),
            range = input$range)
    
  })
  
  # debug browser()
  #loaddata <- ractive({
 #   read.csv("")
 # })
  output$Homogeneity <- renderPlot({
     numericaldata <- numerical_data %>%
       select(input$VariablesH)
     
     numericaldata <- scale(numericaldata, center = input$centerH, scale = input$scaleH) # Normalise so they can share a common y axis
    matplot(numericaldata, type = "l", col = alpha(rainbow(ncol(numericaldata)), 0.4) ) #use transparency so you can see into the
    
  })
  
  output$HomogeneityTabplot <- renderPlot({
   
     dataset1 <- dat
    dataset1$rownum <-rownames(dataset1)
    tabplot::tableplot(dataset1, sortCol = "rownum")
  })
  
  
  
  output$heatmap <- renderPlotly({
    # require(ECharts2Shiny)
    df <- dat
    d <- scale(x=dplyr::select_if(df,is.numeric),center=input$centerHeatmap,scale=input$scaleHeatmap)
    
    p <- plot_ly(z = d, colors = colorRamp(c("blue", "orange")), type = "heatmap")})
  
  
})
