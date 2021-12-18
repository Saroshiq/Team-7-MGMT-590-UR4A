options(scipen=999)
library(shiny)
library(rvest)
library(jsonlite)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(DT)
library(bslib)

r <- read_html('https://stockanalysis.com/stocks/') %>% 
  html_element('#__NEXT_DATA__') %>% 
  html_text() %>% 
  jsonlite::parse_json()

  stocks <- map_df(r$props$pageProps$stocks, ~ .x)%>%
    mutate(url = paste0('https://stockanalysis.com/stocks/', s))
  stocks
  colnames(stocks) <- c('Ticker', 'Company Name', 'Market Cap', 'Industry', 'URL')
  stocks <- stocks[,1:4]
  stocks
  ticker_values <- (stocks[,1])

# Define UI for application
ui= fluidPage(
  theme = bs_theme(version = 4, bootswatch = "yeti"),
  # Header or Title Panel 
  titlePanel(title = h4("Portfolio Analysis and Optimization", align="left")),
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      #conditionalPanel(condition = "input.tabselected == 1"),
      conditionalPanel(condition = "input.tabselected == 2",h4("Portfolio Analysis"),
                       # Default value is the date in client's time zone
                       dateInput("dt", "Select a date:"),
                       selectInput("tkr","Enter tickers", choices = ticker_values, multiple = TRUE),
                       textInput("num","Enter weights"),
                       actionButton(inputId = "btn",label="Display Plots",
                                    icon("fas fa-line-chart")),
                       br(),
                       br(),
                       actionButton(inputId = "btn2",label="Display Metrics",
                                    icon("list-alt"))),
      conditionalPanel(condition = "input.tabselected == 3",h4("Portfolio Optimization"),
                       # Default value is the date in client's time zone
                       selectInput("tkr2","Enter tickers", choices = ticker_values, multiple = TRUE),
                       dateInput("dt2", "Select a date:"),
                       actionButton("btn3","Optimize",icon("fas fa-cogs"))),
      conditionalPanel(condition = "input.tabselected == 4",h4("Portfolio Backtesting"),
                       # Default value is the date in client's time zone
                       selectInput("tkr3","Enter tickers", choices = ticker_values, multiple = TRUE),
                       dateInput("dt3", "Select a date:"),
                       #dateInput("dt4", "Pick a date to compare with:"),
                       actionButton("btn4","Test",icon("fas fa-play")))
                       
    ),
    
    # Main Panel
    mainPanel(
      div(
      tabsetPanel(type="tab", id="tabselected", selected=1, 
                  tabPanel("Stock Analysis",br(),br(),dataTableOutput("T"), height = "100px",
                           width = "100px", value=1),
                  tabPanel("Portfolio Analysis", br(),plotOutput("plot1"),
                           plotOutput("plot2"),plotOutput("plot3"), 
                           dataTableOutput("table"),height = "100px",
                           width = "100px", value=2),
                  tabPanel("Portfolio Optimization", br(),plotOutput("plot4"), 
                           plotOutput("plot5"),height = "100px",
                           width = "100px", value=3),
                  tabPanel("Portfolio Backtesting", br(),plotOutput("plot6"),
                           plotOutput("plot7"), height = "100px",
                           width = "100px", value=4)
                  
      ),class = "span2")
      
    )
    
  ))




server <- function(input, output) {
  #############################CODE FOR PART 2#######################
  portfolioPrices <- NULL
  
  data <- eventReactive(input$btn, {
    
    req(input$dt)
    tickers <- as.character(unlist(strsplit(input$tkr, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt,  periodicity = "daily", auto.assign = FALSE)[,4])
    }
    #Note to self: Write Code to check benchmark prices
    # colnames(portfolioPrices) <- tickers
    portfolioPrices})
  
  #Calculate Returns For DF
  PerformanceSummary <- eventReactive(input$btn, {
    weights <- as.numeric(unlist(strsplit(input$num, ",")))
    dailyReturns <- na.omit(ROC(data(), type="discrete"))
    
    #Calculate Portfolio Returns
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    
    #Plot Performance
    chart.CumReturns(portfolioReturn)
    charts.PerformanceSummary(portfolioReturn,main="Wealth index, period performance, and drawdown")
  })
  Stats <- eventReactive(input$btn2, {
    removeUI(selector = "#plot1")
    removeUI(selector = "#plot2")
    weights <- as.numeric(unlist(strsplit(input$num, ",")))
    dailyReturns <- na.omit(ROC(data(), type="discrete"))
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    Return.portfolio(dailyReturns, weights=weights)
    benchmarkPrices <- getSymbols.yahoo("SPY", from=input$dt, periodicity = "daily", auto.assign=FALSE)[,4]
    colSums(is.na(benchmarkPrices))
    benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))
    a<-round(CAPM.beta(portfolioReturn, benchmarkReturns, .035/252),2)
    b<-round(CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/252),2)
    c<-round(CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/252),2)
    
    #CAPM.alpha(portfolioReturn, benchmarkReturns, .035/252)
    d<-round(CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252),2)
    df<-data.frame(x_xs=c("Beta","Bull","Bear","Jensen Alpha"),
                   y_xs=c(a,b,c,d))
    ggplot(df, aes(x=x_xs, y=y_xs,fill=x_xs )) + 
      geom_bar(stat="identity", width=.4) + 
      geom_text(aes(label=y_xs), position=position_dodge(width=0.5), vjust=-0.25)+
      labs(title="Capital Asset Pricing Model Metrics") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  Stats2 <- eventReactive(input$btn2, {
    weights <- as.numeric(unlist(strsplit(input$num, ",")))
    dailyReturns <- na.omit(ROC(data(), type="discrete"))
    
    #Calculate Portfolio Returns
    portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
    sharpe_ratio <- data.frame(SharpeRatio(portfolioReturn, Rf = .035/252, p = 0.95, FUN = "StdDev",
                                           weights = NULL, annualize = FALSE))
    annualized_returns <- data.frame(table.AnnualizedReturns(portfolioReturn,Rf=.035/252, geometric=TRUE))
    
    return_df<- rbind(sharpe_ratio,annualized_returns)
    names(return_df)<-c("Portfolio Returns")
    return_df
  })
  output$plot1 <- renderPlot({
    plot(data(), main = "Portfolio Prices")
  })
  output$plot2 <- renderPlot({
    PerformanceSummary()
  })
  output$plot3 <- renderPlot({
    Stats()
  })
  output$table <- renderDataTable({
    Stats2()
  })
  #############################END OF PART 2#######################

  
  #############################CODE FOR PART 3#######################
  portfolioPrices <- NULL
  opti2 <- eventReactive(input$btn3, {
    req(input$dt2)
    tickers <- as.character(unlist(strsplit(input$tkr2, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt2,  periodicity = "daily", auto.assign = F)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices))
    portf <- portfolio.spec(colnames(portfolioReturns))
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=.10, max=.40)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    
    optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
    wt<-chart.Weights(optPort)
    wt
  })
  portfolioPrices <- NULL
  opti <- eventReactive(input$btn3, {
    req(input$dt2)
    tickers <- as.character(unlist(strsplit(input$tkr2, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt2,  periodicity = "daily", auto.assign = F)[,4])
    }

    portfolioReturns <- na.omit(ROC(portfolioPrices))
    portf <- portfolio.spec(colnames(portfolioReturns))
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=.10, max=.40)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")

    optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
    wt<-chart.Weights(optPort)

    ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                                   risk_aversion = NULL)
  })
  
  output$plot5 <- renderPlot({
    opti2()
  })

  output$plot4 <- renderPlot({
    chart.EfficientFrontier(opti(),
                            match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                            cex.axis = 0.8, element.color = "red", main = "Efficient Frontier",
                            RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                            chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                            cex.assets = 0.8)
  })

  #############################END OF PART 3#######################
  
  ###########################CODE 4 BEGINS###########################
  
  portfolioPrices <- NULL
  graph6 <- eventReactive(input$btn3, {
    req(input$dt3)
    tickers <- as.character(unlist(strsplit(input$tkr3, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt3,  periodicity = "daily", auto.assign = FALSE)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices))
    
    portf <- portfolio.spec(colnames(portfolioReturns))
    
    portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
    portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
    portf <- add.constraint(portf, type="box", min=.10, max=.40)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)
    
    rp <- random_portfolios(portf, 10000, "sample")
    
    opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                                portf,
                                                optimize_method="random",
                                                rp=rp,
                                                rebalance_on="months",
                                                training_period=1,
                                                rolling_window=10)
    
    equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
    benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
    colnames(benchmark) <- "Benchmark Portfolio"
    
    sp500prices <- getSymbols.yahoo("SPY", from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,4]
    sp500Rets <- na.omit(ROC(sp500prices))
    sp500Rets <- as.xts(sp500Rets)
    
    chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")
    
    rebal_weights <-extractWeights(opt_rebal)
    rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)
    
    rets_df <- cbind(rebal_returns, benchmark, sp500Rets)
    
    
  })    
  
  graph7 <- eventReactive(input$btn4, {
    
    portfolioReturns <- na.omit(ROC(graph6()))
    
    portf <- portfolio.spec(colnames(portfolioReturns))
    
    portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
    portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
    portf <- add.constraint(portf, type="box", min=.10, max=.40)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)
    
    rp <- random_portfolios(portf, 10000, "sample")
    
    opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                                portf,
                                                optimize_method="random",
                                                rp=rp,
                                                rebalance_on="months",
                                                training_period=1,
                                                rolling_window=10)
    
  })
  
  output$plot6 <- renderPlot({
    charts.PerformanceSummary(graph6(), main="P/L Over Time")
  })
  
  output$plot7 <- renderPlot({
    chart.Weights(graph7(), main="Rebalanced Weights Over Time")
  })
  
  

#################################End of CODE 4########################################

   output$T <- renderDataTable({
       stocks 
     })
  }
  
shinyApp(ui = ui, server = server)