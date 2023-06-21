library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(stringr)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(plotly)

str1 = readRDS('tickers/str1')
str2 = readRDS('tickers/str2')

checkbox_list = setNames(str1, str1)
checkbox_list = checkbox_list[1]

# x = "btc eth lina reef self sand mdt neo cfx fet twt Vet matic sand ar chz dent doge dot fet icp lrc fet mana mask mina near glmr rsr movr Dydx algo auction dar ens imx loka woo scrt ape gal arb op stg apt hook ldo inj ankr cfx ocean xor ceres dego ceek hero ufo bonk dexe oxt tlm tko tru burger lit ckb uma cro mbox yfdai perp rfox ygg iota gala sui yooshi sidus cru wnxm cwar swash velo vra srm ordi icx snx flux ilv ach high pha pros vite iost qtum trx ctxc phb ctk ont ern nkn wtc cocos chess for dodo df yfii sun kas kda fil alu ksm orn dc caw tomo celer one mx dext deso psg og cgg rare hft agix zil nexa fida tvk cream hard eos atom pdex kai brise pyr joe"
# str1 = strsplit(x, split = " ")[[1]]
# str1 = paste0(str1,"usdt")
# str1 = toupper(str1)
# str2 = tolower(str1)
# checkbox_list = setNames(str2,str1)
# 
# checkbox_list = list("Bitcoin" = 'btcusd',
#                      "Ethereum" = 'ethusd',
#                      "Linear Finance" = "linausdt",
#                      "Reef" = 'reefusdt',
#                      'Selfkey' = 'keyusdt',
#                      'Sandbox' = 'sandusd',
#                      'Measureable Data Token' = 'mdtusd',
#                      'Neo' = 'neousd',
#                      'Conflux' = 'cfxusdt',
#                      'Fetch.AI' = 'fetusd',
#                      'Trust Wallet Token' = 'twtusdt',
#                      'XRP' = 'xrpusd',
#                      'VETUSDT' = 'vetusdt',
#                      'MATICUSDT' = 'maticusdt',
#                      'ARUSDT' = 'arusdt',
#                      'CHZUSDT' = 'chzusdt',
#                      'DENTUSDT' = 'dentusdt',
#                      'DOGEUSDT' = 'dogeusdt',
#                      'DOTUSDT' = 'dotusdt',
#                      'ICPUSDT' = 'icpusdt')

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Crypto Currency",
    mainText = 'Predictor',
    badgeText = "v1.4"
  ),
  titleWidth = 300
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Overview/Backtesting", tabName = "create", icon = icon("house")),
      #menuItem("Predict Tomorrow", tabName = "predict"),
      menuItem("Predict Next Candle (Multiple)", tabName = 'predictMultiple', icon = icon('money-bill-trend-up')),
      menuItem("Predict Next 7 Days/Weeks", tabName = 'predictNextWeek', icon = icon('chart-line')),
      menuItem("Build TradingView Model", tabName = 'inputCoin', icon = icon('upload'))
      
      # menuItem("Most Likely Outcome", tabName = "likely")
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      tabItem(tabName = "create",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                # HTML('<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_blank">
                #        <input type="hidden" name="cmd" value="_s-xclick">
                #        <input type="hidden" name="hosted_button_id" value="2MGB68YUJEB5Q">
                #        <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_subscribeCC_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!" target="_blank">
                #        <img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1" target="_blank">
                #        </form>'),
                
                strong(h1("Creating a Model:")),
                box(width = 10,
                paste0("On this tab you can use the sliders to modify how the predictive model is created. First you need to select a timeframe ",
                       "and coin that you're interested in predicting. ","The first slider is used ",
                       "to select the percentage increase in the timeframe that you've selected. The second slider is used ",
                       "to select how confident you want the model to be in order to classify a 'BUY'. The model will make a prediction on a scale from ",
                       "0-1, the closer to 1 the prediction is, the more confident the model is that your selected percentage increase will happen in your selected timeframe."),
                ),
                br(),

      
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                         selectInput("timeframe","Pick a Timeframe", choices = list("5 Minute" = "5min",
                                                                                    "15 Minute" = "15min",
                                                                                    "30 Minute" = "30min",
                                                                                    '1 Hour' = '1hour',
                                                                                    "4 Hour" = "4hour",
                                                                                    "8 Hour" = "8hour",
                                                                                    "1 Day" = "1day",
                                                                                    "1 Week" = "7day",
                                                                                    "1 Month" = '1month')),
                         selectInput("select","Pick a crypto to predict", choices = checkbox_list),
                         br(),
                         sliderInput("slider1","Select Percentage Increase", min = -2, max = 2, step = 0.1, value = 0),
                         sliderInput("slider2", "Confidence Score 'BUY' Threshold", min = 0.1, max = 1, step = 0.02, value = 0.9),
                         # strong("Note: IT IS STRONGLY RECOMMENDED TO PLACE YOUR TAKE PROFIT TO THE SAME VALUE AS YOUR TARGET PERCENTAGE INCREASE"),
                         # br(),
                         # paste0("Metrics by default are calculated based on the candles closing value. ",
                         #        "You can use the TP (take profit) input field to specify your TP. ",
                         #        "Setting a TP can limit your gains, but can also limit your losses! ",
                         #        "Leaving the TP value at 0 will set the metrics to be calculated based on candles closing value."),
                         # numericInput('tp',"Set TP % (must be positive)", value = 0, min = 0),
                         # numericInput("sl","Set SL (must be negative)", value = 0, max = 0),
                         actionButton('action1', label = "Generate"),
                         br(),
                         br(),
                       ),
                       box(title = "Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                         # infoBoxOutput("OverallAccuracy", width = 6),
                         infoBoxOutput("Buy", width = 6),
                         infoBoxOutput("SumPercentage", width = 6),
                         # infoBoxOutput("DontBuy", width = 6),
                         infoBoxOutput("Predictions", width = 6),
                         infoBoxOutput("Hits", width = 6)
                         
                       ),
                       box(title = "Histogram", width = NULL, status = "primary", solidHeader = TRUE,
                         paste0("Ideally, we'd like there to be a near 0 probability or a near 1 probability for all predictions. ",
                                "Values that are more in the middle can give us an unclear prediction."),
                         plotOutput("modelPlot")
                       )
                ),
                column(width = 6,
                box(width = NULL, title = "Backtest", status = "primary", solidHeader = TRUE,
                  strong(h4("Variable Info:")),
                  strong('Actual:'),
                  paste0("If the next candle actually hit the target percentage increase, this will be 'HIT TARGET', otherwise 'MISSED TARGET'. ",
                         "The color will be GREEN if a profit could have been made and RED if a loss could have been made."),
                  br(),
                  strong("Actual High:"),
                  paste0("This was the next candles high"),
                  br(),
                  strong("Actual Low:"),
                  paste0("This was the next candles low"),
                  br(),
                  strong("Actual Close:"),
                  paste0("This was the next candles close"),
                  br(),
                  strong("Confidence Score:"),
                  paste0("This is the confidence the model has that the next candle would reach the target percentage increase (on a scale of 0 to 1)"),
                  br(),
                  strong("Signal:"),
                  paste0("If the 'Confidence Score' is higher than the selected prediction BUY threshold, this will be 'DID BUY', otherwise 'DIDN'T BUY'"),
                  br(),
                  br(),
                  dataTableOutput("table1")
                )
              )
              )
      ),
      # tabItem(tabName = "predict",
      #         fluidRow(
      #           strong(h3("About:")),
      #           strong("Note that you must create a model on the previous tab before predicting tomorrow!"),
      #           br(),
      #           paste0("This tab will simply generate a prediction using the model you created in the 'Creating a Model' Tab. ",
      #                  "Remember that the probability is on a scale of 0-1, where a larger value represents a higher probability of your ",
      #                  "model's prediction coming true."),
      #           br(),
      #           br(),
      #           box(title = "Predict Tomorrow", status = "primary", solidHeader = TRUE,
      #             textInput("open","Open"),
      #             textInput("close","Close"),
      #             textInput("low","Low"),
      #             textInput("high","High"),
      #             actionButton("action2","Predict"),
      #             br(),
      #             br(),
      #             infoBoxOutput("predict", width = NULL)
      #           )
      #         )
      #         ),
      tabItem(tabName = "predictMultiple",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Predict Next Candle (Multiple):")),
                box(width = 10,
                paste0("On this tab you can generate predictions for multiple coins! Simply use the check boxes to select which coins you'd like to predict.",
                       " If you'd like to export these results, simply press the 'csv' button on top of the table below."),
                ),
                br(),
                br(),
                box(title = "Predict Multiple", status = "primary", solidHeader = TRUE,
                    # actionButton('selectall','Select All'),
                    selectInput('checkGroup',label = 'Select Coin(s)', choices = checkbox_list, multiple = TRUE),
                    # checkboxGroupInput('checkGroup', label = 'Select Coin(s)',
                    #                    choices = checkbox_list,
                    #                    selected = 'btcusd'),
                    selectInput("timeframePredict","Pick a Timeframe", choices = list("5 Minute" = "5min",
                                                                                      "15 Minute" = "15min",
                                                                                      "30 Minute" = "30min",
                                                                                      '1 Hour' = '1hour',
                                                                                      "4 Hour" = "4hour",
                                                                                      "8 Hour" = "8hour",
                                                                                      "1 Day" = "1day",
                                                                                      "1 Week" = "7day",
                                                                                      "1 Month" = '1month')),
                    sliderInput("slider3", "Select Prediction 'BUY' Threshold", min = 0.1, max = 1, step = 0.05, value = 0.9),
                    actionButton("action4","Predict"),
                    br(),
                    br(),
                    strong(paste0("****** NOTE THAT I AM NOT RESPONSIBLE FOR FINANCIAL LOSS OR GAIN. PLACE TRADES AT YOUR OWN RISK. ",
                                  "IT IS GOOD TO USE THIS TOOL TO HELP YOU MAKE DECISIONS SUPPORTED BY OTHER EVIDENCE. ******")),
                    br(),
                    br()


                ),
                box(title = "Candlestick Chart", status = "primary", solidHeader = TRUE,
                    br(),
                    selectInput('candlestickInput','Choose Crypto to View (options updated after predictions are made)', choices = NULL),
                    plotlyOutput('candlestickPlot')

                ),

                box(title = "Predictions", status = "primary", solidHeader = TRUE, width =12,
                    br(),
                    strong(textOutput('timeRemaining')),
                    br(),
                    strong(h4("Variable Info:")),
                    strong('Coin:'),
                    paste0("The coin being predicted"),
                    br(),
                    strong('Price.Change:'),
                    paste0("The price change that's being predicted"),
                    br(),
                    strong('Confidence.Score.HIT.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL be hit next candle"),
                    br(),
                    strong('Confidence.Score.MISS.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL NOT be hit next candle"),
                    br(),
                    strong('Signal:'),
                    paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold"),
                    br(),
                  dataTableOutput("multipleOutput"),

                )

              )
      ),

      tabItem(tabName = "inputCoin",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Generate Model using TradingView Data:")),
                box(width = 10,
                paste0("On this tab you can generate a predictive model for data that you input from TradingView. You need to export TradingView data ",
                       "with no indicators on the chart. The 'Time Format' must also be set to ISO time. Name the exported file follwing the format <coinsymbol>.csv. For example, BTCUSD data would simply be BTCUSD.csv. Once you've exported the TradingView data",
                       " you simply drag that file into the input below. A timeframe must also be selected."),
                ),
                br(),
                br(),
                box(title = "Build Model from TradingView", status = "primary", solidHeader = TRUE,
                    fileInput('tvDataDrop', label = 'Input TradingView Data Here'),
                    selectInput("tvTimeFrame","Pick a Timeframe", choices = list("5 Minute" = "5min",
                                                                                 "15 Minute" = "15min",
                                                                                 "30 Minute" = "30min",
                                                                                 '1 Hour' = '1hour',
                                                                                 "4 Hour" = "4hour",
                                                                                 "8 Hour" = "8hour",
                                                                                 "1 Day" = "1day",
                                                                                 "1 Week" = "7day",
                                                                                 "1 Month" = '1month')),
                    sliderInput("tvSlider","Select Percentage Increase", min = 1, max = 5, step = 1, value = 1),
                    actionButton('action6','Predict')

                ),
                box(title = "Predict Next Candle", status = "primary", solidHeader = TRUE, width =12,
                    strong(h4("Variable Info:")),
                    strong('Coin:'),
                    paste0("The coin being predicted"),
                    br(),
                    strong('Price.Change:'),
                    paste0("The price change that's being predicted"),
                    br(),
                    strong('Confidence.Score.HIT.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL be hit next candle"),
                    br(),
                    strong('Confidence.Score.MISS.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL NOT be hit next candle"),
                    br(),
                    strong('Signal:'),
                    paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold"),
                    br(),
                withSpinner(dataTableOutput('TVPrediction'))
                )

              )
      ),
      
      tabItem(tabName = "predictNextWeek",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Predict Next 7 Days/Weeks:")),
                box(width=10,
                paste0("On this tab you may pick a crypto to forecast for the next 7 days/weeks! The machine learning model utilizes the past 14 candles of data ",
                       "to predict the next 7 candles price movements!"),
                ),
                br(),
                br(),
                br(),
                br(),
                strong('Note: Previous data is displayed in BLUE while forecasted data is displayed in RED'),
                br(),
                br(),
                box(title = "Predict Next 7 Days/Weeks", status = "primary", solidHeader = TRUE,
                    selectInput('selectTimeFrame', 'Select a time frame', choices = list('7 Days' = 'daily',
                                                                                         '7 Weeks' = 'weekly')),
                    selectInput('selectNextWeek', "Select a Coin", choices = checkbox_list),
                    actionButton("action5", "Predict"),
                    br(),
                    br()


                ),
                plotOutput("nextWeekOutput")
              )
      )
    )
  )
  
  
)

# Define server logic
server <- function(input, output, session) {
# .GlobalEnv = environment()
  # Read in functions
  source("DogeCoinML.R")
  
  output$timeRemaining = renderText(paste0("Please note there is ",getTimeRemaining(input$timeframePredict)," before the current candle closes! displayed predictions are for the current candle!"))
  output$TVPrediction = NULL
  
  observeEvent(input$action3, {
    showModal(modalDialog("Predicting Most Likely...", footer = NULL))
    on.exit(removeModal())
    all.bst.names = list.files(path = "bsts", pattern = ".rds")
    all.bst.numbers = str_match(string = all.bst.names, pattern = "bst_(.*)\\.")[,2]
    all.bst.path = list.files(path = "bsts", pattern = ".rds", full.names = TRUE)
    all.bst = lapply(all.bst.path, readRDS)
    assign('all.bst.numbers',all.bst.numbers,.GlobalEnv)
    assign('all.bst',all.bst,.GlobalEnv)
    
    predict.best(0.3, all.bst, all.bst.names)
    
    all.predictions = round(all.predictions, digits = 4)
    max.pred = which(all.predictions == max(all.predictions))
    max.bst = all.bst.numbers[max.pred]
    
    output$mostLikely = renderText(paste0("The most probable outcome over the next 24 hours is a change of ",max.bst,"% or more."))
    output$percentChance = renderText(paste0(max(all.predictions)," Probability Predicted"))
  })

  
  observeEvent(input$action1, {
    showModal(modalDialog("Generating Your Model...", footer = NULL))
    on.exit(removeModal())
    createModel(input$slider1, input$slider2, input$select, input$timeframe, input$slider1)
    # output$OverallAccuracy = renderInfoBox({
    #   infoBox("Overall Accuracy",paste0(round(overall.accuracy, digits = 2), "%"), icon = icon('check'))
    #   })
    output$Buy = renderInfoBox({infoBox("Profitable Trades", paste0(round(yes.buy.correct.perc, digits = 2), "%"), icon = icon("thumbs-up"))
    })
    output$SumPercentage = renderInfoBox({
      infoBox("Sum Percentage", paste0(round(sum.percentage, digits = 2), "%"),icon = icon("money-bill-trend-up"))
      })
    # output$DontBuy = renderInfoBox({infoBox("'Don't Buy' Correct", paste0(round(no.buy.correct.perc, digits = 2),"%"),icon = icon("thumbs-down"))
    #   })
    output$Predictions = renderInfoBox({infoBox("Number of Predictions", paste0(nrow(compare)))
    })
    output$Hits = renderInfoBox({infoBox("Number of BUYS", paste0(nrow(compare[compare$Signal == 'DID BUY',])))
    })
    
    colnames(compare) = c('Actual', 'Actual High', 'Actual Low','Actual Close', 'Confidence Score', 'Signal', 'profit')
    

    
    
    compare$`Actual High` = paste0(compare$`Actual High`,"%")
    compare$`Actual Low` = paste0(compare$`Actual Low`,"%")
    compare$`Actual Close` = paste0(compare$`Actual Close`,"%")
    
    compare$Signal[compare$Signal == 1] = "DID BUY"
    compare$Signal[compare$Signal == 0] = "DIDN'T BUY"
    
    compare$Actual[compare$Actual == 0] = 'MISSED TARGET'
    compare$Actual[compare$Actual == 1] = 'HIT TARGET'
    
    table1.colored = datatable(compare, rownames = FALSE, options = list(pageLength = 20,
      columnDefs = list(list(targets = 6, visible = FALSE))
    )) %>%
      formatStyle('Actual','profit',
                  backgroundColor = styleEqual(c(0,1), c('darkred','lightgreen'))) %>%
      formatStyle('Signal',
                  backgroundColor = styleEqual(c("DIDN'T BUY","DID BUY"), c('darkred','lightgreen')))

    
      
    output$table1 = renderDataTable(table1.colored)
    # output$modelPlot = renderPlot(hist(compare$Confidence.Score))
    output$modelPlot = renderPlot(ggplot(data = compare, aes(x = `Confidence Score`)) + geom_histogram(colour = "blue", alpha = 0.3))
  })
  
  observeEvent(input$action2, {
    predict.tomorrow(0.3, input$select)
    output$textToday = renderText(paste0("Probability of: ",round(predict.now, digits = 4)))
    output$predict = renderInfoBox({
      infoBox("Predicted Probability",round(predict.now, digits = 4))
    })
  })
  
  observeEvent(input$action4, {
    showModal(modalDialog("Generating predictions...", footer = NULL))
    on.exit(removeModal())
    x = input$checkGroup
    updateSelectInput(session = session, inputId = 'candlestickInput', choices = x, selected = head(x,1))
    
    predict.tomorrow.multiple(input$checkGroup, input$timeframePredict, input$slider3, .GlobalEnv)
    dt.colored = datatable(predictions.df.comb,
                           rownames = FALSE,
                           extensions = "Buttons",
                           options = list(paging = FALSE, searching = FALSE, dom = 'Bfrtip', buttons = c('csv'))) %>%
      formatStyle("Signal",
                  backgroundColor = styleEqual(c("DON'T BUY SIGNAL", "BUY SIGNAL"), c('darkred','lightgreen')))
    output$multipleOutput = renderDataTable(dt.colored)
    output$candlestickPlot = renderPlotly(createCandlePlot(input$candlestickInput))
  })

  observeEvent(input$action5, {
    showModal(modalDialog("Generating predictions...", footer = NULL))

    output$nextWeekOutput = renderPlot(predict_week(input$selectNextWeek, input$selectTimeFrame))
    on.exit(removeModal())
    
  })
  
  observeEvent(input$action6, {
    showModal(modalDialog("Generating predictions...", footer = NULL))
    on.exit(removeModal())
    
    if(is.null(input$tvDataDrop)){
      return(NULL)
    }else{
      df = input$tvDataDrop
    }
    
    output$TVPrediction = renderDataTable(build.TV.model(df, input$tvTimeFrame))
    # output$TVPrediction = renderDataTable(predictions.df.comb)
    
  })
  
  observeEvent(input$selectall, {
    updateCheckboxGroupInput(session = session, 'checkGroup',choices = checkbox_list, selected = checkbox_list)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
