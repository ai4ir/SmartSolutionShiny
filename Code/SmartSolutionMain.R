# options(error=browser)
options(error=NULL)  
source("Base/global.R", encoding="UTF-8")



ui <- navbarPage( "SSS", id="SmartReportMain",
                  theme=shinytheme("cerulean"),
                  # cerulean  cosmo cyborg darkly flatly journal united
                  useShinyjs(),  # Set up shinyjs
                  selected="Sourcing",
                  tabPanel("전처리", value="Tidying",
                           tidyingMainUI() ),
                  tabPanel("Sourcing", value="Sourcing",
                           sourcingMainUI() ),
                  tabPanel(paste0("Sampling"),
                           samplingMainUI() ),

                  tabPanel(paste0("그래프 탐색"), value="Explore",
                           exploreMainUI()
                  ),
                  tabPanel(paste0("ExploreTable"), value=paste0("ExploreTable"),
                           exploreTableMainUI()
                  ),
                  tabPanel(paste0("기계 학습"), value="MachineLearning",
                           machineLearningMainUI()
                  ),
                  tabPanel("Modeling",
                           modelingMainUI()
                  ),
                  tabPanel(paste0("Predict"), value="Predict",
                           predictMainUI()
                  ),
                  tabPanel(paste0("Optimize"), value="Optimize",
                           optimizeMainUI()
                  ),  
                  tabPanel(paste0("globalOptionSS"), value="옵션",
                                                globalOptionSSMainUI()
                  )

)

server <- function(input, output, session) {
  
  observe({
    switch(input$SmartReportMain,
           Tidying = {
             hideTab(inputId="SmartReportMain", target="Sampling")
             hideTab(inputId="SmartReportMain", target="Explore")
             hideTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
             
           },
           Sourcing = {
             showTab(inputId="SmartReportMain", target="Sampling")
             showTab(inputId="SmartReportMain", target="Explore")
             showTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
             fromReportCommon <<- "_source"
           },
           
           Sampling = {
             if(!is.na(curSampleExploreTable)) {
               curSampleExplore <<- curSampleExploreTable
               curSampleExploreTable <<- NA
             }
             fromReportCommon <<- "_sample"
             
           },
           ExploreTable = {
             if(is.na(curSampleExploreTable)) 
               curSampleExploreTable <<- curSampleExplore
           },
           Modeling = {
             # updateTabsetPanel(session, "modeling", selected = "predMeas1")
             updateTabsetPanel(session, "modeling", selected = "revampModel")
           },
           Predict = {
             showTab(inputId="predict", target="predPlotSeed1")
             if(is.null(ggObjPred1)) {
               hideTab(inputId="predict", target="predPlot1")
             } else {
               showTab(inputId="predict", target="predPlot1")
               output$predPlot1 <- renderText({
                 predPlot1TabTitle
               })
             }
             if(is.null(ggObjPred2)) {
               hideTab(inputId="predict", target="predPlot2")
             } else {
               showTab(inputId="predict", target="predPlot2")
               output$predPlot2 <- renderText({
                 predPlot2TabTitle
               })
             }
             if(is.null(ggObjPred3)) {
               hideTab(inputId="predict", target="predPlot3")
             } else {
               showTab(inputId="predict", target="predPlot3")
               output$predPlot3 <- renderText({
                 predPlot3TabTitle
               })
             }
             
           }
    )
  })
  
  output$sum1 <- renderPrint({
    summary(cars)
    
  })   
  
  # Tidying Tab
  tidyingMain(input, output, session)
  
  # Sourcing Tab
  sourcingMain(input, output, session)
  
  # Sampling Tab
  samplingMain(input, output, session)
  

  # Explore Tab
  exploreMain(input, output, session)
  
  # ExploreTable Tab
  exploreTableMain(input, output, session)
  
  # MachineLearning Tab
  machineLearningMain(input, output, session)

  # modelingMain Tab
  modelingMain(input, output, session)

  #     
  # #    callModule(sourcingModule,"Predict")                       # 공통 모듈 
  #  #   PredictFunc(input, output, session)
  # 
  
  # Predict Tab
  predictMain(input, output, session)
  
  # Optimize Tab
  optimizeMain(input, output, session)
  
  # globalOptionSS Tab
  globalOptionSSMain(input, output, session)
  
  
  
}

shinyApp(ui, server)


#shinyApp(ui=htmlTemplate("www/test.html"), server)
