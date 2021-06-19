# options(error=browser)
options(error=NULL)  
options(warn=-1)  # 끄기
# options(warn=0)  # 켜기
# getOption("warn")
source("Base/global.R", encoding="UTF-8")

ui <- navbarPage( "SmartSolutionShiny", id="SmartReportMain", 
                  theme=shinytheme("cerulean"), # United
                  useShinyjs(),  # Set up shinyjs
                  selected="Sourcing",
                  tabPanel("전처리", value="Tidying",
                           tidyingMainUI() ),
                  tabPanel("Sourcing",
                           sourcingMainUI() ),
                  tabPanel(paste0("Sampling"), value="Sampling",
                           samplingMainUI() ),

                  tabPanel(paste0("Explore"), value=paste0("Explore"), 
                           exploreMainUI()
                  ),
                  tabPanel(paste0("ExploreTable"), value=paste0("ExploreTable"),
                           exploreTableMainUI()
                  ),
                  tabPanel("Modeling",
                           modelingMainUI()
                  ),
                  tabPanel(paste0("Predict"), value="Predict",
                           predictMainUI()
                  )
                  
                  # tabPanel(paste0("코일 연결부 길이 예측"), value="PredictSpareCoilLength",
                  #          predictSpareCoilLengthModuleUI("PredictSpareCoilLength")
                  # )
                  # tabPanel(paste0("Optimize"), value="Optimize",
                  #          optimizeMainUI()
                  #          
                  # )
)

server <- function(input, output, session) {

  observe({
    switch(input$SmartReportMain,
           Tidying = {
             hideTab(inputId="SmartReportMain", target="Sampling")
             hideTab(inputId="SmartReportMain", target="Explore")
             hideTab(inputId="SmartReportMain", target="ExploreTable")
             hideTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
           },
           Sourcing = {
             hideTab(inputId="SmartReportMain", target="Sampling")
             hideTab(inputId="SmartReportMain", target="Explore")
             hideTab(inputId="SmartReportMain", target="ExploreTable")
             hideTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
           },
           Sampling = {
             if(!is.na(curSampleExploreTable)) {
               curSampleExplore <<- curSampleExploreTable
               curSampleExploreTable <<- NA
             }
             
           },
           ExploreTable = {
             if(is.na(curSampleExploreTable)) 
               curSampleExploreTable <<- curSampleExplore
           },
           

           Predict = {

             
             hideTab(inputId="predict", target="predPlotSeed1")
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
  
  # modelingMain Tab
  modelingMain(input, output, session)
  
  # Predict Tab
  predictMain(input, output, session)
  
  # # 코일 연결부 길이 예측
  # callModule(predictSpareCoilLengthModule,"PredictSpareCoilLength")
  
}

shinyApp(ui, server)


#shinyApp(ui=htmlTemplate("www/test.html"), server)
