

modelingMainUI <- function() {
  tabsetPanel( type="tabs", id="modeling", selected="revampModel",
               
               tabPanel(title="갱신", value="revampModel",
                        fluidPage(
                          fluidRow(
                            column(2, actionButton("developModel", "모델 재학습") ),
                            column(2, actionButton("renderReportModeling", "검증 리포트") )
                          ),
                          tags$hr(),
                          fluidRow(
                            column(3,

                                   radioButtons("modelSelectTipA", "모델 선택",
                                                c("모델 mtcars " = "mtcars"
                                                ))
                                   
                            ),
                            
                            column(9,
                                   verbatimTextOutput("strModeling"))
                          )
                        )
                        
               ),
               tabPanel(title="측정-예측", value="predMeas1",
                        predMeas1TabModuleUI("predMeas1")
               )
  )
  
  
}

modelingMain <- function(input, output, session) {
  observe({
    switch(input$modeling,
           revampModel = {
             
           },
           predMeas1 = {
             # updateTextInput(session, inputId="predPlotSeed1-VF2Level", value="0.01 0.02 0.03")
             if(is.null(curSelModelResultReport)) return()
             # curSampleExplore <<- add_predictions(curSampleExplore, curSelModel, var="predVal")
             curSampleExplore <<- add_predictions(curSampleExplore, curSelModelResultReport, var="predVal")
             df <- curSampleExplore
             attr(df[,"predVal"], "max") <- max(df[,"predVal"], na.rm=TRUE)
             attr(df[,"predVal"], "mean") <- mean(df[,"predVal"], na.rm=TRUE)
             attr(df[,"predVal"], "min") <- min(df[,"predVal"], na.rm=TRUE)
             attr(df[,"predVal"], "label") <- attr(DFSource[,"predVal"], "label")
             curSampleExplore <<- df
             

             aesList[["y"]][1] <<- dfModelNest[["modelY"]][[1]]
             graphOption[["yAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "label")
             graphOption[["minY"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "min")
             graphOption[["maxY"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "max")
             aesList[["x"]][1] <<- "predVal"
             graphOption[["xAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["x"]][1]], "label")
             graphOption[["minX"]][1]<<- attr(curSampleExplore[,aesList[["x"]][1]], "min")
             graphOption[["maxX"]][1]<<- attr(curSampleExplore[,aesList[["x"]][1]], "max")
             
           }
    )
  })
  
  
  
  
  
  observe({
    treatModalModeling(input, output, session)
    
  })

  observeEvent(input$developModel, {
    shinyjs::toggle("developModel")
    modelSelectTipA <- isolate(input$modelSelectTipA)


    modelTemplate <- paste0("model_",modelSelectTipA)
    # dfModel <<- divideDomain(curSampleExplore, modelProcess, modelGrCC)
    dfModel <<- curSampleExplore



    switch(modelTemplate,
           model_mtcars = {
             dfModelNest <- renderModel_mtcars(dfModel)
           },


           mtcars = {
             dfModel <- dfModel %>% mutate(clusterGr="mtcars")
             dfModelNest <- dfModel %>% 
               group_by(clusterGr) %>% 
               nest()
             modelY <- "mpg"
             modelDirectRaw <- vector("list", length(dfModelNest$clusterGr))
             modelDirectRaw[[1]] <- c("wt","am","gear","hp")
             modelFramework <- vector("list", length(dfModelNest$clusterGr))
             model <- vector("list", length(dfModelNest$clusterGr))
             modelFramework[[1]] <- function(df) {
               lm(mpg ~ wt + am + gear + hp, data = df)
             }
           },
           diamonds = {

           },
           {}
           
    )

    for(i in 1:length(dfModelNest$clusterGr)) {
      dfModelNest$model[[i]] <- dfModelNest$modelFramework[[i]](dfModelNest$data[[i]])
      dfModelNest[["data"]][[i]] <- dfModelNest[["data"]][[i]] %>%
        mutate(rStandard=rstandard(dfModelNest[["model"]][[i]])) %>% 
        attachAttrOneVar( var="rStandard", label="rStandard", labelShort="rStandard") %>%
        mutate(cooksDist=cooks.distance(dfModelNest[["model"]][[i]])) %>%
        attachAttrOneVar( var="cooksDist", label="cooksDist", labelShort="cooksDist")

    }
    # dfModelNest$model[[1]]

    
    pathModel <- paste0("../Model/",dfModelNest$path[[1]], modelTemplate)
    save(dfModelNest,
         file=paste(pathModel,  "/모델.Rdata", sep=""))
    strAlert <- paste0(pathModel,"에 모델.Rdata가 저장되었습니다.")
    alert(strAlert)
    shinyjs::toggle("developModel")
    
  })
  observeEvent(input$renderReportModeling, {
    shinyjs::toggle("renderReportModeling")   
    modelSelectTipA <- isolate(input$modelSelectTipA)
    modelTemplate <- paste0("model_",modelSelectTipA)
    
    pathMiddle <- ""
    if(str_detect(modelTemplate,"_AR_")[1]) pathMiddle <- "AR/"  ### for model developer
    if(str_detect(modelTemplate,"_CR_")[1]) pathMiddle <- "CR/"  ### for model developer
    if(str_detect(modelTemplate,"_HT_")[1]) pathMiddle <- "HT/"  ### for model developer
    if(str_detect(modelTemplate,"_ACC_")[1]) pathMiddle <- "ACC/"  ### for model developer

    pathModel <- paste0("../Model/",pathMiddle,modelTemplate)
    pathHTMLReportModelVerify <<- paste0("../USER/",pathMiddle,modelTemplate,"/output")
    load(file=paste(pathModel,  "/모델.Rdata", sep=""))
    dfModelNest <<- dfModelNest
    dfReportCommon <<- as.data.frame(dfModelNest[["data"]][1])  ### clusterGr에 여러개일 때 수정 필요
    # dfModelNest$model[[2]]
    triggerMCP <<- "modelResult"
    aesList[["y"]] <<- dfModelNest$modelY[1]
    showModal(ModalModeling())
    
    
  })
  
  callModule(predMeas1TabModule,"predMeas1")

}