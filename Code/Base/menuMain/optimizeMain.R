optimizeMainUI <- function() {
  
    tabsetPanel( type="tabs", id="optimize",

                 tabPanel("최적화", value="optimizeTab1",
                          fluidPage(
                              fluidRow(
                                  column(1, actionButton("fileOptimize", "파일")),
                                  column(1, actionButton("doPredictOptim", "예측값 갱신")),
                                  column(3, h3(textOutput("PredResultOptim"))),
                                  # column(2, h3(textOutput("PredResultFailOptim"))),
                                  column(1, actionButton("doOptimize", "최적값 갱신")),
                                  column(1, h3("Y 목표값")),
                                  column(5, inlineCSS("#targetVarValue {font-size: 20px;}"),
                                         numericInput("targetVarValue", NULL, value=0))
                              ),
                              fluidRow(
                                column(2),
                                column(10, h3(textOutput("PredResultFailOptim")))

                              ),
                              tags$hr(),
                              fluidRow(
                                  column(3, renderOptimizeUI_2() ),
                                  column(9,
                                         includeCSS("Base/www/html/OptimizeTable.css"),
                                         column(12, htmlTemplate("Base/www/html/OptimizeTable.html")))

                              )
                          )
                 )
                 
    )

}


optimizeMain <- function(input, output, session) {
  
  observeEvent(input$optimize, {
    curTabOptimize <<- input$optimize

  })
  
  observeEvent(reactDFSource(),{

    # 샘플링 조건 탭의 입력 위젯 처리
    revampOptimizeUI(input, output, session)
    revampOptimizeUI_2(input, output, session)

  })
  
  ### optimize 초기  조건 파일 처리 ###
  ### optimize 초기  조건 파일 처리 ###
  ### optimize 초기  조건 파일 처리 ###
  observeEvent(input$fileOptimize, {
    getOptimizeOptimize(input, output, session)
    showModal(ModalFileOptimize(pathDir=pathNameFileDesign))
  })
  observeEvent(input$renderModalFileOptimize2, {
    showModal(ModalFileOptimize2(selFileName=input$nameFileOptimize, failed=FALSE))
  })
  
  observeEvent(input$saveModalFileOptimize, {
    getOptimizeOptimize(input, output, session)
    namePathFile <- paste0(pathNameFileDesign,"/", input$selNameFileOptimize)
    curDesignDF <<- curOptimizeDF 
    save(curDesignDF, file=namePathFile)
    removeModal()
    # curPathNameFileDomain <<- namePathFile
    # save(curPathNameFileDomain, file="initEnv.Rdata")
  })
  
  observeEvent(input$loadModalFileOptimize, {
    
    namePathFile <- paste0(pathNameFileDesign,"/", input$selNameFileOptimize)
    # extFileName <- str_extract("xxx.xlsx","xlsx")
    #
    # if(extFileName=="xlsx") {
    #     updateDomainUIFromEXCEL(input,output, session, namePathFile=namePathFile)
    #
    # } else {
    #     load(file=namePathFile, .GlobalEnv)
    #     updateDomainUI(input,output, session)
    # }
    
    load(file=namePathFile, .GlobalEnv)
    curOptimizeDF  <<- curDesignDF
    updateOptimizeUI(input,output, session)
    removeModal()
    
    
  })
  
  ### 예측 실행 ### 
  ### 예측 실행 ### 
  ### 예측 실행 ### 
  
  
  observeEvent(input$doPredictOptim, {
    getOptimizeOptimize(input, output, session)
    curOptimizeDF <<- curOptimizeDF %>% add_predictions(dfModelNest[["model"]][[1]], var="predVal")
    reactPredValOpt(round(curOptimizeDF[1,"predVal"], 
                       attr(DFSource$predVal,"digit")))
  })
  
  observeEvent(input$doOptimize, {
    getOptimizeOptimize(input, output, session)
    curOptimizeDF <<- curOptimizeDF %>% add_predictions(dfModelNest[["model"]][[1]], var="predVal")
    reactPredValOpt(curOptimizeDF[1,"predVal"])

    
    updateOptimumValueUI(input,output,session)
    # taregtVarValue <<- input[["targetVarValue"]]
    # curOptVar <<- "C"
    # digit <- attr(curOptimizeDF,"digit")[which(attr(curOptimizeDF,"names")==curOptVar)]
    # optValue <- optim(c(curOptimizeDF[1,"C"]), meas_distance, data=curOptimizeDF)
    # html("OptimizeTable1Optim1",round(optValue[["par"]],digit) )
    
    
  })
  
  output$PredResultOptim <- renderText({
    # str <- paste0("예측값 : ",
    #               round(reactPredValOpt(),attr(DFSource[,which(attr(DFSource,"names")=="predVal")],"digit")) )
    # str
    predVal <- req(reactPredValOpt())
    strPred <- predVal %>% renderStrPred()
    strPred
    
  })
  
  output$PredResultFailOptim <- renderText({
    predVal <- req(reactPredValOpt())
    
    strFailRatio <- renderStrFailRatio(predVal)
    
    strFailRatio

  })
  

  # 범용 리포트
  # observeEvent(input$renderReportCommonSample, {
  #   dfReportCommon <<- curSampleExplore
  #   fromReportCommon <<- "_sample"
  #   showModal(ModalActionButtonsReportCommon())
  # })
  ### 범용 리포트 처리를 위한 나머지 이벤트 핸들러는 sourcingMain과 같이 사용함 ###

  # Modal에서 선택된  범주형 변수의 레벨을 selCatDomainExplore에 저장  
  observe ({
    # input$selCatExplore1
    updateCatVarOptimize(input, output, session)
  })
  



#     
# 
# 
# 
#     # # 샘플링 결과 출력
#     # output$curSampleExploreResult <- renderPrint({
#     #     reactCurSampleExplore()
#     # 
#     #     unique(curSampleExplore$GangJong)
#     # 
#     #     
#     # })
# 
# 
# #    DualGraph 탭의 selectInput 위젯의 목록을 update
#     observeEvent(reactCurSampleExplore(),{
#         # if(exists("curSampleExplore")) {
#         #     curSampleExploreDWTTGangJong <<- unique(curSampleExplore[,"GangJong"])
#         # }
# 
#         updateSelectInputExploreDualGraphA(input, output, session)
# 
#     })
# 
# 
# 
# 
# 
# 



# 
# 
#     observeEvent(input$renderReportSampling, {
#         RenderReportSampling(input, output, session)
#     })
#     
#     observeEvent(input$calcMeanDesign, {
#         df <- calcTransVar(curSampleExplore)
#         for(i in 1:NCOL(curDesignDF)) {
#             varName <- names(curDesignDF)[i]
#             print(paste0("calcmeanDesign :  varName = ", varName))
#             if(is.numeric(curDesignDF[1,varName])) {
#                 curDesignDF[1,i] <<- mean(df[,varName], na.rm=TRUE)
#             } else {
#                 curDesignDF[1,i] <<- df[1,varName]
#             }
# 
#         }
#         
#         updateDesignUI(input, output, session)
#         
#     })
#     
}

meas_distance <- function(curOptValue, data) {
  tempoDF <- data
  tempoDF[1,curOptVar] <- curOptValue
  tempoDF <- add_predictions(tempoDF, curSelModel, var="predVal" )
  diff <- taregtVarValue - tempoDF[1,"predVal"]
  sqrt(mean(diff^2))
}
