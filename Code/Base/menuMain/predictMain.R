predictMainUI <- function() {
  
    tabsetPanel( type="tabs", id="predict",

                 tabPanel("예측", value="predictTab1",
                          fluidPage(
                              fluidRow(
                                  column(1, actionButton("fileDesignPredict", "파일")),
                                  column(1, actionButton("doPredict", "예측값 갱신")),
                                  column(10, h3(textOutput("PredResult")))
                              ),
                              fluidRow(
                                column(2),
                                column(10, h3(textOutput("PredResultFail")))
                              ),
                              tags$hr(),
                              fluidRow(
                                  column(3,
                                         # dateRangeInput('dateRange1',
                                         #                label = 'Date range input: yyyy-mm-dd',
                                         #                start = Sys.Date() - 2, end = Sys.Date() + 2
                                         # ),
                                         renderDesignUI_2()
                                         ),
                                         
 
                                  column(9,
                                         includeCSS("Base/www/html/DesignTable.css"),
                                         column(12, htmlTemplate("Base/www/html/DesignTable.html")))

                              )
                          )
                 ),
                 tabPanel(title="그래프1", value="predPlotSeed1",
                          predPlotSeed1TabModuleUI("predPlotSeed1")
                 ),
                 tabPanel(title=uiOutput("predPlot1"), value="predPlot1",
                          predPlot1TabModuleUI("predPlot1")
                 ),
                 tabPanel(title=uiOutput("predPlot2"), value="predPlot2",
                          predPlot2TabModuleUI("predPlot2")
                 ),
                 tabPanel(title=uiOutput("predPlot3"), value="predPlot3",
                          predPlot2TabModuleUI("predPlot3")
                 )


                 
    )

}


predictMain <- function(input, output, session) {
  
  observe({
    switch(input$predict,
      predictTab1 = {

      },
      predPlot1 = {

      },
      predPlotSeed1 = {
        # updateTextInput(session, inputId="predPlotSeed1-VF2Level", value="0.01 0.02 0.03")
        aesList[["y"]][1] <<- "predVal"
        graphOption[["yAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "label")

      }
    )
  })

  
  callModule(predPlot1TabModule,"predPlot1")
  callModule(predPlot2TabModule,"predPlot2")
  callModule(predPlot3TabModule,"predPlot3")
  callModule(predPlotSeed1TabModule,"predPlotSeed1")

  
  observeEvent(input$predict, {
    curTabPredict <<- input$predict

  })
  
  observeEvent(reactDFSource(),{
    
    # Design 조건 탭의 입력 위젯 처리
    revampDesignUI(input, output, session)
    revampDesignUI_2(input, output, session)

  })
  
  ### design 조건 파일 처리 ###
  ### design 조건 파일 처리 ###
  ### design 조건 파일 처리 ###
  observeEvent(input$fileDesignPredict, {
    getDesignPredict(input, output, session)
    showModal(ModalFileDesign(pathDir=pathNameFileDesign))
  })
  observeEvent(input$renderModalFileDesign2, {
    showModal(ModalFileDesign2(selFileName=input$nameFileDesign, failed=FALSE))
  })
  
  observeEvent(input$saveModalFileDesign, {
    getDesignPredict(input, output, session)
    namePathFile <- paste0(pathNameFileDesign,"/", input$selNameFileDesign)
    save(curDesignDF, file=namePathFile)
    removeModal()
    # curPathNameFileDomain <<- namePathFile
    # save(curPathNameFileDomain, file="initEnv.Rdata")
  })
  
  observeEvent(input$loadModalFileDesign, {
    
    namePathFile <- paste0(pathNameFileDesign,"/", input$selNameFileDesign)
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
    updateDesignUI(input,output, session)
    removeModal()
    
    
  })
  
### 예측 실행 ### 
### 예측 실행 ### 
### 예측 실행 ### 
  
  
  observeEvent(input$doPredict, {
    getDesignPredict(input, output, session)
    curDesignDF <<- curDesignDF %>% add_predictions(curSelModel, var="predVal")
    reactPredVal(curDesignDF[1,"predVal"])
  })
  
  output$PredResult <- renderText({
    # str <- paste0(stringList[["predict"]][["predictTab1"]][["pred"]], 
    #               round(reactPredVal(),digitList[["predict"]][["predictTab1"]][["pred"]]) )
    reactPredVal()
    strPred <- curDesignDF %>% renderStrPred()
    strPred
  })
  
  output$PredResultFail <- renderText({
    predVal <- req(reactPredVal())
    std <- 12
    
    modelY <- dfModelNest[["modelY"]][[1]]
    
    if(is.null(MinReqExplore[modelY])) {
      failPercentUnderReq <- 0.0
    } else {
      zMin <- (MinReqExplore[modelY] - predVal) / 12
      failPercentUnderReq <- round(100 * pnorm(zMin),2)
    }
    
    if(is.null(MaxReqExplore[modelY])) {
      failPercentOverReq <- 0.0
    } else {
      zMax <- (MaxReqExplore[modelY] - predVal) / 12
      failPercentOverReq <- round(100 * pnorm(zMax, lower.tail=FALSE),2)
    }
    percentInside <- round(100 - failPercentUnderReq - failPercentOverReq, 2)
    str <- paste0(failPercentUnderReq,"%, ", MinReqExplore[modelY], "MPa, ", 
                  percentInside, "%, ", MaxReqExplore[modelY], "MPa, ",
                  failPercentOverReq, "%"
    )
    # print(paste0("output$PredResultFailOptom - str :", str))
    str
  })
  
  # output$PredResultFail <- renderText({
  #   predVal <- req(reactPredVal())
  #   if(is.null(MinReqExplore)) {
  #     # return("불량률 계산 불가능 상황")
  #     hide("PredResultFail")
  #     return()
  #   } else {
  #     show("PredResultFail")
  #   }
  #   
  #   std <- 12
  #   modelY <- dfModelNest[["modelY"]][[1]]
  #   print(paste0("output$PredResultFail - predVal :", predVal))
  #   if(is.na(MinReqExplore[modelY])) {
  #     failPercentUnderReq <- 0.0
  #   } else {
  #     zMin <- (MinReqExplore[modelY] - predVal) / 12
  #     failPercentUnderReq <- round(100 * pnorm(zMin),2)
  #   }
  #   if(is.na(MaxReqExplore[modelY])) {
  #     failPercentOverReq <- 0.0
  #   } else {
  #     zMax <- (MaxReqExplore[modelY] - predVal) / 12
  #     failPercentOverReq <- round(100 * pnorm(zMax, lower.tail=FALSE),2)
  #   }
  #   percentInside <- round(100 - failPercentUnderReq - failPercentOverReq, 2)
  #   str <- paste0(failPercentUnderReq,"%, ", MinReqExplore[modelY], "MPa, ", 
  #                 percentInside, "%, ", MaxReqExplore[modelY], "MPa, ",
  #                 failPercentOverReq, "%"
  #                 )
  #   str
  # })
  
  observeEvent(input$renderReportSampling, {
    # showModal(ModalCheckboxGroup(outputFileNamesSamplingReport, outputFileNamesSamplingReport,
    #                              "okModalReportSampling"))
    showModal(ModalCheckboxGroup(title="리포트 선정 대화창", modalCheckboxID="ModalCheckboxGroup", label="리포트 선정",
                                 choiceNames=outputFileFinalNamesSamplingReport, choiceValues=outputFileNamesSamplingReport,
                                 modalOKButtonID="okModalReportSampling"))
  })
  
  observeEvent(input$okModalReportSampling, {
    # params <- list(DFSource=curSampleExplore, MaxDomainExplore=MaxDomainExplore, MinDomainExplore=MinDomainExplore)
    DFSourceRmd <- curSampleExplore %>% select(-c(sampleCode, bHOT, clusterGr))
    params <- list(DFSource=DFSourceRmd, pathHTMLReport=pathHTMLReport)
    renderReportCheckboxGroup(input, output, session, params, outputFileNamesSamplingReport, outputFileFinalNamesSamplingReport,
                              pathFileRmdSamplingReport, pathHTMLReport)
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
    updateCatVarPredict(input, output, session)
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

  observeEvent(input$remainVar, {
    choiceNames <- colnames(curSampleExplore)
    choiceValues <- choiceNames
    # showModal(ModalCheckboxGroup(choiceNames, choiceValues,
    #                              "okModalRemainVar"))
    showModal(ModalCheckboxGroup(title="변수 선정 대화창", modalCheckboxID="ModalCheckboxGroup", label="변수 선정",
                                 choiceNames=choiceNames, choiceValues=choiceValues,
                                 modalOKButtonID="okModalRemainVar"))
  })
  
  observeEvent(input$okModalRemainVar, {
    curSampleExplore <<- curSampleExplore[,input$ModalCheckboxGroup]
    noVar <- NCOL(curSampleExplore) 
    alert(paste0("변수 선정이 완료되었습니다.  ",noVar,"개의 변수가 있습니다."))  
  })
}


