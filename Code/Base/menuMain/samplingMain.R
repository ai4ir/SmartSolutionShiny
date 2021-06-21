samplingMainUI <- function() {

    tabsetPanel( type="tabs",id="samplingMain", 

                 tabPanel("샘플링 GUI",  value="samplingGUI",
                          fluidPage(
                              fluidRow(
                                  column(1, actionButton("fileDomain", "파일") ),
                                  column(1, actionButton("renderReportSampling", "두께 리포트") ),
                                  column(1, actionButton("renderReportCommonSample", "탐색 리포트") ),
                                  column(1, actionButton("domainFromSource", "소스 도메인")),
                                  column(1, actionButton("clearSampleCond", "샘플 조건 제거")),
                                  column(1, tags$h4(" ::::::: ")),
                                  column(1, actionButton("initCurSampleExplore", "나데이타 소환")),
                                  column(1, actionButton("validateDF", "유효 데이터")),
                                  column(1, actionButton("expandCurSampleExplore", "데이터 확장")),
                                  column(1, actionButton("areaTreat", "영역 처리")),
                                  column(1, actionButton("remainVar", "변수 선정")),

                                  column(1, actionButton("calcMeanDesign", "설계용 평균"))
                              ),
                              tags$hr(),
                              fluidRow(
                                  column(3,
                                         dateRangeInput('dateRange1',
                                                        label = 'Date range input: yyyy-mm-dd',
                                                        # start = Sys.Date() - 2, end = Sys.Date() + 2
                                                        start = ymd("2000-01-01"), end = Sys.Date() + 2
                                         ),
                                         renderDomainUI_2()
                                         ),
                                         
 
                                  column(9,
                                         includeCSS("Base/www/html/DomainExploreTable.css"),
                                         column(12, htmlTemplate("Base/www/html/DomainExploreTable.html")))

                              )
                          )
                 ),
                 tabPanel("scatterGirafe1", value="scatterGirafe1",
                          fluidRow(
                            column(1,), # actionButton("fileDomain", "파일") ),
                            column(1,), # actionButton("renderReportSampling", "리포트") ),
                            column(1, actionButton("renderReportCommonSample2", "탐색 리포트") ),
                            column(1,), #actionButton("domainFromSource", "소스 도메인")),
                            column(1,) ,# actionButton("clearSampleCond", "샘플 조건 제거")),
                            column(1, tags$h4(" ::::::: ")),
                            column(1,), # actionButton("initCurSampleExplore", "나데이타 소환")),
                            column(1,), # actionButton("expandCurSampleExplore", "데이터 확장")),
                            column(2, actionButton("areaTreat", "영역 처리")),
                            column(1,), # actionButton("remainVar", "변수 선정")),
                            column(1,) # actionButton("calcMeanDesign", "설계용 평균"))
                          ),
                          tags$hr(),
                          scatterGirafe1TabModuleUI("scatterGirafe1")
                 )

                 
    )

}


samplingMain <- function(input, output, session) {
  
  callModule(scatterGirafe1TabModule,"scatterGirafe1")

  observeEvent(reactDFSource(),{

    # 샘플링 조건 탭의 입력 위젯 처리
    if(input$sourceDataOrModel == "EXCEL") renderAttrSamplingUI()
    revampDomainUI(input, output, session)
    revampDomainUI_2(input, output, session)

  })
  
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
    DFSourceRmd <- DFSourceRmd[DFSourceRmd$pathFile!="",]  ### for SRW
    noData <- NROW(DFSourceRmd)
    if(noData==0) {
      alert(paste0("두께 프로파일이 하나도 없습니다.  Sourcing부터 다시 하십시요"))  
      
    } else {
      alert(paste0("남기기가 완료되었습니다.  ",noData,"개의 두께 프로파일이 있습니다."))  
    }
    params <- list(DFSource=DFSourceRmd, pathHTMLReport=pathHTMLReport)
    renderReportCheckboxGroup(input, output, session, params, outputFileNamesSamplingReport, outputFileFinalNamesSamplingReport,
                              pathFileRmdSamplingReport, pathHTMLReport)
  })
  
  # samplingGUI Tab에서의 범용 리포트
  observeEvent(input$renderReportCommonSample, {
    dfReportCommon <<- curSampleExplore
    # fromReportCommon <<- "_sample"
    showModal(ModalActionButtonsReportCommon())
    # hideButton <- c("renderReportCommonPlotViolinOneY")
    hideButton <- NA
    for(i in seq_along(hideButton)) {
      hide(hideButton[i])
    }

  })
  # scatterGirafe1 Tab에서의 범용 리포트
  observeEvent(input$renderReportCommonSample2, {
    dfReportCommon <<- curSampleExplore
    # fromReportCommon <<- "_sample"
    showModal(ModalActionButtonsReportCommon())
  })
  ### 범용 리포트 처리를 위한 나머지 이벤트 핸들러는 sourcingMain과 같이 사용함 ###

  # Modal에서 선택된  범주형 변수의 레벨을 selCatDomainExplore에 저장  
  observe ({
    # input$selCatExplore1
    updateCatVarSample(input, output, session)
  })
  

  observeEvent(input$initCurSampleExplore, {
      curSampleExplore <<- DFSource
      noData <- NROW(curSampleExplore)
      alert(paste0("전체 데이타가 재소환되었습니다.  ",noData,"개의 데이타가 있습니다."))
      selCatDomainExplore <<- 
        resetLevelsCatVar(curSampleExplore, selCatDomainExplore, catVarWithoutModal)
      revampDomainUI_2(input,output,session)
  })
  
  observeEvent(input$validateDF, {
    
    removeVar <- NA
    var <- setdiff(colnames(curSampleExplore),removeVar) 
    var <- c("ALL.ALL",var)
    choiceNames <- "All.ALL"
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
    }
    
    showModal(ModalCheckboxGroup(title="유효성 검사 Var 선정 대화창", modalCheckboxID="selValidateVarModal",
                                 label="유효성 검사 Var 선정",
                                 choiceNames=choiceNames, choiceValues=var,
                                 selected=NULL,
                                 modalOKButtonID="okValidateVarModal"))

  })
  
  observeEvent(input$okValidateVarModal, {
    varX <- input$selValidateVarModal 
    
    if(varX[1]=="ALL.ALL") {
      removeVar <- NA
      varX <- setdiff(colnames(curSampleExplore),removeVar) 
      # varX <<- setdiff(selVar,varX[2:length(varX)])

    } 
    
    curSampleExplore <<- validateDF(curSampleExplore, varX) 
    noData <- NROW(curSampleExplore)
    alert(paste0("유효 데이터만 남았습니다.  ",noData,"개의 데이타가 있습니다."))
    
    removeModal()
    
  })
  
  
  
  observeEvent(input$expandCurSampleExplore, {
    # catVar <- c(catVarWithoutModal, catVarWithModal)
    catVar <- c(catVarWithoutModal)
    showModal(ModalRadioButtons(choiceNames=catVar, choiceValues=catVar,
                                okButtonName="okExpandVar", labelStr=" 확장 변수 선정",
                                strExplain="확장 변수를 선정하세요",
                                modalRadioButtonsID="selModalExpandVar",
                                failed = FALSE))
  })
  
  
  observeEvent(input$okExpandVar, { 
    noDataOld <- NROW(curSampleExplore)
    curSampleExplore <<- 
      DFSource[DFSource[,input$selModalExpandVar] %in% selCatDomainExplore[[input$selModalExpandVar]],]
    noDataNew <- NROW(curSampleExplore)
    alert(paste0(noDataOld, "개에서 ",noDataNew,"개로 데이터가 확장되었습니다."))  
    removeModal()
    replyButtonBisectArea(input, output, session)
    
  })

  observeEvent(input$areaTreat, {
    showModal(ModalActionButtonsAreaTreat())
  })
  observeEvent(input$resetReq, {
      getDomainExplore(input, output, session)
      MinReqExplore <<- MinDomainExplore
      MaxReqExplore <<- MaxDomainExplore
      alert("영역선 변경 완료되었습니다.")
      removeModal()
      

  })
# 
# 
#     # getDomainExploreEx <- reactive({
#     #     getDomainExplore(input, output, session)
#     #     selectBool <- rep(TRUE, nrow(DFSourceExplore))
#     #
#     #
#     #     # 연속형 변수
#     #     for( varName in names(MinDomainExplore)) {
#     #         print(paste0("trigerSample - varName ", varName))
#     #         if(is.numeric(DFSourceExplore[,varName])) {
#     #             value <- DFSourceExplore[,varName]
#     #             min <- as.vector(MinDomainExplore[varName])
#     #             max <- as.vector(MaxDomainExplore[varName])
#     #             selectBool <- updateSelectBool(value=value, min=min, max=max, initBool=selectBool)
#     #         }
#     #     }
#     #
#     #     # 범주형 변수
#     #
#     #     for( varName in names(selCatDomainExplore)) {
#     #         value <- DFSourceExplore[,varName]
#     #         selectedCat <- selCatDomainExplore[[varName]]
#     #         selectBool <- updateSelectBoolCat(value=value, selectedCat=selectedCat, initBool=selectBool)
#     #     }
#     #
#     #     return(selectBool)
#     #
#     # })
# 

    observeEvent(input$saveArea, {
      replyButtonSaveArea(input, output, session)
      selCatDomainExplore <<- 
        resetLevelsCatVar(curSampleExplore, selCatDomainExplore, catVarWithoutModal)
      removeModal()
      revampDomainUI_2(input,output,session)

    })
      
    observeEvent(input$deleteArea, {
      replyButtonDeleteArea(input, output, session)
      selCatDomainExplore <<- 
        resetLevelsCatVar(curSampleExplore, selCatDomainExplore, catVarWithoutModal)
      revampDomainUI_2(input,output,session)
      removeModal()
    })

    observeEvent(input$bisectArea, {
      replyButtonBisectArea(input, output, session)
      removeModal()
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
    observeEvent(input$fileDomain, {
        getDomainExplore(input, output, session)
        showModal(ModalFileDomain(pathDir=pathNameFileDomain))
    })
    observeEvent(input$renderModalFileDomain2, {
        showModal(ModalFileDomain2(selFileName=input$nameFileDomain, failed=FALSE))
    })

    observeEvent(input$saveModalFileDomain, {
        getDomainExplore(input, output, session)
        # namePathFile <- paste0(pathNameFileDomain,"/", input$selNameFileDomain)
        namePathFile <- file.path(pathNameFileDomain,input$selNameFileDomain)
        save(MinDomainExplore, MaxDomainExplore, file=namePathFile)
        removeModal()
        # curPathNameFileDomain <<- namePathFile
        # save(curPathNameFileDomain, file="initEnv.Rdata")
    })

    observeEvent(input$loadModalFileDomain, {

        namePathFile <- paste0(pathNameFileDomain,"/", input$selNameFileDomain)
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
        updateDomainUI(input,output, session)
        removeModal()


    })
    
    observeEvent(input$clearSampleCond, {
      for(i in 1:length(MaxDomainExplore)) {
        MaxDomainExplore[i] <<- NA
        MinDomainExplore[i] <<- NA
      }
      updateDomainUI(input,output, session)
    })

    observeEvent(input$domainFromSource, {
        updateDomainUIFromSourceDomain(input,output, session)

    })
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


