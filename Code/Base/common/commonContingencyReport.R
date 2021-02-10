contingencyList <- list(x=NA, y=NA, pValueVector=NA)

ModalCommonContingency <- function(failed = FALSE) {
  labelStr <- "범주형 변수 독립성 검정 리포트 옵션"
  modalDialog(
    title=labelStr,
    tags$p("향후 버튼이 추가될 수 있습니다."),
    actionButton("contingencyY", label="target 번주형 변수 선정"),
    actionButton("contingencyX", label="X 범주형 변수 선정"),
    # actionButton("MCP_size", label="size 변수 선정"),
    # actionButton("MCP_shape", label="shape 변수 선정"),
    tags$p(""),
    tags$hr(),
    tags$p(paste0(" target Y : ", str_c(contingencyList[["y"]], collapse=", "))),
    tags$p(paste0(" X : ", str_c(contingencyList[["x"]], collapse=", "))),



    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),


    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalCommonContingency", "OK")
    ),
    size="l"
  )
}

treatModalCommonContingency <- function(input, output, session) {

  observeEvent(input$contingencyY, {
    var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=2, maxNoLevels=10)
    curItemContingencyList <<- "y"

    choiceNames <- attr(dfReportCommon[,var[1]],"labelShort")
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    strExplain <- "수준이 2이상 10 이하인 변수만 선택됬습니다."
    showModal(ModalRadioButtons(choiceNames, var,"okContingency", "selModal", "ContingencyY", "교차표 y 변수 선정",strExplain))
  })

  observeEvent(input$contingencyX, {
    var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=2, maxNoLevels=10)
    curItemContingencyList <<- "x"
    
    choiceNames <- attr(dfReportCommon[,var[1]],"labelShort")
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    strExplain <- "수준이 2이상 10 이하인 변수만 선택됬습니다."
    showModal(ModalCheckboxGroup(choiceNames, var, "okContingency", "selModal", "y 선정 대화창", label="y 선정", strExplain, selected="ALL.ALL"))
  })

  observeEvent(input$okContingency, {
    contingencyList[[curItemContingencyList]] <<- input$selModal
    removeModal()
    showModal(ModalCommonContingency())
  })

  observeEvent(input$okModalCommonContingency, {
    withProgress(message="기술통계 리포트 작성중", value=0, {
      incProgress(0.2)
      
      pValueVec <- vapply(contingencyList[["x"]], calcPValueFisherExact,contingencyList[["y"]],dfReportCommon, FUN.VALUE=numeric(1))
      pValueVec <- pValueVec[order(pValueVec)]
      contingencyList[["pValueVector"]] <<- pValueVec
      contingencyList[["x"]] <<- names(pValueVec)
      
      
      outputFiles <- ""
      params <- list(df=dfReportCommon, contingencyList=contingencyList )
      outputFileName <- paste0("commonContingencyReport", fromReportCommon, "_","general", ".html")
      outputFileFinalName <- paste0("교차표 독립성 검정 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                    "-",contingencyList[["y"]],"-",dim(dfReportCommon)[[1]], ".html")
      outputFiles <- paste0(outputFiles, " ", outputFileFinalName)
      options(warn=-1)
      rmarkdown::render(pathFileRmdCommonContingency, output_file = outputFileName,
                        output_dir = pathHTMLReport,
                        params = params,
                        envir = new.env(parent = globalenv()), encoding="UTF-8")
      file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
      incProgress(0.8)

    }) #withProgress(message="리포트 작성중", value=0, {
    strAlert <- paste0(pathHTMLReport,"에 ", outputFiles,"이 저장되었습니다.")
    alert(strAlert)
    removeModal()
    
    
    
  })
  
}