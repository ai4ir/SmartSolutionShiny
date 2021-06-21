
# numVar <- extractNumVarName(curSampleExplore)
# numVar <- c("ALL.ALL",numVar)
# choiceNames <- "All.ALL"
# for(i in 2:length(numVar)) {
#   choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
# }
# strExplain <- "nuneric 변수만 제공됬습니다."
# showModal(ModalCheckboxGroup(choiceNames, numVar, "okMCP", "selModal", "y 선정 대화창", label="y 선정", strExplain, selected="ALL.ALL"))

ModalCheckboxGroup <- function(choiceNames, choiceValues, modalOKButtonID="okMCP", modalCheckboxID="selModal",  
                               title="대화창", label="label", 
                               strExplain="향후 설명이 추가될 예정입니다.",
                               selected=NULL,
                               failed = FALSE) {

  modalDialog(
    title=title,
    
    tags$p(strExplain),

    checkboxGroupInput(modalCheckboxID,label, choiceNames = choiceNames, choiceValues=choiceValues, selected=selected ),

    # print(paste0("ModalCheckboxCatVarExplore - selVar :", selVar)),

    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),


    footer = tagList(
      modalButton("취소"),
      actionButton(modalOKButtonID, "OK")
    ),
    size="l"
  )
}

renderReportCheckboxGroup <- function(input, output, session, params, outputFileNamesReport,outputFileFinalNamesReport,
                                      pathFileRmdReport, pathHTMLReport) {
  removeModal() 
  selReport <- input$ModalCheckboxGroup
  print(paste0("sel report of ModalCheckboxGroup : ", selReport))

  withProgress(message="리포트 작성중", value=0, {
    incProgress(0.1)
    outputFiles <- ""
    
    for(i in seq_along(selReport)) {
      pos <- which(outputFileNamesReport==selReport[i])
      outputFileName <- outputFileNamesReport[pos]
      outputFileFinalName <- outputFileFinalNamesReport[pos]
      outputFiles <- paste0(outputFiles, " ", outputFileFinalName)

      ### 수동 render ###  Sourcing
      ### 수동 render ### 
      # pos <- 1
      # outputFileName <- outputFileNamesSourcingReport[pos]
      # outputFileFinalName <- outputFileFinalNamesSourcingingReport[pos]
      # pathFileRmdReport <- pathFileRmdSourcingReport 
      # DFSourceRmd <- DFSource %>% select(-c(sampleCode, bHOT, clusterGr))
      # params <- list(DFSource=DFSourceRmd, pathHTMLReport=pathHTMLReport)
      ### 수동 render ###  Sampling
      ### 수동 render ###
      # pos <- 1
      # outputFileName <- outputFileNamesSamplingReport[pos]
      # outputFileFinalName <- outputFileFinalNamesSamplingReport[pos]
      # pathFileRmdReport <- pathFileRmdSamplingReport 
      # DFSourceRmd <- curSampleExplore %>% select(-c(sampleCode, bHOT, clusterGr))
      # params <- list(DFSource=DFSourceRmd, pathHTMLReport=pathHTMLReport)
      ### 수동 render ###
      ### 수동 render ###
      options(warn=-1)
      rmarkdown::render(pathFileRmdReport[pos], output_file = outputFileName,
                        output_dir = pathHTMLReport,
                        params = params,
                        envir = new.env(parent = globalenv()), encoding="UTF-8")
      file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
      incProgress(i/length(selReport))
    } #for(i in 1:length(selReport)) {
    strAlert <- paste0(pathHTMLReport,"에 ", outputFiles,"이 저장되었습니다.")
    alert(strAlert)
    
  }) #withProgress(message="리포트 작성중", value=0, {
  
}

ModalRadioButtons <- function(choiceNames, choiceValues, okButtonName="okMCP", modalRadioButtonsID="selModal",
                              title="대화창 제목",   label="라디오버튼 라벨",
                              strExplain="향후 설명이 추가될 예정입니다.",selected=NULL,
                              failed = FALSE) {
  modalDialog(
    title=title,
    
    tags$p(strExplain),
    
    radioButtons(modalRadioButtonsID,label,
                 choiceNames = choiceNames,
                 choiceValues = choiceValues, 
                 selected=selected
    ),
    
    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(okButtonName, "Next")
    ),
    size="l"
  )
}

ModalActionButtonsReportCommon <- function(failed = FALSE) {
  modalDialog(
    title="탐색 리포트 종류 선택",
    
    tags$p("리포트 종류 버튼을 누르세요"),

    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("renderReportCommonPlotScatterOneY", "Scatter One Y"),
      actionButton("renderReportCommonPlotScatterOneX", "Scatter One X"),
      actionButton("renderReportCommonPlotViolinOneY", "ViolinOneY"),
      actionButton("renderReportCommonPlotViolinOneX", "ViolinOneX"),
      actionButton("renderReportCommonPlotHistogram", "Histogram"),
      actionButton("renderReportCommonContingency", "교차표 분석"),
      actionButton("renderReportCommonCollectDiag", "수집진단"),
      actionButton("renderReportCommonDescriptive", "기술통계")
    ),
    size="l"
    
  )
  

}
