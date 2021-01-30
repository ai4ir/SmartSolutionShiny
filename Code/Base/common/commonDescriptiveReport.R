# aesList <- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL)

ModalCommonDescriptive <- function(failed = FALSE) {
  labelStr <- "기술통계 범용 리포트 옵션"
  modalDialog(
    title=labelStr,
    # actionButton("MCP_y", label="Y 변수 선정"),
    # actionButton("MCP_color", label="color 변수 선정"),
    # actionButton("MCP_size", label="size 변수 선정"),
    # actionButton("MCP_shape", label="shape 변수 선정"),
    # tags$p(""),
    # tags$hr(),
    # tags$p(paste0(" Y     : ", aesList[["y"]])),
    # tags$p(paste0(" color : ", aesList[["color"]])),
    # tags$p(paste0(" size  : ", aesList[["size"]])),
    tags$p("향후 옵션이 추가될 수 있습니다."),



    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),


    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalCommonDescriptive", "OK")
    ),
    size="l"
  )
}

treatModalCommonDescriptive <- function(input, output, session) {
  
  # observeEvent(input$MCP_y, {
  #   numVar <- extractNumVarName(dfReportCommon)
  #   curAes <<- "y"
  #   showModal(ModalRadioButtons(numVar, numVar, "okMCP", "Y 변수",  failed = FALSE))
  # })
  # 
  # observeEvent(input$MCP_color, {
  #   var <- unique(colnames(dfReportCommon))
  #   curAes <<- "color"
  #   showModal(ModalRadioButtons(var, var, "okMCP", "color 변수",  failed = FALSE))
  # })
  # 
  # observeEvent(input$MCP_size, {
  #   var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=7) 
  #   strExplain <- "수준이 2이상 6 이하인 변수만 선택됬습니다."
  #   curAes <<- "size"
  #   showModal(ModalRadioButtons(var, var, "okMCP", "size 변수",  strExplain, failed = FALSE))
  # })
  # 
  # observeEvent(input$MCP_shape, {
  #   var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=7) 
  #   strExplain <- "수준이 2이상 6 이하인 변수만 선택됬습니다."
  #   curAes <<- "shape"
  #   showModal(ModalRadioButtons(var, var, "okMCP", "shape 변수",  failed = FALSE))
  # })
  # 
  # observeEvent(input$okMCP, {
  #   aesList[[curAes]] <<- input$ModalRadioButtons
  #   removeModal()
  #   showModal(ModalCommonPlot())
  # })
  # 
  observeEvent(input$okModalCommonDescriptive, {
    withProgress(message="기술통계 리포트 작성중", value=0, {
      incProgress(0.2)
      outputFiles <- ""
      params <- list(df=dfReportCommon)
      outputFileName <- paste0("commonDescriptiveReport", fromReportCommon, "_","general", ".html")
      outputFileFinalName <- paste0("범용 기술통계 리포트", fromReportCommon, "_",chosenDFSourceFile, ".html")
      outputFiles <- paste0(outputFiles, " ", outputFileFinalName)
      rmarkdown::render(pathFileRmdCommonDescriptive, output_file = outputFileName,
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