

ModalGraphOptionGeomLine <- function(failed = FALSE) {
  labelStr <- "그래프 옵션"
  # failed <- FALSE
  
  modalDialog(
    title=labelStr,
    textInput("xAxisTitle", label = h4("X축 제목"), value = attr(curSampleExplore[,aesList[["x"]][1]], "label")),
    textInput("yAxisTitle", label = h4("Y축 제목"), value = attr(curSampleExplore[,aesList[["y"]][1]], "label")),
    numericInput("minX", label = h4("X축 최소값"),
                 value = min(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)),
    numericInput("minY", label = h4("Y축 최소값"),
                 value = min(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)),
    numericInput("maxX", label = h4("X축 최대값"),
                 value = max(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)),
    numericInput("maxY", label = h4("Y축 최대값"),
                 value = max(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)),
    numericInput("axisTitleSize", label = h4("축 제목 크기(권장:30~50)"), 
                 value = graphOption[["axisTitleSize"]][1]),
    numericInput("axisTextSize", label = h4("축 라벨 크기(권장:20~40)"), 
                 value = graphOption[["axisTextSize"]][1]),
    # choiceNames <- c( "No Fitting", "Fit: 1차식","Fit: 2차식"),
    # choiceValues <- c("NoFit", "Fit1", "Fit2" ),
    # showModal(ModalRadioButtons(choiceNames, choiceValues, "okScatter1TabFit", "bFit 변수", failed = FALSE)),

    # tags$p(""),
    # tags$hr(),

    
    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalGraphOptionGeomLine", "OK")
    ),
    size="l"
  )
}

treatModalGraphOptionGeomLine <- function(input, output, session) {


  observeEvent(input$okModalGraphOptionGeomLine, {
    graphOption[["xAxisTitle"]][1] <<- input$xAxisTitle
    graphOption[["yAxisTitle"]][1] <<- input$yAxisTitle
    graphOption[["minX"]][1] <<- input$minX
    graphOption[["minY"]][1] <<- input$minY
    graphOption[["maxX"]][1] <<- input$maxX
    graphOption[["maxY"]][1] <<- input$maxY
    graphOption[["axisTitleSize"]][1] <<- input$axisTitleSize
    graphOption[["axisTextSize"]][1] <<- input$axisTextSize

    removeModal()
    # switch(triggerMCP,
    #        module = {},
    #        commonPlot = {
    #          showModal(ModalCommonPlot())
    #        },
    #        modelResult = {
    #          showModal(ModalModeling())
    #        },
    #        {alert("trigerMCP가 비었습니다.")}
    # )

    
  })
  
  # observeEvent(input$okScatter1TabFit, {
  #   graphOption[["fitOption"]][1] <<- input$selModal
  # })
  
}