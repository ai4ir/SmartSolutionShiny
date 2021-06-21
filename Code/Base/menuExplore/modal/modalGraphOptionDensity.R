# graphOption <- list(minX=0, maxX=100, minY=0, maxY=100, 
#                     axisTitleSize=40, axisTextSize=30, pointSize=8, colorLegendPointSize=8, 
#                     legendTitleSize=40, legendTextSize=25, xAxisTitle="xTitle", yAxisTitle="yTitle")

ModalGraphOptionDensity <- function(failed = FALSE) {
  labelStr <- "그래프 옵션"
  # failed <- FALSE
  
  modalDialog(
    title=labelStr,
    textInput("xAxisTitle", label = h4("X축 제목"), value = aesList[["x"]][1]),
    # textInput("yAxisTitle", label = h4("Y축 제목"), value = aesList[["y"]][1]),
    numericInput("axisTitleSize", label = h4("축 제목 크기(권장:30~50)"), 
                 value = graphOption[["axisTitleSize"]][1]),
    numericInput("axisTextSize", label = h4("축 라벨 크기(권장:20~40)"), 
                 value = graphOption[["axisTextSize"]][1]),

    # tags$p(""),
    # tags$hr(),

    
    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalGraphOptionDensity", "OK")
    ),
    size="l"
  )
}

treatModalGraphOptionDensity <- function(input, output, session) {


  observeEvent(input$okModalGraphOptionDensity, {
    graphOption[["xAxisTitle"]][1] <<- input$xAxisTitle
    # graphOption[["yAxisTitle"]][1] <<- input$yAxisTitle
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
  

  
}