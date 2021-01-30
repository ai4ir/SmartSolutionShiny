# graphOption <- list(minX=0, maxX=100, minY=0, maxY=100, 
#                     axisTitleSize=40, axisTextSize=30, pointSize=8, colorLegendPointSize=8, 
#                     legendTitleSize=40, legendTextSize=25, xAxisTitle="xTitle", yAxisTitle="yTitle",
#                     fitOption="NoFit")

aesList <- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
                axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
                legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)

graphOption <- list(minX=0, maxX=100, minY=0, maxY=100, 
                    xAxisTitle="xTitle", yAxisTitle="yTitle")

ModalGraphOptionGlobal <- function(failed = FALSE) {
  labelStr <- "글로벌 옵션"
  # failed <- FALSE
  
  modalDialog(
    title=labelStr,
    radioButtons("fitOption","Fit Option", choiceNames = c("NoFit", "Fit1", "Fit2"), choiceValues=c("NoFit", "Fit1", "Fit2"), 
                       selected=aesList[["fitOption"]]),
    tags$p(""),
    tags$p(""),
    tags$hr(),
    numericInput("pointSize", label = h4("기호 크기"),
                 value =aesList[["pointSize"]][1]),

    numericInput("axisTitleSize", label = h4("축 제목 크기(권장:30~50)"), 
                 value = aesList[["axisTitleSize"]][1]),
    numericInput("axisTextSize", label = h4("축 라벨 크기(권장:20~40)"), 
                 value = aesList[["axisTextSize"]][1]),
    # choiceNames <- c( "No Fitting", "Fit: 1차식","Fit: 2차식"),
    # choiceValues <- c("NoFit", "Fit1", "Fit2" ),
    # showModal(ModalRadioButtons(choiceNames, choiceValues, "okScatter1TabFit", "bFit 변수", failed = FALSE)),



    
    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalGraphOptionGlobal", "OK")
    ),
    size="l"
  )
}

treatModalGraphOptionGlobal <- function(input, output, session) {


  observeEvent(input$okModalGraphOptionGlobal, {
    aesList[["fitOption"]][1] <<- input$fitOption
    aesList[["pointSize"]][1] <<- input$pointSize
    aesList[["axisTitleSize"]][1] <<- input$axisTitleSize
    aesList[["axisTextSize"]][1] <<- input$axisTextSize

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
  
  observeEvent(input$okScatter1TabFit, {
    graphOption[["fitOption"]][1] <<- input$selModal
  })
  
}