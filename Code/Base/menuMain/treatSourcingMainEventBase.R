treatSourcingMainEventBase <- function(input, output, session) {
  # 범용 리포트
  observeEvent(input$renderReportCommonSource, {
    dfReportCommon <<- DFSource
    fromReportCommon <<- "_source"
    showModal(ModalActionButtonsReportCommon())
  })
  
  
  observeEvent(input$renderReportCommonPlotScatterOneY, {
    pathFileRmdCommonPlot <<- "Base/common/Rmd/commonPlotReportScatterOneY.Rmd"
    curCommonPlot <<- "ScatterOneY"
    fileNameSuffix <<- "_Scatter_OneY_"
    aesList[["y"]] <<- NA
    triggerMCP <<- "commonPlot"
    showModal(ModalCommonPlot())
  })
  
  observeEvent(input$renderReportCommonPlotScatterOneX, {
    pathFileRmdCommonPlot <<- "Base/common/Rmd/commonPlotReportScatterOneX.Rmd"
    curCommonPlot <<- "ScatterOneX"
    fileNameSuffix <<- "_Scatter_OneX_"
    aesList[["x"]] <<- NA
    triggerMCP <<- "commonPlot"
    showModal(ModalCommonPlot())
  })
  
  observeEvent(input$renderReportCommonPlotViolinOneY, {
    pathFileRmdCommonPlot <<- "Base/common/Rmd/commonPlotReportViolinOneY.Rmd"
    curCommonPlot <<- "ViolinOneY"
    fileNameSuffix <<- "_ViolinOneY_"
    triggerMCP <<- "commonPlot_Violin"
    # aesListScatter <<- aesList
    showModal(ModalCommonPlot())
    hideButton=c( "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id", "MCP_fitOption")
    for(i in seq_along(hideButton)) {
      hide(hideButton[i])
    }
  })
  
  observeEvent(input$renderReportCommonPlotViolinOneX, {
    pathFileRmdCommonPlot <<- "Base/common/Rmd/commonPlotReportViolinOneX.Rmd"
    curCommonPlot <<- "ViolinOneX"
    fileNameSuffix <<- "_ViolinOneX_"
    triggerMCP <<- "commonPlot_Violin"
    # aesListScatter <<- aesList
    showModal(ModalCommonPlot())
    hideButton=c( "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id", "MCP_fitOption")
    for(i in seq_along(hideButton)) {
      hide(hideButton[i])
    }
  })
  
  observeEvent(input$renderReportCommonPlotHistogram, {
    pathFileRmdCommonPlot <<- "Base/common/Rmd/commonPlotReportHistogram.Rmd"
    curCommonPlot <<- "Histogram"
    fileNameSuffix <<- "_Histogram_"
    aesList[["y"]] <<- NA
    triggerMCP <<- "commonPlot_Histogram"
    showModal(ModalCommonPlot())
    hideButton=c("MCP_y","MCP_color", "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id",
                 "MCP_fitOption")
    for(i in seq_along(hideButton)) {
      hide(hideButton[i])
    }
    
  })
  
  observe({
    treatModalCommonPlot(input, output, session)
  })
  
  observeEvent(input$renderReportCommonContingency, {
    pathFileRmdCommonContingency <<- "Base/common/Rmd/commonContingencyReport.Rmd"
    showModal(ModalCommonContingency())
  })
  
  observe({
    treatModalCommonContingency(input, output, session)
  })
  
  observeEvent(input$renderReportCommonDescriptive, {
    pathFileRmdCommonDescriptive <<- "Base/common/Rmd/commonDescriptiveReport.Rmd"
    showModal(ModalCommonDescriptive())
  })
  
  observe({
    treatModalCommonDescriptive(input, output, session)
  })
  
  #변수 처리
  observeEvent(input$treatVar, {
    showModal(ModalTreatVar())
  })
  
  observe({
    treatModalTreatVar(input, output, session)
  })
  
}

