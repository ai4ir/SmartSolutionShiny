

ModalCommonPlot <- function(failed = FALSE) {
  labelStr <- "플롯 옵션"
  modalDialog(
    title=labelStr,
    actionButton("MCP_y", label="Y 변수 선정"),
    actionButton("MCP_x", label="x 변수 선정"),
    actionButton("MCP_color", label="color 변수 선정"),
    actionButton("MCP_size", label="size 변수 선정"),
    actionButton("MCP_shape", label="shape 변수 선정"),
    actionButton("MCP_tooltip", label="tooltip 변수 선정"),
    actionButton("MCP_data_id", label="data_id 변수 선정"),
    actionButton("MCP_fitOption", label="Fitting 함수 선정"),
    tags$p(""),
    tags$hr(),
    tags$p(paste0(" Y : ", str_c(aesList[["y"]], collapse=", "))),
    tags$p(paste0(" X : ", str_c(aesList[["x"]], collapse=", "))),
    tags$p(paste0(" color : ", aesList[["color"]])),
    tags$p(paste0(" size  : ", aesList[["size"]])),
    tags$p(paste0(" shape : ", aesList[["shape"]])),
    tags$p(paste0(" tooltip  : ", aesList[["tooltip"]])),
    tags$p(paste0(" data_id : ", aesList[["data_id"]])),
    tags$p(paste0(" fitOption : ", aesList[["fitOption"]])),

    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalCommonPlot", "OK")
    ),
    size="l"
  )
  

}

treatModalCommonPlot <- function(input, output, session) {


  observeEvent(input$MCP_y, {
    numVar <- extractNumVarName(dfReportCommon)
    # numVar <- rownames(orderVFcluster(dfReportCommon))

    curAes <<- "y"
    
    switch(curCommonPlot, 
           ScatterOneY = {
             numVar <- rownames(orderVFcluster(dfReportCommon))
             choiceNames <- attr(dfReportCommon[,numVar[1]],"labelShort")
             for(i in 2:length(numVar)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
             }
             strExplain <- "nuneric 변수만 제공됬습니다."
             showModal(ModalRadioButtons(choiceNames, numVar,"okMCP", "selModal", "ScatterOneY", "y 변수 선정",strExplain))

           },
           ViolinOneY = {
             choiceNames <- attr(dfReportCommon[,numVar[1]],"labelShort")
             for(i in 2:length(numVar)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
             }
             strExplain <- "nuneric 변수만 제공됬습니다."
             showModal(ModalRadioButtons(choiceNames, numVar,"okMCP", "selModal", "violinOneY", "y 변수 선정",strExplain))
           },
           {
             numVar <- extractNumVarName(dfReportCommon)
              # numVar <- rownames(orderVFcluster(dfReportCommon))
             numVar <- c("ALL.ALL",numVar)
             
             # choiceNames <- "All.ALL"
             # for(i in 2:length(numVar)) {
             #   choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
             # }
             
             choiceNames <- "All.ALL"
             for(i in 2:length(numVar)) {
               if(is.null(attr(dfReportCommon[,numVar[i]],"labelShort") )) {
                 choiceNames <- c(choiceNames,paste0("임시 라벨 ", i))
               } else {
                 choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
               }
             }
             
             
             strExplain <- "nuneric 변수만 제공됬습니다."
             showModal(ModalCheckboxGroup(choiceNames, numVar, "okMCP", "selModal", "y 선정 대화창", label="y 선정", strExplain, selected="ALL.ALL"))
           }
    )
    

  })
  
  observeEvent(input$MCP_x, {

    curAes <<- "x"
    
    switch(curCommonPlot, 
           ScatterOneX = {
             numVar <- rownames(orderVFcluster(dfReportCommon))
             choiceNames <- attr(dfReportCommon[,numVar[1]],"labelShort")
             for(i in 2:length(numVar)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
             }
             strExplain <- "numeric 변수만 선택됬습니다."
             showModal(ModalRadioButtons(choiceNames, numVar,"okMCP", "selModal", "ScatterOneX", "x 변수 선정",strExplain))
             
           },
           ViolinOneY = {
             var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=6) 
             dfSubset <- dfReportCommon[,var]
             choiceNames <- attr(dfSubset[,var[1]],"labelShort")
             for(i in 2:length(var)) {
               choiceNames <- c(choiceNames, attr(dfSubset[,var[i]],"labelShort") )
             }
             strExplain <- "수준이 1이상 6 이하인 변수만 선택됬습니다."
             showModal(ModalCheckboxGroup(title="ViolinOneY : X 선정 대화창", modalCheckboxID="selModal", label="ViolinOneY : X 선정",
                                          choiceNames=choiceNames, choiceValues=var,
                                          modalOKButtonID="okMCP", strExplain=strExplain, selected="ALL.ALL"))
           },
           ViolinOneX = {
             var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=6) 
             choiceNames <- attr(dfReportCommon[,var[1]],"labelShort")
             for(i in 2:length(var)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
             }
             strExplain <- "수준이 1이상 6 이하인 변수만 선택됬습니다."
             showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "ViolinOneX", "x 변수 선정",strExplain))
             
           },
           CollectDiag = {
             var <- colnames(dfReportCommon)
             choiceNames <- attr(dfReportCommon[,var[1]],"labelShort")
             for(i in 2:length(var)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
             }
             strExplain <- "모든 변수가 선택됬습니다."
             showModal(ModalCheckboxGroup(title="수집 진단 : X 선정 대화창", modalCheckboxID="selModal", label="collectDiag : X 선정",
                                          choiceNames=choiceNames, choiceValues=var,
                                          modalOKButtonID="okMCP", strExplain=strExplain, selected="ALL.ALL"))
             
           },
           {
             numVar <- rownames(orderVFcluster(dfReportCommon))
             numVar <- c("ALL.ALL",numVar)
             choiceNames <- "ALL.ALL"             
             for(i in 2:length(numVar)) {
               choiceNames <- c(choiceNames, attr(dfReportCommon[,numVar[i]],"labelShort") )
             }
             showModal(ModalCheckboxGroup(title="X 선정 대화창", modalCheckboxID="selModal", label="X 선정",
                                          choiceNames=choiceNames, choiceValues=numVar,
                                          modalOKButtonID="okMCP", selected=NULL))
           }
           )


  })
  
  observeEvent(input$MCP_color, {
    var <- unique(colnames(dfReportCommon))
    var <- c("NULL",var)
    curAes <<- "color"
    choiceNames <- "NULL"
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    showModal(ModalRadioButtons(choiceNames, var, "okMCP", "selModal", "심볼 색 대화창", "color 변수 선정"))
  })
  
  observeEvent(input$MCP_size, {
    var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=7) 
    var <- c("NULL",var)
    strExplain <- "수준이 2이상 6 이하인 변수만 선택됬습니다."
    curAes <<- "size"
    choiceNames <- "NULL"
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "심볼 사이즈", "사이즈 변수 선정",strExplain))
  })
  
  observeEvent(input$MCP_shape, {
    var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=7) 
    var <- c("NULL",var)
    strExplain <- "수준이 2이상 6 이하인 변수만 선택됬습니다."
    curAes <<- "shape"
    choiceNames <- "NULL"  
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "심볼 모양", "모양 변수 선정",strExplain))
  })
  
  observeEvent(input$MCP_fill, {
    var <- selectVarWithGivenLevels(df=dfReportCommon, minNoLevels=1, maxNoLevels=7) 
    var <- c("NULL",var)
    strExplain <- "수준이 2이상 7 이하인 변수만 선택됬습니다."
    curAes <<- "fill"
    choiceNames <- "NULL"
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "심볼 fill", "fill 변수 선정",strExplain))
  })
  
  observeEvent(input$MCP_tooltip, {
    var <- unique(colnames(dfReportCommon))
    var <- c("tooltip.DEFAULT", var)
    curAes <<- "tooltip"
    choiceNames <- "tooltip.DEFAULT"
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }
    strExplain <- "풍선 도움말에 표시될 내용이 들어있는 변수를 선정하세요"
    showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "tooltip 변수", "tooltip 변수 선정",strExplain))
  })
  
  observeEvent(input$MCP_data_id, {
    var <- unique(colnames(dfReportCommon))
    curAes <<- "data_id"
    choiceNames <- attr(dfReportCommon[,var[1]],"labelShort")
    for(i in 2:length(var)) {
      choiceNames <- c(choiceNames, attr(dfReportCommon[,var[i]],"labelShort") )
    }    
    strExplain <- "data_id 변수를 선정하세요"
    showModal(ModalRadioButtons(choiceNames, var,"okMCP", "selModal", "data_id", "data_id 변수 선정",strExplain))
  })
  
  observeEvent(input$MCP_fitOption, {
    choiceNames <- c("NoFit", "Fit1", "Fit2")
    choiceValues <- c("NoFit", "Fit1", "Fit2")
    curAes <<- "fitOption"
    strExplain <- "No fitting, 1차식, 2차식 fitting이 있습니다. 다른 fitting curve가 필요하신면 연락주세요."
    showModal(ModalRadioButtons(choiceNames, choiceNames,"okMCP", "selModal", "fitOption", "fitOption 선정",strExplain))
  })
  
  observeEvent(input$okMCP, {
    aesList[[curAes]] <<- input$selModal  # aesList[["x"]] <<- "Mn"
    if(aesList[[curAes]][1]=="ALL.ALL" ) {
      # aesList[[curAes]] <<- rownames(orderVFcluster(dfReportCommon))
      aesList[[curAes]] <<- colnames(dfReportCommon)
    }
    if(aesList[[curAes]][1]=="NULL" | aesList[[curAes]][1]=="tooltip.DEFAULT"   ) {
      aesList[[curAes]] <<- NULL
    }

    removeModal()
    switch(triggerMCP,
           # module = {
           #   switch( curAes,
           #     x = {
           #       graphOption[["xAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["x"]][1]], "label")
           #       # graphOption[["xAxisTitle"]][1]<<- aesList[["x"]][1]
           #       if(curTabExplore %in% c("scatter1")) {
           #         graphOption[["minX"]][1] <<- min(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)
           #         graphOption[["maxX"]][1] <<- max(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)
           #       }
           #     },
           #     y = {
           #       graphOption[["yAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "label")
           #     # graphOption[["yAxisTitle"]][1]<<- aesList[["y"]][1]
           #       graphOption[["minY"]][1] <<- min(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)
           #       graphOption[["maxY"]][1] <<- max(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)
           #     },
           #     {}
           #   )
           # },
           commonPlot = {
             showModal(ModalCommonPlot())
           },
           commonPlot_Violin = {
             showModal(ModalCommonPlot())
             hideButton=c( "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id", "MCP_fitOption")
             for(i in seq_along(hideButton)) {
               hide(hideButton[i])
             }
           },
           commonPlot_Histogram = {
             showModal(ModalCommonPlot())
             hideButton=c("MCP_y","MCP_color", "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id", "MCP_fitOption")
             for(i in seq_along(hideButton)) {
               hide(hideButton[i])
             }
           },
           commonPlot_CollectDiag = {
             showModal(ModalCommonPlot())
             hideButton=c("MCP_y","MCP_color", "MCP_size", "MCP_shape", "MCP_tooltip", "MCP_data_id", "MCP_fitOption")
             for(i in seq_along(hideButton)) {
               hide(hideButton[i])
             }
           },
           modelResult = {
             showModal(ModalModeling())
           },
           {alert("triggerMCP가 비었습니다.")}
    )



    # switch(curCommonPlot,
    #        OneY = {showModal(ModalCommonPlotOneY())},
    #        {showModal(ModalCommonPlot())}
    #        
    #        )
    
  })
  
  observeEvent(input$okModalCommonPlot, {
    withProgress(message="Common Plot Report Progress", value=0, {
      incProgress(0.2)
      # params <- list(df=dfReportCommon, aesList=aesList)
      outputFileName <- paste0("commonPlotReport", fromReportCommon, "_","general", ".html")
      switch(curCommonPlot,
             ScatterOneX = {
               selVar <- aesList[["x"]] 
               paramsRmd <- list(df=dfReportCommon, aesList=aesList)
               outputFileFinalName <- paste0("탐색 그래프 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix, selVar,"-",aesList[["color"]], "-",aesList[["data_id"]], 
                                             "-",aesList[["fitOption"]],"-",dim(dfReportCommon)[[1]],
                                             "_",dim(dfReportCommon)[2],".html")
             },
             ScatterOneY = {
               selVar <- aesList[["y"]] 
               paramsRmd <- list(df=dfReportCommon, aesList=aesList)
               outputFileFinalName <- paste0("탐색 그래프 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix, selVar,"-",aesList[["color"]], "-",aesList[["data_id"]], 
                                             "-",aesList[["fitOption"]],"-",dim(dfReportCommon)[[1]],
                                             "_",dim(dfReportCommon)[2],".html")
             },
             ViolinOneY = {
               selVar <- paste0(aesList[["spare1"]],"-",aesList[["y"]]) 
               # aesList[["color"]] <- "clusterLassoSel"
               # aesList[["y"]] <<- rownames(orderVFcluster(dfReportCommon))
               
               pValueVec <- vapply(aesList[["x"]], calcPValueOneYAov,dfReportCommon,aesList[["y"]], FUN.VALUE=numeric(1))
               pValueVec <- pValueVec[order(pValueVec)]
               aesList[["pValueVector"]] <<- pValueVec
               aesList[["x"]] <<- names(pValueVec)

               aesList[["clusterMethod"]][1] <<- "clusterKmeans"
           
               paramsRmd <- list(df=dfReportCommon, aesList=aesList, aesListScatter=aesListScatter)
               outputFileFinalName <- paste0("ViolinOneY 탐색 그래프 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix, selVar,"-",aesList[["color"]],"-",dim(dfReportCommon)[[1]], 
                                             "_",dim(dfReportCommon)[2],".html")
               
             },
             ViolinOneX = {
               selVar <- aesList[["x"]] 

               pValueVec <- vapply(aesList[["y"]], calcPValueOneXAov,dfReportCommon,aesList[["x"]], FUN.VALUE=numeric(1))
               pValueVec <- pValueVec[order(pValueVec)]
               aesList[["pValueVector"]] <<- pValueVec
               aesList[["y"]] <<- names(pValueVec)
               
               aesList[["clusterMethod"]][1] <<- "clusterKmeans"
               

               paramsRmd <- list(df=dfReportCommon, aesList=aesList, aesListScatter=aesListScatter)
               outputFileFinalName <- paste0("ViolinOneX 탐색 그래프 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix, selVar,"-",aesList[["color"]],"-",dim(dfReportCommon)[[1]],
                                             "_",dim(dfReportCommon)[2],".html")
               
             },
             Histogram = {
               pValueVec <- vapply(aesList[["x"]], calcPValueShapiro,dfReportCommon, FUN.VALUE=numeric(1))
               pValueVec <- pValueVec[order(pValueVec)]
               aesList[["pValueVector"]] <<- pValueVec
               aesList[["x"]] <<- names(pValueVec)
               paramsRmd <- list(df=dfReportCommon, aesList=aesList)
               outputFileFinalName <- paste0("Histogram 탐색 그래프 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix,"-",aesList[["color"]], "-",aesList[["data_id"]], 
                                             "-",aesList[["fitOption"]],"-",dim(dfReportCommon)[[1]],
                                             "_",dim(dfReportCommon)[2],".html")
             },
             CollectDiag = {

               paramsRmd <- list(df=dfReportCommon, aesList=aesList)
               outputFileFinalName <- paste0("수집진단 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                             fileNameSuffix,"-",aesList[["color"]], "-",aesList[["data_id"]], 
                                             "-",aesList[["fitOption"]],"-",dim(dfReportCommon)[[1]],
                                             "_",dim(dfReportCommon)[2],".html")
             },
             {}
             )

      options(warn=-1)
      rmarkdown::render(pathFileRmdCommonPlot, output_file = outputFileName,
                        output_dir = pathHTMLReport,
                        params = paramsRmd,
                        envir = new.env(parent = globalenv()), encoding="UTF-8")
      file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
      incProgress(0.8)

    }) #withProgress(message="리포트 작성중", value=0, {
    
    strAlert <- paste0(pathHTMLReport,"에 ", outputFileFinalName,"이 저장되었습니다.")
    alert(strAlert)

    removeModal()
  })
  
}