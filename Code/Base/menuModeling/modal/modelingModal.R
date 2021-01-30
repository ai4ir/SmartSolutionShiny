
ModalModeling <- function(failed = FALSE) {
  labelStr <- "모델 리포팅 옵션"
  # browser()
  df <- as.data.frame(dfModelNest)
  choiceNames <- as.character(df[,"clusterGr"])
  choiceValues <- as.character(df[,"clusterGr"])
  

  
  modalDialog(
    title=labelStr,
    fluidRow(
      radioButtons("selModel","모델 선택",
                   choiceNames = choiceNames,
                   choiceValues = choiceValues
      )
    ),

    tags$p(""),
    tags$p(""),
    tags$hr(),
    fluidRow(
      radioButtons("selModelResultReport","리포트종류 선택", selected = curSelModelResultReport,
                   choiceNames = c("검증(전체 변수)", "검증(예측변수)", "검증(미예측변수)", "도메인 히스토그램"),
                   choiceValues = c("verifyModel", "verifyModelwPred", "verifyModelwoPred", "domainModel")
      )
    ),
    tags$p(""),
    tags$p(""),
    tags$hr(),
    # actionButton("MCP_y", label="Y 변수 선정"),
    # actionButton("MCP_x", label="x 변수 선정"),
    actionButton("MCP_color", label="color 변수 선정"),
    actionButton("MCP_size", label="size 변수 선정"),
    actionButton("MCP_shape", label="shape 변수 선정"),
    actionButton("MCP_tooltip", label="tooltip 변수 선정"),
    actionButton("MCP_data_id", label="data_id 변수 선정"),
    tags$p(""),
    tags$hr(),
    # tags$p(paste0(" Y : ", str_c(aesList[["y"]], collapse=", "))),
    # tags$p(paste0(" X : ", str_c(aesList[["x"]], collapse=", "))),
    tags$p(paste0(" color : ", aesList[["color"]])),
    tags$p(paste0(" size  : ", aesList[["size"]])),
    tags$p(paste0(" shape : ", aesList[["shape"]])),
    tags$p(paste0(" tooltip  : ", aesList[["tooltip"]])),
    tags$p(paste0(" data_id : ", aesList[["data_id"]])),

    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okModalModeling", "OK")
    ),
    size="l"
  )
}

treatModalModeling <- function(input, output, session) {

  observeEvent(input$okModalModeling, {
    withProgress(message="Common Plot Report Progress", value=0, {
      selCluster <- input$selModel
      selRow <- dfModelNest[dfModelNest$clusterGr==selCluster,]
      selModel <<- selRow[["model"]][[1]]
      numVar <- extractNumVarName(curSampleExplore)
      aesList[["x"]] <<- intersect(numVar, dfModelNest$modelDirectRaw[[1]])
      


      incProgress(0.2)
      ### 내부 디버깅 ###
      # selCluster <- "HSLA"
      ### 내부 디버깅 ###
      params <- list(df=curSampleExplore, selModel=selModel, aesList=aesList)
      outputFileName <- paste0("reportModel.html")
      
      switch(input$selModelResultReport,
             verifyModel = {
               outputFileFinalName <- paste0("모델링 검증 리포트_전체 변수_", chosenDFSourceFile,"_",
                                             selCluster, ".html")
               options(warn=-1)
               pathFileRmdModel <- "Base/menuModeling/Rmd/modelingVerifyReport.Rmd"
               rmarkdown::render(pathFileRmdModel, output_file = outputFileName,
                                 output_dir = pathHTMLReport,
                                 params = params,
                                 envir = new.env(parent = globalenv()), encoding="UTF-8")
               file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
            },
            verifyModelwPred = {
              outputFileFinalName <- paste0("모델링 검증 리포트_예측변수_", chosenDFSourceFile,"_",
                                            selCluster, ".html")
              options(warn=-1)
              pathFileRmdModel <- "Base/menuModeling/Rmd/modelingVerifyReportwPred.Rmd"
              rmarkdown::render(pathFileRmdModel, output_file = outputFileName,
                                output_dir = pathHTMLReport,
                                params = params,
                                envir = new.env(parent = globalenv()), encoding="UTF-8")
              file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
            },
            verifyModelwoPred = {
              outputFileFinalName <- paste0("모델링 검증 리포트_미예측 변수_", chosenDFSourceFile,"_",
                                            selCluster, ".html")
              options(warn=-1)
              pathFileRmdModel <- "Base/menuModeling/Rmd/modelingVerifyReportwoPred.Rmd"
              rmarkdown::render(pathFileRmdModel, output_file = outputFileName,
                                output_dir = pathHTMLReport,
                                params = params,
                                envir = new.env(parent = globalenv()), encoding="UTF-8")
              file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
            },
            domainModel = {
              outputFileFinalName <- paste0("모델링 도메인 리포트_", chosenDFSourceFile,"_",
                                            selCluster, ".html")
              options(warn=-1)
              pathFileRmdModel <- "Base/menuModeling/Rmd/modelingDomainReport.Rmd"
              rmarkdown::render(pathFileRmdModel, output_file = outputFileName,
                                output_dir = pathHTMLReport,
                                params = params,
                                envir = new.env(parent = globalenv()), encoding="UTF-8")
              file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
              
              
            }
             
      )
      
      incProgress(0.8)

    }) #withProgress(message="리포트 작성중", value=0, {
    
    strAlert <- paste0(pathHTMLReport,"에 ", outputFileFinalName,"이 저장되었습니다.")
    alert(strAlert)

    removeModal()
  })
  
  observeEvent(input$selModelResultReport, {
    switch(input$selModelResultReport,
           verifyModel = {
             show("MCP_color")
             show("MCP_size")
             show("MCP_shape")
             show("MCP_tooltip")
             show("MCP_data_id")
             curSelModelResultReport <<- "verifyModel"
           },
           verifyModelwPred = {
             show("MCP_color")
             show("MCP_size")
             show("MCP_shape")
             show("MCP_tooltip")
             show("MCP_data_id")
             curSelModelResultReport <<- "verifyModelwPred"
           },
           verifyModelwoPred = {
             show("MCP_color")
             show("MCP_size")
             show("MCP_shape")
             show("MCP_tooltip")
             show("MCP_data_id")
             curSelModelResultReport <<- "verifyModelwoPred"
           },
           domainModel = {
             show("MCP_color")
             show("MCP_size")
             show("MCP_shape")
             show("MCP_tooltip")
             show("MCP_data_id")
             curSelModelResultReport <<- "domainModel"
           }
      )
  })
  
}