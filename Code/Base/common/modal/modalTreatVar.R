


ModalTreatVar <- function(failed = FALSE) {
  labelStr <- "변수 전처리"
  modalDialog(
    title=labelStr,
    actionButton("MTV_del", label="변수 삭제"),
    actionButton("MTV_remain", label="변수 남기기"),
    actionButton("MTV_mutate", label="변수 생성"),
    actionButton("MTV_change", label="속성 변경"),
    actionButton("MTV_saveRDS", label="RDS 저장"),
    actionButton("MTV_saveCSV", label="CSV 저장"),
    tags$p(""),
    tags$hr(),
    tags$p(paste0(" 삭제변수 : ", MTVCodeList[["del"]])),
    tags$p(paste0(" 잔류변수 : ", MTVCodeList[["remain"]])),
    tags$p(paste0(" 생성변수 : ", MTVCodeList[["mutate"]])),
    tags$p(paste0(" 속성변경 : ", MTVCodeList[["change"]])),


    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("종료")
    ),
    size="l"
  )
}

treatModalTreatVar <- function(input, output, session) {


  observeEvent(input$MTV_del, {
    allVar <- colnames(DFSource)
    showModal(ModalCheckboxGroup(title="삭제할 변수 선정 대화창", modalCheckboxID="ModalCheckboxGroup",
                                 label="분석에 불필요한 변수를 체크하세요!!",
                                 choiceNames=allVar, choiceValues=allVar,
                                 modalOKButtonID="okMTV_del"))
  })

  observeEvent(input$okMTV_del, {

    chosenVarModal <- str_c("\"", input$ModalCheckboxGroup, "\"", collapse=", ")
    # ttt <<- input$ModalCheckboxGroup
    # chosenVarModal <- str_c("\"", ttt, "\"", collapse=", ")
    stringCode <- paste0("chosenVar <- c(", chosenVarModal, ")")
    exprCode <- parse(text=stringCode)
    MTVCodeList[["del"]] <<- exprCode
    eval(exprCode)
    removeModal()

    remainVar <- setdiff(colnames(DFSource), chosenVar)
    
    DFSource <<- DFSource[, remainVar]
    reactDFSource(DFSource)

    showModal(ModalTreatVar())

  })
  
  observeEvent(input$MTV_remain, {
    allVar <- colnames(DFSource)
    showModal(ModalCheckboxGroup(title="남길 변수 선정 대화창", modalCheckboxID="ModalCheckboxGroup",
                                 label="분석에 필요한 변수를 체크하세요!!",
                                 choiceNames=allVar, choiceValues=allVar,
                                 modalOKButtonID="okMTV_remain"))
  })
  
  observeEvent(input$okMTV_remain, {
    
    chosenVarModal <- str_c("\"", input$ModalCheckboxGroup, "\"", collapse=", ")
    # ttt <<- input$ModalCheckboxGroup
    # chosenVarModal <- str_c("\"", ttt, "\"", collapse=", ")
    stringCode <- paste0("chosenVar <- c(", chosenVarModal, ")")
    exprCode <- parse(text=stringCode)
    MTVCodeList[["remain"]] <<- exprCode
    eval(exprCode)
    removeModal()
    
    remainVar <- chosenVar
    
    DFSource <<- DFSource[, remainVar]
    reactDFSource(DFSource)
    
    showModal(ModalTreatVar())
    
  })
  
  observeEvent(input$MTV_mutate, {
    alert("개발 예정입니다.")
    removeModal()
  })
  
  observeEvent(input$MTV_change, {
    numVar <- extractNumVarName(DFSource)
    showModal(ModalCheckboxGroup(title="범주형 변수 선정 대화창", modalCheckboxID="ModalCheckboxGroup", 
                                 label="자동 분류로는 연속형변수이지만 실제는 범주형변수를 체크하세요!!",
                                 choiceNames=numVar, choiceValues=numVar,
                                 modalOKButtonID="okMTV_change"))
  })
  
  observeEvent(input$okMTV_change, {
    
    chosenVarModal <- str_c("\"", input$ModalCheckboxGroup, "\"", collapse=", ")
    # ttt <<- input$ModalCheckboxGroup
    # chosenVarModal <- str_c("\"", ttt, "\"", collapse=", ")
    stringCode <- paste0("chosenVar <- c(", chosenVarModal, ")")
    exprCode <- parse(text=stringCode)
    MTVCodeList[["change"]] <<- exprCode
    eval(exprCode)
    removeModal()

    for(x in chosenVar) {
      DFSource[,x] <<- as.factor(DFSource[,x])
    }
    
    showModal(ModalTreatVar())

  })
  
  
  observeEvent(input$MTV_saveRDS, {
    pathFile <- paste0("../USER/EXCEL/",chosenDFSourceFile, "_전처리.rds")
    write_rds(DFSource, pathFile)
    strAlert <- paste0(pathFile, " 가 저장되었습니다.")
    alert(strAlert)
  })
  
  observeEvent(input$MTV_saveCSV, {
    pathFile <- paste0("../USER/EXCEL/",chosenDFSourceFile, "_전처리.csv")
    write_csv(DFSource, pathFile)
    strAlert <- paste0(pathFile, " 가 저장되었습니다.")
    alert(strAlert)
  })
  

}