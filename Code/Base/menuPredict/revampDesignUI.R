### SmartReport/Base/function ###

revampDesignUI <- function(input,output, session) {

  subFunc <- function(DesignTableNames, tableNo, DesignTableLabel) {
    if(is.null(DesignTableNames[1])) {
      for(i in 1:12) {
        inVarName <- paste0("DesignTable", tableNo, "Name",i) # 변수명
        hide(inVarName)
        inVar <- paste0("DesignTable", tableNo, "Cell",i)   # HTML ID
        hide(inVar)
      }
      return()
    }

    # minVec <- rep(NA,length(DomainTableNames))
    # names(minVec) <- DomainTableNames
    # maxVec <- minVec
    # names(maxVec) <- DomainTableNames
    
    html(id=paste0("DesignTable",tableNo,"Head1"), html="변수")
    html(id=paste0("DesignTable",tableNo,"Head2"), html="값")

    
    for(i in 1:length(DesignTableNames)) {
      inVarName <- paste0("DesignTable", tableNo,"Name", i) # 변수명
      html(id=inVarName, html=DesignTableLabel[i])
      show(inVarName)
      inVar <- paste0("DesignTable", tableNo, "Cell",i)   # HTML ID
      value <- curDesignDF[1,DesignTableNames[i]]
      updateNumericInput(session, inVar, value=value)
      show(inVar)
    }
    
    
    startPos <- length(DesignTableNames) +1
    for(i in startPos:12) {
      inVarName <- paste0("DesignTable", tableNo,"Name", i) # 변수명
      hide(inVarName)
      inVar <- paste0("DesignTable", tableNo, "Cell",i)   # HTML ID
      hide(inVar)
    }
    
    # MinDomainExplore <<- c(MinDomainExplore, minVec)
    # MaxDomainExplore <<- c(MaxDomainExplore, maxVec)  

  }


  subFunc(DesignTable1Names, 1, DesignTable1NamesLabel)
  subFunc(DesignTable2Names, 2, DesignTable2NamesLabel)
  subFunc(DesignTable3Names, 3, DesignTable3NamesLabel)
    
    # load("../USER/CoilTensile/Domain/CSA_X52.Rdata", .GlobalEnv)
    # updateDomainUI(input,output, session)
  

}

renderDesignUI_2 <- function() {
  
  # 범주형 변수의 범주 선정
 # tagList(
      fluidRow(title="catVarContentExplore",
               column(4,
                      # p("채취 위치 :"),

                      radioButtons("selCatPredictFewLevel1", 
                                         label="selCatFewLevel1", 
                                         choiceNames = list("init1", "init2"), 
                                         choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel2", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel3", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel4", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel5", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel6", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel7", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel8", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel9", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatPredictFewLevel10", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2"))
                      
                      
               ),
               column(4,
                      tags$p("범주형 변수"),
                      actionButton("selCatPredict1", catVarWithModalPredict[1]),
                      tags$p(""),
                      actionButton("selCatPredict2", catVarWithModalPredict[2]),
                      tags$p(""),
                      actionButton("selCatPredict3", catVarWithModalPredict[3]),
                      tags$p(""),
                      actionButton("selCatPredict4", catVarWithModalPredict[4]),
                      # tags$p(""),
                      actionButton("selCatPredict5", catVarWithModalPredict[5]),
                      # tags$p(""),
                      actionButton("selCatPredict6", catVarWithModalPredict[6]),
                      # tags$p(""),
                      actionButton("selCatPredict7", catVarWithModalPredict[7]),
                      # tags$p(""),
                      actionButton("selCatPredict8", catVarWithModalPredict[8]),
                      # tags$p(""),
                      actionButton("selCatPredict9", catVarWithModalPredict[9]),
                      # tags$p(""),
                      actionButton("selCatPredict10", catVarWithModalPredict[10])
                      
               )
      )
    # )

}

revampDesignUI_2 <- function(input,output, session) {
  for(i in 1:10) {
    inVarName <- paste0("selCatPredict",i) # 변수명
    hide(inVarName)
  }
  
  if(length(catVarWithModalPredict)<11) {
    seqNumber <- seq_along(catVarWithModalPredict)
  } else {
    seqNumber <- 1:10
  }
  
  for(i in seqNumber) {
    inVarName <- paste0("selCatPredict",i) # 변수명
    html(inVarName, catVarWithModalPredict[i] )
    show(inVarName)
  }
  
  for(i in 1:10) {
    inVarName <- paste0("selCatPredictFewLevel",i) # 변수명
    hide(inVarName)
  }

  if(length(catVarWithoutModalPredict)<11) {
    seqNumber <- seq_along(catVarWithoutModalPredict)
  } else {
    seqNumber <- 1:10
  }
  
  for(i in seqNumber) {
    inVarName <- paste0("selCatPredictFewLevel",i) # 변수명
    html(inVarName, catVarWithoutModalPredict[i] )
    updateRadioButtons(session, inVarName,
                             label = catVarWithoutModalPredict[i],
                             choices = as.character(selCatDesignPredict[[catVarWithoutModalPredict[i]]])
    )
    show(inVarName)
  }
  
}


### UI에 표시된 내용을 MinDomainExplore, MaxDomainExplore, selCatDomainExplore에 입력 ###
getDesignPredict <- function(input, output, session)  {
  
  subFunc <- function(DesignTableNames, tableNo) {
    for(i in 1:length(DesignTableNames)) {
      var <- DesignTableNames[i]
      inVar <- paste0("DesignTable", tableNo, "Cell",i)
      curDesignDF[var] <<- input[[inVar]]
    }
  }
  subFunc(DesignTable1Names, 1)
  subFunc(DesignTable2Names, 2)
  subFunc(DesignTable3Names, 3)

  # Modal을 이용하지 않고 Category를 선택하는 변수

  for(i in seq_along(catVarWithoutModalPredict)) {
      var <- catVarWithoutModalPredict[i]
      inVarName <- paste0("selCatPredictFewLevel",i) # 변수명
      curDesignDF[1,var] <<- input[[inVarName]]
  }

  return() 
}


### 파일 등에서 읽은 값을 UI에 표시 ###
updateDesignUI <- function(input,output, session) {

  subFunc <- function(DesignTableNames, tableNo) {
    for(i in 1:length(DesignTableNames)) {
      var <- DesignTableNames[i]
      inVar <- paste0("DesignTable",tableNo, "Cell",i)   # 변수값
      value <- curDesignDF[1,var]
      updateNumericInput(session, inVar, value=value)
    }
  } 
  
  subFunc(DesignTable1Names, 1)
  subFunc(DesignTable2Names, 2)
  subFunc(DesignTable3Names, 3)
}



# Modal에서 선택된  범주형 변수의 레벨을 selCatDomainExplore에 저장
updateCatVarPredict <- function(input, output, session) {

  subFunc <- function(buttonNo) {
    curSelCatVar <<- catVarWithModalPredict[buttonNo]
    strVec <- as.character(unique(curSampleExplore[,curSelCatVar]))
    strVec <- sort(strVec)
    selCatDesignPredict[[curSelCatVar]] <- c(strVec)
    choiceNames <- unique(selCatDesignPredict[[curSelCatVar]])
    choiceValues <- unique(selCatDesignPredict[[curSelCatVar]])
    showModal(ModalRadioButtons(choiceNames, choiceValues, "okModalSelCatVarPredict", curSelCatVar,
                                modalRadioButtonsID="ModalSelCatVarPredict", failed = FALSE))
    # showModal(ModalCheckboxGroup(title=curSelCatVar, modalCheckboxID="ModalSelCatVarExplore", label="범주 선정", 
    #                                      choiceNames=choiceNames, choiceValues=choiceValues,selected="ALL", 
    #                                      modalOKButtonID="okModalSelCatVarPredict"))
  }
  
  observeEvent(input$selCatPredict1, {  subFunc(1) })
  observeEvent(input$selCatPredict2, {  subFunc(2) })
  observeEvent(input$selCatPredict3, {  subFunc(3) })
  observeEvent(input$selCatPredict4, {  subFunc(4) })
  observeEvent(input$selCatPredict5, {  subFunc(5) })
  observeEvent(input$selCatPredict6, {  subFunc(6) })
  observeEvent(input$selCatPredict7, {  subFunc(7) })
  observeEvent(input$selCatPredict8, {  subFunc(8) })

  observeEvent(input$okModalSelCatVarPredict, {
    # print(paste0(" okModalSelCatVarPredict: curSelCatVar : ", curSelCatVar))
    # print(paste0(" okModalSelCatVarPredict: selCat : ", input[["ModalSelCatVarPredict"]]))

    if(is.null(input[["ModalSelCatVarPredict"]])) {
      alert("선택된 radioButton가 없습니다.  취소하려면 취소 버튼을 누르세요.")
    } else {
      curDesignDF[[curSelCatVar]] <<- input[["ModalSelCatVarPredict"]]
      print(paste0(" ModalSelCatVarPredict: selCatDesignPredict : ", selCatDesignPredict[[curSelCatVar]]))
      removeModal()
    }

  })
}

### EXCEL파일에서 읽은 값을 UI에 표시 ###
updateDomainUIFromEXCEL <- function(input,output, session, namePathFile) {
    # propName <- c("TS_YS","TS_TS","TS_EL","TS_Uel","TS_YR")
    # 
    # for(i in 1:length(propName)) {
    #     var <- propName[i]
    #     inVar <- paste0("minDomainExploreP",i)   # 변수값
    #     value <- as.vector(MinDomainExplore[var])
    #     updateNumericInput(session, inVar, value=value)
    #     inVar <- paste0("maxDomainExploreP",i)   # 변수값
    #     value <- as.vector(MaxDomainExplore[var])
    #     updateNumericInput(session, inVar, value=value)
    # }
    
    chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B")
    
    for(i in 1:length(chemCompName)) {
        var <- chemCompName[i]
        inVar <- paste0("minDomainExploreC",i)   # 변수값
        value <- as.vector(MinDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
        inVar <- paste0("maxDomainExploreC",i)   # 변수값
        value <- as.vector(MaxDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
    }
    
    rollingName <- c("T0","T0time","RDT","FDT","CT","CoilT")
    rollingName <- c("CoilT")  # PipeMill
    
    
    for(i in 1:length(rollingName)) {
        var <- rollingName[i]
        inVar <- paste0("minDomainExploreR",i)   # 변수값
        value <- as.vector(MinDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
        inVar <- paste0("maxDomainExploreR",i)   # 변수값
        value <- as.vector(MaxDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
    }
    
    # for (i in 1:length(numVarExplore)) {
    #     updateNumericInput(session,paste0("min",numVarExplore[i]), value=ReqMinDF[1,numVarExplore[i]])
    #     updateNumericInput(session,paste0("max",numVarExplore[i]), value=ReqMaxDF[1,numVarExplore[i]])
    # }
    # updateSelectInput(session, "anonCustomerExplore",label="고객 선택", choices=as.character(c("ALL",unique(DFSourceExplore[,"anonCustomer"]))),
    #                   selected=as.character(ReqMinDF[1,"anonCustomer"]))
    # 
    # updateSelectInput(session, "GangJongExplore",label="강종 선택", choices=as.character(c("ALL",as.character(unique(DFSourceExplore[,"GangJong"])))),
    #                   selected=as.character(ReqMinDF[1,"GangJong"]))
    # updateSelectInput(session, "FormingProcessExplore",label="조관법", choices=as.character(c("ALL",as.character(unique(DFSourceExplore[,"FormingProcess"])))),
    #                   selected=as.character(ReqMinDF[1,"FormingProcess"]))
}

