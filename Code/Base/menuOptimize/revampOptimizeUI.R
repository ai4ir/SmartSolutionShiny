### smartReport/function ###

revampOptimizeUI <- function(input,output, session) {

  subFunc <- function(OptimizeTableNames, tableNo, OptimizeTableLabel) {
    if(is.null(OptimizeTableNames[1])) {
      for(i in 1:12) {
        inVarName <- paste0("OptimizeTable", tableNo, "Name",i) # 변수명
        hide(inVarName)
        inVar <- paste0("OptimizeTable", tableNo, "Cell",i)   # HTML ID
        hide(inVar)
      }
      return()
    }

    # minVec <- rep(NA,length(DomainTableNames))
    # names(minVec) <- DomainTableNames
    # maxVec <- minVec
    # names(maxVec) <- DomainTableNames
    
    html(id=paste0("OptimizeTable",tableNo,"Head1"), html="변수")
    html(id=paste0("OptimizeTable",tableNo,"Head2"), html="설계값")
    html(id=paste0("OptimizeTable",tableNo,"Head3"), html="최적화값")

    
    for(i in 1:length(OptimizeTableNames)) {
      inVarName <- paste0("OptimizeTable", tableNo,"Name", i) # 변수명
      html(id=inVarName, html=OptimizeTableLabel[i])
      show(inVarName)
      inVar <- paste0("OptimizeTable", tableNo, "Cell",i)   # HTML ID
      value <- curOptimizeDF[1,OptimizeTableNames[i]]
      updateNumericInput(session, inVar, value=value)
      show(inVar)
    }
    
    
    startPos <- length(OptimizeTableNames) +1
    for(i in startPos:12) {
      inVarName <- paste0("OptimizeTable", tableNo,"Name", i) # 변수명
      hide(inVarName)
      inVar <- paste0("OptimizeTable", tableNo, "Cell",i)   # HTML ID
      hide(inVar)
    }
    
    # MinDomainExplore <<- c(MinDomainExplore, minVec)
    # MaxDomainExplore <<- c(MaxDomainExplore, maxVec)  

  }


  subFunc(OptimizeTable1Names, 1, OptimizeTable1NamesLabel)
  subFunc(OptimizeTable2Names, 2, OptimizeTable2NamesLabel)
  subFunc(OptimizeTable3Names, 3, OptimizeTable3NamesLabel)
    
    # load("../USER/CoilTensile/Domain/CSA_X52.Rdata", .GlobalEnv)
    # updateDomainUI(input,output, session)
  

}

renderOptimizeUI_2 <- function() {
  
  # 범주형 변수의 범주 선정
 # tagList(
      fluidRow(title="catVarContentExplore",
               column(4,
                      # p("채취 위치 :"),

                      radioButtons("selCatOptimizeFewLevel1", 
                                         label="selCatFewLevel1", 
                                         choiceNames = list("init1", "init2"), 
                                         choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel2", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel3", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel4", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel5", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel6", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel7", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel8", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel9", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2")),
                      radioButtons("selCatOptimizeFewLevel10", 
                                   label="selCatFewLevel1", 
                                   choiceNames = list("init1", "init2"), 
                                   choiceValues = list("init1", "init2"))
                      
                      
               ),
               column(4,
                      tags$p("범주형 변수"),
                      actionButton("selCatOptimize1", catVarWithModalOptimize[1]),
                      tags$p(""),
                      actionButton("selCatOptimize2", catVarWithModalOptimize[2]),
                      tags$p(""),
                      actionButton("selCatOptimize3", catVarWithModalOptimize[3]),
                      tags$p(""),
                      actionButton("selCatOptimize4", catVarWithModalOptimize[4]),
                      # tags$p(""),
                      actionButton("selCatOptimize5", catVarWithModalOptimize[5]),
                      # tags$p(""),
                      actionButton("selCatOptimize6", catVarWithModalOptimize[6]),
                      # tags$p(""),
                      actionButton("selCatOptimize7", catVarWithModalOptimize[7]),
                      # tags$p(""),
                      actionButton("selCatOptimize8", catVarWithModalOptimize[8]),
                      # tags$p(""),
                      actionButton("selCatOptimize9", catVarWithModalOptimize[9]),
                      # tags$p(""),
                      actionButton("selCatOptimize10", catVarWithModalOptimize[10])
                      
               )
      )
    # )

}

revampOptimizeUI_2 <- function(input,output, session) {
  for(i in 1:10) {
    inVarName <- paste0("selCatOptimize",i) # 변수명
    hide(inVarName)
  }
  
  if(length(catVarWithModalOptimize)<11) {
    seqNumber <- seq_along(catVarWithModalOptimize)
  } else {
    seqNumber <- 1:10
  }
  
  for(i in seqNumber) {
    inVarName <- paste0("selCatOptimize",i) # 변수명
    html(inVarName, catVarWithModalOptimize[i] )
    show(inVarName)
  }
  
  for(i in 1:10) {
    inVarName <- paste0("selCatOptimizeFewLevel",i) # 변수명
    hide(inVarName)
  }

  if(length(catVarWithoutModalOptimize)<11) {
    seqNumber <- seq_along(catVarWithoutModalOptimize)
  } else {
    seqNumber <- 1:10
  }
  
  for(i in seqNumber) {
    inVarName <- paste0("selCatOptimizeFewLevel",i) # 변수명
    html(inVarName, catVarWithoutModalOptimize[i] )
    updateRadioButtons(session, inVarName,
                             label = catVarWithoutModalOptimize[i],
                             choices = as.character(selCatOptimizeOptimize[[catVarWithoutModalOptimize[i]]])
    )
    show(inVarName)
  }
  
}


### UI에 표시된 내용을 MinDomainExplore, MaxDomainExplore, selCatDomainExplore에 입력 ###
getOptimizeOptimize <- function(input, output, session)  {
  
  subFunc <- function(OptimizeTableNames, tableNo) {
    for(i in 1:length(OptimizeTableNames)) {
      var <- OptimizeTableNames[i]
      inVar <- paste0("OptimizeTable", tableNo, "Cell",i)
      curOptimizeDF[var] <<- input[[inVar]]
    }
  }
  subFunc(OptimizeTable1Names, 1)
  subFunc(OptimizeTable2Names, 2)
  subFunc(OptimizeTable3Names, 3)

  # Modal을 이용하지 않고 Category를 선택하는 변수

  for(i in seq_along(catVarWithoutModalOptimize)) {
      var <- catVarWithoutModalOptimize[i]
      inVarName <- paste0("selCatOptimizeFewLevel",i) # 변수명
      curOptimizeDF[1,var] <<- input[[inVarName]]
  }

  return() 
}

### 최적값을 UI에 표시 ###
updateOptimumValueUI <- function(input,output, session) {
  
  subFunc <- function(OptimizeTableNames, tableNo) {
    if(length(OptimizeTableNames)==0) {
      return()
    }
    taregtVarValue <<- input[["targetVarValue"]]
    for(i in 1:length(OptimizeTableNames)){
      curOptVar <<- OptimizeTableNames[i]
      digit <- attr(DFSource[,which(attr(DFSource,"names")==curOptVar)],"digit")
      optValue <- optim(c(curOptimizeDF[1,curOptVar]), meas_distance, data=curOptimizeDF)
      inVar <- paste0("OptimizeTable",tableNo, "Optim",i)   # 변수값      
      html(id=inVar,round(optValue[["par"]],digit) )
    }
  } 
  
  subFunc(OptimizeTable1Names, 1)
  subFunc(OptimizeTable2Names, 2)
  subFunc(OptimizeTable3Names, 3)
}

### 파일 등에서 읽은 값을 UI에 표시 ###
updateOptimizeUI <- function(input,output, session) {

  subFunc <- function(OptimizeTableNames, tableNo) {
    for(i in 1:length(OptimizeTableNames)) {
      var <- OptimizeTableNames[i]
      inVar <- paste0("OptimizeTable",tableNo, "Cell",i)   # 변수값
      value <- curOptimizeDF[1,var]
      updateNumericInput(session, inVar, value=value)
    }
  } 
  
  subFunc(OptimizeTable1Names, 1)
  subFunc(OptimizeTable2Names, 2)
  subFunc(OptimizeTable3Names, 3)
}


# Modal에서 선택된  범주형 변수의 레벨을 selCatDomainExplore에 저장
updateCatVarOptimize <- function(input, output, session) {

  subFunc <- function(buttonNo) {
    curSelCatVar <<- catVarWithModalOptimize[buttonNo]
    strVec <- as.character(unique(curSampleExplore[,curSelCatVar]))
    strVec <- sort(strVec)
    selCatOptimizeOptimize[[curSelCatVar]] <- c(strVec)
    choiceNames <- unique(selCatOptimizeOptimize[[curSelCatVar]])
    choiceValues <- unique(selCatOptimizeOptimize[[curSelCatVar]])
    showModal(ModalRadioButtons(choiceNames, choiceValues, "okModalSelCatVarOptimize", curSelCatVar,
                                modalRadioButtonsID="ModalSelCatVarOptimize", failed = FALSE))
    # showModal(ModalCheckboxGroup(title=curSelCatVar, modalCheckboxID="ModalSelCatVarExplore", label="범주 선정", 
    #                                      choiceNames=choiceNames, choiceValues=choiceValues,selected="ALL", 
    #                                      modalOKButtonID="okModalSelCatVarOptimize"))
  }
  
  observeEvent(input$selCatOptimize1, {  subFunc(1) })
  observeEvent(input$selCatOptimize2, {  subFunc(2) })
  observeEvent(input$selCatOptimize3, {  subFunc(3) })
  observeEvent(input$selCatOptimize4, {  subFunc(4) })
  observeEvent(input$selCatOptimize5, {  subFunc(5) })
  observeEvent(input$selCatOptimize6, {  subFunc(6) })
  observeEvent(input$selCatOptimize7, {  subFunc(7) })
  observeEvent(input$selCatOptimize8, {  subFunc(8) })

  observeEvent(input$okModalSelCatVarOptimize, {
    # print(paste0(" okModalSelCatVarOptimize: curSelCatVar : ", curSelCatVar))
    # print(paste0(" okModalSelCatVarOptimize: selCat : ", input[["ModalSelCatVarOptimize"]]))

    if(is.null(input[["ModalSelCatVarOptimize"]])) {
      alert("선택된 radioButton가 없습니다.  취소하려면 취소 버튼을 누르세요.")
    } else {
      curOptimizeDF[[curSelCatVar]] <<- input[["ModalSelCatVarOptimize"]]
      print(paste0(" ModalSelCatVarOptimize: selCatOptimizeOptimize : ", selCatOptimizeOptimize[[curSelCatVar]]))
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

