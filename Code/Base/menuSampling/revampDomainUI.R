### smartReport/function ###

revampDomainUI <- function(input,output, session) {
  MinDomainExplore <<- NULL
  MaxDomainExplore <<- NULL
  subFunc <- function(DomainTableNames, tableNo, DomainTableLabel) {
    if(is.null(DomainTableNames[1])) {
      for(i in 1:12) {
        inVarName <- paste0("DomainTable", tableNo, "Name",i) # 변수명
        hide(inVarName)
        inVar <- paste0("minDomTable", tableNo, "Cell",i)   # HTML ID
        hide(inVar)
        inVar <- paste0("maxDomTable", tableNo, "Cell",i)   # HTML ID
        hide(inVar)
      }
      return()
    }

    minVec <- rep(NA,length(DomainTableNames))
    names(minVec) <- DomainTableNames
    maxVec <- minVec
    names(maxVec) <- DomainTableNames
    
    html(id=paste0("DomainTable",tableNo,"Head1"), html="변수")
    html(id=paste0("DomainTable",tableNo,"Head2"), html="하한")
    html(id=paste0("DomainTable",tableNo,"Head3"), html="상한")

    
    for(i in 1:length(DomainTableNames)) {
      var <- DomainTableLabel[i]
      inVarName <- paste0("DomainTable", tableNo,"Name", i) # 변수명
      html(id=inVarName, html=var)
      show(inVarName)
      inVar <- paste0("minDomTable", tableNo, "Cell",i)   # HTML ID
      show(inVar)
      inVar <- paste0("maxDomTable", tableNo, "Cell",i)   # HTML ID
      show(inVar)
    }
    
    
    startPos <- length(DomainTableNames) +1
    for(i in startPos:12) {
      inVarName <- paste0("DomainTable", tableNo,"Name", i) # 변수명
      hide(inVarName)
      inVar <- paste0("minDomTable", tableNo, "Cell",i)   # HTML ID
      hide(inVar)
      inVar <- paste0("maxDomTable", tableNo, "Cell",i)   # HTML ID
      hide(inVar)
    }
    
    MinDomainExplore <<- c(MinDomainExplore, minVec)
    MaxDomainExplore <<- c(MaxDomainExplore, maxVec)  

  }

  subFunc(DomainTable1Names, 1, DomainTable1NamesLabel)
  subFunc(DomainTable2Names, 2, DomainTable2NamesLabel)
  subFunc(DomainTable3Names, 3, DomainTable3NamesLabel)
    
    # load("../USER/CoilTensile/Domain/CSA_X52.Rdata", .GlobalEnv)
    # updateDomainUI(input,output, session)

}

renderDomainUI_2 <- function() {
  
  # 범주형 변수의 범주 선정
 # tagList(
      fluidRow(title="catVarContentExplore",
               column(4,
                      # p("채취 위치 :"),

                      checkboxGroupInput("selCatExploreFewLevels1", 
                                         label="selCatFewLevels1", 
                                         choiceNames = list("init1", "init2"), 
                                         choiceValues = list("init1", "init2"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels2", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels3", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels4", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels5", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels6", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels7", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels8", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels9", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels10", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels11", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL"),
                      checkboxGroupInput("selCatExploreFewLevels12", label="selCatFewLevels2",
                                         choiceNames = list("init3", "init4"), 
                                         choiceValues = list("init3", "init4"),
                                         selected="ALL")
                      
               ),
               column(4,
                      tags$p("범주형 변수"),
                      actionButton("selCatExplore1", catVarWithModal[1]),
                      tags$p(""),
                      actionButton("selCatExplore2", catVarWithModal[2]),
                      tags$p(""),
                      actionButton("selCatExplore3", catVarWithModal[3]),
                      tags$p(""),
                      actionButton("selCatExplore4", catVarWithModal[4]),
                      tags$p(""),
                      actionButton("selCatExplore5", catVarWithModal[5]),
                      tags$p(""),
                      actionButton("selCatExplore6", catVarWithModal[6]),
                      tags$p(""),
                      actionButton("selCatExplore7", catVarWithModal[7]),
                      tags$p(""),
                      actionButton("selCatExplore8", catVarWithModal[8]),
                      tags$p(""),
                      actionButton("selCatExplore9", catVarWithModal[9]),
                      tags$p(""),
                      actionButton("selCatExplore10", catVarWithModal[10])
                      
               )
      )
    # )

}

revampDomainUI_2 <- function(input,output, session) {
  for(i in 1:10) {
    inVarName <- paste0("selCatExplore",i) # 변수명
    hide(inVarName)
  }
  
  if(length(catVarWithModal)<11) {
    seqNumber <- seq_along(catVarWithModal)
  } else {
    seqNumber <- 1:10
  }
  
  for(i in seqNumber) {
    inVarName <- paste0("selCatExplore",i) # 변수명
    html(inVarName, attr(curSampleExplore[,catVarWithModal[i]],"labelShort") )
    show(inVarName)
  }
  
  for(i in 1:12) {
    inVarName <- paste0("selCatExploreFewLevels",i) # 변수명
    hide(inVarName)
  }


  for(i in seq_along(catVarWithoutModal)) {
    # i<-1
    inVarName <- paste0("selCatExploreFewLevels",i) # 변수명
    itemList <- as.character(selCatDomainExplore[[catVarWithoutModal[i]]])
    itemList <- itemList[order(itemList)]
    # html(inVarName, catVarWithModal[i] )
    # browser()
    updateCheckboxGroupInput(session, inVarName,
                             label = attr(curSampleExplore[,catVarWithoutModal[i]],"labelShort"),
                             choices = itemList,
                             selected = itemList
    )
    show(inVarName)
  }
  
}


### UI에 표시된 내용을 MinDomainExplore, MaxDomainExplore, selCatDomainExplore에 입력 ###
getDomainExplore <- function(input, output, session)  {
  
  subFunc <- function(DomainTableNames, tableNo) {
    for(i in 1:length(DomainTableNames)) {
      var <- DomainTableNames[i]
      inVar <- paste0("minDomTable", tableNo, "Cell",i)
      MinDomainExplore[var] <<- input[[inVar]]
      inVar <- paste0("maxDomTable", tableNo, "Cell",i)
      MaxDomainExplore[var] <<- input[[inVar]]
    }
  }
  subFunc(DomainTable1Names, 1)
  subFunc(DomainTable2Names, 2)
  subFunc(DomainTable3Names, 3)

  # Modal을 이용하지 않고 Category를 선택하는 변수

  for(i in seq_along(catVarWithoutModal)) {
      var <- catVarWithoutModal[i]
      inVarName <- paste0("selCatExploreFewLevels",i) # 변수명
      selCatDomainExplore[[var]] <<- input[[inVarName]]
  }

  return() 
}


### 파일 등에서 읽은 값을 UI에 표시 ###
updateDomainUI <- function(input,output, session) {
  
  subFunc <- function(DomainTableNames, tableNo) {
    for(i in 1:length(DomainTableNames)) {
      var <- DomainTableNames[i]
      inVar <- paste0("minDomTable",tableNo, "Cell",i)   # 변수값
      value <- as.vector(MinDomainExplore[var])
      updateNumericInput(session, inVar, value=value)
      inVar <- paste0("maxDomTable", tableNo, "Cell",i)   # 변수값
      value <- as.vector(MaxDomainExplore[var])
      updateNumericInput(session, inVar, value=value)
    }
  } 
  
  subFunc(DomainTable1Names, 1)
  subFunc(DomainTable2Names, 2)
  subFunc(DomainTable3Names, 3)
}

### Source 도메인의 상하한값을 UI에 표시 ###
updateDomainUIFromSourceDomain<- function(input,output, session, bClear=FALSE) {

  subFunc <- function(DomainTableNames, minDomTableCell, maxDomTableCell) {
    if(is.null(DomainTableNames)) return()
    print(DomainTableNames)
    if(length(DomainTableNames)==1) {
      df <- as.data.frame(DFSource[,DomainTableNames])
      colnames(df) <- DomainTableNames[1]
    } else {
      df <- DFSource[,DomainTableNames]
    }


    func1 <- function(x) {
      # print(x)
      # if(x=="thick") browser()
      if(bClear) {
        return(NA)
      } else {
        min(df[,x], na.rm=TRUE)
      }
    }
    minVec <- vapply(DomainTableNames, func1, FUN.VALUE=numeric(1))
    func1 <- function(x) {
      max(df[,x], na.rm=TRUE)
    }
    maxVec <- vapply(DomainTableNames, func1, FUN.VALUE=numeric(1))
    for(i in 1:length(DomainTableNames)) {
      var <- DomainTableNames[i]
      decimal <- nchar(as.character(maxVec[var])) - nchar(as.character(floor(maxVec[var])))
      stepInterval <- 1
      if (decimal > 0 ) {
        decimal <- decimal - 1
        stepInterval <- 10 ^ -decimal
      }
      inVar <- paste0(minDomTableCell,i)   # 변수값
      value <- round(as.vector(minVec[var]), decimal)
      updateNumericInput(session, inVar, value=value, min = as.vector(minVec[var]), max = as.vector(maxVec[var]),
                         step = stepInterval)
      inVar <- paste0(maxDomTableCell,i)   # 변수값
      value <- round(as.vector(maxVec[var]), decimal)
      updateNumericInput(session, inVar, value=value, min = as.vector(minVec[var]), max = as.vector(maxVec[var]),
                         step = stepInterval)
    }
  }
  
  subFunc(DomainTable1Names, "minDomTable1Cell", "maxDomTable1Cell")
  subFunc(DomainTable2Names, "minDomTable2Cell", "maxDomTable2Cell")
  subFunc(DomainTable3Names, "minDomTable3Cell", "maxDomTable3Cell")
    
}

# Modal에서 선택된  범주형 변수의 레벨을 selCatDomainExplore에 저장
updateCatVarSample <- function(input, output, session) {

  subFunc <- function(buttonNo) {
    curSelCatVar <<- catVarWithModal[buttonNo]
    strVec <- as.character(unique(curSampleExplore[,curSelCatVar]))
    strVec <- sort(strVec)
    selCatDomainExplore[[curSelCatVar]] <- c("ALL",strVec)
    choiceNames <- unique(selCatDomainExplore[[curSelCatVar]])
    choiceValues <- unique(selCatDomainExplore[[curSelCatVar]])
    

    showModal(ModalCheckboxGroup(title=curSelCatVar, modalCheckboxID="ModalSelCatVarExplore", label="범주 선정", 
                                         choiceNames=choiceNames, choiceValues=choiceValues,selected=NULL, 
                                         modalOKButtonID="okModalSelCatVarExplore"))
  }
  
  observeEvent(input$selCatExplore1, {  subFunc(1) })
  observeEvent(input$selCatExplore2, {  subFunc(2) })
  observeEvent(input$selCatExplore3, {  subFunc(3) })
  observeEvent(input$selCatExplore4, {  subFunc(4) })
  observeEvent(input$selCatExplore5, {  subFunc(5) })
  observeEvent(input$selCatExplore6, {  subFunc(6) })
  observeEvent(input$selCatExplore7, {  subFunc(7) })
  observeEvent(input$selCatExplore8, {  subFunc(8) })

  observeEvent(input$okModalSelCatVarExplore, {
    # print(paste0(" okModalSelCatVarExplore: curSelCatVar : ", curSelCatVar))
    # print(paste0(" okModalSelCatVarExplore: selCat : ", input[["ModalSelCatVarExplore"]]))
    if(is.null(input[["ModalSelCatVarExplore"]])) {
      alert("선택된 checkbox가 없습니다.  취소하려면 취소 버튼을 누르세요.")
    } else {
      selCatDomainExplore[[curSelCatVar]] <<- input[["ModalSelCatVarExplore"]]
      print(paste0(" okModalSelCatVarExplore: selCatDomainExplore : ", selCatDomainExplore[[curSelCatVar]]))
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

