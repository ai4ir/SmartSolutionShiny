validateDF <- function(df, varCheckValid) {
  df <- sticky_all(df)
  selectBool <- rep(TRUE, dim(df)[[1]])

  for(var in varCheckValid) {
    if(is.numeric(df[,var])) {
      selectBool <- updateSelectBool(value=df[,var], min=attr(df[,var],"validMin"), max=attr(df[,var],"validMax"), initBool=selectBool)
      noTRUE <- length(selectBool[selectBool==TRUE])
      print(paste0("validateSample - varName : ", var, ",  noTRUE :", noTRUE, ", 
                 min:", attr(df[,var],"validMin"), ",  max: ", attr(df[,var],"validMax")))
    }

  }
  validDF <- df[selectBool==TRUE,]
}


updateSelectBool <- function(value, min, max, initBool) {
  if(is.na(min) & is.na(max)) { return(initBool) }
  if(is.na(min)) min <- -100000000
  if(is.na(max)) max <- 100000000

  curBool <- ifelse(is.na(value), FALSE, min <= value & value <= max )
  resultBool <- initBool & curBool
  return(resultBool)
}

updateSelectBoolCat <- function(value, selectedCat, initBool,varName) {
  value <- as.character(value)
  if ( "ALL" %in% selectedCat | length(selCatDomainExploreInit[[varName]])==length(selectedCat) ) {
    return(initBool)
  } else {
    curBool <-  ifelse(is.na(value), FALSE, value %in% selectedCat)
    resultBool <- initBool & curBool
    return(resultBool)
  }
  
}

mutateBoolSample <- function(input, output, session) {

  withProgress(message="샘플 진행중", value=0, {
    incProgress(0.2)
    getDomainExplore(input, output, session)
    
    selectBool <- rep(TRUE, nrow(curSampleExplore))
    
    # date 변수 Sampling
    
    for(i in seq_along(dateVarNames)) {
      varName <- dateVarNames[i]
      inVarName <- paste0("dateRange",i)
      value <- curSampleExplore[,varName]
      min <- input[[inVarName]][1]
      max <- input[[inVarName]][2]
      selectBool <- updateSelectBool(value=value, min=min, max=max, initBool=selectBool)
      noTRUE <- length(selectBool[selectBool==TRUE])
      print(paste0("trigerSample - varName : ", varName, ",  noTRUE :", noTRUE, ",  min:", min, ",  max: ", max))
    }
    
    
    # 연속형 변수
    for( varName in names(MinDomainExplore)) {
      # if(varName=="CoilT") browser()
      print(paste0("trigerSample - varName : ", varName))
      if(is.numeric(curSampleExplore[,varName])) {
        value <- curSampleExplore[,varName]
        min <- as.vector(MinDomainExplore[varName])
        max <- as.vector(MaxDomainExplore[varName])
        selectBool <- updateSelectBool(value=value, min=min, max=max, initBool=selectBool)
      }
      noTRUE <- length(selectBool[selectBool==TRUE])
      print(paste0("trigerSample - varName : ", varName, ",  noTRUE :", noTRUE, ",  min:", min, ",  max: ", max))
    }
    
    # 범주형 변수
    incProgress(0.4)
    
    
    for( varName in names(selCatDomainExplore)) {
      print(paste0("trigerSample - varName ", varName))
      value <- curSampleExplore[,varName]
      selectedCat <- selCatDomainExplore[[varName]]
      selectBool <- updateSelectBoolCat(value=value, selectedCat=selectedCat, initBool=selectBool,varName=varName)
      noTRUE <- length(selectBool[selectBool==TRUE])
      print(paste0("trigerSample - varName : ", varName, ",  noTRUE :", noTRUE))
    }
    incProgress(0.4)
    return(selectBool)
  })
  
}


replyButtonBisectArea <- function(input, output, session) {
  withProgress(message="양분화 진행중", value=0, {
    incProgress(0.2)
    selectBool <- mutateBoolSample(input, output, session)
    func1 <- function(x) {
      if(is.na(x)) {"NA"} 
      else if (x) {"inside"}
      else {"outside"}
    }
    clusterSamplingGUI <- vapply(selectBool, func1, FUN.VALUE=character(1))
    attr(clusterSamplingGUI, "label") <- "cluseter By Sampling GUI" ; 
    attr(clusterSamplingGUI, "labelShort") <- "clusterSamplingGUI" ;
    # if("clusterSamplingGUI" %in% colnames(curSampleExplore) ) {
    #   curSampleExplore <<- curSampleExplore %>% select(!clusterSamplingGUI)
    # }
    curSampleExplore <<- curSampleExplore %>% 
      mutate(clusterSamplingGUI = clusterSamplingGUI)
    aesList[["clusterMethod"]][1] <<- "clusterSamplingGUI"
    bHOT <- ifelse(clusterSamplingGUI=="inside","Hot", "Normal")
    # attr(bHOT, "label") <- "bHOT" ;   attr(bHOT, "labelShort") <- "bHOT" ; 
    curSampleExplore[,"bHOT"] <<- bHOT
    curSampleExplore <<- 
      attachAttrOneVar(curSampleExplore, var="bHOT", label="bHOT", labelShort="bHOT")
    curSampleExplore <<- sticky_all(curSampleExplore)
    

    # familyGangJong <- unique(curSampleExplore$GangJong)
    # curSampleExplore <<- DFSourceExplore[DFSourceExplore$GangJong %in% familyGangJong, ]
    
    curSampleExploreHot <<- curSampleExplore[clusterSamplingGUI=="Hot",]
    curSampleExploreNormal <<- curSampleExplore[!clusterSamplingGUI=="Hot",]
    incProgress(0.8)
  })

  noData <- NROW(curSampleExplore)
  alert(paste0("양분화가 완료되었습니다.  ",noData,"개의 데이타가 있습니다."))  

   # revampTTSE(input, output, session)
  
}

replyButtonDeleteArea <- function(input, output, session) {

  withProgress(message="버리기 진행중", value=0, {
    incProgress(0.2)
    selectBool <- mutateBoolSample(input, output, session)
    curSampleExplore <<- curSampleExplore[!selectBool,]
    incProgress(0.8)
  })
  
  noData <- NROW(curSampleExplore)
  alert(paste0("버리기가 완료되었습니다.  ",noData,"개의 데이타가 있습니다."))  
  
  # curSampleExploreEmbryo <- curSampleExplore[selectBool,]
  # familyGangjong <- unique(curSampleExploreEmbryo$GangJong)
  # curSampleExplore <<- curSampleExplore[!curSampleExplore$GangJong %in% familyGangjong, ]
  
  
  # curSampleExplore가 변화했으므로 반응성 촉발
  #reactCurSampleExplore(curSampleExplore)
  
  
  # GangJong <- unique(curSampleExplore[,"GangJong"])
  # strGangJong <- GangJong[1]
  # if (length(GangJong)>1) {
  #     for(i in 2:length(GangJong)) {
  #         strGangJong <- sprintf("%s %s", strGangJong,GangJong[i] )
  #     }
  # }
  # 
  # 
  # showModal(ModalTextOutput(failed=FALSE, strGangJong=strGangJong))

  
}

replyButtonSaveArea <- function(input, output, session) {
  withProgress(message="남기기 진행중", value=0, {
    incProgress(0.2)
    selectBool <- mutateBoolSample(input, output, session)
    curSampleExplore <<- curSampleExplore[selectBool,]
  #   
  #   # 
  #   # 
  #   # curSampleExploreEmbryo <- DFSourceExplore[selectBool,]
  #   # familyGangjong <- unique(curSampleExploreEmbryo$GangJong)
  #   # curSampleExplore <<- DFSourceExplore[DFSourceExplore$GangJong %in% familyGangjong, ]
  #   # if(nrow(curSampleExplore)==0) {
  #   #   alert("조건을 만족하는 데이타가 전혀 없습니다.  조건을 완화재 주세요")
  #   #   return()
  #   # }
  #   # 
  #   # selectBool <- rep(TRUE, nrow(curSampleExplore))
  #   # # 비확대 연속형 변수
  #   # 
  #   # for( varName in numVarExploreSamplingHard) {
  #   #   print(paste0("trigerSample 영역 축소 - varName ", varName))
  #   #   if(is.numeric(curSampleExplore[,varName])) {
  #   #     value <- curSampleExplore[,varName]
  #   #     min <- as.vector(MinDomainExplore[varName])
  #   #     max <- as.vector(MaxDomainExplore[varName])
  #   #     selectBool <- updateSelectBool(value=value, min=min, max=max, initBool=selectBool)
  #   #   }
  #   # }
  #   # 
  #   # # 범주형 변수는 전체 비확대
  #   # incProgress(0.6)
  #   # 
  #   # for( varName in names(selCatDomainExplore)) {
  #   #   print(paste0("trigerSample 영역 축소 - varName ", varName))
  #   #   value <- curSampleExplore[,varName]
  #   #   selectedCat <- selCatDomainExplore[[varName]]
  #   #   selectBool <- updateSelectBoolCat(value=value, selectedCat=selectedCat, initBool=selectBool)
  #   # }
  #   # curSampleExplore <<- curSampleExplore[selectBool,]
  #   # 
  #   # if(nrow(curSampleExplore)==0) {
  #   #   alert("조건을 만족하는 데이타가 전혀 없습니다.  조건을 완화재 주세요")
  #   #   return()
  #   # }
  #   # 
  #   # # curSampleExplore가 변화했으므로 반응성 촉발
  #   # incProgress(0.8)
  #   # 
  #   # reactCurSampleExplore(curSampleExplore)
  #   # 
  #   # 
  #   # GangJong <- unique(curSampleExplore[,"GangJong"])
  #   # GangJong <- GangJong[order(GangJong)]
  #   # 
  #   # # str 벡터를 하나의 string으로 통합
  #   # strGangJong <- GangJong[1]
  #   # if (length(GangJong)>1) {
  #   #   for(i in 2:length(GangJong)) {
  #   #     strGangJong <- sprintf("%s %s", strGangJong,GangJong[i] )
  #   #   }
  #   # }
  #   # 
  #   # showModal(ModalTextOutput(failed=FALSE, strGangJong=strGangJong))
    incProgress(0.8)
  })
  
  noData <- NROW(curSampleExplore)
  if(noData==0) {
    alert(paste0("남은 데이타가 0개입니다. Sourcing부터 다시 하십시요"))  
    
  } else {
    alert(paste0("남기기가 완료되었습니다.  ",noData,"개의 데이타가 있습니다."))  
  }
    

  
}