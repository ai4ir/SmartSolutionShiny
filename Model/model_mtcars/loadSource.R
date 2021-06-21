############################################ model_AR_CMn_TS ##############################
loadSource <- function() {
  ############## Modeling 관련 고유 정보 ################  
  # alert("모델 개발중입니다. 임시 모델을 읽어 옵니다.")
  pathModel <<- "../Model/model_mtcars"                   ### for model developer
  load(file=paste(pathModel,  "/모델.Rdata", sep=""))
  dfModelNest <<- dfModelNest
  curSelModel <<- dfModelNest[["model"]][[1]]
  curSelModelY <<- dfModelNest[["modelY"]]
  # df <- dfModelNest
  # df <- unnest(df, data)
  df <- dfModelNest[["data"]][[1]]
  df <- df %>% add_predictions(curSelModel, var="predMPG")     ### for model developer
  # df <- curSelModel[["model"]]
  # predYS1 <- curSelModel[["fitted.values"]]

  
  df <- as.data.frame(df)
  
  ### for model developer
  df <- attachAttrOneVar(df, var="predMPG", label="예측 MPG", labelShort="예측 MPG", digit=1) 
  ### for model developer
  df <- attachAttrOneVar(df, var="bHOT", label="bHOT", labelShort="bHOT")
  df <- attachAttrOneVar(df, var="sampleCode", label="sampleCode", labelShort="샘플 코드")
  df <- attachAttrOneVar(df, var="rowNoSource", label="rowNoSource", labelShort="rowNoSource 코드")



  numVarConst <- extractNumVarNameAndConst(df)
  decimalVec <- vapply(numVarConst, renderDigitVector,  FUN.VALUE=numeric(1), df)
  decimalVec <- as.vector(decimalVec)
  for(x in numVarConst) {
    # numVarConst <- "thick"
    attr(df[,x], "max") <- max(df[,x], na.rm=TRUE)
    attr(df[,x], "mean") <- mean(df[,x], na.rm=TRUE)
    attr(df[,x], "min") <- min(df[,x], na.rm=TRUE)
    attr(df[,x], "digit") <- decimalVec[which(numVarConst==x)]
  }


  df <- sticky_all(df)
  DFSource <<- df

  selModel <<- dfModelNest[["model"]][[1]]
  
  chosenDFSourceFile <<- "model_mtcars"       ### for model developer
  

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/model_mtcars/output"      ### for model developer

  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("sourcing/tensileHSLAlowC/Rmd/weldsSeah.Rmd",
                                  "Rmd/commonDescriptiveReport.Rmd"
  )
  outputFileNamesSourcingReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      # "commonDescriptiveReport.html")
  outputFileFinalNamesSourcingReport <<- c("HSLA low C인장 품질(Sourcing).html")   
                                      # "범용 기술통계.html")
  
  ############## Sampling 관련 고유 정보 ################
  ############## Sampling 관련 고유 정보 ################
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("sourcing/tensileHSLAlowC/Rmd/weldsSeah.Rmd",
                                  "sourcing/tensile/Rmd/SamplingReport2.Rmd",
                                  "sourcing/tensile/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesSamplingReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesSamplingReport <<- c("model_mtcars MPG(Sampling).html")   ### for model developer
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  # DomainTable1Names <<- c("C", "Si","Mn","P","S","Cr","Ni","Mo","Cu","Al","Nb")
  # DomainTable1NamesLabel <<- c("C(%)", "Si(%)","Mn(%)","P(ppm)","S(ppm)","Cr(%)","Ni(%)",
  #                              "Mo(%)","Cu(%)","Al(%)","Nb(%)")
  # DomainTable2Names <<- c("Ti","V","B", "thick")
  # DomainTable2NamesLabel <<- c("Ti(%)","V(%)","B(ppm)","thick(mm)")
  
  DomainTable1Names <<- c("wt","am","gear","hp")                         ### for model developer
  DomainTable1NamesLabel <<- c("wt","am","gear","hp")     ### for model developer
  DomainTable2Names <<- c("mpg")                         ### for model developer
  DomainTable2NamesLabel <<-c("mpg")                       ### for model developer
  DomainTable3Names <<- c("rStandard", "cooksDist")                       ### for model developer
  DomainTable3NamesLabel <<- c("rStandard", "cooksDist")                  ### for model developer

  pathNameFileDomain <<- "../USER/model_mtcars/Domain"              ### for model developer
  pathNameFileDesign <<- "../USER/model_mtcars/Design"              ### for model developer
  pathNameFileNewModel <<- "../Model/model_mtcars"                  ### for model developer 


  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  catVarWithoutModal <<- NULL                       ### for model developer
  catVarWithModal <<- NULL                                                ### for model developer 
  # catVarWithoutModal <<- c("FACTORY")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수

  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- render_dfDomainCatExplore(catVarNameExplore)
  names(dfDomainCatExplore) <- catVarNameExplore
  
  # 5월 14일 수정 # 
  selCatDomainExplore <<-renderDFDomainCat(dfDomainCatExplore, catVarNameExplore,catVarWithModal, DFSource)
  selCatDomainExploreInit <<- selCatDomainExplore

  ############## Modeling 관련 고유 정보 ################  
  ############## Modeling 관련 고유 정보 ################  
  ############## Modeling 관련 고유 정보 ################  
  # pathModel <<- "../Model/tensileHSLAlowC"
  
  ############## Predict 관련 고유 정보 ################
  ############## Predict 관련 고유 정보 ################
  ############## Predict 관련 고유 정보 ################
  pathFileRmdPredictReport <<- c("sourcing/tensileHSLAlowC/Rmd/weldsSeah.Rmd",
                                  "sourcing/tensileHSLA/Rmd/SamplingReport2.Rmd",
                                  "sourcing/tensileHSLA/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesPredictReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesPredictReport <<- c("HSLA low C 인장 예측(Predict).html")   
  # "범용 기술통계.html")
  
  # DesignTable1Names <<- NULL
  DesignTable1Names <<- c("wt","am","gear","hp")                         ### for model developer
  DesignTable1NamesLabel <<- c("wt","am","gear","hp")    ### for model developer
  DesignTable2Names <<- NULL                        ### for model developer
  DesignTable2NamesLabel <<- NULL                    ### for model developer
  DesignTable3Names <<- NULL                                             ### for model developer
  DesignTable3NamesLabel <<- NULL                                        ### for model developer
  
  pathNameFileDesign <<- "../USER/model_mtcars/Design"             ### for model developer
  pathNameFileDomain <<- "../USER/model_mtcars/Domain"             ### for model developer  
  pathNameFileNewModel <<- "../Model/model_mtcars"                 ### for model developer
  
  
  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
 
  ############## dfDesignCat 계산 ################
  catVarWithoutModal <<- NULL                      ### for model developer
  catVarWithModal <<- NULL                                               ### for model developer
  # catVarWithoutModal <<- c("FACTORY")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNamePredict <- c(catVarWithoutModalPredict, catVarWithModalPredict)
  dfDesignCatPredict <- list("ManufacturingProcess")                     ### for model developer
  names(dfDesignCatPredict) <- catVarNamePredict
  
  selCatDesignPredict <<-renderDFDomainCat(dfDesignCatPredict, catVarNamePredict,catVarWithModalPredict,
                                           DFSource, bAddAll=FALSE)
  

  ############## Optimize 관련 고유 정보 ################
  pathFileRmdOptimizeReport <<- c("sourcing/tensileHSLAlowC/Rmd/weldsSeah.Rmd",
                                 "sourcing/tensileHSLA/Rmd/SamplingReport2.Rmd",
                                 "sourcing/tensileHSLA/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesOptimizeReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesOptimizeReport <<- c("HSLA low C 인장 예측(Optimize).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  OptimizeTable1Names <<- c("wt","am","gear","hp")                         ### for model developer
  OptimizeTable1NamesLabel <<- c("wt","am","gear","hp")      ### for model developer
  OptimizeTable2Names <<- NULL                         ### for model developer
  OptimizeTable2NamesLabel <<- NULL                     ### for model developer
  OptimizeTable3Names <<- NULL                                               ### for model developer
  OptimizeTable3NamesLabel <<- NULL                                          ### for model developer
  
  pathNameFileOptimize <<- "../USER/AR/Optimize/"                            ### for model developer
  
  
  
  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
  
  ############## dfOptimizeCat 계산 ################
  catVarWithoutModalOptimize <<- NULL                                        ### for model developer
  catVarWithModalOptimize <<- NULL                                           ### for model developer
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNameOptimize <- c(catVarWithoutModalOptimize, catVarWithModalOptimize)
  dfOptimizeCatOptimize <- list()
  names(dfOptimizeCatOptimize) <- catVarNameOptimize
  
  selCatOptimizeOptimize <<- renderDFDomainCat(dfOptimizeCatOptimize, catVarNameOptimize,
                                               catVarWithModalOptimize, DFSource, bAddAll=FALSE)
  

}


renderStrPred <- function(predVal) {
  if(is.null(predVal)) {
    strPred <- "예측값 갱신 버튼을 누르면 예측값이 나옵니다."
    return(strPred)
  }
  
  strPred <- paste0("예측 MPG : ", round(predVal,2))        ### for model developer
  return(strPred)
  
}


renderStrFailRatio <- function(predVal) {

  std <- 12                                                           ### for model developer
  
  modelY <- dfModelNest[["modelY"]][[1]]
  
  if(is.null(MinReqExplore[modelY])) {
    failPercentUnderReq <- 0.0
  } else {
    zMin <- (MinReqExplore[modelY] - predVal) / 12
    failPercentUnderReq <- round(100 * pnorm(zMin),2)
  }
  
  if(is.null(MaxReqExplore[modelY])) {
    failPercentOverReq <- 0.0
  } else {
    zMax <- (MaxReqExplore[modelY] - predVal) / 12
    failPercentOverReq <- round(100 * pnorm(zMax, lower.tail=FALSE),2)
  }
  percentInside <- round(100 - failPercentUnderReq - failPercentOverReq, 2)
  strFailRatio <- paste0(failPercentUnderReq,"%, ", MinReqExplore[modelY], "MPG, ",    ### for model developer
                         percentInside, "%, ", MaxReqExplore[modelY], "MPG, ",         ### for model developer
                         failPercentOverReq, "%"
  )
  
  return(strFailRatio)
  
}