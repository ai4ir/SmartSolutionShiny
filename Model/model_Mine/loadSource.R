############################################ salesRecord ##############################
loadSource <- function() {
  ############## Modeling 관련 고유 정보 ################  
  pathModel <<- "../Model/model_Mine/"
  load(file=paste(pathModel,  "/모델.Rdata", sep=""))
  dfModelNest <<- dfModelNest
  curSelModel <<- dfModelNest[["model"]][[1]]
  curSelModelY <<- dfModelNest[["modelY"]]
  # df <- dfModelNest
  # df <- unnest(df, data)
  df <- dfModelNest[["data"]][[1]]
  df <- df %>% add_predictions(curSelModel, var="predTop_YP")
  # df <- curSelModel[["model"]]
  # predYS1 <- curSelModel[["fitted.values"]]
  df <- df %>% mutate(predVal=0)
  
  df <- as.data.frame(df)
  
  attr(df[,"bHOT"], "label") <- "bHOT" ; 
  attr(df[,"bHOT"], "labelShort") <- "bHOT" ; 
  attr(df[,"predVal"], "label") <- "신규 예측 YS(MPa)" ; 
  attr(df[,"predVal"], "labelShort") <- "신규 예측 YS" ; 
  attr(df[,"predTop_YP"], "label") <- "예측 YS(MPa)" ; 
  attr(df[,"predTop_YP"], "labelShort") <- "예측 YS" ; 
  attr(df[,"sampleCode"], "label") <- "sampleCode" ; 
  attr(df[,"sampleCode"], "labelShort") <- "샘플 코드" ; 
  attr(df[,"rowNoSource"], "label") <- "rowNoSource" ; 
  attr(df[,"rowNoSource"], "labelShort") <- "rowNoSource" ; 


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

  # attr(df[,"YS"][["YS"]], "label") <- "항복강도[MPa]"  ### tibble에서 사용
  # attr(df[,"thick"], "label") <- "두께[mm]" ; attr(df[,"thick"], "digit") <- 2       
  # attr(df[,"width"], "label") <- "폭[mm]" ;          
  # attr(df[,"C"], "label") <- "C[%]" ;                
  # attr(df[,"Si"], "label") <- "Si[%]" ;                
  # attr(df[,"Mn"], "label") <- "Mn[%]" ;                
  # attr(df[,"YS"], "label") <- "측정 항복강도[MPa]";       
  # attr(df[,"TS"], "label") <- "측정 인장강도[MPa]"; 
  # attr(df[,"Al"], "label") <- "Al[%]"; attr(df[,"Al"], "digit") <- 3
  # attr(df[,"predVal"], "label") <- "예측 항복강도[MPa]"; attr(df[,"predVal"], "digit") <- 1
  
  df <- sticky_all(df)
  DFSource <<- df

  selModel <<- dfModelNest[["model"]][[1]]
  
  chosenDFSourceFile <<- "Top_YP"

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/myModel/output"

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
  outputFileFinalNamesSamplingReport <<- c("HSLA low C인장 품질(Sampling).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  DomainTable1Names <<- c("C", "Si","Mn","P","S","Cr","Ni","Mo","Cu","Al","Nb")
  DomainTable1NamesLabel <<- c("C(%)", "Si(%)","Mn(%)","P(ppm)","S(ppm)","Cr(%)","Ni(%)",
                               "Mo(%)","Cu(%)","Al(%)","Nb(%)")
  DomainTable2Names <<- c("Ti","V","B", "thick")
  DomainTable2NamesLabel <<- c("Ti(%)","V(%)","B(ppm)","thick(mm)")
  DomainTable3Names <<- NULL
  DomainTable3NamesLabel <<- NULL

  pathNameFileDomain <<- "../USER/myModel/Domain"
  pathNameFileDesign <<- "../USER/myModel/Design"
  pathNameFileNewModel <<- "../Model/myModel"


  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  catVarWithoutModal <<- c("MANUFACTURING_PROCESS")
  catVarWithModal <<- NULL
  # catVarWithoutModal <<- c("FACTORY")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수

  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- list("MANUFACTURING_PROCESS")
  names(dfDomainCatExplore) <- catVarNameExplore
  
  selCatDomainExplore <<-renderDFDomainCat(dfDomainCatExplore, catVarNameExplore,catVarWithModal, DFSource)

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
  DesignTable1Names <<- c("C","Mn","Nb", "Ti")
  DesignTable1NamesLabel <<- c("C(%)","Mn(%)","Nb(%)", "Ti(%)")
  DesignTable2Names <<- NULL
  DesignTable2NamesLabel <<- NULL
  DesignTable3Names <<- NULL
  DesignTable3NamesLabel <<- NULL
  
  pathNameFileDesign <<- "../USER/YS1/Design"
  pathNameFileDomain <<- "../USER/YS1/Domain"
  pathNameFileNewModel <<- "../Model/YS1"
  
  
  # dateVarNames <<- NULL
  dateVarNames <<- c("date")
  
 
  ############## dfDesignCat 계산 ################
  catVarWithoutModal <<- c("MANUFACTURING_PROCESS")
  catVarWithModal <<- NULL
  # catVarWithoutModal <<- c("FACTORY")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNamePredict <- c(catVarWithoutModalPredict, catVarWithModalPredict)
  dfDesignCatPredict <- list("MANUFACTURING_PROCESS")
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
  OptimizeTable1Names <<- c("C","Mn","Nb", "Ti")
  OptimizeTable1NamesLabel <<- c("C(%)","Mn(%)","Nb(%)", "Ti(%)")
  OptimizeTable2Names <<- NULL
  OptimizeTable2NamesLabel <<- NULL
  OptimizeTable3Names <<- NULL
  OptimizeTable3NamesLabel <<- NULL
  
  pathNameFileOptimize <<- "../USER/YS1/Optimize"
  
  
  
  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
  
  ############## dfOptimizeCat 계산 ################
  catVarWithoutModalOptimize <<- NULL
  catVarWithModalOptimize <<- NULL
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNameOptimize <- c(catVarWithoutModalOptimize, catVarWithModalOptimize)
  dfOptimizeCatOptimize <- list("customer", "spec")
  names(dfOptimizeCatOptimize) <- catVarNameOptimize
  
  selCatOptimizeOptimize <<- renderDFDomainCat(dfOptimizeCatOptimize, catVarNameOptimize,
                                               catVarWithModalOptimize, DFSource, bAddAll=FALSE)
  

}


renderStrPred <- function(df) {
  if(is.null(df[1,"predVal"])) {
    strPred <- "예측값 갱신 버튼을 누르면 예측값이 나옵니다."
    return(strPred)
  }
  
  strPred <- paste0("예측 항복강도(MPa) : ", round(curDesignDF[1,"predVal"],2))
  return(strPred)
  
}