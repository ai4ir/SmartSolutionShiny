############################################ mtcars ##############################
loadSource <- function() {
  ############## Modeling 관련 고유 정보 ################  
  pathModel <<- "../Model/mtcars"
  load(file=paste(pathModel,  "/모델.Rdata", sep=""))
  dfModelNest <<- dfModelNest
  curSelModel <<- dfModelNest[["model"]][[1]]
  # df <- dfModelNest
  # df <- unnest(df, data)
  df <- dfModelNest[["data"]][[1]]
  df <- df %>% add_predictions(curSelModel, var="predMPG")
  df <- df %>% mutate(predVal=0)
  
  df <- as.data.frame(df)
  
  attr(df[,"bHOT"], "label") <- "bHOT" ; 
  attr(df[,"bHOT"], "labelShort") <- "bHOT" ; 
  attr(df[,"predVal"], "label") <- "신규 예측 연비(mpg)" ; 
  attr(df[,"predVal"], "labelShort") <- "신규 예측 연비" ; 
  attr(df[,"predMPG"], "label") <- "예측 연비(mpg)" ; 
  attr(df[,"predMPG"], "labelShort") <- "예측 연비" ; 
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
  
  chosenDFSourceFile <<- "mtcarsModel"
  

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/mtcarsModel/output"
  
  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("each/sourcing/mtcarsModel/Rmd/SourcingReport1.Rmd",
                                  "each/sourcing/mtcarsModel/Rmd/SourcingReport2.Rmd"
  )

  outputFileNamesSourcingReport <<- c("SourcingReport1.html",   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      "SourcingReport2.html")
  outputFileFinalNamesSourcingReport <<- c("mtcars 리포트1.html",   
                                           "mtcars 리포트2.html")
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("each/sourcing/mtcarsModel/Rmd/SamplingReport1.Rmd",
                                  "each/sourcing/mtcarsModel/Rmd/SamplingReport2.Rmd",
                                  "each/sourcing/mtcarsModel/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesSamplingReport <<- c("SamplingReport1.html",
                                      "SamplingReport2.html",
                                      "SamplingReportTable.html")

  DomainTable1Names <<- c("mpg","cyl","disp","hp","wt")
  DomainTable1NamesLabel <<- c("Miles/G","No Cylinder","배기량","마력","무게")
  DomainTable2Names <<- NULL
  DomainTable2NamesLabel <<- NULL
  DomainTable3Names <<- NULL
  DomainTable3NamesLabel <<- NULL
  # chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B",  "N2", "Ca")
  # chemCompNameLabel <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb", "Ti","SolAl", "B(ppm)", "N2(ppm)",  "Ca(ppm)")
  
  pathNameFileDomain <<- "../USER/mtcars/Domain"
  pathNameFileDesign <<- "../USER/mtcars/Design"
  pathNameFileNewModel <<- "../Model/mtcars"


#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  catVarWithoutModal <<- c("am", "vs")  # Modal을 이용하지 않고 Category를 선택하는 변수
  catVarWithModal <<- c("gear", "carb")  # Modal을 이용하여 Category를 선택하는 변수

  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- list("am", "vs", "gear", "carb" )
  names(dfDomainCatExplore) <- catVarNameExplore

  for(i in 1:NROW(catVarNameExplore)) {
    if(is.factor(DFSource[,catVarNameExplore[i]][[1]])) {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        attr(DFSource[,catVarNameExplore[i]][[1]], "levels")
    } else {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        as.character(unique(DFSource[,catVarNameExplore[i]]))
    }
  }

  selCatDomainExplore <<- dfDomainCatExplore
  
  ############## Modeling 관련 고유 정보 ################  
  ############## Modeling 관련 고유 정보 ################  
  ############## Modeling 관련 고유 정보 ################  
  pathModel <<- "../Model/mtcars"
  
  ############## Predict 관련 고유 정보 ################
  ############## Predict 관련 고유 정보 ################
  ############## Predict 관련 고유 정보 ################
  pathFileRmdPredictReport <<- c("each/sourcing/tensile/Rmd/weldsSeah.Rmd",
                                  "each/sourcing/tensile/Rmd/SamplingReport2.Rmd",
                                  "each/sourcing/tensile/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesPredictReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesPredictReport <<- c("인장 예측(Predict).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  DesignTable1Names <<- c("wt","hp")
  DesignTable1NamesLabel <<- c("무게","마력")
  DesignTable2Names <<- NULL
  DesignTable2NamesLabel <<- NULL
  DesignTable3Names <<- NULL
  DesignTable3NamesLabel <<- NULL
  
  pathNameFileDesign <<- "../USER/mtcarsModel/Design"
  pathNameFileDomain <<- "../USER/mtcarsModel/Domain"
  pathNameFileNewModel <<- "../Model/mtcars"
  
  
  
  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
  
  ############## dfDesignCat 계산 ################
  catVarWithoutModalPredict <<- NULL
  catVarWithModalPredict <<- c( "am", "gear")
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNamePredict <- c(catVarWithoutModalPredict, catVarWithModalPredict)
  dfDesignCatPredict <- list( "am", "gear")
  names(dfDesignCatPredict) <- catVarNamePredict
  
  for(i in seq_along(catVarNamePredict)) {
    if(is.factor(DFSource[,catVarNamePredict[i]][[1]])) {
      dfDesignCatPredict[[catVarNamePredict[i]]] <-
        attr(DFSource[,catVarNamePredict[i]][[1]], "levels")
    } else {
      dfDesignCatPredict[[catVarNamePredict[i]]] <-
        as.character(unique(as.data.frame(DFSource)[,catVarNamePredict[1]]))
    }
  }
  
  
  
  # for(x in catVarWithModalPredict) {
  #   strVec <- as.character(unique(as.data.frame(DFSource)[,x]))
  #   strVec <- sort(strVec)
  # }
  
  selCatDesignPredict <<- dfDesignCatPredict

}

renderStrPred <- function(df) {
  if(is.null(df[1,"predVal"])) {
    strPred <- "예측값 갱신 버튼을 누르면 예측값이 나옵니다."
    return(strPred)
  }
  
  strPred <- paste0("예측 연비(mpg) : ", round(curDesignDF[1,"predVal"],2))
  return(strPred)
  
}
