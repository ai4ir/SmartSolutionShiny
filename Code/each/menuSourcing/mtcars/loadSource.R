############################################ mtcars ##############################
loadSource <- function() {
  DFSource <<- read_rds("../SourceData/mtcars/mtcars_전처리.rds")
  
  # DFSource <<- select(DFSource, -sampleCode, -bHOT, -clusterGr)
  # DFSource$carb <- as.factor(DFSource$carb)
  # write_rds(DFSource, "../SourceData/mtcars/mtcars_전처리.rds")
  
  chosenDFSourceFile <<- "mtcars"


  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/mtcars/output"
  
  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("each/menuSourcing/mtcars/Rmd/SourcingReport1.Rmd",
                                  "each/menuSourcing/mtcars/Rmd/SourcingReport2.Rmd"
  )

  outputFileNamesSourcingReport <<- c("SourcingReport1.html",   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      "SourcingReport2.html")
  outputFileFinalNamesSourcingReport <<- c("mtcars 리포트1.html",   
                                           "mtcars 리포트2.html")
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("each/menuSourcing/mtcars/Rmd/SamplingReport1.Rmd",
                                  "each/menuSourcing/mtcars/Rmd/SamplingReport2.Rmd",
                                  "each/menuSourcing/mtcars/Rmd/SamplingReportTable.Rmd"
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
  pathFileRmdPredictReport <<- c("each/menuSourcing/tensile/Rmd/weldsSeah.Rmd",
                                  "each/menuSourcing/tensile/Rmd/SamplingReport2.Rmd",
                                  "each/menuSourcing/tensile/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesPredictReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesPredictReport <<- c("인장 예측(Predict).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  DomainTable1Names <<- c("mpg","disp","hp","wt")
  DomainTable1NamesLabel <<- c("Miles/G","배기량","마력","무게")
  DesignTable2Names <<- NULL
  DesignTable2NamesLabel <<- NULL
  DesignTable3Names <<- NULL
  DesignTable3NamesLabel <<- NULL
  
  pathNameFileDesign <<- "../USER/tensile/Design"
  
  
  
  dateVarNames <<- NULL
  # dateVarNames <<- c("date")
  
  
  ############## dfDesignCat 계산 ################
  catVarWithoutModalPredict <<- c("am", "vs")
  catVarWithModalPredict <<- c("cyl", "gear")
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  
  catVarNamePredict <- c(catVarWithoutModalPredict, catVarWithModalPredict)
  dfDesignCatPredict <- list("am", "vs", "cyl", "gear")
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
  
  
  
  for(x in catVarWithModalPredict) {
    strVec <- as.character(unique(as.data.frame(DFSource)[,x]))
    strVec <- sort(strVec)
  }
  
  selCatDesignPredict <<- dfDesignCatPredict
  


}

