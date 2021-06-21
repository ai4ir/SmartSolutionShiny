############################################ CoilKWTensile ##############################
loadSource <- function() {
  DFSource <<- read_rds("../SourceData/diamonds/diamonds_전처리.rds")
  
  # DFSource <<- diamonds
  # DFSource <<- select(DFSource, -sampleCode, -bHOT, -clusterGr)
  # DFSource$carb <- as.factor(DFSource$carb)
  # write_rds(DFSource, "../SourceData/diamonds/diamonds_전처리.rds")
  
  chosenDFSourceFile <<- "diamonds"
  


  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/diamonds/output"
  
  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("each/menuSourcing/diamonds/Rmd/SourcingReport1.Rmd",
                                  "each/menuSourcing/diamonds/Rmd/SourcingReport2.Rmd"
  )
  outputFileNamesSourcingReport <<- c("SourcingReport1.html",   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      "SourcingReport2.html")
  outputFileFinalNamesSourcingReport <<- c("diamonds 리포트1.html",   
                                           "diamonds 리포트2.html")
  
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("each/menuSourcing/diamonds/Rmd/SamplingReport1.Rmd"
  )
  outputFileNamesSamplingReport <<- c("SamplingReport1.html")
    ############## Source별 고유 정보 ################
  DomainTable1Names <<- c("price","carat","x","y","z","depth")
  DomainTable1NamesLabel <<- c("Dollar","캐럿","x(mm)","y(mm","z(mm)", "형상비")
  DomainTable2Names <<- NULL
  DomainTable2NamesLabel <<- NULL
  DomainTable3Names <<- NULL
  DomainTable3NamesLabel <<- NULL
  # chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B",  "N2", "Ca")
  # chemCompNameLabel <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb", "Ti","SolAl", "B(ppm)", "N2(ppm)",  "Ca(ppm)")
  
  pathNameFileDomain <<- "../USER/diamonds/Domain"
  pathNameFileDesign <<- "../USER/diamonds/Design"
  pathNameFileNewModel <<- "../Model/diamonds"

#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  catVarWithoutModal <<- c("cut")  # Modal을 이용하지 않고 Category를 선택하는 변수
  catVarWithModal <<- c("color", "clarity")  # Modal을 이용하여 Category를 선택하는 변수

  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- list("cut", "color", "clarity" )
  names(dfDomainCatExplore) <- catVarNameExplore

  for(i in 1:NROW(catVarNameExplore)) {
    if(is.factor(DFSource[,catVarNameExplore[i]][[1]])) {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        attr(diamonds[,catVarNameExplore[i]][[1]], "levels")
    } else {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        as.character(unique(DFSource[,catVarNameExplore[i]]))
    }

  }

  selCatDomainExplore <<- dfDomainCatExplore
  selCatDomainExploreInit <<- dfDomainCatExplore

}

