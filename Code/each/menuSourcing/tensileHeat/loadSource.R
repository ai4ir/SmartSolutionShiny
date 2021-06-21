
############################################ tensile ##############################
loadSource <- function() {

  DFSource <<- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/tensileHeat_전처리.rds")  
  chosenDFSourceFile <<- "tensileHeat"
  

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/tensileHeat/output"

  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("each/menuSourcing/tensile/Rmd/weldsSeah.Rmd",
                                  "common/Rmd/commonDescriptiveReport.Rmd"
  )
  outputFileNamesSourcingReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      # "commonDescriptiveReport.html")
  outputFileFinalNamesSourcingReport <<- c("인장 품질(Sourcing).html")   
                                      # "범용 기술통계.html")
  
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("each/menuSourcing/tensile/Rmd/weldsSeah.Rmd",
                                  "each/menuSourcing/tensile/Rmd/SamplingReport2.Rmd",
                                  "each/menuSourcing/tensile/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesSamplingReport <<- c("tensile.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesSamplingReport <<- c("인장 품질(Sampling).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  DomainTable1Names <<- c("C", "Si","Mn","P","S", "Nb", "V", "Ti", "Cu", "Ni", "Cr","Mo")
  DomainTable1NamesLabel <<- c("C(%)", "Si(%)","Mn(%)","P(ppm)","S(ppm)", "Nb(%)", "V(%)", "Ti(%)", "Cu(%)", "Ni(%)", "Cr(%)","Mo(%)")
  DomainTable2Names <<- NULL
  DomainTable2NamesLabel <<-  NULL
  DomainTable3Names <<- NULL
  DomainTable3NamesLabel <<- NULL

  pathNameFileDomain <<- "../USER/tensileHeat/Domain"
  pathNameFileDesign <<- "../USER/tensileHeat/Design"
  pathNameFileNewModel <<- "../Model/tensile"


  # dateVarNames <<- NULL
  dateVarNames <<- c("DecisionDate")
  
#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수

  
  catVarWithoutModal <<- c( "TensileShape" ,"TensileSpecimenDirection", "Factory", "ManufacturingProcess", "RollingMode", 
                            "grCCC","grCCC2",  "RollingModeDirection", "SRT_FurnanceNo", "RollingMeasureCode" )
  catVarWithModal <<- c("CCMachine", "ChemicalCompositionCode", "MslabCharicteristic", "TensileShapeCode")
  dfDomainCatExplore <- list(  "TensileShape" ,"TensileSpecimenDirection", "Factory", "ManufacturingProcess", "RollingMode",
                               "grCCC", "grCCC2",   "RollingModeDirection", "SRT_FurnanceNo", "RollingMeasureCode", 
                               "CCMachine", "ChemicalCompositionCode", "MslabCharicteristic", "TensileShapeCode")
  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)

  names(dfDomainCatExplore) <- catVarNameExplore

  # for(i in seq_along(catVarNameExplore)) {
  #   if(is.factor(DFSource[,catVarNameExplore[i]][[1]])) {
  #     dfDomainCatExplore[[catVarNameExplore[i]]] <-
  #       attr(DFSource[,catVarNameExplore[i]][[1]], "levels")
  #   } else {
  #     dfDomainCatExplore[[catVarNameExplore[i]]] <-
  #       as.character(unique(as.data.frame(DFSource)[,catVarNameExplore[1]]))
  #   }
  # }
  
  dfDomainCatExplore <- resetLevelsCatVar(DFSource, dfDomainCatExplore, catVarNameExplore)

  for(x in catVarWithModal) {
    strVec <- as.character(unique(as.data.frame(DFSource)[,x]))
    strVec <- sort(strVec)
    dfDomainCatExplore[[x]] <- c("ALL",strVec)
  }
  
  selCatDomainExplore <<- dfDomainCatExplore
  selCatDomainExploreInit <<- dfDomainCatExplore
  
  ############## Modeling 관련 고유 정보 ################  
  # pathModel <<- "../Model/tensile"
  

}

