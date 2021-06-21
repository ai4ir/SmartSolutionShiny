############################################ EH47BCA ##############################
loadSource <- function() {

  DFSource <<- read_rds("../SourceData/EH47BCA/210510 EH47BCA_NRL.v1 2_전처리.rds")  
  chosenDFSourceFile <<- "EH47BCA"
  

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/EH47BCA/output"

  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("each/menuSourcing/EH47BCA/Rmd/weldsSeah.Rmd",
                                  "common/Rmd/commonDescriptiveReport.Rmd"
  )
  outputFileNamesSourcingReport <<- c("EH47BCA.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      # "commonDescriptiveReport.html")
  outputFileFinalNamesSourcingReport <<- c("EH47BCA(Sourcing).html")   
                                      # "범용 기술통계.html")
  
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("each/menuSourcing/EH47BCA/Rmd/weldsSeah.Rmd",
                                  "each/menuSourcing/EH47BCA/Rmd/SamplingReport2.Rmd",
                                  "each/menuSourcing/EH47BCA/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesSamplingReport <<- c("EH47BCA.html")   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
  # "commonDescriptiveReport.html")
  outputFileFinalNamesSamplingReport <<- c("EH47BCA(Sampling).html")   
  # "범용 기술통계.html")
  
  # DomainTable1Names <<- NULL
  DomainTable1Names <<- c("C", "Si","Mn","P","S","Cr","Ni","Mo","Cu","Nb")
  DomainTable1NamesLabel <<- c("C(%)", "Si(%)","Mn(%)","P(ppm)","S(ppm)","Cr(%)","Ni(%)",
                               "Mo(%)","Cu(%)","Nb(%)")
  DomainTable2Names <<- c("PlateThickness", "PlateWidth", "PlateLength")
  DomainTable2NamesLabel <<-  c("두께(mm)", "폭(mm)", "길이(mm)" )
  DomainTable3Names <<- c("SRT_3rdZoneTemp", "SRT_HomogenousTime")
  DomainTable3NamesLabel <<- c("3가열대 온도", "균열대 시간")

  pathNameFileDomain <<- "../USER/EH47BCA/Domain"
  pathNameFileDesign <<- "../USER/EH47BCA/Design"
  pathNameFileNewModel <<- "../Model/EH47BCA"


  # dateVarNames <<- NULL
  dateVarNames <<- c("DecisionDate")
  
#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  # catVarWithoutModal <<- c("factory")  or NULL # Modal을 이용하지 않고 Category를 선택하는 변수
  # catVarWithModal <<- c("material") or NULL # Modal을 이용하여 Category를 선택하는 변수
  catVarWithoutModal <<- c( "TensileShape" ,"TensileSpecimenDirection", "Factory", "ManufaturingProcess",
                            "RollingMode", "RollingModeDirection", "SRT_FurnanceNo",
                            "RollingMeasureCode","NRL_FirstTest" )
  catVarWithModal <<- c("CCMachine", "MslabCharicteristic", "TensileShapeCode", "month")
  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  
  dfDomainCatExplore <- render_dfDomainCatExplore(catVarNameExplore)
  names(dfDomainCatExplore) <- catVarNameExplore
  dfDomainCatExplore <- resetLevelsCatVar(DFSource, dfDomainCatExplore, catVarNameExplore)

  for(x in catVarWithModal) {
    strVec <- as.character(unique(as.data.frame(DFSource)[,x]))
    strVec <- sort(strVec)
    dfDomainCatExplore[[x]] <- c("ALL",strVec)
  }
  
  selCatDomainExplore <<- dfDomainCatExplore
  selCatDomainExploreInit <<- dfDomainCatExplore
  
  ############## Modeling 관련 고유 정보 ################  
  # pathModel <<- "../Model/EH47BCA"
  

}

