############################################ mtcars ##############################
tidySource <- function() {
  
  dfOrg <- diamonds
  
  dfOrg <- as.data.frame(dfOrg)
  
  ### "label" attribute는 그래프 라벨을 위해 반드시 필요함 ###
  for(x in colnames(dfOrg)) {
    attr(dfOrg[,x], "label") <- x
  }
  
  attr(dfOrg[,"clarity"], "label") <- "투명도"
  
  ### "digit" attribute는 집계표 생성을 위해 반드시 필요함 ###
  numVarConst <- extractNumVarNameAndConst(dfOrg)
  decimalVec <- vapply(numVarConst, renderDigitVector,  FUN.VALUE=numeric(1), dfOrg)
  decimalVec <- as.vector(decimalVec)
  for(x in numVarConst) {
    # numVarConst <- "thick"
    attr(dfOrg[,x], "max") <- max(dfOrg[,x], na.rm=TRUE)
    attr(dfOrg[,x], "mean") <- mean(dfOrg[,x], na.rm=TRUE)
    attr(dfOrg[,x], "min") <- min(dfOrg[,x], na.rm=TRUE)
    attr(dfOrg[,x], "digit") <- decimalVec[which(numVarConst==x)]
  }
  
  ### sticky_all은 sunset에서도 attribute를 유지하기 위해 필요함 ###
  dfOrg <- sticky_all(dfOrg)
  
  
  DFSource <<- dfOrg
  
  write_rds(DFSource, "../SourceData/diamonds/diamonds_전처리.rds")
  write_excel_csv(DFSource, path="../USER/diamonds/output/diamonds_전처리.csv", na="")
  
  return()
  
  ### 이래 영역은 tidySource.R을 개발하면서 임시로 사용하는 코딩 모음입니다. ###
  
  xxx <- timeLoss[2:5,]
  
  pie(xxx$timeLossDay, labels=xxx$intvCat)
  
  
}

