############################################ salesRecord ##############################
attachAttr <- function(filePath, df) {
  df <- as.data.frame(df)
  meta <- read_xlsx(filePath,
                    sheet=1, range=cell_rows(1:4))
  meta <- as.data.frame(meta)
  colnames(df) <- colnames(meta)
  for(i in 1: NCOL(df)) {
    attr(df[,i],"validMax") <- meta[1,i]
    attr(df[,i],"validMin") <- meta[2,i]
  }
  
  label <- read_xlsx(filePath,
                     sheet=1, range=cell_rows(5))
  label <- as.data.frame(label)
  for(i in 1: NCOL(df)) {
    attr(df[,i],"label") <- colnames(label)[i]
  }
  
  labelShort <- read_xlsx(filePath,
                          sheet=1, range=cell_rows(6))
  labelShort <- as.data.frame(labelShort)
  for(i in 1: NCOL(df)) {
    attr(df[,i],"labelShort") <- colnames(labelShort)[i]
  }
  
  # validMinDF <- vector(mode="numeric",dim(dfOrg)[2] )
  validMaxDF <- rep(NA,dim(df)[2] )
  for(i in 1: dim(df)[2]) {
    if(!is.null(attr(df[,i],"validMax"))) {
      validMaxDF[i] <- attr(df[,i],"validMax")
    }
  }
  attr(df,"validMax") <- validMaxDF
  
  validMinDF <- rep(NA,dim(df)[2] )
  for(i in 1: dim(df)[2]) {
    if(!is.null(attr(df[,i],"validMin"))) {
      validMinDF[i] <- attr(df[,i],"validMin")
    }
  }
  attr(df,"validMin") <- validMinDF
  
  
  
  numVarConst <- extractNumVarNameAndConst(df)
  decimalVec <- vapply(numVarConst, renderDigitVector,  FUN.VALUE=numeric(1), df)
  decimalVec <- as.vector(decimalVec)
  for(x in numVarConst) {
    # numVarConst <- "thick"
    attr(df[,x], "max") <- max(df[,x], na.rm=TRUE)
    attr(df[,x], "mean") <- mean(df[,x], na.rm=TRUE)
    attr(df[,x], "min") <- min(df[,x], na.rm=TRUE)
    attr(df[,x], "digit") <- decimalVec[which(numVarConst==x)]
    if(!is.na(meta[3,x])) {
      attr(df[,x],"digit") <- meta[3,x]
    }
  }
  df <- sticky_all(df)
  return(df)
  
}
  