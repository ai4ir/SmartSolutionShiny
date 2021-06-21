############################################ salesRecord ##############################

attachAttr <- function(filePath, df) {
  df <- as.data.frame(df)
  meta <- read_xlsx(filePath,
                    sheet=1, range=cell_rows(1:4))
  meta <- as.data.frame(meta)
  colnames(df) <- colnames(meta)
  for(i in 1: NCOL(df)) {
    if(!is.na( meta[1,i])) {
      attr(df[,i],"validMax") <- meta[1,i]
    } else {
      attr(df[,i],"validMax") <- NA
    }
    if(!is.na( meta[2,i])) {
      attr(df[,i],"validMin") <- meta[2,i]
    } else {
      attr(df[,i],"validMin") <- NA
    }
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
  
  # # validMinDF <- vector(mode="numeric",dim(dfOrg)[2] )
  # validMaxDF <- rep(NULL,dim(df)[2] )
  # for(i in 1: dim(df)[2]) {
  #   if(!is.null(attr(df[,i],"validMax"))) {
  #     validMaxDF[i] <- attr(df[,i],"validMax")
  #   }
  # }
  # attr(df,"validMax") <- validMaxDF
  # 
  # validMinDF <- rep(NULL,dim(df)[2] )
  # for(i in 1: dim(df)[2]) {
  #   if(!is.null(attr(df[,i],"validMin"))) {
  #     validMinDF[i] <- attr(df[,i],"validMin")
  #   }
  # }
  # attr(df,"validMin") <- validMinDF
  
  
  
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

attachAttrOneVar <- function(df, var, label="임시 라벨", labelShort="임시 라벨", digit=2,
                             unit=NA, validMin=NA, validMax=NA) {
  df <- as.data.frame(df)
  attr(df[,var], "label") <- label ;   attr(df[,var], "labelShort") <- labelShort ; 
  attr(df[,var], "validMin") <- validMin ;   attr(df[,var], "validMax") <- validMax ;
  attr(df[,var], "digit") <- digit ;   attr(df[,var], "unit") <- unit ;
  sticky_all(df)
  return(df)
}

render_dfDomainCatExplore <- function(catVarNameExplore) {
  strMid <- ""
  for(i in seq(length(catVarNameExplore))) {
    strMid <- paste0(strMid, "\"",  catVarNameExplore[i], "\", ")
  }
  strMid <- str_sub(strMid,1,-3)
  stringCode <- paste0("dfDomainCatExplore <- list(",strMid , ")")
  exprCode <- parse(text=stringCode)
  eval(exprCode)
  return(dfDomainCatExplore)
}
  

renderNameVarNA <- function(df) {
  varNA <- NULL
  for(x in colnames(df)) {
    if(sum(!is.na(df[,x]))==0) {
      varNA <- c(varNA,x)
    }
  }
  
  return(varNA)
}

removeVarTidy <- function(df, removedVar=NULL) {
  # varNA <- c("v12","v13","v14","v15","v16","v17")
  
  tidyingList[["removedVarNA"]] <<- renderNameVarNA(df)
  
  tidyingList[["removedVar"]] <<- removedVar
  tidyingList[["remainedVar"]] <<-
    setdiff(colnames(df), c(tidyingList[["removedVar"]],tidyingList[["addedVar"]], tidyingList[["removedVarNA"]])  )
  df <- df[,c(tidyingList[["remainedVar"]], tidyingList[["addedVar"]])]
  return(df)
  
}