############################################ EXCEL ##############################
MTVCodeList <- list(del=NA, remain=NA, mutate=NA, change=NA)

loadSource <- function() {
  projectWD <- getwd()
  setwd("..")
  dirPath <- paste0(getwd(),"/SourceData/EXCEL")
  setwd(dirPath)
  # Tell R to sleep until the current directory matches the expected directory
  while(getwd() != normalizePath(dirPath, winslash="/")) {
    Sys.sleep(0.02)
  }
  filePath <<- file.choose()
  
  ### filePath <- "..\\SourceData\\EXCEL\\mtcars_meta.xlsx"   ### 지우지 마세요
  
  chosenDFSourceFileExt <<- str_split(filePath, "\\\\")[[1]][length( str_split(filePath, "\\\\")[[1]])]
  chosenDFSourceFile <<- str_split(chosenDFSourceFileExt, "\\.")[[1]][1]
  chosenDFSourceExt <- str_split(chosenDFSourceFileExt, "\\.")[[1]][2]
  
  # filePathDir <- str_sub(filePath,1,str_length(filePath)-str_length(chosenDFSourceFile)-str_length(chosenDFSourceExt)-1)


  setwd(projectWD)
  switch(chosenDFSourceExt,
         xlsx = {
           DFSource <<- read_xlsx_meta(filePath)
         },
         csv = {
           DFSource <<- read_csv_meta(filePath)
         },
         rds = {
           DFSource <<- read_rds(filePath)
         })

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/EXCEL/output"
  
  hide("renderReportSourcing")
  # ############## Sourcing 관련 고유 정보 ################
  # pathFileRmdSourcingReport <<- c("each/sourcing/mtcars/Rmd/SourcingReport1.Rmd",
  #                                 "each/sourcing/mtcars/Rmd/SourcingReport2.Rmd"
  # )
  # outputFileNamesSourcingReport <<- c("SourcingReport1.html",
  #                                     "SourcingReport2.html")
  # ############## Sampling 관련 고유 정보 ################
  # pathFileRmdSamplingReport <<- c("each/sourcing/mtcars/Rmd/SamplingReport1.Rmd",
  #                                 "each/sourcing/mtcars/Rmd/SamplingReport2.Rmd",
  #                                 "each/sourcing/mtcars/Rmd/SamplingReportTable.Rmd"
  # )
  # outputFileNamesSamplingReport <<- c("SamplingReport1.html",
  #                                     "SamplingReport2.html",
  #                                     "SamplingReportTable.html")


  
  pathNameFileDomain <<- "../USER/EXCEL/Domain"
  pathNameFileDesign <<- "../USER/EXCEL/Design"
  # pathNameFileNewModel <<- "../Model/mtcars"


   

}

renderAttrSamplingUI <- function() {
  
  ############## 변수 분리 ################
  numVar <- extractNumVarName(DFSource)
  catVar <- extractCatVarName(DFSource)
  
  func1 <- function(x) {
    if(length(unique(DFSource[,x])) > 3 ) {
      TRUE
    } else {
      FALSE
    }
    
  }
  
  boolTrueNum <- vapply(numVar, func1, logical(1))
  numVarTrue <- numVar[boolTrueNum]
  
  lengthNumVarTrue <- length(numVarTrue)
  
  if (lengthNumVarTrue>12) {
    DomainTable1Names <<- numVarTrue[1:12]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable1NamesLabel <<- DomainTable1Names
  } else {
    DomainTable1Names <<- numVarTrue[1:lengthNumVarTrue]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable1NamesLabel <<- DomainTable1Names
  }
  
  if (lengthNumVarTrue>24) {
    DomainTable2Names <<- numVarTrue[13:24]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable2NamesLabel <<- DomainTable1Names
  } else if(lengthNumVarTrue>12) {
    DomainTable2Names <<- numVarTrue[13:lengthNumVarTrue]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable2NamesLabel <<- DomainTable1Names
  } else {
    DomainTable2Names <<- NULL
    DomainTable2NamesLabel <<- DomainTable1Names    
  }
  
  if (lengthNumVarTrue>36) {
    DomainTable3Names <<- numVarTrue[25:36]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable3NamesLabel <<- DomainTable1Names
  } else if(lengthNumVarTrue>24) {
    DomainTable3Names <<- numVarTrue[13:lengthNumVarTrue]
    # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
    DomainTable3NamesLabel <<- DomainTable1Names
  } else {
    DomainTable3Names <<- NULL
    DomainTable3NamesLabel <<- DomainTable1Names    
  }

  
  # chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B",  "N2", "Ca")
  # chemCompNameLabel <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb", "Ti","SolAl", "B(ppm)", "N2(ppm)",  "Ca(ppm)")
  
  ############## dfDomainCat 계산 ################
  func1 <- function(x) {
    if( length(unique(DFSource[,x])) == 2 || length(unique(DFSource[,x])) == 3) {
      TRUE
    } else {
      FALSE
    }
  }
  boolTwoThreeLevels <- vapply(catVar, func1, logical(1))
  catVarTwoThreeLevels <- union(catVar[boolTwoThreeLevels], numVar[!boolTrueNum])
  catVarWithoutModal <<- catVarTwoThreeLevels # Modal을 이용하지 않고 Category를 선택하는 변수
  
  func1 <- function(x) {
    if( length(unique(DFSource[,x])) < 4) {
      TRUE
    } else {
      FALSE
    }
  }
  boolFewLevels <- vapply(catVar, func1, logical(1))
  catVarWithModal <<- setdiff(catVar[!boolFewLevels], "rowNoSource")  # Modal을 이용하여 Category를 선택하는 변수
  
  
  
  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- vector("list",length(catVarNameExplore))
  
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
  
  
  
}


read_xlsx_meta <- function(filePath) {
  
  ### filePath <- "..\\SourceData\\EXCEL\\mtcars_meta.xlsx"   ### 지우지 마세요
  
  df <- read_xlsx(filePath, sheet=1, skip=7, col_names=FALSE)
  df <- as.data.frame(df)
  # str(df)
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
  
  unit <- rep(NA, NCOL(df))
  unit <- read_xlsx(filePath,
                          sheet=1, range=cell_rows(7))
  unit <- as.data.frame(unit)
  for(i in 1: NCOL(df)) {
    attr(df[,i],"unit") <- NA
  }
  for(i in 1: NCOL(unit)) {
    attr(df[,i],"unit") <- colnames(unit)[i]
  }
  for(i in 1: NCOL(unit)) { 
    if(str_sub(attr(df[,colnames(df)[i]],"unit"),1,3)=="...") {
      attr(df[,i],"unit") <- NA
    }
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
