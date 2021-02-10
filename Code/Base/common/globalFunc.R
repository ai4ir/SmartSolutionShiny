### globalFunc.R ###
extractConstName <- function(df) {
  df <- as.data.frame(df)
  varNameVec <- colnames(df)
  func1 <- function(x) {
    length(unique(df[,x])) < 2
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  # catVar <- varNameVec[!booleanVec]
  constName <-  varNameVec[booleanVec]
}
## numeric only, no date ### var <- extractNumVarName(curSampleExplore)
extractNumVarName <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출
  varNameVec <- colnames(df)
  func1 <- function(x) {
    is.numeric(df[,x])  && (length(unique(df[,x])) > 1)
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  # catVar <- varNameVec[!booleanVec]
  numVar <-  varNameVec[booleanVec]
}

### date only  ### var <- extractDateVarName(curSampleExplore)
extractDateVarName <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출
  
  varNameVec <- colnames(df)
  func1 <- function(x) {
    bool1 <- is.Date(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool2 <- is.POSIXct(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool3 <- is.difftime(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    return(bool1 | bool2 | bool3) 
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  dateVar <-  varNameVec[booleanVec]
}
### numeric or date  ### 
extractContVarName <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출

  varNameVec <- colnames(df)
  func1 <- function(x) {
    bool1 <- is.numeric(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool2 <- is.Date(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool3 <- is.POSIXct(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool4 <- is.difftime(df[,x])  && (length(setdiff(unique(df[,x]),NA)) > 1)
    bool1 | bool2 | bool3 | bool4
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  # catVar <- varNameVec[!booleanVec]
  contVar <-  varNameVec[booleanVec]
}
extractNumVarNameAndConst <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출
  varNameVec <- colnames(df)
  func1 <- function(x) {
    is.numeric(df[,x])  
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  # catVar <- varNameVec[!booleanVec]
  numVar <-  varNameVec[booleanVec]
}
extractCatVarName <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출
  varNameVec <- colnames(df)
  func1 <- function(x) {
    !is.numeric(df[,x])  && (length(unique(df[,x])) > 1)
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  catVar <- varNameVec[booleanVec]
}
## numeric only, no date ###
extractCharVarName <- function(df) {
  df <- as.data.frame(df)
  # numeric variable 만 추출
  varNameVec <- colnames(df)
  func1 <- function(x) {
    is.character(df[,x])  && (length(unique(df[,x])) > 1)
  }
  booleanVec <- vapply(varNameVec, func1, FUN.VALUE=logical(1))
  # catVar <- varNameVec[!booleanVec]
  charVar <-  varNameVec[booleanVec]
}
calcNoLevels <- function(df) {
  subFunc <- function(x) {
    length(unique(x))
  }
  vapply(df, subFunc, numeric(1))
}
# calcNoLevels(DFSource)
selectVarWithGivenLevels <- function(df, minNoLevels, maxNoLevels) {
  # noLevels <- calcNoLevels(dfReportCommon)
  noLevels <- calcNoLevels(df)
  bool1 <- ifelse(minNoLevels - 1 < noLevels, TRUE, FALSE)
  bool2 <- ifelse(noLevels < maxNoLevels + 1 , TRUE, FALSE)
  bool <- bool1 & bool2
  var <- colnames(df)[bool]
}

### digit 벡터를 만드는 함수
### 사용법 decimalVec <- vapply(x, renderDigitVector, FUN.VALUE=numeric(1), df)
renderDigitVector <- function(x, df) {
  minValue <- min(df[,x], na.rm=TRUE)
  minDecimal <- nchar(as.character(minValue)) - nchar(as.character(floor(minValue)))
  maxValue <- max(df[,x], na.rm=TRUE)
  maxDecimal <- nchar(as.character(maxValue)) - nchar(as.character(floor(maxValue)))
  decimal <- max(minDecimal, maxDecimal)
  decimal <- ifelse( decimal > 1, decimal - 1, decimal)
  return(decimal)
}

renderDFDomainCat <- function(dfDomainCat, catVarName,catVarWithModal, df, bAddAll=TRUE) {
  for(x in catVarName) {
    if(is.factor(df[,x][[1]])) {
      dfDomainCat[[x]] <-
        attr(df[,x][[1]], "levels")
    } else {
      dfDomainCat[[x]] <-
        as.character(unique(as.data.frame(df)[,x]))
    }
  }
  
  for(x in catVarWithModal) {
    strVec <- as.character(unique(as.data.frame(df)[,x]))
    strVec <- sort(strVec)
    if(bAddAll==TRUE)
      dfDomainCat[[x]] <- c("ALL",strVec)
  }
  
  return(dfDomainCat)
  
}


missing_fixer <- function(na_value) {
  function(x) {
    x[x==na_value] <- NA
    x
  }
}


resetLevelsCatVar <- function(df, dfDomainCat, catVar) {
  # df <- DFSource
  # dfDomainCat <- dfDomainCatExplore
  # catVar <- catVarNameExplore
  for(i in seq_along(catVar)) {
    # i <- 1
    if(is.factor(df[,catVar[i]][[1]])) {
      dfDomainCat[[catVar[i]]] <-
        attr(df[,catVar[i]][[1]], "levels")
    } else {
      dfDomainCat[[catVar[i]]] <-
        as.character(unique(as.data.frame(df)[,catVar[i]])) %>% sort()  ### sort하면 NA가 제거됨
    }
  }
  
  return(dfDomainCat)
}


# renderPValueVec <- function(df, numVar) {
#     print("renderPValueVec Portal -------------------------------------")
#     # ANOVA 분석에 의한 연속형 변수 서열화
#     if(length(unique(df[,"bHOT"]))>1) {
#         
#         df <- df[df$bHOT != "",]
#         
#         func1 <- function(x) {
#             print(paste0("renderPValueVec => var : ", x))
#             dfTempo <- df[!is.na(df[,x]),]
#             if(length(unique(dfTempo[,x])) < 2 | length(unique(dfTempo[,"bHOT"])) < 2 ) {
#                 pValue <- 100
#             } else {
#                 # print(x)
#                 result <- aov(dfTempo[,x] ~ dfTempo[,"bHOT"])
#                 pValue <- as.vector(unlist(summary(result)))[9] * 100
#             }
#         }
#         
#         pValueVec <- vapply(numVar, func1, FUN.VALUE=numeric(1))
# 
#     }
#     
# }
# 