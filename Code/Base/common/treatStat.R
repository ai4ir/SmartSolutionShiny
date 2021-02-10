### treatStat.R ### 기본적 통계 처리용 함수
### df <- DFSource; x <- "mpg"; y <- "wt"
calcR2OneYFit1 <- function(x, df, y) {
  df2 <- df[,c(x,y)] %>% mutate(x=df[,x], y=df[,y])
  modelResult <- lm(y~x , data=df2)
  # adjR2 <- broom::glance(modelResult)[["adj.r.squared"]] # 음수값이 나오는 경우 발생
  R2 <- broom::glance(modelResult)[["r.squared"]]
}
calcR2OneYFit2 <- function(x, df, y) {
  df2 <- df[,c(x,y)] %>% mutate(x=df[,x], y=df[,y], x2=x*x)
  modelResult <- lm(y~x + x2, data=df2)
  # adjR2 <- broom::glance(modelResult)[["adj.r.squared"]] # 음수값이 나오는 경우 발생
  R2 <- broom::glance(modelResult)[["r.squared"]]
}
calcR2OneXFit1 <- function(y, df, x) {
  df2 <- df[,c(x,y)] %>% mutate(x=df[,x], y=df[,y])
  modelResult <- lm(y~x , data=df2)
  # adjR2 <- broom::glance(modelResult)[["adj.r.squared"]] # 음수값이 나오는 경우 발생
  R2 <- broom::glance(modelResult)[["r.squared"]]
}
calcR2OneXFit2 <- function(y, df, x) {
  df2 <- df[,c(x,y)] %>% mutate(x=df[,x], y=df[,y], x2=x*x)
  modelResult <- lm(y~x + x2, data=df2)
  # adjR2 <- broom::glance(modelResult)[["adj.r.squared"]] # 음수값이 나오는 경우 발생
  R2 <- broom::glance(modelResult)[["r.squared"]]
}

### df <- DFSource; x <- "mpg"; pValue <- calcPValueShapiro(df,x)
calcPValueShapiro <- function(x, df) {
  x1 <- df[,x]
  if(length(x1) > 5000) x1 <- sample(x1, 5000)
  result <- shapiro.test(x1)
  pValue <- result[["p.value"]]*100
  return(pValue)
}

### df <- DFSource; y <-  "NRL_FirstTest" ; x <- "PL_REFUR_NO"; pValue <- calcPValueShapiro(df,x) 
calcPValueFisherExact <- function(x,y, df) {
  df <- df[,c(x,y)]
  fmla <- as.formula(paste0(" ~ ",x,"+ ", y))
  fisherExactResult <- xtabs(fmla,df) %>% fisher.test()
  pValue <- fisherExactResult[["p.value"]]*100
  return(pValue)
}

# df <- DFSource; x <- "am"; y <- "mpg"; pValue <- calcPValueOneYAov("am",df, "mpg")
# catVar <- c("am","vs")
# pValueVec <- vapply(catVar, calcPValueOneYAov,df,"mpg", FUN.VALUE=numeric(1))
# pValueVec <- pValueVec[order(pValueVec)]
# orderedCatVar <- names(pValueVec)

calcPValueOneYAov <- function(x,df,y) {
  dfTempo <- df
  noLevel <- length(unique(dfTempo[,x]))
  
  if(noLevel<2 | length(unique(dfTempo[,y])) < 2) {return(100)}
  
  ### 아래 for 문은 clusterResult의 특정 level에서 모든 x가 NA인 경우 pValue를 100으로 처리
  for(i in seq_along(noLevel)) {
    subSet <- dfTempo[dfTempo[,x]==unique(dfTempo[,x])[i], ]  
    if(is.na(mean(subSet[,y],na.rm=TRUE))) {
      pValue <- 100
      return( pValue)
    }
  }
  
  # print(x)
  result <- aov(dfTempo[,y] ~ dfTempo[,x])
  pValue <- as.vector(unlist(summary(result)))[9] * 100
  return(pValue)
  
}


# x <- "am"; y <- "mpg"; pValue <- calcPValueOneXAov("mpg",df, "am")
# numVar <- c("mpg","disp","drat","qsec")
# pValueVec <- vapply(numVar, calcPValueOneXAov,df,"am", FUN.VALUE=numeric(1))
# orderedNumVar <- numVar[order(pValueVec)]
# pValueVec <- pValueVec[order(pValueVec)]

calcPValueOneXAov <- function(y,df,x) {
  dfTempo <- df
  noLevel <- length(unique(dfTempo[,x]))
  
  if(noLevel<2 | length(unique(dfTempo[,y])) < 2) {return(100)}
  
  ### 아래 for 문은 clusterResult의 특정 level에서 모든 x가 NA인 경우 pValue를 100으로 처리
  for(i in seq_along(noLevel)) {
    subSet <- dfTempo[dfTempo[,x]==unique(dfTempo[,x])[i], ]  
    if(is.na(mean(subSet[,y],na.rm=TRUE))) {
      pValue <- 100
      return( pValue)
    }
  }
  
  # print(x)
  result <- aov(dfTempo[,y] ~ dfTempo[,x])
  pValue <- as.vector(unlist(summary(result)))[9] * 100
  return(pValue)
  
}
