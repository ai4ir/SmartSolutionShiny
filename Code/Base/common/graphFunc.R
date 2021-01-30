### graphFunc.R ###
orderVFcluster <- function(df) {
  # ANOVA 분석에 의한 연속형 변수 서열화, 연속형 변수들중에 어느 변수가 cluster간 평균값 파이가 큰지 조사함.
  # clusterResult <- "clusterLassoSel"
  # df <- curSampleExplore
  # clusterResult <- "clusterSamplingGUI"
  # df <- curSampleExplore

  if(is.na(aesList[["clusterMethod"]][1])) {
    numVar <- extractContVarName(df)
    pValue <- rep(1, times=length(numVar) )
    orderedVF <- data.frame(pValue)
    rownames(orderedVF) <- numVar
    return(orderedVF)
  }
  
  clusterResult <- aesList[["clusterMethod"]][1]
  
  if(length(unique(df[,clusterResult]))>1) {
    
    # dfNoNA <- df[df[,clusterResult] != "" & !is.na(df[,clusterResult]),]
    dfNoNA <- df
    
    func1 <- function(x) {
      print(paste0("renderExploreDualGraphASub => var : ", x))
      # dfTempo <- dfNoNA[!is.na(dfNoNA[,"C"]),]
      # dfTempo <- dfNoNA[!is.na(dfNoNA[,x]),]
      dfTempo <- dfNoNA
      noLevel <- length(unique(dfTempo[,clusterResult]))
      
      ### 아래 for 문은 clusterResult의 특정 level에서 모든 x가 NA인 경우 pValue를 100으로 처리
      for(i in seq_along(noLevel)) {
        subSet <- dfTempo[dfTempo[,clusterResult]==unique(dfTempo[,clusterResult])[i], ]  
        if(is.na(mean(subSet[,x],na.rm=TRUE))) {
          pValue <- 100
          return( pValue)
        }
      }

      if(length(unique(dfTempo[,x])) < 2 | length(unique(dfTempo[,clusterResult])) < 2 ) {
        pValue <- 100
      } else {
        # print(x)
        result <- aov(dfTempo[,x] ~ dfTempo[,clusterResult])
        pValue <- as.vector(unlist(summary(result)))[9] * 100
      }
    }
    
    # pValueVec <- vapply(c("C", "Nb", "Mn"), func1, FUN.VALUE=numeric(1))
    numVarExplore <- extractContVarName(curSampleExplore)
    
    pValueVec <- vapply(numVarExplore, func1, FUN.VALUE=numeric(1))
    pValueVec <- pValueVec[order(pValueVec)]
    
    orderedVF <- as.data.frame(pValueVec)

    # orderVFcluster <- names(pValueVec)
    
    return(orderedVF)
  }
}


renderScatterPlot <- function(df, x, y, color=NULL, size=NULL, shape=NULL, tooltip = NULL, data_id=NULL,curSelGirafe=NULL) {
  # x="wt"; y="mpg"; color="cyl"
  if(!("rowNoSource" %in% colnames(df))) {
    df <- df %>% mutate(rowNoSource=row.names(df))
  }
  dfGraph <- df[,c(x,y,color, size, shape,tooltip, data_id)] 
  if(length(dfGraph[,color])<10)
    dfGraph[,color] <- as.factor(dfGraph[,color])
  
  dfGraph[,size] <- as.factor(dfGraph[,size])
  dfGraph[,shape] <- as.factor(dfGraph[,shape])
  if(!is.null(color) && !is.factor(dfGraph[,color])  && length(unique(dfGraph[,color])) < 6 && x!=color) {
    dfGraph[,color] <- as.factor(dfGraph[,color])
  }
  ### fitting하여 R2값 도출
  
  labelAnnotate <- NULL
  if(!is.Date(dfGraph[,x]) & !is.Date(dfGraph[,y]) & !is.POSIXct(dfGraph[,x]) & !is.POSIXct(dfGraph[,y])) {
    dfModel <- dfGraph[,c(x, y)]
    colnames(dfModel) <- c("xVec", "yVec")
    switch(aesList[["fitOption"]],
           NoFit = {},
           Fit1 = {
             model <- lm(yVec ~ xVec,data=dfModel)
             myfunc <- function(x) {
               model[["coefficients"]][1] + model[["coefficients"]][2] * x 
             }
             R2 <- broom::glance(model)[["r.squared"]]
             # labelAnnotate <- paste0("R2 : ", round(R2,3) )
             labelAnnotate <- paste0("R2 : ", round(R2,3), ", intercept :",round(model[["coefficients"]][1],3),
                                     "  slope :", round(model[["coefficients"]][2],3) )
             xAnnotate <-  min(dfGraph[,x],na.rm=TRUE) +
               0.2*(max(dfGraph[,x],na.rm=TRUE)-min(dfGraph[,x],na.rm=TRUE))
           },
           Fit2 = {
             dfModel <- dfModel %>% mutate(xVec2 = xVec * xVec)
             model <- lm(yVec ~ xVec + xVec2,data=dfModel)
             myfunc <- function(x) {
               model[["coefficients"]][1] + model[["coefficients"]][2] * x + model[["coefficients"]][3]*x*x
             }
             R2 <- broom::glance(model)[["r.squared"]]
             labelAnnotate <- paste0("R2 : ", round(R2,3))
             xAnnotate <-  min(dfGraph[,x],na.rm=TRUE) +
               0.8*(max(dfGraph[,x],na.rm=TRUE)-min(dfGraph[,x],na.rm=TRUE))
             
           },
           {}
    )
    
  }
  
  if(is.null(tooltip)) {
    tooltip <- "tooltip"
    dfGraph <- dfGraph %>% mutate(tooltip=paste(as.character(dfGraph[,y]), "\n", dfGraph[,x], "\n",
                                                dfGraph[,color], "\n", dfGraph[,data_id]))
  }
  
  
  if(!is.numeric(dfGraph[,color]) & 
     length(unique(dfGraph[,color])) > 20 )
  {
    kk <- table(dfGraph[,color])
    kk <- kk[order(kk, decreasing=TRUE)]
    kk <- kk[1:20]
    dfGraph[,color] <- ifelse(dfGraph[,color] %in% names(kk), 
                              dfGraph[,color], "기타"
    )
  }
  
  theme_update(axis.title=element_text(size=16))
  theme_update(axis.text=element_text(size=12))
  
  gg1 <- ggplot(dfGraph) +
    geom_point_interactive(aes_string(x = x, y=y, color = color, size=size, shape=shape,
                                      tooltip = tooltip, data_id = data_id), size = 1) +
    guides(color = guide_legend(override.aes = list(size = 1))) #+ 
    # theme(legend.title = element_text(size = 3),
    #       legend.text  = element_text(size = 2)
    #       # legend.key.size = unit(0.1, "lines")
    # )
  
  if(!is.null(labelAnnotate)) {
    gg1 <- gg1 +  stat_function(fun=myfunc, geom="line", color="black") +
      annotate("text",
               x=xAnnotate, 
               y=min(dfGraph[,y], na.rm=TRUE), label=labelAnnotate, color="black", size=3)
  }
  
  # gg1 <- gg1 + scale_fill_manual_interactive(
  #   # values = c(Male = "#0072B2", Female = "#009E73"),
  #   # data_id = function(breaks) { as.character(breaks)},
  #   # tooltip = function(breaks) { as.character(breaks)},
  #   # onclick = function(breaks) { paste0("alert(\"", as.character(breaks), "\")") },
  #   guide = guide_legend_interactive(
  #     title.theme = element_text_interactive(
  #       size = 8 #,
  #       # data_id = "legend.title",
  #       # onclick = "alert(\"Gender levels\")",
  #       # tooltip = "Gender levels"
  #     ),
  #     label.theme = element_text_interactive(
  #       size = 6
  #     )
  #   )
  # )
  
  if(is.null(curSelGirafe)) {
    girafe( code = print(gg1), #width_svg = 8, height_svg = 4,
            options = list(opts_selection( #selected = curSelGirafe,
              type = "multiple", only_shiny = TRUE)
            )
    )

  } else {
    girafe( code = print(gg1), #width_svg = 8, height_svg = 4,
            options = list(opts_selection( selected = curSelGirafe,
              type = "multiple", only_shiny = TRUE)
            )
    )

  }
  

}

