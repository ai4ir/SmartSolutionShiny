renderMyModel <- function(dfModel)  {
  
  # dfModel <- dfModel[dfModel[,"C"]<0.12 & !is.na(dfModel$C),]
  # dfModel <- dfModel[dfModel[,"Ti"]<0.05 & !is.na(dfModel$Ti),]
  
  ### domain 축소 ###
  dfModel <- dfModel[dfModel$MANUFACTURING_PROCESS=="N",]    ### for model developer
  dfModel <- dfModel[dfModel$Tensile_Shape=="봉상",]           ### for model developer
  
  # logicalVec <- ymd_hms("2020-01-01 00:00:00") <= dfModel$date & dfModel$date < ymd_hms("2020-04-01 00:00:00") 
  # dfModel <- dfModel[logicalVec,]
  # 
  
  ### NA 처리 ### 
  dfModelTempo <- dfModel %>% mutate(clusterGr="Tempo")  ### 검중 리포트 파일명의 마지막 단어 ###
  dfNest <- dfModelTempo %>% group_by(clusterGr) %>% nest()
  modelY <- "Top_YP_WRSLT"          ### for model developer
  modelDirectRaw <- vector("list", length(dfNest$clusterGr))
  modelDirectRaw[[1]] <- c("C","Mn","Nb", "Ti")    ### for model developer
  dfModel <- dfModel[,c(modelY,modelDirectRaw[[1]],"rowNoSource", "bHOT", "sampleCode",
                        "MANUFACTURING_PROCESS", "Tensile_Shape", "SPEC_HEATOUT_AIM")]    ### for model developer
  dfModel <- na.omit(dfModel)
  
  ### nest 처리 ###
  dfModel <- dfModel %>% mutate(clusterGr="model_Mine")  ### 검중 리포트 파일명의 마지막 단어 ###  ### for model developer      
  # dfModel <- dfModel %>% mutate(clusterGr=ifelse(customer %in% "D","CMnSteel","HSLA"))
  dfModelNest <- dfModel %>% 
    group_by(clusterGr) %>% 
    nest()
  
  # modelDirectRaw[[2]] <- c("thick","C","Mn","P","S", "B")
  modelFramework <- vector("list", length(dfModelNest$clusterGr))
  model <- vector("list", length(dfModelNest$clusterGr))
  modelFramework[[1]] <- function(df) {
    lm(Top_YP_WRSLT ~ C + Mn  + Nb + Ti , data = df, na.action=na.omit)    ### for model developer
  }
  # modelFramework[[2]] <- function(df) {
  #   lm(YS ~ thick + C + Mn + Si + P + S + B, data = df)
  # }
  
  dfModelNest <- dfModelNest %>% 
    mutate(
      modelFramework = modelFramework,
      model=model,
      modelDirectRaw = modelDirectRaw,
      modelY = modelY,
      path = ""
    )
  
  return(dfModelNest)
}