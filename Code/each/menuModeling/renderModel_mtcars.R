renderModel_mtcars <- function(dfModel)  {

  #### 5월 10일 수정 ####
  
  ### NA 처리 ###  omit 하기 전에 모델에 필요한 변수만을 가지고 omit 해야 함.
  dfModelTempo <- dfModel %>% mutate(clusterGr="Tempo") 
  # 모델에 필요한 변수만을 가지고 omit 하기 위한 임시 nest
  dfNest <- dfModelTempo %>% group_by(clusterGr) %>% nest()  
  modelY <- "mpg"          ### for model developer
  modelDirectRaw <- vector("list", length(dfNest$clusterGr))
  modelDirectRaw[[1]] <- c("wt","am","gear","hp")    ### for model developer
  essentialvar <- c("rowNoSource", "bHOT", "sampleCode")
  dfModel <- dfModel[,c(modelY,modelDirectRaw[[1]],essentialvar)] 
  
  fo <- as.formula(paste0("mpg~",paste( modelDirectRaw[[1]],collapse = "+")))
  

  dfModel <- na.omit(dfModel)
  dfModel <- validateDF(dfModel, c(modelY,modelDirectRaw[[1]] ))
  

  ### nest 처리 ###
  dfModel <- dfModel %>% mutate(clusterGr="model_mtcars")  ### 검중 리포트 파일명의 마지막 단어 ###  ### for model developer      
  dfModelNest <- dfModel %>%  group_by(clusterGr) %>% nest()
  
  # modelDirectRaw[[2]] <- c("thick","C","Mn","P","S", "B")
  modelFramework <- vector("list", length(dfModelNest$clusterGr))
  model <- vector("list", length(dfModelNest$clusterGr))
  
  
  # #### 4월30일 추가 기능 #### 아웃라이어 제거에 대한 부분 
  # 
  # # essentialvar <- c("rowNoSource", "bHOT", "sampleCode","RollingMode","grCCC2","ManufacturingProcess")
  # ### for model developer
  # # modelDirectRaw[[1]] <- c("C","Mn","Si","P","S","SRT","PlateThickness")    
  # ### for model developer
  # dfModel <- dfModel[,c(modelY,modelDirectRaw[[1]],essentialvar)] 
  
  
  
  
  ################################ Linear Model #######################################
  modelFramework[[1]] <- function(df) {
    lm(fo, data = df, na.action=na.omit)    ### for model developer
  }
  ################################ Linear Model ####################################### 
  
  ################################ RandomForest Model ################################## 
  # modelFramework[[1]] <- function(df) {
  #   randomForest(fo , data=df,
  #                ntree=300, mtry= 5, na.action=na.omit, importance=T )    ### for model developer
  # }
  ################################ RandomForest Model ################################## 
  
  
  dfModelNest <- dfModelNest %>% 
    mutate(
      modelFramework = modelFramework,
      model=model,
      modelDirectRaw = modelDirectRaw,
      modelY = modelY,
      path = "",   ### for model developer
      algorithmML ="LM"  ### for model developer, LM, RF
    )
  
  return(dfModelNest)
}
