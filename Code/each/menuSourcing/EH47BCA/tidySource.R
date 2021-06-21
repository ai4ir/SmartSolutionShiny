############################################ salesRecord ##############################
tidySource <- function() {

  # df <- read_xlsx("../SourceData/EH47BCA/210510 EH47BCA_NRL.v1 2.xlsx",
  #                             sheet=2,
  #                             skip=1, col_names=FALSE)
  # write_rds(df, "../SourceData/EH47BCA/210510 EH47BCA_NRL.v1 2.rds")
  chosenDFSourceFile <<- "EH47BCA"
  pathTidyingReport <<- "../USER/EH47BCA/output"
  
  df <- read_rds("../SourceData/EH47BCA/210510 EH47BCA_NRL.v1 2.rds")   

  # df <- rbind(df1, df2)
  # df <- df2
  
  filePath <- "../SourceData/EH47BCA/210510 EH47BCA_NRL_meta.xlsx"
  df <- attachAttr(filePath, df)
  
  # # 쓰레기 데이터 제거
  # df <- df[df$Tensile_Shape %in% c("판상", "봉상"),]

  #  factor 변수 지정
  catVar <- c("TensileShape" ,"TensileSpecimenDirection", "Factory", "ManufaturingProcess",
              "RollingMode", "RollingModeDirection", "SRT_FurnanceNo",
              "RollingMeasureCode","NRL_FirstTest",
              "CCMachine", "MslabCharicteristic", "TensileShapeCode")
  for(var in catVar) {
    df[,var] <- as.factor(df[,var])
  }

  filePath <- "../SourceData/EH47BCA/210510 EH47BCA_NRL_meta.xlsx"
  df <- attachAttr(filePath, df)
  
  # 생산월 데이터 추가 
  month <- month(unsticky(df$DecisionDate))
  df <- df %>% mutate(month=month) 
  attr(df[,"month"],"label") <- "생산 월"
  attr(df[,"month"],"labelShort") <- "생산 월"
  attr(df[,"month"],"digit") <- 0
  attr(df[,"month"],"validMin") <- 1
  attr(df[,"month"],"validMax") <- 12
  
  
 
  df <- addTransVarTaskSteelPlate(df)
  df <- addTransVarEachEH47BCA(df)
  df <- sticky_all(df)
  dfAll <<- df
  
  removedVar <- c("ControlRollingCode", "ControlRollingMethod", "bControlRolling", "bRM")
  df <- removeVarTidy(df, removedVar)  
  df <- sticky_all(df)
  DFSource <<- df
  
  
  # attributes(DFSource) 
  write_rds(DFSource, "../SourceData/EH47BCA/210510 EH47BCA_NRL.v1 2_전처리.rds")
  write_excel_csv(DFSource, path="../USER/EH47BCA/output/EH47BCA.csv", na="")
  

  return(df)

}

addTransVarEachEH47BCA <- function(df) {
  
  #maxSRT
  df <- df %>% mutate( maxSRT = pmax(df$SRT_1stZoneTemp, df$SRT_2ndZoneTemp, df$SRT_3rdZoneTemp, df$SRT_HomogenousTemp))
  attr(df[,"maxSRT"],"label") <- "최고 분위기 온도" ;
  attr(df[,"maxSRT"], "labelShort") <- "최고 분위기 온도";
  
  df <- df %>% mutate(rateReheatSlab = SRT_SlabTemp/SRT_SlabTotalTime) %>%
    attachAttrOneVar(var="rateReheatSlab", label="급속가열도", labelShort="급속가열도")

  tidyingList[["addedVar"]] <<- c(tidyingList[["addedVar"]], "maxSRT","rateReheatSlab")
  
  return(df)
}



# removeVarEachEH47BCA <- function(df) {
#   # varNA <- c("HeatTreatmentTemp", "HeatTreatmentTime")
#   # varNA <- c(varNA, "PWHTTemp", "PWHTTempRnage", "PWHTTime", "PWHTCycle","NORTemp", "NORTemp", "NORTime",
#   #            "NORCycle", "PWHTTemp_Add1", "PWHTTempRnage_Add1", "PWHTTime_Add1", "PWHTCycle_Add1", "NORTemp_Add1",
#   #            "NORTempRnage_Add1", "NORTime_Add1", "NORCycle_Add1", "PWHTTemp_Add2", "PWHTTempRnage_Add2",
#   #            "PWHTTime_Add2", "PWHTCycle_Add2", "NORTemp_Add2", "NORTempRnage_Add2", "NORTime_Add2",
#   #            "NORCycle_Add2")
#   # varNA <- c(varNA, "DWTTTempCode", "DWTTTemp", "DWTT_Ind1", "DWTT_Ind2", "DWTT_Ind3", "DWTT_Avg", 
#   #            "HIC_CLR_Ind1", "HIC_CLR_Ind2", "HIC_CLR_Ind3", "HIC_CLR_Ind4", "HIC_CLR_Ind5", "HIC_CLR_Ind6",
#   #            "HIC_CLR_Ind7", "HIC_CLR_Ind8", "HIC_CLR_Ind9", "HIC_CTR_Ind1", "HIC_CTR_Ind2", "HIC_CTR_Ind3",
#   #            "HIC_CTR_Ind4", "HIC_CTR_Ind5", "HIC_CTR_Ind6", "HIC_CTR_Ind7", "HIC_CTR_Ind8", "HIC_CTR_Ind9")
#   # varNA <- c(varNA, "CVNTempCode", "CVNTempCode_Add1", "CVN_Ind3",
#   #            "CVNTempCode_Add2", "CVNTemp_Add2",  "CVN_Ind1_Add2", "CVN_Ind2_Add2", "CVN_Ind3_Add2", "CVN_Avg_Add2", 
#   #            "CVNTempCode_Add3", "CVNTemp_Add3", "CVN_Ind1_Add3", "CVN_Ind2_Add3", "CVN_Ind3_Add3", "CVN_Avg_Add3")
#   
# 
#   
#   
#   tidyingList[["removedVarNA"]] <<- renderNameVarNA(df)
#   
#   removedVar <- c("ControlRollingCode", "ControlRollingMethod", "bControlRolling", "bRM")
#   tidyingList[["removedVar"]] <<- removedVar
#   tidyingList[["remainedVar"]] <<-
#     setdiff(colnames(df), c(tidyingList[["removedVar"]],tidyingList[["addedVar"]], tidyingList[["removedVarNA"]])  )
#   df <- df[,c(tidyingList[["remainedVar"]], tidyingList[["addedVar"]])]
#   # tidyingList[["remainedVar"]] <<- setdiff(colnames(df), tidyingList[["removedVar"]] )
#   # df <- df[,tidyingList[["remainedVar"]]]
#   return(df)
#   
# }
