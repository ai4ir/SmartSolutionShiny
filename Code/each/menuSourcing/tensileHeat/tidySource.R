############################################ salesRecord ##############################
tidySource <- function() {

  # df1 <- read_xlsx("../SourceData/tensileHeat/210312/16_20년도수정(0312)/16년_중복제거_210312.xlsx",
  #                             sheet=1,
  #                             skip=2, col_names=FALSE)
  # write_rds(df1, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/16년_중복제거_210312.rds")
  # 
  # df1 <- read_xlsx("../SourceData/tensileHeat/210312/16_20년도수정(0312)/17년_중복제거_210312.xlsx",
  #                  sheet=1,
  #                  skip=2, col_names=FALSE)
  # write_rds(df1, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/17년_중복제거_210312.rds")
  # 
  # df1 <- read_xlsx("../SourceData/tensileHeat/210312/16_20년도수정(0312)/18년_중복제거_210312.xlsx",
  #                  sheet=1,
  #                  skip=2, col_names=FALSE)
  # write_rds(df1, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/18년_중복제거_210312.rds")
  # 
  # df1 <- read_xlsx("../SourceData/tensileHeat/210312/16_20년도수정(0312)/19년_중복제거_210312.xlsx",
  #                  sheet=1,
  #                  skip=2, col_names=FALSE)
  # write_rds(df1, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/19년_중복제거_210312.rds")
  # 
  # df1 <- read_xlsx("../SourceData/tensileHeat/210312/16_20년도수정(0312)/20년_중복제거_210312.xlsx",
  #                  sheet=1,
  #                  skip=2, col_names=FALSE)
  # write_rds(df1, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/20년_중복제거_210312.rds")
  chosenDFSourceFile <<- "tensileHeat"
  
  withProgress(message="전처리 진행중", value=0, {
    incProgress(0.1)
    df <- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/16년_중복제거_210312.rds")
    df2 <- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/17년_중복제거_210312.rds") 
    df <- rbind(df, df2)
    df2 <- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/18년_중복제거_210312.rds") 
    df <- rbind(df, df2)
    df2 <- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/19년_중복제거_210312.rds") 
    df <- rbind(df, df2)
    df2 <- read_rds("../SourceData/tensileHeat/210312/16_20년도수정(0312)/20년_중복제거_210312.rds") 
    df <- rbind(df, df2)
    
    incProgress(0.2)
    
    filePath <- "../SourceData/tensileHeat/210312/16_20년도수정(0312)/tensileHeat_meta_210312_중복삭제_valid.xlsx"
    df <- attachAttr(filePath, df)
    
    df <- df[df$TensileShape %in% c("판상", "봉상"),]
    identifierGrCMn <- c("XX")
    # identifierGrNbVTi <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준.xls",
    #                  sheet=1, skip=2, range="B3:B42",col_names=FALSE))[,1] %>%  as.vector()
    # identifierGrCrMo <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준.xls",
    #                                             sheet=1, skip=2, range="C3:C182",col_names=FALSE))[,1] %>%  as.vector()
    # identifierGrCuNi <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준.xls",
    #                                             sheet=1, skip=2, range="D3:D56",col_names=FALSE))[,1] %>%  as.vector()
    identifierGrNbVTi <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준_수정본.xls",
                                                sheet=1, skip=2, range="B3:B22",col_names=FALSE))[,1] %>%  as.vector()
    identifierGrCrMo <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준_수정본.xls",
                                               sheet=1, skip=2, range="C3:C51",col_names=FALSE))[,1] %>%  as.vector()
    identifierGrCuNi <- as.data.frame(read_xls("../SourceData/tensileHeat/210312/16_20년도수정(0312)/출강목표별 합금성분 구분기준_수정본.xls",
                                               sheet=1, skip=2, range="D3:D86",col_names=FALSE))[,1] %>%  as.vector()
    func1 <- function(identifier) {
      if(identifier %in% identifierGrCuNi) {
        grCCC <- "CuNi"
      } else if(identifier %in% identifierGrCrMo) {
        grCCC <- "CrMo"
      } else if(identifier %in% identifierGrNbVTi) {
        grCCC <- "NbVTi"
      } else if(identifier %in% identifierGrCMn) {
        grCCC <- "CMn"
      } else {
        grCCC <- "GrFail"     
      }
      return(grCCC)
    }
    incProgress(0.3)
    grCCC <- vapply(str_sub(df[,"ChemicalCompositionCode"],9,10),func1, FUN.VALUE=character(1))
    df <- df %>% mutate(grCCC=grCCC)
    df <- as.data.frame(df)
    attr(df[,"grCCC"], "label") <- "출강목표 그룹" ; 
    attr(df[,"grCCC"], "labelShort") <- "출강목표 그룹" ; 
    
    # setdiff(unique(str_sub(df$ChemicalCompositionCode,9,10)),c(identifierGrCMn,identifierGrNbVTi,identifierGrCrMo,identifierGrCuNi))
    
    bVec1 <- df$Nb<0.003 & df$V<0.0015 & df$Ti<0.0035 & df$Cu<0.07 & df$Ni<0.035 & df$Cr<0.07 & df$Mo<0.015
    bVec2 <- df$Mn>0.8 & df$Cu<0.07 & df$Ni<0.035 & df$Cr<0.07 & df$Mo<0.015
    bVec3 <- df$Mn>0.8 & df$Cr<0.07 & df$Mo<0.015 
    bVec4 <- df$Mn>0.8
    
    grCCC2 <- ifelse(bVec1,"CMn", ifelse(bVec2,"NbVTi",ifelse(bVec3,"CuNi",ifelse(bVec4,"CrMo","etc"))))
    
    df <- df %>% mutate(grCCC2=grCCC2)
    attr(df[,"grCCC2"], "label") <- "출강목표 그룹2" ;
    attr(df[,"grCCC2"], "labelShort") <- "출강목표 그룹2" ;
    
    
    df <- addTransVarTaskSteelPlate(df)
    df <- sticky_all(df)
    dfAll <<- df
    
    df <- removeVarEachTensileHeat(df)
    df <- sticky_all(df)
    DFSource <<- df
 
    incProgress(0.5)
    # attributes(DFSource) 
    write_rds(DFSource, "../SourceData/tensileHeat/210312/16_20년도수정(0312)/tensileHeat_전처리.rds")
    write_excel_csv(DFSource, path="../USER/tensileHeat/output/tensileHeat(sourcing).csv", na="")
    
    pathTidyingReport <<- "../USER/tensileHeat/output"
    incProgress(0.8)
    
    return(df)
    
    

    
  }) 
 

}


removeVarEachTensileHeat <- function(df) {
  tidyingList[["removedVar"]] <<- c("ControlRollingCode", "ControlRollingMethod", "bControlRolling", "bRM")
  tidyingList[["remainedVar"]] <<-
    setdiff(colnames(df), c(tidyingList[["removedVar"]],tidyingList[["addedVar"]])  )
  df <- df[,c(tidyingList[["remainedVar"]], tidyingList[["addedVar"]])]
  # tidyingList[["remainedVar"]] <<- setdiff(colnames(df), tidyingList[["removedVar"]] )
  # df <- df[,tidyingList[["remainedVar"]]]
  return(df)
  
}
