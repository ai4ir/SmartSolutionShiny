############################################ mtcars ##############################
tidySource <- function() {

  dfOrg <- mtcars
  
  dfOrg$vs <- as.factor(dfOrg$vs)
  dfOrg <- dfOrg %>%  mutate(vs=fct_recode(vs,
                                    "V-shaped"="0",
                                    "straight"="1"))
  dfOrg$am <- as.factor(dfOrg$am)
  dfOrg <- dfOrg %>%  mutate(am=fct_recode(am,
                                           "automatic"="0",
                                           "manual"="1"))
  
  filePath <- "../SourceData/mtcars/mtcars_meta.xlsx"
  df <- attachAttr(filePath, dfOrg)

  DFSource <<- df
  
  write_rds(DFSource, "../SourceData/mtcars/mtcars_전처리.rds")
  write_excel_csv(DFSource, path="../USER/mtcars/output/mtcars_전처리.csv", na="")

}

