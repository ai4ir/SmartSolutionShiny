### 철강 후판의 변환변수를 생성하여 df에 추가하는 함수

addTransVarTaskSteelPlate <- function(df) {
  
  #페라이트상변태시작온도
  df <- dplyr::mutate(df, Ar3 = (((1670-558*(C+(Mn+Mo)/3.875+Cu/15.5+Cr/20.67+Ni/5.636)+16*((PlateThickness/25.4)-0.315)-32)*(5/9))))
  attr(df[,"Ar3"], "label") <- "Ar3(C)" ; 
  attr(df[,"Ar3"], "labelShort") <- "Ar3" ; 

  #고용온도계산
  # df <- dplyr::mutate(df, Intemp = (-7510/(-2.96+log10(Nb*C)))-273)
  #Nb고용도
  # df$InNb = ifelse(df$Intemp < df$SRT_SlabTemp,0,-6770/(df$SRT_SlabTemp)+2.26)
  #Bs
  df <- dplyr::mutate(df, Bs = 656-58*C-35*Mn-75*Si-15*Ni-34*Cr-41*Mo)
  attr(df[,"Bs"], "label") <- "Bs(C)" ; 
  attr(df[,"Bs"], "labelShort") <- "Bs" ;
  
  
  #Ms
  df <- dplyr::mutate(df, Ms = 529-423*C-30.4*Mn-17.7*Ni-12.1*Cr-11.0*Si-7.0*Mo)
  attr(df[,"Ms"], "label") <- "Ms(C)" ; 
  attr(df[,"Ms"], "labelShort") <- "Ms" ;
  
  #Tnr
  df <- df %>% mutate(df, Tnr = 887+464*C+890*Ti+363*S_Al-357*Si+(6445*(Nb*0.80)-644*(sqrt(Nb*0.80))+(732*V-230*(sqrt(V)))))
  attr(df[,"Tnr"],"label") <- "Tnr" ;
  attr(df[,"Tnr"], "labelShort") <- "Tnr";
  
  tidyingList[["addedVar"]] <<- c("Ar3", "Tnr", "Bs", "Ms")
  
  return(df)
}
