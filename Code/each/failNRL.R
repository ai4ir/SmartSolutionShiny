df <- DFSource
df <- df[df$PLATE_WO_T==100,]
dim(df)
df <- df[,c("PL_REFUR_SHBED_INFUR_HR", "NRL_FirstTest")]
df <- df[order(df$PL_REFUR_SHBED_INFUR_HR,decreasing=TRUE), ]
df <- df %>% mutate(bFail=ifelse(df$NRL_FirstTest=="Break",1,0))  
df <- df %>%   mutate(cumFail=cumsum(bFail))
df <- df %>%   mutate(cumTrial=1:dim(df)[[1]])
df <- df %>%   mutate(cumFailPercent=cumFail/cumTrial*100)
ggplot(df) + geom_point(aes(x=PL_REFUR_SHBED_INFUR_HR, y=cumFailPercent)) + labs(x="균열시간(분)", y="누적 불량률(%)")
