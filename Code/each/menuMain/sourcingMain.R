# sourcingCat <- "RExample"
# sourcingCat <- "Poongjeon"
# sourcingCat <- "Seah"
# sourcingCat <- "POSCO"
sourcingCat <- "SmartShiny"



sourcingMainUI <- function() {
  tabsetPanel( type="tabs",
               
    tabPanel("SourcingSub1",
      fluidPage(
        fluidRow(
          column(2, actionButton("renderReportSourcing", "리포트") ),
          includeCSS("Base/www/html/globalModal.css"),
          # column(2, actionButton("renderReportCommonPlot", "탐색 리포트") ),
          column(2, actionButton("renderReportCommonSource", "탐색 리포트") ),
          column(6, tags$h4(":::::::") ),
          column(2, actionButton("treatVar", "변수 전처리") )
          
          
        ),
        tags$hr(),
        fluidRow(
          column(3,
                 switch(sourcingCat,
                        SmartShiny = {radioButtons("source", "원천 데이타",
                                             c("empty",
                                               "mtcars"="mtcars",
                                               "mtcars 모델" = "mtcarsModel",
                                               "diamonds"="diamonds",
                                               "엑셀"="EXCEL"
                                               #"clipboard"
                                             ))}    
                 )
          ),
          column(9,
                 verbatimTextOutput("strDFsource"))
        )
      )

    )
  )
  

}

sourcingMain <- function(input, output, session) {
  
  # 범용 리포트
  treatSourcingMainEventBase(input, output,session)
  
  # 맞춤 리포트
  observeEvent(input$renderReportSourcing, {
    # showModal(ModalCheckboxGroup(outputFileNamesSourcingReport, outputFileNamesSourcingReport,
    #                              "okModalReportSourcing"))
    showModal(ModalCheckboxGroup(title="리포트 선정 대화창", modalCheckboxID="ModalCheckboxGroup", label="리포트 선정",
                                 choiceNames=outputFileFinalNamesSourcingReport, choiceValues=outputFileNamesSourcingReport,
                                 modalOKButtonID="okModalReportSourcing"))
  })
  
  observeEvent(input$okModalReportSourcing, {
    DFSourceRmd <- DFSource %>% select(-c(sampleCode, bHOT, clusterGr))
    params <- list(DFSource=DFSourceRmd, pathHTMLReport=pathHTMLReport)
    renderReportCheckboxGroup(input, output, session, params, outputFileNamesSourcingReport,outputFileFinalNamesSourcingReport, 
                         pathFileRmdSourcingReport, pathHTMLReport)
  })
  

  
  output$strDFsource <- renderPrint({
    theme_update(axis.title=element_text(size=40))
    theme_update(axis.text=element_text(size=30))
    update_geom_defaults("point", list(size = 8))
    # theme_update(text=element_text(size=30))
    guides(color = guide_legend(override.aes = list(size = 8)))
    chosenDFSourceFile <<- ""
    hide("renderReportSourcing")
    hide("treatVar")
    # hide("fileDomain")
    hide("renderReportSampling")
    show("renderReportCommonSample")
    # hide("domainFromSource")
    # hide("initCurSampleExplore")
    show("areaTreat")
    hide("remainVar")
    hide("calcMeanDesign")
    # hide("renderReportCommonSource") 
    
    aesList <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
                     axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
                     legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
    aesListScatter <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
                            axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
                            legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
    

    switch(input$source,
           mtcars = {
             source("each/menuSourcing/mtcars/loadSource.R", encoding="UTF-8")
             loadSource()
             showTab(inputId="SmartReportMain", target="Sampling")
             showTab(inputId="SmartReportMain", target="Explore")
             showTab(inputId="SmartReportMain", target="ExploreTable")
             showTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
           },
           mtcarsModel = {
             source("../Model/mtcars/loadSource.R", encoding="UTF-8")
             loadSource()
             showTab(inputId="SmartReportMain", target="Sampling")
             showTab(inputId="SmartReportMain", target="Explore")
             showTab(inputId="SmartReportMain", target="ExploreTable")
             showTab(inputId="SmartReportMain", target="Modeling")
             showTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
           },
           diamonds = {
             source("each/menuSourcing/diamonds/loadSource.R", encoding="UTF-8")
             loadSource()
             showTab(inputId="SmartReportMain", target="Sampling")
             showTab(inputId="SmartReportMain", target="Explore")
             showTab(inputId="SmartReportMain", target="ExploreTable")
             hideTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
           },
           EXCEL = {
             # show("treatVar")
             # show("renderReportCommonSource")
             source("each/menuSourcing/EXCEL/loadSource.R", encoding="UTF-8")
             loadSource()
             showTab(inputId="SmartReportMain", target="Sampling")
             showTab(inputId="SmartReportMain", target="Explore")
             showTab(inputId="SmartReportMain", target="ExploreTable")
             hideTab(inputId="SmartReportMain", target="Modeling")
             hideTab(inputId="SmartReportMain", target="Predict")
             hideTab(inputId="SmartReportMain", target="Optimize")
            

           },
           clipboard = {
               DFSource <<- read.delim("clipboard", stringsAsFactors = T) 
               # 빈 InsertReqInputUI 함수 정의
               source("each/menuSourcing/clipboard/function/InsertReqInputUI.R", encoding="UTF-8")
           },
           DEVELOPER = {
               source(file=file.choose(), encoding="UTF-8")
               df <- myCode()
               DFSource <<- as.data.frame(df)
               # 빈 InsertReqInputUI 함수 정의
               source("each/menuSourcing/DEVELOPER/function/InsertReqInputUI.R", encoding="UTF-8")
           },
           empty = {
               return()
           },
           {}

    )
 #     browser()
 
    func1 <- function(x) {
      if(is.na(x)) {"Normal"} 
      else {"Hot"}
    }   
    
    switch(input$source,
           erae2nd = {
             bHOT <- vapply(DFSource[,"specimenNo"], func1, FUN.VALUE=character(1))
           },
           erae3rd = {
             bHOT <- vapply(DFSource[,"specimenNo"], func1, FUN.VALUE=character(1))
           },
           {bHOT <- rep("Normal", NROW(DFSource))}
    )
    

    if(!("bHOT" %in% colnames(DFSource))) {
      bHOT <- rep("Normal", NROW(DFSource))
      clusterGr <- rep("1", NROW(DFSource))
      sampleCode <- rep("remains", NROW(DFSource))
      rowNoSource <- row.names(DFSource)
      attr(bHOT, "label") <- "bHOT" ; 
      attr(bHOT, "labelShort") <- "bHOT" ; 
      attr(clusterGr, "label") <- "clusterGr" ; 
      attr(clusterGr, "labelShort") <- "클러스터" ; 
      attr(sampleCode, "label") <- "sampleCode" ; 
      attr(sampleCode, "labelShort") <- "샘플 코드" ; 
      attr(rowNoSource, "label") <- "rowNoSource" ; 
      attr(rowNoSource, "labelShort") <- "rowNoSource" ;  
      
      validMax <- attr(DFSource,"validMax")
      validMin <- attr(DFSource,"validMin")
      DFSource <<- cbind( DFSource,sampleCode, bHOT, clusterGr, rowNoSource )
      DFSource <<- as.data.frame(DFSource)
      attr(DFSource,"validMax") <<- validMax
      attr(DFSource,"validMin") <<- validMin
    }

    DFSource <<- as.data.frame(DFSource)
    
    DFSource <- sticky_all(DFSource)
    
    
    reactDFSource(DFSource)
    curSampleExplore <<- DFSource
    curDesignDF <<- DFSource[1,]
    curOptimizeDF <<- DFSource[1,]
      
    str(DFSource)

  })   
    

}