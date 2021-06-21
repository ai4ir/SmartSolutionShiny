# sourcingCat <- "RExample"
# sourcingCat <- "Poongjeon"
# sourcingCat <- "Husteel"
sourcingCat <- "Hyundai"

sourcingMainUI <- function() {
  tabsetPanel( type="tabs",  id="sourcing",
               
               tabPanel("SourcingSub1",
                        fluidPage(
                          fluidRow(
                            column(2, actionButton("renderReportSourcing", "리포트") ),
                            includeCSS("Base/www/html/globalModal.css"),
                            # column(2, actionButton("renderReportCommonPlot", "그래프 리포트") ),
                            column(2, actionButton("renderReportCommonSource", "탐색 리포트") ),
                            column(4, tags$h4(":::::::") ),
                            column(2, actionButton("readSource", "소스 불러오기") ),
                            column(2, actionButton("treatVar", "변수 전처리") )
                            
                            
                          ),
                          tags$hr(),
                          fluidRow(
                            column(3,
                                   radioButtons("sourceDataOrModel", "데이타 or 모델",
                                                c("초기화"="empty",
                                                  "개발자용 데이터 : mtcars" = "mtcars",
                                                  "범용 데이터 : 엑셀"="EXCEL",
                                                  "모델"="modelSource"
                                                  )),
                                   radioButtons("modelSelectTip1", "모델 선택",
                                                c("모델 mtcars " = "mtcars"
                                                ))

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
    # shinyjs::showModal(ModalCheckboxGroup(outputFileNamesSourcingReport, outputFileNamesSourcingReport,
    #                              "okModalReportSourcing"))
    shinyjs::showModal(ModalCheckboxGroup(title="리포트 선정 대화창", modalCheckboxID="ModalCheckboxGroup", label="리포트 선정",
                                 choiceNames=outputFileFinalNamesSourcingReport, choiceValues=outputFileNamesSourcingReport,
                                 modalOKButtonID="okModalReportSourcing"))
  })
  
  observeEvent(input$okModalReportSourcing, {
    DFSourceRmd <- DFSource %>% select(-c(sampleCode, bHOT, clusterGr))
    params <- list(DFSource=DFSourceRmd)
    renderReportCheckboxGroup(input, output, session, params, outputFileNamesSourcingReport,outputFileFinalNamesSourcingReport, 
                              pathFileRmdSourcingReport, pathHTMLReport)
  })
  
  
  observe({
    sourceDataOrModel <- input$sourceDataOrModel

    if(sourceDataOrModel=="modelSource") {
      shinyjs::show("modelSelectTip1")
      shinyjs::show("readSource")
    } else {
      shinyjs::hide("modelSelectTip1")
    }
    
  })
  
  observeEvent(input$readSource, {
    shinyjs::toggle("readSource")
    
    output$strDFsource <- renderPrint({
      theme_update(axis.title=element_text(size=40))
      theme_update(axis.text=element_text(size=30))
      update_geom_defaults("point", list(size = 8))
      # theme_update(text=element_text(size=30))
      guides(color = guide_legend(override.aes = list(size = 8)))
      chosenDFSourceFile <<- ""
      hide("renderReportSourcing")
      hide("treatVar")
      shinyjs::show("fileDomain")
      hide("renderReportSampling")   # 맞춤 리포팅
      shinyjs::show("renderReportCommonSample")
      # hide("domainFromSource")
      shinyjs::show("initCurSampleExplore")
      shinyjs::show("areaTreat")
      hide("remainVar")
      hide("calcMeanDesign")
      # hide("renderReportCommonSource") 
      
      aesList <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
                       axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
                       legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
      aesListScatter <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
                              axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
                              legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
      sourceDataOrModel <- isolate(input$sourceDataOrModel)
      modelSelectTip1 <- isolate(input$modelSelectTip1)

      if(sourceDataOrModel=="modelSource") {
        pathFileModelSource <- paste0("../Model/",modelSelectTip1,"/loadSource.R")
      }

      switch(sourceDataOrModel,
             modelSource = {
               if(str_detect(pathFileModelSource,"mtcars")[1] | 
                  str_detect(pathFileModelSource,"_CR_")[1] | 
                  str_detect(pathFileModelSource,"_HT_")[1]) {
                 showTab(inputId="SmartReportMain", target="Sampling")
                 showTab(inputId="SmartReportMain", target="Explore")
                 showTab(inputId="SmartReportMain", target="Modeling")
                 showTab(inputId="SmartReportMain", target="Predict")
                 showTab(inputId="SmartReportMain", target="Optimize")
                 source(pathFileModelSource, encoding="UTF-8")
                 loadSource()
               } else {
                 alert("모델 개발중입니다.  긴급하면 연락주세요.")
                 toggle("readSource")
                 return()
               }
             },


             # model_HeatTreat_TS = {
             #   showTab(inputId="SmartReportMain", target="Modeling")
             #   showTab(inputId="SmartReportMain", target="Predict")
             #   showTab(inputId="SmartReportMain", target="Optimize")
             #   source("../Model/열처리재/model_HeatTreat_TS/loadSource.R", encoding="UTF-8")
             #   loadSource()
             # },

             model_Mine = {
               showTab(inputId="SmartReportMain", target="Modeling")
               showTab(inputId="SmartReportMain", target="Predict")
               showTab(inputId="SmartReportMain", target="Optimize")
               source("../Model/model_Mine/loadSource.R", encoding="UTF-8")
               loadSource()
             },
             mtcars = {
               shinyjs::show("renderReportSourcing")
               source("each/menuSourcing/mtcars/loadSource.R", encoding="UTF-8")
               loadSource()
             },
             mtcarsModel = {
               source("../Model/mtcars/loadSource.R", encoding="UTF-8")
               loadSource()
             },
             diamonds = {
               source("each/menuSourcing/diamonds/loadSource.R", encoding="UTF-8")
               loadSource()
             },
             EXCEL = {
               # shinyjs::show("treatVar")
               # shinyjs::show("renderReportCommonSource")
               hideTab(inputId="SmartReportMain", target="Modeling")
               hideTab(inputId="SmartReportMain", target="Predict")
               hideTab(inputId="SmartReportMain", target="Optimize")
               source("each/menuSourcing/EXCEL/loadSource.R", encoding="UTF-8")
               loadSource()
               
               
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
               # aesList <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
               #                  axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
               #                  legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
               # aesListScatter <<- list(x=NA, y=NA, color=NULL, size=NULL, shape=NULL, fill=NULL, grid=NULL, tooltip=NULL, data_id="rowNoSource", 
               #                         axisTitleSize=40, axisTextSize=30, pointSize=3, colorLegendPointSize=8, 
               #                         legendTitleSize=40, legendTextSize=25, fitOption="NoFit", clusterMethod=NA, spare1=NULL, pValueVector=NA)
               return()
             },
             {}
             
      )
      
      
      
      
      
      if(!("bHOT" %in% colnames(DFSource))) {
        bHOT <- rep("Normal", NROW(DFSource))
        attr(bHOT, "label") <- "bHOT" ;   attr(bHOT, "labelShort") <- "bHOT" ; 
        attr(bHOT, "validMax") <- NA ;   attr(bHOT, "validMin") <- NA ; 
        clusterGr <- rep("1", NROW(DFSource))
        attr(clusterGr, "label") <- "clusterGr" ;     attr(clusterGr, "labelShort") <- "클러스터" ; 
        attr(clusterGr, "validMax") <- NA ;   attr(clusterGr, "validMin") <- NA ; 
        sampleCode <- rep("remains", NROW(DFSource))
        attr(sampleCode, "label") <- "sampleCode" ;     attr(sampleCode, "labelShort") <- "샘플 코드" ; 
        attr(sampleCode, "validMax") <- NA ;   attr(sampleCode, "validMin") <- NA ; 
        rowNoSource <- as.numeric(row.names(DFSource))
        attr(rowNoSource, "label") <- "rowNoSource" ;  attr(rowNoSource, "labelShort") <- "rowNoSource" ;  
        attr(rowNoSource, "validMax") <- NA ;   attr(rowNoSource, "validMin") <- NA ; 
        
        DFSource <<- cbind( DFSource,sampleCode, bHOT, clusterGr, rowNoSource )
        DFSource <<- as.data.frame(DFSource)

      }
      
      if(!is.null(curSelModel)) {
        DFSource <<- DFSource %>%  add_predictions(curSelModel, var="predVal")
        attr(DFSource$predVal, "label") <<- paste0("예측 ",curSelModelY) ; 
        attr(DFSource$predVal, "labelShort") <<- paste0("예측 ",curSelModelY) ; 
        attr(DFSource$predVal, "validMax") <- NA ;   attr(DFSource$predVal, "validMin") <- NA ; 
        numVarConst <- c("predVal")
        decimalVec <- vapply(numVarConst, renderDigitVector,  FUN.VALUE=numeric(1), DFSource)
        decimalVec <- as.vector(decimalVec)
        for(x in numVarConst) {
          attr(DFSource[,x], "max") <- max(DFSource[,x], na.rm=TRUE)
          attr(DFSource[,x], "mean") <- mean(DFSource[,x], na.rm=TRUE)
          attr(DFSource[,x], "min") <- min(DFSource[,x], na.rm=TRUE)
          attr(DFSource[,x], "digit") <- decimalVec[which(numVarConst==x)]
        }
      }
      

      DFSource <<- as.data.frame(DFSource) %>% sticky_all()  # sticky_all을 해야 subsetting시 bHOT 등의 attr이 유지됨
      
      reactDFSource(DFSource)
      curSampleExplore <<- DFSource
      curDesignDF <<- DFSource[1,]
      curOptimizeDF <<- DFSource[1,]
      

      str(DFSource)
      toggle("readSource")
      
    })   
    
  })
  
  
  
  
  
}