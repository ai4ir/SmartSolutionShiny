

exploreTable1TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1, actionButton(ns("rowVar"), label="행 변수 선정") 
            ),
            column(1  #, actionButton(ns("colVar"), label="열 변수 선정")
            ),
            column(1
            ),
            column(1
            ),
            column(1
            ),
            column(2 
            ),
            column(2
            ),
            column(2 
            ),
            column(1  #, actionButton(ns("tableUpdate"), label="Table 갱신") 
            )

        ),
        
        DT::dataTableOutput(ns("table"))
        
    )
}

exploreTable1TabModule <- function(input, output, session) {
    ns <-session$ns
    
    observeEvent(input$rowVar, {
        catVar <- extractCatVarName(curSampleExplore)
        showModal(ModalRadioButtons(choiceNames=catVar, choiceValues=catVar,
                                    okButtonName=ns("okRowVar"), labelStr="행변수 선정",
                                    strExplain="행변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModalRowVar"),
                                    failed = FALSE))
    })
    
    

    observeEvent(input$okRowVar, {
       rowVar <- input$selModalRowVar
       colVar <- "bHOT"
       removeModal()
       if(length(unique(curSampleExplore[,colVar]))==1) {
           alert("양분화가 안되었습니다. 양분화 후 실행하세요!!!")
           return()
       }
       output$table <- renderPrint({ 
           tab <- table(curSampleExplore[,rowVar], curSampleExplore[,colVar])
           dfSpread <- tab %>% as.data.frame() %>%
               spread(key=Var2, value=Freq)
           colnames(dfSpread)[1] <- input$selModalRowVar
           dfSpread <- dfSpread %>% mutate(inSide=round(100* Hot/(Hot+Normal),2),
                                           outSide=round(100*Normal/(Hot+Normal),2))
           dfSpread <- dfSpread[order(dfSpread$outSide),]
           output$table <- DT::renderDataTable(DT::datatable({dfSpread}))
           
       }) 

    })
     
        
    


}