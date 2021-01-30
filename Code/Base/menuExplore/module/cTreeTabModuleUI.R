cTreeTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("cTreeVar"),"CTree 변수 선정")
            ),
            column(2,
                   selectInput(ns("targetVar"),"Target Variable",choices="empty")
            ),
            column(2,
                   numericInput(ns("maxDepth"),"maximum Depth",2, min=1, max=6)
            ),
            column(1,
                   actionButton(ns("renderCTree"),"CTree 재구성")
            ),
        ),
        fluidRow(
            verbatimTextOutput(ns("value"))
        ),
        fluidRow( column(6, plotOutput(ns("CTree")))

        )
        

    )
}

cTreeTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$cTreeVar, {

        numVar <- rownames(orderVFcluster(curSampleExplore))
        showModal(ModalCheckboxGroup(title="CTree Var 선정 대화창", modalCheckboxID=ns("selCTreeVarModal"),
                                     label="CTree Var 선정",
                                     choiceNames=numVar, choiceValues=numVar,
                                     modalOKButtonID=ns("okCTreeVarModal")))
        
    })
    
    observeEvent(input$okCTreeVarModal, {
        selVar <- input$selCTreeVarModal       
        output$value <- renderText({ 
            if (is.null(selVar))
                "No data selected"
            else
                paste0("선정된 변수 : ",str_c(selVar,collapse=","))
        })
        removeModal()
        updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$renderCTree, {
        selVar <- isolate(input$selCTreeVarModal)
        targetVar <- isolate(input$targetVar)
        maxDepth <- isolate(input$maxDepth)
        df <- curSampleExplore[,c(targetVar,selVar)]
        if(!is.numeric(df[,targetVar])) {
            df[,targetVar] <- as.factor(df[,targetVar])
        }
        
        # boolValidRow <- !is.na(df[,selVar[1]])
        # for(i in 2:length(selVar)) {
        #     boolValid <- !is.na(df[,selVar[i]])
        #     boolValidRow <- boolValidRow & boolValid
        # }
        
        boolValidRow <- !is.na(df[,targetVar])
        
        curSampleCTree <<- df[boolValidRow,]
        
        df <- curSampleCTree
        
        treeControl <- ctree_control(maxdepth=maxDepth)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        cTreeResult <- ctree(fmla, df, controls=treeControl)
        
        output$CTree <- renderPlot({
            plot(cTreeResult)
        })
        

    })
    
}