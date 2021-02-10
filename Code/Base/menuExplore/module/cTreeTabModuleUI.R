cTreeList <- list(varX=NA, varY=NA)
cTreeTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("cTreeVar"),"CTree X 변수 선정")
            ),
            column(2,
                   actionButton(ns("targetVar"),"CTree target 변수 선정")
            ),
            column(2,
                   numericInput(ns("maxPValue"),"최대 P값",0.05, min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   numericInput(ns("maxDepth"),"maximum Depth",2, min=1, max=6)
            ),
            column(1,
                   actionButton(ns("renderCTree"),"CTree 재구성")
            ),
        ),
        fluidRow(
            verbatimTextOutput(ns("varX"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varY"))
        ),
        fluidRow( column(6, plotOutput(ns("CTree")))

        )
        

    )
}

cTreeTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$cTreeVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
            union(extractDateVarName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        var <- c("ALL.ALL",var)
        choiceNames <- "All.ALL"
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }

        showModal(ModalCheckboxGroup(title="CTree X Var 선정 대화창", modalCheckboxID=ns("selCTreeVarModal"),
                                     label="CTree X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     modalOKButtonID=ns("okCTreeVarModal")))
        
    })
    
    observeEvent(input$okCTreeVarModal, {
        varX <- input$selCTreeVarModal       
        output$varX <- renderText({ 
            if (is.null(varX))
                "No data selected"
            else
                paste0("선정된 X 변수 : ",str_c(varX,collapse=","))
        })
        cTreeList[["varX"]] <<- varX
        removeModal()

    })
    
    observeEvent(input$targetVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        choiceNames <- attr(curSampleExplore[,var[1]],"labelShort")
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }
        showModal(ModalRadioButtons(choiceNames=choiceNames, choiceValues=var, ns("okCTreeTargetVarModal"), "CTree target Var 선정 대화창",
                                    strExplain="target 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selCTreeTargetVarModal"),failed = FALSE))

    })
    
    observeEvent(input$okCTreeTargetVarModal, {
        varY <- input$selCTreeTargetVarModal       
        output$varY <- renderText({ 
            if (is.null(varY))
                "No data selected"
            else
                paste0("선정된 target 변수 : ",str_c(varY,collapse=","))
        })
        cTreeList[["varY"]] <<- varY
        removeModal()
        # updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$renderCTree, {
        # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # targetVar <- c("NRL_FirstTest")
        # maxDepth <- 3;
        if(cTreeList[["varX"]][1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
        } else {
            selVar <- cTreeList[["varX"]]
        }
        targetVar <- cTreeList[["varY"]]
        maxDepth <- isolate(input$maxDepth)
        maxPValue <- isolate(input$maxPValue)
        minCriterion <- 1 - maxPValue
        var <- union(selVar, targetVar)
        df <- curSampleExplore[,c(var)]
        if(!is.numeric(df[,targetVar])) {
            df[,targetVar] <- as.factor(df[,targetVar])
        }
        
        boolValidRow <- !is.na(df[,targetVar])
        
        curSampleCTree <<- df[boolValidRow,]
        
        df <- curSampleCTree
        

        ###  sticky 가 없어야 classification tree 가 가능함
        for(v in colnames(df)) {
            df[,v] <- unstick(df[,v])
        }
        
        treeControl <- ctree_control(mincriterion=minCriterion, maxdepth=maxDepth, stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        cTreeResult <- ctree(fmla, df, controls=treeControl)

        output$CTree <- renderPlot({
            plot(cTreeResult)
        })
        

    })
    
}