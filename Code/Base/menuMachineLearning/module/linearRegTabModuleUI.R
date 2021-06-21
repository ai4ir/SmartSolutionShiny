if(!library(MASS, logical.return=TRUE)) {
    install.packages("MASS")
    library(MASS)
}  
linearRegList <- list(varX=NA, varY=NA, direction="both")
linearRegTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("linearRegVar"),"linearReg X 변수 선정")
            ),
            column(2,
                   actionButton(ns("targetVar"),"linearReg target 변수 선정")
            ),
            # column(2,
            #        numericInput(ns("maxPValue"),"최대 P값",linearRegList[["maxPValue"]], min=0.01, max=0.3, step=0.01)
            # ),
            # column(2,
            #        numericInput(ns("maxDepth"),"maximum Depth",linearRegList[["maxDepth"]], min=1, max=6)
            # ),
            column(1,
                   actionButton(ns("renderLinearReg"),"linearReg 재구성")
            ),
        ),
        fluidRow(
            verbatimTextOutput(ns("varX"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varY"))
        ),
        fluidRow( column(6, verbatimTextOutput(ns("linearReg")))
        )
    )
}

linearRegTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$linearRegVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
            union(extractDateVarName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        var <- c("ALL.ALL",var)
        choiceNames <- "All.ALL"

        for(i in 2:length(var)) {
            if(is.null(attr(curSampleExplore[,var[i]],"labelShort") )) {
                choiceNames <- c(choiceNames,paste0("임시 라벨 ", i))
            } else {
                choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
            }
        }

        showModal(ModalCheckboxGroup(title="linearReg X Var 선정 대화창", modalCheckboxID=ns("selLinearRegVarModal"),
                                     label="linearReg X Var 선정", 
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=linearRegList[["varX"]],
                                     modalOKButtonID=ns("okLinearRegVarModal")))
        
    })
    
    observeEvent(input$okLinearRegVarModal, {
        varX <- input$selLinearRegVarModal       

        if(varX[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            linearRegList[["varX"]] <<- setdiff(selVar,varX[2:length(varX)])
        } else {
            linearRegList[["varX"]] <<- varX
        }
        
        output$varX <- renderText({ 
            if (is.null(varX)) {
                "No data selected"
            } else if(varX[1]=="ALL.ALL") {
                paste0(length(linearRegList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse="-"))
            } else {
                paste0(length(linearRegList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse=","))
            }
        })

        removeModal()

    })
    
    observeEvent(input$targetVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        choiceNames <- attr(curSampleExplore[,var[1]],"labelShort")
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }
        showModal(ModalRadioButtons(choiceNames=choiceNames, choiceValues=var, ns("okLinearRegTargetVarModal"), "linearReg target Var 선정 대화창",
                                    strExplain="target 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selLinearRegTargetVarModal"),failed = FALSE))

    })
    
    observeEvent(input$okLinearRegTargetVarModal, {
        varY <- input$selLinearRegTargetVarModal       
        output$varY <- renderText({ 
            if (is.null(varY))
                "No data selected"
            else
                paste0("선정된 target 변수 : ",str_c(varY,collapse=","))
        })
        linearRegList[["varY"]] <<- varY
        removeModal()
        # updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$renderLinearReg, {
        # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # targetVar <- c("NRL_FirstTest")
        # maxDepth <- 3;

        selVar <- linearRegList[["varX"]]
        targetVar <- linearRegList[["varY"]]
        # linearRegList[["maxDepth"]] <- isolate(input$maxDepth)
        # linearRegList[["maxPValue"]] <- isolate(input$maxPValue)
        # minCriterion <- 1 - linearRegList[["maxPValue"]]
        var <- union(selVar, targetVar)
        df <- curSampleExplore[,c(var)]
        if(!is.numeric(df[,targetVar])) {
            df[,targetVar] <- as.factor(df[,targetVar])
        }
        
        boolValidRow <- !is.na(df[,targetVar])
        
        curSamplelinearReg <<- df[boolValidRow,]
        
        df <- curSamplelinearReg
        ###  sticky 가 없어야 classification tree 가 가능함
        for(v in colnames(df)) {
            df[,v] <- unstick(df[,v])
        }
        
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        linearRegResult <- lm(fmla, df)
        linearRegStep <- step(linearRegResult,direction=linearRegList[["direction"]])

        output$linearReg <- renderPrint({
            summary(linearRegStep)
        })
        

    })
    
}