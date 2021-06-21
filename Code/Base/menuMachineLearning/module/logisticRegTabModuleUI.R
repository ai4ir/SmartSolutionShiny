# if(!library(logisticReg, logical.return=TRUE)) {
#     install.packages("logisticReg")
#     library(logisticReg)
# }
logisticRegList <- list(varX=NA, varY=NA, num.trees = 100)
logisticRegTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("logisticRegVar"),"logisticReg X 변수 선정")
            ),
            column(2,
                   actionButton(ns("targetVar"),"logisticReg target 변수 선정")
            ),
            column(2,
                   # numericInput(ns("maxPValue"),"최대 P값",0.05, min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   # numericInput(ns("maxDepth"),"maximum Depth",2, min=1, max=6)
            ),
            column(1,
                   actionButton(ns("renderLogisticReg"),"logisticReg 재구성")
            ),
        ),
        fluidRow(
            verbatimTextOutput(ns("varX"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varY"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varImportance"))
        ),
        fluidRow( column(6, plotOutput(ns("logisticReg")))

        )
        

    )
}

logisticRegTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$logisticRegVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
            union(extractDateVarName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        var <- c("ALL.ALL",var)
        choiceNames <- "All.ALL"
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }

        showModal(ModalCheckboxGroup(title="logisticReg X Var 선정 대화창", modalCheckboxID=ns("selLogisticRegVarModal"),
                                     label="logisticReg X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     modalOKButtonID=ns("okLogisticRegVarModal")))
        
    })
    
    observeEvent(input$okLogisticRegVarModal, {
        varX <- input$selLogisticRegVarModal       
        output$varX <- renderText({ 
            if (is.null(varX)) {
                "No data selected"
            } else if(varX[1]=="ALL.ALL") {
                paste0("선정된 X 변수 : ",str_c(varX,collapse="-"))
            } else {
                paste0("선정된 X 변수 : ",str_c(varX,collapse=","))
            }
        })
        
        if(varX[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            logisticRegList[["varX"]] <<- setdiff(selVar,varX[2:length(varX)])
        } else {
            logisticRegList[["varX"]] <<- varX
        }
        removeModal()

    })
    
    observeEvent(input$targetVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        choiceNames <- attr(curSampleExplore[,var[1]],"labelShort")
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }
        showModal(ModalRadioButtons(choiceNames=choiceNames, choiceValues=var, ns("okLogisticRegTargetVarModal"), "logisticReg target Var 선정 대화창",
                                    strExplain="target 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selLogisticRegTargetVarModal"),failed = FALSE))

    })
    
    observeEvent(input$okLogisticRegTargetVarModal, {
        varY <- input$selLogisticRegTargetVarModal       
        output$varY <- renderText({ 
            if (is.null(varY))
                "No data selected"
            else
                paste0("선정된 target 변수 : ",str_c(varY,collapse=","))
        })
        logisticRegList[["varY"]] <<- varY
        removeModal()
        # updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$renderLogisticReg, {
        alert("개발중입니다. 신속한 개발이 필요하면 연락 주세요.")
        return()
        # # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # # targetVar <- c("NRL_FirstTest")
        # # maxDepth <- 3;
        selVar <- logisticRegList[["varX"]]
        targetVar <- logisticRegList[["varY"]]
        # maxDepth <- isolate(input$maxDepth)
        # maxPValue <- isolate(input$maxPValue)

        var <- union(selVar, targetVar)
        df <- curSampleExplore[,c(var)]
        if(!is.numeric(df[,targetVar])) {
            df[,targetVar] <- as.factor(df[,targetVar])
        }

        boolValidRow <- !is.na(df[,targetVar])

        curSamplelogisticReg <<- df[boolValidRow,]

        df <- curSamplelogisticReg
        df <- df[complete.cases(df),]


        ###  sticky 가 없어야 classification tree 가 가능함
        for(v in colnames(df)) {
            df[,v] <- unstick(df[,v])
        }

        df_train <- df
        t.formula <- paste0(targetVar, " ~ .")

        rf  <- logisticReg(formula = eval(parse(text = (t.formula))),importance = "permutation",
                      data = df_train, num.threads = 20, seed = 1000,
                      num.trees = logisticRegList[["num.trees"]], respect.unordered.factors = TRUE)

        pred.nrl <- predict(rf, data=df)

        # rf$variable.importance

        result <- pred.nrl$predictions

        output$varImportance <- renderPrint({
            rf$variable.importance
        })

        output$logisticReg <- renderPlot({
            plot(df[,targetVar], result)
        })


    })
    
}