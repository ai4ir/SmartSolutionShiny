if(!library(randomForest, logical.return=TRUE)) {
    install.packages("randomForest")
    library(randomForest)
}
randomForestList <- list(varX=NA, varY=NA, num.trees = 100)
randomForestTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("randomForestVar"),"randomForest X 변수 선정")
            ),
            column(2,
                   actionButton(ns("targetVar"),"randomForest target 변수 선정")
            ),
            column(2,
                   # numericInput(ns("maxPValue"),"최대 P값",0.05, min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   # numericInput(ns("maxDepth"),"maximum Depth",2, min=1, max=6)
            ),
            column(1,
                   actionButton(ns("renderRandomForest"),"randomForest 재구성")
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
        fluidRow( column(6, plotOutput(ns("randomForest")))

        )
        

    )
}

randomForestTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$randomForestVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
            union(extractDateVarName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        var <- c("ALL.ALL",var)
        choiceNames <- "All.ALL"
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }

        showModal(ModalCheckboxGroup(title="randomForest X Var 선정 대화창", modalCheckboxID=ns("selRandomForestVarModal"),
                                     label="randomForest X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=randomForestList[["varX"]],
                                     modalOKButtonID=ns("okRandomForestVarModal")))
        
    })
    
    observeEvent(input$okRandomForestVarModal, {
        varX <- input$selRandomForestVarModal       


        if(varX[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            randomForestList[["varX"]] <<- setdiff(selVar,varX[2:length(varX)])
        } else {
            randomForestList[["varX"]] <<- varX
        }
        
        output$varX <- renderText({ 
            if (is.null(varX)) {
                "No data selected"
            } else if(varX[1]=="ALL.ALL") {
                paste0(length(randomForestList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse="-"))
            } else {
                paste0(length(randomForestList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse=","))
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
        showModal(ModalRadioButtons(choiceNames=choiceNames, choiceValues=var, ns("okRandomForestTargetVarModal"), "randomForest target Var 선정 대화창",
                                    strExplain="target 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selRandomForestTargetVarModal"),failed = FALSE))

    })
    
    observeEvent(input$okRandomForestTargetVarModal, {
        varY <- input$selRandomForestTargetVarModal       
        output$varY <- renderText({ 
            if (is.null(varY))
                "No data selected"
            else
                paste0("선정된 target 변수 : ",str_c(varY,collapse=","))
        })
        randomForestList[["varY"]] <<- varY
        removeModal()
        # updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$renderRandomForest, {
        # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # targetVar <- c("NRL_FirstTest")
        # maxDepth <- 3;
        selVar <- randomForestList[["varX"]]
        targetVar <- randomForestList[["varY"]]
        # maxDepth <- isolate(input$maxDepth)
        # maxPValue <- isolate(input$maxPValue)

        var <- union(selVar, targetVar)
        df <- curSampleExplore[,c(var)]
        if(!is.numeric(df[,targetVar])) {
            df[,targetVar] <- as.factor(df[,targetVar])
        }
        
        boolValidRow <- !is.na(df[,targetVar])
        
        curSamplerandomForest <<- df[boolValidRow,]
        
        df <- curSamplerandomForest
        

        ###  sticky 가 없어야 classification tree 가 가능함
        for(v in colnames(df)) {
            df[,v] <- unstick(df[,v])
        }
        
        df <- df[complete.cases(df),]
        df_train <- df
        t.formula <- paste0(targetVar, " ~ .")
        

        rf <- randomForest(formula = eval(parse(text = (t.formula))), data = df_train,
                           num.trees = randomForestList[["num.trees"]],importance = T)

        output$varImportance <- renderPrint({
            rf$importance
        })
        
        output$randomForest <- renderPlot({
            varImpPlot(rf)
        })
        

    })
    
}