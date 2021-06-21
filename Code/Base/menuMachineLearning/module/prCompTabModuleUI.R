# if(!library(randomForest, logical.return=TRUE)) {
#     install.packages("randomForest")
#     library(randomForest)
# }
prCompList <- list(varX=NA)
prCompTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(2,
                   actionButton(ns("prCompVar"),"prComp X 변수 선정")
            ),
            column(2,
                   # actionButton(ns("targetVar"),"randomForest target 변수 선정")
            ),
            column(2,
                   # numericInput(ns("maxPValue"),"최대 P값",0.05, min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   # numericInput(ns("maxDepth"),"maximum Depth",2, min=1, max=6)
            ),
            column(1,
                   actionButton(ns("renderPrComp"),"PrComp 재구성")
            ),
        ),
        fluidRow(
            verbatimTextOutput(ns("varX"))
        ),
        fluidRow(
            column(6,
                   verbatimTextOutput(ns("model"))
                   
            ),
            column(6,
                   verbatimTextOutput(ns("summaryModel"))
            )
        ),

        fluidRow( 
            column(6,
                   plotOutput(ns("screePlot"))
                   
            ),
            column(6,
                   plotOutput(ns("biPlot"))
            )
            
        )
        

    )
}

prCompTabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$prCompVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
            union(extractDateVarName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        var <- c("ALL.ALL",var)
        choiceNames <- "All.ALL"
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }

        showModal(ModalCheckboxGroup(title="prComp X Var 선정 대화창", modalCheckboxID=ns("selPrCompVarModal"),
                                     label="prComp X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=prCompList[["varX"]],
                                     modalOKButtonID=ns("okPrCompVarModal")))
        
    })
    
    observeEvent(input$okPrCompVarModal, {
        varX <- input$selPrCompVarModal       


        if(varX[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            prCompList[["varX"]] <<- setdiff(selVar,varX[2:length(varX)])
        } else {
            prCompList[["varX"]] <<- varX
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
    

    observeEvent(input$renderPrComp, {
        # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # targetVar <- c("NRL_FirstTest")
        # maxDepth <- 3;
        selVar <- prCompList[["varX"]]

        var <- selVar
        df <- curSampleExplore[,c(var)]

        curSamplePrComp <<- df
        
        df <- curSamplePrComp
        

        ###  sticky 가 없어야 classification tree 가 가능함
        for(v in colnames(df)) {
            df[,v] <- unstick(df[,v])
        }
        
        df <- df[complete.cases(df),]
        df_train <- df
        
        modelPrComp <- prcomp(df_train, scale = TRUE)
        
        output$model <- renderPrint({
            modelPrComp
        })

        output$summaryModel <- renderPrint({
            summary(modelPrComp)
        })
        
        output$screePlot <- renderPlot({
            screeplot(modelPrComp, main = "", col = "green", type = "lines", pch = 1, 
                      npcs = length(modelPrComp$sdev))
        })
        
        output$biPlot <- renderPlot({
            biplot(modelPrComp)
        })
        
        curSampleExplore <<- curSampleExplore %>% mutate(prComp1=modelPrComp$x[,1]) %>%
            attachAttrOneVar(var="prComp1", label="prComp1", labelShort="prComp1")

    })
    
}