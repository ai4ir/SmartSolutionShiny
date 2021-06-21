if(!library(party, logical.return=TRUE)) {
    install.packages("party")
    library(party)
}
cTreeList <- list(varX=NA, varY=NA, maxDepth=4, maxPValue=0.1)
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
                   numericInput(ns("maxPValue"),"최대 P값",cTreeList[["maxPValue"]], min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   numericInput(ns("maxDepth"),"maximum Depth",cTreeList[["maxDepth"]], min=1, max=6)
            ),
            column(2,
                   actionButton(ns("renderCTree"),"CTree 재구성")
            ),
            column(2,
                   actionButton(ns("renderReportML_cTree"),"AI VF 도출 보고서")
            )
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
            if(is.null(attr(curSampleExplore[,var[i]],"labelShort") )) {
                choiceNames <- c(choiceNames,paste0("임시 라벨 ", i))
            } else {
                choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
            }
        }

        showModal(ModalCheckboxGroup(title="CTree X Var 선정 대화창", modalCheckboxID=ns("selCTreeVarModal"),
                                     label="CTree X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeList[["varX"]],
                                     modalOKButtonID=ns("okCTreeVarModal")))
        
    })
    
    observeEvent(input$okCTreeVarModal, {
        varX <- input$selCTreeVarModal       

        if(varX[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeList[["varX"]] <<- setdiff(selVar,varX[2:length(varX)])
        } else {
            cTreeList[["varX"]] <<- varX
        }
        
        output$varX <- renderText({ 
            if (is.null(varX)) {
                "No data selected"
            } else if(varX[1]=="ALL.ALL") {
                paste0(length(cTreeList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse="-"))
            } else {
                paste0(length(cTreeList[["varX"]]), "개 X 변수 : ",str_c(varX,collapse=","))
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
    
    observeEvent(input$maxDepth, {
        cTreeList[["maxDepth"]] <<- input$maxDepth
    })
    
    observeEvent(input$maxPValue, {
        cTreeList[["maxPValue"]] <<- input$maxPValue
    })
    
    observeEvent(input$renderCTree, {
        # selVar <- c("C", "Mn", "PL_REFUR_NO")
        # targetVar <- c("NRL_FirstTest")
        # maxDepth <- 3;

        selVar <- cTreeList[["varX"]]
        targetVar <- cTreeList[["varY"]]
        # minCriterion <- 1 - cTreeList[["maxPValue"]]
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
        
        treeControl <- ctree_control(mincriterion=1 - cTreeList[["maxPValue"]], maxdepth=cTreeList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        cTreeResult <- ctree(fmla, df, controls=treeControl)

        output$CTree <- renderPlot({
            plot(cTreeResult)
        })
        

    })
    
    observeEvent(input$renderReportML_cTree, {
        
        withProgress(message="전처리 리포트 작성중", value=0, {
            incProgress(0.2)
            
            pathFileRmdML <<- "Base/menuMachineLearning/Rmd/baseReportMachineLearning.Rmd"

            outputFileName <- "tempo.html"
            selVar <- cTreeList[["varX"]]
            targetVar <- cTreeList[["varY"]]
            params <- list(df=curSampleExplore, selVar=selVar, targetVar=targetVar,
                           cTreeList=cTreeList, rangerList=rangerList )
            outputFileFinalName <- paste0("AI VF 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                          "-",dim(DFSource)[[1]], "-",dim(DFSource)[[2]], ".html")
            options(warn=-1)
            rmarkdown::render(pathFileRmdML, output_file = outputFileName,
                              output_dir = pathHTMLReport,
                              params = params,
                              envir = new.env(parent = globalenv()), encoding="UTF-8")
            file.rename(paste0(pathHTMLReport,"/",outputFileName), paste0(pathHTMLReport,"/",outputFileFinalName))
            incProgress(0.8)
            
        }) #withProgress(message="리포트 작성중", value=0, {
        strAlert <- paste0(pathHTMLReport,"에 ", outputFileFinalName,"이 저장되었습니다.")
        alert(strAlert)
        
    })
    
}