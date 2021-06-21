if(!library(party, logical.return=TRUE)) {
    install.packages("party")
    library(party)
}
if(!library(ranger, logical.return=TRUE)) {
    install.packages("ranger")
    library(ranger)
}
if(!library(randomForest, logical.return=TRUE)) {
    install.packages("randomForest")
    library(randomForest)
}
cTreeTotalList <- list(Group1 = NA, Group2 = NA, Group3 = NA, Group4 = NA, Group5 = NA, Group6 = NA, varY=NA, maxDepth=4, maxPValue=0.1)
cTreeTotalTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(
        inlineCSS("
                   #cTreeTotal-selcTreeGroup1VarModal,  #cTreeTotal-selcTreeGroup2VarModal,
                   #cTreeTotal-selcTreeGroup3VarModal,  #cTreeTotal-selcTreeGroup4VarModal,
                   #cTreeTotal-selcTreeGroup5VarModal,  #cTreeTotal-selcTreeGroup6VarModal,
                   #cTreeTotal-selcTreeTotalTargetVarModal
                   {width: 1200px;}
                   #cTreeTotal-selcTreeGroup1VarModal .checkbox,
                   #cTreeTotal-selcTreeGroup2VarModal .checkbox,
                   #cTreeTotal-selcTreeGroup3VarModal .checkbox,
                   #cTreeTotal-selcTreeGroup4VarModal .checkbox,
                   #cTreeTotal-selcTreeGroup5VarModal .checkbox,
                   #cTreeTotal-selcTreeGroup6VarModal .checkbox,
                   #cTreeTotal-selcTreeTotalTargetVarModal .radio
                   {float: left; margin: 10px;}
                  "),
        fluidRow(
            column(
                2,actionButton(ns("Group1Var"),"Group 1 변수 선정")
            ),
            column(
                2,actionButton(ns("Group2Var"),"Group 2 변수 선정")
            ),
            column(
                2,actionButton(ns("Group3Var"),"Group 3 변수 선정")
            ),
            column(
                2,actionButton(ns("Group4Var"),"Group 4 변수 선정")
            ),
            column(
                2,actionButton(ns("Group5Var"),"Group 5 변수 선정")
            ),
            column(
                2,actionButton(ns("Group6Var"),"Group 6 변수 선정")
            )
        ),
        fluidRow(
            column(2,
                   numericInput(ns("maxPValue"),"최대 P값",cTreeList[["maxPValue"]], min=0.01, max=0.3, step=0.01)
            ),
            column(2,
                   numericInput(ns("maxDepth"),"maximum Depth",cTreeList[["maxDepth"]], min=1, max=6)
            ),
            
        ),
        
        fluidRow(
            column(
                2,actionButton(ns("cTreeGroup1"),"Group1 재구성")
            ),
            column(
                2,actionButton(ns("cTreeGroup2"),"Group2 재구성")
            ),
            column(
                2,actionButton(ns("cTreeGroup3"),"Group3 재구성")
            ),
            column(
                2,actionButton(ns("cTreeGroup4"),"Group4 재구성")
            ),
            column(
                2,actionButton(ns("cTreeGroup5"),"Group5 재구성")
            ),
            column(
                2,actionButton(ns("cTreeGroup6"),"Group6 재구성")
            ),
            
        ),
        fluidRow(
            column(
                2,actionButton(ns("rendercTreeTotal"),"cTreeTotal 재구성")
            ),
            column(
                2,actionButton(ns("targetVar"),"CTree target 변수 선정")
            )
        ),
        fluidRow(
            verbatimTextOutput(ns("Group1"))
        ),
        fluidRow(
            verbatimTextOutput(ns("Group2"))
        ),
        fluidRow(
            verbatimTextOutput(ns("Group3"))
        ),
        fluidRow(
            verbatimTextOutput(ns("Group4"))
        ),
        fluidRow(
            verbatimTextOutput(ns("Group5"))
        ),
        fluidRow(
            verbatimTextOutput(ns("Group6"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varY"))
        ),
        fluidRow(
            verbatimTextOutput(ns("varImpr"))
        ),
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal1")))
        ),
        
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal2")))
        ),
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal3")))
        ),
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal4")))
        ),
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal5")))
        ),
        fluidRow(
            column(6,plotOutput(ns("cTreeTotal6")))
        ),
        fluidRow(
            column(6,plotOutput(ns("ranger")))
        ),
        fluidRow(
            column(6,plotOutput(ns("randomForest")))
        )
    )
}

cTreeTotalTabModule <- function(input, output, session) {
    
    ns <-session$ns
    #------------------------------------------------------------------Group1--------------------
    observeEvent(input$Group1Var, {
        
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

        showModal(ModalCheckboxGroup(title="cTreeGroup1 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup1VarModal"),
                                     label="cTreeGroup1 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group1"]],
                                     modalOKButtonID=ns("okcTreeGroup1VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup1VarModal, {
        Group1 <- input$selcTreeGroup1VarModal       

        if(Group1[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group1"]] <<- setdiff(selVar,Group1[2:length(Group1)])
        } else {
            cTreeTotalList[["Group1"]] <<- Group1
        }
        
        output$Group1 <- renderText({ 
            if (is.null(Group1)) {
                "No data selected"
            } else if(Group1[1]=="ALL.ALL") {
                paste0("Group1 , ",length(cTreeTotalList[["Group1"]]), "개 X 변수 : ",str_c(Group1,collapse="-"))
            } else {
                paste0("Group1 , ",length(cTreeTotalList[["Group1"]]), "개 X 변수 : ",str_c(Group1,collapse=","))
            }
        })
        

        removeModal()

    })
    
    
    #------------------------------------------------------------------Group2--------------------
    observeEvent(input$Group2Var, {
        #alert("준비중입니다.")
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
        
        showModal(ModalCheckboxGroup(title="cTreeGroup2 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup2VarModal"),
                                     label="cTreeGroup2 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group2"]],
                                     modalOKButtonID=ns("okcTreeGroup2VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup2VarModal, {
        Group2 <- input$selcTreeGroup2VarModal       
        
        if(Group2[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group2"]] <<- setdiff(selVar,Group2[2:length(Group2)])
        } else {
            cTreeTotalList[["Group2"]] <<- Group2
        }
        
        output$Group2 <- renderText({ 
            if (is.null(Group2)) {
                "No data selected"
            } else if(Group2[1]=="ALL.ALL") {
                paste0("Group2 , ",length(cTreeTotalList[["Group2"]]), "개 X 변수 : ",str_c(Group2,collapse="-"))
            } else {
                paste0("Group2 , ",length(cTreeTotalList[["Group2"]]), "개 X 변수 : ",str_c(Group2,collapse=","))
            }
        })
        
        
        removeModal()
        
    })
    
    
    #------------------------------------------------------------------Group3--------------------
    observeEvent(input$Group3Var, {
        #alert("준비중입니다.")
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
        
        showModal(ModalCheckboxGroup(title="cTreeGroup3 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup3VarModal"),
                                     label="cTreeGroup3 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group3"]],
                                     modalOKButtonID=ns("okcTreeGroup3VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup3VarModal, {
        Group3 <- input$selcTreeGroup3VarModal       
        
        if(Group3[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group3"]] <<- setdiff(selVar,Group3[2:length(Group3)])
        } else {
            cTreeTotalList[["Group3"]] <<- Group3
        }
        
        output$Group3 <- renderText({ 
            if (is.null(Group3)) {
                "No data selected"
            } else if(Group3[1]=="ALL.ALL") {
                paste0("Group3 , ",length(cTreeTotalList[["Group3"]]), "개 X 변수 : ",str_c(Group3,collapse="-"))
            } else {
                paste0("Group3 , ",length(cTreeTotalList[["Group3"]]), "개 X 변수 : ",str_c(Group3,collapse=","))
            }
        })
        
        
        removeModal()
        
    })
    
    
    #------------------------------------------------------------------Group4--------------------
    observeEvent(input$Group4Var, {
        #alert("준비중입니다.")
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
        
        showModal(ModalCheckboxGroup(title="cTreeGroup4 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup4VarModal"),
                                     label="cTreeGroup4 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group4"]],
                                     modalOKButtonID=ns("okcTreeGroup4VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup4VarModal, {
        Group4 <- input$selcTreeGroup4VarModal       
        
        if(Group4[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group4"]] <<- setdiff(selVar,Group4[2:length(Group4)])
        } else {
            cTreeTotalList[["Group4"]] <<- Group4
        }
        
        output$Group4 <- renderText({ 
            if (is.null(Group4)) {
                "No data selected"
            } else if(Group4[1]=="ALL.ALL") {
                paste0("Group4 , ",length(cTreeTotalList[["Group4"]]), "개 X 변수 : ",str_c(Group4,collapse="-"))
            } else {
                paste0("Group4 , ",length(cTreeTotalList[["Group4"]]), "개 X 변수 : ",str_c(Group4,collapse=","))
            }
        })
        
        
        removeModal()
        
    })
    
    
    #------------------------------------------------------------------Group5--------------------
    observeEvent(input$Group5Var, {
        #alert("준비중입니다.")
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
        
        showModal(ModalCheckboxGroup(title="cTreeGroup5 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup5VarModal"),
                                     label="cTreeGroup5 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group5"]],
                                     modalOKButtonID=ns("okcTreeGroup5VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup5VarModal, {
        Group5 <- input$selcTreeGroup5VarModal       
        
        if(Group5[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group5"]] <<- setdiff(selVar,Group5[2:length(Group5)])
        } else {
            cTreeTotalList[["Group5"]] <<- Group5
        }
        
        output$Group5 <- renderText({ 
            if (is.null(Group5)) {
                "No data selected"
            } else if(Group5[1]=="ALL.ALL") {
                paste0("Group5 , ",length(cTreeTotalList[["Group5"]]), "개 X 변수 : ",str_c(Group5,collapse="-"))
            } else {
                paste0("Group5 , ",length(cTreeTotalList[["Group5"]]), "개 X 변수 : ",str_c(Group5,collapse=","))
            }
        })
        
        
        removeModal()
        
    })
    
    
    #------------------------------------------------------------------Group6--------------------
    observeEvent(input$Group6Var, {
        #alert("준비중입니다.")
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
        
        showModal(ModalCheckboxGroup(title="cTreeGroup6 X Var 선정 대화창", modalCheckboxID=ns("selcTreeGroup6VarModal"),
                                     label="cTreeGroup6 X Var 선정",
                                     choiceNames=choiceNames, choiceValues=var,
                                     selected=cTreeTotalList[["Group6"]],
                                     modalOKButtonID=ns("okcTreeGroup6VarModal")))
        
    })
    
    observeEvent(input$okcTreeGroup6VarModal, {
        Group6 <- input$selcTreeGroup6VarModal       
        
        if(Group6[1]=="ALL.ALL") {
            removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore)) %>%
                union(extractDateVarName(curSampleExplore))
            selVar <- setdiff(colnames(curSampleExplore),removeVar) 
            cTreeTotalList[["Group6"]] <<- setdiff(selVar,Group6[2:length(Group6)])
        } else {
            cTreeTotalList[["Group6"]] <<- Group6
        }
        
        output$Group6 <- renderText({ 
            if (is.null(Group6)) {
                "No data selected"
            } else if(Group6[1]=="ALL.ALL") {
                paste0("Group6 , ",length(cTreeTotalList[["Group6"]]), "개 X 변수 : ",str_c(Group6,collapse="-"))
            } else {
                paste0("Group6 , ",length(cTreeTotalList[["Group6"]]), "개 X 변수 : ",str_c(Group6,collapse=","))
            }
        })
        
        
        removeModal()
        
    })
    
    
    #------------------------------------------------------------------target X --------------------
    observeEvent(input$targetVar, {
        
        removeVar <- union(extractCharVarName(curSampleExplore), extractConstName(curSampleExplore))
        var <- setdiff(colnames(curSampleExplore),removeVar) 
        choiceNames <- attr(curSampleExplore[,var[1]],"labelShort")
        for(i in 2:length(var)) {
            choiceNames <- c(choiceNames, attr(curSampleExplore[,var[i]],"labelShort") )
        }
        showModal(ModalRadioButtons(choiceNames=choiceNames, choiceValues=var, ns("okcTreeTotalTargetVarModal"), "cTreeTotal target Var 선정 대화창",
                                    strExplain="target 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selcTreeTotalTargetVarModal"),failed = FALSE))

    })
    
    observeEvent(input$okcTreeTotalTargetVarModal, {
        varY <- input$selcTreeTotalTargetVarModal       
        output$varY <- renderText({ 
            if (is.null(varY))
                "No data selected"
            else
                paste0("선정된 target 변수 : ",str_c(varY,collapse=","))
        })
        cTreeTotalList[["varY"]] <<- varY
        removeModal()
        # updateSelectInput(session,"targetVar", choices=setdiff(rownames(orderVFcluster(curSampleExplore)),selVar))
    })
    
    observeEvent(input$maxDepth, {
        cTreeTotalList[["maxDepth"]] <<- input$maxDepth
    })
    
    observeEvent(input$maxPValue, {
        cTreeTotalList[["maxPValue"]] <<- input$maxPValue
    })
    
    #------------------------------------Group1 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup1,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group1 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group1"]]))==0){
            #alert("준비중입니다.")
            selVar1 <- cTreeTotalList[["Group1"]]
            var1 <- union(selVar1, targetVar)
            df1 <- curSampleExplore[,c(var1)]
            if(!is.numeric(df1[,targetVar])) {
                df1[,targetVar] <- as.factor(df1[,targetVar])
            }
            boolValidRow1 <- !is.na(df1[,targetVar])
            curSampleCTree1 <<- df1[boolValidRow1,]
            df1 <- curSampleCTree1
            for(v in colnames(df1)) {
                df1[,v] <- unstick(df1[,v])
            }
            #-------------------rf---
            df1_rf <- df1[complete.cases(df1),]
            df1_train <- df1_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf1 <- randomForest(formula = eval(parse(text = (t.formula))), data = df1_train, num.trees = 100, importance = T)
            
            #------rg
            rg1 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df1_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl1 <- predict(rg1, data=df1_rf)
            result_rg1 <- pred.nrl1$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult1 <- ctree(fmla, df1, controls=treeControl)
            
            # output$cTreeTotal1 <- renderPlot({
            #     plot(df1_rf[,targetVar], result_rg1)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg1$variable.importance
                #rf1
                #testtt<- importance(rf1)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult1)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf1)
                
            })
        }
    })
    
    #------------------------------------Group2 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup2,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group2 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group2"]]))==0){
            #alert("준비중입니다.")
            selVar2 <- cTreeTotalList[["Group2"]]
            var2 <- union(selVar2, targetVar)
            df2 <- curSampleExplore[,c(var2)]
            if(!is.numeric(df2[,targetVar])) {
                df2[,targetVar] <- as.factor(df2[,targetVar])
            }
            boolValidRow2 <- !is.na(df2[,targetVar])
            curSampleCTree2 <<- df2[boolValidRow2,]
            df2 <- curSampleCTree2
            for(v in colnames(df2)) {
                df2[,v] <- unstick(df2[,v])
            }
            #-------------------rf---
            df2_rf <- df2[complete.cases(df2),]
            df2_train <- df2_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf2 <- randomForest(formula = eval(parse(text = (t.formula))), data = df2_train, num.trees = 100, importance = T)
            
            #------rg
            rg2 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df2_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl2 <- predict(rg2, data=df2_rf)
            result_rg2 <- pred.nrl2$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult2 <- ctree(fmla, df2, controls=treeControl)
            
            # output$cTreeTotal2 <- renderPlot({
            #     plot(df2_rf[,targetVar], result_rg2)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg2$variable.importance
                #rf2
                #testtt<- importance(rf2)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult2)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf2)
                
            })
        }
    })
    
    #------------------------------------Group3 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup3,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group3 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group3"]]))==0){
            #alert("준비중입니다.")
            selVar3 <- cTreeTotalList[["Group3"]]
            var3 <- union(selVar3, targetVar)
            df3 <- curSampleExplore[,c(var3)]
            if(!is.numeric(df3[,targetVar])) {
                df3[,targetVar] <- as.factor(df3[,targetVar])
            }
            boolValidRow3 <- !is.na(df3[,targetVar])
            curSampleCTree3 <<- df3[boolValidRow3,]
            df3 <- curSampleCTree3
            for(v in colnames(df3)) {
                df3[,v] <- unstick(df3[,v])
            }
            #-------------------rf---
            df3_rf <- df3[complete.cases(df3),]
            df3_train <- df3_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf3 <- randomForest(formula = eval(parse(text = (t.formula))), data = df3_train, num.trees = 100, importance = T)
            
            #------rg
            rg3 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df3_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl3 <- predict(rg3, data=df3_rf)
            result_rg3 <- pred.nrl3$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult3 <- ctree(fmla, df3, controls=treeControl)
            
            # output$cTreeTotal3 <- renderPlot({
            #     plot(df3_rf[,targetVar], result_rg3)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg3$variable.importance
                #rf3
                #testtt<- importance(rf3)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult3)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf3)
                
            })
        }
    })
    
    #------------------------------------Group4 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup4,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group4 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group4"]]))==0){
            #alert("준비중입니다.")
            selVar4 <- cTreeTotalList[["Group4"]]
            var4 <- union(selVar4, targetVar)
            df4 <- curSampleExplore[,c(var4)]
            if(!is.numeric(df4[,targetVar])) {
                df4[,targetVar] <- as.factor(df4[,targetVar])
            }
            boolValidRow4 <- !is.na(df4[,targetVar])
            curSampleCTree4 <<- df4[boolValidRow4,]
            df4 <- curSampleCTree4
            for(v in colnames(df4)) {
                df4[,v] <- unstick(df4[,v])
            }
            #-------------------rf---
            df4_rf <- df4[complete.cases(df4),]
            df4_train <- df4_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf4 <- randomForest(formula = eval(parse(text = (t.formula))), data = df4_train, num.trees = 100, importance = T)
            
            #------rg
            rg4 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df4_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl4 <- predict(rg4, data=df4_rf)
            result_rg4 <- pred.nrl4$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult4 <- ctree(fmla, df4, controls=treeControl)
            
            # output$cTreeTotal4 <- renderPlot({
            #     plot(df4_rf[,targetVar], result_rg4)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg4$variable.importance
                #rf4
                #testtt<- importance(rf4)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult4)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf4)
                
            })
        }
    })
    
    #------------------------------------Group5 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup5,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group5 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group5"]]))==0){
            #alert("준비중입니다.")
            selVar5 <- cTreeTotalList[["Group5"]]
            var5 <- union(selVar5, targetVar)
            df5 <- curSampleExplore[,c(var5)]
            if(!is.numeric(df5[,targetVar])) {
                df5[,targetVar] <- as.factor(df5[,targetVar])
            }
            boolValidRow5 <- !is.na(df5[,targetVar])
            curSampleCTree5 <<- df5[boolValidRow5,]
            df5 <- curSampleCTree5
            for(v in colnames(df5)) {
                df5[,v] <- unstick(df5[,v])
            }
            #-------------------rf---
            df5_rf <- df5[complete.cases(df5),]
            df5_train <- df5_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf5 <- randomForest(formula = eval(parse(text = (t.formula))), data = df5_train, num.trees = 100, importance = T)
            
            #------rg
            rg5 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df5_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl5 <- predict(rg5, data=df5_rf)
            result_rg5 <- pred.nrl5$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult5 <- ctree(fmla, df5, controls=treeControl)
            
            # output$cTreeTotal5 <- renderPlot({
            #     plot(df5_rf[,targetVar], result_rg5)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg5$variable.importance
                #rf5
                #testtt<- importance(rf5)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult5)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf5)
                
            })
        }
    })
    
    #------------------------------------Group6 개별 Ctree--------------------------------------------------------------
    observeEvent(input$cTreeGroup6,{
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group6 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group6"]]))==0){
            #alert("준비중입니다.")
            selVar6 <- cTreeTotalList[["Group6"]]
            var6 <- union(selVar6, targetVar)
            df6 <- curSampleExplore[,c(var6)]
            if(!is.numeric(df6[,targetVar])) {
                df6[,targetVar] <- as.factor(df6[,targetVar])
            }
            boolValidRow6 <- !is.na(df6[,targetVar])
            curSampleCTree6 <<- df6[boolValidRow6,]
            df6 <- curSampleCTree6
            for(v in colnames(df6)) {
                df6[,v] <- unstick(df6[,v])
            }
            #-------------------rf---
            df6_rf <- df6[complete.cases(df6),]
            df6_train <- df6_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf6 <- randomForest(formula = eval(parse(text = (t.formula))), data = df6_train, num.trees = 100, importance = T)
            
            #------rg
            rg6 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df6_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl6 <- predict(rg6, data=df6_rf)
            result_rg6 <- pred.nrl6$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult6 <- ctree(fmla, df6, controls=treeControl)
            
            # output$cTreeTotal6 <- renderPlot({
            #     plot(df6_rf[,targetVar], result_rg6)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                rg6$variable.importance
                #rf6
                #testtt<- importance(rf6)
                #testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult6)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf6)
                
            })
        }
    })
    
    #-----------------------------------------------------------------------ctree 발생------------------------------
    
    observeEvent(input$rendercTreeTotal, {
        alert("준비중입니다.")
        targetVar <- cTreeTotalList[["varY"]]
        treeControl <- ctree_control(mincriterion=1 - cTreeTotalList[["maxPValue"]], maxdepth=cTreeTotalList[["maxDepth"]], stump=FALSE)
        fmla <- as.formula(paste0(targetVar, " ~ ."))
        
        #-----------------------------------------------------------------Group1 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group1"]]))==0){
            #alert("준비중입니다.")
            selVar1 <- cTreeTotalList[["Group1"]]
            var1 <- union(selVar1, targetVar)
            df1 <- curSampleExplore[,c(var1)]
            if(!is.numeric(df1[,targetVar])) {
                df1[,targetVar] <- as.factor(df1[,targetVar])
            }
            boolValidRow1 <- !is.na(df1[,targetVar])
            curSampleCTree1 <<- df1[boolValidRow1,]
            df1 <- curSampleCTree1
            for(v in colnames(df1)) {
                df1[,v] <- unstick(df1[,v])
            }
            #-------------------rf---
            df1_rf <- df1[complete.cases(df1),]
            df1_train <- df1_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf1 <- randomForest(formula = eval(parse(text = (t.formula))), data = df1_train, num.trees = 100, importance = T)
            
            #------rg
            rg1 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df1_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl1 <- predict(rg1, data=df1_rf)
            result_rg1 <- pred.nrl1$predictions
            #------------------------------------------------------------------------------------------
            
            cTreeResult1 <- ctree(fmla, df1, controls=treeControl)
            
            # output$cTreeTotal1 <- renderPlot({
            #     plot(df1_rf[,targetVar], result_rg1)
            #     
            # 
            # })
            output$varImpr <- renderPrint({
                #rg1$variable.importance
                #rf1
                testtt<- importance(rf1)
                testtt
            })
            output$cTreeTotal1 <- renderPlot({
                
                plot(cTreeResult1)
                
                
            })
            output$cTreeTotal2 <- renderPlot({
                
                varImpPlot(rf1)
                
            })
        }
        
        
        #-----------------------------------------------------------------Group2 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group2"]]))==0){
            #alert("준비중입니다.")
            selVar2 <- cTreeTotalList[["Group2"]]
            var2 <- union(selVar2, targetVar)
            df2 <- curSampleExplore[,c(var2)]
            if(!is.numeric(df2[,targetVar])) {
                df2[,targetVar] <- as.factor(df2[,targetVar])
            }
            boolValidRow2 <- !is.na(df2[,targetVar])
            curSampleCTree2 <<- df2[boolValidRow2,]
            df2 <- curSampleCTree2
            for(v in colnames(df2)) {
                df2[,v] <- unstick(df2[,v])
            }
            #-------------------rf---
            df2_rf <- df2[complete.cases(df2),]
            df2_train <- df2_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf2 <- randomForest(formula = eval(parse(text = (t.formula))), data = df2_train, num.trees = 100, importance = T)
            #------rg
            rg2 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df2_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl2 <- predict(rg2, data=df2_rf)
            result_rg2 <- pred.nrl2$predictions
            #------------------------------------------------------------------------------------------
            cTreeResult2 <- ctree(fmla, df2, controls=treeControl)
            
            # output$cTreeTotal2 <- renderPlot({
            #     plot(df2_rf[,targetVar], result_rg2)
            # 
            # })
            
        }
        
        #-----------------------------------------------------------------Group3 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group3"]]))==0){
            #alert("준비중입니다.")
            selVar3 <- cTreeTotalList[["Group3"]]
            var3 <- union(selVar3, targetVar)
            df3 <- curSampleExplore[,c(var3)]
            if(!is.numeric(df3[,targetVar])) {
                df3[,targetVar] <- as.factor(df3[,targetVar])
            }
            boolValidRow3 <- !is.na(df3[,targetVar])
            curSampleCTree3 <<- df3[boolValidRow3,]
            df3 <- curSampleCTree3
            for(v in colnames(df3)) {
                df3[,v] <- unstick(df3[,v])
            }
            #-------------------rf---
            df3_rf <- df3[complete.cases(df3),]
            df3_train <- df3_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf3 <- randomForest(formula = eval(parse(text = (t.formula))), data = df3_train, num.trees = 100, importance = T)
            #------rg
            rg3 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df3_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl3 <- predict(rg3, data=df3_rf)
            result_rg3 <- pred.nrl3$predictions
            #------------------------------------------------------------------------------------------
            cTreeResult3 <- ctree(fmla, df3, controls=treeControl)
            
            # output$cTreeTotal3 <- renderPlot({
            #     plot(df3_rf[,targetVar], result_rg3)
            # 
            # })
            
        }
        
        #-----------------------------------------------------------------Group4 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group4"]]))==0){
            #alert("준비중입니다.")
            selVar4 <- cTreeTotalList[["Group4"]]
            var4 <- union(selVar4, targetVar)
            df4 <- curSampleExplore[,c(var4)]
            if(!is.numeric(df4[,targetVar])) {
                df4[,targetVar] <- as.factor(df4[,targetVar])
            }
            boolValidRow4 <- !is.na(df4[,targetVar])
            curSampleCTree4 <<- df4[boolValidRow4,]
            df4 <- curSampleCTree4
            for(v in colnames(df4)) {
                df4[,v] <- unstick(df4[,v])
            }
            #-------------------rf---
            df4_rf <- df4[complete.cases(df4),]
            df4_train <- df4_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf4 <- randomForest(formula = eval(parse(text = (t.formula))), data = df4_train, num.trees = 100, importance = T)
            #------rg
            rg4 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df4_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl4 <- predict(rg4, data=df4_rf)
            result_rg4 <- pred.nrl4$predictions
            #------------------------------------------------------------------------------------------
            cTreeResult4 <- ctree(fmla, df4, controls=treeControl)
            
            # output$cTreeTotal4 <- renderPlot({
            #     plot(df4_rf[,targetVar], result_rg4)
            # 
            # })
            
        }
        
        #-----------------------------------------------------------------Group5 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group5"]]))==0){
            #alert("준비중입니다.")
            selVar5 <- cTreeTotalList[["Group5"]]
            var5 <- union(selVar5, targetVar)
            df5 <- curSampleExplore[,c(var5)]
            if(!is.numeric(df5[,targetVar])) {
                df5[,targetVar] <- as.factor(df5[,targetVar])
            }
            boolValidRow5 <- !is.na(df5[,targetVar])
            curSampleCTree5 <<- df5[boolValidRow5,]
            df5 <- curSampleCTree5
            for(v in colnames(df5)) {
                df5[,v] <- unstick(df5[,v])
            }
            #-------------------rf---
            df5_rf <- df5[complete.cases(df5),]
            df5_train <- df5_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf5 <- randomForest(formula = eval(parse(text = (t.formula))), data = df5_train, num.trees = 100, importance = T)
            #------rg
            rg5 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df5_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl5 <- predict(rg5, data=df5_rf)
            result_rg5 <- pred.nrl5$predictions
            #------------------------------------------------------------------------------------------
            cTreeResult5 <- ctree(fmla, df5, controls=treeControl)
            
            # output$cTreeTotal5 <- renderPlot({
            #     plot(cTreeResult5)
            # 
            # })
            
        }
        
        #-----------------------------------------------------------------Group6 Ctree----------------------------
        if(sum(is.na(cTreeTotalList[["Group6"]]))==0){
            #alert("준비중입니다.")
            selVar6 <- cTreeTotalList[["Group6"]]
            var6 <- union(selVar6, targetVar)
            df6 <- curSampleExplore[,c(var6)]
            if(!is.numeric(df6[,targetVar])) {
                df6[,targetVar] <- as.factor(df6[,targetVar])
            }
            boolValidRow6 <- !is.na(df6[,targetVar])
            curSampleCTree6 <<- df6[boolValidRow6,]
            df6 <- curSampleCTree6
            for(v in colnames(df6)) {
                df6[,v] <- unstick(df6[,v])
            }
            #-------------------rf---
            df6_rf <- df6[complete.cases(df6),]
            df6_train <- df6_rf
            t.formula <- paste0(targetVar, " ~ .")
            rf6 <- randomForest(formula = eval(parse(text = (t.formula))), data = df6_train, num.trees = 100, importance = T)
            #------rg
            rg6 <- ranger(formula = eval(parse(text = (t.formula))),importance = "permutation",data = df6_train, num.threads = 20, seed = 1000, num.trees = 100, respect.unordered.factors = TRUE)
            pred.nrl6 <- predict(rg6, data=df6_rf)
            result_rg6 <- pred.nrl6$predictions
            #------------------------------------------------------------------------------------------
            cTreeResult6 <- ctree(fmla, df6, controls=treeControl)
            
            # output$cTreeTotal6 <- renderPlot({
            #     plot(cTreeResult6)
            # 
            # })
            
        }
        
        
        

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
