exploreTable2TabModuleList <- list(tableCol1=character(0), tableCol2=character(0),
                                   tableCol3=character(0), tableContVar=character(0))

exploreTable2TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1, actionButton(ns("updateTable"), label="테이블 갱신") 
            ),
            column(1, actionButton(ns("renderSampleData"), label="남기기") 
            ),
            column(10, verbatimTextOutput(ns("test999"))
            )

        ),
        
        DT::dataTableOutput(ns("table"))
        
    )
}

exploreTable2TabModule <- function(input, output, session) {
    ns <-session$ns
    
    observe({
        treatModalTable(input, output, session)
    })
    
    observeEvent(input$renderSampleData, {
        s <-  input$table_rows_selected
        if(length(s)==0) {
            alert("선택된 행이 없습니다.\n 1개 이상의 행을 선택해야 합니다.")
            return()
        }
        colName <- exploreTable2TabModuleList[["tableCol1"]]
        boolVec <- curSampleExploreTable[,colName]==summaryTableCSV[s[1],colName]
        if(length(exploreTable2TabModuleList[["tableCol2"]])==1) {
            colName <- exploreTable2TabModuleList[["tableCol2"]]
            boolVec2 <- curSampleExploreTable[,colName]==summaryTableCSV[s[1],colName]
            boolVec <- boolVec & boolVec2
        }
        if(length(exploreTable2TabModuleList[["tableCol3"]])==1) {
            colName <- exploreTable2TabModuleList[["tableCol3"]]
            boolVec2 <- curSampleExploreTable[,colName]==summaryTableCSV[s[1],colName]
            boolVec <- boolVec & boolVec2
        }
            
        curSampleExplore <<- curSampleExploreTable[boolVec,]

        
        if(length(s)==1) {
            noData <- NROW(curSampleExplore)
            string <- paste("renderSampledata : ", noData,"개")
            alert(string)
            return()
        }
        
        for(i in 2:length(s)) {
            colName <- exploreTable2TabModuleList[["tableCol1"]]
            boolVec <- curSampleExploreTable[,colName]==summaryTableCSV[s[i],colName]
            if(length(exploreTable2TabModuleList[["tableCol2"]])==1) {
                colName <- exploreTable2TabModuleList[["tableCol2"]]
                boolVec2 <- curSampleExploreTable[,colName]==summaryTableCSV[s[i],colName]
                boolVec <- boolVec & boolVec2
            }
            if(length(exploreTable2TabModuleList[["tableCol3"]])==1) {
                colName <- exploreTable2TabModuleList[["tableCol3"]]
                boolVec2 <- curSampleExploreTable[,colName]==summaryTableCSV[s[i],colName]
                boolVec <- boolVec & boolVec2
            }
            
            curSampleExplore <<- rbind(curSampleExplore, curSampleExploreTable[boolVec,])
        }
        
        noData <- NROW(curSampleExplore)
        string <- paste("renderSampledata : ", noData,"개")
        alert(string)
        
    })
    
    # print the selected indices
    output$test999 = renderPrint({
        s <-  input$table_rows_selected
        if (length(s)) {
            cat('선택된 행 : ')
            cat(s, sep = ', ')
        }
    })
    
}



ModalTable <- function(okButtonName, labelStr="테이블 옵션 선정",
                       strExplain="테이블 옵션을 선정하세요",
                       failed = FALSE, session) {
    labelStr <- "테이블 구성 옵션"
    # browser()
    df <- as.data.frame(curSampleExplore)
    ns <-session$ns

    modalDialog(
        title=labelStr,
        actionButton(ns("ModalTable_catVar1"), label="1열 그룹 변수 선정"),
        actionButton(ns("ModalTable_catVar2"), label="2열 그룹 변수 선정"),
        actionButton(ns("ModalTable_catVar3"), label="3열 그룹 변수 선정"),
        actionButton(ns("ModalTable_contVar"), label="연속형 변수 선정"),
        tags$p(""),
        tags$hr(),
        # tags$p(paste0(" Y : ", str_c(aesList[["y"]], collapse=", "))),
        # tags$p(paste0(" X : ", str_c(aesList[["x"]], collapse=", "))),
        tags$p(paste0(" 1열 그룹 변수 : ", exploreTable2TabModuleList[["tableCol1"]])),
        tags$p(paste0(" 2열 그룹 변수 : ", exploreTable2TabModuleList[["tableCol2"]])),
        tags$p(paste0(" 3열 그룹 변수 : ", exploreTable2TabModuleList[["tableCol3"]])),
        tags$p(paste0(" 연속형 변수  : ", 
                      str_c(exploreTable2TabModuleList[["tableContVar"]], collapse=","))),

        
        if (failed)
            div(tags$b("Invalid data", style = "color: red;")),
        
        
        footer = tagList(
            modalButton("Cancel"),
            actionButton(okButtonName, "OK")
        ),
        size="l"
    )
}

treatModalTable <- function(input, output, session) {
    ns <-session$ns

    observeEvent(input$updateTable, {
        catVar <- extractCatVarName(curSampleExplore)
        showModal(ModalTable(okButtonName=ns("okModalTable"), labelStr="테이블 옵션 선정",
                             strExplain="테이블 옵션을 선정하세요",
                             failed = FALSE, session))
    })
    
    observeEvent(input$ModalTable_catVar1  , {
        var <- c(catVarWithoutModal, catVarWithModal, DomainTable1Names, DomainTable2Names, DomainTable3Names)
        showModal(ModalRadioButtons(choiceNames=var, choiceValues=var,
                                    okButtonName=ns("okModalTable_catVar1"), 
                                    labelStr="1열 그룹 변수선정", 
                                    strExplain="1열 그룹 변수를선정하세요",
                                    modalRadioButtonsID=ns("selModalTable_catVar1"),failed = FALSE)
        )
    })
    
    observeEvent(input$okModalTable_catVar1, {
        exploreTable2TabModuleList[["tableCol1"]] <<- input$selModalTable_catVar1
        removeModal()
        showModal(ModalTable(okButtonName=ns("okModalTable"), labelStr="테이블 옵션 선정",
                             strExplain="테이블 옵션을 선정하세요",
                             failed = FALSE, session))
    })
    
    observeEvent(input$ModalTable_catVar2  , {
        var <- c(catVarWithoutModal, catVarWithModal, DomainTable1Names, DomainTable2Names, DomainTable3Names)
        var <- setdiff(var,exploreTable2TabModuleList[["tableCol1"]] )
        showModal(ModalRadioButtons(choiceNames=var, choiceValues=var,
                                    okButtonName=ns("okModalTable_catVar2"), 
                                    labelStr="2열 그룹 변수선정", 
                                    strExplain="2열 그룹 변수를선정하세요",
                                    modalRadioButtonsID=ns("selModalTable_catVar2"),failed = FALSE)
        )
    })
    
    observeEvent(input$okModalTable_catVar2, {
        exploreTable2TabModuleList[["tableCol2"]] <<- input$selModalTable_catVar2
        removeModal()
        showModal(ModalTable(okButtonName=ns("okModalTable"), labelStr="테이블 옵션 선정",
                             strExplain="테이블 옵션을 선정하세요",
                             failed = FALSE, session))
    })
    
    observeEvent(input$ModalTable_catVar3  , {
        var <- c(catVarWithoutModal, catVarWithModal, DomainTable1Names, DomainTable2Names, DomainTable3Names)
        var <- setdiff(var,
                          c(exploreTable2TabModuleList[["tableCol1"]],
                            exploreTable2TabModuleList[["tableCol2"]]))
        if(length(var)==0) {
            alert("그룹 변수가 2개 이하입니다.")
            return()
        }
        
        showModal(ModalRadioButtons(choiceNames=var, choiceValues=var,
                                    okButtonName=ns("okModalTable_catVar3"), 
                                    labelStr="3열 그룹 변수선정", 
                                    strExplain="3열 그룹 변수를선정하세요",
                                    modalRadioButtonsID=ns("selModalTable_catVar3"),failed = FALSE)
        )
    })
    
    observeEvent(input$okModalTable_catVar3, {
        exploreTable2TabModuleList[["tableCol3"]] <<- input$selModalTable_catVar3
        removeModal()
        showModal(ModalTable(okButtonName=ns("okModalTable"), labelStr="테이블 옵션 선정",
                             strExplain="테이블 옵션을 선정하세요",
                             failed = FALSE, session))
    })
    
    observeEvent(input$ModalTable_contVar  , {
        contVar <- c(DomainTable1Names, DomainTable2Names, DomainTable3Names)
        # showModal(ModalRadioButtons(choiceNames=contVar, choiceValues=contVar,
        #                             okButtonName=ns("okModalTable_contVar"), 
        #                             labelStr="연속형 변수선정", 
        #                             strExplain="연속형 변수를선정하세요",
        #                             modalRadioButtonsID=ns("selModalTable_contVar"),failed = FALSE)
        # )
                  
        showModal(ModalCheckboxGroup(title="연속형 변수선정", 
                                     modalCheckboxID=ns("selModalTable_contVar"),
                                     label="연속형 변수선정",
                                     choiceNames=contVar, choiceValues=contVar, 
                                     selected=DomainTable3Names,
                                    modalOKButtonID=ns("okModalTable_contVar"), failed = FALSE) 
        )
    })
    
    observeEvent(input$okModalTable_contVar, {
        exploreTable2TabModuleList[["tableContVar"]] <<- input$selModalTable_contVar
        removeModal()
        showModal(ModalTable(okButtonName=ns("okModalTable"), labelStr="테이블 옵션 선정",
                             strExplain="테이블 옵션을 선정하세요",
                             failed = FALSE, session))
    })
    
    
    observeEvent(input$okModalTable, {
        withProgress(message="Common Plot Report Progress", value=0, {

            incProgress(0.2)

            
            output$table <- renderPrint({ 
                # catVar <- c("customer", "spec")
                # contVar <- c("YS", "TS", "YR", "El", "Nb")
                catVar <- c(exploreTable2TabModuleList[["tableCol1"]],
                            exploreTable2TabModuleList[["tableCol2"]],
                            exploreTable2TabModuleList[["tableCol3"]])
                contVar <- exploreTable2TabModuleList[["tableContVar"]]
                

                sourceTable <- curSampleExploreTable[,c(catVar, contVar)]
                
                if(length(catVar)==1) {
                    sourceTable[,catVar[1]] <- as.character(sourceTable[,catVar[1]])
                    stringCode <- paste0("ppp <- sourceTable %>% group_by(", 
                                         catVar[1], ")")
                }  else if(length(catVar)==2) {
                    sourceTable[,catVar[1]] <- as.character(sourceTable[,catVar[1]])
                    sourceTable[,catVar[2]] <- as.character(sourceTable[,catVar[2]])
                    stringCode <- paste0("ppp <- sourceTable %>% group_by(", 
                                         catVar[1],", ",
                                         catVar[2],")")
                }  else {
                    sourceTable[,catVar[1]] <- as.character(sourceTable[,catVar[1]])
                    sourceTable[,catVar[2]] <- as.character(sourceTable[,catVar[2]])
                    sourceTable[,catVar[3]] <- as.character(sourceTable[,catVar[3]])
                    stringCode <- paste0("ppp <- sourceTable %>% group_by(", 
                                         catVar[1],", ",
                                         catVar[2],", ",
                                         catVar[3],")")
                }
                
                # stringCode <- paste0("ppp <- sourceTable %>% group_by(", "project", ", ", "sampleOrg", ")")
                exprCode <- parse(text=stringCode)
                eval(exprCode)
                
                stringCode <- paste0( "qqq <- ppp %>%
                                 summarize(noData=n(), 
                                           mean=mean(",contVar[1],", na.rm=TRUE),
                                           min=min(",contVar[1],", na.rm=TRUE),
                                           max=max(",contVar[1], ",na.rm=TRUE),
                                           sd=sd(",contVar[1], ",na.rm=TRUE))")
                exprCode <- parse(text=stringCode)
                eval(exprCode)
                zzz <- as.data.frame(ppp)
                
                qqq$noData <- as.character(round(qqq$noData,0))
                qqq$mean <- as.character(round(qqq$mean,attr(zzz[,contVar[1]], "digit")))
                qqq$min <- as.character(round(qqq$min,attr(zzz[,contVar[1]], "digit")))
                qqq$max <- as.character(round(qqq$max,attr(zzz[,contVar[1]], "digit")))
                qqq$sd <- as.character(round(qqq$sd,attr(zzz[,contVar[1]], "digit")))

                
                rrr <- gather(qqq, "mean","min","max","sd", key="item", value="valuerrr")
                rrr <- as.data.frame(rrr)
                if(length(catVar)==1) {
                    sss <- rrr[order(rrr[,catVar[1]]),]
                } else if(length(catVar)==2) {
                    sss <- rrr[order(rrr[,catVar[1]], rrr[,catVar[2]]),]
                } else {
                    sss <- rrr[order(rrr[,catVar[1]], rrr[,catVar[2]], rrr[,catVar[3]]),]
                }
                
                colnames(sss)[which(colnames(sss)=="valuerrr")] <- contVar[1]
                summaryTable <- sss 
                
                if(length(contVar)>1) {
                    for(i in 2:length(contVar)) {
                        stringCode <- paste0( "qqq <- ppp %>%
                                 summarize(noData=n(), 
                                           mean=mean(",contVar[i],", na.rm=TRUE),
                                           min=min(",contVar[i],", na.rm=TRUE),
                                           max=max(",contVar[i], ",na.rm=TRUE),
                                           sd=sd(",contVar[i], ",na.rm=TRUE))")
                        exprCode <- parse(text=stringCode)
                        eval(exprCode)
                        qqq$noData <- as.character(round(qqq$noData,0))
                        qqq$mean <- as.character(round(qqq$mean,attr(zzz[,contVar[i]], "digit")))
                        qqq$min <- as.character(round(qqq$min,attr(zzz[,contVar[i]], "digit")))
                        qqq$max <- as.character(round(qqq$max,attr(zzz[,contVar[i]], "digit")))
                        qqq$sd <- as.character(round(qqq$sd,attr(zzz[,contVar[i]], "digit")))
                        rrr <- gather(qqq, "mean","min","max", "sd", key="item", value="valuerrr")
                        rrr <- as.data.frame(rrr)
                        if(length(catVar)==1) {
                            sss <- rrr[order(rrr[,catVar[1]]),]
                        } else if(length(catVar)==2) {
                            sss <- rrr[order(rrr[,catVar[1]], rrr[,catVar[2]]),]
                        } else {
                            sss <- rrr[order(rrr[,catVar[1]], rrr[,catVar[2]], rrr[,catVar[3]]),]
                        }
                        colnames(sss)[which(colnames(sss)=="valuerrr")] <- contVar[i]
                        summaryTempo <- sss %>% select(-"noData")
                        summaryTable <- 
                            left_join(summaryTable, summaryTempo,
                                      by=c(catVar,"item"))
                    }
                    
                }
                
                

                
                varNamesExceptNoData <- setdiff(colnames(summaryTable), "noData")
                
                summaryTable <- summaryTable[,c(varNamesExceptNoData, "noData")]
                
                func1 <- function(x) {
                    ifelse(is.na(x) | x=="NaN" | x=="Inf" | x=="-Inf", "-", x)
                }

                summaryTableCSV <<- lapply(summaryTable, func1) %>% lapply(unlist) %>% as.data.frame()
                
                
                
                write_excel_csv(summaryTableCSV, path=paste0(pathHTMLReport, "/집계표.csv"), na="")
                
                output$table <- DT::renderDataTable(DT::datatable({summaryTableCSV},
                                                                  options = list( pageLength = 20)))
                
            }) 
             
            
            
            incProgress(0.8)
            
        }) #withProgress
        
        removeModal()
        
    })
    
}