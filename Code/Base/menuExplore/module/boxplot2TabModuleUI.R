boxplot2TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1, textOutput(ns("aesList_y")) 
            ),
            column(1, textOutput(ns("aesList_x")) 
            ),
            column(1, textOutput(ns("aesList_color"))
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
            column(1 
            )
            
        ),
        fluidRow(
            column(1, actionButton(ns("MCP_y"), label="Y 변수 선정") 
            ),
            column(1, actionButton(ns("MCP_x"), "X 변수 선정") 
            ),
            column(1, actionButton(ns("MCP_color"), label="color 변수 선정")
            ),
            column(1 
            ),
            column(1 
            ),
            column(2 
            ),
            column(2, actionButton(ns("modalGraphOption"), label="그래프 옵션 선정") 
            ),
            column(2 
            ),
            column(1, actionButton(ns("graphUpdate"), label="그래프 갱신") 
            )

        ),
        

        
        fluidRow(
            column(12,
                   plotOutput(ns("plot1"),width = "100%", height = "800px")
            )
        ),
        fluidRow(
            column(12,
                   tableOutput(ns("table1"))
            )
        )
    )
}

boxplot2TabModule <- function(input, output, session) {
    ns <-session$ns
    
    observeEvent( reactAesList(), {
        output$aesList_y <- renderText({ 
            aesList[["y"]][1]
        })
        output$aesList_x <- renderText({ 
            aesList[["x"]][1]
        })
        output$aesList_color <- renderText({ 
            aesList[["color"]][1]
        })
    })
    
    observeEvent(input$MCP_y, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "y"
        strExplain <- "y 변수를 선정하세요 "
        showModal(ModalRadioButtons(numVar, numVar,ns("okNext"), ns("selModal"), "y 변수", "y 변수 선정",strExplain))
    })

    observeEvent(input$MCP_x, {
        catVar <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=2, maxNoLevels=20) 
        if(length(catVar)==0) {
            alert("수준 20 이하인 변수가 없습니다.")
            aesList[["x"]] <<- NA
            return()
        }
        curAes <<- "x"
        strExplain <- "x 변수를 선정하세요 "
        showModal(ModalRadioButtons(catVar, catVar,ns("okNext"), ns("selModal"), "x 변수", "x 변수 선정",strExplain))
    })
    
    observeEvent(input$MCP_color, {
        catVar <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=2, maxNoLevels=20) 
        if(length(catVar)==0) {
            alert("수준 20 이하인 변수가 없습니다.")
            aesList[["x"]] <<- NA
            return()
        }
        catVar <- c("NULL", catVar)
        curAes <<- "color"
        strExplain <- "color 변수를 선정하세요 "
        showModal(ModalRadioButtons(catVar, catVar,ns("okNext"), ns("selModal"), "color 변수", "color 변수 선정",strExplain))
    })
    
    observeEvent(input$okNext, {
        treatOKNextExplore(input$selModal)
        reactAesList(aesList)
    })

    observeEvent(input$graphUpdate, {
        reactAesList(aesList)
        theme_update(axis.title=element_text(size=graphOption[["axisTitleSize"]][1]))
        theme_update(axis.text=element_text(size=graphOption[["axisTextSize"]][1]))

        x <- aesList[["x"]][1]
        y <- aesList[["y"]][1]
        color <- aesList[["color"]][1]
        varName <- unique(c(x,y,color))
        dfGraph <- curSampleExplore[,varName]  
        if(globalOptionSS[["graphDataValid"]]=="valid") {
            dfGraph <- validateDF(dfGraph)
        }
        
        dfGraph[,x] <- as.factor(dfGraph[,x])

        if(!is.numeric(dfGraph[,x]) & 
           length(unique(dfGraph[,x])) > 6 )
        {
            kk <- table(dfGraph[,x])
            kk <- kk[order(kk, decreasing=TRUE)]
            kk <- kk[1:6]
            dfGraph[,x] <- ifelse(dfGraph[,x] %in% names(kk), 
                                  dfGraph[,x], "기타"
            )
        }

        output$plot1 <- renderPlot({

            # ggplot(data=dfGraph,
            #                aes_string(x=x, y=y,
            #                           group=aesList[["color"]][1], color=aesList[["color"]][1]) ) +
            #    geom_boxplot()
            
            ggObj <- ggplot(data=dfGraph,
                            aes_string(x=x, y=y,
                                       color=aesList[["color"]][1])) + geom_boxplot() 
            ggObj <- ggObj +
                labs(title=paste0(sourcingCat,"  ",chosenDFSourceFile),
                     x=graphOption[["xAxisTitle"]][1], y=graphOption[["yAxisTitle"]][1],
                     fill=attr(dfGraph[,fill],"labelShort")) +
                theme(legend.title = element_text(size = 40),
                      legend.text  = element_text(size = 25))
            # legend.key.size = unit(0.1, "lines"))
            
            if( y %in% names(MinReqExplore) && !is.na(MinReqExplore[y])) 
                ggObj <- ggObj + geom_hline(yintercept=MinReqExplore[y])
            if( y %in% names(MaxReqExplore) && !is.na(MaxReqExplore[y])) 
                ggObj <- ggObj + geom_hline(yintercept=MaxReqExplore[y])
            
            ggObj
        })
        
        output$table1 <- renderTable({
            stringCode <- paste0("kk <- dfGraph %>% group_by(", x, ", ", color, ")")
            exprCode <- parse(text=stringCode)
            eval(exprCode)

            stringCode <- paste0( "kk <- kk %>% summarize(noData=n()) %>% spread(key=",
                                  x, ", value=noData)")
            exprCode <- parse(text=stringCode)
            eval(exprCode)
            
        }, striped=TRUE, hover=TRUE, bordered=TRUE)
        
    })
                 
    observeEvent(input$modalGraphOption, {
        showModal(ModalGraphOptionBoxplot())
    })
    

}