boxplot1TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1, textOutput(ns("aesList_y")) 
            ),
            column(1, textOutput(ns("aesList_x")) 
            ),
            column(1, textOutput(ns("aesList_grid"))
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
            column(1, actionButton(ns("MCP_grid"), label="Grid 변수 선정")
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
            column(1, 
                   tags$h1("-----"),
                   tags$h1("-----"),
                   tags$h1("-----"),
                   tags$h1("-----"),
                   radioButtons(ns("groupMethod"), "인터벌 설정 방법", 
                                c("등간격"="sameInterval", "박스 갯수"="sameNumber")),
                   tags$p(""),
                   tags$p("숫자 입력 필수"),
                   numericInput(ns("intervalValue"), "간격 혹은 갯수",NA)
            ),
            column(11,
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

boxplot1TabModule <- function(input, output, session) {
    ns <-session$ns
    
    observeEvent( reactAesList(), {
        output$aesList_y <- renderText({ 
            aesList[["y"]][1]
        })
        output$aesList_x <- renderText({ 
            aesList[["x"]][1]
        })
        output$aesList_grid <- renderText({ 
            aesList[["grid"]][1]
        })
    })
    
    observeEvent(input$MCP_y, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "y"
        strExplain <- "y 변수를 선정하세요 "
        showModal(ModalRadioButtons(numVar, numVar,ns("okNext"), ns("selModal"), "y 변수", "y 변수 선정",strExplain))
    })
    
    observeEvent(input$MCP_x, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "x"
        strExplain <- "x 변수를 선정하세요 "
        showModal(ModalRadioButtons(numVar, numVar,ns("okNext"), ns("selModal"), "x 변수", "x 변수 선정",strExplain))
    })
    
    observeEvent(input$MCP_grid, {
        catVar <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=2, maxNoLevels=5)
        catVar <- c("NULL", catVar)
        if(length(catVar)==0) {
            alert("수준 2 이하인 변수가 없습니다.")
            aesList[["x"]] <<- NA
            return()
        }
        curAes <<- "grid"
        strExplain <- "grid 변수를 선정하세요 "
        showModal(ModalRadioButtons(catVar, catVar,ns("okNext"), ns("selModal"), "grid 변수", "grid 변수 선정",strExplain))
    })
    
    observeEvent(input$okNext, {
        treatOKNextExplore(input$selModal)
        reactAesList(aesList)
    })
    
    observeEvent(input$graphUpdate, {
        reactAesList(aesList)
        theme_update(axis.title=element_text(size=graphOption[["axisTitleSize"]][1]))
        theme_update(axis.text=element_text(size=graphOption[["axisTextSize"]][1]))
        dfGraph <- curSampleExplore
        
        x <- aesList[["x"]][1]
        y <- aesList[["y"]][1]
        color <- aesList[["color"]][1]
        grid <- aesList[["grid"]][1]
        interval <- isolate(input$intervalValue)

        dfGraph[,color] <- as.factor(dfGraph[,color])

        output$plot1 <- renderPlot({

            # ggObj <- ggplot(data=dfGraph,
            #                    aes_string(x=x, y=y,
            #                               color=color)) +
            ggObj <- ggplot(data=dfGraph,
                            aes_string(x=x, y=y))  
            if(isolate(input$groupMethod)=="sameInterval") {
                if(!is.na(x) & !is.na(interval)) {
                    stringCode <- paste0("ggObj <- ggObj +
                        geom_boxplot(mapping=aes(group=(cut_width(",x,",",interval,"))))") 
                    eval(parse(text=stringCode))
                } 
            } else {
                if(!is.na(x) & !is.na(interval)) {
                    stringCode <- paste0("ggObj <- ggObj +
                        geom_boxplot(mapping=aes(group=(cut_number(",x,",",interval,"))))") 
                    eval(parse(text=stringCode))
                } 
                
            }
            
            if(!is.null(grid)) {
                stringCode <- paste0("ggObj <- ggObj + facet_grid(~",grid,")") 
                eval(parse(text=stringCode))
            }

            


            # xlim(graphOption[["minX"]][1], graphOption[["maxX"]][1]) +
            # ylim(graphOption[["minY"]][1], graphOption[["maxY"]][1]) +
            # guides(color = guide_legend(override.aes = list(size = 10))) +
            ggObj <- ggObj + 
            labs(title=paste0(sourcingCat,"  ",chosenDFSourceFile),
                 x=graphOption[["xAxisTitle"]][1], y=graphOption[["yAxisTitle"]][1]) +
            theme(legend.title = element_text(size = 40),
                  legend.text  = element_text(size = 25))
                  # legend.key.size = unit(0.1, "lines"))
            
            if( y %in% names(MinReqExplore) && !is.na(MinReqExplore[y])) 
                ggObj <- ggObj + geom_hline(yintercept=MinReqExplore[y])
            if( y %in% names(MaxReqExplore) && !is.na(MaxReqExplore[y])) 
                ggObj <- ggObj + geom_hline(yintercept=MaxReqExplore[y])

            ggObj
        
            })
        
        # output$table1 <- renderTable({
        #     stringCode <- paste0("kk <- dfGraph %>% group_by(", x, ", ", color, ")")
        #     exprCode <- parse(text=stringCode)
        #     eval(exprCode)
        #     
        #     stringCode <- paste0( "kk <- kk %>% summarize(noData=n()) %>% spread(key=",
        #                           x, ", value=noData)")
        #     exprCode <- parse(text=stringCode)
        #     eval(exprCode)
        #     
        #     # kk <- kk %>%
        #     #     summarize(noData=n()) %>% spread(key=project, value=noData)
        #     
        # }, striped=TRUE, hover=TRUE, bordered=TRUE)
        
    })
                 
    observeEvent(input$modalGraphOption, {
        showModal(ModalGraphOptionBoxplot())
    })
    

}