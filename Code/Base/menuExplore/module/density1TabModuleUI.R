density1TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1
            ),
            column(1, textOutput(ns("aesList_x")) 
            ),
            column(1
            ),
            column(1, textOutput(ns("aesList_fill"))
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
            column(1 
            ),
            column(1, actionButton(ns("MCP_x"), "X 변수 선정") 
            ),
            column(1
            ),
            column(1, actionButton(ns("MCP_fill"), "fill 변수 선정")
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
                   plotOutput(ns("graph"),width = "100%", height = "800px")
            )
        )
    )
}

density1TabModule <- function(input, output, session) {
    ns <-session$ns
    
    observeEvent( reactAesList(), {
        output$aesList_x <- renderText({ 
            aesList[["x"]][1]
        })
        output$aesList_fill <- renderText({ 
            aesList[["fill"]][1]
        })
    })
    
    observeEvent(input$MCP_x, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "x"
        strExplain <- "x 변수를 선정하세요 "
        showModal(ModalRadioButtons(numVar, numVar,ns("okNext"), ns("selModal"), "x 변수", "x 변수 선정",strExplain))
    })
    
    observeEvent(input$MCP_fill, {
        catVar <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=2, maxNoLevels=7) 
        if(length(catVar)==0) {
            alert("수준 7 이하인 변수가 없습니다.")
            aesList[["x"]] <<- NA
            return()
        }
        catVar <- c("NULL", catVar)
        curAes <<- "fill"
        strExplain <- "fill 변수를 선정하세요 "
        showModal(ModalRadioButtons(catVar, catVar,ns("okNext"), ns("selModal"), "fill 변수", "fill 변수 선정",strExplain))
    })
    
    observeEvent(input$okNext, {
        treatOKNextExplore(input$selModal)
        reactAesList(aesList)
    })

    observeEvent(input$graphUpdate, {
        reactAesList(aesList)
        theme_update(axis.title=element_text(size=graphOption[["axisTitleSize"]][1]))
        theme_update(axis.text=element_text(size=graphOption[["axisTextSize"]][1]))
        output$graph <- renderPlot({
            x <- aesList[["x"]][1]
            dfGraph <- curSampleExplore
            if(!is.null(aesList[["fill"]][1])) {
                dfGraph[,aesList[["fill"]][1]] <- as.factor(dfGraph[,aesList[["fill"]][1]])
            }

            ggObj <- ggplot(data=dfGraph,
                               aes_string(x=aesList[["x"]][1])) +
            geom_density(aes_string(fill=aesList[["fill"]][1]), alpha=0.25) +
            # geom_jitter() +
            # xlim(graphOption[["minX"]][1], graphOption[["maxX"]][1]) +
            # ylim(graphOption[["minY"]][1], graphOption[["maxY"]][1]) +
            # guides(color = guide_legend(override.aes = list(size = 10))) +
            labs(title=paste0(sourcingCat,"  ",chosenDFSourceFile),
                 x=graphOption[["xAxisTitle"]][1]) +
            theme(legend.title = element_text(size = 40),
                  legend.text  = element_text(size = 25)) 
            
            if( x %in% names(MinReqExplore) && !is.na(MinReqExplore[x])) 
                ggObj <- ggObj + geom_vline(xintercept=MinReqExplore[x])
            if( x %in% names(MaxReqExplore) && !is.na(MaxReqExplore[x])) 
                ggObj <- ggObj + geom_vline(xintercept=MaxReqExplore[x])
            
            ggObj
        })
        
    })
                 
    observeEvent(input$modalGraphOption, {
        showModal(ModalGraphOptionDensity())
    })
    

}