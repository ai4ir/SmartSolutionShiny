geom_line1TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(1, textOutput(ns("aesList_y")) 
            ),
            column(1, textOutput(ns("aesList_x")) 
            ),
            column(1, textOutput(ns("aesList_color"))
            ),
            column(1, textOutput(ns("aesList_size")) 
            ),
            column(1, textOutput(ns("aesList_shape")) 
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
            column(1, actionButton(ns("MCP_size"), label="size 변수 선정") 
            ),
            column(1, actionButton(ns("MCP_shape"), label="shape 변수 선정") 
            ),
            column(2 
            ),
            column(2, actionButton(ns("modalGraphOption"), label="그래프 옵션 선정") 
            ),
            column(2, actionButton(ns("modalGraphGlobal"), label="글로벌 옵션 선정") 
            ),
            column(1, actionButton(ns("graphUpdate"), label="그래프 갱신") 
            )

        ),
        
        
        fluidRow(
            column(12,
                   plotOutput(ns("scatter1"),width = "100%", height = "800px")
            )
        )
    )
}

geom_line1TabModule <- function(input, output, session) {
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
        output$aesList_size <- renderText({ 
            aesList[["size"]][1]
        })
        output$aesList_shape <- renderText({ 
            aesList[["shape"]][1]
        })
    })
    
    observeEvent(input$MCP_y, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "y"
        showModal(ModalRadioButtons(numVar, numVar, ns("okNext"), "y 변수",
                                    strExplain="y 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })

    observeEvent(input$MCP_x, {
        numVar <- rownames(orderVFcluster(curSampleExplore))
        curAes <<- "x"
        showModal(ModalRadioButtons(numVar, numVar, ns("okNext"), "x 변수",
                                    strExplain="x 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    observeEvent(input$MCP_color, {
        var <- unique(colnames(curSampleExplore))
        var <- c("NULL",var)
        curAes <<- "color"
        showModal(ModalRadioButtons(var, var, ns("okNext"), "color 변수",
                                    strExplain="color 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    observeEvent(input$MCP_size, {
        # var <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=1, maxNoLevels=7) 
        # browser()
        strExplain <- "심볼의 사이즈를 결정하는 변수를 선택해 주세요"
        var <- unique(colnames(curSampleExplore))
        var <- c("NULL",var)
        curAes <<- "size"
        showModal(ModalRadioButtons(var, var, ns("okNext"), "size 변수",
                                    strExplain="size 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    
    observeEvent(input$MCP_shape, {
        var <- selectVarWithGivenLevels(df=curSampleExplore, minNoLevels=1, maxNoLevels=7) 
        if(length(var)==0) {
            alert("수준 6 이하인 변수가 없습니다.")
            aesList[["shape"]] <<- NULL
            return()
        }
        var <- c("NULL",var)
        strExplain <- "수준이 2이상 6 이하인 변수만 선택됬습니다."
        curAes <<- "shape"
        showModal(ModalRadioButtons(var, var, ns("okNext"), "shape 변수",
                                    strExplain="shape 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    observeEvent(input$okNext, {
        treatOKNextExplore(input$selModal)
        reactAesList(aesList)
    })

    observeEvent(input$graphUpdate, {
        theme_update(axis.title=element_text(size=aesList[["axisTitleSize"]][1]))
        theme_update(axis.text=element_text(size=aesList[["axisTextSize"]][1]))
        dfGraph <- curSampleExplore
        x <- aesList[["x"]][1]
        y <- aesList[["y"]][1]
        color <- aesList[["color"]][1]
        size <- aesList[["size"]][1]
        shape <- aesList[["shape"]][1]
        
        dfGraph[,size] <- as.factor(dfGraph[,size])
        dfGraph[,shape] <- as.factor(dfGraph[,shape])
        if(!is.null(color) && !is.factor(dfGraph[,color])  && length(unique(dfGraph[,color])) < 21 && x!=color) {
            dfGraph[,color] <- as.factor(dfGraph[,color])
        }
        
        if(!is.numeric(dfGraph[,aesList[["color"]][1]]) & 
           !is.Date(dfGraph[,aesList[["color"]][1]]) &
           length(unique(dfGraph[,aesList[["color"]][1]])) > 20 )
        {
          kk <- table(dfGraph[,aesList[["color"]][1]])
          kk <- kk[order(kk, decreasing=TRUE)]
          kk <- kk[1:20]
          dfGraph[,aesList[["color"]][1]] <- ifelse(dfGraph[,aesList[["color"]][1]] %in% names(kk), 
                 dfGraph[,aesList[["color"]][1]], "기타"
                 )
        }
        
        labelAnnotate <- NULL
        if(!is.Date(dfGraph[,x]) & !is.Date(dfGraph[,y]) & !is.POSIXct(dfGraph[,x]) & !is.POSIXct(dfGraph[,y])) {
            dfModel <- dfGraph[,c(x, y)]
            colnames(dfModel) <- c("xVec", "yVec")
            switch(aesList[["fitOption"]],
                   NoFit = {},
                   Fit1 = {
                       model <- lm(yVec ~ xVec,data=dfModel)
                       myfunc <- function(x) {
                           model[["coefficients"]][1] + model[["coefficients"]][2] * x 
                       }
                       R2 <- broom::glance(model)[["r.squared"]]
                       labelAnnotate <- paste0("R2 : ", round(R2,3))
                       xAnnotate <-  min(dfGraph[,x],na.rm=TRUE) +
                           0.8*(max(dfGraph[,x],na.rm=TRUE)-min(dfGraph[,x],na.rm=TRUE))
                   },
                   Fit2 = {
                       dfModel <- dfModel %>% mutate(xVec2 = xVec * xVec)
                       model <- lm(yVec ~ xVec + xVec2,data=dfModel)
                       myfunc <- function(x) {
                           model[["coefficients"]][1] + model[["coefficients"]][2] * x + model[["coefficients"]][3]*x*x
                       }
                       R2 <- broom::glance(model)[["r.squared"]]
                       labelAnnotate <- paste0("R2 : ", round(R2,3))
                       xAnnotate <-  min(dfGraph[,x],na.rm=TRUE) +
                           0.8*(max(dfGraph[,x],na.rm=TRUE)-min(dfGraph[,x],na.rm=TRUE))

                   },
                   {}
            )

        }
        

        
        output$scatter1 <- renderPlot({
            ggObj <- ggplot(data=dfGraph,
                               aes_string(x=x, y=y,
                                          color=aesList[["color"]][1],
                                          shape=aesList[["shape"]][1])) 
            if(is.null(aesList[["size"]][1])) {
                ggObj <- ggObj + geom_line( aes_string( group=aesList[["color"]][1]), size= 5) 
            } else {
                ggObj <- ggObj + geom_line( aes_string(size=aesList[["size"]][1], color=aesList[["color"]][1])) 
            }

            if(!is.Date(dfGraph[,x]) & !is.Date(dfGraph[,y]) & !is.POSIXct(dfGraph[,x]) & !is.POSIXct(dfGraph[,y])) {
                ggObj <- ggObj + 
                    xlim(graphOption[["minX"]][1], graphOption[["maxX"]][1]) +
                    ylim(graphOption[["minY"]][1], graphOption[["maxY"]][1]) 
            }
            ggObj <- ggObj +
            labs(title=paste0(sourcingCat,"  ",chosenDFSourceFile),
                 x=graphOption[["xAxisTitle"]][1], y=graphOption[["yAxisTitle"]][1]) +
            theme(legend.title = element_text(size = 40),
                  legend.text  = element_text(size = 25)
                  # legend.key.size = unit(0.1, "lines"))
                  )      
            if(!is.null(MinReqExplore)) {
                if( !is.na(MinReqExplore[y]) && y %in% names(MinReqExplore)) 
                    ggObj <- ggObj + geom_hline(yintercept=MinReqExplore[y])
                if( !is.na(MinReqExplore[x]) && x %in% names(MinReqExplore)) 
                    ggObj <- ggObj + geom_vline(xintercept=MinReqExplore[x])
            }
            
            if(!is.null(MaxReqExplore)) {
                if( !is.na(MaxReqExplore[y]) && y %in% names(MaxReqExplore)) 
                    ggObj <- ggObj + geom_hline(yintercept=MaxReqExplore[y])
                
                if( !is.na(MaxReqExplore[x]) && x %in% names(MaxReqExplore)) 
                    ggObj <- ggObj + geom_vline(xintercept=MaxReqExplore[x])
            }
            
            dfModel <- dfGraph[,c(x, y)]
            colnames(dfModel) <- c("xVec", "yVec")
            
            if(!is.null(labelAnnotate)) {
                ggObj <- ggObj +  stat_function(fun=myfunc, geom="line", color="black") +
                    annotate("text",
                             x=xAnnotate, 
                             y=min(dfGraph[,y], na.rm=TRUE), label=labelAnnotate, color="black", size=10)
            }



            if(length(unique(dfGraph$bHOT))>1) {
                dfHot <- dfGraph[dfGraph$bHOT=="Hot",]
                ggObj <- ggObj + geom_point(data=dfHot, color="red", shape=1)
            }
            
            ggObj
        })
        
    })
                 
    observeEvent(input$modalGraphOption, {
        showModal(ModalGraphOptionGeomLine()
                  )
    })
    
    observeEvent(input$modalGraphGlobal, {
        showModal(ModalGraphOptionGlobal()
        )
    })

}


