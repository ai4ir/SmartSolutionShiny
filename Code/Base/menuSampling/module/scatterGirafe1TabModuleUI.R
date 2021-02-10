scatterGirafe1TabModuleUI <- function(Id) {

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
            column(1, textOutput(ns("aesList_tooltip"))  
            ),
            column(1, textOutput(ns("aesList_data_id"))  
            ),
            column(1
            ),
            column(1 
            ),
            column(3 
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
            column(1, actionButton(ns("MCP_tooltip"), label="tooltip 변수 선정") 
            ),
            column(1, actionButton(ns("MCP_data_id"), label="girafe 선정 변수 선정") 
            ),
            column(2, actionButton(ns("modalGraphOption"), label="그래프 옵션 선정") 
            ),
            column(2, actionButton(ns("modalGraphGlobal"), label="글로벌 옵션 선정") 
            ),
            column(1, actionButton(ns("graphUpdate"), label="그래프 갱신") 
            )

        ),
        

        
        fluidRow(
            column(8,
                   girafeOutput(ns("scatter1"),width = "100%", height = "800px")
            ),
            column(4, DT::dataTableOutput(ns("scatterGirafe1Right"))
            )
        )
    )
}

scatterGirafe1TabModule <- function(input, output, session) {
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
        output$aesList_tooltip <- renderText({ 
            aesList[["tooltip"]][1]
        })
        output$aesList_data_id <- renderText({ 
            aesList[["data_id"]][1]
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
    
    observeEvent(input$MCP_tooltip, {
        charVar <- extractCharVarName(curSampleExplore)
        curAes <<- "tooltip"
        showModal(ModalRadioButtons(charVar, charVar, ns("okNext"), "tooitip 변수",
                                    strExplain="tooltip 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    observeEvent(input$MCP_data_id, {
        charVar <- extractCharVarName(curSampleExplore)
        curAes <<- "data_id"
        showModal(ModalRadioButtons(charVar, charVar, ns("okNext"), "data_id 변수",
                                    strExplain="girafe data_id 변수를 선정하세요",
                                    modalRadioButtonsID=ns("selModal"),failed = FALSE))
    })
    
    observeEvent(input$okNext, {
        treatOKNextExplore(input$selModal)
        reactAesList(aesList)
    })

    observeEvent(input$graphUpdate, {
        aesListScatter <<- aesList
        aesListScatter[["color"]] <<- "clusterLassoSel"
        reactAesList(aesList)
        theme_update(axis.title=element_text(size=graphOption[["axisTitleSize"]][1]))
        theme_update(axis.text=element_text(size=graphOption[["axisTextSize"]][1]))
        dfGraph <- curSampleExplore
        if(!is.numeric(dfGraph[,aesList[["color"]][1]]) & 
           length(unique(dfGraph[,aesList[["color"]][1]])) > 20 )
        {
          kk <- table(dfGraph[,aesList[["color"]][1]])
          kk <- kk[order(kk, decreasing=TRUE)]
          kk <- kk[1:20]
          dfGraph[,aesList[["color"]][1]] <- ifelse(dfGraph[,aesList[["color"]][1]] %in% names(kk), 
                 dfGraph[,aesList[["color"]][1]], "기타"
                 )
        }
        
        # dfGraph[,aesList[["color"]][1]] <- colorVarContent
        x <- aesList[["x"]][1]
        y <- aesList[["y"]][1]
        color <- aesList[["color"]]
        size <- aesList[["size"]]
        shape <- aesList[["shape"]]
        tooltip <- aesList[["tooltip"]]
        data_id <- aesList[["data_id"]]
        
        curSelGirafe <- input$scatter1_selected
        
        output$scatter1 <- renderGirafe({
            renderScatterPlot(curSampleExplore, x, y, color=color, size=size, shape=shape,
                              tooltip = tooltip, data_id=data_id, curSelGirafe=curSelGirafe)
            # browser()
            # girafe(ggObj=gg1)
            # girafe( code = print(gg1), width_svg = 8, height_svg = 4)
            # girafe( code = print(gg1), width_svg = 8, height_svg = 4)

        })
        
        aesList[["spare1"]] <<- aesList[["y"]]
        
        output$scatterGirafe1Right <- DT::renderDataTable({
            selected <- input$scatter1_selected
            # browser()
            curSampleExplore[curSampleExplore[,aesList[["data_id"]][1]] %in% selected,1:10]
        })
        
    })
    
    
    observeEvent(input$scatter1_selected, {
        selected <- input$scatter1_selected
        output$scatterGirafe1Right <- DT::renderDataTable({
            # browser()
            curSampleExplore[curSampleExplore[,aesList[["data_id"]][1]] %in% selected,1:10]
        })
        
        clusterLasso <- ifelse(curSampleExplore[,"rowNoSource"] %in% selected, "inLasso", "outLasso")
        attr(clusterLasso, "label") <- "cluseter By Lasso" ; attr(clusterLasso, "labelShort") <- "cluseterByLasso" ;
        curSampleExplore <<- curSampleExplore %>% 
            mutate(clusterLassoSel = clusterLasso)
        
        bHOT <- ifelse(clusterLasso=="inLasso","Hot", "Normal")
        attr(bHOT, "label") <- "bHOT" ;   attr(bHOT, "labelShort") <- "bHOT" ; 
        curSampleExplore[,"bHOT"] <<- bHOT
        curSampleExplore <<- sticky_all(curSampleExplore)
        
        aesList[["clusterMethod"]][1] <<- "clusterLassoSel"
        
        
    })

                 
    observeEvent(input$modalGraphOption, {
        showModal(ModalGraphOptionScatter())
    })

    observeEvent(input$modalGraphGlobal, {
        showModal(ModalGraphOptionGlobal()
        )
    })
    
}



