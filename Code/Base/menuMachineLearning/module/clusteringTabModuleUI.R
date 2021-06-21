clusteringTabModuleUI <- function(Id) {
    ns <- NS(Id)
    fluidPage(

        fluidRow(
            column(1,
                   actionButton(ns("ClusteringVar"),"Clustering 변수 선정")
            ),
            column(1,
                   numericInput(ns("noCluster"),"Cluster 개수",2, min=1, max=9)
            ),
            column(1,
                   actionButton(ns("renderCluster"),"Cluster 재구성")
            ),

            column(2,
                   selectInput(ns("yVarLeft"),"왼쪽 y축 변수",choices="empty")
            ),
            column(2,
                   selectInput(ns("xVarLeft"),"왼쪽 x축 변수",choices="empty")
            ),
            column(2,
                   selectInput(ns("yVarRight"),"오른쪽 y축 변수",choices="empty")
            ),
            column(2,
                   selectInput(ns("xVarRight"),"오른쪽 x축 변수",choices="empty")
            ),
            column(1,
                   actionButton(ns("updateGraph"),"그래프 갱신")
            )
        ),
        fluidRow(
            verbatimTextOutput(ns("value"))
        ),
        fluidRow( column(6, plotOutput(ns("ScatterLeft"))),
                  column(6, plotOutput(ns("ScatterRight")))
        )
        

    )
}

clusteringTabModule <- function(input, output, session) {
    
    ns <-session$ns
    

    
    observeEvent(input$ClusteringVar, {

        numVar <- rownames(orderVFcluster(curSampleExplore))
        showModal(ModalCheckboxGroup(title="clustering Var 선정 대화창", modalCheckboxID=ns("selClusteringVarModal"),
                                     label="clustering Var 선정",
                                     choiceNames=numVar, choiceValues=numVar,
                                     modalOKButtonID=ns("okClusteringVarModal")))

    })
    
    observeEvent(input$okClusteringVarModal, {
        selVar <- input$selClusteringVarModal       
        output$value <- renderText({ 
            if (is.null(selVar))
                "No data selected"
            else
                paste0("선정된 변수 : ",str_c(selVar,collapse=","))
        })
        removeModal()


    })
    
    observeEvent(input$renderCluster, {
        alert("개발중입니다. 신속한 개발이 필요하면 연락 주세요.")
        return()
        selVar <- isolate(input$selClusteringVarModal)
        df <- curSampleExplore[,c("rowNoSource",selVar)]
        
        boolValidRow <- !is.na(df[,selVar[1]])
        for(i in 2:length(selVar)) {
            boolValid <- !is.na(df[,selVar[i]])
            boolValidRow <- boolValidRow & boolValid
        }
        
        curSampleCluster <<- df[boolValidRow,]
        
        df <- curSampleCluster[,selVar]
        
        # Hartigan-Wong , Lloyd, MacQueen
        clustersK <<- kmeans(df, isolate(input$noCluster), algorithm="MacQueen") 
        
        if (!is.null(selVar) ) {
            
            # vals$ModalVar <- input$ModalVar
            # selVar <- numVarExplore[as.numeric(vals$ModalVar)]
            updateSelectInput(session,"yVarLeft", choices=selVar)
            updateSelectInput(session,"xVarLeft", choices=selVar)
            updateSelectInput(session,"yVarRight", choices=selVar)
            updateSelectInput(session,"xVarRight", choices=selVar)

        } else {
            numVar <- extractNumVarName(curSampleCluster)
            showModal(ModalCheckboxGroup(title="clustering Var 선정 대화창", modalCheckboxID=ns("selClusteringVarModal"),
                                         label="clustering Var 선정",
                                         choiceNames=numVar, choiceValues=numVar,
                                         modalOKButtonID=ns("okClusteringVarModal")))
        }
        
        clusterKmeans <- as.character(clustersK$cluster)
        curSampleCluster <<- curSampleCluster %>% mutate(clusterKmeans=clusterKmeans)
        
        curSampleExplore <<- curSampleExplore %>% select(setdiff(colnames(curSampleExplore),"clusterKmeans"))
        
        curSampleExplore <<- curSampleExplore %>% 
            left_join(curSampleCluster[,c("rowNoSource","clusterKmeans")],by="rowNoSource")
        
        aesList[["clusterMethod"]][1] <<- "clusterKmeans"
        
        
        
    })
    
    observeEvent(input$updateGraph, {
        xLeft <- isolate(input$xVarLeft)
        xRight <- isolate(input$xVarRight)
        yLeft <- isolate(input$yVarLeft)
        yRight <- isolate(input$yVarRight)
        
        
        output$ScatterLeft <- renderPlot({
            clCenters <- as.data.frame(clustersK$centers)
            
            df <- curSampleExplore
            ggplot(data=df, aes_string(x=xLeft, y=yLeft, color="clusterKmeans")) +
                geom_point(size=3) + labs(x=xLeft, y=yLeft) +
                geom_point(data=clCenters, x=clCenters[,xLeft], y=clCenters[,yLeft], color="black", pch=4, size=10 )
            

        })
        
        output$ScatterRight <- renderPlot({

            clCenters <- as.data.frame(clustersK$centers)
                
            df <- curSampleExplore
            ggplot(data=df, aes_string(x=xRight, y=yRight, color="clusterKmeans")) +
                geom_point(size=3) + labs(x=xRight, y=yRight) +
                geom_point(data=clCenters, x=clCenters[,xRight], y=clCenters[,yRight], color="black", pch=4, size=10 )
            

        })
        
        

    })
    

    

    

    

    
}