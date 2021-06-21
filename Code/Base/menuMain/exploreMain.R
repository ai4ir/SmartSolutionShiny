exploreMainUI <- function() {

    tabsetPanel( id="explore", type="tabs",
                 tabPanel("scatter1", value="scatter1",
                          scatter1TabModuleUI("scatter1")
                 ),
                 # tabPanel("geom_line1", value="geom_line1",
                 #          geom_line1TabModuleUI("geom_line1")
                 # ),
                 # tabPanel("scatterGirafe1", value="scatterGirafe1",
                 #          scatterGirafe1TabModuleUI("scatterGirafe1")
                 # ),
                 tabPanel("연속형 변수 박스플롯", value="boxplot1",
                          boxplot1TabModuleUI("boxplot1")
                 ),
                 tabPanel("범주형 변수 박스플롯 1", value="boxplot2",
                          boxplot2TabModuleUI("boxplot2")
                 ),
                 tabPanel("범주형 변수 박스플롯 2", value="boxplot3",
                          boxplot3TabModuleUI("boxplot3")
                 ),
                 tabPanel("violin1", value="violin1",
                          violin1TabModuleUI("violin1")
                 ),
                 tabPanel("barCount", value="barCount",
                          barCountTabModuleUI("barCount")
                 ),
                 tabPanel("histogram1", value="histogram1",
                          histogram1TabModuleUI("histogram1")
                 ),
                 tabPanel("density1", value="density1",
                          density1TabModuleUI("density1")
                 )

                 # tabPanel("scatter2", value="scatter2",
                 #          scatter2TabModuleUI("scatter2")
                 # )

    )

}


exploreMain <- function(input, output, session) {
    
    observeEvent(input$explore, {
        curTabExplore <<- input$explore
        # triggerMCP  <<- "module"   ### or "modal"
    })

    
    callModule(scatter1TabModule,"scatter1")
    callModule(geom_line1TabModule,"geom_line1")
    # callModule(scatterGirafe1TabModule,"scatterGirafe1")
    callModule(boxplot1TabModule,"boxplot1")
    callModule(boxplot2TabModule,"boxplot2")
    callModule(boxplot3TabModule,"boxplot3")
    callModule(violin1TabModule,"violin1")
    callModule(barCountTabModule,"barCount")
    callModule(histogram1TabModule,"histogram1")
    callModule(density1TabModule,"density1")
    # callModule(scatter2TabModule,"scatter2")
    
    observe({
        treatModalGraphOptionGlobal(input, output, session)
    })
    
    observe({
        treatModalGraphOptionScatter(input, output, session)
    })
    
    observe({
        treatModalGraphOptionGeomLine(input, output, session)
    })
    
    observe({
        treatModalGraphOptionBoxplot(input, output, session)
    })
    
    observe({
        treatModalGraphOptionBar(input, output, session)
    })
    
    observe({
        treatModalGraphOptionHistogram(input, output, session)
    })
    observe({
        treatModalGraphOptionDensity(input, output, session)
    })

}


