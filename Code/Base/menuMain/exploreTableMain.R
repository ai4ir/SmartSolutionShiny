exploreTableMainUI <- function() {

    tabsetPanel( id="exploreTable", type="tabs",
                 tabPanel("집계표", value="exploreTable2",
                          exploreTable2TabModuleUI("exploreTable2")
                 ),
                 tabPanel("종합불량률", value="exploreTable1",
                          exploreTable1TabModuleUI("exploreTable1")
                 ),
                 tabPanel("히스토그램", value="exploreTableHistogram",
                          histogram1TabModuleUI("exploreTableHistogram")
                 ),
                 tabPanel("scatter1", value="scatter1",
                          scatter1TabModuleUI("exploreTableScatter1")
                 )
                 # tabPanel("항목별 불량률", value="exploreTable2",
                 #          exploreTable1TabModuleUI("exploreTable2")
                 # )

    )

}


exploreTableMain <- function(input, output, session) {
    
    observeEvent(input$exploreTable, {
        curTabExploreTable <<- input$exploreTable
        triggerMCP  <<- "module"   ### or "modal"
    })

    
    callModule(exploreTable1TabModule,"exploreTable1")
    callModule(exploreTable2TabModule,"exploreTable2")
    callModule(histogram1TabModule,"exploreTableHistogram")
    callModule(scatter1TabModule,"exploreTableScatter1")


}


