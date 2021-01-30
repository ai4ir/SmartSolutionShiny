predPlot2TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(12,
                   plotOutput(ns("plot"),width = "100%", height = "1000px")
            )
        )
    )
}

predPlot2TabModule <- function(input, output, session) {
    ns <-session$ns
    
    # observe({
    # 
    #     switch(input$predict,
    #            predictTab1 = {
    #                
    #            },
    #            predPlot1 = {
    #                
    #            }
    #     )
    # })
    # 
    output$plot <- renderPlot({
        reactPredVal()
        ggObjPred2
    })

}


