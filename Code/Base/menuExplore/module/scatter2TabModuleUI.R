scatter2TabModuleUI <- function(Id) {

    ns <- NS(Id)
    fluidPage(
        fluidRow(
            column(2, actionButton(ns("xSel"), "X 변수 선정") 
            ),
            column(2, actionButton(ns("ySel"), "Y 변수 선정") 
            )


        ),
        
        fluidRow(
            column(6,
                   plotOutput(ns("VF_Meas"))
            ),
            column(6,
                   plotOutput(ns("VF_Meas2"))
            )
        )
    )
}

scatter2TabModule <- function(input, output, session) {
    
    ns <-session$ns
    
    observeEvent(input$xSel, {
        alert(paste0("현재 탭은 ", curTabExplore, "입니다."))
    })



}