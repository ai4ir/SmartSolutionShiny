globalOptionSS <- list(graphDataValid="valid")

globalOptionSSMainUI <- function() {
  tabsetPanel( type="tabs",
               tabPanel("globalOptionSSSub1",
                        fluidPage(
                          fluidRow(
                            column(3,
                                   radioButtons("globalOptionSS_graphDataValid", "그래프 데이터",
                                                selected = globalOptionSS[["graphDataValid"]],
                                                c("유효 데이터"="valid",
                                                  "전체 데이터"="whole"
                                                ))
                            ),
                            column(9,
                                   # verbatimTextOutput("strDFsourceTidy")
                                   )
                          )
                        )
                        
               )
  )
}

globalOptionSSMain <- function(input, output, session) {
  
  observeEvent(input$globalOptionSS_graphDataValid, {
    globalOptionSS[["graphDataValid"]] <<- input$globalOptionSS_graphDataValid

  })    

}