
tidyingMainUI <- function() {
  tabsetPanel( type="tabs",
    tabPanel("TidyingSub1",
      fluidPage(
        fluidRow(
          column(2,
                 actionButton("renderReportTidying", "데이터 전처리 리포트")
                 ),
          column(10,  )
        ),
        tags$hr(),
        fluidRow(
          column(3,
                 switch(sourcingCat,
                        Hyundai = {radioButtons("sourceTidy", "원천 데이타",
                                             c("empty"="emptyTidy",
                                               "mtcars" = "mtcarsTidy"
                                               # "범용"="EXCEL", "clipboard"
                                             ))}    
                 )
          ),
          column(9,
                 verbatimTextOutput("strDFsourceTidy"))
        )
      )

    )
  )
}

tidyingMain <- function(input, output, session) {
  
  # 범용 리포트
  treatTidyingMainEventBase(input, output,session)
  
  output$strDFsourceTidy <- renderPrint({
    switch(input$sourceTidy,

           tensileHeatTidy = {
             source("each/menuSourcing/tensileHeat/tidySource.R", encoding="UTF-8")
             tidySource()
           },
           
           EH47BCATidy = {
             source("each/menuSourcing/EH47BCA/tidySource.R", encoding="UTF-8")
             tidySource()
           },
           
           mtcarsTidy = {
             source("each/menuSourcing/mtcars/tidySource.R", encoding="UTF-8")
             tidySource()
           },

           emptyTidy = {
             DFSource <<- NA
             return()
           },
           {}
    )
    
    hideTab(inputId="SmartReportMain", target="Sampling")
    hideTab(inputId="SmartReportMain", target="Explore")
    hideTab(inputId="SmartReportMain", target="Modeling")
    hideTab(inputId="SmartReportMain", target="Predict")
    hideTab(inputId="SmartReportMain", target="Optimize")

    str(DFSource)

  })   
    

}