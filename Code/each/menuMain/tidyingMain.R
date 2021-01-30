
tidyingMainUI <- function() {
  tabsetPanel( type="tabs",
               
    tabPanel("TidyingSub1",
      fluidPage(
        fluidRow(
          column(3,
                 switch(sourcingCat,
                        SmartShiny = {radioButtons("sourceTidy", "원천 데이타",
                                             c("empty"="emptyTidy",
                                               "mtcars" = "mtcars",
                                               "diamonds" = "diamonds"
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
  output$strDFsourceTidy <- renderPrint({
    switch(input$sourceTidy,
           mtcars = {
             
             source("each/menuSourcing/mtcars/tidySource.R", encoding="UTF-8")
             tidySource()
           },
           diamonds = {
             
             source("each/menuSourcing/diamonds/tidySource.R", encoding="UTF-8")
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