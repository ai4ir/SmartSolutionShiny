

modelingMainUI <- function() {
  tabsetPanel( type="tabs",
               
    tabPanel("modelingSub1",
      fluidPage(
        fluidRow(
          column(2, actionButton("developModel", "모델 갱신") ),
          column(2, actionButton("renderReportModeling", "갱신 리포트") )
        ),
        tags$hr(),
        fluidRow(
          column(3,
                 switch(sourcingCat,
                        SmartShiny = {radioButtons("modelTemplate", "갱신 모델 선택",
                                                c("mtcars 연비 모델"="mtcars"
                                                  # "범용"="EXCEL", "clipboard"
                                                ))}
                 )
          ),
          column(9,
                 verbatimTextOutput("strModeling"))
        )
      )

    )
  )
  

}

modelingMain <- function(input, output, session) {
  observe({
    treatModalModeling(input, output, session)
  })

  observeEvent(input$developModel, {
    dfModel <- curSampleExplore
    switch(input$modelTemplate,
           mtcars = {
             dfModel <- dfModel %>% mutate(clusterGr="mtcars")
             dfModelNest <- dfModel %>% 
               group_by(clusterGr) %>% 
               nest()
             modelY <- "mpg"
             modelDirectRaw <- vector("list", length(dfModelNest$clusterGr))
             modelDirectRaw[[1]] <- c("wt","am","gear","hp")
             modelFramework <- vector("list", length(dfModelNest$clusterGr))
             model <- vector("list", length(dfModelNest$clusterGr))
             modelFramework[[1]] <- function(df) {
               lm(mpg ~ wt + am + gear + hp, data = df)
             }
           },
           diamonds = {

           },
           {}
           
    )

    dfModelNest <- dfModelNest %>% 
      mutate(
        modelFramework = modelFramework,
        model=model,
        modelDirectRaw = modelDirectRaw,
        modelY = modelY
      )
    # dfModelNest$modelFramework[[2]]
    for(i in 1:length(dfModelNest$clusterGr)) {
      dfModelNest$model[[i]] <- dfModelNest$modelFramework[[i]](dfModelNest$data[[i]])
    }
    # dfModelNest$model[[1]]
    pathModel <- paste0("../Model/",input$modelTemplate)
    save(dfModelNest,
         file=paste(pathModel,  "/모델.Rdata", sep=""))
    strAlert <- paste0(pathModel,"에 모델.Rdata가 저장되었습니다.")
    alert(strAlert)
    
  })
  observeEvent(input$renderReportModeling, {
    pathModel <- paste0("../Model/",input$modelTemplate)
    load(file=paste(pathModel,  "/모델.Rdata", sep=""))
    dfModelNest <<- dfModelNest
    # dfReportCommon <<- as.data.frame(dfModelNest[["data"]][1])
    dfReportCommon <<- curSampleExplore
    # dfModelNest$model[[2]]
    triggerMCP <<- "modelResult"
    aesList[["y"]] <<- dfModelNest$modelY[1]
    showModal(ModalModeling())
    
  })

}