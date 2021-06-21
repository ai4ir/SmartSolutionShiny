tidyingList <- list(remainedVar=NULL, removedVar=NULL, addedVar = NULL, removedVarNA=NULL)


treatTidyingMainEventBase <- function(input, output, session) {
  
  
  observeEvent(input$renderReportTidying, {
    
    withProgress(message="전처리 리포트 작성중", value=0, {
      incProgress(0.2)
      
      pathFileRmdTidying <<- "Base/menuTidying/Rmd/baseReportTidying.Rmd"

      outputFileName <- "tempo.html"
      params <- list(df=dfAll, tidyingList=tidyingList )
      outputFileName <- paste0("tidyingReport", fromReportCommon, "_","general", ".html")
      outputFileFinalName <- paste0("전처리 리포트", fromReportCommon, "_",chosenDFSourceFile,
                                    "-",dim(DFSource)[[1]], "-",dim(DFSource)[[2]], ".html")
      options(warn=-1)
      rmarkdown::render(pathFileRmdTidying, output_file = outputFileName,
                        output_dir = pathTidyingReport,
                        params = params,
                        envir = new.env(parent = globalenv()), encoding="UTF-8")
      file.rename(paste0(pathTidyingReport,"/",outputFileName), paste0(pathTidyingReport,"/",outputFileFinalName))
      incProgress(0.8)

    }) #withProgress(message="리포트 작성중", value=0, {
    strAlert <- paste0(pathTidyingReport,"에 ", outputFileFinalName,"이 저장되었습니다.")
    alert(strAlert)
    removeModal()

  })
  
  
  
}

