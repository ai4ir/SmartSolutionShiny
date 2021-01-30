treatOKNextExplore <- function(selModal) {
  if(selModal=="NULL") {
    aesList[[curAes]] <<- NULL
  } else {
    aesList[[curAes]] <<- selModal
  }

  switch( curAes,
          x = {
            graphOption[["xAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["x"]][1]], "label")
            # graphOption[["xAxisTitle"]][1]<<- aesList[["x"]][1]
            if(curTabExplore %in% c("scatter1")) {
              graphOption[["minX"]][1] <<- min(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)
              graphOption[["maxX"]][1] <<- max(curSampleExplore[,aesList[["x"]][1]], na.rm=TRUE)
            }
          },
          y = {
            graphOption[["yAxisTitle"]][1]<<- attr(curSampleExplore[,aesList[["y"]][1]], "label")
            # graphOption[["yAxisTitle"]][1]<<- aesList[["y"]][1]
            graphOption[["minY"]][1] <<- min(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)
            graphOption[["maxY"]][1] <<- max(curSampleExplore[,aesList[["y"]][1]], na.rm=TRUE)
          },
          {}
  )
  removeModal()
}