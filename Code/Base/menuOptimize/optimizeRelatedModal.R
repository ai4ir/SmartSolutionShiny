### optimizeRelatedModal ###

ModalFileOptimize <- function(pathDir, failed = FALSE) {
  if(length(dir(pathDir))==0) {
    # alert(paste0(pathDir, "에 파일이 하나도 없습니다.  빈 파일을 만드세요."))
    # return()
    choices="empty.Rdata"
  } else {
    choices=dir(pathDir)
  }
    
  
  modalDialog(
      title="파일 이름 선정",
      tags$p("새로운 이름으로 저장하는 경우, 가장 유사한 이름을 선택하세요!"),

      radioButtons("nameFileOptimize","파일명",choices=choices ),

      # print(paste0("ModalCheckboxCatVarExplore - selVar :", selVar)),
      
      if (failed)
          div(tags$b("Invalid data", style = "color: red;")),
      
      
      footer = tagList(
          modalButton("Cancel"),
          actionButton("renderModalFileOptimize2", "계속")
      ),
      size="l"
  )
}


ModalFileOptimize2 <- function(selFileName, failed = FALSE) {
    modalDialog(
        title="영역 파일 관리",
        
        textInput("selNameFileOptimize","파일명 입력", value=selFileName),

        if (failed)
            div(tags$b("Invalid data", style = "color: red;")),
        
        
        footer = tagList(
            modalButton("Cancel"),
            actionButton("loadModalFileOptimize", "열기"),
            actionButton("saveModalFileOptimize", "저장")
            
        ),
        size="m"
    )
}

