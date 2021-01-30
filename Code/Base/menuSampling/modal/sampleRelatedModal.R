### sampleRelatedModal ###


# ModalCheckboxCatVarExplore <<- function(title="대화창", modalCheckboxID, label="label", choiceNames, choiceValues,
#                                         modalOKButtonID, failed = FALSE) {
#     modalDialog(
#         title=title,
#         
#         checkboxGroupInput(modalCheckboxID,label, choiceNames = choiceNames, choiceValues=choiceValues ),
#         
#         # print(paste0("ModalCheckboxCatVarExplore - selVar :", selVar)),
#         
#         if (failed)
#             div(tags$b("Invalid data", style = "color: red;")),
#         
#         
#         footer = tagList(
#             modalButton("취소"),
#             actionButton(modalOKButtonID, "OK")
#         ),
#         size="l"
#     )
# }


ModalFileDomain <- function(pathDir, failed = FALSE) {
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

      radioButtons("nameFileDomain","파일명",choices=choices ),

      # print(paste0("ModalCheckboxCatVarExplore - selVar :", selVar)),
      
      if (failed)
          div(tags$b("Invalid data", style = "color: red;")),
      
      
      footer = tagList(
          modalButton("Cancel"),
          actionButton("renderModalFileDomain2", "계속")
      ),
      size="l"
  )
}


ModalFileDomain2 <- function(selFileName, failed = FALSE) {
    modalDialog(
        title="영역 파일 관리",
        
        textInput("selNameFileDomain","파일명 입력", value=selFileName),

        if (failed)
            div(tags$b("Invalid data", style = "color: red;")),
        
        
        footer = tagList(
            modalButton("Cancel"),
            actionButton("loadModalFileDomain", "열기"),
            actionButton("saveModalFileDomain", "저장")
            
        ),
        size="m"
    )
}

ModalActionButtonsAreaTreat <- function(failed = FALSE) {
  modalDialog(
    title="범용 리포트 종류 선택",
    tags$p("영역 처리 버튼을 누르세요"),
    
    if (failed)
      div(tags$b("Invalid data", style = "color: red;")),
    
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("resetReq", "영역선 변경"),
      actionButton("saveArea", "남기기"),
      actionButton("deleteArea", "버리기"),
      actionButton("bisectArea", "양분화")
    ),
    size="l"
  )
}
