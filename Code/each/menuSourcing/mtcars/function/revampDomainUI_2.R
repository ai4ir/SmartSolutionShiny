### menuSourcing/mtcars/function ###

revampDomainUI_2 <- function(input,output, session) {
    
    # 범주형 변수의 범주 선정

    insertUI(
        selector="#catVarContainerExplore",
        where = "afterEnd",
        ui = tagList(
            fluidRow(title="catVarContentExplore",
                     column(4,
                            # p("채취 위치 :"),
                            checkboxGroupInput("am", label="변속기 :",
                                               c("ALL" = "ALL",
                                                 "자동" = "0",
                                                 "수동" = "1"), selected="ALL"),
                            checkboxGroupInput("vs", label="엔진 모양 :",
                                               c("ALL" = "ALL",
                                                 "V 형" = "0",
                                                 "일자형" = "1"), selected="ALL")
                     ),
                     column(4,
                            actionButton("selCatExplore1", "Gear 범주 선정"),
                            tags$p(""),
                            actionButton("selCatExplore2", "Carb 범주 선정"),
                            tags$p("")
                            # actionButton("selCatExplore3", "고객사 선정"),
                            # tags$p(""),
                            # actionButton("selCatExplore4", "코일 선정")
                            # tags$p(""),
                            # actionButton("selCatExplore4", "열연공장 선정"),
                            # tags$p(""),
                            # actionButton("selCatExplore5", "HCR/CCR 선정"),
                            # tags$p(""),
                            # actionButton("selCatExplore6", "슬라브 타입 선정"),
                     )
            )
        )
    )
}








### EXCEL파일에서 읽은 값을 UI에 표시 ###
updateDomainUIFromEXCEL <- function(input,output, session, namePathFile) {
    # propName <- c("TS_YS","TS_TS","TS_EL","TS_Uel","TS_YR")
    # 
    # for(i in 1:length(propName)) {
    #     var <- propName[i]
    #     inVar <- paste0("minDomainExploreP",i)   # 변수값
    #     value <- as.vector(MinDomainExplore[var])
    #     updateNumericInput(session, inVar, value=value)
    #     inVar <- paste0("maxDomainExploreP",i)   # 변수값
    #     value <- as.vector(MaxDomainExplore[var])
    #     updateNumericInput(session, inVar, value=value)
    # }
    
    chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B")
    
    for(i in 1:length(chemCompName)) {
        var <- chemCompName[i]
        inVar <- paste0("minDomainExploreC",i)   # 변수값
        value <- as.vector(MinDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
        inVar <- paste0("maxDomainExploreC",i)   # 변수값
        value <- as.vector(MaxDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
    }
    
    rollingName <- c("T0","T0time","RDT","FDT","CT","CoilT")
    rollingName <- c("CoilT")  # PipeMill
    
    
    for(i in 1:length(rollingName)) {
        var <- rollingName[i]
        inVar <- paste0("minDomainExploreR",i)   # 변수값
        value <- as.vector(MinDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
        inVar <- paste0("maxDomainExploreR",i)   # 변수값
        value <- as.vector(MaxDomainExplore[var])
        updateNumericInput(session, inVar, value=value)
    }
    
    # for (i in 1:length(numVarExplore)) {
    #     updateNumericInput(session,paste0("min",numVarExplore[i]), value=ReqMinDF[1,numVarExplore[i]])
    #     updateNumericInput(session,paste0("max",numVarExplore[i]), value=ReqMaxDF[1,numVarExplore[i]])
    # }
    # updateSelectInput(session, "anonCustomerExplore",label="고객 선택", choices=as.character(c("ALL",unique(DFSourceExplore[,"anonCustomer"]))),
    #                   selected=as.character(ReqMinDF[1,"anonCustomer"]))
    # 
    # updateSelectInput(session, "GangJongExplore",label="강종 선택", choices=as.character(c("ALL",as.character(unique(DFSourceExplore[,"GangJong"])))),
    #                   selected=as.character(ReqMinDF[1,"GangJong"]))
    # updateSelectInput(session, "FormingProcessExplore",label="조관법", choices=as.character(c("ALL",as.character(unique(DFSourceExplore[,"FormingProcess"])))),
    #                   selected=as.character(ReqMinDF[1,"FormingProcess"]))
}

