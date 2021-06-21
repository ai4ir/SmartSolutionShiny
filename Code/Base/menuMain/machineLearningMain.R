machineLearningMainUI <- function() {

    tabsetPanel( id="machineLearing", type="tabs",
                 tabPanel("cTree", value="cTree",
                          cTreeTabModuleUI("cTree")
                 ),
                 tabPanel("ranger", value="ranger",
                          rangerTabModuleUI("ranger")
                 ),
                 tabPanel("randomForest", value="randomForest",
                          randomForestTabModuleUI("randomForest")
                 ),
                 tabPanel("linearReg", value="linearReg",
                          linearRegTabModuleUI("linearReg")
                 ),
                 tabPanel("logisticReg", value="logisticReg",
                          logisticRegTabModuleUI("logisticReg")
                 ),
                 tabPanel("clustering", value="clustering",
                          clusteringTabModuleUI("kmeans")
                 ),
                 tabPanel("cTreeTotal", value="cTreeTotal",
                          cTreeTotalTabModuleUI("cTreeTotal")
                 ), 
                 tabPanel("주성분 분석", value="prComp",
                          prCompTabModuleUI("prComp")
                 )

    )

}


machineLearningMain <- function(input, output, session) {
    
    observeEvent(input$explore, {
        curTabMachineLearning <<- input$machineLearning
        # triggerMCP  <<- "module"   ### or "modal"
    })

    
    callModule(cTreeTabModule,"cTree")
    callModule(rangerTabModule,"ranger")
    callModule(randomForestTabModule,"randomForest")
    callModule(linearRegTabModule,"linearReg")
    callModule(logisticRegTabModule,"logisticReg")
    callModule(clusteringTabModule,"kmeans")
    callModule(cTreeTotalTabModule,"cTreeTotal")
    callModule(prCompTabModule,"prComp")
}


