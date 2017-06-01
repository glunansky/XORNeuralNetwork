SampleXOR <- function(){
  # With this function, we sample one of the four XOR patterns
  #@param: InputInit is a list with four possible input nodes
  #@param: Sample one of the four possible patterns
  #* The loop gives the corresponding Target value to each input combination
  InputInit <- list(c(1,1), c(1,0), c(0,1), c(0,0))    
  Input <<- sample(InputInit,1)
  if (Input[[1]][1] == 1 & Input[[1]][2] == 1){
    Target <<- 0
  } else if (Input[[1]][1] == 1 & Input[[1]][2] == 0){
    Target <<- 1
  } else if (Input[[1]][1] == 0 & Input[[1]][2] == 1){
    Target <<- 1
  } else {
    Target <<- 0
  }
}


Sigmoid <-function(x)
{
  1/(1 + exp(-x))
}

SigmoidDer <- function(x)
{
  exp(x)/((exp(x) + 1)^2)
}







ui <- fluidPage(
  titlePanel("The XOR Neural Network"),
  
  tabsetPanel(
    
    tabPanel("Training",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 
                 sliderInput(inputId = "niters", label = "Number of iterations",
                             min = 0, max = 100000, step = 1000, value = 10000),
                 
                 
                 numericInput(inputId = "nhidden", label = "Number of Hidden Nodes", 
                              value = 3, min=1, max=10, step=1),
                 
                 sliderInput(inputId = "learningr", label = "Set learning rate",
                             min=0, max=.7, step=0.05, value=.2),
                 
                 
                 actionButton(inputId = "btrain", label = "TRAIN"),
                 
                 actionButton(inputId = "bsave", label = "Save this Neural Network")
               ),
               
               mainPanel(
                 
                 plotOutput("errorplot")
                 
               )
             )
    ),
    
    tabPanel("Testing",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 numericInput(inputId = "node1", label = "Input Node 1", value = 0, min = 0, max = 1, step = 1),
                 
                 numericInput(inputId = "node2", label = "Input Node 2", value = 0, min = 0, max = 1, step = 1),
                 
                 actionButton(inputId = "btest", label = "TEST")
                 
               ),
               
               mainPanel(
                 textOutput("testtext1"),
                 textOutput("testtext2"),
                 textOutput("testtext3"),
                 textOutput("testtext4"),
                 textOutput("testtext5")
                 
               )
             )
    ),
    
    tabPanel("The XOR Neural Network"
             
             
    )
    
  )
  
)






server <- function(input, output) {
  
  eventReactive(input$btrain, {
    
    output$errorplot <- renderPlot({
      
      for(i in 1:input$niters){
        SampleXOR()
        
        
        if(i == 1){
          
          #ForwardPropagation
          Weights1 <<- matrix(runif(input$nhidden,0,1),2,input$nhidden)            
          
          HiddenNodes <<- colSums(Input[[1]] * Weights1)
          
          HiddenResults <<- Sigmoid(HiddenNodes)
          Weights2 <<- matrix(runif(input$nhidden,0,1),1,input$nhidden)
          OutputInit <<- sum(HiddenResults * Weights2)
          OutputResult <<- Sigmoid(OutputInit)
          
          #Backpropagation
          
          
          OutputSum <<- Target - OutputResult
          
          DeltaOutputSum <<- SigmoidDer(OutputInit) * OutputSum
          
          DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *input$learningr
          
          DeltaWeightsIn <<- Weights2 + DeltaWeightsOut
          
          DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *input$learningr
          
          DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
          
          
          DeltaWeights2 <<- Weights1 + DeltaWeights
          
          counter<-i
          
          plot(input$niters,OutputSum, xlim=c(0,input$niters), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
        
          
         
        } else{
          
          
          Weights1 <<- DeltaWeights2
          
          Weights2 <<- DeltaWeightsIn
          
          HiddenNodes <<- colSums(Input[[1]] * Weights1)
          
          HiddenResults <<- Sigmoid(HiddenNodes)
          OutputInit <<- sum(HiddenResults * Weights2)
          OutputResult <<- Sigmoid(OutputInit)
          
          #Backpropagation
          
          
          OutputSum <<- Target - OutputResult
          
          DeltaOutputSum <<- SigmoidDer(OutputInit) * OutputSum
          
          DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *input$learningr
          
          DeltaWeightsIn <<- Weights2 + DeltaWeightsOut
          
          DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *input$learningr
          
          DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
          
          DeltaWeights2 <<- Weights1 + DeltaWeights
          
          counter<-i+1
          
          if (Input[[1]][1] == 1 & Input[[1]][2] == 1){
            points(counter, OutputSum, col="red")
          } else if (Input[[1]][1] == 1 & Input[[1]][2] == 0){
            points(counter, OutputSum, col="blue")
          } else if (Input[[1]][1] == 0 & Input[[1]][2] == 1){
            points(counter, OutputSum, col="pink")
          } else {
            points(counter, OutputSum, col="green")
          }
          
        }
      }
      
    })
  })
  
  observeEvent(input$bsave, {
    SavedWeights1 <<- DeltaWeights2
    SavedWeights2 <<- DeltaWeightsIn
    iterations <<- input$niters
    hiddennodes <<- input$nhidden
    learningrate <<- input$learningr
  })
  
  observeEvent(input$btest, {
    
    
    if (input$node1 == 1 & input$node2 == 1){
      Target <<- 0
    } else if (input$node1 == 1 & input$node2 == 0){
      Target <<- 1
    } else if (input$node1 == 0 & input$node2 == 1){
      Target <<- 1
    } else {
      Target <<- 0
    }
    
    Weights1 <<- SavedWeights1          
    HiddenNodes <<- colSums(c(input$node1, input$node2) * Weights1)
    HiddenResults <<- Sigmoid(HiddenNodes)
    Weights2 <<- SavedWeights2
    OutputInit <<- sum(HiddenResults * Weights2)
    OutputResult <<- round(Sigmoid(OutputInit),4)
    OutputSum <<- Target - OutputResult
    
    output$testtext1 <- renderText({
      paste("For this network with input values", input$node1, "and", input$node2,", and with", iterations, "iterations", "\n")
    })
    output$testtext2 <- renderText({
      paste(hiddennodes, "hidden nodes and a learning rate of", learningrate, "\n")
    })
    
    output$testtext3 <- renderText({
      paste("The result of the output node is", OutputResult, "\n")
    })
    
    output$testtext4 <- renderText({
      paste("The target was", Target, "\n")
    })
    
    output$testtext5 <- renderText({
      paste("The error is", OutputSum, "\n")
      
    })
  })
}  




shinyApp(ui = ui, server = server)
