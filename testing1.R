
###################################
## Programming XOR function      ##
## Shiny app                     ##  
## Gaby Lunansky, 21-05-2017     ##
###################################


library("shiny")






## Call XOR function
## Source code for training the XOR Neural Network

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


ErrorGraph <- plot(counter, OutputSum, xlim=c(0,niter), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")

ErrorGraph <- plot(counter, OutputSum, xlim=c(0,100), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")



{
  if (Input[[1]][1] == 1 & Input[[1]][2] == 1){
    points(c, OutputSum, col="red")
  } else if (Input[[1]][1] == 1 & Input[[1]][2] == 0){
    points(c, OutputSum, col="blue")
  } else if (Input[[1]][1] == 0 & Input[[1]][2] == 1){
    points(c, OutputSum, col="pink")
  } else {
    points(c, OutputSum, col="green")
  }
  
}


XORTraining <- function(niter, nhidden, lr){
  
  for(i in 1:niter){
    SampleXOR()
    counter <- i
    
    if(i == 1){
      
      #ForwardPropagation
      Weights1 <<- matrix(runif(nhidden,0,1),2,nhidden)            
      
      HiddenNodes <<- colSums(Input[[1]] * Weights1)
      
      HiddenResults <<- Sigmoid(HiddenNodes)
      Weights2 <<- matrix(runif(nhidden,0,1),1,nhidden)
      OutputInit <<- sum(HiddenResults * Weights2)
      OutputResult <<- Sigmoid(OutputInit)
      
      #Backpropagation
      
      
      OutputSum <<- Target - OutputResult
      
      DeltaOutputSum <<- SigmoidDer(OutputInit) * OutputSum
      
      DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *lr
      
      DeltaWeightsIn <<- Weights2 + DeltaWeightsOut
      
      DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *lr
      
      DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
      
      
      DeltaWeights2 <<- Weights1 + DeltaWeights
      
      plot(counter, OutputSum, xlim=c(0,niter), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
      
      

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
      
      DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *lr
      
      DeltaWeightsIn <<- Weights2 + DeltaWeightsOut
      
      DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *lr
      
      DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
      
      DeltaWeights2 <<- Weights1 + DeltaWeights
    }
    
    {
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
}  





## Shiny app

ui <- fluidPage(
  titlePanel("The XOR Neural Network"),
  
  tabsetPanel(
    
    tabPanel("Training",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 
                 sliderInput(inputId = "niters", label = "Number of iterations",
                             min = 0, max = 1000000, step = 1000, value = 10000),
                 
                 
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
             
             radioButtons(inputId = "node1", label = "Input Node 1", choices = c(0,1), inline = TRUE),
             
             radioButtons(inputId = "node2", label = "Input Node 2", choices = c(0,1), inline = TRUE)
             
    ),
    
    tabPanel("The XOR Neural Network"
             
             
    )
    
  )
  
)






server <- function(input, output) {
  observeEvent(input$btrain, {
    
    XORTraining(niter=input$niters, nhidden=input$nhidden, lr=input$learningr)
    })
    
    output$errorplot <- plot(x=1:10, y=11:20)
}    
    



shinyApp(ui = ui, server = server)






#
ErrorGraph <- plot(counter, OutputSum, xlim=c(0,niter), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")

ErrorGraph <- plot(counter, OutputSum, xlim=c(0,100), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")



{
  if (Input[[1]][1] == 1 & Input[[1]][2] == 1){
    points(c, OutputSum, col="red")
  } else if (Input[[1]][1] == 1 & Input[[1]][2] == 0){
    points(c, OutputSum, col="blue")
  } else if (Input[[1]][1] == 0 & Input[[1]][2] == 1){
    points(c, OutputSum, col="pink")
  } else {
    points(c, OutputSum, col="green")
  }
  
}



