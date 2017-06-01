
###################################
## Programming XOR function      ##
##                               ##  
## Gaby Lunansky, may 2017       ##
###################################

## Program a Neural Network that gives the XOR function:
## 1,1 --> 0
## 0,1 --> 1
## 1,0 --> 1
## 0,0 --> 0


library("shiny")


###################################
# First, we run the source codes  #
###################################

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
  # Function for the activation function of the NN
{
  1/(1 + exp(-x))
}

SigmoidDer <- function(x)
  # Take the derivative of the Sigmoid activation function
{
  exp(x)/((exp(x) + 1)^2)
}


Forward <- function(){
  # Function that is used within the XORTraining function
  # Forward propagation function
  HiddenNodes <<- colSums(Input[[1]] * Weights1)
  HiddenResults <<- Sigmoid(HiddenNodes)
  OutputInit <<- sum(HiddenResults * Weights2)
  OutputResult <<- Sigmoid(OutputInit)
}


Backward <- function(lrate){
  # Function that is used within the XORTraining function
  # Backward propagation function
  # Uses interactive learning rate (when implemented in Shiny app)
  
  OutputSum <<- Target - OutputResult
  
  DeltaOutputSum <<- SigmoidDer(OutputInit) * OutputSum
  
  DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *lrate
  
  DeltaWeightsIn <<- Weights2 + DeltaWeightsOut
  
  DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *lrate
  
  DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
  
  DeltaWeights2 <<- Weights1 + DeltaWeights
}




XORTraining <- function(niter, nhidden, lrate){
  #* parameter niter = number of iterations
  #* parameter nhidden = number of hidden nodes
  #* parameter lrate = learning rate
  
  # We start with creating a global hidden matrix, in where the relevant values will be stored for every iteration
  plotdata <<- matrix(NA, 4, niter) 
  rownames(plotdata) <<- c("Iteration", "Error", "Input Node 1", "Input Node 2")
  
  for(i in 1:niter){
    # We sample a different combination of input nodes & corresponding Target value for every iteration
    # In this way, the NN tries to find optimal weights for every combination of input nodes
    
    SampleXOR()
    counter <- i     # Counter for the iterations
    
    if(i == 1){      # In the first iteration, we have to sample the edge weights
      
      Weights1 <<- matrix(runif(nhidden,0,1),2,nhidden)            
      
      
      Weights2 <<- matrix(runif(nhidden,0,1),1,nhidden)
      
      Forward()
      
      Backward(lrate=lrate)
      
      # We save the relevant values in our global hidden plotdata matrix
      plotdata[1,i] <<- counter
      plotdata[2,i] <<- round(OutputSum,2)
      plotdata[3,i] <<- Input[[1]][1] 
      plotdata[4,i] <<- Input[[1]][2]
      
      
    } else {   # After the first iteration, we update our weight matrix with every iteration of the loop, thus do not sample the values anymore
      
      
      Weights1 <<- DeltaWeights2    # We update the weightmatrix with the found results of the former iteration
      
      Weights2 <<- DeltaWeightsIn
      
      Forward()
      
      Backward(lrate=lrate)
      
      # We save the relevant values in our global hidden plotdata matrix
      plotdata[1,i] <<- counter   
      plotdata[2,i] <<- round(OutputSum,2)
      plotdata[3,i] <<- Input[[1]][1] 
      plotdata[4,i] <<- Input[[1]][2]
      
      }
  }
}

####################################
##    Code for running in Shiny   ##
####################################

ui <- fluidPage(
  titlePanel("The XOR Neural Network"),
  
  tabsetPanel(
    
    tabPanel("Training",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 # This reactive slider gives the number of iterations
                 sliderInput(inputId = "niters", label = "Number of iterations",
                             min = 10000, max = 100000, step = 1000, value = 10000),
                 
                 # This reactive numeric input bar gives the number of nodes
                 numericInput(inputId = "nhidden", label = "Number of Hidden Nodes", 
                              value = 3, min=1, max=10, step=1),
                 
                 # This reactive slider gives the value of the learning rate
                 sliderInput(inputId = "learningr", label = "Set learning rate",
                             min=0, max=.7, step=0.05, value=.2),
                 
                 # With this button, the user can save the trained network
                 actionButton(inputId = "bsave", label = "Save this Neural Network")
               ),
               
               mainPanel(
                 
                 # The graph that gives the error for every combination of input nodes during the testing phase
                 plotOutput("errorplot")
                 
               )
             )
    ),
    
    tabPanel("Testing",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 
                # In the testing tab, the user can choose the input values for both input nodes
                 numericInput(inputId = "node1", label = "Input Node 1", value = 0, min = 0, max = 1, step = 1),
                 
                 numericInput(inputId = "node2", label = "Input Node 2", value = 0, min = 0, max = 1, step = 1),
                 
                 # With this button, the user can test the trained network for the chosen input nodes
                 actionButton(inputId = "btest", label = "TEST")
                 
               ),
               
               mainPanel(
                 
                # This text will give a summary of the tested network
                 h4(htmlOutput("testtext1")),
                 h4(htmlOutput("testtext2")),
                 h4(htmlOutput("testtext3")),
                 h4(htmlOutput("testtext4")),
                 h4(htmlOutput("testtext5")),
                 
                 # This text will give a conditional conclusion of the correctness of the trained NN
                 h3(htmlOutput("final"))
                 
                )
             )
    ),
    
    tabPanel("The XOR Neural Network",
             
                  # This tab will give some general information on the XOR NN 
    
                  p("This app aims for its users to play around with a simple neural network - the XOR neural network, in order to grasp the purpose and functionality of Neural Networks (NNs)."),
                  
                  p("The XOR network in this app tries to find the combination of weights that can solve the following problem:"),
                    strong("Input nodes 1 & 1 --> give output node result 0"),
                    br(),
                    strong("Input nodes 0 & 0 --> give output node result 0"),
                    br(),
                    strong("Input nodes 1 & 0 --> give output node result 1"),
                    br(),
                    strong("Input nodes 0 & 1 --> give output node result 1"),
                    
             br(),
             br(),
                  p("In the 'Training Tab', the user can change the number of iterations for the training phase, the number of hidden nodes, and the value of the learning rate. The graph shows the error of each combination of input nodes over the iterations. The error indicates the difference between the current output node value, and its target (which is a 0 or a 1). When all four lines are close to 0, the NN found optimal weights. To test these weights, the user clicks the button "Save this Neural Network""),
                  p("Once saved, the user can test the NN in the 'Test Tab'. The different combinations of input nodes can be selected in the left panel. By clicking the 'TEST' button, the NN goes through the Forward Propagation function with the saved weight matrix. In the left panel, a summary of the results is shown, providing a conclusion regarding the correctness of the trained network for the selected input nodes. If the absolute value of the error (difference between the value of the output node and the target) is lower than .3, it is concluded that the network is well-trained. 
                    "),
             br(),
             br(),
             br(),
             p("By Gaby Lunansky")
             
             )
  )
)
  





server <- function(input, output){
     
       output$errorplot <- renderPlot({
         
         # Since the plot is responsive, the input of the XORTraining function depends on the values chosen by the user
         
        XORTraining(niter=input$niters, nhidden=input$nhidden, lrate = input$learningr)
        
         plot(NULL,xlim=c(0,input$niters), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
         legend("topright", fill=c("cyan3", "orchid4", "blue", "palevioletred"), legend = c("Nodes 1 & 1", "Nodes 0 & 0", "Nodes 1 & 0", "Nodes 0 & 1"))
         
         
         # With these conditional statements, we give a different colour for every possible combination of nodes
         # We know the value for each combination since it is stored in our "plotdata" matrix, after running the XORTraining() function
         for(i in 1:input$niters){
         
         if (plotdata[3,i] == 1 & plotdata[4,i] == 1){
           points(plotdata[1,i], plotdata[2,i], col="cyan3")
         } else if (plotdata[3,i] == 1 & plotdata[4,i]  == 0){
           points(plotdata[1,i], plotdata[2,i], col="blue")
         } else if (plotdata[3,i] == 0 & plotdata[4,i] == 1){
           points(plotdata[1,i], plotdata[2,i], col="palevioletred")
         } else {
           points(plotdata[1,i], plotdata[2,i], col="orchid4")
         }
       }
    })
       # If the user clicks the "save" button, we save the values of the current parameters
       # We change them from responsive objects into stored objects for the test phase 
       observeEvent(input$bsave, {
         SavedWeights1 <<- DeltaWeights2
         SavedWeights2 <<- DeltaWeightsIn
         iterations <<- input$niters
         hiddennodes <<- input$nhidden
         learningrate <<- input$learningr
       })
       
       observeEvent(input$btest, {
         
         # When the user clicks the "save" button, the network will be tested with the saved values from the training phase
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
         
         
         # This text gives a summary of the results and parameter values
         output$testtext1 <- renderUI({
           paste("For this network with input values", input$node1,"and", input$node2, " and ", iterations, "iterations", "\n")
         })
         output$testtext2 <- renderUI({
           paste(hiddennodes, "hidden nodes and a learning rate of", learningrate, "\n")
         })
         
         output$testtext3 <- renderUI({
          paste("The result of the output node is", OutputResult, "\n")
         })
         
         output$testtext4 <- renderUI({
           paste("The target was", Target, "\n")
         })
         
         output$testtext5 <- renderUI({
           paste("The error is", OutputSum, "\n")
         })
         
         # This text gives a conclusion on the corectness of the trained network
         # If the absolute value of the error is < .3, it concludes that the network is well trained for the chosen combination of input nodes
         output$final <- renderUI({
           if(abs(OutputSum) < .3){
             strong(paste("Nice! The network is well-trained for this combination of input nodes!"))
           } else{
             strong(paste("Too bad! The network is poorly trained for this combination of input nodes"))
           }
           
         })
      })
}
       

shinyApp(ui = ui, server = server)

