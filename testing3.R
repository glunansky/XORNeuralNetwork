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




Forward <- function(){
  HiddenNodes <<- colSums(Input[[1]] * Weights1)
  HiddenResults <<- Sigmoid(HiddenNodes)
  OutputInit <<- sum(HiddenResults * Weights2)
  OutputResult <<- Sigmoid(OutputInit)
}


Backward <- function(lrate){
  
OutputSum <<- Target - OutputResult

DeltaOutputSum <<- SigmoidDer(OutputInit) * OutputSum

DeltaWeightsOut <<-DeltaOutputSum * HiddenResults *lrate

DeltaWeightsIn <<- Weights2 + DeltaWeightsOut

DeltaHiddenSum  <<- DeltaOutputSum * Weights2 * SigmoidDer(HiddenNodes) *lrate

DeltaWeights  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)


DeltaWeights2 <<- Weights1 + DeltaWeights
}






XORTraining <- function(niter, nhidden, lrate){
  
for(i in 1:niter){
  SampleXOR()
  
  
  if(i == 1){

    Weights1 <<- matrix(runif(nhidden,0,1),2,nhidden)            
    
    
    Weights2 <<- matrix(runif(nhidden,0,1),1,nhidden)
    
    Forward()
    
    Backward(lrate=lrate)
    
    counter<-i
    
    plot(input$niters,OutputSum, xlim=c(0,input$niters), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
    
  } else {
    
    
    Weights1 <<- DeltaWeights2
    
    Weights2 <<- DeltaWeightsIn
    
    Forward()
    
    Backward(lrate=lrate)
    
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
}

    



counter<-i

plot(input$niters,OutputSum, xlim=c(0,input$niters), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")


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
