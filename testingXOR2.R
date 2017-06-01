# MASTER

## VERSIE 2 ## DEZE KLOPT

## Program a Neural Network that gives the XOR function:
## 1,1 --> 0
## 0,1 --> 1
## 1,0 --> 1
## 0,0 --> 0


SampleXOR <- function(){
  # With this function, we sample one of the four XOR functions
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


SigmoidFunction <- function(x, deriv = FALSE) {
  result <- 1 / (1 + exp(-x))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}

ForwardIteration <- function(){
  # First we calculate the sum of the edge weights from the input nodes to the hidden nodes
  # Then we apply the activation function (Sigmoid Function) to calculate the hidden node values
  # Afterwards, we calculate the sum of the edge weights from hidden nodes to output
  # Lastly, we apply the sigmoid function to this edge sum and calculate the output node value
  HiddenNodes <<- colSums(Input[[1]] * Weights[c(1,2),])
  HiddenResults <<- SigmoidFunction(HiddenNodes)
  OutputInit <<- sum(HiddenResults * Weights[3,])
  OutputResult <<- SigmoidFunction(OutputInit)
}

## DEZE KLOPT
BackwardIteration <- function() {
  OutputSum <<- Target - OutputResult
  DeltaOutputSum <<- SigmoidFunction(OutputInit, deriv = TRUE) * OutputSum
  DeltaWeightsOut <<-DeltaOutputSum * HiddenResults * .2
  WeightsNew <<- matrix(NA, 3,3)
  colnames(WeightsNew) <<- c("Hidden1", "Hidden2", "Hidden3")
  rownames(WeightsNew) <<- c("InputNode1", "InputNode2", "OutputNode")
  WeightsNew[3,]  <<- DeltaWeightsOut + Weights[3,]
  DeltaHiddenSum  <<- DeltaOutputSum * Weights[3,] * SigmoidFunction(HiddenNodes, deriv = TRUE) * .2
  DeltaWeightsIn  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
  WeightsNew[1,1] <<- Weights[1,1] + DeltaWeightsIn[1]
  WeightsNew[1,2] <<- Weights[1,2] + DeltaWeightsIn[2]
  WeightsNew[1,3] <<- Weights[1,3] + DeltaWeightsIn[3]
  WeightsNew[2,1] <<- Weights[2,1] + DeltaWeightsIn[4]
  WeightsNew[2,2] <<- Weights[2.2] + DeltaWeightsIn[5]
  WeightsNew[2,3] <<- Weights[2,3] + DeltaWeightsIn[6]
}



XORNetwork <- function(niter){
 
  for (i in 1:niter){
    SampleXOR()
    
    if (i == 1){
      Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
      ForwardIteration()
      BackwardIteration()
      print(OutputResult)
      
      c<-i
      graph <- plot(c, OutputSum, xlim=c(0,niter), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
      
    } else if (Target != OutputResult){
      Weights <<- WeightsNew
      ForwardIteration()
      BackwardIteration()
      c <- i
      points(c, OutputSum)
      
      cat(i, " ", OutputSum, ' \n')
    } else{
      break
    }
  }   
  print(Target)
  print(Weights)
}   
