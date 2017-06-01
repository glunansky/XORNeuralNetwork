###################################
## Programming XOR function      ##
## Version 3                     ##  
## Gaby Lunansky, 21-05-2017     ##
###################################

## Program a Neural Network that gives the XOR function:
## 1,1 --> 0
## 0,1 --> 1
## 1,0 --> 1
## 0,0 --> 0
 

## VERSIE 1
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

BackwardIteration <- function() {
  OutputSum <<- Target - OutputResult
  DeltaOutputSum <<- SigmoidFunction(OutputSum, deriv = TRUE) * OutputSum
  DeltaWeightsOut <<- DeltaOutputSum * HiddenResults
  WeightsNew <<- matrix(NA, 3,3)
  colnames(WeightsNew) <<- c("Hidden1", "Hidden2", "Hidden3")
  rownames(WeightsNew) <<- c("InputNode1", "InputNode2", "OutputNode")
  WeightsNew[3,]  <<- HiddenNodes- DeltaWeightsOut
  DeltaHiddenSum  <<- DeltaOutputSum * HiddenNodes * SigmoidFunction(Weights[3,])
  DeltaWeightsIn  <<- c(Input[[1]][1] * DeltaHiddenSum, Input[[1]][2] * DeltaHiddenSum)
  WeightsNew[1,1] <<- Weights[1,1] - DeltaWeightsIn[1]
  WeightsNew[1,2] <<- Weights[1,2] - DeltaWeightsIn[2]
  WeightsNew[1,3] <<- Weights[1,3] - DeltaWeightsIn[3]
  WeightsNew[2,1] <<- Weights[2,1] - DeltaWeightsIn[4]
  WeightsNew[2,2] <<- Weights[2.2] - DeltaWeightsIn[5]
  WeightsNew[2,3] <<- Weights[2,3] - DeltaWeightsIn[6]
}

XORNetwork <- function(){
  SampleXOR()
      for (i in 1:100){
        if (i == 1){
            Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
            ForwardIteration()
            BackwardIteration()
            print(OutputResult)
          } else if (i==100) {
            Weights <<- WeightsNew
            ForwardIteration()
            BackwardIteration()
            ForwardIteration()
            print(OutputResult)
          } else {
              if (Target != OutputResult){
                Weights <<- WeightsNew
                ForwardIteration()
                BackwardIteration()
                print(OutputResult)
              } else{
            stop()
              }
            } 
      }   
  print(OutputResult)
  print(Target)
  print(Weights)
}

################### DEBUGGING ####################################
## checking if the problem is with certain input node
## 1,1 --> 0
## 0,1 --> 1
## 1,0 --> 1
## 0,0 --> 0

Input<-list(c(1,1))  #works at 100, does not work at 1000, does not work at 10 000, does not work at 100 000
Target<- 0 

Input<- list(c(0,1)) # works at 100, does not work at 1000, does not work at 10 000, does not work at 100 000
Target <- 1

Input <- list(c(1,0)) # works at 100, does not work at 1000, does not work at 10 000, does not work at 100 000
Target <- 1

Input<- list(c(0,0)) # works at 100, works at 1000, works at 10 000, works at 100 000 
Target <- 0 

      
        
#################################################################################
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



XORNetwork <- function(niter=100){
  SampleXOR()
  for (i in 1:niter){
    if (i == 1){
      Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
      ForwardIteration()
      BackwardIteration()
      print(OutputResult)
      
      c<-i
      graph <- plot(c, OutputSum, xlim=c(0,68000), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
      
    } else if (Target != OutputResult){
      Weights <<- WeightsNew
      ForwardIteration()
      BackwardIteration()
      c <- i+1
      points(c, OutputSum)
      
      cat(i, " ", OutputResult, ' \n')
    } else{
      break
    }
  }   
  print(Target)
  print(Weights)
}   


########################## NU NOG ALLE NODES INEEN

XORTraining <- function(niter=100){
  for (i in 1:niter){
    SampleXOR()
    if (i == 1){
      Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
      ForwardIteration()
      BackwardIteration()
      print(OutputResult)
    } else if (Target != OutputResult){
      Weights <<- WeightsNew
      ForwardIteration()
      BackwardIteration()
      print(Input)
      print(Target)
      print(Weights)
      cat(i, " ", OutputResult, " \n")
    } else{
      break
    }
  }   
}   

## Functie wanneer netwerk al getraind is 
XOR <- function(){
  SampleXOR()
  HiddenNodes <- colSums(Input[[1]] * Weights[c(1,2),])
  HiddenResults <- SigmoidFunction(HiddenNodes)
  OutputInit <- sum(HiddenResults * Weights[3,])
  OutputResult <- SigmoidFunction(OutputInit)
  
  cat("The input nodes are", Input[[1]], " \n")
  cat("The target node is", Target, " \n")
  cat("The result is", OutputResult, "\n")
}


## spelen met learning rate

BackwardIteration <- function() {
  OutputSum <<- Target - OutputResult
  DeltaOutputSum <<- SigmoidFunction(OutputInit, deriv = TRUE) * OutputSum
  DeltaWeightsOut <<-DeltaOutputSum * HiddenResults * .05
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


#### plotten



XORNetwork <- function(niter){
  for (i in 1:niter){
    if (i == 1){
      SampleXOR()
      Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
      ForwardIteration()
      BackwardIteration()
      print(OutputResult)
      c<-i
      graph <- plot(c, OutputResult, xlim=c(0,niter), ylim=c(-1,1), main="Output Result XOR Neural Network", type="b", xlab="Number of iterations", ylab="Output")
    } else {
      SampleXOR()
      Weights <<- WeightsNew
      ForwardIteration()
      BackwardIteration()
      cat(i, " ", OutputResult, ' \n')
      c <- i+1
      points(c, OutputResult, type="b")
  
    }
  }   
  print(Target)
  print(Weights)
}   

?plot




### error plotten


XORError <- function(niter=100){
  for (i in 1:niter){
    SampleXOR()
    if (i == 1){
      Weights <<- matrix(runif(6,0,1),3,3)  # In the first iteration, we sample the edge weights
      ForwardIteration()
      BackwardIteration()
      print(OutputResult)
      c<-i
      graph <- plot(c, OutputSum, xlim=c(0,niter), ylim=c(-1,1), main="Error XOR Neural Network", xlab="Number of iterations", ylab="Error")
    } else {
      Weights <<- WeightsNew
      ForwardIteration()
      BackwardIteration()
      cat(i, " ", OutputResult, ' \n')
      c <- i
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
    } 
  }   
}   



