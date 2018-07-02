require(datasets)  # source package
str(iris) 
head(iris)


#      w1
# X1 > w2 b1 > w5
#                 b3 > Sigmoid() > Y
# X2 > w3 b2 > w6
#      w4
learning_rate <- 0.1

w1=0
w2=0
w3=0
w4=0
w5=0
w6=0
w7=0
w8=0
w9=0
w10=0
b1=0
b2=0
b3=0

#Sigmoid
Sigmoid <- function(x){
  1/(1+exp(-x))
}
#The Neural 2*2*1
NeuralTrain<- function(X1,X2,Y1,Y2,answer){
  #input layer -> hidden layer
  #n1 = Sigmoid( w1*X1 + w2*X2 + b1 );
  n1 <- X1*w1+X2*w2+Y1*w3+Y2*w4+b1
  h1 <- Sigmoid(n1)
  #n2 = Sigmoid( w3*X1 + w4*X2 + b2 );
  n2 <- X1*w5+X2*w6+Y1*w7+Y2*w8+b2
  h2 <- Sigmoid(n2)
  #hidden layer -> output layer
  #output = Sigmoid( w5*n1 + w6*n2 + b3 );
  n3 <- h1*w9+h2*w10+b3
  h3 <- Sigmoid(n3)
  #backward path
  #output layer->hidden layer
  #Use eval.parent(substitute(Values)) update variable to Global Environment
  #learning_rate*(answer-output)*output*(1-output)*h1
  eval.parent(substitute(w9 <- w9+ learning_rate * (answer - h3)*h3*(1-h3)*h1)) 
  eval.parent(substitute(w10 <- w10+learning_rate * (answer - h3)*h3*(1-h3)*h2))
  eval.parent(substitute(b3 <- b3+ learning_rate * (answer - h3)*h3*(1-h3)*1))
  #hidden layer->input layer
  eval.parent(substitute(w1 <- w1 + learning_rate * (answer - h3)*h3*(1-h3)*w9*(h1)*(1-h1)*X1))
  eval.parent(substitute(w2 <- w2 + learning_rate * (answer - h3)*h3*(1-h3)*w9*(h1)*(1-h1)*X2))
  eval.parent(substitute(w3 <- w3 + learning_rate * (answer - h3)*h3*(1-h3)*w9*(h1)*(1-h1)*Y1))
  eval.parent(substitute(w4 <- w4 + learning_rate * (answer - h3)*h3*(1-h3)*w9*(h1)*(1-h1)*Y2))
  eval.parent(substitute(b1 <- b1 + learning_rate * (answer - h3)*h3*(1-h3)*w9*(h1)*(1-h1)*1))
  
  eval.parent(substitute(w5 <- w5 + learning_rate * (answer - h3)*h3*(1-h3)*w10*(h2)*(1-h2)*X1))
  eval.parent(substitute(w6 <- w6 + learning_rate * (answer - h3)*h3*(1-h3)*w10*(h2)*(1-h2)*X2))
  eval.parent(substitute(w7 <- w7 + learning_rate * (answer - h3)*h3*(1-h3)*w10*(h2)*(1-h2)*Y1))
  eval.parent(substitute(w8 <- w8 + learning_rate * (answer - h3)*h3*(1-h3)*w10*(h2)*(1-h2)*Y2))
  
  eval.parent(substitute(b2 <- b2 + learning_rate * (answer - h3)*h3*(1-h3)*w10*(h2)*(1-h2)*1))
  
  return (answer - h3)
}
Neural <- function(X1,X2,Y1,Y2){
  n1 <- X1*w1+X2*w2+Y1*w3+Y2*w4+b1
  h1 <- Sigmoid(n1)
  n2 <- X1*w5+X2*w6+Y1*w7+Y2*w8+b2
  h2 <- Sigmoid(n2)
  #hidden layer -> output layer
  #output = Sigmoid( w5*n1 + w6*n2 + b3 );
  n3 <- h1*w9+h2*w10+b3
  output <- Sigmoid(n3)
  return(output)
}

learnSetosa <- function(Species){
  x <- ifelse(Species == "setosa" ,1,0)
  return(x)
}

learnVersicolor <- function(Species){
  x <- ifelse(Species == "versicolor" ,1,0)
  return(x)
}
learnVirginica <- function(Species){
  x <- ifelse(Species == "virginica" ,1,0)
  return(x)
}

#init
w1 =runif(1)
w2 =runif(1)
w3 =runif(1)
w4 =runif(1)
w5 =runif(1)
w6 =runif(1)
w7 =runif(1)
w8 =runif(1)
b1 =runif(1)
b2 =runif(1)


# learn versicolor
# this will converge
set.seed(10)
versicolor_module <- c(1:30000)
for(i in c(1:30000)){
  num <- sample(100, 1,replace = TRUE)
  output <- NeuralTrain(iris[51:150,]$Sepal.Length[num]
                        ,iris[51:150,]$Sepal.Width[num]
                        ,iris[51:150,]$Petal.Length[num]
                        ,iris[51:150,]$Petal.Width[num]
                        ,learnVersicolor(iris[51:150,]$Species[num])
  )
  versicolor_module[i]<-output
  print(output)
}
plot(versicolor_module)

# learn setosa
# this will converge
set.seed(1)
setosa_module <- c(1:30000)
for(i in c(1:30000)){
  num <- sample(100, 1,replace = TRUE)
  output <- NeuralTrain(iris$Sepal.Length[num]
                        ,iris$Sepal.Width[num]
                        ,iris$Petal.Length[num]
                        ,iris$Petal.Width[num]
                        ,learnSetosa(iris$Species[num])
  )
  setosa_module[i]<-output
  print(output)
}
plot(setosa_module,type = "l")
# learn virginica
# this will converge
set.seed(100)
virginica_module <- c(1:30000)
for(i in c(1:30000)){
  num <- sample(100, 1,replace = TRUE)
  output <- NeuralTrain(iris[51:150,]$Sepal.Length[num]
                        ,iris[51:150,]$Sepal.Width[num]
                        ,iris[51:150,]$Petal.Length[num]
                        ,iris[51:150,]$Petal.Width[num]
                        ,learnVirginica(iris[51:150,]$Species[num])
  )
  virginica_module[i]<-output
  print(output)
}
plot(virginica_module,type = "l")

data2 <- c(1:150)
for(i in c(1:150)){
  output <- Neural(iris[1:150,]$Sepal.Length[i]
                        ,iris[1:150,]$Sepal.Width[i]
                        ,iris[1:150,]$Petal.Length[i]
                        ,iris[1:150,]$Petal.Width[i]
                   )
  data2[i]<- output
    print(output)
}
plot(data2)

