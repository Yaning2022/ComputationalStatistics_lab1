
# Question One:

x1<-1/3
x2<-1/4
if(x1-x2==1/12){
  print ( "Subtraction is correct " )
}else{
  print ( " Subtraction is wrong" )
}


x1<-1
x2<-1/2
if(x1-x2==1/2){
  print ( "Subtraction is correct " )
}else{
  print ( " Subtraction is wrong" )
}

# Improvement:

x1<-1/3
x2<-1/4
if(all.equal(x1-x2,1/12)==TRUE){
  print ( "Subtraction is correct " )
}else{
  print ( " Subtraction is wrong" )
}




#Question Two:

derivative<-function(x){
  epsilon<-10^-15
  deri<-(x+epsilon-x)/epsilon
  return(deri)
}


x<-1
derivative(x)
x<-100000
derivative(x)



#Question Three:


myvar<-function(x){
  n<-length(x)
  variance<-1/(n-1)*(sum(x^2)-1/n*(sum(x)^2))
  return(variance)
}

set.seed(123)
x<-rnorm(10000,10^8,1)


Y<-c()
for(i in 1:10000){
  xi<-x[1:i]
  Yi<-myvar(xi)-var(xi)
  Y<-c(Y,Yi)
}
plot(1:10000,Y)




set.seed(123)
x<-rnorm(10000,10^8,1)
Y<-c()
myvar2<-function(x){
  n<-length(x)
  var_new<-sum((x-sum(x)/n)^2)/(n-1)
  return(var_new)
}
for(i in 1:10000){
  xi<-x[1:i]
  Yi<-myvar2(xi)-var(xi)
  Y<-c(Y,Yi)
}
plot(1:10000,Y,ylim = c(-4,4))


#Question 4

function_1 <- function(...){

  for(i in 1:200){

    if(prod(1:i) ==Inf){
      max_n_1 <- i
      return(max_n_1)
    }
  }
}

max_n_1 <- function_1()


function_2 <- function(...){
  for(i in 1:200){
    for(j in 0:i){
      if(prod((j + 1):i) == Inf || prod(1:(i - j)) == Inf){
        max_n_2 <- i
        max_k_2 <- j
        return(list(n_2 = max_n_2,k_2 =max_k_2))
      }
    }
  }
}
result <- function_2()
max_n_2 <- result[[1]]
max_k_2 <- result[[2]]

max_n_3 <- 0
max_k_3 <- 0
function_3 <- function(n){
  for(i in 1:n){
    for(j in 0:i){
        if(prod(((j + 1):i)/(1:(i - j))) == Inf){
        max_n_3 <<- i
        max_k_3 <<- j

      }
    }
  }
}
function_3(200)



n <- c(max_n_1,max_n_2,max_n_3)
k <- c(0,max_k_2,max_k_3)

data_plot1 <- data.frame(formula = c("formula1","formula2","formula3"),n)
data_plot2 <- data.frame(formula = c("formula1","formula2","formula3"),k)

library(ggplot2)

ggplot(data = data_plot1, aes(x = formula, y = n)) + geom_col() + ggtitle("the vaule of n when overflow occurs")

ggplot(data = data_plot2, aes(x = formula, y = k)) + geom_col() + ggtitle("the value of k when overflow occurs")


