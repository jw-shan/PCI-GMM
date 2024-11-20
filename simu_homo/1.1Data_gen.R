source("1.3functions.R")

datagen <- function(n,seed=NA){
  
  if(!is.na(seed)){set.seed(seed)}
  
  # paras
  beta_a <- c(-0.1,0.5,0.5) #beta
  beta_z <- c(0.5,1,0.5,1) #alpha
  beta_w <- c(1,-1,1) #gamma
  beta_y <- c(1,0.5,0.5,1,1,0) #eta
  
  
  # generate
  eps1 <- rnorm(n)
  eps2 <- rnorm(n)
  eps3 <- rnorm(n)
  
  X  <- rnorm(n)
  U  <- rnorm(n)
  v1 <- 1
  v2 <- 1
  
  ps <- plogis(cbind(1,X,U)%*%beta_a)
  A  <- rbinom(n,1,ps)
  Z  <- cbind(1,A,X,U)%*%beta_z + eps1
  W  <- cbind(1,X,U)%*%beta_w + rnorm(n,sd=v1)
  Y  <- cbind(1,A,W,X,U,A*X)%*%beta_y + rnorm(n,sd=v2)

  
  return(list(Z=Z,W=W,Y=Y,X=X,A=A,ate=beta_y[2],U=U))
}


# test
# est<-vector(length = 500)
# for (i in 1:500) {
#   data.input <- datagen(10000,i+100)
#   A <- data.input$A
#   X <- data.input$X
#   Y <- data.input$Y
#   U <- data.input$U
#   est[i]=lm(Y~A+X+U)$coefficients[2]
# }
# mean(est)
# data.input <- datagen(10000)
# A <- data.input$A
# X <- data.input$X
# U <- data.input$U
# Y <- data.input$Y
# lm(Y~A+X+U)
# Y <- data.input$Y
# Z <- data.input$Z
# summary(data.input$mis.pr)
# mean(data.input$R)
# 
# 
# 
# # # # true-value
# data.test <- datagen(10000000)
# mean(data.test$R)
# Y00 <- data.test$Y.0.0.true
# Y10 <- data.test$eta
# mean(Y10) #theta
# mean(Y00) - mean(Y10) #NIE
# rm(data.test)
