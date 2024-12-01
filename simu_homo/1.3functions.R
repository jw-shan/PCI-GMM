polynomial_basis4 <- function(x1,x2,x3,x4){
  cbind(1, x1, x1^2, x1^3, x2, x2^2, x2^3, 
        x3[,1], x3[,1]^2, x3[,1]^3,
        x3[,2], x3[,2]^2, x3[,2]^3,
        x3[,3],
        # x3[,4], x3[,4]^2, x3[,4]^3,
        x4,
        deparse.level = 0)
}


polynomial_basis3 <- function(x1,x2,x3){
  cbind(1, x1, x1^2, x1^3, x2, x2^2, x2^3, 
        x3[,1], x3[,1]^2, x3[,1]^3,
        x3[,2], x3[,2]^2, x3[,2]^3,
        x3[,3],
        # x3[,4], x3[,4]^2, x3[,4]^3,
        deparse.level = 0)
}


polynomial_basis2 <- function(x1,x3){
  cbind(1, x1, x1^2, x1^3, 
        x3[,1], x3[,1]^2, x3[,1]^3,
        x3[,2], x3[,2]^2, x3[,2]^3,
        x3[,3], 
        # x3[,4], x3[,4]^2, x3[,4]^3,
        deparse.level = 0)
}


polynomial_basis1 <- function(x3){
  cbind(1,x3[,1], x3[,1]^2, x3[,1]^3,
        x3[,2], x3[,2]^2, x3[,2]^3,
        x3[,3],  
        # x3[,4],  x3[,4]^2, x3[,4]^3,
        deparse.level = 0)
}


ind <- function(x){
  ifelse(x > 0, 1, 0)
}


expit <- function(x){exp(x)/(1+exp(x))}
iexpit <- function(x){1+exp(-x)}
iplus <- function(x){ifelse(x>=0,1,0)} 

normalize <- function(x){(x-min(x))/(max(x)-min(x))}