source("1.3functions.R")

# ====== GMM with diverging moments (proposed) =========

DGMM <- function(data.input,K=NA){
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  if(is.na(K)){
  # === select K ====
  K.max <- 12
  CCC <- matrix(NA, ncol = 12, nrow = n)
  CCC[,1] <- 1
  CCC[,2] <- A
  CCC[,3] <- Z
  CCC[,4] <- X
  CCC[,5] <- X^2
  CCC[,6] <- X^3
  CCC[,7] <- A*X^2
  CCC[,8] <- Z*X^2
  CCC[,9] <- A*X^3
  CCC[,10] <- Z*X^3
  CCC[,11] <- A*X
  CCC[,12] <- Z*X
  
  
  loss = rep(99999999,K.max)
  for (K in 4:K.max) {
    uk <- CCC[,1:K]
    g1 <- function(gamma,x){
      res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X 
      res <- as.numeric(res)
      return(res*uk)
    }
    res <- gmm(g1,cbind(Z,A,W,X,Y),rep(0,4),weightsMatrix=t(uk)%*%uk/n)
    summary(res)
    h.hat = cbind(1,A,W,X)%*%res$coefficients
    h.deriv = cbind(1,A,W,X) #N*p
    rho = as.vector(Y-h.hat)
    Upsilon = t(rho*uk)%*%(rho*uk)/n #K*K
    Gamma = -t(uk)%*%(h.deriv)/n #K*p
    Omega = t(Gamma)%*%ginv(Upsilon)%*%Gamma #p*p
    d.tilde = uk %*% ginv(t(uk)%*%uk/n) %*% Gamma  #N*p
    eta.tilde = -h.deriv-d.tilde #N*p
    D.star = uk %*% ginv(Upsilon) %*% Gamma  #N*p
    xi = diag(uk %*% ginv(Upsilon) %*% t(uk))/n #N*1
    
    Pi = colSums(xi*rho*eta.tilde%*%ginv(Omega)) #p*1
    Phi = colSums(xi*((D.star*rho^2+h.deriv)%*%ginv(Omega))^2) - diag(ginv(Omega))
    SGMM = sum(Pi^2/n + Phi)
    loss[K]=SGMM
  }
  
  K.sel = which(loss==min(loss))}else{K.sel=K}
  
  uk <- CCC[,1:K.sel]
  g1 <- function(gamma,x){
    res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X 
    res <- as.numeric(res)
    return(res*uk)
  }
  
  # g1 <- function(gamma,x) # x=(Z,A,W,X,Y)
  # {
  #   # h(W,A,X)= gamma[1] + gamma[2]*A + gamma[3]*W + gamma[4]*X  
  #   res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X 
  #   
  #   ee <- res
  #   ee <- cbind(ee,res*Z)
  #   ee <- cbind(ee,res*A)
  #   ee <- cbind(ee,res*X)
  #   ee <- cbind(ee,res*X^2)
  #   ee <- cbind(ee,res*Z*X^2)
  #   ee <- cbind(ee,res*A*X^2)
  #   ee <- cbind(ee,res*X^3)
  #   
  #   return(ee)
  # }
  
  res <- gmm(g1,cbind(Z,A,W,X,Y),rep(0,4))
  summary(res)
  tau.hat = res$coefficients[2] 
  std = sqrt(res$vcov[2,2])
  
  
  return(list(tau.hat=tau.hat, std=std,K.sel=K.sel,h.coef=res$coefficients))
  
}


# ====== GMM with fixed moments =========
FGMM <- function(data.input){
  
 
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  g1 <- function(gamma,x) # x=(Z,A,W,X,Y)
  {
    res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X  
    ee <- res
    ee <- cbind(ee,res*Z)
    ee <- cbind(ee,res*A)
    ee <- cbind(ee,res*X)
    # ee <- cbind(ee,gamma[5] - gamma[2])
    return(ee)
  }
  
  res <- gmm(g1,cbind(Z,A,W,X,Y),rep(0,4))
  summary(res)
  tau.hat = res$coefficients[2] 
  std = sqrt(res$vcov[2,2])
  
  # res <- gmm(g1,cbind(Z,A,W,X,Y),rep(0,5))
  # tau.hat = res$coefficients[5]  
  
  return(list(tau.hat=tau.hat,std=std))
  
}


# ====== proximal ipw =========
PIPW <- function(data.input,var.cal=F,...){
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)

  ee.ipw <- function(gamma){
    q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z + gamma[4]*X))
    ee <- rep(0,4)
    ee[1] <- sum((2*A-1)*q)
    ee[2] <- sum((2*A-1)*q*W)
    ee[3] <- sum(A*q-1)
    ee[4] <- sum((2*A-1)*q*X)
    ee
  }

  res <- BBsolve(par=rep(0,4), fn=ee.ipw, quiet=T)
  q.hat <- 1 + exp((2*A-1)*(cbind(1,A,Z,X)%*%res$par))
  tau.hat = mean((2*A-1)*q.hat*Y)
  
  
  #estimate asymptotic variance (stacking estimating functions)
  ipw.std = NA
  if(var.cal==T){
    ee.ipw.stack <- function(gamma){
      q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z + gamma[4]*X))
      res <- (2*A-1)*q
      ee <- rep(0,5)
      ee[1] <- sum(res)
      ee[2] <- sum(res*W)
      ee[3] <- sum(res*A-1)
      ee[4] <- sum(res*X)
      # add parameter
      ee[5] <- sum((2*A-1)*q*Y-gamma[5])
      ee
    }
    dg = jacobian(func=ee.ipw.stack,x=c(res$par,tau.hat))/n
    
    g.ipw.stack <- function(gamma){
      q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z + gamma[4]*X))
      res <- (2*A-1)*q
      ee <- t(cbind(res, res*W, res*A-1, res*X, (2*A-1)*q*Y-gamma[5]))
      ee
    }
    gg <- g.ipw.stack(c(res$par,tau.hat))
    
    ipw.var = diag(ginv(dg)%*%(gg%*%t(gg)/n)%*%t(ginv(dg))/n)[5]
    ipw.std = sqrt(ipw.var)
    
  }
  
  return(list(tau.hat=tau.hat,std=ipw.std))
}


# ====== proximal OR =========
POR <- function(data.input,var.cal=F,...){
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  
  ee.or <- function(gamma){
    res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X  
    ee <- rep(0,4)
    ee[1] <- sum(res)
    ee[2] <- sum(res*Z)
    ee[3] <- sum(res*A)
    ee[4] <- sum(res*X)
    ee
  }
  
  res <- BBsolve(par=rep(0,4), fn=ee.or, quiet=T)
  # h.hat <- cbind(1,A,W,X)%*%res$par
  tau.hat = res$par[2]
  
  #estimate asymptotic variance (stacking estimating functions)
  or.std = NA
  if(var.cal==T){
    dg = jacobian(func=ee.or,x=c(res$par))/n
    
    g.or.stack <- function(gamma){
      res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X  
      ee <- t(cbind(res, res*Z, res*A, res*X))
      ee
    }
    gg <- g.or.stack(res$par)
    
    or.var = diag(ginv(dg)%*%(gg%*%t(gg)/n)%*%t(ginv(dg))/n)[2]
    or.std = sqrt(or.var)
    
  }
  
  return(list(tau.hat=tau.hat,std=or.std))
}






# ====== DR estiamtion =========
PDR <- function(data.input,var.cal=F,q.model=T,...){
  
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  Z.t <- if(q.model==T){Z}else{sqrt(abs(Z))+1}
  
  # estimate h
  ee.or <- function(gamma){
    res <- Y - gamma[1] - gamma[2]*A - gamma[3]*W - gamma[4]*X  
    ee <- rep(0,4)
    ee[1] <- sum(res)
    ee[2] <- sum(res*Z)
    ee[3] <- sum(res*A)
    ee[4] <- sum(res*X)
    ee
  }
  
  res.or <- BBsolve(par=rep(0,4), fn=ee.or, quiet=T)
  h.hat <- cbind(1,A,W,X)%*%res.or$par
  
  # estimate q
  ee.ipw <- function(gamma){
    q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z.t + gamma[4]*X))
    ee <- rep(0,4)
    ee[1] <- sum((2*A-1)*q)
    ee[2] <- sum((2*A-1)*q*W)
    ee[3] <- sum(A*q-1)
    ee[4] <- sum((2*A-1)*q*X)
    ee
  }
  
  res.ipw <- BBsolve(par=rep(0,4), fn=ee.ipw, quiet=T)
  q.hat <- 1 + exp((2*A-1)*(cbind(1,A,Z.t,X)%*%res.ipw$par))
  
  # DR estimator
  tau.hat = mean((2*A-1)*q.hat*(Y-h.hat)+res.or$par[2])
  tau.hat
  
  
  #estimate asymptotic variance (stacking estimating functions)
  dr.std = NA
  if(var.cal==T){
    
    # If q is correct specified, calculate the std directly
    if(q.model==T){
      dr.std = sqrt(mean(((2*A-1)*q.hat*(Y-h.hat)+res.or$par[2]-tau.hat)^2)/n)
      return(list(tau.hat=tau.hat,std=dr.std))
    }
    
    
    ee.dr.stack <- function(gamma){
      h <- gamma[1] + gamma[2]*A + gamma[3]*W + gamma[4]*X  
      q <- 1 + exp((2*A-1)*(gamma[5] + gamma[6]*A + gamma[7]*Z.t + gamma[8]*X))
      
      ee <- rep(0,9)
      ee[1] <- sum((Y-h))
      ee[2] <- sum((Y-h)*Z)
      ee[3] <- sum((Y-h)*A)
      ee[4] <- sum((Y-h)*X)
      
      ee[5] <- sum((2*A-1)*q)
      ee[6] <- sum((2*A-1)*q*W)
      ee[7] <- sum(A*q-1)
      ee[8] <- sum((2*A-1)*q*X)
      
      ee[9] <- sum((2*A-1)*q*(Y-h) + gamma[2] - gamma[9]) 
      ee
    }
    dg = jacobian(func=ee.dr.stack,x=c(res.or$par,res.ipw$par,tau.hat))/n
    
    g.dr.stack <- function(gamma){
      h <- gamma[1] + gamma[2]*A + gamma[3]*W + gamma[4]*X  
      q <- 1 + exp((2*A-1)*(gamma[5] + gamma[6]*A + gamma[7]*Z.t + gamma[8]*X))
      
      ee <- t(cbind(
        Y-h, (Y-h)*Z, (Y-h)*A, (Y-h)*X,
        (2*A-1)*q, (2*A-1)*q*W, A*q-1, (2*A-1)*q*X, 
        (2*A-1)*q*(Y-h) + gamma[2] - gamma[9]
      ))
      ee
    }
    gg <- g.dr.stack(c(res.or$par,res.ipw$par,tau.hat))
    
    dr.var = diag(ginv(dg)%*%(gg%*%t(gg)/n)%*%t(ginv(dg))/n)[9]
    dr.std = sqrt(dr.var)
    
  }
  
  
  return(list(tau.hat=tau.hat,std=dr.std))
  
}



# ====== DR with eff. h =========
PDR.eff <- function(data.input,var.cal=F,bs.times=50,...){
  
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  
  # estimate h
  res.or <- DGMM(data.input,...)
  h.hat <- cbind(1,A,W,X)%*%res.or$h.coef
  K.sel <- res.or$K.sel
  
  # estimate q when q is correctly specified
  ee.ipw <- function(gamma){
    q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z + gamma[4]*X))
    ee <- rep(0,4)
    ee[1] <- sum((2*A-1)*q)
    ee[2] <- sum((2*A-1)*q*W)
    ee[3] <- sum(A*q-1)
    ee[4] <- sum((2*A-1)*q*X)
    ee
  }
  
  res.ipw <- BBsolve(par=rep(0,4), fn=ee.ipw, quiet=T)
  q.hat <- 1 + exp((2*A-1)*(cbind(1,A,Z,X)%*%res.ipw$par))
  
  tau.hat = mean((2*A-1)*q.hat*(Y-h.hat)+res.or$h.coef[2])
  tau.hat
  dr.std = sqrt(mean(((2*A-1)*q.hat*(Y-h.hat)+res.or$h.coef[2]-tau.hat)^2)/n)
  
  
  # estimate q when q is mis-specified
  Z.mis <- sqrt(abs(Z))+1
  ee.ipw.mis <- function(gamma){
    q <- 1 + exp((2*A-1)*(gamma[1] + gamma[2]*A + gamma[3]*Z.mis + gamma[4]*X))
    ee <- rep(0,4)
    ee[1] <- sum((2*A-1)*q)
    ee[2] <- sum((2*A-1)*q*W)
    ee[3] <- sum(A*q-1)
    ee[4] <- sum((2*A-1)*q*X)
    ee
  }
  
  res.ipw.mis <- BBsolve(par=rep(0,4), fn=ee.ipw.mis, quiet=T)
  q.hat.mis <- 1 + exp((2*A-1)*(cbind(1,A,Z.mis,X)%*%res.ipw.mis$par))
  
  tau.hat.mis = mean((2*A-1)*q.hat.mis*(Y-h.hat)+res.or$h.coef[2])
  tau.hat.mis
  
  
  #estimate asymptotic variance (bootstrap)
  dr.std.mis = NA
  if(var.cal==T){
    tau.bs.mis=0
    for (bs.i in 1:bs.times) {
      index = sample(n,n,T)
      A.bs <- A[index]
      X.bs <- X[index]
      Y.bs <- Y[index]
      Z.bs <- Z[index]
      W.bs <- W[index]
      data.bs = list(A=A.bs,X=X.bs,Y=Y.bs,Z=Z.bs,W=W.bs)
      tau.bs.mis[bs.i] <- PDR.eff(data.bs,var.cal=F,K.sel)$tau.hat.mis
    }
    dr.std.mis = sd(tau.bs.mis)
  }
  
  
  return(list(tau.hat=tau.hat, tau.hat.mis=tau.hat.mis, 
              std=dr.std, std.mis=dr.std.mis))
  
}


# ====== proximal 2sls =========
P2SLS <- function(data.input,var.cal=F,...){
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  m_iv <- ivreg(Y~A+W+X | A+Z+X)
  summary(m_iv)
  tau.hat = summary(m_iv)$coefficients[2,1]
  std = summary(m_iv)$coefficients[2,2]
  
  return(list(tau.hat=tau.hat,std=std))
}


# ====== Naive =========
LS <- function(data.input,var.cal=F,...){
  
  # data.input <- datagen(500)
  A <- data.input$A
  X <- data.input$X
  Y <- data.input$Y
  Z <- data.input$Z
  W <- data.input$W
  n=length(A)
  
  m <- lm(Y~A+W+Z+X)
  tau.hat = summary(m)$coefficients[2,1]
  std = summary(m)$coefficients[2,2]
    
  return(list(tau.hat=tau.hat,std=std))
}
