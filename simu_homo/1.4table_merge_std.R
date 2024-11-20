rm(list=ls())
# library(knitr)



## load data
load("res_homo.rdata")

# initaialize res
final.df <- data.frame(methods = methods,
                       bias.500=NA, SD.500=NA,length.500=NA,CP.500=NA,power.500=NA,
                       bias.1000=NA, SD.1000=NA,length.1000=NA,CP.1000=NA,power.1000=NA)


summary_est <- function(col,true.value,std){
  # threshold <- 150
  # col = col[abs(col)<=threshold] 
  J = length(col)
  
  Delta <- mean(col)
  bias  <- abs(Delta - true.value)
  SD <- sd(col)
  CP <- sum(true.value > col - 1.96 * std & true.value <  col+1.96 * std, na.rm = TRUE)/J
  # len <- median(2*1.96*std, na.rm = TRUE)
  len <- mean(ifelse(std<2,2*1.96*std,NA), na.rm = TRUE)
  power <- sum(col-1.96*std>0, na.rm = TRUE)/J
  
  # format
  bias <- format(round(bias,2), nsmall=2)
  SD <- format(round(SD,2), nsmall=2)
  CP <- format(round(CP,3), nsmall=3)
  len <- format(round(len,2), nsmall=2)
  power <- format(round(power,3), nsmall=3)
  
  res <- c(bias,SD,len,CP,power)
  # res <- round(res,3)
  # res <- format(res,nsmall = 3)
  return(res)
}


for (i in 1:n.method) { # 5 approaches * 12 stat
  final.df[i,2:6] <- summary_est(ate.est[,i],true.value,sd.est[,i])
  final.df[i,7:11] <- summary_est(ate.est[,n.method+i],true.value,sd.est[,n.method+i])
}


# View(cbind(final.df,final.nde.df))

## transform to latex
# table.comb = cbind(final.df,final.nde.df[,3:5],final.ate.df[,3:5])
# View(final.df)
# View(final.df.ver)
# write.csv(final.df.ver, file = "final.csv")
print(final.df)
# final.df.table <- final.df[,c(2,3,4,6,19,20,22,23,24,26)]
# kable(final.df.table,format = "latex")

# library(knitr)
# kable(final.df,format = "latex")


