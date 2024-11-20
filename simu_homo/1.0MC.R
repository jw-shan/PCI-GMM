rm(list=ls())

source("1.1Data_gen.R")
source("1.2Estimation.R")
source("1.3functions.R")
library(MASS)
library(stats)
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gmm)
library(hrbrthemes)
library(BB)
library(numDeriv)
library(ivreg)
# library(nloptr)



# Current time
current_time <- Sys.time()
print(current_time)
formatted_time <- format(current_time, "24%m%d_%H%M%S")



# Monte Carlo setting
# seed = sample(999999,1)
seed = 695476 # for replication of the results in paper
# seed = 19336922 # for replication of the results in paper
numt = c(400,800)
# numt = 
J = 500
methods=c("Naive","GMM-f","P2SLS","PIPW","PDR","GMM-d")
n.method = length(methods)

# true value
true.value = 0.5


# print information
print(numt)
print(paste0("seed=",seed))
print(paste0("J=",J))
print(paste0("true.vaue=",true.value))



# parallel computation settings
ncores = detectCores()
coremax = 80
if (ncores<coremax) {
  cl = makeCluster(ncores)
}else{
  cl = makeCluster(coremax)
}
clusterExport(cl,ls())
rbb<-clusterEvalQ(cl,c(library(gmm),library(MASS),library(BB),library(numDeriv),library(ivreg)))



## Estimation function
estimation <- function(trial) {
  
  # 6+6+6 cols: theta,n=500; theta,n=1000; alpha1,n=500; alpha1,n=1000; alpha0,n=500; alpha0, n=1000. + their std + std of nie,nde,ate 
  # 5 rows: oracle, w=0.6, w=0.3, w=0.1, MAR
  res = matrix(NA, nrow = n.method, ncol = length(numt)*3) 
  # trial=i=1
  for (i in seq_along(numt)) {
    data.input<-datagen(numt[i],seed+trial)
    
    naive <- LS(data.input)
    p2sls <- P2SLS(data.input)
    gmm.div <- DGMM(data.input)
    gmm.fix <- FGMM(data.input)
    pipw <- PIPW(data.input,var.cal=T)
    pdr <- PDR(data.input,var.cal=T)

    # plug.in <- DGMM(data.input,plugin=T)

    # res[2,i] <- plug.in$tau.hat
    res[1,i] <- naive$tau.hat
    res[2,i] <- gmm.fix$tau.hat
    res[3,i] <- p2sls$tau.hat
    res[4,i] <- pipw$tau.hat
    res[5,i] <- pdr$tau.hat
    res[6,i] <- gmm.div$tau.hat
    
    res[1,i+2] <- naive$std
    res[2,i+2] <- gmm.fix$std
    res[3,i+2] <- p2sls$std
    res[4,i+2] <- pipw$std
    res[5,i+2] <- pdr$std
    res[6,i+2] <- gmm.div$std
    
    res[1,i+4] <- gmm.div$K.sel
    # res[2,i+4] <- gmm.fix$K.sel
    
  }
  
  return(res)
}

est  <- parSapply(cl,1:J,estimation)
est  <- t(est)

ate.est <- est[,1:(n.method*2)]
sd.est <- est[,1:(n.method*2)+n.method*2]
K.sel <- est[,n.method*c(4,5)+1]
# summary(ate.est)
apply(ate.est, 2, sd)
# summary(sd.est)


# save data
save.image(paste0("res_",formatted_time,".RData"))

source("1.4table_merge_std.R")
source("plot.R")


if(0){
  
ate.est.plot=ate.est[,c(6,3)]
sd.est.plot=sd.est[,c(6,3)]

# # ====== CI =========
# summ_CI <- function(ate.est,sd.est,truvalue=0.5){
#   J = length(ate.est)
#   count <- sum(0 < ate.est - 1.96 * sd.est, na.rm = TRUE)
#   prop <- count/J
#   ave.len <- mean(1.96 * sd.est*2)
#   CP <- sum(truvalue < ate.est + 1.96 * sd.est & truvalue > ate.est - 1.96 * sd.est, na.rm = TRUE)/J
#   return(c(ave.len,prop,CP))
# }
# 
# CI.df <- data.frame(methods=c("g-formula","proposed","g-formula800","proposed800"),ave.len=NA,prop=NA,CP=NA)
# CI.df[2,2:4]<-summ_CI(ate.est[,1],sd.est[,1])
# CI.df[1,2:4]<-summ_CI(ate.est[,2],sd.est[,2])
# CI.df[4,2:4]<-summ_CI(ate.est[,3],sd.est[,3])
# CI.df[3,2:4]<-summ_CI(ate.est[,4],sd.est[,4])
# CI.df


# #  ====== density plot ========
# # Dummy data
# data1 <- data.frame(
#   var1 = ate.est.plot[,1],
#   var2 = ate.est.plot[,2]
# )
# 
# # Chart
# p1 <- ggplot(data1, aes(x=x) ) +
#   # Top
#   geom_density( aes(x = var1, y = ..density..), fill="#69b3a2") +
#   geom_label( aes(x=2, y=.3, label="Proposed method"), color="#69b3a2") +
#   # Bottom
#   geom_density( aes(x = var2, y = -..density..), fill= "#404080") +
#   geom_label( aes(x=2, y=-.3, label="Proximal 2SLS"), color="#404080") +
#   # theme_ipsum() +
#   theme_minimal() +  
#   # theme_classic() +  
#   xlab("Point estimate") +
#   theme(axis.title.x = element_text(hjust = 0.5),
#         axis.title.y = element_text(hjust = 0.5)) +
#   xlim(-1,2.5)
#   # geom_vline(xintercept = 0.5, color="red", linetype="dashed")  
# p1
# 
# # 
# # # Dummy data
# # data2 <- data.frame(
# #   var1 = ate.est.plot[,3],
# #   var2 = ate.est.plot[,4]
# # )
# # 
# # # Chart
# # p2 <- ggplot(data2, aes(x=x) ) +
# #   # Top
# #   geom_density( aes(x = var1, y = ..density..), fill="#69b3a2") +
# #   geom_label( aes(x=2, y=.25, label="The proposed method"), color="#69b3a2") +
# #   # Bottom
# #   geom_density( aes(x = var2, y = -..density..), fill= "#404080") +
# #   geom_label( aes(x=2, y=-.25, label="Proximal 2SLS"), color="#404080") +
# #   # theme_ipsum() +
# #   theme_minimal() +  
# #   # theme_classic() +  
# #   xlab("Estimates") +
# #   theme(axis.title.x = element_text(hjust = 0.5),
# #         axis.title.y = element_text(hjust = 0.5)) +
# #   xlim=c(-1,3)
# #   # geom_vline(xintercept = 0.5, color="red", linetype="dashed")  
# # p2
# 
# # 设置尺寸并保存为pdf
# ggsave("density_p.pdf", plot = p1, width = 4.7, height = 3.3)
# 


#  ====== 95% CI lower plot ========
# Dummy data
data2 <- data.frame(
  var1 = ate.est.plot[,1]-1.96*sd.est.plot[,1],
  var2 = ate.est.plot[,2]-1.96*sd.est.plot[,2]
)



# Chart
p2 <- ggplot(data2, aes(x=x) ) +
  # Top
  geom_histogram( aes(x = var1, y = ..count.., fill = ifelse(var1 < 0, "var1_neg", "var1_pos")),breaks=seq(-2,1,0.1)) +
  geom_label( aes(x=1.5, y=12, label="Proposed method"), color="#69b3a2") +
  # Bottom
  geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(-2,1,0.1)) +
  geom_label( aes(x=1.5, y=-12, label="Proximal 2SLS"), color="#404080") +
  scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  
  theme_minimal() +  
  xlab("Lower bound of 95% CI") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = "none") +
  # geom_vline(xintercept = 0, color="grey",size=0.3) +
  xlim(-1.4,2)

p2


ggsave("95lower.pdf", plot = p2, width = 4.7, height = 3.3)




#  ====== CI length plot ========
# Dummy data
data3 <- data.frame(
  var1 = 2*1.96*sd.est.plot[,1],
  var2 = 2*1.96*sd.est.plot[,2]
)



# Chart
p3 <- ggplot(data3, aes(x=x) ) +
  # Top
  geom_histogram( aes(x = var1, y = ..count.., fill = ifelse(var1 < 0, "var1_neg", "var1_pos")),breaks=seq(0,5,0.05)) +
  geom_label( aes(x=2, y=16, label="Proposed method"), color="#69b3a2") +
  # Bottom
  geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(0,5,0.05)) +
  geom_label( aes(x=2, y=-16, label="Proximal 2SLS"), color="#404080") +
  scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  
  theme_minimal() +  
  xlab("Length of 95% CI") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = "none") +
  # geom_vline(xintercept = 0, color="grey",size=0.3) +
  xlim(0,2.4)

p3


ggsave("CI_len.pdf", plot = p3, width = 4.7, height = 3.3)

}


print(Sys.time())
print(Sys.time()-current_time)

# save data
save.image(paste0("res_",formatted_time,".RData"))
print(paste0("file:","res_",formatted_time,".RData"))


