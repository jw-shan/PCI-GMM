library(reshape2)


# 设置颜色，其中第6种方法颜色更突出
colors <- c("blue", "green", "purple", "orange", "brown", "red")  # 可以根据需要调整颜色
colors <- c("#1f77b4", "#2ca02c", "#17becf", "#9467bd", "#8c564b", "#d62728")





#  n=500
est.df.500 = data.frame(ate.est[,1:n.method])
colnames(est.df.500) = methods
df.long.500 <- melt(est.df.500) # 将数据转化为长格式
colnames(df.long.500) <- c("Method", "Estimate")
df.long.500$Method <- factor(df.long.500$Method, labels = methods)


# 画图
pp1<- ggplot(df.long.500, aes(x = Estimate, color = Method)) +
  stat_density( geom = "line", position = "identity") +
  scale_color_manual(values = colors) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # 添加虚线
  labs(title = "(a) n=400",
       x = "Estimates",
       y = "Density") +
  xlim(-0.5,1.5)+
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(fill = NA)))
pp1

ggsave("density_n1_homo.pdf", plot = pp1, width = 4.7, height = 3.3)


#  n=1000
est.df.1000 = data.frame(ate.est[,1:n.method+n.method])
colnames(est.df.1000) = methods
df.long.1000 <- melt(est.df.1000) # 将数据转化为长格式
colnames(df.long.1000) <- c("Method", "Estimate")
df.long.1000$Method <- factor(df.long.1000$Method, labels = methods)



# 画图
pp2<- ggplot(df.long.1000, aes(x = Estimate, color = Method)) +
  stat_density( geom = "line", position = "identity")  +
  scale_color_manual(values = colors)  +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # 添加虚线
  labs(title = "(b) n=800",
       x = "Estimates",
       y = "Density") +
  xlim(-0.5,1.5)+
  theme_minimal()
pp2

ggsave("density_n2_homo.pdf", plot = pp2, width = 4.7, height = 3.3)





# ===== plot in introduction ======== 
if(0){
  
 
  
  
  # #  ====== density plot ========
  # # Dummy data
  # data1 <- data.frame(
  #   var1 = ate.est[,1],
  #   var2 = ate.est[,2]
  # )
  # 
  # # Chart
  # p1 <- ggplot(data1, aes(x=x) ) +
  #   # Top
  #   geom_density( aes(x = var1, y = ..density..), fill="#69b3a2") +
  #   geom_label( aes(x=2, y=.3, label="Proposed method"), color="#69b3a2") +
  #   # Bottom
  #   geom_density( aes(x = var2, y = -..density..), fill= "#404080") +
  #   geom_label( aes(x=2, y=-.3, label="Proximal g-formula"), color="#404080") +
  #   # theme_ipsum() +
  #   theme_minimal() +  # 使用 theme_minimal 代替 theme_ipsum
  #   # theme_classic() +  # 使用 theme_classic
  #   xlab("Point estimate") +
  #   theme(axis.title.x = element_text(hjust = 0.5),
  #         axis.title.y = element_text(hjust = 0.5)) +
  #   xlim(-1,2.5)
  # # geom_vline(xintercept = 0.5, color="red", linetype="dashed")  # 添加红色虚线
  # p1
  # 
  # 
  # # 设置尺寸并保存为pdf
  # ggsave("density_p_homo.pdf", plot = p1, width = 4.7, height = 3.3)
  
  
  
  #  ====== 95% CI lower plot ========
  # Dummy data
  data2 <- data.frame(
    var1 = ate.est[,6]-1.96*sd.est[,6],
    var2 = ate.est[,2]-1.96*sd.est[,2]
  )
  
  
  
  # Chart
  p2 <- ggplot(data2, aes(x=x) ) +
    # Top
    geom_histogram( aes(x = var1, y = ..count.., fill = ifelse(var1 < 0, "var1_neg", "var1_pos")),breaks=seq(-2,1,0.1)) +
    geom_label( aes(x=1.5, y=12, label="Proposed method"), color="#69b3a2") +
    # Bottom
    geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(-2,1,0.1)) +
    geom_label( aes(x=1.5, y=-12, label="Proximal g-formula"), color="#404080") +
    scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  # 设置颜色
    theme_minimal() +  # 使用 theme_minimal 代替 theme_ipsum
    xlab("Lower bound of 95% CI") +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5),
          legend.position = "none") +
    # geom_vline(xintercept = 0, color="grey",size=0.3) +
    xlim(-1.3,2.2)
  
  p2
  
  
  ggsave("95lower_homo.pdf", plot = p2, width = 4.7, height = 3.3)
  
  
  
  
  #  ====== CI length plot ========
  # Dummy data
  data3 <- data.frame(
    var1 = 2*1.96*sd.est[,1],
    var2 = 2*1.96*sd.est[,2]
  )
  
  
  
  # Chart
  p3 <- ggplot(data3, aes(x=x) ) +
    # Top
    geom_histogram( aes(x = var1, y = ..count.., fill = ifelse(var1 < 0, "var1_neg", "var1_pos")),breaks=seq(0,5,0.05)) +
    geom_label( aes(x=2.5, y=12, label="Proposed method"), color="#69b3a2") +
    # Bottom
    geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(0,5,0.05)) +
    geom_label( aes(x=2.5, y=-12, label="Proximal g-formula"), color="#404080") +
    scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  # 设置颜色
    theme_minimal() +  # 使用 theme_minimal 代替 theme_ipsum
    xlab("Length of 95% CI") +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5),
          legend.position = "none") +
    # geom_vline(xintercept = 0, color="grey",size=0.3) +
    xlim(0,3)
  
  p3
  
  
  ggsave("CI_len_homo.pdf", plot = p3, width = 4.7, height = 3.3)
  
}


# == hist of K selection ========

pdf(paste0("hist_K_homo.pdf"), height=8 , width=12,pointsize = 30)
par(mfrow = c(1, 2))
hist(K.sel[,1], main=paste0("n=",numt[1]), breaks=0:8, xlab="K", col="lightblue", border="black", ylim=c(0, 550))
hist(K.sel[,2], main=paste0("n=",numt[2]), breaks=0:8, xlab="K", col="lightblue", border="black", ylim=c(0, 550))
dev.off()




