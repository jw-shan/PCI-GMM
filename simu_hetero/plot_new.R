library(reshape2)

## load data
load("res.rdata")

# set colors
# colors <- c("blue", "green", "purple", "orange", "brown", "red")   
colors <- c("#1f77b4", "#2ca02c", "#17becf", "#9467bd", "#8c564b", "#d62728")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# empirical distributions ======
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#  n=500
est.df.500 = data.frame(ate.est[,1:n.method])
colnames(est.df.500) = methods
df.long.500 <- melt(est.df.500) # 将数据转化为长格式
colnames(df.long.500) <- c("Method", "Estimate")
df.long.500$Method <- factor(df.long.500$Method, labels = methods)

# plot
pp1<- ggplot(df.long.500, aes(x = Estimate, color = Method)) +
  stat_density( geom = "line", position = "identity") +
  scale_color_manual(values = colors) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(title = "n=400",
       x = "Estimates",
       y = "Density") +
  xlim(-0.5,1.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# +
  # guides(color = guide_legend(override.aes = list(fill = NA)))
pp1

ggsave("density_n1.pdf", plot = pp1, width = 4.7, height = 3.3)


#  n=1000
est.df.1000 = data.frame(ate.est[,1:n.method+n.method])
colnames(est.df.1000) = methods
df.long.1000 <- melt(est.df.1000) # 将数据转化为长格式
colnames(df.long.1000) <- c("Method", "Estimate")
df.long.1000$Method <- factor(df.long.1000$Method, labels = methods)

# plot
pp2<- ggplot(df.long.1000, aes(x = Estimate, color = Method)) +
  stat_density( geom = "line", position = "identity")  +
  scale_color_manual(values = colors)  +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +  # 添加虚线
  labs(title = "n=800",
       x = "Estimates",
       y = "Density") +
  xlim(-0.5,1.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
pp2

ggsave("density_n2.pdf", plot = pp2, width = 4.7, height = 3.3)




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# comparison between proposed method and proximal DR ======
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ate.est.plot=ate.est[,c(6,5)]
sd.est.plot=sd.est[,c(6,5)]



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
  # Bottom
  geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(-2,1,0.1)) +
  # Labels
  # geom_label( aes(x=-1, y=100, label="Proposed method"), color="#69b3a2",size=3) +
  # geom_label( aes(x=-0.35, y=100, label="Proximal DR"), color="#404080",size=3) +
  # annotate("text", x=-1, y=75, label="0.966",size=4)+
  # annotate("text", x=-0.35, y=75, label="0.571",size=4)+
  # annotate("text", x=-1.5, y=75, label="Power =",size=4)+
  ## verticle
  # annotate("text", x=-1, y=120, label="Method",size=4)+
  geom_label( aes(x=-1, y=100, label="Proposed Method"), color="#69b3a2",size=3) +
  geom_label( aes(x=-1, y=80, label="Proximal DR"), color="#404080",size=3) +
  annotate("text", x=-0.4, y=120, label="Power",color="gray60",size=4)+
  annotate("text", x=-0.4, y=100, label="0.966",color="gray60",size=4)+
  annotate("text", x=-0.4, y=80, label="0.571",color="gray60",size=4)+
  # geom_label( aes(x=2.3, y=100, label="0.48"),size=4) +
  # geom_label( aes(x=2.3, y=83, label="1.08"),size=4) +
  # geom_label( aes(x=2.3, y=117, label="Power"),size=4) +
  # annotate("text", x=2.1, y=83, label="1.08",size=4) +
  # annotate("text", x=2.1, y=100, label="0.48",size=4)+
  # annotate("text", x=2.1, y=117, label="Power",size=4) +
  # # Top estimates
  # geom_density( aes(x = var3, y=..count..)) +
  scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  # 设置颜色
  theme_minimal() +  # 使用 theme_minimal 代替 theme_ipsum
  xlab("Lower bound of 95% CI") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = "none") +
  # geom_vline(xintercept = 0, color="grey",size=0.3) +
  xlim(-1.5,0.9)

p2


ggsave("95lower_new.pdf", plot = p2, width = 4.7, height = 3.3)




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
  # geom_label( aes(x=2, y=16, label="Proposed method"), color="#69b3a2") +
  # Bottom
  geom_histogram( aes(x = var2, y = -..count.., fill = ifelse(var2 < 0, "var2_neg", "var2_pos")),breaks=seq(0,5,0.05)) +
  # geom_label( aes(x=2, y=-16, label="Proximal DR"), color="#404080") +
  #### labels
  # annotate("text", x=1, y=160, label="Method",size=4)+
  geom_label( aes(x=1, y=135, label="Proposed Method"), color="#69b3a2",size=3) +
  geom_label( aes(x=1, y=110, label="Proximal DR"), color="#404080",size=3) +
  annotate("text", x=1.6, y=160, label="Average length",color="gray60",size=4)+
  annotate("text", x=1.6, y=135, label="0.48",color="gray60",size=4)+
  annotate("text", x=1.6, y=110, label="1.08",color="gray60",size=4)+
  scale_fill_manual(values = c("var1_neg" = "grey", "var1_pos" = "#69b3a2", "var2_neg" = "grey", "var2_pos" = "#404080")) +  # 设置颜色
  theme_minimal() +  # 使用 theme_minimal 代替 theme_ipsum
  xlab("Length of 95% CI") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        legend.position = "none") +
  # geom_vline(xintercept = 0, color="grey",size=0.3) +
  xlim(0,2)

p3


ggsave("CI_len_new.pdf", plot = p3, width = 4.7, height = 3.3)



# == hist of K selection ========
pdf(paste0("hist_K.pdf"), height=8 , width=12,pointsize = 30)
par(mfrow = c(1, 2))
hist(K.sel[,1], main=paste0("n=",numt[1]), breaks=2:12, xlab="K", col="lightblue", border="black", ylim=c(0, 550))
hist(K.sel[,2], main=paste0("n=",numt[2]), breaks=2:12, xlab="K", col="lightblue", border="black", ylim=c(0, 550))
dev.off()




