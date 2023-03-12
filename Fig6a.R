#####
install.packages("factoextra")
#载入包
library(factoextra)
setwd("/Users/Mike/PhdFile/PHD_Research/科研成果进击-论文专利/博士期间文章/0paper2修改中-JiulongRiver_pterin/2-Metal Paper-To be wtitten/DataAnalysis/R_Code")
# 载入数据
Metal0 <- read.csv("Fig6a.csv", header = T, sep = ",")
Metal <- na.omit(Metal0)
# 数据进行标准化
df <- scale(Metal[3:13,2:49]) 
df <- na.omit(df)
# 查看数据的前五行
head(df, xn = 5)
#确定最佳聚类数目
fviz_nbclust(df, kmeans, method = "wss",linecolor = "Salmon",cex = 2) + 
  geom_vline(xintercept = 4, linetype = 2,colour = "grey")
set.seed(123)
d <- dist(df)  # 计算距离 默认欧式距离
fit_average <- hclust(d, method="average") # 聚类
result_hc <- plot(fit_average, hang = -1,  main = "Average Linkage Clustering")
result <- dist(df, method = "euclidean")
result_hc <- hclust(d = result, method = "ward.D2")
fviz_dend(result_hc, cex = 0.5)
fviz_dend(result_hc, k = 4, show_labels =FALSE,
          cex = 2, main = "",lwd = 1,xlab = "Dissolved Metal",
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE          
)
