################################################
install.packages("ggcorrplot")
library("ggplot2")
library("ggcorrplot")
library("reshape2")
library("dplyr")
#20220504 Paper5_XLY 相关性热图 
setwd("/Users/Mike/PhdFile/PHD_Research/科研成果进击-论文专利/博士期间文章/0paper2修改中-JiulongRiver_pterin/2-Metal Paper-To be wtitten/DataAnalysis/R_Code")
Data1 <- read.csv("Fig_S2a.csv", header = TRUE, sep = ",")
head(Data1[,1:8])
dim(Data1) 
corData1 <- round(cor(Data1), 1) #round()函数自定义小数点后位数
data2 <- as.data.frame(corData1) %>% #将矩阵转换成数据框
  mutate(x=rownames(corData1)) %>%  #新建一列x，是11种属性变量
  melt(id='x') %>%                   #将宽数据转换成长数据，更适合ggplot2绘图
  rename('y'='variable','Corr'='value')  #将variable列名改成y
list <- rownames(corData1)
list <- factor(list,levels = list)
ggplot(data2,aes(factor(x,levels = list),
                 factor(y,levels = list), #定义x，y轴顺序，防止被默认改变
                 fill=Corr))+  #根据相关性值填充颜色
  geom_tile()+  #色块函数
  scale_fill_gradient2(low = 'Blue',mid = 'white',high ='red',
                       limits=c(-1,1),breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1))+
  labs(x=NULL,y=NULL)+
  theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(face = "italic", color = "black",
                                   size = 10, angle = 45, vjust = 1, hjust = 1))
#library(ggcorrplot)
ggcorrplot(corData1,method = "circle",lab=T, lab_size = 3.5)+
  scale_fill_gradient2(low = '#CCFF66',mid = 'white',high ='#CC66FF',
                       limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1))





#使用ggcorrplot包的cor_pmat()函数计算p值：
pcorData1 <- cor_pmat(corData1)
head(pcorData1[,1:3])           
ggcorrplot(corData1,hc.order = T,  #分等级聚类重排矩阵
           method = "circle",
           ggtheme = ggplot2::theme_void(base_size = 12), #主题修改
           #colors = c("CornflowerBlue","white","Salmon"), #autumn自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
           #colors = c("#F4A582","white","#92C5DE"), #spring，看自己喜欢，或是参考好看的文献Figure用法。
           #colors = c("#CC99FF","white","#FF9933"), #winter 自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
           colors = c("#CCFF66","white","#CC66FF"), #summer自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
           lab = T,lab_size = 3,    #相关系数文本字体大小
           tl.cex = 10,             #坐标轴字体大小
           p.mat = pcorData1,         #添加显著性信息
           sig.level = 0.05,        #显著性水平
           pch = 4,                 #不够显著的色块进行标记，pch表示选择不同的标记方法，可以尝试其他数字表示什么标记方法
           pch.cex = 5)            #不显著标记的大小，使用insig = "blank"将不显著的空白处理



