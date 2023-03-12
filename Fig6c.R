install.packages("factoextra")
install.packages("gmodels")   
install.packages("vegan")  
install.packages("permute")
install.packages("lattice")
library(factoextra)   
library(gmodels)
library(ggplot2)
library(lattice)
library(permute)
library(vegan)


############################# Step 1:data preparation #################################

#输入环境因子、物种以及分组信息
ENV=read.table("Fig6c.txt",sep="\t",row.names=1, header=T)  #环境因子进行平方根转换

#delwte NA
row.has.na <- apply(ENV, 1, function(x){any(is.na(x))})
predictors_no_NA <- ENV[!row.has.na, ]
ENV<-predictors_no_NA
dim(ENV)
head(ENV)

group<-ENV[c(1:2)] #group
OTU<-ENV[c(12:13)] #metal data
ENV<-ENV[c(7:11)]  #envir factor

##########################################################################################
##########################################################################################
##########################################################################################
#【重点】将环境因子加入分析！！！！！！！！！（物种+环境+样本)

otu.pca <-fast.prcomp(OTU,scale. = T)   #这里用prcomp或fast.prcomp两种算法都可以计算PCA，下面的步骤选择了第二种方法

names(otu.pca)
a<-summary(otu.pca)  # 将PCA分析的结果导入数组a，
a
tmp <- a[4]$importance    # 将a的第四个子数组中名称为importance矩阵的值，导入变量tmp。你可以输入a[4]查看，其中包含了6个主成分的信息；
pro1 <- as.numeric(sprintf("%.3f",tmp[2,1]))*100  # 矩阵第二行第一列，即PC1所占的方差
pro2 <- as.numeric(sprintf("%.3f",tmp[2,2]))*100  


#提取并转换“样本”数据  
PCA_sample = data.frame(species=row.names(otu.pca$x),PC1 = otu.pca$x[,1], PC2 = otu.pca$x[,2])  #样本PC
PCA_sample

#环境因子显著性分析,以及提取环境因子数据
ef<-envfit(PCA_sample[2:3], ENV, perm=1000)
ef 
ef.df<-as.data.frame(ef$vectors$arrows*sqrt(ef$vectors$r))
ef.df$species<-rownames(ef.df)
A <- as.list(ef$vectors)
pvals<-as.data.frame(A$pvals)
arrows<-as.data.frame(A$arrows*sqrt(A$r))
C<-cbind(arrows, pvals)
Cred<-subset(C,pvals<0.05)  #设定选择的显著性因子的P值范围,即选择显著因子
Cred

Cred<- cbind(Cred, Species = rownames(Cred))
Cred

#提取并转换“物种”数据  
species<-data.frame(species=row.names(otu.pca$rotation),PC1=otu.pca$rotation[,1],PC2=otu.pca$rotation[,2])  
species 

head(group)

#plot
p<-ggplot(data = PCA_sample, aes(x=PC1, y=PC2))+
  geom_point(aes(shape=group$GROUP1,color=group$GROUP2), size=2)+scale_shape_manual(values = c(0,1,2,3))+ 
  stat_ellipse( linetype = 2,level = 0.65,aes(group  = group$GROUP2, colour =  group$GROUP2))+
  geom_segment(data=Cred,aes(x=0,y=0,xend=PC1*3,yend=PC2*3),arrow = arrow(length = unit(0.4, "cm" )), size = 0.8,colour="black")+
  geom_text(data=Cred,aes(x=PC1*3-0.2,y=PC2*3-0.2,label=Species),size=4,hjust=0)+
  geom_point(data =species,aes(x =  PC1*4, y = PC2*4), shape="*", size=7, colour = "black")+
  geom_text(data = species,aes(PC1*4+0.05,PC2*4+0.1,label=species),colour = "black",vjust=0,hjust=0)+
  labs(x=paste("PC1 "," (",pro1,"%)",sep=""),y=paste("PC2 "," (",pro2,"%)",sep=""))+
  geom_hline(yintercept=0,linetype=1,color="grey") + geom_vline(xintercept=0,linetype=1,color="grey")+  
  theme_bw() + theme(panel.grid.minor  = element_blank())+ylim(c(-3,4))+xlim(c(-5,4))


p
