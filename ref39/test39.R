setwd("C:\\Users\\intern\\OneDrive\\Study\\GISL\\Data\\impute\\ref39")
load("./ref39.RData")
load("./mi.RData")
load("./label.RData")
D <- log2(t + 1) # log transformation of count data

Input <- t(D) # data matrix, cells in rows, genes in columns



library(DrImpute)
library(Rtsne)
library(gislkit)
scim<-t(read.csv("scimpute_count.csv",header = 1,row.names = 1))
drim<-DrImpute(t(hi.t))#gene by cell
drim<-t(drim)



spm<-cor(x = t(Input),method="spearman")
prn<-cor(x = t(Input),method="pearson")
hi.t<-impute(Input,mi,spm,prn)


a<-Rtsne(hi.t,dims =2,initial_dims = 1000,max_iter = 10000,theta = 0.0,eta = 150)
x<-a$Y



plot(x[which(lb=="NP"),1], x[which(lb=="NP"),2], type="p",pch=21, bg = "salmon1", col= "salmon2", cex=1.7, xlab="tsne1", ylab="tsne2",ylim=c(-100,100), xlim=c(-100,100),  cex.lab=1.5, cex.axis=1.7, font.lab=2, bty='n')

points(x[which(lb=="NF"),1], x[which(lb=="NF"),2], type="p",pch=21, bg = "plum3", col = "plum4", cex=1.7)

points(x[which(lb=="TH"),1], x[which(lb=="TH"),2], type="p",pch=21, bg = "skyblue1", col= "skyblue2", cex=1.7)

points(x[which(lb=="PEP"),1], x[which(lb=="PEP"),2], type="p",pch=21, bg = "olivedrab2", col= "olivedrab3", cex=1.7)


temp <- legend("topright",  border = NULL,legend = c("NP","NF", "TH", "PEP"),
               lty = c(0), pch=c(21,21,21,21), pt.bg=c("salmon1","plum3","skyblue1","olivedrab2"), xjust = 0, yjust = 0, bty = "n")


