setwd("C:\\Users\\intern\\OneDrive\\Study\\GISL\\Data\\impute\\ref10")
b <-read.table("Pollen2014.txt", sep=',', header = T,row.names=1)

lb <-read.table("SupplementaryLabels.txt", sep=',', header = T)

D <- log2(as.matrix(b) + 1) # log transformation of count data

Input <- t(D) # data matrix, cells in rows, genes in columns



library(DrImpute)
library(Rtsne)
scim<-t(read.csv("scimpute_count.csv",header = 1,row.names = 1))
drim<-DrImpute(t(Input))#gene by cell
drim<-t(drim)



hi.t<-impute(Input,mi)


a<-Rtsne(drim,dims = 2,initial_dims = 1000,max_iter = 10000,theta = 0.0,eta = 150)
x<-a$Y



plot(x[which(lb[,3]=="K562"),1], x[which(lb[,3]=="K562"),2], type="p",pch=21, bg = "salmon1", col= "salmon2", cex=1.7, xlab="tsne1", ylab="tsne2",ylim=c(-25,30), xlim=c(-20,20),  cex.lab=1.5, cex.axis=1.7, font.lab=2, bty='n')
points(x[which(lb[,3]=="HL60"),1], x[which(lb[,3]=="HL60"),2], type="p",pch=21, bg = "orangered3", col="orangered4",cex=1.7)
points(x[which(lb[,3]=="2339"),1], x[which(lb[,3]=="2339"),2], type="p",pch=21, bg = "plum3", col = "plum4", cex=1.7)
points(x[which(lb[,3]=="iPS"),1], x[which(lb[,3]=="iPS"),2], type="p",pch=21, bg = "gray61", col = "gray46", cex=1.7)
points(x[which(lb[,3]=="2338"),1], x[which(lb[,3]=="2338"),2], type="p",pch=21, bg = "steelblue2", col= "steelblue3", cex=1.7)
points(x[which(lb[,3]=="BJ"),1], x[which(lb[,3]=="BJ"),2], type="p",pch=21, bg = "skyblue1", col= "skyblue2", cex=1.7)
points(x[which(lb[,3]=="NPC"),1], x[which(lb[,3]=="NPC"),2], type="p",pch=21, bg = "olivedrab2", col= "olivedrab3", cex=1.7)
points(x[which(lb[,3]=="GW21"),1], x[which(lb[,3]=="GW21"),2], type="p", pch=21, bg = "darkolivegreen4", col= "darkolivegreen", cex=1.7)
points(x[which(lb[,3]=="GW21+3"),1], x[which(lb[,3]=="GW21+3"),2], type="p", pch=21, bg = "darkseagreen", col= "darkseagreen4", cex=1.7)
points(x[which(lb[,3]=="Kera"),1], x[which(lb[,3]=="Kera"),2], type="p",pch=3, col = "royalblue3", cex=2)
points(x[which(lb[,3]=="GW16"),1], x[which(lb[,3]=="GW16"),2], type="p", pch=4, col = "aquamarine3", cex=1.5)

temp <- legend("topright",  border = NULL,legend = c("K562","HL60", "2339","", "iPS"), col = c("salmon2", "orangered4", "plum4","white","gray46"),
               text.width = strwidth("00,00000,0000000"),
               lty = c(0), pch=c(21,21,21,21,21), pt.bg=c("salmon1","orangered3","plum3","white","gray61"), xjust = 0, yjust = 0, bty = "n")


temp <- legend("topright",  border = NULL,legend = c("2338","BJ", "Kera", "", "NPC", "GW21", "GW21+3", "GW16"), col = c("steelblue3", "skyblue2", "royalblue3", "white", "olivedrab3", "darkolivegreen", "darkseagreen4", "aquamarine3" ),
               text.width = strwidth("00,000,0"),
               lty = c(0), pch=c(21,21,3,3,21,21,21,4), pt.bg=c("steelblue2","skyblue2", "white", "white", "olivedrab2", "darkolivegreen4", "darkseagreen"), xjust = 0, yjust = 0,bty = "n")
