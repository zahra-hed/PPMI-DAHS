#
#  Part 2 of PPMI project
#  :Defining & Solving the Model
#

#install.packages("dplyr")
#install.packages("ggbiplot2")

library(dplyr)

#read main data frame
main <- read.csv("main.csv")

#K-means clustering : state ----------------------------------------------------
KM <- kmeans(scale(main[c("M1","M2","M3","M4")]), 4)
main$cluster <- KM$cluster

main$HNY[which(main$HNY==101)] <- NA
#group by clusters
if(F)'
cluster.summary <- main %>% group_by(cluster) %>% summarise(
  M1 = mean(M1), M2 = mean(M2), M3 = mean(M3), M4 = mean(M4), HNY = mean(HNY, na.rm=T),
  age=mean(age),diag=mean(diag),year=mean(year))
cluster.summary <- round(cluster.summary,1)
'

#PCA visualization
if(F)'
pca.main <- prcomp(main[c("M1","M2","M3","M4")], center=T, scale.=T)
plot(pca.main, type="l")
pca <- data.frame(pca.main$x, main$cluster)
pca1 <- pca %>% filter(main.cluster==1)
pca2 <- pca %>% filter(main.cluster==2)
pca3 <- pca %>% filter(main.cluster==3)
pca4 <- pca %>% filter(main.cluster==4)

plot(pca1$PC1, pca1$PC2, cex=0.5, xlim=c(-3,9), ylim=c(-4,4), col="aquamarine3") 
points(pca2$PC1, pca2$PC2, cex=0.5,  col="blue")
points(pca3$PC1, pca3$PC2, cex=0.5, col="darkviolet")
points(pca4$PC1, pca4$PC2, cex=0.5,  col="red")
'

#Action Definition -------------------------------------------------------------

#next cluster & LEDD difference & action difference
main[c("cluster.n","LEDD.n","LEDD.n.bin","medtype.n","medtype1.n","medtype2.n","medtype3.n")] <- c(0)
for(i in 1:(nrow(main)-1)){
  main$cluster.n[i] <- main$cluster[i+1]
  main$LEDD.n[i] <- main$LEDD[i+1] - main$LEDD[i]
  main$LEDD.n.bin[i] <- main$LEDD.n[i]/max(abs(main$LEDD.n[i]),1)
  main$medtype1.n[i] <- main$medtype1[i+1] - main$medtype1[i]
  main$medtype2.n[i] <- main$medtype2[i+1] - main$medtype2[i]
  main$medtype3.n[i] <- main$medtype3[i+1] - main$medtype3[i]
  main$medtype.n[i] <- main$medtype1.n[i]+1 + (main$medtype2.n[i]+1)*3 + (main$medtype3.n[i]+1)*9
}; rm(i)
main[which(main$N_MAX==main$NUM), c("cluster.n","LEDD.n","LEDD.n.bin","medtype.n","medtype1.n","medtype2.n","medtype3.n")] <- NA




main$medtype.n[which(main$medtype.n==26)] = 1
main$medtype.n[which(main$medtype.n==27)] = 2
main$medtype.n[which(main$medtype.n==29)] = 3
main$medtype.n[which(main$medtype.n==30)] = 4
main$medtype.n[which(main$medtype.n==35)] = 5
main$medtype.n[which(main$medtype.n==36)] = 6
main$medtype.n[which(main$medtype.n==38)] = 7
main$medtype.n[which(main$medtype.n==39)] = 8

#action table
action.t <- data.frame(matrix(nrow=8, ncol=3))
main$LEDD.n.bin <- main$LEDD.n.bin + 2

for(i in 1:8){
  for(j in 1:3){
    action.t[i,j] <- nrow(main %>% filter(medtype.n == i) %>% filter(LEDD.n.bin==j))
  }
}


#freq check




#MDP Solving
