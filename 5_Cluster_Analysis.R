#
#   
#   5. Time - Series Cluster Analysis
#   

library(dplyr)
setwd("C:/Users/kosai/Desktop/school/ppmi/dataset")

#grab clusters from python #4
clusters <- read.csv("DTW.csv")
ts_df <- read.csv("ts_df.csv")

#remove dummy column + add cluster info to 
clusters['X'] <- NULL
names(clusters)[1] <- 'cluster'
ts_df <- cbind(ts_df, clusters)
ts_df['X'] <- NULL

#next_cluster column, info about next cluster
ts_df$cluster_n <- NA
for(i in 1:nrow(ts_df)){
  if(!(is.na(ts_df$action[i]))){
    ts_df$cluster_n[i] <- ts_df$cluster[i+1]
  }
}

# action2 
#   0:  Decrease or keep LEDD
#   1:  Small LEDD increase (<= 100)
#   2:  Large LEDD increase (> 100)
# 

ts_df$action_2 <- NA
ts_df$action_2[which(ts_df$LEDD_d <= 0)] <- 0
ts_df$action_2[which((ts_df$LEDD_d <= 100)&(ts_df$LEDD_d > 0))] <- 1
ts_df$action_2[which(ts_df$LEDD_d > 100)] <- 2

#Transition probability
P <- list()
for(i in -1:1){
  tmp <- ts_df %>% filter(action == (i))
  xx <- - matrix(0, nrow=4, ncol=4)
  
  for(j in 0:3){
    for(k in 0:3){
      xx[(j+1),(k+1)] <- (nrow(tmp %>% filter(cluster == j) %>% filter(cluster_n == k)))/ (nrow(tmp %>% filter(cluster == j)))
    }
  }
  P[[i+2]] <- xx
  
}

# (3X4, 4) sized dataframe
P <- rbind(P[[1]], P[[2]], P[[3]])
P <- round(P, 3)
P[,4] <- 1 - P[,1] - P[,2] - P[,3] #make sure they all add up to 1

#export
write.csv(P, file = "P.csv")
