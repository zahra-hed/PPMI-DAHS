#
#  Part 2 of PPMI project
#  :Defining & Solving the Model
#

#install.packages("dplyr")
install.packages("cluster")
install.packages("pheatmap")
install.packages("devtools")

library(devtools)
install_github("relund/mdp")

library(dplyr)
library(factoextra)
library(cluster)
library(pheatmap)
library(MDP2)

#read main data frame
main <- read.csv("main.csv")


#K-means clustering : state ----------------------------------------------------

set.seed(8409)
scaled.vars <- scale(main[c("M1","M2","M3","M4")])

#optimal # of clusters in k-means
if(F)'
#elbow method
fviz_nbclust(scaled.vars, kmeans, method = "wss")

#silhouette method
fviz_nbclust(scaled.vars, kmeans, method = "silhouette")

#gap statistic method
gap_stat <- clusGap(scaled.vars, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
'

#K-means with k = 3 
KM <- kmeans(scaled.vars, 3)
main$cluster <- KM$cluster

#group by clusters
if(F)'
cluster.summary <- main %>% group_by(cluster) %>% summarise(
  M1 = mean(M1), M2 = mean(M2), M3 = mean(M3), M4 = mean(M4), HNY = mean(HNY, na.rm=T),
  age=mean(age),diag=mean(diag),year=mean(year))
cluster.summary <- round(cluster.summary,1)


#PCA visualization
fviz_cluster(KM, data = data.frame(scaled.vars), geom = "point", pointsize = 0.8)
'
rm(scaled.vars, KM)
#Action Definition -------------------------------------------------------------

#next cluster & LEDD difference & action difference
main[c("cluster.n","LEDD.n","LEDD.n.bin","medtype.n","medtype1.n","medtype2.n","medtype3.n", "medcount.n")] <- c(0)
for(i in 1:(nrow(main)-1)){
  main$cluster.n[i] <- main$cluster[i+1]
  main$LEDD.n[i] <- main$LEDD[i+1] - main$LEDD[i]
  main$LEDD.n.bin[i] <- main$LEDD.n[i]/max(abs(main$LEDD.n[i]),1)
  main$medtype1.n[i] <- main$medtype1[i+1] - main$medtype1[i]
  main$medtype2.n[i] <- main$medtype2[i+1] - main$medtype2[i]
  main$medtype3.n[i] <- main$medtype3[i+1] - main$medtype3[i]
  main$medcount.n[i] <- main$medcount[i+1] - main$medcount[i]
  main$medtype.n[i] <- main$medtype1.n[i]+1 + (main$medtype2.n[i]+1)*3 + (main$medtype3.n[i]+1)*9
}; rm(i)
main[which(main$N_MAX==main$NUM), c("cluster.n","LEDD.n","LEDD.n.bin","medtype.n","medtype1.n","medtype2.n","medtype3.n","medcount.n")] <- NA
main$LEDD.n.bin[which((main$LEDD.n.bin>0)&(main$LEDD.n.bin<1))] <- 1
main$LEDD.n.bin[which((main$LEDD.n.bin<0)&(main$LEDD.n.bin>-1))] <- -1

#LEDD discretization 0, 0~500, 500~
main[c("LEDD.d","LEDD.n.d")] <- c(0)
main$LEDD.d <- ceiling(main$LEDD/500)
main$LEDD.d[which(main$LEDD.d>2)] <- 2

main$LEDD.n.d <- (main$LEDD.n.bin) * ceiling(abs(main$LEDD.n)/500)
main$LEDD.n.d[which(main$LEDD.n.d > 2)] <- 2
main$LEDD.n.d[which(main$LEDD.n.d < -2)] <- -2

#LEDD - medication count 
med.s <- data.frame(matrix(0, nrow=3, ncol=4))
for(i in 0:2){
  for(j in 0:3){
    med.s[i+1,j+1] <-nrow(main %>% filter(LEDD.d == i) %>% filter(medcount == j))
  }
}

#States
main$medc <- c(0)
for(i in 1:nrow(main)){
  if(main$medcount[i] == 0){
    main$medc[i] <- 1
  }
  else if((main$medcount[i] == 1)&(main$LEDD.d[i] == 1)){
    main$medc[i] <- 2
  }
  else if((main$medcount[i] == 1)&(main$LEDD.d[i] == 2)){
    main$medc[i] <- 3
  }
  else if((main$medcount[i] == 3)&(main$LEDD.d[i] %in% c(1,2))){
    main$medc[i] <- 5
  }
  else if((main$medcount[i] %in% c(1,2))&(main$LEDD.d[i] == 2)){
    main$medc[i] <- 4
  }
}

main$state <- main$medc + (main$cluster-1) * 5

med.s2 <- main %>% group_by(state) %>% summarize(M1= mean(M1), M2=mean(M2), M3=mean(M3), M4=mean(M4))


main$state.n <- c(0)
for(i in 1:(nrow(main)-1)){
  main$state.n[i] <- main$state[i+1]
}; rm(i)
main[which(main$N_MAX==main$NUM), c("state.n")] <- NA 
#action table

#LEDD - medication count  movement check
med.t <- data.frame(matrix(0, nrow=5, ncol=7))
for(i in -2:2){
  for(j in -3:3){
    med.t[i+3,j+4] <-nrow(main %>% filter(LEDD.n.d == i) %>% filter(medcount.n == j))
  }
}
rownames(med.t) <- c(-3,-2,-1,0,1,2,3); colnames(med.t) <- c(-1,0,1)
if(F)'
What is this????
action.t <- data.frame(matrix(nrow=8, ncol=3))

for(i in 1:8){
  for(j in 1:3){
    action.t[i,j] <- nrow(main %>% filter(medtype.n == i) %>% filter(LEDD.n.bin==j))
  }
}

write.csv(main, file="main.csv")
'
main$action <- NA
for(i in which(!is.na(main$medcount.n))){
  if((main$medcount.n[i] < 0)&(main$LEDD.n.d[i] < 0)){
    main$action[i] <- 1
  }
  else if((main$medcount.n[i] >= 0)&(main$LEDD.n.d[i] < 0)){
    main$action[i] <- 2
  }
  else if(main$LEDD.n.d[i] == 0){
    main$action[i] <- 3
  }
  else if((main$medcount.n[i] <= 0)&(main$LEDD.n.d[i] > 0)){
    main$action[i] <- 4
  }
  else if((main$medcount.n[i] > 0)&(main$LEDD.n.d[i] > 0)){
    main$action[i] <- 5
  }
}
states <- main[c("state", "M1","M2","M3","M4","medcount","LEDD.d")]
states$M1 <- states$M1/max(states$M1)
states$M2 <- states$M2/max(states$M2)
states$M3 <- states$M3/max(states$M3)
states$M4 <- states$M4/max(states$M4)

states <- states %>% group_by(state) %>% summarize(M1=mean(M1), M2=mean(M2),M3=mean(M3), M4=mean(M4), medcount=mean(medcount), LEDD.d = mean(LEDD.d))



#creating state-action-state table
actions <- list()
for (i in 1:5) {
  df <- data.frame(matrix(0, nrow = 15, ncol = 15))
  actions[[i]] <- df
}

for (i in 1:5){
  temp <- main %>% filter(action == i) %>% filter(NUM != N_MAX)
  for (j in 1:15){
    for (k in 1:15)
      actions[[i]][j,k] <- nrow(temp %>% filter(state==j) %>% filter(state.n==k))/max(nrow(temp %>% filter(state==j)),1)
    }
}



#function, input : action, state i, output : possible states
transPr <- function(a, i){
  temp <- actions[[a]][i]
  which(temp != 0)
  temp[temp != 0]
  return(list(pr=temp[temp!=0],id = which(temp!=0)))
}

D <- matrix(0, nrow = 15, ncol = 15)
P <- list()
for (i in 1:5){
  P[[i]] <- as.matrix(actions[[i]])  
}
save.image(file='myEnvironment.RData')

if(F)'
w <- binaryMDPWriter("PPMI")
w$setWeights(c("Duration", "Net reward"))
w$process()
  w$stage()
    for(ii in 1:24){
      w$state(label = ii)
        for(jj in 1:5){
          if(sum(actions[[jj]][ii]) != 0){
            dat <- transPr(jj, ii)
            w$action(label = jj, weights = c(), pr = dat$pr, id = dat$id, end =T)
          }
        }
      w$endState()
    }
  w$endStage()
w$endProcess()
w$closeWriter()
'
#REWARD NEEDS FIXING REWARD FUNCTION
a=0.01; b=0.01
AvgReward <- function(a, i, rew){
  r <- 0
  temp <- transPr(a,i)
  if (length(temp$pr) != 0){
    for(ii in 1:length(temp$pr)){
      r <- r + temp$pr[ii] * rew[i,temp$id[ii]]
    }
  }
  return(r)
}
hypers <- function(a,b){
  reward <- data.frame(matrix(0,nrow=15,ncol=15))
  for(i in 1:15){
    for(j in 1:15){
      reward[i,j] = sqrt(states$M1[i]^2 + states$M2[i]^2 + states$M3[i]^2 + states$M4[j]^2) - sqrt(states$M1[j]^2 + states$M2[j]^2 + states$M3[j]^2 + states$M4[j]^2) +
        a*(states$medcount[i] - states$medcount[j]) + b*(states$LEDD.d[i] - states$LEDD.d[j])
    }
  }
  reward <- reward * 100
  R <- matrix(0, ncol = 5, nrow = 15)
  for(i in 1:15){
    for(j in 1:5){
      R[i,j] <- AvgReward(j,i, reward)
    }
  }
  return(R)
}
x <- hypers(0.01,0.01)

#MDP solving
res <- data.frame(matrix(0,nrow = 15,ncol = 0))
RS <- list()
aa <- 1


#changing over hyperparameters
for (h in c(0.01, 0.1, 0.5)){
  for (hh in c(0.01, 0.1, 0.5)){
    w <- binaryMDPWriter("PPMI")
    w$setWeights(c("Duration", "Net reward"))
    R <- hypers(h, hh)
    RS[[aa]] <- R
    w$process(P,R,D)
    w$closeWriter()
    MDP <- loadMDP("PPMI")
    runPolicyIteDiscount(MDP, "Net reward", "Duration", discountFactor = 0.1, maxIte = 100)
    x <- getPolicy(MDP)
    res[paste(h,",",hh)] = x$actionLabel
    aa <- aa+1
  }
}

write.csv(res,file="policy.csv")
for (i in 1:9){
  write.csv(RS[[i]], file=paste(i,".csv"))
}


load('myEnvironment.RData')
