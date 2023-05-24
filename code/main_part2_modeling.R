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

#LEDD discretization Q3 = 700, 1100, 1490
main[c("LEDD.d","LEDD.n.d")] <- c(0)
main$LEDD.d[which(main$medcount == 1)] <- ceiling(main$LEDD[which(main$medcount == 1)]/600)
main$LEDD.d[which(main$medcount == 2)] <- ceiling(main$LEDD[which(main$medcount == 2)]/1100)
main$LEDD.d[which(main$medcount == 3)] <- ceiling(main$LEDD[which(main$medcount == 3)]/1490)
main$LEDD.d[which(main$LEDD.d>2)] <- 2

#needs fixing - but does not really matter
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


##absorbing state definition
#checking for absorbing states

patno <- unique(main$PATNO)
main.list <- list()
for (i in 1:length(patno)){
  main.list[[i]] <- main %>% filter(PATNO == patno[i])
}

crit <- function(condition, value){
  return(value %in% condition)
}
count_consecutive_trues <- function(x) {
  count <- 0
  memory <- c(0)
  idx <- c(0)
  for (i in seq_along(x)) {
    if (x[i]) {
      count <- count + 1
    } else {
      memory <- append(memory, count)
      idx <- append(idx, i-1)
      count <- 0
    }
  }
  memory <- append(memory, count)
  idx <- append(idx, i)
  result <- data.frame(memory= memory, idx = idx)
  result <- result %>% filter(memory == max(memory))
  return(c(result[1,]))
}
conseq.table <- function(c, lim1, lim2){
  ress <- data.frame(matrix(ncol=4, nrow = 1))
  for(count in c(0:20)){
    TT <- 0; TF <- 0; TF2 <- 0; TF3 <- 0
    for(i in 1:length(patno)){
      temp <- main.list[[i]]
      prob.t <- sapply(temp$cluster, function(x) crit(c, x))
      if(count_consecutive_trues(prob.t)$memory > count){
        TT <- TT + 1
        temp2 <- temp$cluster[count_consecutive_trues(prob.t)$idx:nrow(temp)]
        if(sum(crit(lim,temp2)) > 0){
          TF <- TF + 1
        }
        if(sum(crit(lim2,temp2)) > 0){
          TF2 <- TF2 + 1
        }
        if(sum(crit(lim2,temp2)) > 1){
          TF3 <- TF3 + 1
        }
      }
    }
    ress[count+1,] <- c(TT, paste0(TF,"(",round(100*TF/TT,1),"%)"), paste0(TF2,"(",round(100*TF2/TT,1),"%)"), paste0(TF3,"(",round(100*TF3/TT,1),"%)"))
  }
  return(ress)
}


bad2good <- conseq.table(c(1),c(3,2),c(3))
write.csv(bad2good, file="b2g.csv")

if(F)'
plot(bad2good$X3, type ="b", col = "red")
abline(v = 7, lty = 2)
abline(v = 16, lty = 2)
'



#States
main$medc <- main$medcount * 2 + main$LEDD.d - 1
main$medc[which(main$medcount == 0)] <- c(1)
main$state <- main$medc + (3-main$cluster) * 7


#add absorbing states after 16+good state => good absorbing /// after 7+ bad state|Death => bad absorbing
main.list <- list()
for (i in 1:length(patno)){
  main.list[[i]] <- main %>% filter(PATNO == patno[i])
}

#adding absorbing states
for (i in 1:length(patno)){
  for (j in 1:nrow(main.list[[i]])){
    b.list <- sapply(main.list[[i]]$cluster, function(x) crit(1, x))
    if(count_consecutive_trues(b.list)$memory > 6){
      main.list[[i]]$state[count_consecutive_trues(b.list)$idx:nrow(main.list[[i]])] <- c(22)
    }
  }
}

#adding death
for(i in 1:length(patno)){
  if(sum(main.list[[i]]$death) > 0){
    main.list[[i]]$state[nrow(main.list[[i]])] <- 22
  }
}

main <- main.list[[1]]
for(i in 2:length(patno)){
  main <- rbind(main, main.list[[i]])
}



#check the properties of states
med.s2 <- main %>% group_by(state) %>% summarize(M1= mean(M1), M2=mean(M2), M3=mean(M3), M4=mean(M4))
med.s2$freq <- c(0)
for(i in 1:22){
  med.s2$freq[i] <- paste0(nrow(main %>% filter(state == i)),"(",round(nrow(main %>% filter(state == i))*100/8112,2),"%)")
}

#next states
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
colnames(med.t) <- c(-3,-2,-1,0,1,2,3); rownames(med.t) <- c(-2,-1,0,1,2)
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
for(i in 1:nrow(main)){
  if(main$N_MAX[i] != main$NUM[i]){
    if((main$medcount.n[i] == 0)&(main$LEDD.n.d[i] == 0)){
      main$action[i] <- 0
    }
    else if((main$medcount.n[i] == 0)&(main$LEDD.n.d[i] > 0)){
      main$action[i] <- 1
    }
    else if((main$medcount.n[i] == 0)&(main$LEDD.n.d[i] < 0)){
      main$action[i] <- 2
    }
    else if((main$medtype1.n[i] == 1)&(main$medtype2.n[i] == 0)&(main$medtype3.n[i] == 0)){
      main$action[i] <- 3
    }
    else if((main$medtype1.n[i] == 0)&(main$medtype2.n[i] == 1)&(main$medtype3.n[i] == 0)){
      main$action[i] <- 4
    }
    else if((main$medtype1.n[i] == 0)&(main$medtype2.n[i] == 0)&(main$medtype3.n[i] == 1)){
      main$action[i] <- 5
    }
    else if((main$medcount.n[i] == 2)|(main$medcount.n[i] == 3)){
      main$action[i] <- 6
    }
    else if(main$medcount.n[i] == -1){
      main$action[i] <- 7
    }
    else if(main$medcount.n[i] < -1){
      main$action[i] <- 8
    }
  }
}

if(F)'
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
'
states <- main[c("state", "M1","M2","M3","M4","medc","LEDD.d")]

states$M1 <- states$M1/max(states$M1)
states$M2 <- states$M2/max(states$M2)
states$M3 <- states$M3/max(states$M3)
states$M4 <- states$M4/max(states$M4)

states <- states %>% group_by(state) %>% summarize(M1=mean(M1), M2=mean(M2),M3=mean(M3), M4=mean(M4), medcount=mean(medcount), LEDD.d = mean(LEDD.d))



#creating state-action-state table
actions <- list()
for (i in 1:9) {
  df <- data.frame(matrix(0, nrow = 22, ncol = 22))
  actions[[i]] <- df
}

for (i in 1:9){
  temp <- main %>% filter(action == i) %>% filter(NUM != N_MAX)
  for (j in 1:22){
    for (k in 1:22){
      actions[[i]][j,k] <- nrow(temp %>% filter(state==j) %>% filter(state.n==k) %>% filter(action == i))/max(nrow(temp %>% filter(state==j) %>% filter(action == i)),1)
    }
  }
}



#function, input : action, state i, output : possible states
transPr <- function(a, i){
  temp <- actions[[a]][i,]
  return(list(pr=temp[temp!=0],id = which(temp!=0)))
}

D <- matrix(0, nrow = 23, ncol = 23)
P <- list()
for (i in 1:5){
  P[[i]] <- as.matrix(actions[[i]])  
}

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
a=0.05; b=0.05

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
  reward <- data.frame(matrix(0,nrow=23,ncol=23))
  for(i in 1:23){
    for(j in 1:23){
      reward[i,j] = sqrt(states$M1[i]^2 + states$M2[i]^2 + states$M3[i]^2 + states$M4[j]^2) - sqrt(states$M1[j]^2 + states$M2[j]^2 + states$M3[j]^2 + states$M4[j]^2) +
        a*(states$medcount[i] - states$medcount[j]) + b*(states$LEDD.d[i] - states$LEDD.d[j])
    }
  }
  reward <- reward * 100
  reward[1,] <- 80
  reward[,1] <- 80
  reward[,22] <- -70
  reward[22,] <- -70
  
  
  R <- matrix(0, ncol = 5, nrow = 23)
  for(i in 2:22){
    for(j in 1:5){
      R[i,j] <- AvgReward(j,i, reward)
    }
  }
  return(R)
}

x <- hypers(0.01,0.01)




#MDP solving
res <- data.frame(matrix(0,nrow = 23,ncol = 0))
RS <- list()
aa <- 1


#changing over hyperparameters
for (h in c(0, 0.03, 0.05)){
  for (hh in c(0, 0.03, 0.05)){
    w <- binaryMDPWriter("PPMI")
    w$setWeights(c("Duration", "Net reward"))
    R <- hypers(h, hh)
    RS[[aa]] <- R
    w$process(P,R,D)
    w$closeWriter()
    MDP <- loadMDP("PPMI")
    runPolicyIteDiscount(MDP, "Net reward", "Duration", discountFactor = 0.9, maxIte = 1000)
    x <- getPolicy(MDP)
    res[paste0(h,",",hh)] = x$actionLabel
    aa <- aa+1
  }
}

write.csv(res,file="policy.csv")
for (i in 1:9){
  write.csv(RS[[i]], file=paste(i,".csv"))
}

res <- mutate_all(res, function(x) as.numeric(x))

pheatmap(as.numeric(res), cluster_rows = F, cluster_cols=F)
save.image(file='myEnvironment.RData')
load('myEnvironment.RData')





#MDP SOLVING PART 2
install.packages("MDPtoolbox")
library(MDPtoolbox)

MDPsolv <- function(a,b,r,p){
  reward <- data.frame(matrix(0,nrow=22,ncol=22))
  for(i in 1:22){
    for(j in 1:22){
      reward[i,j] = sqrt(states$M1[i]^2 + states$M2[i]^2 + states$M3[i]^2 + states$M4[j]^2) - sqrt(states$M1[j]^2 + states$M2[j]^2 + states$M3[j]^2 + states$M4[j]^2) +
        a*(states$medcount[i] - states$medcount[j]) + b*(states$LEDD.d[i] - states$LEDD.d[j])
    }
  }
  reward <- reward * 100
  reward[,22] <- p
  reward[22,] <- p
  
  #Probability
  P <- array(0, c(22,22,9))
  for(i in 1:9){
    P[,,i] <- as.array(as.matrix(actions[[i]]))
  }
  R <- array(0, c(22,22,9)) #Reward
  for(i in 1:9){
    temp <- reward
    for(k in 1:22){
      for(j in 1:22){
        if(actions[[i]][k,j] == 0){
          temp[k,j] <- 0
        }
      }
    }
    R[,,i] <- as.array(as.matrix(temp))
  }
  return(mdp_policy_iteration(P,R,r)$policy)
}
MDPsolv(0.01, 0.01, 0.9, 100)
result <- data.frame(matrix(0, ncol=22, nrow=1))

result[1,] <- MDPsolv(0.01, 0.01, 0.9, 100)
result[2,] <- MDPsolv(0.03, 0.03, 0.9,100)
result[3,] <- MDPsolv(0.05, 0.05, 0.9,100)
result[4,] <- MDPsolv(0.1, 0.1, 0.9,100)
result[5,] <- MDPsolv(0.03, 0.03, 0.99,100)
result[6,] <- MDPsolv(0.1, 0.01, 0.9,100)
result[7,] <- MDPsolv(0.01, 0.1, 0.9,100)

pheatmap(result[,2:22], cluster_rows = F, cluster_cols = F)


##빈도수 개산
freqs <- list()
for (i in 1:9) {
  df <- data.frame(matrix(0, nrow = 22, ncol = 22))
  freqs[[i]] <- df
}

for (i in 1:9){
  temp <- main %>% filter(action == i) %>% filter(NUM != N_MAX)
  for (j in 1:22){
    for (k in 1:22){
      freqs[[i]][j,k] <- nrow(temp %>% filter(state==j) %>% filter(state.n==k) %>% filter(action == i))
    }
  }
}
pal <- colorRampPalette(c("white","black"))(100)
pheatmap(freqs[[6]], cluster_rows = F, cluster_cols = F, color = pal)

