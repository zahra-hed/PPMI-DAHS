#
#  Part 1 of PPMI project
#  :Data analysis & Handling
#

#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("mice)
#install.packages("stringr")
#install.packages("corrplot")
#install.packages("moments")

library(moments)
library(stringr)
library(dplyr)
library(tidyverse)
library(mice)
library(corrplot)

setwd("C://Users//kosai//Desktop//PPMI//data")

#Features -----------------------------------------------------------------------------------------------------------------------------

#call features : MDS-UPDRS 1~4
temp = list.files(pattern="MDS-UPDRS|MDS_UPDRS") 
myfiles = lapply(temp, read.delim)

#pre processing function; creates the necessary columns for future use
preprocessing <- function(inputdata){
  if ("INFODT" %in% colnames(inputdata)){
   inputdata$DATE <- as.numeric(str_sub(inputdata$INFODT, -3,-1))*100 + as.numeric(str_sub(inputdata$INFODT, 1,2))
  }
  inputdata <- inputdata %>% group_by(PATNO) %>% arrange(DATE, .by_group=TRUE)
  ##RECID creation col : NO
  patno_temp = 1
  inputdata[,"NO"] <- c(0)
  for (i in 1:nrow(inputdata)){
    if (i == 1){
      inputdata$NO[1] = patno_temp
    }
    else {
      if (inputdata$PATNO[i] == inputdata$PATNO[i - 1]){
        inputdata$NO[i] = patno_temp
      }
      else {
        patno_temp = patno_temp + 1;
        inputdata$NO[i] = patno_temp
      }
    }
  }
  ##Continuous date creation col : DATE2
  inputdata[,"DATE2"] <- c(0)
  inputdata$DATE <- as.numeric(inputdata$DATE)
  for (i in 1:nrow(inputdata)){
    inputdata$DATE2[i] = floor(inputdata$DATE[i]/100)*100 + 100 * ((inputdata$DATE[i]) - (floor(inputdata$DATE[i]/100)*100)) / 12
  }
  ##KEY creation col : KEY
  inputdata$KEY <- c(0)
  for (i in 1:nrow(inputdata)){
    inputdata$KEY[i] = inputdata$DATE[i] + inputdata$PATNO[i] * 10000
  }
  ##Time interval calculation col : DATE_I
  inputdata$DATE_I <- c(0)
  inputdata$DATE_I[1] = 0.1
  for (i in 2:nrow(inputdata)){
    if (inputdata$NO[i]==inputdata$NO[i-1]){
      inputdata$DATE_I[i]= (inputdata$DATE2[i]-inputdata$DATE2[i-1])
    }
    else {
      inputdata$DATE_I[i]=0.1
    }
  }
  ##Number for datapoint
  inputdata[,"NUM"] <- c(0)
  temp = 1
  inputdata$NUM[1] = temp
  for (i in 2:nrow(inputdata)){
    if (inputdata$NO[i] == inputdata$NO[i - 1]){
      temp = temp + 1
      inputdata$NUM[i] = temp
    }
    else {
      temp = 1
      inputdata$NUM[i] = temp
    }
  }
  inputdata$N_MAX <- c(0)
  inputdata$N_MAX[length(inputdata$DATE)] = (inputdata$NUM[(length(inputdata$DATE))])
  for (i in (nrow(inputdata) - 1):1){
    if (inputdata$NO[i] == inputdata$NO[i + 1]){
      inputdata$N_MAX[i] = inputdata$N_MAX[i + 1]
    }
    else{
      inputdata$N_MAX[i] = inputdata$NUM[i]
    }
  }
  inputdata <- inputdata %>% filter(DATE_I != 0)
  return(inputdata)
}
for (i in 1:length(temp)) assign(temp[i], preprocessing(read.csv(temp[i]))) ; rm(temp); rm(myfiles)

#creating full List- main
main  <- list(
  data.frame(KEY = as.vector(`MDS-UPDRS_Part_I.csv`$KEY), 
             M1 = as.vector(`MDS-UPDRS_Part_I.csv`$NP1RTOT)),
  data.frame(KEY = as.vector(`MDS-UPDRS_Part_I_Patient_Questionnaire.csv`$KEY), 
             M1Q = as.vector(`MDS-UPDRS_Part_I_Patient_Questionnaire.csv`$NP1PTOT)),
  data.frame(KEY = as.vector(`MDS_UPDRS_Part_II__Patient_Questionnaire.csv`$KEY), 
             M2 = as.vector(`MDS_UPDRS_Part_II__Patient_Questionnaire.csv`$NP2PTOT)),
  data.frame(KEY = as.vector(`MDS-UPDRS_Part_III.csv`$KEY), 
             M3 = as.vector(`MDS-UPDRS_Part_III.csv`$NP3TOT), 
             HNY = as.integer(as.vector(`MDS-UPDRS_Part_III.csv`$NHY)), na.rm=TRUE),
  data.frame(KEY = as.vector(`MDS-UPDRS_Part_IV__Motor_Complications.csv`$KEY), 
             M4 = as.vector(`MDS-UPDRS_Part_IV__Motor_Complications.csv`$NP4TOT))
  )

#Reduce list to data frame
main <- main %>% reduce(full_join, by = "KEY"); rm(`MDS-UPDRS_Part_I.csv`,
                                                   `MDS-UPDRS_Part_I_Patient_Questionnaire.csv`, 
                                                   `MDS_UPDRS_Part_II__Patient_Questionnaire.csv`, 
                                                   `MDS-UPDRS_Part_III.csv`, 
                                                   `MDS-UPDRS_Part_IV__Motor_Complications.csv`)
main[,c("PATNO","DATE")] <- c(0)
for (i in 1:nrow(main)){
  main$PATNO[i] = as.integer(floor(main$KEY[i]/10000))
  main$DATE[i] = as.integer(main$KEY[i] %% 10000)
}

#age adjustment
for(i in 2:nrow(main)){
  if(main$NUM[i] != 1)
  main$age[i] <- main$age[i-1] + main$DATE_I[i] / 100
}; rm(i)
main$age <- round(main$age,1)

#final data frame
main <- preprocessing(main) 


#bio info -----------------------------------------------------------------------------------------------------------------------------

#call patient information: death, age, concohort, education, sex, diagnosis history
temp = list.files(pattern="Conclusion.csv|Demographics.csv|Participant_Status.csv|Socio_Economics.csv|Conclusion.csv|Diagnosis.csv")
myfiles = lapply(temp, read.delim)
for (i in 1:length(temp)) assign(temp[i],read.csv(temp[i])) ; rm(temp) ; rm(myfiles)

#custom max function for max(NAs)
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

#calling for the wanted columns
info <- list(
  distinct(data.frame(PATNO = as.vector(Participant_Status.csv$PATNO),
                      CONCOHORT = as.vector(Participant_Status.csv$CONCOHORT), 
                      age = as.vector(Participant_Status.csv$ENROLL_AGE))),
  distinct(data.frame(PATNO = as.vector(Demographics.csv$PATNO), 
                      sex = as.vector(Demographics.csv$SEX))),
  data.frame(distinct(data.frame(PATNO = as.vector(Socio_Economics.csv$PATNO), 
                                 edu = as.vector(Socio_Economics.csv$EDUCYRS))) %>% group_by(PATNO) %>% summarise(edu = my.max(edu))),
  distinct(data.frame(PATNO = as.vector(Conclusion.csv$PATNO), 
                      death = as.vector(Conclusion.csv$WDRSN))),
  distinct(data.frame(PATNO = Diagnosis.csv$PATNO, 
                      diag = Diagnosis.csv$PDDXDT))
  )
info <- info %>% reduce(full_join, by = "PATNO"); rm(Conclusion.csv, 
                                                     Demographics.csv, 
                                                     Diagnosis.csv, 
                                                     Participant_Status.csv, 
                                                     Socio_Economics.csv)

#a final data frame : main
main <- left_join(main,info); rm(info)

#change death to a binary var
main$death[which(main$death!=3)] = 0; main$death[which(is.na(main$death))] = 0; main$death[which(main$death == 3)] = 1

#only CONCOHORT == 1
main <- main %>% filter(CONCOHORT==1); main$CONCOHORT <- NULL

#add columns for year since first visit / first diagnosis
main$year <- c(0)
main$diag <- round((main$DATE2 - as.numeric(str_sub(main$diag[1], -3,-1))*100 + as.numeric(str_sub(main$diag[1], 1,2))*100/12)/100,2)
for (i in 1:nrow(main)){
  if (main$NUM[i] == 1){
    main$year[i] <- main$DATE2[i]
  }
  else{
    main$year[i] <- main$year[i-1]
  }
}
main$year <- round((main$DATE2 - main$year)/100,2)
main$na.rm <- NULL

#Only patients with 10 or more visits
main <- preprocessing(main %>% filter(N_MAX > 9))


#imputations -------------------------------------------------------------------------------------------------------------------------

#various imputation methods & performance comparison -> only the best one runs for now

#imputes input data frame with the nearest visit from same patient
samepat.imp <- function(df, patlist){
  full <- data.frame(df,patlist)
  for(i in 1:ncol(df)){
    for(j in 1:nrow(df)){
      if(is.na(df[j,i])){
        temp <- full[which(!(is.na(full[names(full)[i]]))),] %>% filter(PATNO==full$PATNO[j])
        if(nrow(temp)==0){
          temp <- full
        }
        temp$DATE2 <- abs(temp$DATE2 - full$DATE2[j])
        temp <- temp %>% arrange(DATE2)
        df[j,i] <- temp[1,i]
      }
    }
  }
  return(df)
}
#unused lines
if(F)'
#impute input data frame with mice & My rule
my.mice <- function(df, k = 5){
  imp <- mice(df, m=k, print=F)
  result <- data.frame(matrix(0, nrow=2, ncol=k))
  colnames(result) <- c(1:k); rownames(result) <- c("mean", "sd")
  for (i in 1:k){
    for (j in 1:ncol(df)){
      result["mean",i] <- result["mean",i] + (
        (mean(unlist(df[j]), na.rm = T) - mean(unlist(complete(imp,i)[j])))^2
      ) / (mean(unlist(df[j]), na.rm = T))
      result["sd",i] <- result["sd",i] + (
        (sd(unlist(df[j]), na.rm = T) - sd(unlist(complete(imp,i)[j])))^2
      ) / (sd(unlist(df[j]), na.rm = T))
    }
  }
  return(complete(imp,which.max(result[1,])))
}

#imputes input data frame with mean
mean.imp <- function(df){
  for (col in colnames(df)) {
    if (any(is.na(df[[col]]))) {
      col_mean <- mean(df[[col]], na.rm = TRUE)
      df[[col]][is.na(df[[col]])] <- col_mean
    }
  }
  return(df)
}

#masks input data
mask <- function(df, ratio = NULL){
  if (is.null(ratio)){
    ratio = rep(0.2,ncol(df))
  }
  for (i in 1:ncol(df)){
    df[sample(1:nrow(df),round(nrow(df)*ratio[i])),i] <- NA
  }
  return(round(df))
}


#create a No-NA data set for validation
main$NAR <- c(0)
for (i in 1:nrow(main)){
  main$NAR[i] <- sum(is.na(main[i,2:7]))
}; rm(i)
NO_NA <- main %>% filter(NAR == 0)

perf.imp <- function(df, iter=25){
  #create a data frame for comparing different methods
  imp.result <- data.frame(matrix(0, nrow=3,ncol=2))
  rownames(imp.result) <- c("Mean","Mice","Hot-Deck"); colnames(imp.result) <- c("SAE", "SSE")
  
  
  #iterate for different masks
  for (i in 1:iter){
    masked <- mask(df[2:7], c(0.01, 0.008, 0.001, 0.103, 0.283, 0.046))
    mice.res <- my.mice(masked, k=5)
    samepat.res <- samepat.imp(masked, df[c("PATNO","DATE2")])
    mean.result <- mean.imp(masked)
    
    #calculates the SAE
    imp.result["Mice","SAE"] <- imp.result["Mice","SAE"] + sum(abs(mice.res-df[2:7]))/sum(is.na(masked))
    imp.result["Hot-Deck","SAE"] <- imp.result["Hot-Deck","SAE"] + sum(abs(samepat.res-df[2:7]), na.rm=T)/sum(is.na(masked))
    imp.result["Mean","SAE"] <- imp.result["Mean","SAE"] + sum(abs(mean.result-df[2:7]))/sum(is.na(masked))
    
    #calculates SSE
    imp.result["Mice","SSE"] <- imp.result["Mice","SSE"] + sqrt(sum((mice.res-df[2:7])^2)/sum(is.na(masked)))
    imp.result["Hot-Deck","SSE"] <- imp.result["Hot-Deck","SSE"] + sqrt(sum((samepat.res-df[2:7])^2, na.rm=T)/sum(is.na(masked)))
    imp.result["Mean","SSE"] <- imp.result["Mean","SSE"] + sqrt(sum((mean.result-df[2:7])^2)/sum(is.na(masked)))
  }
  imp.result <- imp.result / iter
  return(imp.result)
}
perf.imp(NO_NA, 10)


#result for performance 
              SAE      SSE
Mean     2.954910 6.543230
Mice     3.364143 7.655886
Hot-Deck 2.177763 5.593528
'

#impute using the best one = hotdeck
main[2:7] <- samepat.imp(main[2:7], main[c("PATNO","DATE2")])
main$M1 <- main$M1 + main$M1Q; main$M1Q <- NULL

#Compare statistics before & after imputation
if(F)'
boxplot(main[c("M1","M2","M3","M4")],
        col="light blue")
NO_NA$M1 <- NO_NA$M1 + NO_NA$M1Q
NO_NA$M1Q <- NULL
boxplot(NO_NA[c("M1","M2","M3","M4")],
        col="light blue")
'

#drop the 33 datasets
main <- main %>% filter(!(is.na(M4)))

hist(unlist(x),breaks=c(0,1,2,3,4,5))

#medication ---------------------------------------------------------------------------------------------------------------------------

#read LEDD data
medication <- read.csv(file="LEDD_Concomitant_Medication_Log.csv")


#define start, stop & Change unfinished medication to 2023/12
if(F)'
medication[c("start","stop")] <- c(0)
medication$stop[which(medication$stop == "#VALUE!")] <- 2312
medication$stop <- as.integer(medication$stop)


#fill the missing LEDD values
for (i in 1:6856){    
  if(medication$LEDD[i] == ""){
    medication$LEDD[i] = medication$LEDDSTRMG[i] * medication$LEDDOSFRQ[i] * medication$LEDDOSE[i]
  }
}
'
#group by types : LEVODOPA, DOPAMINE AGONISTS, OTHERS
medication$type <- c(0)
for(i in 1:6856){    
  if(sum(str_detect(medication$LEDTRT[i], 
                    c("LEVOD", "Levodopa", "Madopar", "MADOPAR", "Sinemet", "SINEMET", "Duodopa", "DUODOPA", "CARBIDOPA", "SINEMENT",
                      "SINAMET", "CARB/LEVO", "SINEMENT", "Carbidopa", "LEVADOPA", "NACOM", "LEVOCOMP", "Rytary", "RYTARY", 
                      "CO-CARLEDOPA","L-DOPA", "CARBIDOPA","LEVOPAR", "DOPA PUMP", "DOPICAR", "ISICOM", "CARELDOPA", "Inbrija", 
                      "Stalevo", "INBRIJA", "STALEVO", "LEVIDOPA", "RYTRAY", "LEDOPODO BENSERAZIDE")
                    ))>0){
    medication$type[i] <- 1
  }
  else if(sum(str_detect(medication$LEDTRT[i], 
                         c("PRAMIPEXOLE","Pramipexole","Ropin","ROPIN","Mirapexin", "MIRAPEXIN", "Requip", "REQUIP", "Rotigotine", 
                           "ROTIGOTIN", "Neupro", "NEUPRO", "APOMORPHINE", "Apomorphine", "MIRAPEX", "PRAMIPREXOLE", "NEURPRO", 
                           "NUPRO", "PRAMIPLEXOLE", "PRAMIPEXOL", "ROPIRINOL","ROPRINIROLE", "CLARIUM RETARD", "SIFROL RETARD", 
                           "SIFROL", "Piribedil","PIRIBEDIL", "CLARIUM", "NUEPRO","ROTIGINE", "APOMORFIN","APHOORPHINE", "NEUROPATCH", 
                           "PRAMPEXOLE", "APOKYN")
                         ))>0){
    medication$type[i] <- 2
  }
  else{
    medication$type[i] <- 3
  }
}

#apply medication type to main data frame
main[c("medtype1", "medtype2", "medtype3", "medtype", "LEDD", "LEDDmult")] <- c(0)
for (i in 1:nrow(main)){
  med.t <- medication %>% filter(PATNO == main$PATNO[i])
  if(nrow(med.t) != 0){
    for (j in 1:nrow(med.t)){
      if ((med.t$start[j] <= main$DATE[i]) & (med.t$stop[j] >= main$DATE[i])){
        if(med.t$type[j] == 1){
          main$medtype1[i] <- 1
        }
        else if(med.t$type[j] == 2){
          main$medtype2[i] <- 1
        }
        else {
          main$medtype3[i] <- 1
        }
      }
    }
  }
  rm(med.t)
}; rm(i,j)


#fill in LEDD into main data frame
for (i in 1:nrow(main)){
  med.t <- medication %>% filter(PATNO == main$PATNO[i])
  if(nrow(med.t)!= 0){
    for (j in 1:nrow(med.t)){
      if ((med.t$start[j] <= main$DATE[i]) & (med.t$stop[j] >= main$DATE[i])){
        if (med.t$LEDD[j] == "LD x 0.33"){
          main$LEDDmult[i] <- main$LEDDmult[i] + 1
        }
        else{
          main$LEDD[i] <- sum(main$LEDD[i], as.numeric(med.t$LEDD[j]), na.rm=TRUE)
        }
      }
    }
  }
 rm(med.t)
}; rm(i,j)

main$medtype <- main$medtype1 + 2*main$medtype2 + 4*main$medtype3
main$LEDD <- main$LEDD + 0.33*main$LEDD*main$LEDDmult
main$LEDDmult <- NULL
write.csv(main, file="main.csv")

