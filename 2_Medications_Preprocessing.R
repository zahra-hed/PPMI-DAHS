#
#
#   2. Medication Preprocessing
#   
#   -> Takes main_df, main_patients from [1_Feature_Preprocessing]
#
#   
#   **3 major types of medications exists for Parkinson
#       Levodopa
#       Dopamine Agonists
#       Others
#
#   
#   Returns a dataframe consisting medication related columns
#       med1 ~ 3 : whether each type is used
#       medcount : amount of medications used
#       DATE : YY, MM for every possible year-month
#
#

library(dplyr)
library(stringr)


setwd("C:/Users/kosai/Desktop/school/ppmi/dataset")

#read csv
medication <- read.csv('medication/LEDD_Concomitant_Medication_Log_11Jul2023.csv')
main_df <- read.csv('main_df.csv')
patients <- read.csv('patients.csv')

#Possible ways of referring each medication types
levodopa <- c("LEVOD", "Levodopa", "Madopar", "MADOPAR", "Sinemet", "SINEMET", "Duodopa", "DUODOPA", "CARBIDOPA", "SINEMENT",
              "SINAMET", "CARB/LEVO", "SINEMENT", "Carbidopa", "LEVADOPA", "NACOM", "LEVOCOMP", "Rytary", "RYTARY", 
              "CO-CARLEDOPA","L-DOPA", "CARBIDOPA","LEVOPAR", "DOPA PUMP", "DOPICAR", "ISICOM", "CARELDOPA", "Inbrija", 
              "Stalevo", "INBRIJA", "STALEVO", "LEVIDOPA", "RYTRAY", "LEDOPODO BENSERAZIDE")

dopamine_agonists <- c("PRAMIPEXOLE","Pramipexole","Ropin","ROPIN","Mirapexin", "MIRAPEXIN", "Requip", "REQUIP", "Rotigotine", 
                       "ROTIGOTIN", "Neupro", "NEUPRO", "APOMORPHINE", "Apomorphine", "MIRAPEX", "PRAMIPREXOLE", "NEURPRO", 
                       "NUPRO", "PRAMIPLEXOLE", "PRAMIPEXOL", "ROPIRINOL","ROPRINIROLE", "CLARIUM RETARD", "SIFROL RETARD", 
                       "SIFROL", "Piribedil","PIRIBEDIL", "CLARIUM", "NUEPRO","ROTIGINE", "APOMORFIN","APHOORPHINE", "NEUROPATCH", 
                       "PRAMPEXOLE", "APOKYN")

#
# add columns to medications 
#   med
#     1 : levodopa
#     2 : dopamine agonists
#     3 : others
#   S_Y : start YY
#   S_M : start MM
#   E_Y : end YY
#   E_M : end MM
#

medication[c('med','S_Y','S_M','E_Y','E_M')] <- c(0)
medication$STOPDT[which(medication$STOPDT == "")] <- "07/2023"

for(i in 1:nrow(medication)){
  if(sum(str_detect(medication$LEDTRT[i], levodopa)) > 0){
    medication$med[i] <- 1
  }
  else if(sum(str_detect(medication$LEDTRT[i], dopamine_agonists)) > 0){
    medication$med[i] <- 2
  }
  else{
    medication$med[i] <- 3
  }
  
  #Date adjustments
  medication[i,c('S_Y','S_M','E_Y','E_M')] <- c(str_sub(medication$STARTDT[i],-2,-1),
                                                str_sub(medication$STARTDT[i],1,2),
                                                str_sub(medication$STOPDT[i],-2,-1),
                                                str_sub(medication$STOPDT[i],1,2))
}


#
# only interested in patients from 1_
# 
medication <- medication %>% filter(PATNO %in% patients$PATNO)

#
#LEDD imputations
# Median-based imputation
# Similar medicines are imputed using median from same type of medicines
#

medication$LEDD <- as.numeric(medication$LEDD)
medication$LEDD[which(is.na(medication$LEDD))] = medication$LEDDSTRMG[which(is.na(medication$LEDD))] * medication$LEDDOSFRQ[which(is.na(medication$LEDD))] * medication$LEDDOSE[which(is.na(medication$LEDD))]

med_NA <- medication %>% filter(is.na(medication$LEDD))
med_Full <- medication %>% filter(!(is.na(medication$LEDD)))


#filled cheating sheet
LEDD_ <- read.csv("LEDD.csv")
for(i in 1:nrow(med_NA)){
  med_NA$LEDD[i] <- (LEDD_ %>% filter(LEDTRT == med_NA$LEDTRT[i]))$LEDD[1]
}

medication <- rbind(med_NA, med_Full)

#
# monthly medication information
#   PATNO
#   Y : years
#   M : months
#   med1 : levodopa
#   med2 : dopamine agonist
#   med3 : others
#   medcount : # of medicines
#

med_df <- data.frame(matrix(nrow = 0, ncol = 8))
columns <- c('PATNO','Y','M','med1','med2','med3','medcount', 'LEDD')
names(med_df) <- columns

#
# check the year-span for each patients
# create black dataframe containing all possible months 
# Mark the months with medicines
#

for(i in 1:nrow(patients)){
  tmp <- main_df %>% filter(PATNO == patients$PATNO[i])
  first <- min(tmp$Y)
  last <- max(tmp$Y)
  
  #blank dataframe for years first -> last
  tmp <- data.frame(matrix(0, nrow = ((last - first + 1)*12), ncol = 8))
  names(tmp) <- columns
  
  #call the medications
  tmp_med <- medication %>% filter(PATNO == patients$PATNO[i])
  
  count <- 1
  for(y in seq(first, last)){
    for(m in 1:12){
      tmp[count,'Y'] <- y
      tmp[count,'M'] <- m
      tmp[count,'PATNO'] <- patients$PATNO[i]
      
      #add medication info
      tmp_med_t <- tmp_med %>% filter((S_Y < y)|((S_Y == y) & (S_M <= m))) %>% 
        filter((E_Y > y)|((E_Y == y) & (E_M >= m)))
      if(nrow(tmp_med_t) != 0){
        for(j in 1:nrow(tmp_med_t)){
          if(tmp_med_t$med[j] == 1){
            tmp[count,'med1'] <- 1
          }
          else if(tmp_med_t$med[j] == 2){
            tmp[count,'med2'] <- 1
          }
          else if(tmp_med_t$med[j] == 3){
            tmp[count,'med3'] <- 1
          }
          tmp[count,'LEDD'] <- sum(tmp[count,'LEDD'], tmp_med_t$LEDD[j], na.rm=TRUE)
        }
      }
      count <- count + 1
    }
  }
  
  #add the temporary dataframe into med_df
  med_df <- rbind(med_df, tmp)
}


#
#export dataframe
#

write.csv(med_df, file = "med_df.csv")

