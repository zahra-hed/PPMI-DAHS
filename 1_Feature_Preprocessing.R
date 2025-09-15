#
#
#   1. PPMI Data set Feature Preprocessing
#
#   Features
# 
#     HNY; Hoehn and Yahr Stage
# 
#   Motor Assessments & UPDRS
#     MDP – UPDRS I ~ IV 
# 
#   Autonomic Test
#     SCOPA-AUT; Autonomic Dysfunction 
# 
#   Sleep Disorder
#     RBD; Rem Sleep Behavior Disorder
#
#   Neuropsychological Test
#     MoCa; Montreal Cognitive Assessment
#     SDMT; Symbol Digits Modalities Test
#     GDS; Geriatic Depression Scale
# 
#   Bio Specimen (Isn't it demographics??)
#     Age
#     Sex
#     Education
#
#
#   Returns two dataframes
#     main_df : dataframe w/ major features & YYMM/PATNO
#     patients : dataframe w/ PATNO & Patient data
#
#

#loading the libraries
library(dplyr)
library(stringr)
library(purrr)

#setting the working directory to the features directory
setwd("D:/Masters/thesis/code/PPMI_DAHS/dataset/features")

#call features
temp = list.files() 
#create a list of features datasets
features_list <- list()

#Adds M : MM, Y : YY columns
Add_DATE <- function(inputdata){
  #YY/MM
  inputdata$Y <- as.numeric(str_sub(inputdata$INFODT, -3,-1))
  inputdata$M <- as.numeric(str_sub(inputdata$INFODT, 1,2))
  
  ##the second arrange will overwrite the first sort by M
  inputdata <- inputdata %>% group_by(PATNO) %>% arrange(M, .by_group=TRUE) %>% arrange(Y, .by_group=TRUE)
  
  #delete visits with the same YYMM; keep only the first one
  rep_idx <- c()
  for(i in 2:nrow(inputdata)){
    if(inputdata$INFODT[i] == inputdata$INFODT[i-1]){rep_idx <- append(rep_idx,i)}
  }
  inputdata <- inputdata[-rep_idx,]
  
  return(inputdata)
}

for (i in 1:length(temp)) features_list[[temp[i]]] <- Add_DATE(read.csv(temp[i]))

#sum for GDS(2), RBD(9), SCOPA-AUT(10)
features_list[[1]]$GDS <- rowSums(features_list[[1]][6:20], na.rm = TRUE)
features_list[[7]]$RBD <- rowSums(features_list[[7]][7:27], na.rm = TRUE)
features_list[[8]]$SCOPA_AUT <- rowSums(features_list[[8]][c(7:30,32:34,36,38,40)], na.rm = TRUE)


#a full dataframe
main_df <- features_list %>% reduce(full_join, by = c('PATNO','Y','M'))
columns <- c('PATNO','Y','M','NHY','NP1RTOT','NP2PTOT', 'NP3TOT','NP4TOT','SCOPA_AUT','RBD','MCATOT','SDMTOTAL','GDS')

main_df <- main_df[columns]

#Adjust columns names
names(main_df)[4:13] <- c('HNY','MU1','MU2','MU3','MU4','SAUT','RBD','MOCA','SDMT','GDS')
main_df <- main_df %>% group_by(PATNO) %>% arrange(M, .by_group = TRUE) %>% arrange(Y, .by_group = TRUE)

# Add biospecimen / socio-economic data
setwd("D:/Masters/thesis/code/PPMI_DAHS/dataset/biospecimen")

temp = list.files() 
bio_list <- list()

for (i in 1:length(temp)) bio_list[[temp[i]]] <- read.csv(temp[i])

#extract only the wanted information: birth/sex/edu/cohort
patients <- bio_list %>% reduce(full_join, by = c('PATNO'))
columns <- c('PATNO','SEX','EDUCYRS','COHORT','BIRTHDT','ENROLL_DATE','ENROLL_AGE')
patients <- patients[columns]

#remove duplicate
patients <- patients %>% group_by(PATNO) %>% summarize(EDUCYRS = max(EDUCYRS),
                                                       COHORT = max(COHORT),
                                                       SEX = max(SEX),
                                                       ENROLL_AGE = max(ENROLL_AGE))


#   Preprocessing
#
#   - Parkinson Disease patients only
#   - Start HnY stage of 2
#   - More than 5+ year time span
#

#filter both dataframes for Parkinson patients only
patients <- patients %>% filter(COHORT == 1)
main_df <- main_df %>% filter(PATNO %in% patients$PATNO)

#fill in the missing HNY - where it is missing(first = initial stage)
## this code only fills the missing HNY if its the first record with that PATNO (initial stage?)
for (i in nrow(main_df):2){
  if (main_df$PATNO[i] != main_df$PATNO[i - 1]) {
    if (is.na(main_df$HNY[i])) {
      for (j in (i + 1):nrow(main_df)) {
        if (main_df$PATNO[j] == main_df$PATNO[i] && !is.na(main_df$HNY[j])) {
          main_df$HNY[i] <- main_df$HNY[j]
          break
        }
      }
    }
  }
}


#Keep only the records that have a initial ...
#HNY stage of 2 & more than 7 year time span

##then why is it at least 5 distinct years??

main_list <- list()
count <- 1

for (i in 1:nrow(patients)){
  tmp <- main_df %>% filter(PATNO == patients$PATNO[i])
  
  ##has data for at least 5 years
  if ((nrow(distinct(tmp["Y"])) > 4) && (tmp[1, "HNY"] <= 2)) {
    main_list[[count]] <- tmp
    count <- count + 1
  }
}


#update the dataframe & patients dataframe
main_df <- main_list[[1]]
for(i in 2:length(main_list)){
  main_df <- rbind(main_df, main_list[[i]])
}

##we can use this instead of the above for loop
#main_df <- bind_rows(main_list)

patients <- patients %>% filter(PATNO %in% unlist(distinct(main_df["PATNO"])))


#
# After preprocessing
# export the final dataframe
#

setwd("D:/Masters/thesis/code/PPMI_DAHS/dataset/generated_data")
write.csv(main_df, file = "main_df.csv")
write.csv(patients, file = "patients.csv")