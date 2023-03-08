install.packages('dplyr')
install.packages('tidyverse')

library("dplyr")
library('tidyverse')

setwd("C://Users//kosai//Desktop//PPMI//features")
(temp = list.files(pattern="*.csv"))
myfiles = lapply(temp, read.delim)
preprocessing <- function(inputdata){
  inputdata <- inputdata %>% group_by(PATNO) %>% arrange(DATE, .by_group=TRUE)
  ##RECID creation col : NO
  patno_temp = 1
  inputdata[,"NO"] <- c(0)
  for (i in 1:length(inputdata$REC_ID)){
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
  for (i in 1:length(inputdata$REC_ID)){
    inputdata$DATE2[i] = floor(inputdata$DATE[i]/100)*100 + 100 * ((inputdata$DATE[i]) - (floor(inputdata$DATE[i]/100)*100)) / 12
  }
  ##KEY creation col : KEY
  inputdata$KEY <- c(0)
  for (i in 1:length(inputdata$REC_ID)){
    inputdata$KEY[i] = inputdata$DATE[i] + inputdata$PATNO[i] * 10000
  }
  ##Time interval calculation col : DATE_I
  inputdata$DATE_I <- c(0)
  inputdata$DATE_I[1] = 0.1
  for (i in 2:length(inputdata$REC_ID)){
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
  for (i in 2:length(inputdata$REC_ID)){
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
  for (i in (length(inputdata$REC_ID) - 1):1){
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
for (i in 1:length(temp)) assign(temp[i], preprocessing(read.csv(temp[i]))) ; temp <- NULL; myfiles <- NULL

total_features <- full_join(data.frame(KEY = as.vector(BJLO.csv$KEY), BJLO = as.vector(BJLO.csv$JLO_TOTRAW)),
                       data.frame(KEY = as.vector(ESS.csv$KEY), ESS = as.vector(ESS.csv$TOTAL)),
                       data.frame(KEY = as.vector(GDS.csv$KEY), GDS = as.vector(GDS.csv$GDS_SUM)),
                       data.frame(KEY = as.vector(HVLT.csv$KEY), HBLT = as.vector(HVLT.csv$DVT_TOTAL_RECALL)),
                       data.frame(KEY = as.vector(LNS.csv$KEY), LNS = as.vector(LNS.csv$LNS_TOTRAW)),
                       data.frame(KEY = as.vector(M1.csv$KEY), M1 = as.vector(M1.csv$NP1RTOT)),
                       data.frame(KEY = as.vector(M1Q.csv$KEY), M1Q = as.vector(M1Q.csv$NP1PTOT)),
                       data.frame(KEY = as.vector(M2.csv$KEY), M2 = as.vector(M2.csv$NP2PTOT)),
                       data.frame(KEY = as.vector(M3.csv$KEY), M3 = as.vector(M3.csv$NP3TOT)),
                       data.frame(KEY = as.vector(M4.csv$KEY), M4 = as.vector(M4.csv$NP4TOT)),
                       data.frame(KEY = as.vector(MCI.csv$KEY), MCI = as.vector(MCI.csv$COGSTATE)),
                       data.frame(KEY = as.vector(MOCA.csv$KEY), MOCA = as.vector(MOCA.csv$MCATOT)),
                       data.frame(KEY = as.vector(MSF.csv$KEY), MSF = as.vector(MSF.csv$DVT_SFTANIM)),
                       data.frame(KEY = as.vector(QUIP.csv$KEY), QUIP = as.vector(QUIP.csv$QUIP_SUM)),
                       data.frame(KEY = as.vector(RBD.csv$KEY), RBD = as.vector(RBD.csv$sum)),
                       data.frame(KEY = as.vector(SDMT.csv$KEY), SDMT = as.vector(SDMT.csv$SDMTOTAL)),
                       data.frame(KEY = as.vector(STAI.csv$KEY), STAI = as.vector(STAI.csv$STAI_SUM)),
                       data.frame(KEY = as.vector(SCOPA-AUT.csv$KEY), SCOPA = as.vector(SCOPA-AUT.csv$sum))
)
df <- total_features %>% reduce(full_join, by = "KEY") ; total_features <- NULL

df[,"PATNO"] <- c(0); df[,"DATE"] <- c(0)
for (i in 1:length(df$KEY)){
  df$PATNO[i] = as.integer(floor(df$KEY[i]/10000))
  df$DATE[i] = as.integer(df$KEY[i] %% 10000)
}
df$REC_ID <- c(0); preprocessing(df) -> df; df$REC_ID <- NULL
df$year <- c(0); temp = 0

for (i in 1:length(df$PATNO)){
  temp = temp + df$DATE_I[i]
  if (df$NUM[i] == 1){
    temp = 0
  }
  df$year[i] = (temp %/% 100)+1
}

df <- df %>%
  group_by(PATNO, year) %>%                # class별로 분리
  summarise(BJLO = mean(BJLO), HVLT = mean(HVLT), SDMT = mean(SDMT), MSF = mean(MSF), MCI = mean(MCI),
            MOCA = mean(MOCA), GDS = mean(GDS), STAI = mean(STAI), QUIP = mean(QUIP), ESS = mean(ESS), RBD = mean(RBD), LNS = mean(LNS),
            SCOPA = mean(SCOPA_AUT), M1 = mean(M1), M2 = mean(M2), M3 = mean(M3), M4 = mean(M4), PATNO = mean(PATNO), 
            DATE= min(DATE), DATE2 = min(DATE2)
)



##YEAR CUT
##CONCOHRT CUT
total <- total |> filter(CONCOHORT == 1)
