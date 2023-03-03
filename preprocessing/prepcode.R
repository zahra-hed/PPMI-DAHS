##DATA
BJLO <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Benton_Judgement_of_Line_Orientation.csv") #Benton Judgement of line Orientation
HVLT <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Hopkins_Verbal_Learning_Test_-_Revised.csv") #Hopkins Verbal Learning Test_revised
SDMT <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Symbol_Digit_Modalities_Test.csv") #Symbol Digit Modalities Test
MSF <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Modified_Semantic_Fluency.csv") #Modified Semantic Fleuncy
MOCA <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Montreal_Cognitive_Assessment__MoCA_.csv") #Montreal_Cognitive Assesment
GDS <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Geriatric_Depression_Scale__Short_Version_.csv") #Geriatic Depression Scale
STAI <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//State-Trait_Anxiety_Inventory.csv") #State-trait Anxiety Inventory
QUIP <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//QUIP-Current-Short.csv") #QUIP
ESS <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Epworth_Sleepiness_Scale.csv") #Epworth Sleepiness Scale
SCOPA <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//SCOPA-AUT.csv") #SCOPA-AUT
M1 <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//MDS-UPDRS_Part_I.csv")
M1Q <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//MDS-UPDRS_Part_I_Patient_Questionnaire.csv")
M2 <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//MDS_UPDRS_Part_II__Patient_Questionnaire.csv")
M3 <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//MDS_UPDRS_Part_III.csv")
M4 <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//MDS-UPDRS_Part_IV__Motor_Complications.csv")
DTSC <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//DaTScan_Analysis.csv")
LNS <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Letter_-_Number_Sequencing.csv")
UPSIT <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Letter_-_Number_Sequencing.csv")
RBD <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//REM_Sleep_Behavior_Disorder_Questionnaire.csv")
MCI <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Cognitive_Categorization.csv")
UPSIT <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//University_of_Pennsylvania_Smell_Identification_Test__UPSIT_.csv")


demographic <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Demographics.csv")
Status <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Participant_Status.csv")
socio_economics <-read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Socio-Economics.csv")
death_a <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Conclusion_of_Study_Participation-Archived.csv")
death <- read.csv(file="C://Users//kosai//Desktop//Project PP//Parkinson Features//Conclusion_of_Study_Participation.csv")



##Pre processing---------------------
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

preprocessing(BJLO) -> BJLO
preprocessing(ESS) -> ESS
preprocessing(GDS) -> GDS
preprocessing(HVLT) -> HVLT
preprocessing(M1) -> M1
preprocessing(M1Q) -> M1Q
preprocessing(M2) -> M2
preprocessing(M3) -> M3
preprocessing(M4) -> M4
preprocessing(MOCA) -> MOCA
preprocessing(MSF) -> MSF
preprocessing(QUIP) -> QUIP
preprocessing(SCOPA) -> SCOPA
preprocessing(DTSC) -> DTSC
preprocessing(LNS) -> LNS
preprocessing(RBD) -> RBD
preprocessing(MCI) -> MCI
preprocessing(UPSIT) -> UPSIT
preprocessing(SDMT) -> SDMT
preprocessing(STAI) -> STAI

##Creating Total Data set ------------------------------------------------
KEY <- c(0); total <- data.frame(KEY)
for (i in 1:14){
  data.frame(Total_Variables[i]) -> variable
  temp <- data.frame(variable$KEY, variable$score); names(temp) <- c("KEY", variable); total <- full_join(total, temp, by ="KEY")
}


##HARD CODING ------
temp<- data.frame(BJLO$KEY, BJLO$JLO_TOTRAW); names(temp)<-c("KEY","BJLO"); total<-temp
temp<- data.frame(HVLT$DVT_TOTAL_RECALL, HVLT$KEY); names(temp)<-c("HVLT","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(SDMT$SDMTOTAL, SDMT$KEY); names(temp)<-c("SDMT","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(MSF$DVT_SFTANIM, MSF$KEY); names(temp)<-c("MSF","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(MOCA$MCATOT, MOCA$KEY); names(temp)<-c("MOCA","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(GDS$GDS_SUM, GDS$KEY); names(temp)<-c("GDS","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(STAI$STAI_SUM, STAI$KEY); names(temp)<-c("STAI","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(QUIP$QUIP_SUM, QUIP$KEY); names(temp)<-c("QUIP","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(ESS$TOTAL, ESS$KEY); names(temp)<-c("ESS","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(SCOPA$SUM, SCOPA$KEY);names(temp)<-c("SCOPA","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(M1$NP1RTOT, M1$KEY); names(temp)<-c("M1","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(M1Q$NP1PTOT, M1Q$KEY); names(temp)<-c("M1Q","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(M2$NP2PTOT, M2$KEY); names(temp)<-c("M2","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(M3$NP3TOT, M3$KEY); names(temp)<-c("M3","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(M4$NP4TOT, M4$KEY); names(temp)<-c("M4","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(LNS$LNS_TOTRAW, LNS$KEY); names(temp)<-c("LNS","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(RBD$sum, RBD$KEY); names(temp)<-c("RBD","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(DTSC$PUT, DTSC$CAUD, DTSC$KEY); names(temp)<-c("PUT", "CAUD","KEY"); total<-full_join(total,temp, by="KEY")
temp<- data.frame(MCI$COGSTATE, MCI$KEY); names(temp)<-c("MCI","KEY"); total<-full_join(total,temp, by="KEY")
#temp<- data.frame(UPSIT$TOTAL_CORRECT, UPSIT$KEY); names(temp)<-c("UPSIT","KEY"); total<-full_join(total,temp, by="KEY")

