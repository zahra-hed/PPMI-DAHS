#
#
#   3. Imputation & Time Series Clustering
#   
#   -> Takes main_df, patients, med_df from [1_Feature_Preprocessing], [2_Medications_Preprocessing]
#   
#
#   **interpolate data when & only when the feature is for a whole year
#   **cut the data into 3-year time window, stride = 1
#   
#   returns 
#       a matrix with 3-year time window for clustering (size = 3 * feature_number * -1)
#       a dataframe with each column corresponding to each 3-year time window
#   

library(dplyr)

setwd("D:/Masters/thesis/code/PPMI_DAHS/dataset/generated_data")

#read csv
med_df <- read.csv('med_df.csv')
main_df <- read.csv('main_df.csv')
patients <- read.csv('patients.csv')

#
# create the time series dataframe
#   PATNO
#   START : Start Date(YY)
#   END : End Date(YY)
#   med1 ~ med3 : medications   
#   med : medication policy
#       
#   
#   action: whether the policy changes
#

ts_df <- data.frame(matrix(0, nrow = 0, ncol = 9))
columns <- c("PATNO", "START", "END", "med1", "med2", "med3","LEDD", "med", "action")
names(ts_df) <- columns
# first_times <- data.frame()

for (i in 1:length(unique(main_df$PATNO))){
  tmp <- main_df %>% filter(PATNO == unique(main_df$PATNO)[i])
  first <- min(tmp$Y)
  last <- max(tmp$Y)
  tmp <- data.frame(matrix(0, nrow = (last - first - 1), ncol = 9))
  names(tmp) <- columns
  
  #call medications for this patient
  tmp_med <- med_df %>% filter(PATNO == unique(main_df$PATNO)[i])
  
  for (j in 1:nrow(tmp)){
    tmp$PATNO[j] <- unique(main_df$PATNO)[i]
    tmp$START[j] <- j + first - 1
    tmp$END[j] <- j + first + 1
    
    #add medication information
    tmp_med_t <- tmp_med %>% filter(Y > tmp$START[j]) %>% filter(Y < tmp$END[j])
    tmp_med_t <- tmp_med_t %>% summarize(med1 = mean(med1), med2 = mean(med2), med3 = mean(med3), LEDD = sum(LEDD))
    tmp[j, c('med1','med2','med3','LEDD')] <- tmp_med_t
  }
  ts_df <- rbind(ts_df, tmp)
  # first_times <- rbind(first_times, tmp[1,])
}

#check histograms for 3-year // 1-year
#check the first medical policies(is it levodopa?)
if(F)"
first_times$med1 <- round(first_times$med1); table(first_times$med1)
first_times$med2 <- round(first_times$med2); table(first_times$med2)
first_times$med3 <- round(first_times$med3); table(first_times$med3)
first_times$med <- first_times$med1 + first_times$med2 + first_times$med3; table(first_times$med)

ggplot(ts_df) + geom_histogram(aes(x=med3))
table(ts_df$med1)

"
if(F)"
#table(first_times$med1) levodopa:none 522:455
first_times <- first_times %>% filter(med1 == 1)
first_times <- unlist(first_times['PATNO'])

#filter main_df, ts_df, patients, med_df to patients that start with levodopa
main_df <- main_df %>% filter(PATNO %in% first_times)
med_df <- med_df %>% filter(PATNO %in% first_times)
patients <- patients %>% filter(PATNO %in% first_times)
ts_df <- ts_df %>% filter(PATNO %in% first_times)
"
#LEDD care
## I guess it's taking the mean of the LEDD since it was summed across three years (36 months)
ts_df$LEDD <- ts_df$LEDD / 36

#discretize LEDD (LEDD delta)
for (i in 1:(nrow(ts_df) - 1)){
  ts_df$LEDD_d[i] <- ts_df$LEDD[i + 1] - ts_df$LEDD[i]
}
#update ts_df -> medications policies
ts_df$med1 <- round(ts_df$med1); ts_df$med2 <- round(ts_df$med2); ts_df$med3 <- round(ts_df$med3)
ts_df$med <- ts_df$med1 + 2 * ts_df$med2 + 4 * ts_df$med3



#fill in action column
for (i in 1:(nrow(ts_df) - 1)) {
  # First check if we're still with the same patient
  if (ts_df$PATNO[i] != ts_df$PATNO[i + 1]) {
    ts_df$action[i] <- NA  # Different patient - no action comparison
    next  # Skip to next iteration
  }
  
  # Now handle LEDD changes for the same patient
  ledd_change <- ts_df$LEDD_d[i]
  
  if (ledd_change >= 50) {
    ts_df$action[i] <- 1     # Significant dose increase
  } else if (ledd_change <= -50) {
    ts_df$action[i] <- -1    # Significant dose decrease
  } else {
    ts_df$action[i] <- 0     # No significant change (|<50| mg)
  }
}
##takes care of putting NA for the LEDD_d of the last record of each patient
ts_df$LEDD_d[which(is.na(ts_df$action))] = NA


#
# interpolate main_df
#

#scale features first
main_df[5:14] <- scale(main_df[5:14])

df <- data.frame()

#create raw dataframe, with only features / 3-year time span => from year-summarized dataframe
raw <- data.frame(matrix(nrow = 0, ncol = 11))

for (i in 1:length(unique(main_df$PATNO))){
  tmp <- main_df %>% filter(PATNO == unique(main_df$PATNO)[i])
  
  #add rows for missing years
  count <- 1
  while (TRUE) {
    if ((tmp$Y[count] + 1) < tmp$Y[count + 1]) {
      gap <- tmp$Y[count + 1] - tmp$Y[count] - 1
      gap_df <- data.frame(matrix(nrow = gap, ncol = ncol(main_df)))
      names(gap_df) <- colnames(main_df)
      for (j in 1:gap){
        gap_df[j,'PATNO'] <- unique(main_df$PATNO)[i]
        gap_df[j,'Y'] <- j + tmp$Y[count]
        gap_df[j,'M'] <- 6
      }
      tmp <- rbind(tmp[1:count,], gap_df, tmp[(count+1):nrow(tmp),])
    }
    count <- count + 1
    if(count == nrow(tmp)) break
  }

  #year-wise summary (aggregation)
  tmp <- tmp %>% group_by(PATNO, Y) %>% summarize(HNY = mean(HNY, na.rm = TRUE), MU1 = mean(MU1, na.rm = TRUE), MU2 = mean(MU2, na.rm = TRUE), 
                                                  MU3 = mean(MU3, na.rm = TRUE), MU4 = mean(MU4, na.rm = TRUE), SAUT = mean(SAUT, na.rm = TRUE), RBD = mean(RBD, na.rm = TRUE), 
                                                  MOCA = mean(MOCA, na.rm = TRUE),SDMT = mean(SDMT, na.rm = TRUE), GDS = mean(GDS, na.rm = TRUE))

  #impute
  tmp_i <- tmp
  for (j in 1:nrow(tmp_i)){ ## For each time point
    for (k in 3:12){ ## For columns 3-12 (clinical measures)
      if (is.na(tmp_i[j,k])){
        low <- j
        ##looks backward
        while((low != 1) & is.na(tmp[low,k])){
          low <- low - 1
        }
        high <- j
        ##looks forward
        while((high != nrow(tmp_i)) & is.na(tmp[high,k])){
          high <- high + 1
        }
        ##Calculates Weighted Average
        tmp_i[j,k] <- mean(
          c(unlist(tmp[high,k]) * (high - j), # Forward value × distance
          unlist(tmp[low,k]) * (j - low)), # Backward value × distance
          na.rm = TRUE) / (high - low)
      }
    }
  }
  df <- rbind(df, tmp_i)
  
  #create the raw-features 3-year timespan dataset
  for(j in 1:(nrow(tmp_i)-2)){
    raw <- rbind(raw, tmp_i[j:(j+2),])
  }
}
main_df <- df

#drop patients who are completely missing a feature *SDMT/ MU4
drop_ <- raw$PATNO[which(is.na(raw$SDMT) | is.na(raw$MU4))]
drop_ <- unique(drop_)

#filter
main_df <- main_df %>% filter(!(PATNO %in% drop_))
raw <- raw %>% filter(!(PATNO %in% drop_))
med_df <- med_df %>% filter(!(PATNO %in% drop_))
ts_df <- ts_df %>% filter(!(PATNO %in% drop_))
patients <- patients %>% filter(!(PATNO %in% drop_))

#
#export dataframe
#

write.csv(main_df, file = "main_df.csv")
write.csv(patients, file = "patients.csv")
write.csv(med_df, file = "med_df.csv")

write.csv(ts_df, file = "ts_df.csv")
write.csv(raw, file = "raw.csv")

