library(dplyr)
library(stringr)


data <- read.csv("D:/Masters/thesis/code/PPMI_DAHS/dataset/features/Geriatric_Depression_Scale__Short_Version__11Jul2023.csv")


  #YY/MM
data$Y <- as.numeric(str_sub(data$INFODT, -3,-1))
data$M <- as.numeric(str_sub(data$INFODT, 1,2))
data <- data %>% group_by(PATNO) %>% arrange(Y, M, .by_group = TRUE)
print(select(data, PATNO, INFODT, Y, M), n = 100)