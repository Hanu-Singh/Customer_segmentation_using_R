rm(list=ls())

#Import File - There Are Many Ways To Import CSV File From Local Server

file_CC_General <- file.choose()
CC_General <- read.csv(file_CC_General,header=TRUE)

#View File and See Underlying Structure of Variables

View(CC_General)
str(CC_General)


#Removing the Cust_ID Column as It Is Categorical Variable (Non-Numeric)

CC_General <- CC_General[-1]
View(CC_General)
names(CC_General)

#Writing Function To Compute Statistics and Apply On Dataset

CC_General_func <- function(x){
  nmiss = sum(is.na(x))
  a = x[!is.na(x)]
  n = length(a)
  m = mean(a)
  min = min(a)
  max = max(a)
  s = sd(a)
  p1 = quantile(a, 0.95)
  p2 = quantile(a, 0.99)
  UL = m+3*s
  LL = m-3*s
  return(c(n=n, nmiss=nmiss, Mean=m, Min=min, Max=max, StDev=s, P1=p1, P2=p2, 'Upper Limit'=UL, 'Lower Limit'=LL))
}

#Taking the variable names on which function will be applied

vars <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",          
          "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY",
          "CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE")

#Applying function on the variables selected above

describe_stats <- t(data.frame(apply(CC_General[vars],2,CC_General_func)))
View(describe_stats)

#Treating te Outliers and filling them with UL (Upper Limit) values
#UL (Upper Limit) = Mean + 3*Standard Deviation

CC_General$BALANCE[CC_General$BALANCE > 7809.060] <- 7809.060
CC_General$PURCHASES[CC_General$PURCHASES > 7413.090] <- 7413.090
CC_General$ONEOFF_PURCHASES[CC_General$ONEOFF_PURCHASES > 5572.107] <- 5572.107
CC_General$INSTALLMENTS_PURCHASES[CC_General$INSTALLMENTS_PURCHASES > 3124.082] <- 3124.082
CC_General$CASH_ADVANCE[CC_General$CASH_ADVANCE > 7270.351] <- 7270.351
CC_General$CASH_ADVANCE_FREQUENCY[CC_General$CASH_ADVANCE_FREQUENCY > 0.736] <- 0.736
CC_General$CASH_ADVANCE_TRX[CC_General$CASH_ADVANCE_TRX > 23.723] <- 23.723
CC_General$PURCHASES_TRX[CC_General$PURCHASES_TRX > 89.283] <- 89.283
CC_General$CREDIT_LIMIT[CC_General$CREDIT_LIMIT > 15410.910] <- 15410.910
CC_General$PAYMENTS[CC_General$PAYMENTS > 10418.320] <- 10418.320
CC_General$MINIMUM_PAYMENTS[CC_General$MINIMUM_PAYMENTS > 7981.557] <- 7981.557

# Missing Value Treatment (Imputation)

CC_General$CREDIT_LIMIT[is.na(CC_General$CREDIT_LIMIT)] <- 4494.4
CC_General$MINIMUM_PAYMENTS[is.na(CC_General$MINIMUM_PAYMENTS)] <- 864.20
inputdata <- CC_General[vars]

# Correlation Matrix-computing values among the Data

corr_CC_General <- cor(inputdata)
View(corr_CC_General)

require(psych)
require(GPArotation)

#Plotting Data on Scree Plot

scree(corr_CC_General, factors = T, pc = T, main = "Scree Plot", hline = NULL, add = F)

#Applying Factor Analysis on Dataset

Factor_Analysis <- fa(r = corr_CC_General, 5, rotate = "varimax", fm = "ml")
print(Factor_Analysis)

#Computing Eigen Values to Select Potential Factors

eigen_value <- eigen(corr_CC_General)$values

#Writing Eigen Values Data to External CSV File

write.csv(eigen_value,"Eigen_Values.csv")

#Sorting Factor Analysis Data and Computing Factor Loadings

FA_Sorted <- fa.sort(Factor_Analysis)
ls(FA_Sorted)
loading <- FA_Sorted$loadings
print(loading)

#Writing Loading Data to External CSV File

write.csv(loading,"Factor Loadings.csv")

#Taking Potential or Intelligent KPIs in another Vector for further usage

vars1 <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","PURCHASES_FREQUENCY",
           "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_TRX","MINIMUM_PAYMENTS")

#Changing name of dataset with new KPI to inputdata1

inputdata1 <- CC_General[vars1]

#Scaling variables of the new dataset for clustering

inputdata_final <- scale(inputdata1)

#Applying Kmeans clustering on 3,4,5,6 clusters to check different values at each segment

Clus_3 <- kmeans(inputdata_final,3)
Clus_4 <- kmeans(inputdata_final,4)
Clus_5 <- kmeans(inputdata_final,5)
Clus_6 <- kmeans(inputdata_final,6)

table(Clus_3$cluster)
table(Clus_4$cluster)
table(Clus_5$cluster)
table(Clus_6$cluster)

#Taking previous clusters and combining them with new dataset with different names

CC_General_new <- cbind(CC_General, Km_Clus_3 = Clus_3$cluster, Km_Clus_4 = Clus_4$cluster, Km_Clus_5 = Clus_5$cluster, Km_Clus_6 = Clus_6$cluster)
CC_General_new$Km_Clus_3 <- factor(CC_General_new$Km_Clus_3)
CC_General_new$Km_Clus_4 <- factor(CC_General_new$Km_Clus_4)
CC_General_new$Km_Clus_5 <- factor(CC_General_new$Km_Clus_5)
CC_General_new$Km_Clus_6 <- factor(CC_General_new$Km_Clus_6)

table(CC_General_new$Km_Clus_3)

View(CC_General_new)

#Final output with out dataset is taken in CSV file - This is final output 

write.csv(CC_General_new,"Final_Output_With_Dataset.csv")

#Taking all the variables and putting them with clusters * Mean to show deviation from Mean of centroids - Additional Step \

require(tables)

#
Pro <- tabular(BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+
                 CASH_ADVANCE+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+
                 CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+
                 PRC_FULL_PAYMENT+TENURE ~ mean+(mean*Km_Clus_3)+(mean*Km_Clus_4)+(mean*Km_Clus_5)+(mean*Km_Clus_6), data = CC_General_new)

Pro_1 <- as.matrix(Pro)
Pro_1 <- data.frame(Pro_1)
View(Pro_1)

Profile <- tabular(1~length+(length*Km_Clus_3)+(length*Km_Clus_4)+(length*Km_Clus_5)+(length*Km_Clus_6), data = CC_General_new)

Pro_2 <- as.matrix(Pro)
Pro_2 <- data.frame(Pro_2)
View(Pro_2)
