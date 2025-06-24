setwd("C:/Users/y277h/Desktop/CanTreatCovid")
set.seed(1)

### create tables from Excel

# create daily table

library(stringr)
create_recover_table=function(file){
  data=read.csv(file, header=TRUE)
  recover_table <- data.frame(subjectID=character(),
                              timepoint=integer(), 
                              pdd_fam_dr=integer(),
                              pdd_walkin=integer(),
                              pdd_tel_health=integer(),
                              pdd_er=integer(),
                              pdd_other_healthcare=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Daily e-Diary")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"pdd_fam_dr"],data[i,"pdd_walkin"],data[i,"pdd_tel_health"],data[i,"pdd_er"],data[i,"pdd_other_healthcare"])}
    if (str_detect(data[i,"redcap_event_name"], "Flu Pro Diaries")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"fpp_fam_dr"],data[i,"fpp_walkin"],data[i,"fpp_tel_health"],data[i,"fpp_er"],data[i,"fpp_other_healthcare"])}
  }
  return(recover_table)
}

recover_table=create_recover_table("CanTreatCOVID_CSV_06NOV2024.csv")

# create summary table

create_summary_table=function(file, recover_table){
  data=read.csv(file, header=TRUE)
  summary_table <- data.frame(subjectID=character(),
                              treatment=character(),
                              age=integer(),
                              vaccination_status=integer(),
                              pdd_fam_dr_sum=integer(),
                              pdd_fam_dr_binary=integer(),
                              pdd_walkin_sum=integer(),
                              pdd_walkin_binary=integer(),
                              pdd_tel_health_sum=integer(),
                              pdd_tel_health_binary=integer(),
                              pdd_er_sum=integer(),
                              pdd_er_binary=integer(),
                              pdd_other_healthcare_sum=integer(),
                              pdd_other_healthcare_binary=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE)
    {summary_table[nrow(summary_table) + 1,] = list(data[i,"participant_id"], 0, data[i,"dem_age_calc"],	data[i,"dem_vaccination_status"], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Randomization")==TRUE){if(data[i,"rand_group"]=="Paxlovid"){summary_table[summary_table$subjectID==data[i,"participant_id"],"treatment"]=1}}
  }
  for (i in 1:nrow(summary_table)){summary_table$vaccination_status[i]=max(min(summary_table$vaccination_status[i],2)-1,0)}
  
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"pdd_fam_dr"])==FALSE){
      if (is.na(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_fam_dr_sum"])){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_fam_dr_sum"]=0}
      if (recover_table[i,"pdd_fam_dr"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_fam_dr_sum"]=as.numeric(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_fam_dr_sum"])+1}
    }
  }
  for (i in 1:nrow(summary_table)){if (is.na(summary_table$pdd_fam_dr_sum[i])==FALSE){if (summary_table$pdd_fam_dr_sum[i]>0){summary_table$pdd_fam_dr_binary[i]=1} else {summary_table$pdd_fam_dr_binary[i]=0}}}
  
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"pdd_walkin"])==FALSE){
      if (is.na(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_walkin_sum"])){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_walkin_sum"]=0}
      if (recover_table[i,"pdd_walkin"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_walkin_sum"]=as.numeric(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_walkin_sum"])+1}
    }
  }
  for (i in 1:nrow(summary_table)){if (is.na(summary_table$pdd_walkin_sum[i])==FALSE){if (summary_table$pdd_walkin_sum[i]>0){summary_table$pdd_walkin_binary[i]=1} else {summary_table$pdd_walkin_binary[i]=0}}}
  
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"pdd_tel_health"])==FALSE){
      if (is.na(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_tel_health_sum"])){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_tel_health_sum"]=0}
      if (recover_table[i,"pdd_tel_health"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_tel_health_sum"]=as.numeric(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_tel_health_sum"])+1}
    }
  }
  for (i in 1:nrow(summary_table)){if (is.na(summary_table$pdd_tel_health_sum[i])==FALSE){if (summary_table$pdd_tel_health_sum[i]>0){summary_table$pdd_tel_health_binary[i]=1} else {summary_table$pdd_tel_health_binary[i]=0}}}
  
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"pdd_er"])==FALSE){
      if (is.na(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_er_sum"])){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_er_sum"]=0}
      if (recover_table[i,"pdd_er"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_er_sum"]=as.numeric(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_er_sum"])+1}
    }
  }
  for (i in 1:nrow(summary_table)){if (is.na(summary_table$pdd_er_sum[i])==FALSE){if (summary_table$pdd_er_sum[i]>0){summary_table$pdd_er_binary[i]=1} else {summary_table$pdd_er_binary[i]=0}}}
  
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"pdd_other_healthcare"])==FALSE){
      if (is.na(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_other_healthcare_sum"])){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_other_healthcare_sum"]=0}
      if (recover_table[i,"pdd_other_healthcare"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_other_healthcare_sum"]=as.numeric(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"pdd_other_healthcare_sum"])+1}
    }
  }
  for (i in 1:nrow(summary_table)){if (is.na(summary_table$pdd_other_healthcare_sum[i])==FALSE){if (summary_table$pdd_other_healthcare_sum[i]>0){summary_table$pdd_other_healthcare_binary[i]=1} else {summary_table$pdd_other_healthcare_binary[i]=0}}}
  
  return(summary_table)
}

summary_table=create_summary_table("CanTreatCOVID_CSV_06NOV2024.csv", recover_table)



### primary analysis

# install stan packages

library(rstan)
library(StanHeaders)

# exclude patients with missing vaccination_status
summary_table = summary_table[!is.na(summary_table$vaccination_status) & !is.na(summary_table$pdd_fam_dr_sum) & !is.na(summary_table$pdd_walkin_sum) & !is.na(summary_table$pdd_tel_health_sum) & !is.na(summary_table$pdd_er_sum) & !is.na(summary_table$pdd_other_healthcare_sum),]

# define data for Stan model

y11 = array(as.numeric(summary_table$pdd_fam_dr_binary))
y12 = array(as.numeric(summary_table$pdd_fam_dr_sum))
y21 = array(as.numeric(summary_table$pdd_walkin_binary))
y22 = array(as.numeric(summary_table$pdd_walkin_sum))
y31 = array(as.numeric(summary_table$pdd_tel_health_binary))
y32 = array(as.numeric(summary_table$pdd_tel_health_sum))
y41 = array(as.numeric(summary_table$pdd_er_binary))
y42 = array(as.numeric(summary_table$pdd_er_sum))
y51 = array(as.numeric(summary_table$pdd_other_healthcare_binary))
y52 = array(as.numeric(summary_table$pdd_other_healthcare_sum))
Z = as.matrix(as.numeric(summary_table[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table[,c("age")]))
X2 = as.matrix(as.numeric(summary_table[,c("vaccination_status")]))

# create age group

for (i in 1:length(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}

# standardization of covariates

X1 = X1-mean(X1)
X2 = X2-mean(X2)
X = cbind(X1,X2)

# fit stan model according to CanTreatCovid

stan_dat_11 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                 J = 1, 
                 M = 2,
                 y = y11,
                 Z = Z,
                 X = X)

fit_11 <- stan(file = 'Stan Code.stan', data = stan_dat_11, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_11
summary(fit_11,probs=0.52)

stan_dat_21 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y21,
                    Z = Z,
                    X = X)

fit_21 <- stan(file = 'Stan Code.stan', data = stan_dat_21, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_21
summary(fit_21,probs=0.26)

stan_dat_31 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y31,
                    Z = Z,
                    X = X)

fit_31 <- stan(file = 'Stan Code.stan', data = stan_dat_31, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_31
summary(fit_31,probs=0.43)

stan_dat_41 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y41,
                    Z = Z,
                    X = X)

fit_41 <- stan(file = 'Stan Code.stan', data = stan_dat_41, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_41
summary(fit_41,probs=0.44)

stan_dat_51 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y51,
                    Z = Z,
                    X = X)

fit_51 <- stan(file = 'Stan Code.stan', data = stan_dat_51, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_51
summary(fit_51,probs=0.56)

stan_dat_12 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y12,
                    Z = Z,
                    X = X)

fit_12 <- stan(file = 'Stan Code Poisson.stan', data = stan_dat_12, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_12
summary(fit_12,probs=0.91)

stan_dat_22 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y22,
                    Z = Z,
                    X = X)

fit_22 <- stan(file = 'Stan Code Poisson.stan', data = stan_dat_22, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_22
summary(fit_22,probs=0.38)

stan_dat_32 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y32,
                    Z = Z,
                    X = X)

fit_32 <- stan(file = 'Stan Code Poisson.stan', data = stan_dat_32, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_32
summary(fit_32,probs=0.47)

stan_dat_42 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y42,
                    Z = Z,
                    X = X)

fit_42 <- stan(file = 'Stan Code Poisson.stan', data = stan_dat_42, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_42
summary(fit_42,probs=0.73)

stan_dat_52 <- list(N = nrow(summary_table[!is.na(summary_table$vaccination_status),]),
                    J = 1, 
                    M = 2,
                    y = y52,
                    Z = Z,
                    X = X)

fit_52 <- stan(file = 'Stan Code Poisson.stan', data = stan_dat_52, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_52
summary(fit_52,probs=0.61)



# descriptive

round(mean(summary_table[summary_table$treatment==1,]$pdd_fam_dr_binary)*100,1)
round(mean(summary_table[summary_table$treatment==0,]$pdd_fam_dr_binary)*100,1)
round(mean(summary_table[summary_table$treatment==1,]$pdd_walkin_binary)*100,1)
round(mean(summary_table[summary_table$treatment==0,]$pdd_walkin_binary)*100,1)
round(mean(summary_table[summary_table$treatment==1,]$pdd_tel_health_binary)*100,1)
round(mean(summary_table[summary_table$treatment==0,]$pdd_tel_health_binary)*100,1)
round(mean(summary_table[summary_table$treatment==1,]$pdd_er_binary)*100,1)
round(mean(summary_table[summary_table$treatment==0,]$pdd_er_sum)*100,1)
round(mean(summary_table[summary_table$treatment==1,]$pdd_other_healthcare_binary)*100,1)
round(mean(summary_table[summary_table$treatment==0,]$pdd_other_healthcare_binary)*100,1)

sum(summary_table[summary_table$treatment==1,]$pdd_fam_dr_binary)
sum(summary_table[summary_table$treatment==1,]$pdd_walkin_binary)
sum(summary_table[summary_table$treatment==1,]$pdd_tel_health_binary)
sum(summary_table[summary_table$treatment==1,]$pdd_er_binary)
sum(summary_table[summary_table$treatment==1,]$pdd_other_healthcare_binary)
nrow(summary_table[summary_table$treatment==1,])
sum(summary_table[summary_table$treatment==0,]$pdd_fam_dr_binary)
sum(summary_table[summary_table$treatment==0,]$pdd_walkin_binary)
sum(summary_table[summary_table$treatment==0,]$pdd_tel_health_binary)
sum(summary_table[summary_table$treatment==0,]$pdd_er_binary)
sum(summary_table[summary_table$treatment==0,]$pdd_other_healthcare_binary)
nrow(summary_table[summary_table$treatment==0,])

sum(summary_table[summary_table$treatment==1,]$pdd_fam_dr_sum)
sum(summary_table[summary_table$treatment==0,]$pdd_fam_dr_sum)
sum(summary_table[summary_table$treatment==1,]$pdd_walkin_sum)
sum(summary_table[summary_table$treatment==0,]$pdd_walkin_sum)
sum(summary_table[summary_table$treatment==1,]$pdd_tel_health_sum)
sum(summary_table[summary_table$treatment==0,]$pdd_tel_health_sum)
sum(summary_table[summary_table$treatment==1,]$pdd_er_sum)
sum(summary_table[summary_table$treatment==0,]$pdd_er_sum)
sum(summary_table[summary_table$treatment==1,]$pdd_other_healthcare_sum)
sum(summary_table[summary_table$treatment==0,]$pdd_other_healthcare_sum)


