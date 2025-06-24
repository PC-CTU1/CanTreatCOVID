setwd("C:/Users/y277h/Desktop/CanTreatCovid")
set.seed(1)

### create tables from Excel

# create recover table

library(stringr)
create_recover_table=function(file){
  data=read.csv(file, header=TRUE)
  recover_table <- data.frame(subjectID=character(),
                              timepoint=integer(), 
                              recovered=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Daily e-Diary")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"pdd_recover"])}
    if (str_detect(data[i,"redcap_event_name"], "Flu Pro Diaries")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"fpp_recover"])}
  }
  return(recover_table)
}

recover_table=create_recover_table("CanTreatCOVID_CSV_06NOV2024.csv")

# create summary table

create_summary_table=function(file, recover_table){
  data=read.csv(file, header=TRUE)
  summary_table <- data.frame(subjectID=character(),
                              hospitalization=character(),
                              treatment=character(),
                              age=integer(),
                              vaccination_status=integer(), # 0 or 1
                              vaccination_status_1=integer(), # 1 if 1, 0 if not
                              vaccination_status_2=integer(), # 1 if >=2, 0 if not
                              comorb_status=integer(),
                              comorb_ong=integer(),
                              comorb=integer(),
                              time_to_recovery=integer(),
                              censored=integer(),
                              income=integer(),
                              medical=integer(),
                              medical_1_ind=integer(),
                              medical_2_ind=integer(),
                              race=integer(),
                              race_black_ind=integer(),
                              race_asian_ind=integer(),
                              race_mixed_ind=integer(),
                              race_other_ind=integer(),
                              obesity=integer(),
                              diabetes=integer(),
                              heart_disease_1=integer(),
                              heart_disease_2=integer(),
                              heart_disease=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE)
    {summary_table[nrow(summary_table) + 1,] = list(data[i,"participant_id"], 0, 0, data[i,"dem_age_calc"],	data[i,"dem_vaccination_status"], NA, NA, 0, 0, 0, NA, NA,	data[i,"dem_house_income"],	data[i,"med_conditions"],	NA, NA, data[i,"dem_race"],	NA,	NA,	NA,	NA,	data[i,"chronic_disease_list___22"], data[i,"chronic_disease_list___11"], data[i,"chronic_disease_list___28"], data[i,"chronic_disease_list___29"],	NA)}
  }
  for (i in 1:nrow(data)){
    if (data[i,"hosp1_admin_date"]!="" & data[i,"hosp1_admin_date"]!="{null}"){summary_table[summary_table$subjectID==data[i,"participant_id"],"hospitalization"]=1}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Randomization")==TRUE){if(data[i,"rand_group"]=="Paxlovid"){summary_table[summary_table$subjectID==data[i,"participant_id"],"treatment"]=1}}
  }
  for (i in 1:nrow(data)){
    if (data[i,"comorbitity_status"]==1){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb_status"]=1}
  }
  for (i in 1:nrow(data)){
    if (data[i,"comorb_ong"]=="TRUE"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb_ong"]=1}
  }
  for (i in 1:nrow(recover_table)){
    if (recover_table[i,"recovered"]==0){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"time_to_recovery"]=recover_table[i,"timepoint"];
    summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"censored"]=0}
  }
  for (i in nrow(recover_table):1){
    if (recover_table[i,"recovered"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"time_to_recovery"]=recover_table[i,"timepoint"];
    summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"censored"]=1}
  }
  for (i in 1:nrow(summary_table)){summary_table$comorb[i]=summary_table$comorb_status[i]*summary_table$comorb_ong[i]}
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$income[i])==FALSE){
      if(summary_table$income[i]==1 | summary_table$income[i]==2){summary_table$income[i]=0}
      if(summary_table$income[i]==3 | summary_table$income[i]==4 | summary_table$income[i]==5 | summary_table$income[i]==6){summary_table$income[i]=1}
    }
  }
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$medical[i])==FALSE){
      if(summary_table$medical[i]==0){summary_table$medical_1_ind[i]=0; summary_table$medical_2_ind[i]=0}
      if(summary_table$medical[i]==1){summary_table$medical_1_ind[i]=1; summary_table$medical_2_ind[i]=0}
      if(summary_table$medical[i]==2){summary_table$medical_1_ind[i]=0; summary_table$medical_2_ind[i]=1}
    }
  }
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$race[i])==FALSE){
      if(summary_table$race[i]==1){summary_table$race_black_ind[i]=0; summary_table$race_mixed_ind[i]=0; summary_table$race_other_ind[i]=0; summary_table$race_asian_ind[i]=0}
      if(summary_table$race[i]==2){summary_table$race_black_ind[i]=1; summary_table$race_mixed_ind[i]=0; summary_table$race_other_ind[i]=0; summary_table$race_asian_ind[i]=0}
      if(summary_table$race[i]==9){summary_table$race_black_ind[i]=0; summary_table$race_mixed_ind[i]=1; summary_table$race_other_ind[i]=0; summary_table$race_asian_ind[i]=0}
      if(summary_table$race[i]==3 | summary_table$race[i]==5 | summary_table$race[i]==99){summary_table$race_black_ind[i]=0; summary_table$race_mixed_ind[i]=0; summary_table$race_other_ind[i]=1; summary_table$race_asian_ind[i]=0}
      if(summary_table$race[i]==4 | summary_table$race[i]==6 | summary_table$race[i]==7 | summary_table$race[i]==8){summary_table$race_black_ind[i]=0; summary_table$race_mixed_ind[i]=0; summary_table$race_other_ind[i]=0; summary_table$race_asian_ind[i]=1}
    }
  }
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$vaccination_status[i])==FALSE){
      if(summary_table$vaccination_status[i]==0){summary_table$vaccination_status_1[i]=0; summary_table$vaccination_status_2[i]=0}
      if(summary_table$vaccination_status[i]==1){summary_table$vaccination_status_1[i]=1; summary_table$vaccination_status_2[i]=0}
      if(summary_table$vaccination_status[i]>=2){summary_table$vaccination_status_1[i]=0; summary_table$vaccination_status_2[i]=1}
    }
  }
  for (i in 1:nrow(summary_table)){summary_table$vaccination_status[i]=max(min(summary_table$vaccination_status[i],2)-1,0)}
  for (i in 1:nrow(summary_table)){summary_table$heart_disease[i]=max(summary_table$heart_disease_1[i], summary_table$heart_disease_2[i])}
  return(summary_table)
}

summary_table=create_summary_table("CanTreatCOVID_CSV_06NOV2024.csv", recover_table)


### primary analysis

# install stan packages

library(rstan)
library(StanHeaders)
library(posterior)

# subgroup: age
summary_table_1 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status),]
y = array(as.numeric(summary_table_1$hospitalization))
Z = as.matrix(as.numeric(summary_table_1[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_1[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_1[,c("vaccination_status")]))
INT = X1*Z
X = cbind(X1,X2,INT)

stan_dat <- list(N = nrow(summary_table_1),
                 J = 1, 
                 M = 3,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,5])$`bbeta[3]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,5])$`bbeta[3]`, 0.975)

sum(as.numeric(summary_table_1[summary_table_1$treatment==1 & summary_table_1$age<65,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_1[summary_table_1$treatment==1 & summary_table_1$age<65,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_1[summary_table_1$treatment==0 & summary_table_1$age<65,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_1[summary_table_1$treatment==0 & summary_table_1$age<65,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_1[summary_table_1$treatment==1 & summary_table_1$age>=65,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_1[summary_table_1$treatment==1 & summary_table_1$age>=65,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_1[summary_table_1$treatment==0 & summary_table_1$age>=65,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_1[summary_table_1$treatment==0 & summary_table_1$age>=65,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+INT, family=binomial(link = "logit")))


# subgroup: vaccination
summary_table_2 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status),]
y = array(as.numeric(summary_table_2$hospitalization))
Z = as.matrix(as.numeric(summary_table_2[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_2[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_2[,c("vaccination_status_1")]))
X3 = as.matrix(as.numeric(summary_table_2[,c("vaccination_status_2")]))
INT_1 = X2*Z
INT_2 = X3*Z
X = cbind(X1,X2,INT_1, INT_2)

stan_dat <- list(N = nrow(summary_table_2),
                 J = 1, 
                 M = 4,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,5])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,5])$`bbeta[3]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,5])$`bbeta[3]`, 0.975)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.975)

sum(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==1 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==1 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==1 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==1 & summary_table_2$vaccination_status_2==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==1 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_2[summary_table_2$treatment==0 & summary_table_2$vaccination_status_1==0 & summary_table_2$vaccination_status_2==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+INT_1+INT_2, family=binomial(link = "logit")))


# subgroup: race
summary_table_3 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$race_black_ind)&!is.na(summary_table$race_asian_ind)&!is.na(summary_table$race_mixed_ind)&!is.na(summary_table$race_other_ind),]
y = array(as.numeric(summary_table_3$hospitalization))
Z = as.matrix(as.numeric(summary_table_3[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_3[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_3[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_3[,c("race_black_ind")]))
X4 = as.matrix(as.numeric(summary_table_3[,c("race_asian_ind")]))
X5 = as.matrix(as.numeric(summary_table_3[,c("race_mixed_ind")]))
X6 = as.matrix(as.numeric(summary_table_3[,c("race_other_ind")]))
INT_1 = X3*Z
INT_2 = X4*Z
INT_3 = X5*Z
INT_4 = X6*Z
X = cbind(X1,X2,X3,X4,X5,X6,INT_1,INT_2,INT_3,INT_4)

stan_dat <- list(N = nrow(summary_table_3),
                 J = 1, 
                 M = 10,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,9])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,9])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,9])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,9])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,9])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,10])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,10])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,10])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,10])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,10])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,11])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,11])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,11])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,11])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,11])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,12])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,12])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,12])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,12])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,12])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,9])$`bbeta[7]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,9])$`bbeta[7]`, 0.975)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,10])$`bbeta[8]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,10])$`bbeta[8]`, 0.975)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,11])$`bbeta[9]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,11])$`bbeta[9]`, 0.975)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,12])$`bbeta[10]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,12])$`bbeta[10]`, 0.975)

sum(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==1 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==1 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==1 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==1 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==1 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==1 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==1 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==1 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==1 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==1 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==1 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==1 & summary_table_3$race_other_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==1 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_3[summary_table_3$treatment==0 & summary_table_3$race_black_ind==0 & summary_table_3$race_asian_ind==0 & summary_table_3$race_mixed_ind==0 & summary_table_3$race_other_ind==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+X4+X5+X6+INT_1+INT_2+INT_3+INT_4, family=binomial(link = "logit")))


# subgroup: income
summary_table_4 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$income),]
y = array(as.numeric(summary_table_4$hospitalization))
Z = as.matrix(as.numeric(summary_table_4[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_4[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_4[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_4[,c("income")]))
INT_1 = X3*Z
X = cbind(X1,X2,X3,INT_1)

stan_dat <- list(N = nrow(summary_table_4),
                 J = 1, 
                 M = 4,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.975)

sum(as.numeric(summary_table_4[summary_table_4$treatment==1 & summary_table_4$income==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_4[summary_table_4$treatment==1 & summary_table_4$income==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_4[summary_table_4$treatment==0 & summary_table_4$income==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_4[summary_table_4$treatment==0 & summary_table_4$income==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_4[summary_table_4$treatment==1 & summary_table_4$income==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_4[summary_table_4$treatment==1 & summary_table_4$income==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_4[summary_table_4$treatment==0 & summary_table_4$income==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_4[summary_table_4$treatment==0 & summary_table_4$income==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+INT_1, family=binomial(link = "logit")))


# subgroup: medical condition
summary_table_5 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$medical_1_ind)&!is.na(summary_table$medical_2_ind),]
y = array(as.numeric(summary_table_5$hospitalization))
Z = as.matrix(as.numeric(summary_table_5[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_5[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_5[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_5[,c("medical_1_ind")]))
X4 = as.matrix(as.numeric(summary_table_5[,c("medical_2_ind")]))
INT_1 = X3*Z
INT_2 = X4*Z
X = cbind(X1,X2,X3,X4,INT_1,INT_2)

stan_dat <- list(N = nrow(summary_table_5),
                 J = 1, 
                 M = 6,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,7])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,7])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,7])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,7])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,7])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,8])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,8])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,8])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,8])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,8])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,7])$`bbeta[5]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,7])$`bbeta[5]`, 0.975)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,8])$`bbeta[6]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,8])$`bbeta[6]`, 0.975)

sum(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_1_ind==0 & summary_table_5$medical_2_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_1_ind==0 & summary_table_5$medical_2_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_1_ind==0 & summary_table_5$medical_2_ind==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_1_ind==0 & summary_table_5$medical_2_ind==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_1_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_1_ind==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_1_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_1_ind==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_2_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==1 & summary_table_5$medical_2_ind==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_2_ind==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_5[summary_table_5$treatment==0 & summary_table_5$medical_2_ind==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+X4+INT_1+INT_2, family=binomial(link = "logit")))


# subgroup: obesity
summary_table_6 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$obesity),]
y = array(as.numeric(summary_table_6$hospitalization))
Z = as.matrix(as.numeric(summary_table_6[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_6[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_6[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_6[,c("obesity")]))
INT_1 = X3*Z
X = cbind(X1,X2,X3,INT_1)

stan_dat <- list(N = nrow(summary_table_6),
                 J = 1, 
                 M = 4,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.975)

sum(as.numeric(summary_table_6[summary_table_6$treatment==1 & summary_table_6$obesity==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_6[summary_table_6$treatment==1 & summary_table_6$obesity==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_6[summary_table_6$treatment==0 & summary_table_6$obesity==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_6[summary_table_6$treatment==0 & summary_table_6$obesity==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_6[summary_table_6$treatment==1 & summary_table_6$obesity==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_6[summary_table_6$treatment==1 & summary_table_6$obesity==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_6[summary_table_6$treatment==0 & summary_table_6$obesity==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_6[summary_table_6$treatment==0 & summary_table_6$obesity==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+INT_1, family=binomial(link = "logit")))


# subgroup: diabetes
summary_table_7 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$diabetes),]
y = array(as.numeric(summary_table_7$hospitalization))
Z = as.matrix(as.numeric(summary_table_7[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_7[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_7[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_7[,c("diabetes")]))
INT_1 = X3*Z
X = cbind(X1,X2,X3,INT_1)

stan_dat <- list(N = nrow(summary_table_7),
                 J = 1, 
                 M = 4,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.975)

sum(as.numeric(summary_table_7[summary_table_7$treatment==1 & summary_table_7$diabetes==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_7[summary_table_7$treatment==1 & summary_table_7$diabetes==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_7[summary_table_7$treatment==0 & summary_table_7$diabetes==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_7[summary_table_7$treatment==0 & summary_table_7$diabetes==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_7[summary_table_7$treatment==1 & summary_table_7$diabetes==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_7[summary_table_7$treatment==1 & summary_table_7$diabetes==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_7[summary_table_7$treatment==0 & summary_table_7$diabetes==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_7[summary_table_7$treatment==0 & summary_table_7$diabetes==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+INT_1, family=binomial(link = "logit")))


# subgroup: heart disease
summary_table_8 = summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$heart_disease),]
y = array(as.numeric(summary_table_8$hospitalization))
Z = as.matrix(as.numeric(summary_table_8[,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table_8[,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X2 = as.matrix(as.numeric(summary_table_8[,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table_8[,c("heart_disease")]))
INT_1 = X3*Z
X = cbind(X1,X2,X3,INT_1)

stan_dat <- list(N = nrow(summary_table_8),
                 J = 1, 
                 M = 4,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)

exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2])$`theta[1]`)
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.025))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.5))
exp(quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`, 0.975))
sum(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`<0)/length(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,2]+as_draws_df(fit_primary_CanTreatCovid)[,6])$`theta[1]`)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.025)
quantile(as.vector(as_draws_df(fit_primary_CanTreatCovid)[,6])$`bbeta[4]`, 0.975)

sum(as.numeric(summary_table_8[summary_table_8$treatment==1 & summary_table_8$heart_disease==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_8[summary_table_8$treatment==1 & summary_table_8$heart_disease==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_8[summary_table_8$treatment==0 & summary_table_8$heart_disease==0,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_8[summary_table_8$treatment==0 & summary_table_8$heart_disease==0,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_8[summary_table_8$treatment==1 & summary_table_8$heart_disease==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_8[summary_table_8$treatment==1 & summary_table_8$heart_disease==1,]$hospitalization, na.rm = TRUE))
sum(as.numeric(summary_table_8[summary_table_8$treatment==0 & summary_table_8$heart_disease==1,]$hospitalization, na.rm = TRUE))
length(as.numeric(summary_table_8[summary_table_8$treatment==0 & summary_table_8$heart_disease==1,]$hospitalization, na.rm = TRUE))

summary(glm(y~Z+X1+X2+X3+INT_1, family=binomial(link = "logit")))










### secondary analysis (time to recovery)

# install packages

library(rstanarm)
library(survival)

# read in the dataset

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$time_to_recovery)&!is.na(summary_table$censored),]
T=as.numeric(ToE_table$time_to_recovery)
status=as.numeric(ToE_table$censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

# create time intervals

cut_three <- c(5,10)

# subgroup: age
INT_1=as.numeric(age_group*treatment)

pehm_1=data.frame(T, status, treatment, age_group, vaccination_status, INT_1)
pehm_three_1=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_1, cut = cut_three)
pehm_three_1$interval = factor(pehm_three_1$tstart)
pehm_three_1$interval_length = pehm_three_1$T - pehm_three_1$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + INT_1 + vaccination_status + offset(log(interval_length)), 
                                                  data = pehm_three_1,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,6])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,6])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,6])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,6])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,6])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,6])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,6])$`INT_1`, 0.975)

nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$status==1 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$status==1 & pehm_1$age_group==1,])/nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$status==1 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$status==1 & pehm_1$age_group==1,])/nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$age_group==1,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$status==1 & pehm_1$age_group==0,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$age_group==0,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$status==1 & pehm_1$age_group==0,])/nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==1 & pehm_1$age_group==0,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$status==1 & pehm_1$age_group==0,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$age_group==0,])
nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$status==1 & pehm_1$age_group==0,])/nrow(pehm_1[is.na(pehm_1$age_group)==FALSE & pehm_1$treatment==0 & pehm_1$age_group==0,])


# subgroup: vaccination

vaccination_status_1 = as.numeric(ToE_table$vaccination_status_1)
vaccination_status_2 = as.numeric(ToE_table$vaccination_status_2)
INT_1=as.numeric(vaccination_status_1*treatment)
INT_2=as.numeric(vaccination_status_2*treatment)

pehm_2=data.frame(T, status, treatment, age_group, vaccination_status_1, vaccination_status_2, INT_1, INT_2)
pehm_three_2=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_2, cut = cut_three)
pehm_three_2$interval = factor(pehm_three_2$tstart)
pehm_three_2$interval_length = pehm_three_2$T - pehm_three_2$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status_1 + vaccination_status_2 + INT_1 + INT_2 + offset(log(interval_length)), 
                                                  data = pehm_three_2,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.975)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,9])$`INT_2`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,9])$`INT_2`, 0.975)

pehm_2 = pehm_2[is.na(pehm_2$vaccination_status_1)==FALSE & is.na(pehm_2$vaccination_status_2)==FALSE,]
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_1==1,])/nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_1==1,])/nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_1==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_2==1,])/nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_2==1,])/nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_2==1,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])
nrow(pehm_2[pehm_2$treatment==1 & pehm_2$status==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])/nrow(pehm_2[pehm_2$treatment==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])
nrow(pehm_2[pehm_2$treatment==0 & pehm_2$status==1 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])/nrow(pehm_2[pehm_2$treatment==0 & pehm_2$vaccination_status_1==0 & pehm_2$vaccination_status_2==0,])


# subgroup: race

race_black_ind = as.numeric(ToE_table$race_black_ind)
race_asian_ind = as.numeric(ToE_table$race_asian_ind)
race_mixed_ind = as.numeric(ToE_table$race_mixed_ind)
race_other_ind = as.numeric(ToE_table$race_other_ind)
INT_1=as.numeric(race_black_ind*treatment)
INT_2=as.numeric(race_asian_ind*treatment)
INT_3=as.numeric(race_mixed_ind*treatment)
INT_4=as.numeric(race_other_ind*treatment)

pehm_3=data.frame(T, status, treatment, age_group, vaccination_status, race_black_ind, race_asian_ind, race_mixed_ind, race_other_ind, INT_1, INT_2, INT_3, INT_4)
pehm_three_3=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_3, cut = cut_three)
pehm_three_3$interval = factor(pehm_three_3$tstart)
pehm_three_3$interval_length = pehm_three_3$T - pehm_three_3$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + race_black_ind + race_asian_ind + race_mixed_ind + race_other_ind + INT_1 + INT_2 + INT_3 + INT_4 + offset(log(interval_length)), 
                                                  data = pehm_three_3,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,11])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,11])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,11])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,11])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,11])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,12])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,12])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,12])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,12])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,12])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,13])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,13])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,13])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,13])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,13])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,14])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,14])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,14])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,14])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,14])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,11])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,11])$`INT_1`, 0.975)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,12])$`INT_2`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,12])$`INT_2`, 0.975)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,13])$`INT_3`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,13])$`INT_3`, 0.975)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,14])$`INT_4`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,14])$`INT_4`, 0.975)

pehm_3 = pehm_3[is.na(pehm_3$race_black_ind)==FALSE & is.na(pehm_3$race_asian_ind)==FALSE & is.na(pehm_3$race_mixed_ind)==FALSE & is.na(pehm_3$race_other_ind)==FALSE,]
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_black_ind==1,])/nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_black_ind==1,])/nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_black_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_asian_ind==1,])/nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_asian_ind==1,])/nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_asian_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_mixed_ind==1,])/nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_mixed_ind==1,])/nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_mixed_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_other_ind==1,])/nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_other_ind==1,])/nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_other_ind==1,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])
nrow(pehm_3[pehm_3$treatment==1 & pehm_3$status==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])/nrow(pehm_3[pehm_3$treatment==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])
nrow(pehm_3[pehm_3$treatment==0 & pehm_3$status==1 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])/nrow(pehm_3[pehm_3$treatment==0 & pehm_3$race_black_ind==0 & pehm_3$race_asian_ind==0 & pehm_3$race_mixed_ind==0 & pehm_3$race_other_ind==0,])


# subgroup: medical condition

medical_1_ind = as.numeric(ToE_table$medical_1_ind)
medical_2_ind = as.numeric(ToE_table$medical_2_ind)
INT_1=as.numeric(medical_1_ind*treatment)
INT_2=as.numeric(medical_2_ind*treatment)

pehm_4=data.frame(T, status, treatment, age_group, vaccination_status, medical_1_ind, medical_2_ind, INT_1, INT_2)
pehm_three_4=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_4, cut = cut_three)
pehm_three_4$interval = factor(pehm_three_4$tstart)
pehm_three_4$interval_length = pehm_three_4$T - pehm_three_4$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + medical_1_ind + medical_2_ind + INT_1 + INT_2 + offset(log(interval_length)), 
                                                  data = pehm_three_4,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,9])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,10])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,10])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,10])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,10])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,10])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,9])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,9])$`INT_1`, 0.975)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,10])$`INT_2`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,10])$`INT_2`, 0.975)

pehm_4 = pehm_4[is.na(pehm_4$medical_1_ind)==FALSE & is.na(pehm_4$medical_2_ind)==FALSE,]
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_1_ind==1,])/nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_1_ind==1,])/nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_1_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_2_ind==1,])/nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_2_ind==1,])/nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_2_ind==1,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])
nrow(pehm_4[pehm_4$treatment==1 & pehm_4$status==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])/nrow(pehm_4[pehm_4$treatment==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])
nrow(pehm_4[pehm_4$treatment==0 & pehm_4$status==1 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])/nrow(pehm_4[pehm_4$treatment==0 & pehm_4$medical_1_ind==0 & pehm_4$medical_2_ind==0,])


# subgroup: income

income = as.numeric(ToE_table$income)
INT_1=as.numeric(income*treatment)

pehm_5=data.frame(T, status, treatment, age_group, vaccination_status, income, INT_1)
pehm_three_5=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_5, cut = cut_three)
pehm_three_5$interval = factor(pehm_three_5$tstart)
pehm_three_5$interval_length = pehm_three_5$T - pehm_three_5$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + income + INT_1 + offset(log(interval_length)), 
                                                  data = pehm_three_5,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.975)

nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$status==1 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$status==1 & pehm_5$income==1,])/nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$status==1 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$status==1 & pehm_5$income==1,])/nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$income==1,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$status==1 & pehm_5$income==0,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$income==0,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$status==1 & pehm_5$income==0,])/nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==1 & pehm_5$income==0,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$status==1 & pehm_5$income==0,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$income==0,])
nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$status==1 & pehm_5$income==0,])/nrow(pehm_5[is.na(pehm_5$income)==FALSE & pehm_5$treatment==0 & pehm_5$income==0,])


# subgroup: obesity

obesity = as.numeric(ToE_table$obesity)
INT_1=as.numeric(obesity*treatment)

pehm_6=data.frame(T, status, treatment, age_group, vaccination_status, obesity, INT_1)
pehm_three_6=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_6, cut = cut_three)
pehm_three_6$interval = factor(pehm_three_6$tstart)
pehm_three_6$interval_length = pehm_three_6$T - pehm_three_6$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + obesity + INT_1 + offset(log(interval_length)), 
                                                  data = pehm_three_6,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.975)

nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$status==1 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$status==1 & pehm_6$obesity==1,])/nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$status==1 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$status==1 & pehm_6$obesity==1,])/nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$obesity==1,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$status==1 & pehm_6$obesity==0,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$obesity==0,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$status==1 & pehm_6$obesity==0,])/nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==1 & pehm_6$obesity==0,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$status==1 & pehm_6$obesity==0,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$obesity==0,])
nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$status==1 & pehm_6$obesity==0,])/nrow(pehm_6[is.na(pehm_6$obesity)==FALSE & pehm_6$treatment==0 & pehm_6$obesity==0,])


# subgroup: diabetes

diabetes = as.numeric(ToE_table$diabetes)
INT_1=as.numeric(diabetes*treatment)

pehm_7=data.frame(T, status, treatment, age_group, vaccination_status, diabetes, INT_1)
pehm_three_7=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_7, cut = cut_three)
pehm_three_7$interval = factor(pehm_three_7$tstart)
pehm_three_7$interval_length = pehm_three_7$T - pehm_three_7$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + diabetes + INT_1 + offset(log(interval_length)), 
                                                  data = pehm_three_7,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.975)

nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$status==1 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$status==1 & pehm_7$diabetes==1,])/nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$status==1 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$status==1 & pehm_7$diabetes==1,])/nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$diabetes==1,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$status==1 & pehm_7$diabetes==0,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$diabetes==0,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$status==1 & pehm_7$diabetes==0,])/nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==1 & pehm_7$diabetes==0,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$status==1 & pehm_7$diabetes==0,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$diabetes==0,])
nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$status==1 & pehm_7$diabetes==0,])/nrow(pehm_7[is.na(pehm_7$diabetes)==FALSE & pehm_7$treatment==0 & pehm_7$diabetes==0,])


# subgroup: heart disease

heart_disease = as.numeric(ToE_table$heart_disease)
INT_1=as.numeric(heart_disease*treatment)

pehm_8=data.frame(T, status, treatment, age_group, vaccination_status, heart_disease, INT_1)
pehm_three_8=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm_8, cut = cut_three)
pehm_three_8$interval = factor(pehm_three_8$tstart)
pehm_three_8$interval_length = pehm_three_8$T - pehm_three_8$tstart

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + heart_disease + INT_1 + offset(log(interval_length)), 
                                                  data = pehm_three_8,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4]+as_draws_df(fit_secondary_CanTreatCovid)[,8])$treatment)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.025)
quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,8])$`INT_1`, 0.975)

nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$status==1 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$status==1 & pehm_8$heart_disease==1,])/nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$status==1 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$status==1 & pehm_8$heart_disease==1,])/nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$heart_disease==1,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$status==1 & pehm_8$heart_disease==0,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$heart_disease==0,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$status==1 & pehm_8$heart_disease==0,])/nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==1 & pehm_8$heart_disease==0,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$status==1 & pehm_8$heart_disease==0,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$heart_disease==0,])
nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$status==1 & pehm_8$heart_disease==0,])/nrow(pehm_8[is.na(pehm_8$heart_disease)==FALSE & pehm_8$treatment==0 & pehm_8$heart_disease==0,])





