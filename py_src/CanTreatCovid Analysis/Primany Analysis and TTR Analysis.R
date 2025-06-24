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
    if (str_detect(data[i,"redcap_event_name"], "Daily e-Diary")==TRUE){
      recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"DC_Recovered"])
    }
    if (str_detect(data[i,"redcap_event_name"], "Flu Pro Diaries")==TRUE){
      recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),data[i,"DC_Recovered"])
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table$recovered[i])==FALSE){
      if (recover_table$recovered[i]=="Yes"){recover_table$recovered[i]=1}
      if (recover_table$recovered[i]=="No"){recover_table$recovered[i]=0}
    } else{
      recover_table$recovered[i]=" "
    }
  }
  return(recover_table)
}

recover_table=create_recover_table("CTC_data_mapped_for_PANORAMIC_2024-12-04.csv")

# create summary table

create_summary_table=function(file, recover_table){
  data=read.csv(file, header=TRUE)
  summary_table <- data.frame(subjectID=character(),
                              hospitalization=character(),
                              treatment=character(),
                              age=integer(),
                              vaccination_status=integer(),
                              comorb=integer(),
                              time_to_recovery=integer(),
                              censored=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE & str_detect(data[i,"redcap_event_name"], "FFQ")==FALSE)
    {summary_table[nrow(summary_table) + 1,] = list(data[i,"participant_id"], NA, 0, data[i,"EL_BAS_DOB_Age"],	data[i,"EL_BAS_CovidVaccination"], 0, NA, NA)}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Randomization")==TRUE){if(data[i,"rand_group"]=="Paxlovid"){summary_table[summary_table$subjectID==data[i,"participant_id"],"treatment"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_BAS_VulnerabilityHypertension_raw"])==FALSE){if(data[i,"EL_BAS_VulnerabilityHypertension_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityLung_raw"])==FALSE){if(data[i,"EL_VulnerabilityLung_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityHeart_raw"])==FALSE){if(data[i,"EL_VulnerabilityHeart_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityKidney_raw"])==FALSE){if(data[i,"EL_VulnerabilityKidney_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityLiver_raw"])==FALSE){if(data[i,"EL_VulnerabilityLiver_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityNeuro_raw"])==FALSE){if(data[i,"EL_VulnerabilityNeuro_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityDiabetes_raw"])==FALSE){if(data[i,"EL_VulnerabilityDiabetes_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityImmuneSystem_raw"])==FALSE){if(data[i,"EL_VulnerabilityImmuneSystem_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"EL_VulnerabilityObesity_raw"])==FALSE){if(data[i,"EL_VulnerabilityObesity_raw"]=="Yes"){summary_table[summary_table$subjectID==data[i,"participant_id"],"comorb"]=1}}
  }
  for (i in 1:nrow(recover_table)){
    if (recover_table[i,"recovered"]==0){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"time_to_recovery"]=recover_table[i,"timepoint"];
    summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"censored"]=0}
  }
  for (i in nrow(recover_table):1){
    if (recover_table[i,"recovered"]==1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"time_to_recovery"]=recover_table[i,"timepoint"];
    summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"censored"]=1}
  }
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$vaccination_status[i])==FALSE){
      if (summary_table$vaccination_status[i]=="Yes"){summary_table$vaccination_status[i]=1}
      if (summary_table$vaccination_status[i]=="No"){summary_table$vaccination_status[i]=0}
    }
  }
  for (i in 1:nrow(summary_table)){
    if (summary_table$subjectID[i] %in% c("1-306", "1-296", "1-281", "1-279", "1-275", "1-274", "1-271", "1-234", "1-200", "1-179", "1-168", "1-149", "1-127", "1-104", "1-100", "1-98", "1-80", "1-65", "1-52", "1-15")){
      summary_table$hospitalization[i] = 0
    }
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Day 28")==TRUE){if (is.na(data[i,"DC_InHospital"])==FALSE){if(data[i,"DC_InHospital"]=="No"){summary_table[summary_table$subjectID==data[i,"participant_id"],"hospitalization"]=0}}}
  }
  for (i in 1:nrow(data)){
    if (is.na(data[i,"HO_AdmittedOvernightDate"])==FALSE){summary_table[summary_table$subjectID==data[i,"participant_id"],"hospitalization"]=1}
  }
  return(summary_table)
}

summary_table=create_summary_table("CTC_data_mapped_for_PANORAMIC_2024-12-04.csv", recover_table)



### primary analysis

# install stan packages

library(rstan)
library(StanHeaders)

# exclude patients with missing hospitalization outcome or vaccination_status

y = array(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),]$hospitalization))
Z = as.matrix(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),][,c("treatment")]))
X1 = as.matrix(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),][,c("age")]))
X2 = as.matrix(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),][,c("vaccination_status")]))
X3 = as.matrix(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),][,c("comorb")]))

# standardization of covariates

X1 = (X1-mean(X1))/(4*as.numeric(var(X1)))^0.5
X2 = X2-mean(X2)
X3 = X3-mean(X3)
X = cbind(X1,X2,X3)

# fit stan model according to PANORAMIC

stan_dat <- list(N = nrow(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),]),
                 J = 1, 
                 M = 3,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_PANORAMIC <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_primary_PANORAMIC
summary(fit_primary_PANORAMIC,probs=0.845)

# fit stan model according to CanTreatCovid

X1 = as.matrix(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),][,c("age")]))
for (i in 1:nrow(X1)){if (X1[i]<65) {X1[i]=0} else {X1[i]=1}}
X1 = X1-mean(X1)
X = cbind(X1,X2)

stan_dat <- list(N = nrow(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),]),
                 J = 1, 
                 M = 2,
                 y = y,
                 Z = Z,
                 X = X)

fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)
fit_primary_CanTreatCovid
summary(fit_primary_CanTreatCovid,probs=0.825)


### secondary analysis (time to recovery)

# install packages

library(rstanarm)
library(survival)

# read in the dataset

ToE_table <- summary_table[!is.na(summary_table$vaccination_status)&!is.na(summary_table$time_to_recovery)&!is.na(summary_table$censored),]
T=as.numeric(ToE_table$time_to_recovery)
status=as.numeric(ToE_table$censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)
comorb=as.numeric(ToE_table$comorb)

# standardization of covariates

age = (age-mean(age))/(4*as.numeric(var(age)))^0.5
age_group = age_group-mean(age_group)
vaccination_status = vaccination_status-mean(vaccination_status)
comorb = comorb-mean(comorb)

# create time intervals

cut_three <- c(5,10)

# transform dataset into long-format ones

pehm=data.frame(T, status, treatment, age, age_group, vaccination_status, comorb)
pehm_three=survival::survSplit(formula = Surv(T, status) ~ ., data = pehm, cut = cut_three)
pehm_three$interval = factor(pehm_three$tstart)
pehm_three$interval_length = pehm_three$T - pehm_three$tstart

# fit the model with rstanarm

fit_secondary_PANORAMIC <- rstanarm::stan_glm(formula = status ~ interval + treatment + age + vaccination_status + comorb + offset(log(interval_length)), 
                                              data = pehm_three,
                                              family = poisson(link = "log"),
                                              prior_intercept = normal(location = -2.3, scale = 0.3),
                                              prior = normal(location = 0, scale = 0.3),
                                              iter=20000)
summary(fit_secondary_PANORAMIC)
posterior_interval(fit_secondary_PANORAMIC, prob = 0.95, type = "central")
posterior_interval(fit_secondary_PANORAMIC, prob = 0.0000000001, type = "central")
posterior_interval(fit_secondary_PANORAMIC, prob = 0.999, type = "central")

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + offset(log(interval_length)), 
                                                  data = pehm_three,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
summary(fit_secondary_CanTreatCovid)
posterior_interval(fit_secondary_CanTreatCovid, prob = 0.95, type = "central")
posterior_interval(fit_secondary_CanTreatCovid, prob = 0.0000000001, type = "central")
posterior_interval(fit_secondary_CanTreatCovid, prob = 0.999, type = "central")


### descriptive statistics

# mean and median
median(as.numeric(ToE_table[ToE_table$treatment==1,]$time_to_recovery), na.rm = TRUE)
median(as.numeric(ToE_table[ToE_table$treatment==0,]$time_to_recovery), na.rm = TRUE)
mean(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status)&summary_table$treatment==1,]$hospitalization), na.rm = TRUE)
mean(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status)&summary_table$treatment==0,]$hospitalization), na.rm = TRUE)

# number of participants in each group used in each analysis
sum(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),]$treatment))
nrow(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),])-sum(as.numeric(summary_table[!is.na(summary_table$hospitalization)&!is.na(summary_table$vaccination_status),]$treatment))
sum(as.numeric(pehm$treatment))
nrow(pehm)-sum(as.numeric(pehm$treatment))
table(y,Z)
table(pehm$T,pehm$treatment)
round(table(pehm$T,pehm$treatment)[,1]/sum(table(pehm$T,pehm$treatment)[,1]),4)*100
round(table(pehm$T,pehm$treatment)[,2]/sum(table(pehm$T,pehm$treatment)[,2]),4)*100

# K-M
fit <- survfit(Surv(T, status) ~ 1, data = pehm[pehm$treatment==1,])
quantiles <- quantile(fit, probs = c(0.25, 0.5, 0.75))
quantiles$quantile[1]
quantiles$quantile[2]
quantiles$quantile[3]
fit <- survfit(Surv(T, status) ~ 1, data = pehm[pehm$treatment==0,])
quantiles <- quantile(fit, probs = c(0.25, 0.5, 0.75))
quantiles$quantile[1]
quantiles$quantile[2]
quantiles$quantile[3]

# patients with missing endpoint
summary_table[is.na(summary_table$hospitalization),]$subjectID
summary_table[is.na(summary_table$time_to_recovery),]$subjectID





