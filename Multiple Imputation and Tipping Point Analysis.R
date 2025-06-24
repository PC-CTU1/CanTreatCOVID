setwd("C:/Users/y277h/Desktop/CanTreatCovid/Handle Missing Data")
set.seed(1)

library(rstan)
library(StanHeaders)
library(rstanarm)
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
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"Fever"]=data[i,"EL_BAS_SymptomsFever"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"Cough"]=data[i,"EL_BAS_SymptomsCough"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"ShortBreath"]=data[i,"EL_BAS_SymptomsShortBreath"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"Fatigue"]=data[i,"EL_BAS_SymptomsFatigue"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"MuscleAche"]=data[i,"EL_BAS_SymptomsMuscleAche"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"NauseaVomiting"]=data[i,"EL_BAS_SymptomsNauseaVomiting"]}
  }
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE){summary_table[summary_table$subjectID==data[i,"participant_id"],"LossOfSmellTaste"]=data[i,"EL_BAS_SymptomsLossOfSmellTaste"]}
  }
  return(summary_table)
}

summary_table=create_summary_table("CTC_data_mapped_for_PANORAMIC_2024-12-04.csv", recover_table)
for (i in 1:nrow(summary_table)){if (summary_table$age[i]<65) {summary_table$age_group[i]=0} else {summary_table$age_group[i]=1}}

# define indicator for missing outcome
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$hospitalization[i])){summary_table$missing_hosp[i]=1} else {summary_table$missing_hosp[i]=0}
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$time_to_recovery[i])){summary_table$missing_ttr[i]=1} else {summary_table$missing_ttr[i]=0}
}

# convert baseline symptoms to numbers
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$Fever[i])==FALSE){
    if (summary_table$Fever[i]=="Major problem"){summary_table$Fever[i]=1}
    else {summary_table$Fever[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$Cough[i])==FALSE){
    if (summary_table$Cough[i]=="Major problem"){summary_table$Cough[i]=1}
    else {summary_table$Cough[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$ShortBreath[i])==FALSE){
    if (summary_table$ShortBreath[i]=="Major problem"){summary_table$ShortBreath[i]=1}
    else {summary_table$ShortBreath[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$Fatigue[i])==FALSE){
    if (summary_table$Fatigue[i]=="Major problem"){summary_table$Fatigue[i]=1}
    else {summary_table$Fatigue[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$MuscleAche[i])==FALSE){
    if (summary_table$MuscleAche[i]=="Major problem"){summary_table$MuscleAche[i]=1}
    else {summary_table$MuscleAche[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$NauseaVomiting[i])==FALSE){
    if (summary_table$NauseaVomiting[i]=="Major problem"){summary_table$NauseaVomiting[i]=1}
    else {summary_table$NauseaVomiting[i]=0}
  }
}
for (i in 1:nrow(summary_table)){
  if (is.na(summary_table$LossOfSmellTaste[i])==FALSE){
    if (summary_table$LossOfSmellTaste[i]=="Major problem"){summary_table$LossOfSmellTaste[i]=1}
    else {summary_table$LossOfSmellTaste[i]=0}
  }
}

# frequentist
summary(glm(missing_hosp ~ treatment, data=summary_table, family = "binomial"))
summary(glm(missing_hosp ~ age, data=summary_table, family = "binomial"))
summary(glm(missing_hosp ~ vaccination_status, data=summary_table, family = "binomial"))
summary(glm(missing_hosp ~ comorb, data=summary_table, family = "binomial"))
summary(glm(missing_hosp ~ age_group, data=summary_table, family = "binomial"))
summary(glm(missing_ttr ~ treatment, data=summary_table, family = "binomial"))
summary(glm(missing_ttr ~ age, data=summary_table, family = "binomial"))
summary(glm(missing_ttr ~ vaccination_status, data=summary_table, family = "binomial"))
summary(glm(missing_ttr ~ comorb, data=summary_table, family = "binomial"))
summary(glm(missing_ttr ~ age_group, data=summary_table, family = "binomial"))





# multiple imputation

theta_sample_PANORAMIC = c()
theta_sample_CanTreatCovid = c()
for (i in 1:100){
  imputed_table = summary_table
  
  # prior
  alpha_prior <- student_t(df = 1, location = -3.48, scale = 2.5)
  theta_prior <- student_t(df = 1, location = 0, scale = 2.5)
  
  # standardization of covariates
  imputed_table$age = (imputed_table$age-mean(imputed_table$age))/(4*as.numeric(var(imputed_table$age)))^0.5
  imputed_table$comorb = as.numeric(imputed_table$comorb)-mean(as.numeric(imputed_table$comorb))
  imputed_table$age_group = as.numeric(imputed_table$age_group)-mean(as.numeric(imputed_table$age_group))
  
  # impute vaccination_status
  impute_vac <- stan_glm(as.numeric(vaccination_status)~treatment+age+comorb, data = imputed_table,
                         family = binomial(link = "logit"), 
                         prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                         seed = 1, refresh=0)
  
  imputed_vac = posterior_predict(
    impute_vac,
    newdata = imputed_table[imputed_table$subjectID=="Jan-29",c("treatment","age","comorb")],
    draws = 1
  )
  
  imputed_table[imputed_table$subjectID=="Jan-29","vaccination_status"] = imputed_vac
  
  # standardization of covariates
  imputed_table$vaccination_status = as.numeric(imputed_table$vaccination_status)-mean(as.numeric(imputed_table$vaccination_status))
  
  # impute censored
  impute_censored <- stan_glm(as.numeric(censored)~treatment+age+comorb+vaccination_status, data = imputed_table,
                         family = binomial(link = "logit"), 
                         prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                         seed = 1, refresh=0)
  
  imputed_censored = posterior_predict(
    impute_censored,
    newdata = imputed_table[is.na(imputed_table$censored),c("treatment","age","comorb","vaccination_status")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$censored), ]$censored)){
    imputed_table[is.na(imputed_table$censored), ]$censored[1] = imputed_censored[j] 
  }
  
  # impute Fever
  impute_Fever <- stan_glm(as.numeric(Fever)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_Fever = posterior_predict(
    impute_Fever,
    newdata = imputed_table[is.na(imputed_table$Fever),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$Fever), ]$Fever)){
    imputed_table[is.na(imputed_table$Fever), ]$Fever[1] = imputed_Fever[j] 
  }
  
  # impute Cough
  impute_Cough <- stan_glm(as.numeric(Cough)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_Cough = posterior_predict(
    impute_Cough,
    newdata = imputed_table[is.na(imputed_table$Cough),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$Cough), ]$Cough)){
    imputed_table[is.na(imputed_table$Cough), ]$Cough[1] = imputed_Cough[j] 
  }
  
  # impute ShortBreath
  impute_ShortBreath <- stan_glm(as.numeric(ShortBreath)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_ShortBreath = posterior_predict(
    impute_ShortBreath,
    newdata = imputed_table[is.na(imputed_table$ShortBreath),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$ShortBreath), ]$ShortBreath)){
    imputed_table[is.na(imputed_table$ShortBreath), ]$ShortBreath[1] = imputed_ShortBreath[j] 
  }
  
  # impute Fatigue
  impute_Fatigue <- stan_glm(as.numeric(Fatigue)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_Fatigue = posterior_predict(
    impute_Fatigue,
    newdata = imputed_table[is.na(imputed_table$Fatigue),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$Fatigue), ]$Fatigue)){
    imputed_table[is.na(imputed_table$Fatigue), ]$Fatigue[1] = imputed_Fatigue[j] 
  }
  
  # impute MuscleAche
  impute_MuscleAche <- stan_glm(as.numeric(MuscleAche)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_MuscleAche = posterior_predict(
    impute_MuscleAche,
    newdata = imputed_table[is.na(imputed_table$MuscleAche),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$MuscleAche), ]$MuscleAche)){
    imputed_table[is.na(imputed_table$MuscleAche), ]$MuscleAche[1] = imputed_MuscleAche[j] 
  }
  
  # impute NauseaVomiting
  impute_NauseaVomiting <- stan_glm(as.numeric(NauseaVomiting)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_NauseaVomiting = posterior_predict(
    impute_NauseaVomiting,
    newdata = imputed_table[is.na(imputed_table$NauseaVomiting),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$NauseaVomiting), ]$NauseaVomiting)){
    imputed_table[is.na(imputed_table$NauseaVomiting), ]$NauseaVomiting[1] = imputed_NauseaVomiting[j] 
  }
  
  # impute LossOfSmellTaste
  impute_LossOfSmellTaste <- stan_glm(as.numeric(LossOfSmellTaste)~treatment+age+comorb+vaccination_status+censored, data = imputed_table,
                              family = binomial(link = "logit"), 
                              prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                              seed = 1, refresh=0)
  
  imputed_LossOfSmellTaste = posterior_predict(
    impute_LossOfSmellTaste,
    newdata = imputed_table[is.na(imputed_table$LossOfSmellTaste),c("treatment","age","comorb","vaccination_status","censored")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[is.na(imputed_table$LossOfSmellTaste), ]$LossOfSmellTaste)){
    imputed_table[is.na(imputed_table$LossOfSmellTaste), ]$LossOfSmellTaste[1] = imputed_LossOfSmellTaste[j] 
  }
  
  #impute hospitalization
  impute_outcome_treatment <- stan_glm(as.numeric(hospitalization)~age+vaccination_status+comorb+censored+Fever+Cough+ShortBreath+Fatigue+MuscleAche+NauseaVomiting+LossOfSmellTaste, data = imputed_table[imputed_table$treatment==1 & is.na(imputed_table$hospitalization)==FALSE,],
                                                 family = binomial(link = "logit"), 
                                                 prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                                                 seed = 1, refresh=0)
  
  imputed_outcome_treatment = posterior_predict(
    impute_outcome_treatment,
    newdata = imputed_table[imputed_table$treatment==1 & is.na(imputed_table$hospitalization), c("age","vaccination_status","comorb","censored","Fever","Cough","ShortBreath","Fatigue","MuscleAche","NauseaVomiting","LossOfSmellTaste")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[imputed_table$treatment==1 & is.na(imputed_table$hospitalization), ]$hospitalization)){
    imputed_table[imputed_table$treatment==1 & is.na(imputed_table$hospitalization), ]$hospitalization[1] = imputed_outcome_treatment[j] 
    # after each iteration, the first row in imputed_table[imputed_table$treatment==1 & is.na(imputed_table$hospitalization), ] will be the next row with missing value. That's why it's "hospitalization[1]".
  }
  
  impute_outcome_control <- stan_glm(as.numeric(hospitalization)~age+vaccination_status+comorb+censored+Fever+Cough+ShortBreath+Fatigue+MuscleAche+NauseaVomiting+LossOfSmellTaste, data = imputed_table[imputed_table$treatment==0 & is.na(imputed_table$hospitalization)==FALSE,],
                                               family = binomial(link = "logit"), 
                                               prior = theta_prior, prior_intercept = alpha_prior, QR=TRUE,
                                               seed = 1, refresh=0)
  
  imputed_outcome_control = posterior_predict(
    impute_outcome_control,
    newdata = imputed_table[imputed_table$treatment==0 & is.na(imputed_table$hospitalization), c("age","vaccination_status","comorb","censored","Fever","Cough","ShortBreath","Fatigue","MuscleAche","NauseaVomiting","LossOfSmellTaste")],
    draws = 1
  )
  
  for (j in 1:length(imputed_table[imputed_table$treatment==0 & is.na(imputed_table$hospitalization), ]$hospitalization)){
    imputed_table[imputed_table$treatment==0 & is.na(imputed_table$hospitalization), ]$hospitalization[1] = imputed_outcome_control[j] 
  }
  
  # fit stan model according to PANORAMIC
  y = array(as.numeric(imputed_table$hospitalization))
  Z = as.matrix(as.numeric(imputed_table[,c("treatment")]))
  X1 = as.matrix(as.numeric(imputed_table[,c("age")]))
  X2 = as.matrix(as.numeric(imputed_table[,c("vaccination_status")]))
  X3 = as.matrix(as.numeric(imputed_table[,c("comorb")]))
  X = cbind(X1,X2,X3)
  
  stan_dat <- list(N = nrow(imputed_table),
                   J = 1, 
                   M = 3,
                   y = y,
                   Z = Z,
                   X = X)
  
  fit_primary_PANORAMIC <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)
  
  theta_sample_PANORAMIC = c(theta_sample_PANORAMIC, extract(fit_primary_PANORAMIC)$'theta')
  
  # fit stan model according to CanTreatCovid
  y = array(as.numeric(imputed_table$hospitalization))
  Z = as.matrix(as.numeric(imputed_table[,c("treatment")]))
  X1 = as.matrix(as.numeric(imputed_table[,c("age_group")]))
  X2 = as.matrix(as.numeric(imputed_table[,c("vaccination_status")]))
  X = cbind(X1,X2)
  
  stan_dat <- list(N = nrow(imputed_table),
                   J = 1, 
                   M = 2,
                   y = y,
                   Z = Z,
                   X = X)
  
  fit_primary_CanTreatCovid <- stan(file = 'Stan Code.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)
  
  theta_sample_CanTreatCovid = c(theta_sample_CanTreatCovid, extract(fit_primary_CanTreatCovid)$'theta')
}

exp(c(quantile(theta_sample_PANORAMIC, 0.025), quantile(theta_sample_PANORAMIC, 0.5), quantile(theta_sample_PANORAMIC, 0.975)))
sum(theta_sample_PANORAMIC<0)/length(theta_sample_PANORAMIC)
exp(c(quantile(theta_sample_CanTreatCovid, 0.025), quantile(theta_sample_CanTreatCovid, 0.5), quantile(theta_sample_CanTreatCovid, 0.975)))
sum(theta_sample_CanTreatCovid<0)/length(theta_sample_CanTreatCovid)





# tipping point analysis
TPA_matrix_superiority = matrix(c(rep(0,16*35)), nrow = 16, ncol = 35)
TPA_matrix_odds_ratio = matrix(c(rep(0,16*35)), nrow = 16, ncol = 35)
for (i in 0:15){
  for (j in 0:34){
    y = array(c(rep(1,358),rep(0,358)))
    Z = as.matrix(c(rep(0,358-2-i),rep(1,2+i),rep(0,358-4-j),rep(1,4+j)))
    
    # fit stan model
    stan_dat <- list(N = 716,
                     J = 1, 
                     y = y,
                     Z = Z)
    
    fit_primary <- stan(file = 'Tipping Point Analysis.stan', data = stan_dat, iter = 20000, warmup = 10000, thin = 10, chains = 4)
    TPA_matrix_superiority[i+1, j+1] = sum(rstan::extract(fit_primary)$'theta'<0)/length(rstan::extract(fit_primary)$'theta')
    TPA_matrix_odds_ratio[i+1, j+1] = exp(median(rstan::extract(fit_primary)$'theta'))
  }
}

TPA_matrix_superiority
TPA_matrix_odds_ratio
write.csv(TPA_matrix_odds_ratio, "TPA_matrix_superiority.csv")
write.csv(TPA_matrix_odds_ratio, "TPA_matrix_odds_ratio.csv")

# transpose in Excel

# plot TPA matrix
library(tidyverse)
library(reshape2)
library(ggplot2)

# Read the CSV file
data <- read.csv("TPA_matrix_superiority.csv", header = TRUE, check.names = FALSE)

# Clean up the data (remove empty first column and set row names)
rownames(data) <- data[,1]
data <- data[,-1]

# Convert to matrix format
matrix_data <- as.matrix(data)

# Melt the data for ggplot
melted_data <- melt(matrix_data)
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow", breaks = seq(0, 1, by = 0.1), name = "Probability") +
  guides(
    fill = guide_colorbar(
      barheight = unit(3, "inches"),  # Height of the legend
      barwidth = unit(0.3, "inches"), # Width of the legend
      title.position = "top",         # Position of the title
      title.hjust = 0.5,              # Center-align title
    )
  ) +
  scale_y_reverse(
    breaks = seq(0, max(melted_data$Var1)),  # Step size = 1 for y-axis
                 expand = c(0, 0)  # Remove padding
    ) +
      scale_x_continuous(
    breaks = seq(0, max(as.numeric(melted_data$Var2))),  # Step size = 1 for x-axis
                  expand = c(0, 0)  # Remove padding
    ) +
  labs(x = "Number events within the missing for Paxlovid",
       y = "Number events within the missing for usual care",
       title = "Probability superiority") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank())


data <- read.csv("TPA_matrix_odds_ratio.csv", header = TRUE, check.names = FALSE)

# Clean up the data (remove empty first column and set row names)
rownames(data) <- data[,1]
data <- data[,-1]

# Convert to matrix format
matrix_data <- as.matrix(data)

# Melt the data for ggplot
melted_data <- melt(matrix_data)
ggplot(melted_data, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow", breaks = seq(0, 4.4, by = 0.2), name = "Odds ratio") +
  guides(
    fill = guide_colorbar(
      barheight = unit(3, "inches"),  # Height of the legend
      barwidth = unit(0.3, "inches"), # Width of the legend
      title.position = "top",         # Position of the title
      title.hjust = 0.5,              # Center-align title
    )
  ) +
  scale_y_reverse(
    breaks = seq(0, max(melted_data$Var1)),  # Step size = 1 for y-axis
    expand = c(0, 0)  # Remove padding
  ) +
  scale_x_continuous(
    breaks = seq(0, max(as.numeric(melted_data$Var2))),  # Step size = 1 for x-axis
    expand = c(0, 0)  # Remove padding
  ) +
  labs(x = "Number events within the missing for Paxlovid",
       y = "Number events within the missing for usual care",
       title = "Odds ratio") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank())
