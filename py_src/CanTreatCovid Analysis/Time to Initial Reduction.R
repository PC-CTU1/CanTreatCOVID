setwd("C:/Users/y277h/Desktop/CanTreatCovid")
set.seed(1)

### create tables from Excel

# create recover table

library(stringr)
create_recover_table=function(file){
  data=read.csv(file, header=TRUE)
  recover_table <- data.frame(subjectID=character(),
                              timepoint=integer(), 
                              recovered=integer(),
                              fever=integer(),
                              cough=integer(),
                              sob=integer(),
                              taste=integer(),
                              muscle_ache=integer(),
                              nausea=integer(),
                              fatigue=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Daily e-Diary")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),
                                                    data[i,"pdd_recover"],
                                                    data[i,"pdd_fever"],
                                                    data[i,"pdd_cough"],
                                                    data[i,"pdd_sob"],
                                                    data[i,"pdd_taste"],
                                                    data[i,"pdd_muscle_ache"],
                                                    data[i,"pdd_nausea"],
                                                    data[i,"pdd_fatigue1"])}
    if (str_detect(data[i,"redcap_event_name"], "Flu Pro Diaries")==TRUE)
    {recover_table[nrow(recover_table) + 1,] = list(data[i,"participant_id"],str_sub(data[i,"redcap_event_name"],str_locate(data[i,"redcap_event_name"], "\\(")[1]+1,str_locate(data[i,"redcap_event_name"], "\\)")[1]-1),
                                                    data[i,"fpp_recover"],
                                                    data[i,"fpp_fever"],
                                                    data[i,"fpp_cough"],
                                                    data[i,"fpp_sob"],
                                                    data[i,"fpp_taste"],
                                                    data[i,"fpp_muscle_ache"],
                                                    data[i,"fpp_nausea"],
                                                    data[i,"fpp_fatigue1"])}
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
                              vaccination_status=integer(),
                              comorb_status=integer(),
                              comorb_ong=integer(),
                              comorb=integer(),
                              time_to_recovery=integer(),
                              censored=integer(),
                              fever_initial=integer(),
                              fever_censored=integer(),
                              fever_yes=integer(),
                              fever_severe_days=integer(),
                              cough_initial=integer(),
                              cough_censored=integer(),
                              cough_yes=integer(),
                              cough_severe_days=integer(),
                              sob_initial=integer(),
                              sob_censored=integer(),
                              sob_yes=integer(),
                              sob_severe_days=integer(),
                              taste_initial=integer(),
                              taste_censored=integer(),
                              taste_yes=integer(),
                              taste_severe_days=integer(),
                              muscle_ache_initial=integer(),
                              muscle_ache_censored=integer(),
                              muscle_ache_yes=integer(),
                              muscle_ache_severe_days=integer(),
                              nausea_initial=integer(),
                              nausea_censored=integer(),
                              nausea_yes=integer(),
                              nausea_severe_days=integer(),
                              fatigue_initial=integer(),
                              fatigue_censored=integer(),
                              fatigue_yes=integer(),
                              fatigue_severe_days=integer(),
                              overall_initial=integer(),
                              overall_censored=integer(),
                              overall_yes=integer(),
                              overall_severe_days=integer())
  for (i in 1:nrow(data)){
    if (str_detect(data[i,"redcap_event_name"], "Baseline")==TRUE)
    {summary_table[nrow(summary_table) + 1,] = list(data[i,"participant_id"], 0, 0, data[i,"dem_age_calc"],	data[i,"dem_vaccination_status"], 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)}
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
  for (i in 1:nrow(summary_table)){summary_table$vaccination_status[i]=max(min(summary_table$vaccination_status[i],2)-1,0)}
  
  # fever
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fever"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"fever"])==FALSE & is.na(recover_table[i-1,"fever"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"fever"]<recover_table[i-1,"fever"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fever"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"fever"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fever"])==FALSE){
      if (recover_table[i,"fever"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fever_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # cough
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"cough"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"cough"])==FALSE & is.na(recover_table[i-1,"cough"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"cough"]<recover_table[i-1,"cough"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"cough"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"cough"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"cough"])==FALSE){
      if (recover_table[i,"cough"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"cough_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # shortness of breath
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"sob"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"sob"])==FALSE & is.na(recover_table[i-1,"sob"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"sob"]<recover_table[i-1,"sob"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"sob"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"sob"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"sob"])==FALSE){
      if (recover_table[i,"sob"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"sob_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # taste
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"taste"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"taste"])==FALSE & is.na(recover_table[i-1,"taste"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"taste"]<recover_table[i-1,"taste"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"taste"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"taste"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"taste"])==FALSE){
      if (recover_table[i,"taste"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"taste_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # muscle_ache
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"muscle_ache"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"muscle_ache"])==FALSE & is.na(recover_table[i-1,"muscle_ache"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"muscle_ache"]<recover_table[i-1,"muscle_ache"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"muscle_ache"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"muscle_ache"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"muscle_ache"])==FALSE){
      if (recover_table[i,"muscle_ache"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"muscle_ache_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # nausea
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"nausea"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"nausea"])==FALSE & is.na(recover_table[i-1,"nausea"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"nausea"]<recover_table[i-1,"nausea"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"nausea"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"nausea"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"nausea"])==FALSE){
      if (recover_table[i,"nausea"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"nausea_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # fatigue
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fatigue"])==FALSE){
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_initial"]=recover_table[i,"timepoint"]
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_censored"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_yes"]=1
      summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_severe_days"]=0
    }
  }
  for (i in nrow(recover_table):2){
    if (is.na(recover_table[i,"fatigue"])==FALSE & is.na(recover_table[i-1,"fatigue"])==FALSE){
      if (recover_table[i,"timepoint"]>1 & recover_table[i,"fatigue"]<recover_table[i-1,"fatigue"]){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_initial"]=recover_table[i,"timepoint"]
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_censored"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fatigue"])==FALSE){
      if (recover_table[i,"timepoint"]==1 & recover_table[i,"fatigue"]==0){
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_initial"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_censored"]=1
        summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_yes"]=0
      }
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fatigue"])==FALSE){
      if (recover_table[i,"fatigue"]>1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"fatigue_severe_days"], 1), na.rm = TRUE)}
    }
  }
  
  # overall
  for (i in 1:nrow(summary_table)){
    if (is.na(summary_table$fever_initial[i])==FALSE | is.na(summary_table$cough_initial[i])==FALSE | is.na(summary_table$sob_initial[i])==FALSE | is.na(summary_table$taste_initial[i])==FALSE | is.na(summary_table$muscle_ache_initial[i])==FALSE | is.na(summary_table$nausea_initial[i])==FALSE | is.na(summary_table$fatigue_initial[i])==FALSE){
      if (min(c(summary_table$fever_censored[i], summary_table$cough_censored[i], summary_table$sob_censored[i], summary_table$taste_censored[i], summary_table$muscle_ache_censored[i], summary_table$nausea_censored[i], summary_table$fatigue_censored[i]), na.rm=TRUE)==0){
        initial_vector = c()
        if (summary_table$fever_censored[i]==0){initial_vector = c(initial_vector, summary_table$fever_initial[i])}
        if (summary_table$cough_censored[i]==0){initial_vector = c(initial_vector, summary_table$cough_initial[i])}
        if (summary_table$sob_censored[i]==0){initial_vector = c(initial_vector, summary_table$sob_initial[i])}
        if (summary_table$taste_censored[i]==0){initial_vector = c(initial_vector, summary_table$taste_initial[i])}
        if (summary_table$muscle_ache_censored[i]==0){initial_vector = c(initial_vector, summary_table$muscle_ache_initial[i])}
        if (summary_table$nausea_censored[i]==0){initial_vector = c(initial_vector, summary_table$nausea_initial[i])}
        if (summary_table$fatigue_censored[i]==0){initial_vector = c(initial_vector, summary_table$fatigue_initial[i])}
        summary_table$overall_initial[i] = min(initial_vector)
        summary_table$overall_censored[i] = 0
      }
      else {
        summary_table$overall_censored[i] = 1
        summary_table$overall_initial[i] = max(c(summary_table$fever_initial[i],summary_table$cough_initial[i],summary_table$sob_initial[i],summary_table$taste_initial[i],summary_table$muscle_ache_initial[i],summary_table$nausea_initial[i],summary_table$fatigue_initial[i]), na.rm=TRUE)
      }
      if (summary_table$overall_censored[i] == 1 & summary_table$overall_initial[i] == 1){summary_table$overall_yes[i]=0} else {summary_table$overall_yes[i]=1}
    }
  }
  for (i in 1:nrow(recover_table)){
    if (is.na(recover_table[i,"fever"])==FALSE | is.na(recover_table[i,"cough"])==FALSE | is.na(recover_table[i,"sob"])==FALSE | is.na(recover_table[i,"taste"])==FALSE | is.na(recover_table[i,"muscle_ache"])==FALSE | is.na(recover_table[i,"nausea"])==FALSE | is.na(recover_table[i,"fatigue"])==FALSE){
      if (max(c(recover_table[i,"fever"], recover_table[i,"cough"], recover_table[i,"sob"], recover_table[i,"taste"], recover_table[i,"muscle_ache"], recover_table[i,"nausea"], recover_table[i,"fatigue"]), na.rm = TRUE) > 1){summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"overall_severe_days"] = sum(c(summary_table[summary_table$subjectID==recover_table[i,"subjectID"],"overall_severe_days"], 1), na.rm = TRUE)}
    }
  }
  for (i in 1:nrow(summary_table)){
    if ((is.na(summary_table[i,"fever_censored"])==FALSE | is.na(summary_table[i,"cough_censored"])==FALSE | is.na(summary_table[i,"sob_censored"])==FALSE | is.na(summary_table[i,"taste_censored"])==FALSE | is.na(summary_table[i,"muscle_ache_censored"])==FALSE | is.na(summary_table[i,"nausea_censored"])==FALSE | is.na(summary_table[i,"fatigue_censored"])==FALSE) & is.na(summary_table[i,"overall_severe_days"])==TRUE){
      summary_table[i,"overall_severe_days"] = 0
    }
  }
  
  return(summary_table)
}

summary_table=create_summary_table("CanTreatCOVID_CSV_06NOV2024.csv", recover_table)



### Time to Initial Reduction of Severity of Symptoms

# install packages

library(rstanarm)
library(survival)

# fever

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$fever_initial)&!is.na(summary_table$fever_censored),]
ToE_table = ToE_table[ToE_table$fever_yes == 1,]
T=as.numeric(ToE_table$fever_initial)
status=as.numeric(ToE_table$fever_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$fever_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$fever_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# cough

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$cough_initial)&!is.na(summary_table$cough_censored),]
ToE_table = ToE_table[ToE_table$cough_yes == 1,]
T=as.numeric(ToE_table$cough_initial)
status=as.numeric(ToE_table$cough_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$cough_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$cough_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# shortness of breath

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$sob_initial)&!is.na(summary_table$sob_censored),]
ToE_table = ToE_table[ToE_table$sob_yes == 1,]
T=as.numeric(ToE_table$sob_initial)
status=as.numeric(ToE_table$sob_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$sob_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$sob_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# taste

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$taste_initial)&!is.na(summary_table$taste_censored),]
ToE_table = ToE_table[ToE_table$taste_yes == 1,]
T=as.numeric(ToE_table$taste_initial)
status=as.numeric(ToE_table$taste_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$taste_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$taste_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# muscle_ache

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$muscle_ache_initial)&!is.na(summary_table$muscle_ache_censored),]
ToE_table = ToE_table[ToE_table$muscle_ache_yes == 1,]
T=as.numeric(ToE_table$muscle_ache_initial)
status=as.numeric(ToE_table$muscle_ache_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$muscle_ache_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$muscle_ache_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# nausea

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$nausea_initial)&!is.na(summary_table$nausea_censored),]
ToE_table = ToE_table[ToE_table$nausea_yes == 1,]
T=as.numeric(ToE_table$nausea_initial)
status=as.numeric(ToE_table$nausea_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$nausea_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$nausea_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# fatigue

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$fatigue_initial)&!is.na(summary_table$fatigue_censored),]
ToE_table = ToE_table[ToE_table$fatigue_yes == 1,]
T=as.numeric(ToE_table$fatigue_initial)
status=as.numeric(ToE_table$fatigue_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$fatigue_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$fatigue_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# overall

ToE_table <- summary_table[!is.na(summary_table$age)&!is.na(summary_table$vaccination_status)&!is.na(summary_table$overall_initial)&!is.na(summary_table$overall_censored),]
ToE_table = ToE_table[ToE_table$overall_yes == 1,]
T=as.numeric(ToE_table$overall_initial)
status=as.numeric(ToE_table$overall_censored)
treatment=as.numeric(ToE_table$treatment)
age=as.numeric(ToE_table$age)
age_group=rep(0,length(age))
for (i in 1:length(age)){if (age[i]<65) {age_group[i]=0} else {age_group[i]=1}}
vaccination_status=as.numeric(ToE_table$vaccination_status)

nrow(ToE_table[ToE_table$treatment==1 & ToE_table$overall_censored==0,]) # Number of Patients with Reduction of Severity of Symptom
nrow(ToE_table[ToE_table$treatment==1,])
nrow(ToE_table[ToE_table$treatment==0 & ToE_table$overall_censored==0,])
nrow(ToE_table[ToE_table$treatment==0,])

# run the previous part corresponding to the specific symptom and then run the following

# create time intervals

cut_three <- c(5,10)

# transform dataset into long-format ones

pehm=data.frame(T, status, treatment, age_group, vaccination_status)
pehm_three=survival::survSplit(formula = Surv(T, status) ~ treatment + age_group + vaccination_status, data = pehm, cut = cut_three)
pehm_three$interval = factor(pehm_three$tstart)
pehm_three$interval_length = pehm_three$T - pehm_three$tstart

# fit the model with rstanarm

fit_secondary_CanTreatCovid <- rstanarm::stan_glm(formula = status ~ interval + treatment + age_group + vaccination_status + offset(log(interval_length)), 
                                                  data = pehm_three,
                                                  family = poisson(link = "log"),
                                                  prior_intercept = normal(location = -2.3, scale = 0.3),
                                                  prior = normal(location = 0, scale = 0.3),
                                                  iter=20000)
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.025))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.5))
exp(quantile(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment, 0.975))
sum(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment>0)/length(as.vector(as_draws_df(fit_secondary_CanTreatCovid)[,4])$treatment)





### Number of Days of Severe Symptoms
summary(summary_table[summary_table$treatment==1,]$fever_severe_days)
sd(summary_table[summary_table$treatment==1,]$fever_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$fever_severe_days)
sd(summary_table[summary_table$treatment==0,]$fever_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$cough_severe_days)
sd(summary_table[summary_table$treatment==1,]$cough_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$cough_severe_days)
sd(summary_table[summary_table$treatment==0,]$cough_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$sob_severe_days)
sd(summary_table[summary_table$treatment==1,]$sob_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$sob_severe_days)
sd(summary_table[summary_table$treatment==0,]$sob_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$taste_severe_days)
sd(summary_table[summary_table$treatment==1,]$taste_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$taste_severe_days)
sd(summary_table[summary_table$treatment==0,]$taste_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$muscle_ache_severe_days)
sd(summary_table[summary_table$treatment==1,]$muscle_ache_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$muscle_ache_severe_days)
sd(summary_table[summary_table$treatment==0,]$muscle_ache_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$nausea_severe_days)
sd(summary_table[summary_table$treatment==1,]$nausea_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$nausea_severe_days)
sd(summary_table[summary_table$treatment==0,]$nausea_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$fatigue_severe_days)
sd(summary_table[summary_table$treatment==1,]$fatigue_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$fatigue_severe_days)
sd(summary_table[summary_table$treatment==0,]$fatigue_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==1,]$overall_severe_days)
sd(summary_table[summary_table$treatment==1,]$overall_severe_days, na.rm=TRUE)
summary(summary_table[summary_table$treatment==0,]$overall_severe_days)
sd(summary_table[summary_table$treatment==0,]$overall_severe_days, na.rm=TRUE)





### Number Reporting Severe Symptoms at Days 7 and 14

day7_table = recover_table[is.na(recover_table$fever)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$fever>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$fever>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$cough)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$cough>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$cough>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$sob)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$sob>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$sob>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$taste)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$taste>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$taste>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$muscle_ache)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$muscle_ache>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$muscle_ache>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$nausea)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$nausea>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$nausea>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[is.na(recover_table$fatigue)==FALSE
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
nrow(day7_table[day7_table$treatment==1 & day7_table$fatigue>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$fatigue>1, ])
nrow(day7_table[day7_table$treatment==0, ])

day7_table = recover_table[(is.na(recover_table$fever)==FALSE
                           | is.na(recover_table$cough)==FALSE
                           | is.na(recover_table$sob)==FALSE
                           | is.na(recover_table$taste)==FALSE
                           | is.na(recover_table$muscle_ache)==FALSE
                           | is.na(recover_table$nausea)==FALSE
                           | is.na(recover_table$fatigue)==FALSE)
                           & recover_table$timepoint==7,]
for (i in 1:nrow(day7_table)){day7_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day7_table[i,"subjectID"]),"treatment"]}
for (i in 1:nrow(day7_table)){day7_table$overall[i] = max(c(day7_table$fever[i], day7_table$cough[i], day7_table$sob[i], day7_table$taste[i], day7_table$muscle_ache[i], day7_table$nausea[i], day7_table$fatigue[i]), na.rm=TRUE)}
nrow(day7_table[day7_table$treatment==1 & day7_table$overall>1, ])
nrow(day7_table[day7_table$treatment==1, ])
nrow(day7_table[day7_table$treatment==0 & day7_table$overall>1, ])
nrow(day7_table[day7_table$treatment==0, ])



day14_table = recover_table[is.na(recover_table$fever)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$fever>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$fever>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$cough)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$cough>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$cough>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$sob)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$sob>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$sob>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$taste)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$taste>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$taste>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$muscle_ache)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$muscle_ache>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$muscle_ache>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$nausea)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$nausea>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$nausea>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[is.na(recover_table$fatigue)==FALSE
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
nrow(day14_table[day14_table$treatment==1 & day14_table$fatigue>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$fatigue>1, ])
nrow(day14_table[day14_table$treatment==0, ])

day14_table = recover_table[(is.na(recover_table$fever)==FALSE
                            | is.na(recover_table$cough)==FALSE
                            | is.na(recover_table$sob)==FALSE
                            | is.na(recover_table$taste)==FALSE
                            | is.na(recover_table$muscle_ache)==FALSE
                            | is.na(recover_table$nausea)==FALSE
                            | is.na(recover_table$fatigue)==FALSE)
                           & recover_table$timepoint==14,]
for (i in 1:nrow(day14_table)){day14_table[i,"treatment"] = summary_table[which(summary_table$subjectID==day14_table[i,"subjectID"]),"treatment"]}
for (i in 1:nrow(day14_table)){day14_table$overall[i] = max(c(day14_table$fever[i], day14_table$cough[i], day14_table$sob[i], day14_table$taste[i], day14_table$muscle_ache[i], day14_table$nausea[i], day14_table$fatigue[i]), na.rm=TRUE)}
nrow(day14_table[day14_table$treatment==1 & day14_table$overall>1, ])
nrow(day14_table[day14_table$treatment==1, ])
nrow(day14_table[day14_table$treatment==0 & day14_table$overall>1, ])
nrow(day14_table[day14_table$treatment==0, ])
