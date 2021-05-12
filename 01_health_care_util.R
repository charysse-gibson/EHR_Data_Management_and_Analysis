######################################################################
## Title: 01_health_care_util.R                                        
## Author: Charysse Gibson
## Date created: August 14, 2019
######################################################################
# Purpose: Identify patient health care utilization
######################################################################
# Inputs: cohort_20190725.fst
#         enc_20160712.fst
#         claim_med_serv20160712.fst
# outputs: health_care_util_20190820.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Health care utilization
# (during washout through baseline time period)

#-----Dataset & Variables Needed-----

# Cohort
    # PTID
    # FIRST_OUD

# EMR
    # encounter
    # PTID
    # INTERACTION_TYPE
    # INTERACTION_DATE

# Claims
    # claim_med_serv
    # PTID
    # POS
    # LST_DT

#-----Inputs-----

## cohort dataset
cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 21757       21757

## emr, enounter dataset
encounter <- 
  read_fst('data/source_data/enc_20190712.fst',
           as.data.table = T)
str(encounter)
# convert interaction date
encounter[,interact_date:= as.Date(INTERACTION_DATE,format="%m-%d-%Y")]
# encounter[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3170317       32022
encounter[is.na(INTERACTION_DATE)]
# extract interaction type (care_type) 
enc_interaction <- 
  fread('background_notes/encounter_interaction.csv')
setkey(encounter,INTERACTION_TYPE)
setkey(enc_interaction,INTERACTION_TYPE)
encounter[enc_interaction,care_type:=care_type]
# describe(encounter[care_type=='other',list(INTERACTION_TYPE)]) # 203033/3170317=0.06404186
    # Value          Letter / Email Telephone / Online
    # Frequency               35104             167929

## claims, claim_med_serv dataset
claim_med_serv <- 
  read_fst('data/source_data/claim_med_serv20190712.fst',
           as.data.table = T)
str(claim_med_serv)
## convert service date
claim_med_serv[,service_date:=as.Date(LST_DT,format="%m-%d-%Y")]
# claim_med_serv[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3295678        8771

#-----Identify patient health care utilization----- 
# 1 - compile list of unique patient visit dates
# 2 - restrict count to only washout to follow-up period
# 3 - create patient visit count

## unique patient visits
visits <- 
  unique(rbind(encounter[care_type!='other'&!is.na(interact_date),
                         list(PTID,VISIT_DATE=interact_date)],
               claim_med_serv[!is.na(service_date),
                              list(PTID,VISIT_DATE=service_date)]),
         by=c('PTID','VISIT_DATE'))
# cohort unique visit dates
setkey(cohort,PTID)
setkey(visits,PTID)
cohort_visits <-
  visits[cohort]

health_care_util <- 
  cohort_visits[VISIT_DATE>(FIRST_OUD-730)&VISIT_DATE<FIRST_OUD,list(HEALTH_CARE_ENC=.N),by=PTID]

#-----Outputs-----

write.fst(health_care_util,
          path = paste0('data/created_data/health_care_util_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
