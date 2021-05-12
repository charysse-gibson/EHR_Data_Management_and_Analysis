######################################################################
## Title: 02_follow_up_prd.R                                         
## Author: Charysse Gibson
## Date created: July 31, 2019
######################################################################
# Purpose: Identify patients with follow-up period criteria
######################################################################
# Inputs: cohort_20190725.fst
#         enc_20160712.fst
#         claim_med_serv20160712.fst
# outputs: follow_up_20190820.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Follow-up Period Definition: 2 years

# From Dr. Scherrer's previous paper:
# Patients must have had at least 1 visit after the follow-up period started
# Follow-up period starts after first OUD

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

#-----Identify patients with follow-up period----- 
# 1 - compile list of unique patient visit dates
# 2 - create indicator for patients with at least 1 visit after baseline within the 2 follow-up years

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
    visits[cohort,nomatch=0]
# days difference between first oud and visit
cohort_visits[,date_diff:=difftime(VISIT_DATE,FIRST_OUD,units = 'days')]
# follow-up period unique visits by PTID
followup <-
    unique(cohort_visits[date_diff>0&date_diff<=730],by='PTID')
# follow-up visit indicator
setkey(followup,PTID)
setkey(cohort,PTID)
cohort[followup,FOLLOW_UP:=1]
cohort[is.na(FOLLOW_UP)] # 1766; no visits in the 2 follow-up years

#-----Outputs----- 

write.fst(cohort,
           path = paste0('data/created_data/follow_up_',
                         gsub('-','',Sys.Date()),'.fst'),
           compress = 100)
