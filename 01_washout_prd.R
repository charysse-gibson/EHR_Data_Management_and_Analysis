######################################################################
## Title: 01_washout_prd.R                                         
## Author: Charysse Gibson
## Date created: July 17, 2019
######################################################################
# Purpose: Identify exclusion criteria
#          Extract eligible population
######################################################################
# Inputs: cohort_20190725.fst
#         enc_20160712.fst
#         claim_med_serv20160712.fst
# outputs: 2yr_washout_20190730.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Washout Period Definition: 
# 1 visit in each of the 2 washout years before the start of follow-up period

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

#-----Extract Patients with Washout Period Criteria----- 
# 1 - compile list of unique patient visit dates
# 2 - create indicator for 0-365 days prior to first oud diag (baseline)
# 3 - create second indicator for 365-730 days prior to first oud diag 
# 4 - create indicator for 2_yr_washout

## unique patient visits
visits <- 
    unique(rbind(encounter[care_type!='other'&!is.na(interact_date),list(PTID,VISIT_DATE=interact_date)],
                claim_med_serv[!is.na(service_date),list(PTID,VISIT_DATE=service_date)]),by=c('PTID','VISIT_DATE'))
# cohort unique visit dates
setkey(cohort,PTID)
setkey(visits,PTID)
cohort_visits <-
    visits[cohort]
# days difference between first oud and visit
cohort_visits[,date_diff:=difftime(VISIT_DATE,FIRST_OUD,units = 'days')]
# year 1 & 2 unique visits by PTID
Year1 <-
    unique(cohort_visits[date_diff>0&date_diff<=365],by='PTID')
Year2 <-
    unique(cohort_visits[date_diff>365&date_diff<=730],by='PTID')
# yearly visit indicators
setkey(Year1,PTID)
setkey(Year2,PTID)
setkey(cohort,PTID)
cohort[Year1,VISIT_YR1:=1]
cohort[Year2,VISIT_YR2:=1]
# 2 year washout
cohort[VISIT_YR1==1&VISIT_YR2==1,WASHOUT_2YR:=1]

#       cohort    cohort w/ 2 yr washout
# N      21757                     10208

#-----Outputs-----

# cohort with 2 yr washout
write.fst(cohort,
          path = paste0('data/created_data/washout_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)