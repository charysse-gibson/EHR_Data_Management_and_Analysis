######################################################################
## Title: 01_dep_exposure.R                                         
## Author: Charysse Gibson
## Date created: July 17, 2019
######################################################################
# Purpose: Identify depression exposure
######################################################################
# Inputs: cohort_20190725.fst
#         cohort_diag_20190725.fst
#         ICD9_ICD10_Depressive Disorders Diagnoses_v2.csv
# outputs: dep_exp_20190819.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Prior Depression Definition: 
# 2 outpatient, 1 inpatient within 12-months

# Post Depression Definition: 
# 2 outpatient, 1 inpatient within 12-months

#-----Dataset & Variables Needed-----

# cohort (patients with oud diag & baseline dates)
    # PTID
    # FIRST_OUD

# cohort_diag (all diagnoses in cohort)
    # PTID
    # DATE
    # DIAG
    # care_type
    # SOURCE
    # covariate

# dep_diag
    # Type (ICD9, ICD10)
    # Value (ICD code)
    # Covariate
    # Description

#-----Inputs-----

## cohort (patients with oud diag & baseline dates)
cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 21757       21757

## cohort_diag (all diagnoses in cohort)
cohort_diag <-
  read_fst('data/created_data/cohort_diag_20190725.fst',
           as.data.table = T)
# cohort_diag[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 6784206     21757

# dep diagnoses
dep_diag <- 
  fread('A:/background_notes/ICD9_ICD10_Depressive Disorders Diagnoses_v2.csv')
oud_diag[,DIAG:=gsub('\\.','',value)]

##-----Identify pre-existing depression-----
# 1 - identify dep in cohort
# 2 - one inpatient or two outpatient diagnoses (before first OUD)
# 3 - create exposure indicator variable

# dep diagnoses
dep_diag <- 
  fread('A:/background_notes/ICD9_ICD10_Depressive Disorders Diagnoses_v2.csv')
dep_diag[,DIAG:=gsub('\\.','',Value)]

## identify dep in cohort
setkey(dep_diag,DIAG)
setkey(cohort_diag,DIAG)
cohort_diag[dep_diag,covariate:='DEP']
# cohort_diag[covariate=='DEP',list(.N,uniqueN(PTID))]
    #         N    V2
    # 1: 157881 11910
# cohort_diag[sample(.N,size=.N*.10)]

# one inpatient or two outpatient diagnoses (before first OUD)
cohort_dep <-
  cohort_diag[covariate=='DEP']
setkey(cohort_dep,PTID,DATE)
cohort_dep[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
cohort_dep[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
cohort_first_dep <-
  cohort_dep[(care_type=='inpatient')|(diff>0)&(diff<=365),list(FIRST_DEP=min(DATE)),by=PTID]

# depression exposure indicator variable
setkey(cohort,PTID)
setkey(cohort_first_dep,PTID)
cohort[cohort_first_dep,FIRST_DEP:=FIRST_DEP]
cohort[,DEP_EXP:=as.factor(ifelse(FIRST_OUD>FIRST_DEP,1,0))]

## DEP_EXP key
# 1 - depression prior to first oud
# 0 - depression diagnosis after first oud
# NA - did not have depression diagnosis

# describe(cohort[,list(DEP_EXP)])
    #     n  missing distinct 
    # 10451    11306        2 
    # 11306/21757 = 0.5196488 NA (with no dep diag)
# Value         0    1
# Frequency  5121 5330
# Proportion 0.49 0.51

## recode depression key
cohort[DEP_EXP=='1',PRIOR_DEP:=1]
cohort[DEP_EXP=='0',POST_DEP:=1] #will be removed later

#-----Outputs-----

# cohort with DEP exposure indicator
write.fst(cohort[,list(PTID,FIRST_OUD,PRIOR_DEP,POST_DEP)],
          path = paste0('data/created_data/dep_exp_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
