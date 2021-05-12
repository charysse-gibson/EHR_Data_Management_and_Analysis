######################################################################
## Title: 00_identify_baseline.R                                         
## Author: Charysse Gibson
## Date created: July 7, 2019
######################################################################
# Purpose: Import source datasets
#          Extract relevant events and dates
#          Aggregate information across sources
#          Identify baseline dates
######################################################################
# Inputs: ICD9_ICD10_Opioid Related Diagnoses_v2.csv
#         claim_inp_conf20190712.fst
#         claim_med_diag20190712.fst
#         claim_med_serv20190712.fst
#         diag_20190712.fst
#         encournter_20190712.fst
# outputs: cohort_20190724.fst
#          cohort_diag_20190724.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

#-----Relevant Source Datasets-----

# Claims variables needed: 
    # claim_med_diag
        # PTID (Patient ID)
        # DIAG (ICD Diagnosis)
        # POS (place of service) or HCCC (facility type)?
    #claims_med_serv
        # PTID
        # CLMID (links to claims_med_diag)
        # LST_DT (service date, check with FST_DT)
    #claims_inpt_conf
        # PTID
        # ADMIT_DATE
        # DIAG1:DIAG5

# EMR variables needed:
    # diag
        # PTID
        # DIAG_DATE (MM-DD-YYYY date of diagnosis)
        # DIAGNOSIS_CD (ICD9/ICD10 codes, without decimal points)
        # DIAGNOSIS_CD_TYPE (ICD9, ICD10, SNOWMED)
        # ADMITTING_DIAGNOSIS
        # DISCHARGE_DIAGNOSIS
    # enc
        # PTID
        # ENCID
        # INTERACTION_TYPE
        # INTERACTION_DATE

# OUD specific variables needed:
    # oud_diag
        # Type (ICD9, ICD10)
        # Value (ICD code)
        # Covariate
        # Description
    # oud_patients (for checking only)
        # PTID (patients with OUD codes)

#-----Claims Data-----

## claims med diagnosis, includes ICD codes, link to claim medical services via claim ID
claim_med_diag <- 
  read_fst('data/source_data/claim_med_diag20190712.fst',
           as.data.table = T)
# claim_med_diag[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3771588        8771

## claims medical services, includes date, link to med claims via claim ID
claim_med_serv <- 
  read_fst('data/source_data/claim_med_serv20190712.fst',
           as.data.table = T)
str(claim_med_serv)
## convert service date
claim_med_serv[,service_date:=as.Date(LST_DT,format="%m-%d-%Y")]
# claim_med_serv[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3295678        8771
## type of care (inpatient/outpatient status)
cms_pos <- 
  fread('background_notes/claim_med_serv_pos.csv')
str(claim_med_serv[,list(POS)])
unique(claim_med_serv[,list(POS)])
## convert POS 
claim_med_serv[,POS:=as.character(POS)]
setkey(cms_pos,POS)
setkey(claim_med_serv,POS)
claim_med_serv[cms_pos,care_type:=care_type]
# describe(claim_med_serv[,.(care_type)])
    #       n  missing distinct 
    # 3071485   224193        2 
          # 224193/3071485=0.07299173 (missing)
    # Value       inpatient outpatient
    # Frequency      401292    2670193
    # Proportion      0.131      0.869
# describe(claim_med_serv[,list(CONF_ID),by=PTID])
# describe(claim_med_serv[CONF_ID!='',list(CONF_ID),by=PTID])
    #      n  missing distinct 
    # 403513        0     4651 
# care_type missing and by CONF_ID
claim_med_serv[is.na(care_type),care_type:='outpatient']
claim_med_serv[CONF_ID!='',care_type:='inpatient']

## claims inpatient confinement, includes dates and ICD codes
claim_inpt_conf <- 
  read_fst('data/source_data/claim_inp_conf20190712.fst',
           as.data.table = T)
str(claim_inpt_conf)
# describe(claim_inpt_conf[,list(CONF_ID),by=PTID])
    #     n  missing distinct 
    # 18710        0     4674 
    # 23 CONF_ID "missing" in claim_med_serv?
# convert admission date
claim_inpt_conf[,admission_date:=as.Date(ADMIT_DATE,format="%m-%d-%Y")]
# claim_inpt_conf[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 18710        4674
## type of care (all inpatient care type)
claim_inpt_conf[,care_type:='inpatient']

## Create source variable
claim_med_diag[,SOURCE:='claims']
claim_inpt_conf[,SOURCE:='claims']

## claim_inpt_conf to long
claim_inpt_conf_long <- 
  melt(claim_inpt_conf,
       na.rm = T,
       id.vars = c('PTID','admission_date','CONF_ID','care_type','SOURCE'),
       measure.vars = c('DIAG1', 'DIAG2', 'DIAG3', 'DIAG4', 'DIAG5'),
       variable.name='DIAG_POSITION',
       value.name='DIAG')
# claim_inpt_conf_long[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 93550        4674 (N=18710*5=93550 for diag position)

## extract DATE, CONF_ID, care_type from claims_med_serv to claim_med_diag
setkey(claim_med_serv,PTID,CLMID)
# claim_med_serv[is.na(service_date)] #9291/3295678=0.002819147
setkey(claim_med_diag,PTID,CLMID)
claim_med_diag[claim_med_serv,':='(DATE=service_date,
                                   care_type=care_type)]

## to see claims inpatient connections
# claim_med_serv[CLMID=='QQ15Q6R9D8']
# claim_inpt_conf[CONF_ID=='1QDLRL698R0Q']
# claim_med_diag[CLMID=='QQ15Q6R9D8']

## combine claim_med_diag & claim_inpt_conf_long
claim_inpt_conf_long[,DATE:=admission_date]
# convert 'inpatient' care_type to claim_med_diag
setkey(claim_inpt_conf_long,PTID,DATE,DIAG)
setkey(claim_med_diag,PTID,DATE,DIAG)
claim_med_diag[claim_inpt_conf_long,care_type:='inpatient']
# claims conditions to screen out
# claim_med_diag[is.na(DATE)] #5862
# claim_inpt_conf_long[DIAG==''] #10737
# claim_inpt_conf_long[DIAG=='-------'] #13
# combine datasets
claims <- 
  rbind(claim_med_diag[!is.na(DATE),list(PTID,DATE,DIAG,care_type,SOURCE)],
        claim_inpt_conf_long[(DIAG!='')&(DIAG!='-------'),list(PTID,DATE,DIAG,care_type,SOURCE)])

## duplicates
setkey(claims,PTID,DATE,DIAG,SOURCE,care_type)
# claims[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3848526        8771
claims <- 
  unique(claims,by=c('PTID','DATE','DIAG')) #2945460 ('inpatient' if care_type discrepancy)
    # claim records may have contained multiple times on same dates with different care_type statuses
# check for duplicates
# setkey(claims,PTID,DATE,DIAG)
# claims[unique(claims,by=c('PTID','DATE','DIAG','care_type'))[,.N,by=c('PTID','DATE','DIAG')][N>1]] 

#-----EMR Data-----

## diagnosis and dates for patients
diagnosis <- 
  read_fst('data/source_data/diag_20190712.fst',
           as.data.table = T)
str(diagnosis)
# convert diag date
diagnosis[,diagnosis_date:= as.Date(DIAG_DATE,format="%m-%d-%Y")]
# diagnosis[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 9416526 31178
diagnosis[is.na(diagnosis_date),list(.N)] # no missing dates
# source variable
diagnosis[,SOURCE:='EMR']

## encounter information, includes care_type and interaction date
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

## extract interaction type (care_type) 
enc_interaction <- 
  fread('background_notes/encounter_interaction.csv')
setkey(encounter,INTERACTION_TYPE)
setkey(enc_interaction,INTERACTION_TYPE)
encounter[enc_interaction,care_type:=care_type]
# describe(encounter[care_type=='other',list(INTERACTION_TYPE)]) # 203033/3170317=0.06404186
    # Value          Letter / Email Telephone / Online
    # Frequency               35104             167929

## aggregate diagnosis, care_type, interaction_date
setkey(encounter,PTID,ENCID)
setkey(diagnosis,PTID,ENCID)
diagnosis[encounter,':='(interact_date=interact_date,
                         care_type=care_type)]
# diagnosis[is.na(interact_date),list(.N)] #776372 missing dates
# diagnosis[sample(.N,size = 15)]
    # diagnosis_date and interact_date seem to be the same (with the exception of missing interact_dates)
# describe(diagnosis[,list(care_type)])
    #       n  missing distinct 
    # 8640154   776372        3
        # missing accounts for 776372/8640154=0.08985627
    # Value       inpatient      other outpatient
    # Frequency     2105392      52153    6482609
    # Proportion      0.244      0.006      0.750
# convert NA care_type to 'outpatient'
diagnosis[is.na(care_type),care_type:='outpatient']
# choose/rename date variable
diagnosis[,DATE:=diagnosis_date]
# rename diagnoses variable
diagnosis[,DIAG:=DIAGNOSIS_CD]

## conditions to screen out
# diagnosis status
# describe(diagnosis[,list(DIAGNOSIS_STATUS)])
      #       n  missing distinct 
      # 9416526        0        5 
    # Value     Diagnosis of  History of  Not recorded  Other diagnosis status  Possible diagnosis of
    # Frequency      9171228       37261        166944                   31694                   9399
    # Proportion       0.974       0.004         0.018                   0.003                  0.001
# care_type
# describe(diagnosis[,list(care_type)])
      #       n  missing distinct 
      # 9416526        0        3 
    # Value       inpatient      other outpatient
    # Frequency     2105392      52153    7258981
    # Proportion      0.224      0.006      0.771
# diagnosis code
# diagnosis[DIAGNOSIS_CD=='',.(.N)] #4428
# diagnosis code type
# describe(diagnosis[,list(DIAGNOSIS_CD_TYPE)])
      #       n  missing distinct 
      # 9416526        0        5 
    # Value        ICD10    ICD9   OTHER  SNOMED UNKNOWN
    # Frequency  5352549 3902738   21894  120525   18820
    # Proportion   0.568   0.414   0.002   0.013   0.002

emr <-
  diagnosis[(DIAGNOSIS_STATUS %in% c('Diagnosis of', 'Not recorded'))&
              (care_type!='other')&
              (DIAG!='')&
              (DIAGNOSIS_CD_TYPE%in%c('ICD10','ICD9')),
            list(PTID,DATE,DIAG,SOURCE,care_type)]

#-----OUD Diagnoses-----

## ICD codes for OUD diagnosis
oud_diag <- 
  fread('background_notes/ICD9_ICD10_Opioid Related Diagnoses_v2.csv')
oud_diag[,DIAG:=gsub('\\.','',Value)]

#-----Combine Claims & EMR Data-----
# 1 - Identify OUD diagnoses and relevant info/dates
# 3 - combine claims and EMR data

# identify claims oud diagnoses
setkey(oud_diag,DIAG)
setkey(claims,DIAG)
claims[oud_diag,covariate:=Covariate]
# claims[covariate=='OUD',list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 61326        7240

## identify EMR oud diagnoses
setkey(oud_diag,DIAG)
setkey(emr,DIAG)
emr[oud_diag,covariate:=Covariate]
# emr[covariate=='OUD',list(.N,unique_pats=uniqueN(PTID))]
    #         N unique_pats
    # 1: 270400       25913

## Combined Claims & EMR
baseline <- 
  unique(rbind(claims,emr),by=c('PTID','DATE','DIAG')) #8793608
    # unique PTID,DATE,DIAG precidented by 'inpatient' and 'claims'
# unique(rbind(claims,emr),by=c('PTID','DATE','DIAG','SOURCE','care_type')) #9232220

# baseline[sample(.N,15)]

## check unique patients by source
# baseline[SOURCE=='EMR',list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 5848148       30891
# baseline[SOURCE=='claims',list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 2945460        8771

#-----Source Type Overlap-----

# SOURCE overlap
# describe(baseline[,list(unique_source=uniqueN(SOURCE)),by=PTID])
# unique_source 
    #     n  missing distinct
    # 32263        0        2 
# Value          1     2
# Frequency  24864  7399
# Proportion 0.771 0.229
    # 7399 patients under both EMR & claims
    # 30891 + 8771 - 7399 = 32263

#-----Identify Baseline Dates-----

# OUD diagnosis requirement:
    # 2 outpatient, 1 inpatient within 12-months
    # first of any two dates (or 1 inpatient) is baseline

# 1 - identify only OUD codes 
# 2 - subset to unique dates by PTID
# 3 - sort date: setkey(,PTID,DATE)
# 4 - create date difference by PTID
# 5 - identify first inpatient
# 6 - identify first of any two date within 365 days as baseline
# 7 - take min of 6 and 5

## baseline oud codes by PTID and DATE
baseline_oud_diag <-
  unique(baseline[covariate=='OUD'],by=c('PTID','DATE'))
# baseline_oud_diag[,list(uniqueN(PTID))] #32186
# sort by PTID and DATE
setkey(baseline_oud_diag,PTID,DATE)
# create lag date
baseline_oud_diag[,diff:=DATE-shift(DATE, fill=first(DATE), type = 'lead'),by=PTID]
cohort <- 
  baseline_oud_diag[(care_type=='inpatient')|(diff<0)&(diff>=-365),list(FIRST_OUD=min(DATE)),by=PTID]
cohort[,list(uniqueN(PTID))] #21757

# Next steps:
    # Check for 2 years washout prior to OUD diag
    # Identify patients with prior DEP (one inpatient or two outpatient diagnoses)

#-----OUD Patients Check-----

## List of patients with OUD codes (for checking only)
oud_patients <- 
  read_fst('data/source_data/OUD_patient_list_20190712.fst',
           as.data.table = T)
# oud_patients[,list(.N,unique_pats=uniqueN(PTID))]
#        N unique_pats
# 1: 32269 32269

# Totals:   Claims    EMR   Claims & EMR    Pats w/ OUD diag   Pats w/ OUD diag criteria
#             7240  25831           7469               32106                       21817      

#-----Outputs-----

# cohort
write.fst(cohort,
          path = paste0('data/created_data/cohort_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)

# cohort diagnoses
setkey(cohort,PTID)
setkey(baseline,PTID)
cohort_diag <- 
  baseline[cohort,list(PTID,DATE,DIAG,care_type,SOURCE,covariate)]
# cohort_diag[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 6731416       21817
write.fst(cohort_diag,
          path = paste0('data/created_data/cohort_diag_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress=100)