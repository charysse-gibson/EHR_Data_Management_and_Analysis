######################################################################
## Title: 00_explore_data.R                                        
## Author: Charysse Gibson
## Date created: July 7, 2019
######################################################################
# Purpose: Explore source data
#          Identify baseline dates
######################################################################
# Inputs: ICD9_ICD10_Opioid Related Diagnoses_v2.csv
#         claim_inp_conf20190712.fst
#         claim_med_diag20190712.fst
#         claim_med_serv20190712.fst
#         claim_rx_20190712.fst
#         diag_20190712.fst
#         OUD_patient_list_20190712.fst
# outputs: baseline_20190716.fst
######################################################################

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

#-----Explore Source Datasets-----

# OUD_patient_list
  # PTID (patients with OUD codes)

# Claims variables needed: 
    # claim_med_diag
        # PTID (Patient ID)
        # ICD_Flag (ICD 9 or 10)
        # Diag (ICD Diagnosis)
        # Diag_Position
    #claims_med_serv
        # PTID
        # CLMID (links to claims_med_diag)
        # LST_DT (service date, check with FST_DT)
    #claims_inpt_conf
        # PTID
        # ADMIT_DATE
        # DIAG1:DIAG5
    # claim_rx [keep separate]
        # PTID
        # CLM_ID
        # FILL_DT
        # NCD (national drug data) or GNRC_NM (United States Adopted Names for drug ingredients)?
        # STRENGTH

# EMR variables needed:
    # diag
        # PTID
        # DIAG_DATE (MM-DD-YYYY date of diagnosis)
        # DIAGNOSIS_CD (ICD9/ICD10 codes, without decimal points)
        # DIAGNOSIS_CD_TYPE (ICD9, ICD10, SNOWMED)
    # Exploration only:
        # rx_presc
            # RXDATE (MM-DD-YYYY date associated with prescription)
            # DRUG_NAME
            # NDC (11-digit NDC med presciption code)
            # QUANTITY_OF_DOSE (number of med units to be taken per dose)
            # ??? (persistence/dropout)
        # visit
            # PTID
            # VISITID (? links to encounter table)
            # VISIT_START_DATE
            # VISIT_END_DATE
        # NLP
            # PHQ, PHQ-2, PHQ-9

# Claims & EMR variables needed:
    # Exploration only:
        # patient (use most recent date)
            # PTID
            # BIRTH_YR (for age)
            # GENDER (Male, Female, Unknown)
            # RACE
            # DECEASED_INDICATOR (?)
            # DATE_OF_DEATH (?)

#-----Claims Data-----

## claims med diagnosis, includes ICD codes, link to claim medical services via claim ID
claim_med_diag <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/claim_med_diag20190712.fst',
           as.data.table = T)
# claim_med_diag[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3771588        8771

## claims medical services, includes date, link to med claims via claim ID
claim_med_serv <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/claim_med_serv20190712.fst',
           as.data.table = T)
str(claim_med_serv)
## convert service date
claim_med_serv[,LST_DT_dt:=as.Date(LST_DT,format="%m-%d-%Y")] # change this variable name to keep the old. DATE is a bad variable name. What Date?
# claim_med_serv[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3295678        8771

## claims inpatient confinement, includes dates and ICD codes
claim_inpt_conf <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/claim_inp_conf20190712.fst',
           as.data.table = T)
str(claim_inpt_conf)
## convert admission date
claim_inpt_conf[,ADMIT_DATE_dt:=as.Date(ADMIT_DATE,format="%m-%d-%Y")] # change this variable name to keep the old. DATE is a bad variable name. What Date?
# claim_inpt_conf[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 18710        4674

## claims rx, includes dates and prescription info
claim_rx <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/claim_rx_20190712.fst',
           as.data.table = T)
str(claim_rx)
## convert claim date
claim_rx[,FILL_DATE:=as.Date(FILL_DT,format="%m-%d-%Y")]
# claim_rx[,list(.N,unique_pats=uniqueN(PTID))]
#          N unique_pats
# 1: 1186308        6828

#-----EMR Data-----

## Diagnosis and dates for patients
diagnosis <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/diag_20190712.fst',
           as.data.table = T)
str(diagnosis)
## convert diag date
diagnosis[,DIAG_DATE_dt:= as.Date(DIAG_DATE,format="%m-%d-%Y")]
# diagnosis[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 9416526 31178

#-----OUD Patients Data-----

## List of patients with OUD codes
oud_patients <- 
  read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/OUD_patient_list_20190712.fst',
           as.data.table = T)
# oud_patients[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 32269 32269

## patient demographics
# patient <- 
#   read_fst('Z:/Carey E/OUD_Bup_Depression/data/source_data/patient_20190712.fst',
#            as.data.table = T)
# describe(patient[,list(PTID,BIRTH_YR,GENDER,RACE,DECEASED_INDICATOR,DATE_OF_DEATH)])
## refactor code to extract the missing reason
# patient[,missing_reason:=ifelse(BIRTH_YR%in%c('Unknown','1929 and Earlier'),BIRTH_YR,NA)]
# describe(patient[complete.cases(missing_reason),])
    #   n  missing distinct 
    # 230        0        2 
    # Value      1929 and Earlier          Unknown
    # Frequency               217               13
## create birthyear variable, removing missing values
# patient[,BIRTH_YEAR:=ifelse(BIRTH_YR%in%c('Unknown','1929 and Earlier'),NA,BIRTH_YR)]
# describe(patient[,list(BIRTH_YEAR)]) # 230 missing
## convert birth year structure
# patient[,BIRTH_YEAR:=as.integer(BIRTH_YEAR)]
## create age variable
# patient[,AGE:=2019-BIRTH_YEAR]
# describe(patient[,list(AGE)])

#-----Identify Baseline Dates-----
# idenitfy date of first OUD diagnosis for each patient
# 1 - create source indicator variables 
# 2 - get all OUD diagnoses and associated dates
# 3 - combine claims and EMR data
# 4 - view source and claim type overlap
# 5 - view patients and baseline dates

## Create source indicator variables
claim_med_diag[,SOURCE:='claims']
claim_inpt_conf[,SOURCE:='claims']
claim_med_diag[,CLMTYPE:='Medical Diagnosis']
claim_inpt_conf[,CLMTYPE:='Inpatient Confinement']
diagnosis[,SOURCE:='EMR']

## claims_med_diag oud patients
# combine service date (claim_med_serv) to claim_med_diag
setkey(claim_med_diag,PTID,CLMID)
setkey(claim_med_serv,PTID,CLMID)
claim_med_diag[claim_med_serv,LST_DT_dt:=LST_DT_dt] # date is a bad variable name, what date?
str(claim_med_diag)
# identify oud patients in claim_med_diag 
setkey(oud_patients,PTID)
setkey(claim_med_diag,PTID)
claim_med_diag[oud_patients,nomatch=0]
# claim_med_diag[oud_patients,list(.N,unique_pats=uniqueN(PTID)),nomatch=0]
    #          N unique_pats
    # 1: 3771588        8771

## identify oud patients in claim_inpt_conf
setkey(claim_inpt_conf,PTID)
claim_inpt_conf[oud_patients,nomatch=0]
# claim_inpt_conf[oud_patients,list(.N,unique_pats=uniqueN(PTID)),nomatch=0]
    #        N unique_pats
    # 1: 18710        4674

## claim_inpt_conf to long
claim_inpt_conf_long <- 
  melt(claim_inpt_conf,  # I understand why you did this, but in theory you shouldn't limit to OUD patients yet...you should consider that I might have made an error and look for any patient with OUD 
       na.rm = T,
       id.vars = c('PTID','ADMIT_DATE_dt','ICD_FLAG','SOURCE','CLMTYPE'),
       measure.vars = c('DIAG1', 'DIAG2', 'DIAG3', 'DIAG4', 'DIAG5'),
       variable.name='DIAG_POSITION',
       value.name='DIAG')
# convert diag position
# claim_inpt_conf_long[,DIAG_POSITION:=as.integer(substr(DIAG_POSITION,start = 5,stop = 5))]
claim_inpt_conf_long[,DIAG_POSITION:=as.integer(gsub(pattern = 'DIAG',replacement = '',DIAG_POSITION))]
claim_inpt_conf_long[,describe(DIAG_POSITION)]
claim_inpt_conf_long[order(PTID,ADMIT_DATE_dt,DIAG_POSITION,DIAG)]
# claim_inpt_conf_long[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 93550        4674 (N=18710*5=93550 for diag position)
# convert missing diag position
claim_inpt_conf_long[,DIAG:=ifelse(DIAG=='',NA,DIAG)]
# claim_inpt_conf_long[complete.cases(DIAG),list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 82813        4674
# claim_inpt_conf_long[is.na(DIAG),list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 10737        2025
    # 10737+82813=93550 (missing DIAG check)

## combine claim_med_diag & claim_inpt_conf_long
claims <- 
  rbindlist(list(claim_inpt_conf_long[complete.cases(DIAG),list(PTID, DATE,ICD_FLAG,SOURCE, CLMTYPE,DIAG_POSITION,DIAG)],
                 claim_med_diag[oud_patients,
                                list(PTID,DATE,DIAG_POSITION,DIAG,ICD_FLAG,SOURCE,CLMTYPE),
                                nomatch=0]), 
            use.names=TRUE,fill=TRUE)
# claims[,uniqueN(PTID)]
    # 8771
# claims[CLMTYPE=='Inpatient Confinement',list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 82813        4674
# claims[CLMTYPE=='Medical Diagnosis',list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3771588        8771

## identify oud patients in claim_rx
setkey(claim_rx,PTID)
claim_rx[oud_patients,nomatch=0]
# claim_rx[oud_patients,list(.N,unique_pats=uniqueN(PTID)),nomatch=0]
    #          N unique_pats
    # 1: 1189771        6846

## identify oud patients in EMR (diagnosis)
setkey(diagnosis,PTID)
diagnosis[oud_patients,nomatch=0]
# diagnosis[oud_patients,list(.N,unique_pats=uniqueN(PTID)),nomatch=0]
    #          N unique_pats
    # 1: 9416526       31178

## Combined Claims & EMR
# change diag variable name
setnames(diagnosis,'DIAGNOSIS_CD','DIAG')
diagnosis[order(PTID,DATE,DIAG,DIAGNOSIS_CD_TYPE,SOURCE)]
str(diagnosis)
unique(diagnosis[,list(DIAGNOSIS_CD_TYPE)])
# create ICD_FLAG
diagnosis[,ICD_FLAG := ifelse(DIAGNOSIS_CD_TYPE %in% c('ICD9','ICD10'),
                            as.integer(substr(DIAGNOSIS_CD_TYPE,4,5)),NA)]
    # Warning message:
    #   In ifelse(DIAGNOSIS_CD_TYPE %in% c("ICD9", "ICD10"), as.integer(substr(DIAGNOSIS_CD_TYPE, : NAs introduced by coercion
# check ICD flag conversion
    # head(diagnosis,30)
    # diagnosis[sample(.N,size = .1*.N)]
    # diagnosis[is.na(ICD_FLAG),]
# convert DIAG==''
diagnosis[DIAG=='',list(.N)]
diagnosis[DIAG=='',DIAG:=NA]

## combine claims and EMR (removed SNOWMED,OTHER,UNKNOWN diagnosis code types & where DIAG=='')
baseline <- 
  na.omit(rbind(diagnosis[oud_patients,list(PTID,DATE,DIAG,DIAGNOSIS_CD_TYPE,ICD_FLAG,SOURCE),nomatch=0],
        claims, use.names = TRUE, fill = TRUE),cols=c('DIAG','ICD_FLAG'))
# baseline[DIAG=='',list(.N)]
# baseline[sample(.N,15)]
## DIAGNOSIS_CD_TYPE for claims
baseline[SOURCE=='claims',unique(ICD_FLAG)]
baseline[SOURCE=='claims',DIAGNOSIS_CD_TYPE:=ifelse(ICD_FLAG==9,'ICD9','ICD10')]
# baseline[sample(.N,15)]

## check unique patients by source
baseline[SOURCE=='EMR',list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 9255095       31176
baseline[SOURCE=='claims',list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 3854401        8771

## View overlap
# CLMTYPE overlap
describe(claims[,list(unique_clm_type=uniqueN(CLMTYPE)),
       by=PTID])
# unique_clm_type 
    #    n  missing distinct    
    # 8771        0        2    
# Value          1     2
# Frequency   4097  4674
# Proportion 0.467 0.533
    # 4674 claims under both claim types
# SOURCE overlap
describe(baseline[,list(unique_source=uniqueN(SOURCE)),
                by=PTID])
# unique_source 
    #     n  missing distinct    
    # 32269        0        2     
# Value          1     2
# Frequency  24591  7678
# Proportion 0.762 0.238
    # 7678 under both EMR & claims

## Identify first OUD diagnosis
oud_diag <- 
  fread('Z:/Carey E/OUD_Bup_Depression/background_notes/ICD9_ICD10_Opioid Related Diagnoses_v2.csv')
oud_diag[,DIAG:=gsub('\\.','',Value)]
setkey(oud_diag,DIAG)
setkey(baseline,DIAG)
baseline[oud_diag,COVARIATE:=Covariate]
# baseline[COVARIATE=='OUD',list(.N,uniqueN(PTID))]
    #         N    V2
    # 1: 360751 32269 (consistent with oud_patients)
# baseline[sample(.N,size=.N*.10)]
base_date <- baseline[COVARIATE=='OUD',list(FIRST_OUD=min(DATE)),by=PTID]
# add baseline date to 
setkey(baseline,PTID)
setkey(base_date,PTID)
baseline[base_date,FIRST_OUD:=FIRST_OUD]

# Next steps:
    # Check for 2 years washout prior to OUD diag
    # Identify patients with prior DEP (one inpatient or two outpatient diagnoses)

#-----Outputs-----

write.fst(baseline,
          path = paste0('Z:/Carey E/OUD_Bup_Depression/data/created_data/baseline_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
