######################################################################
## Title: 01_med_exclusions.R                                         
## Author: Charysse Gibson
## Date created: July 31, 2019
######################################################################
# Purpose: Extract Patients with medication exclusions
######################################################################
# Inputs: cohort_20190725.fst
#         claim_rx_20190712.fst
#         emr_rx_20190801.fst
#         background_notes/NDC_BUP_MTD_2019.csv
# outputs: med_excl_20190808.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Medication Exclusions:
# Buprenorphine treatment prior to first OUD 
# Methadone treatment at any time during cohort and follow-up period

#-----Dataset & Variables Needed-----

# cohort
    # PTID
    # FIRST_OUD

# claim_rx (claims prescriptions)
    # PTID
    # CLM_ID
    # FILL_DT
    # NDC (national drug codes)
    # STRENGTH

# emr_rx
    # PTID
    # DATE
    # NDC
    # STRENGTH
    # STRENGTH_UNIT

# NDC_BUP_MTD_2019
    # Treatment
    # Proprietary Name
    # Nonproprietary Name
    # NDC

#-----Inputs-----

## cohort dataset
cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
    #        N unique_pats
    # 1: 21757       21757

## claim_rx dataset
claim_rx <- 
  read_fst('data/source_data/claim_rx_20190712.fst',
           as.data.table = T)
# claim_rx[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 1189771        6846
str(claim_rx)
# convert fill date
claim_rx[,fill_date:=as.Date(FILL_DT,format="%m-%d-%Y")]
claim_rx[!is.na(STRENGTH),list(STRENGTH)]
describe(claim_rx[,list(PTID,fill_date,NDC,STRENGTH)])

## emr_rx
emr_rx <- 
  read_fst('data/created_data/emr_rx_20190801.fst',
           as.data.table = T)
# emr_rx[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 2827797       25611
str(emr_rx)

## combine claim_rx and emr_rx
claim_rx[,DATE:=fill_date]
emr_rx[,STRENGTH:=emr_strength]
setkey(claim_rx,PTID,DATE,NDC,STRENGTH)
setkey(emr_rx,PTID,DATE,NDC,STRENGTH)
all_rx <-
  unique(rbind(claim_rx[,list(PTID,DATE,NDC,STRENGTH)],
      emr_rx[,list(PTID,DATE,NDC,STRENGTH)]),by=c('PTID','DATE','NDC'))
# all_rx[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 4014212       28029

## only patients with OUD diag (cohort)
setkey(cohort,PTID)
setkey(all_rx,PTID)
cohort_rx <- 
  all_rx[cohort,list(PTID,DATE,NDC,STRENGTH)]

#-----Identify prescriptions-----

## import NDC codes for BUP and MTD
ndc_bup_mtd <-
  fread(file = 'background_notes/NDC_BUP_MTD_2019.csv')
str(ndc_bup_mtd)
# convert NDC
ndc_bup_mtd[,NDC:=as.character(NDC)]
describe(ndc_bup_mtd[,list(Treatment)])
# Value        BUP   MTD
# Frequency    208   141

## identify BUP and MTD treatments 
setkey(ndc_bup_mtd,NDC)
setkey(cohort_rx,NDC)
cohort_rx[ndc_bup_mtd,treatment:=Treatment]
describe(cohort_rx[treatment=='BUP'])
# PTID
    #     n  missing distinct 
    # 18184        0     2358 
# NDC 
    #     n  missing distinct 
    # 18184        0       51 
describe(cohort_rx[treatment=='MTD'])
# PTID 
    #     n  missing distinct 
    # 13932        0     2561 
# NDC 
    #     n  missing distinct 
    # 13932        0       36 

## explore missing data
describe(cohort_rx[,list(DATE,NDC)])
# DATE 
    #       n  missing distinct 
    # 3019567     2544     4383 
# NDC 
    #       n  missing distinct 
    # 3019567     2544    32062 
    # includes NONE and UNK (unknown)
cohort_rx[!is.na(DATE)&NDC!='NONE'&NDC!='UNK',list(.N,unique_pats=uniqueN(PTID))]
#          N unique_pats
# 1: 3019215       19213

#-----Extract patients with BUP treatment prior to first OUD----- 
# 1 - identify first OUD date by PTID
# 2 - create indicator if BUP treatment is prior to first OUD
# 3 - sum indicator by PTID
# 4 - if summed indicator > 1, then BUP prior to first OUD

## identify first oud date by PTID
setkey(cohort_rx,PTID)
setkey(cohort,PTID)
cohort_rx[cohort,FIRST_OUD:=FIRST_OUD]

## create sum of prior BUP treatment indicator by PTID
cohort_rx[treatment=='BUP',BUP_OUD_DIFF:=ifelse(DATE<FIRST_OUD,1,0)]
cohort_rx[treatment=='BUP']

## sum indicator by PTID
bup_sum <-
  cohort_rx[treatment=='BUP',list(SUM_DIFF=sum(BUP_OUD_DIFF)),by=PTID]
describe(bup_sum)
# bup_sum[SUM_DIFF=='88'] #PT326235530
# cohort_rx[PTID=='PT326235530'&treatment=='BUP']
    # patient had 88 BUP treatments before first OUD diag, 5 after

## if summed indicator > 1, then BUP prior to first OUD
setkey(cohort,PTID)
setkey(bup_sum,PTID)
cohort[bup_sum,SUM_DIFF:=SUM_DIFF]
cohort[!is.na(SUM_DIFF)]
cohort[,PRIOR_BUP:=ifelse(SUM_DIFF>0,1,0)]
cohort[PRIOR_BUP=='1'] #477 to be removed with BUP treatment prior to first OUD

## check patients without BUP treatment prior to first OUD
uniqueN(cohort[PRIOR_BUP!='1'|is.na(PRIOR_BUP)]) #21280 (21757-21280=477)

## clean list of cohort patients with prior BUP treatment indicator
prior_bup <- 
  cohort[,list(PTID,FIRST_OUD,PRIOR_BUP)]

#-----Extract patients any MTD treatment during 2 yr washout & follow-up-----
# 1 - identify patient MTD treatments and dates
# 2 - combine MTD treatments with patient first OUD
# 3 - identify date difference between MTD treatment and first OUD
# 4 - create indicator if BUP treatment is within 2 yrs prior to or after first OUD
# 5 - subset those with MTD treatment and find unique PTID
# 5 - combine MTD treatment indicator to prior_bup data

## identify patient MTD treatments and dates 
setkey(cohort_rx,PTID,treatment)
mtd_treatments <-
  cohort_rx[treatment=='MTD',list(PTID,treatment,DATE)]

## combine MTD treatments with first OUD
setkey(cohort,PTID)
setkey(mtd_treatments,PTID)
mtd_treatments[cohort,FIRST_OUD:=FIRST_OUD]

## identify date difference between MTD treatment and first OUD
mtd_treatments[,date_diff:=difftime(DATE,FIRST_OUD,units = 'days')]

## indicator if BUP treatment is within baseline and follow-up time period 
mtd_treatments[,MTD_TREATMENT:=ifelse(date_diff>-730&date_diff<730,1,NA)]
# mtd_treatments[is.na(MTD_TREATMENT)] #3250
# mtd_treatments[MTD_TREATMENT==1,list(.N,unique_pats=uniqueN(PTID))] #10682
    #        N unique_pats
    # 1: 10682        2249

## find unique PTID for those MTD treated
mtd_patients <- 
  unique(mtd_treatments[MTD_TREATMENT==1,list(PTID,MTD_TREATMENT)],by='PTID')

## combine MTD treatment indicator to prior_bup data
setkey(prior_bup,PTID)
setkey(mtd_patients,PTID)
prior_bup[mtd_patients,MTD_TREATMENT:=MTD_TREATMENT]

#-----Outputs-----

write.fst(prior_bup,
       path = paste0('data/created_data/med_excl_',
                     gsub('-','',Sys.Date()),'.fst'),
       compress = 100)
