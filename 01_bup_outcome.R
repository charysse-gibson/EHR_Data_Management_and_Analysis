######################################################################
## Title: 01_bup_outcome.R                                         
## Author: Charysse Gibson
## Date created: August 8, 2019
######################################################################
# Purpose: Identify patient outcome (buprenorphine treatment post OUD)
######################################################################
# Inputs: cohort_20190725.fst
#         claim_rx_20190712.fst
#         emr_rx_20190801.fst
#         NDC_BUP_MTD_2019.csv
# outputs: bup_outcome_20190822.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Outcome Defined: 
# Buprenorphine treatment post OUD diagnosis (within follow-up period)
# Include starting dosage and treatment dropout

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

#-----Identify Prescriptions-----

## import NDC codes for BUP
ndc_bup_mtd <-
  fread(file = 'background_notes/NDC_BUP_MTD_2019.csv')
ndc_bup_mtd[,NDC:=as.character(NDC)]
# describe(ndc_bup_mtd[,list(Treatment)])
    # Value        BUP   MTD
    # Frequency    208   141

## identify BUP treatments 
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
ndc_bup_mtd[Treatment=='BUP',list(BUP_Proprietary_Names=unique(tolower(Proprietary_Name)))]

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

#-----Identify Buprenorphine Outcome-----
# 1 - aggregate first OUD date to cohort_rx
# 2 - identify first BUP treatments after first OUD
# 3 - extract strength (starting dosage)
# 4 - create follow-up period cut-off
# 5 - create indicate variable
# 6 - treatment persistance/retention? dropout?

## aggregate first OUD date to cohort_rx
setkey(cohort,PTID)
setkey(cohort_rx,PTID)
cohort_rx[cohort,FIRST_OUD:=FIRST_OUD]

## subset to only BUP and after first OUD
cohort_bup <- 
  cohort_rx[treatment=='BUP'&DATE>FIRST_OUD,list(DATE=min(DATE)),by=PTID]

## extract starting dosage
setkey(cohort_rx,PTID,DATE)
setkey(cohort_bup,PTID,DATE)
cohort_bup[cohort_rx,':=' (FIRST_OUD=FIRST_OUD,
                           STRENGTH=STRENGTH)]
cohort_bup[is.na(STRENGTH)] #537

## create follow-up period cut-off
bup_outcome_1yr <-
  cohort_bup[DATE>FIRST_OUD&DATE<(FIRST_OUD+(1*365))]
bup_outcome_2yr <-
  cohort_bup[DATE>FIRST_OUD&DATE<(FIRST_OUD+(2*365))]
bup_outcome_3yr <-
  cohort_bup[DATE>FIRST_OUD&DATE<(FIRST_OUD+(3*365))]

## create indicator variable
setkey(cohort,PTID)
setkey(bup_outcome_1yr,PTID)
setkey(bup_outcome_2yr,PTID)
setkey(bup_outcome_3yr,PTID)
cohort[bup_outcome_1yr,':='(BUP_OUTCOME_1YR=1)]
cohort[bup_outcome_2yr,':='(BUP_OUTCOME_2YR=1)]
cohort[bup_outcome_3yr,':='(BUP_OUTCOME_3YR=1,BUP_START_DOSE=STRENGTH)]
cohort[!is.na(BUP_OUTCOME_1YR)] # 1491
cohort[!is.na(BUP_OUTCOME_2YR)] # 1711
cohort[!is.na(BUP_OUTCOME_3YR)] # 1818

cohort[is.na(BUP_OUTCOME_1YR),BUP_OUTCOME_1YR:=0]
cohort[is.na(BUP_OUTCOME_2YR),BUP_OUTCOME_2YR:=0]
cohort[is.na(BUP_OUTCOME_3YR),BUP_OUTCOME_3YR:=0]

#-----BUP OUTCOME TESTS-----
bup_outcome_5yr <-
  cohort_bup[DATE>FIRST_OUD&DATE<(FIRST_OUD+(5*365))]
setkey(bup_outcome_5yr,PTID)
cohort[bup_outcome_5yr,':='(BUP_OUTCOME_5YR=1)]
cohort[!is.na(BUP_OUTCOME_5YR)] # 1925

bup_outcome_10yr <-
  cohort_bup[DATE>FIRST_OUD&DATE<(FIRST_OUD+(10*365))]
setkey(bup_outcome_10yr,PTID)
cohort[bup_outcome_10yr,':='(BUP_OUTCOME_10YR=1)]
cohort[!is.na(BUP_OUTCOME_10YR)] # 1987

#-----Outputs----- 

write.fst(cohort,
          path = paste0('data/created_data/bup_outcome_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
