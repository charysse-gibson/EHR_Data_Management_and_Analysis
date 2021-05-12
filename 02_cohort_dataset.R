######################################################################
## Title: 02_cohort_dataset.R                                         
## Author: Charysse Gibson
## Date created: August 8, 2019
######################################################################
# Purpose: Combine all cohort criteria indicators and information
#          Identify eligible patients based on criteria
######################################################################
# Inputs: bup_outcome_20190822.fst
#         cohort_20190725.fst
#         cov_20190820.fst
#         demogr_20190809
#         dep_exp_20190819.fst
#         follow_up_20190819.fst
#         med_excl_20190808.fst
#         phq9_20190822.fst
#         washout_20190730.fst
# outputs: cohort_dataset_20190821.fst
#          eligible_cohort_20190821.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

#-----Dataset & Variables Needed-----

# bup_outcome
    # PTID
    # BUP_OUTCOME
    # BUP_START_DOSE

# cohort
    # PTID
    # FIRST_OUD

# covariates
    # ALCOHOL
    # ANXIETY
    # ARTHRITIS
    # BACK_PAIN
    # HEADACHE
    # ILLICIT_DRUG
    # MUSC_PAIN
    # NEUROPATHY
    # NICOTINE
    # PTSD

# demogr
    # PTID
    # AGE
    # GENDER
    # RACE

# dep_exp
    # PTID
    # PRIOR_DEP
    # POST_DEP

# follow_up
    # PTID
    # FOLLOW_UP

# med_excl
    # PTID
    # PRIOR_BUP
    # MTD_TREATMENT

# washout
    # PTID
    # VISIT_YR1
    # VISIT_YR2
    # WASHOUT_2YR

# health_care_util

# phq9

# bup_outcome

#-----Inputs-----

bup_outcome <-
  read_fst('data/created_data/bup_outcome_20190822.fst',
           as.data.table = T)

cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)

covariates <-
  read_fst('data/created_data/cov_20190820.fst',
           as.data.table = T)

demogr <- 
  read_fst('data/created_data/demogr_20190809.fst',
           as.data.table = T)

dep_exp <- 
  read_fst('data/created_data/dep_exp_20190819.fst',
           as.data.table = T)

follow_up <- 
  read_fst('data/created_data/follow_up_20190820.fst',
           as.data.table = T)

health_care_util <-
  read_fst('data/created_data/health_care_util_20190820.fst',
           as.data.table = T)

med_excl <- 
  read_fst('data/created_data/med_excl_20190808.fst',
           as.data.table = T)

phq9 <-
  read_fst('data/created_data/phq9_20190822.fst',
           as.data.table = T)

washout <- 
  read_fst('data/created_data/washout_20190730.fst',
           as.data.table = T)

#-----Combine Cohort Data-----

setkey(bup_outcome,PTID)
setkey(cohort,PTID)
setkey(covariates,PTID)
setkey(demogr,PTID)
setkey(dep_exp,PTID)
setkey(follow_up,PTID)
setkey(med_excl,PTID)
setkey(phq9,PTID)
setkey(washout,PTID)

cohort[bup_outcome,':='(BUP_OUTCOME_1YR=BUP_OUTCOME_1YR,
                        BUP_OUTCOME_2YR=BUP_OUTCOME_2YR,
                        BUP_OUTCOME_3YR=BUP_OUTCOME_3YR,
                        BUP_OUTCOME_5YR=BUP_OUTCOME_5YR,
                        BUP_OUTCOME_10YR=BUP_OUTCOME_10YR,
                        BUP_START_DOSE=BUP_START_DOSE)]
cohort[demogr,':='(AGE=AGE,
                   GENDER=GENDER,
                   RACE=RACE)]
cohort[covariates,':='(ALCOHOL=ALCOHOL,
                       ANXIETY=ANXIETY,
                       ARTHRITIS=ARTHRITIS,
                       BACK_PAIN=BACK_PAIN,
                       HEADACHE=HEADACHE,
                       ILLICIT_DRUG=ILLICIT_DRUG,
                       MUSC_PAIN=MUSC_PAIN,
                       NEUROPATHY=NEUROPATHY,
                       NICOTINE=NICOTINE,
                       PTSD=PTSD)]
cohort[dep_exp,':='(PRIOR_DEP=PRIOR_DEP,
                    POST_DEP=POST_DEP)]
cohort[follow_up,FOLLOW_UP:=FOLLOW_UP]
cohort[health_care_util,HEALTH_CARE_ENC:=ifelse(!is.na(HEALTH_CARE_ENC),HEALTH_CARE_ENC,0)]
cohort[med_excl,':='(PRIOR_BUP=PRIOR_BUP,
                     MTD_TREATMENT=MTD_TREATMENT)]
cohort[phq9,':='(PHQ9_DATE=PHQ9_DATE,
                 PHQ9_SCORE=PHQ9_SCORE,
                 PHQ9_SCORE_CAT=PHQ9_SCORE_CAT,
                 PHQ9_DEP_SEV=PHQ9_DEP_SEV)]
cohort[washout,':='(VISIT_YR1=VISIT_YR1,
                    VISIT_YR2=VISIT_YR2,
                    WASHOUT_2YR=WASHOUT_2YR)]

#-----Exclusion Critera-----

eligible_cohort <- cohort[AGE>17&AGE<65&
                 WASHOUT_2YR==1&
                 is.na(PRIOR_BUP)&
                 FOLLOW_UP==1&
                 is.na(MTD_TREATMENT)&
                 is.na(POST_DEP)&
                 !is.na(RACE)&
                 !is.na(GENDER)]

eligible_cohort[is.na(BUP_OUTCOME_1YR),BUP_OUTCOME_1YR:=0]
eligible_cohort[is.na(BUP_OUTCOME_2YR),BUP_OUTCOME_2YR:=0]
eligible_cohort[is.na(BUP_OUTCOME_3YR),BUP_OUTCOME_3YR:=0]
eligible_cohort[is.na(BUP_OUTCOME_5YR),BUP_OUTCOME_5YR:=0]
eligible_cohort[is.na(BUP_OUTCOME_10YR),BUP_OUTCOME_10YR:=0]

eligible_cohort[is.na(ALCOHOL),ALCOHOL:=0]
eligible_cohort[is.na(ANXIETY),ANXIETY:=0]
eligible_cohort[is.na(ARTHRITIS),ARTHRITIS:=0]
eligible_cohort[is.na(BACK_PAIN),BACK_PAIN:=0]
eligible_cohort[is.na(HEADACHE),HEADACHE:=0]
eligible_cohort[is.na(ILLICIT_DRUG),ILLICIT_DRUG:=0]
eligible_cohort[is.na(MUSC_PAIN),MUSC_PAIN:=0]
eligible_cohort[is.na(NEUROPATHY),NEUROPATHY:=0]
eligible_cohort[is.na(NICOTINE),NICOTINE:=0]
eligible_cohort[is.na(PTSD),PTSD:=0]

eligible_cohort[,list(.N,unique_pats=uniqueN(PTID))]
    #       N unique_pats
    # 1: 3888        3888

describe(eligible_cohort[,list(PRIOR_DEP)])
    # Value          0     1
    # Frequency   2222  1666
    # Proportion 0.572 0.428

## check BUP starting doses
cohort[!is.na(BUP_START_DOSE)] # only 1337 of BUP treated patients have starting dosage
eligible_cohort[!is.na(BUP_START_DOSE)] # none with starting dosage in eligible cohort

## check BUP outcome
summary(eligible_cohort[,BUP_OUTCOME_1YR])
summary(eligible_cohort[,BUP_OUTCOME_2YR])
summary(eligible_cohort[,BUP_OUTCOME_3YR])
summary(eligible_cohort[,BUP_OUTCOME_5YR])
summary(eligible_cohort[,BUP_OUTCOME_10YR])

#-----Outputs----- 

write.fst(cohort,
          path = paste0('data/created_data/cohort_dataset_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)

write.fst(eligible_cohort,
          path = paste0('data/created_data/eligible_cohort_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
