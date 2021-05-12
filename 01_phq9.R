######################################################################
## Title: 01_phq9.R                                         
## Author: Charysse Gibson
## Date created: July 17, 2019
######################################################################
# Purpose: Identify cohort PHQ-9 measurements
######################################################################
# Inputs: cohort_20190725.fst
# outputs: phq9_20190822.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

#-----Dataset & Variables Needed-----

# cohort (patients with oud diag & baseline dates)
    # PTID
    # FIRST_OUD

# nlp_measure (PHQ-9 Data)
    # PTID
    # MEASUREMENT_TYPE
       
#-----Inputs-----

## cohort (patients with oud diag & baseline dates)
cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
#        N unique_pats
# 1: 21757       21757

## nlp_measure (PHQ-9 data)
nlp_measure <- 
  read_fst('data/source_data/nlp_measure_20190712.fst',
           as.data.table = T)
nlp_measure[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 9914123       24321
phq9 <-
  nlp_measure[grepl('phq-9',MEASUREMENT_TYPE,ignore.case = T)]
phq9[,list(.N,unique_pats=uniqueN(PTID))]
    #       N unique_pats
    # 1: 6572        1774

## check PHQ-9 dates
str(phq9)
phq9[,note_date:=as.Date(NOTE_DATE,format="%m-%d-%Y")]
phq9[,measure_date:=as.Date(MEASUREMENT_DATE,format='%m-%d-%Y')]
# NOTE_DATE may be similar to MEASUREMENT_DATE (but not exact)
phq9[,diff:=note_date-measure_date]
phq9[!is.na(diff)&diff>=1] #218
phq9[!is.na(diff)&diff>90] #81
phq9[!is.na(diff)&diff>365] #15
# some note and measurement dates differ by over 1 year
# using only measurement dates as valid measure dates

## check PHQ-9 missing dates %
phq9[!is.na(measure_date)]
describe(phq9[,list(measure_date)])
# measure_date
    #   n  missing distinct 
    # 704     5868      428 

#-----Extract Cohort PHQ-9 Values-----
# 1 - aggregate first OUD dates
# 2 - sort by PTID and measurement dates
# 3 - subset to most recent PHQ-9 date prior to baseline
# 4 - aggregate PHQ-9 dates to cohort
# 5 - retrieve measurement value

## aggregate first OUD dates
setkey(cohort,PTID)
setkey(phq9,PTID)
phq9[cohort,FIRST_OUD:=FIRST_OUD]
describe(phq9[!is.na(FIRST_OUD),list(measure_date)])
    #   n  missing distinct 
    # 502     4157      317

## sort by PTID and measurement date
setkey(phq9,PTID,MEASUREMENT_DATE)

## subset to most recent PHQ-9 date prior to baseline
cohort_phq9 <-
  phq9[measure_date<FIRST_OUD,list(measure_date=max(measure_date)),by=PTID]

## aggregate PHQ-9 dates to cohort
setkey(cohort,PTID)
setkey(cohort_phq9,PTID)
cohort[cohort_phq9,measure_date:=measure_date]
cohort[!is.na(measure_date)] #71 valid PHQ-9 patient dates

## retrieve measurement value
setkey(cohort,PTID,measure_date)
setkey(phq9,PTID,measure_date)
cohort[phq9,PHQ9_SCORE:=MEASUREMENT_VALUE]
cohort[,list(PTID,FIRST_OUD,PHQ9_DATE=measure_date,PHQ9_SCORE)]

#-----Group PHQ-9 Scores-----

setkey(cohort,PHQ9_SCORE)
dput(unique(cohort$PHQ9_SCORE))
cohort[PHQ9_SCORE %in% c("better", "chronically elevated", "controlled", "elevated", "extremely difficult", 
       "high", "low", "more than half", "n/a", "negative", "normal", 
       "not performed", "positive", "score a", "somewhat difficult", 
       "unassessable", "very difficult", "within normal limits", "worse",
       "10;9","113297","12/14/2017;27","121","15;18","7;1","30","50"),PHQ9:='unknown']
cohort[is.na(PHQ9),list(unique(PHQ9_SCORE))]
cohort[is.na(PHQ9),PHQ9:=PHQ9_SCORE]
cohort[PHQ9=='unknown',PHQ9:=NA]
cohort[,PHQ9:=as.numeric(PHQ9)]
cohort[PHQ9>=0&PHQ9<5,':='(PHQ9_SCORE_CAT='0-4',
                           PHQ9_DEP_SEV='None-minimal')]
cohort[PHQ9>=5&PHQ9<10,':='(PHQ9_SCORE_CAT='5-9',
                           PHQ9_DEP_SEV='Mild')]
cohort[PHQ9>=10&PHQ9<15,':='(PHQ9_SCORE_CAT='10-14',
                            PHQ9_DEP_SEV='Moderate')]
cohort[PHQ9>=15&PHQ9<20,':='(PHQ9_SCORE_CAT='15-19',
                            PHQ9_DEP_SEV='Moderately Severe')]
cohort[PHQ9>=20&PHQ9<28,':='(PHQ9_SCORE_CAT='20-27',
                            PHQ9_DEP_SEV='Severe')]
cohort[!is.na(PHQ9)]
# NOTE: If score was 9.5, patient was put in categories '5-9' and 'Mild' (did not round up)

## order category factors
cohort[,PHQ9_SCORE_CAT:=factor(PHQ9_SCORE_CAT, ordered=TRUE,
                               levels=c('0-4','5-9','10-14','15-19','20-27'))]
cohort[,PHQ9_DEP_SEV:=factor(PHQ9_DEP_SEV, ordered=TRUE,
                               levels=c('None-minimal','Mild','Moderate',
                                        'Moderately Severe','Severe'))]

##-----Outputs-----

write.fst(cohort[,list(PTID,FIRST_OUD,PHQ9_DATE=measure_date,PHQ9_SCORE,PHQ9_SCORE_CAT,PHQ9_DEP_SEV)],
          path = paste0('data/created_data/phq9_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)

