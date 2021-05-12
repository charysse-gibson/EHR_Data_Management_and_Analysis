######################################################################
## Title: 01_demographics.R                                         
## Author: Charysse Gibson
## Date created: August 8, 2019
######################################################################
# Purpose: Identify patient demographics information
######################################################################
# Inputs: cohort_20190725.fst
#         patient_20190712.fst
# outputs: 
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Demographic Information Needed: 
# Age
# Sex
# Race

#-----Dataset & Variables Needed-----

# Cohort
    # PTID
    # FIRST_OUD

# patient
    # PTID
    # BIRTH_YR (for age)
    # GENDER (Male, Female, Unknown)
    # RACE
    # DATE_OF_DEATH (?)

#-----Inputs-----

## cohort dataset
cohort <- 
  read_fst('data/created_data/cohort_20190725.fst',
           as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
#        N unique_pats
# 1: 21757       21757

## patient demographics
patient <-
  read_fst('data/source_data/patient_20190712.fst',
           as.data.table = T)
describe(patient[,list(PTID,BIRTH_YR,GENDER,RACE,DECEASED_INDICATOR,DATE_OF_DEATH)])
# refactor code to extract the missing reason
patient[,missing_reason:=ifelse(BIRTH_YR%in%c('Unknown','1929 and Earlier'),BIRTH_YR,NA)]
describe(patient[complete.cases(missing_reason),])
#   n  missing distinct 
# 230        0        2 
# Value      1929 and Earlier          Unknown
# Frequency               217               13
## create birthyear variable, removing missing values
patient[,BIRTH_YEAR:=ifelse(BIRTH_YR%in%c('Unknown','1929 and Earlier'),NA,BIRTH_YR)]
describe(patient[,list(BIRTH_YEAR)]) # 230 missing
# convert birth year structure
patient[,BIRTH_YEAR:=as.integer(BIRTH_YEAR)]
# create age variable
patient[,AGE:=2019-BIRTH_YEAR]
describe(patient[,list(AGE,GENDER,RACE)])

#-----Identify Patient Demographics Information-----

setkey(cohort,PTID)
setkey(patient,PTID)
cohort[patient,':=' (AGE=AGE, 
                     GENDER=GENDER,
                     RACE=RACE)]
describe(cohort[,.(AGE,GENDER,RACE)])
cohort[GENDER=='Unknown',GENDER:=NA]
cohort[RACE=='Other/Unknown',RACE:=NA]
cohort[AGE>=18&AGE<65] #18382

#-----Outputs----- 

write.fst(cohort,
          path = paste0('data/created_data/demogr_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
