######################################################################
## Title: 01_covariates.R                                         
## Author: Charysse Gibson
## Date created: August 8, 2019
######################################################################
# Purpose: Identify patient covariate data
######################################################################
# Inputs: cohort_20190725.fst
#         icd9toicd10cmgem.csv
# outputs: cov_icd9_icd10_codes.csv
#          cov_20190820.fst
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

# Covariates Defined: 
# 2 outpatient or 1 inpatient diagnoses 
# within washout to baseline time period

# List of covariates:
    # Painful Condition
        # Athritis
        # Back pain
        # Headache
        # Musculoskeletal pain
        # Neuropathic pain
    # Psychiatric Comorbidity
        # PTSD
        # Other anxiety
        # Nicotine dependence/history of smoking
        # Alcohol abuse/dependence
        # Any illicit drug abuse/dependence

#-----Dataset & Variables Needed-----

# ICD9_ICD10_Crosswalk
        # icd9cm
        # icd10cm

# cohort
        # PTID
        # FIRST_OUD

# cohort_diag
        # PTID
        # DATE
        # DIAG
        # care_type
        # SOURCE
        # covariate

#-----Inputs-----

## ICD-9-CM & ICD-10-CM crosswalk (general equivalence mappings)
ICD9_ICD10 <-
    fread(file = "background_notes/icd9toicd10cmgem.csv")
    
## cohort (patients with oud diag & baseline dates)
cohort <- 
    read_fst('data/created_data/cohort_20190725.fst',
             as.data.table = T)
# cohort[,list(.N,unique_pats=uniqueN(PTID))]
        #        N unique_pats
        # 1: 21757       21757

## cohort_diag (cohort patient diagnoses)
cohort_diag <-
    read_fst('data/created_data/cohort_diag_20190725.fst',
             as.data.table = T)
# cohort_diag[,list(.N,unique_pats=uniqueN(PTID))]
        #        N unique_pats
        # 1: 6784206     21757

#-----Identify Covariate Codes-----

## PTSD
PTSD <-
    rbind(unique(ICD9_ICD10[icd9cm=='30981',list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[startsWith(icd10cm,'F431'),list(DIAG=icd10cm)]))
PTSD[,Covariate:='Posttraumatic stress disorder']
    
## Anxiety
Anxiety <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('30002',
                                          '30001',
                                          '3003',
                                          '30023',
                                          '300',
                                          '3000',
                                          '30000'),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('F411',
                                          'F410',
                                          'F42',
                                          'F419')|
                                startsWith(icd10cm,'F401'),list(DIAG=icd10cm)]))
# regex stuff
# temp1 <- c('the','too','roo')
# grep('^to',x=temp1,value = T)
# grepl('^to',x=temp1)
# grep('oo',x=temp1,value = T)

Anxiety[,Covariate:='Other anxiety']   
    
## Nicotine dependence/history of smoking
Nicotine <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('V1582','3051'),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('Z87891','Z720','F17228')|
                                startsWith(icd10cm,'F1720')|
                                startsWith(icd10cm,'F1721'),list(DIAG=icd10cm)]))
Nicotine[,Covariate:='Nicotine']   

## Alcohol abuse/dependence
Alcohol <-
    rbind(unique(ICD9_ICD10[like(icd9cm,'3039')|
                   startsWith(icd9cm,'3050'),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[startsWith(icd10cm,'F10'),list(DIAG=icd10cm)]))
Alcohol[,Covariate:='Alcohol']

# # Any illicit drug abuse/dependence
Illicit_Drug <-
    rbind(unique(ICD9_ICD10[Reduce('|', Map('startsWith', list(icd9cm),
                                            c('3040','3055',
                                              '3041','3054',
                                              '3042','3056',
                                              '3043','3052',
                                              '3044','3057',
                                              '3045','3053',
                                              '3046','3059',
                                              '3047','3048','3049'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[Reduce('|', Map('startsWith', list(icd10cm),
                                            c('F10','F11','F13',
                                              'F14','F12','F15',
                                              'F16','F18','F11',
                                              'F19'))),list(DIAG=icd10cm)]))
Illicit_Drug[,Covariate:='Illicit drug']

## Arthritis
Arthritis <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('7100','7101','7102','7103','7104',
                                          '7108','7109','7200','V134')|
                                Reduce('|', Map('%like%', list(icd9cm),
                                                c('711','713','714','715',
                                                  '716','717','7180','7181', 
                                                  '7182','7183','7185','7186', 
                                                  '7187','7188','7189','719'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('M3210','M3320','M3390','M340','M341',
                                           'M349','M3500','M3501','M355','M359',
                                           'M362','M363','M364','M434','M459',
                                           'R262','R294','R29898','Z8739')|
                                Reduce('|', Map('%like%', list(icd10cm),
                                                c('M00','M01','M02','M05',
                                                  'M06','M08','M12','M13',
                                                  'M14','M15','M16','M17', 
                                                  'M18','M19','M23','M240',
                                                  'M241','M242','M243','M244',
                                                  'M246','M247','M248','M249',
                                                  'M25','M435','M796'))),list(DIAG=icd10cm)]))
Arthritis[,Covariate:='Arthritis']
 
## Back pain
Back_Pain <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('7201','7202','7209',
                                          '7230','7231','7232','7233',
                                          '7235','7236','7237','7239')|
                                Reduce('|', Map('startsWith', list(icd9cm),
                                                c('7208','721','722',
                                                  '724','7561'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('M436','M439','M461',
                                           'M489','M961','Q760',
                                           'Q761','Q762','Q763','Q766')|
                                Reduce('|', Map('startsWith', list(icd10cm),
                                                c('M432','M438','M460','M464',
                                                  'M468','M469','M47',
                                                  'M480','M481','M482','M483',
                                                  'M488','M498',
                                                  'M50','M51','M53','M54','Q764'))),list(DIAG=icd10cm)]))
Back_Pain[,Covariate:='Back pain']

## Headache
Headache <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('30781','7840')|
                                Reduce('|', Map('startsWith', list(icd9cm),
                                                c('339','3460','3461','3462',
                                                  '3463','3464','3465','3467',
                                                  '3468','3469'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('M436','M439','M461',
                                           'M489','M961','Q760',
                                           'Q761','Q762','Q763','Q766')|
                                Reduce('|', Map('startsWith', list(icd10cm),
                                                c('M432','M438','M460','M464',
                                                  'M468','M469','M47',
                                                  'M480','M481','M482','M483',
                                                  'M488','M498',
                                                  'M50','M51','M53','M54','Q764'))),list(DIAG=icd10cm)]))
Headache[,Covariate:='Headache']

## Musculoskeletal pain
Musc_pain <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('7260','7262','7264','7265',
                                          '72671','72672','72690','72700',
                                          '72703','72704','72705','72706',
                                          '72709','7272','7273','72749',
                                          '72750','72751','72789','7279',
                                          '7290','7291','7294','7295',
                                          '72989','7299','72991','72992',
                                          '78199','9056','9057','V437','V483')|
                                Reduce('|', Map('startsWith', list(icd9cm),
                                                c('725','7261','7263','7266',
                                                  '7276','7297', 
                                                  '830','831','832','833',
                                                  '834','835','836','837',
                                                  '838','839','840','841',
                                                  '842','843','844','845',
                                                  '846','847','848', 
                                                  'V436','V496','V497'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('M353','R29898','R2991')|
                                Reduce('|', Map('startsWith', list(icd10cm),
                                                c('M60','M61','M62','M63',
                                                  'M65','M66','M67','M70',
                                                  'M71','M72','M75','M76',
                                                  'M77','M79',
                                                  'S03','S13','S16','S23',
                                                  'S33','S390','S399','S43',
                                                  'S46','S53','S56','S63',
                                                  'S66','S73','S76','S83',
                                                  'S86','S93','S96','Z966',
                                                  'Z971','Z89'))),list(DIAG=icd10cm)]))
Musc_pain[,Covariate:='Musculoskeletal pain']

## Neuropathic pain
Neuropathy <-
    rbind(unique(ICD9_ICD10[icd9cm %in% c('05313','07272','37733', 
                                          '37734','37741')|
                                Reduce('|', Map('startsWith', list(icd9cm),
                                                c('3370','3371',
                                                  '353','354','355','356','357'))),list(DIAG=icd9cm)]),
          unique(ICD9_ICD10[icd10cm %in% c('B0223','B2684','G990')|
                                Reduce('|', Map('startsWith', list(icd10cm),
                                                c('G900',
                                                  'G54','G55','G56','G57',
                                                  'G58','G59','G60','G61',
                                                  'G62','G63','G64','G65'))),list(DIAG=icd10cm)]))
Neuropathy[,Covariate:='Neuropathic Pain']


## combine all covariates
cov <-
    list(Alcohol,Anxiety,Arthritis,Back_Pain,Headache,Illicit_Drug,Musc_pain,Neuropathy,Nicotine,PTSD)
    
cov_diag <- 
    rbindlist(cov)

## create covariate codes csv file
fwrite(cov_diag,file = 'background_notes/cov_icd9_icd10_codes.csv')

#-----Identify Patient Covariate Data-----
# 1 - identify types of patient diagnoses
# 2 - create covariate indicator variables
#     (2 outpatient, 1 inpatient requirement within 12 months)
#     (within washout to baseline time period)

## identify patient diagnoses
setkey(cohort_diag,DIAG)
setkey(cov_diag,DIAG)
cohort_diag[cov_diag,covariate:=Covariate]
# cohort_diag[covariate!='OUD']

## create covariate indicators
setkey(cohort,PTID)
setkey(cohort_diag,PTID)
cohort_diag[cohort,FIRST_OUD:=FIRST_OUD]
# cohort_diag[!is.na(covariate)&covariate!='OUD',list(unique(covariate))]
        # 1:                     Arthritis
        # 2:                  Illicit drug
        # 3:          Musculoskeletal pain
        # 4:                      Nicotine
        # 5:                 Other anxiety
        # 6:                       Alcohol
        # 7:                     Back pain
        # 8:                      Headache
        # 9:              Neuropathic Pain
        # 10: Posttraumatic stress disorder

# Alcohol
alcohol_diag <- 
    cohort_diag[covariate=='Alcohol',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(alcohol_diag,PTID,DATE)
alcohol_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
alcohol_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
alcohol_pats <-
    alcohol_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                     ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
                 list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(alcohol_pats,PTID)
cohort[alcohol_pats,ALCOHOL:=1]

# Anxiety
anxiety_diag <- 
    cohort_diag[covariate=='Other anxiety',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(anxiety_diag,PTID,DATE)
anxiety_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
anxiety_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
anxiety_pats <-
    anxiety_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                     ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
                 list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(anxiety_pats,PTID)
cohort[anxiety_pats,ANXIETY:=1]

# Arthritis
arthritis_diag <- 
    cohort_diag[covariate=='Arthritis',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(arthritis_diag,PTID,DATE)
arthritis_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
arthritis_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
arthritis_pats <-
    arthritis_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                       ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
                   list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(arthritis_pats,PTID)
cohort[arthritis_pats,ARTHRITIS:=1]

# Back pain
bp_diag <- 
    cohort_diag[covariate=='Back pain',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(bp_diag,PTID,DATE)
bp_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
bp_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
bp_pats <-
    bp_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
            list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(bp_pats,PTID)
cohort[bp_pats,BACK_PAIN:=1]

# Headache
headache_diag <- 
    cohort_diag[covariate=='Headache',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(headache_diag,PTID,DATE)
headache_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
headache_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
headache_pats <-
    headache_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                      ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
                  list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(headache_pats,PTID)
cohort[headache_pats,HEADACHE:=1]

# Illicit drug use
illdrug_diag <- 
    cohort_diag[covariate=='Illicit drug',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(illdrug_diag,PTID,DATE)
illdrug_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
illdrug_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
illdrug_pats <-
    illdrug_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                     ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
                 list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(illdrug_pats,PTID)
cohort[illdrug_pats,ILLICIT_DRUG:=1]

# Musculoskeletal pain
mp_diag <- 
    cohort_diag[covariate=='Musculoskeletal pain',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(mp_diag,PTID,DATE)
mp_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
mp_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
mp_pats <-
    mp_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
            list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(mp_pats,PTID)
cohort[mp_pats,MUSC_PAIN:=1]

# Neuropathic pain
neuro_diag <- 
    cohort_diag[covariate=='Neuropathic Pain',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(neuro_diag,PTID,DATE)
neuro_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
neuro_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
neuro_pats <-
    neuro_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                   ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
               list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(neuro_pats,PTID)
cohort[neuro_pats,NEUROPATHY:=1]

# Nicotine use
nic_diag <- 
    cohort_diag[covariate=='Nicotine',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(nic_diag,PTID,DATE)
nic_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
nic_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
nic_pats <-
    nic_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                 ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
             list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(neuro_pats,PTID)
cohort[nic_pats,NICOTINE:=1]

# Postraumatic stress disorder
ptsd_diag <- 
    cohort_diag[covariate=='Posttraumatic stress disorder',list(PTID,DATE,FIRST_OUD,care_type)]
setkey(ptsd_diag,PTID,DATE)
ptsd_diag[,diff:=shift(DATE, fill=first(DATE), type = 'lead')-DATE,by=PTID]
ptsd_diag[,diff:=ifelse(DATE==max(DATE),0,diff),by=PTID]
ptsd_pats <-
    ptsd_diag[((care_type=='inpatient')|(diff>0)&(diff<=365))&
                  ((DATE>(FIRST_OUD-730))&(DATE<FIRST_OUD)),
              list(DATE=min(DATE)),by=PTID]
setkey(cohort,PTID)
setkey(neuro_pats,PTID)
cohort[ptsd_pats,PTSD:=1]

#-----Outputs----- 

write.fst(cohort,
          path = paste0('data/created_data/cov_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)
