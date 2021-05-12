############################################################################################
## Title: 00_cut_buprenorphine_depression_data.R
## Date Created: 2019-06-25
## Created by: Evan Carey
############################################################################################
## Purpose: This file will open data and perform initial data exploration. 
############################################################################################

library(data.table)
library(fst)
library(Hmisc)

## Import OUD Definitions
oud_diag <- 
  fread('Z:/OUD_Bup_Depression/background_notes/ICD9_ICD10_Opioid Related Diagnoses_v2.csv')
oud_diag[,value_noPeriod:=gsub('\\.','',Value)]

########################################################################
####### Identify patients with at least one diagnosis for OUD ##########
########################################################################
#### Diagnosis files
## 2010
diagnoses_2010 <- 
  read_fst('D:/Optum_Data/data/r_data_fst/stl_random_201905_diag_2010.fst',
           as.data.table = T)
setkey(diagnoses_2010,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2010[grepl('^3055',DIAGNOSIS_CD),.N, by=DIAGNOSIS_CD]
diagnoses_2010[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2010[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 471
diagnoses_2010[,uniqueN(PTID)] # 268149
pts_2010 <- 
  diagnoses_2010[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2010)
## explore data?
diagnoses_2010[grepl('\\.',DIAGNOSIS_CD)]
diagnoses_2010[J('ICD9')][grepl('^304',DIAGNOSIS_CD)]
diagnoses_2010[oud_diag[,list(Type,Value)],nomatch=0][,.N,by=DIAGNOSIS_CD]
## 2011
diagnoses_2011 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2011.fst',
           as.data.table = T)
setkey(diagnoses_2011,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2011[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2011[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 1027
diagnoses_2011[,uniqueN(PTID)] # 493913
pts_2011 <- 
  diagnoses_2011[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2011)
## 2012
diagnoses_2012 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2012.fst',
           as.data.table = T)
setkey(diagnoses_2012,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2012[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2012[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 1827
diagnoses_2012[,uniqueN(PTID)] # 751224
pts_2012 <- 
  diagnoses_2012[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2012)
## 2013
diagnoses_2013 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2013.fst',
           as.data.table = T)
setkey(diagnoses_2013,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2013[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2013[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 2727
diagnoses_2013[,uniqueN(PTID)] # 963040
pts_2013 <- 
  diagnoses_2013[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2013)
## 2014
diagnoses_2014 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2014.fst',
           as.data.table = T)
setkey(diagnoses_2014,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2014[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2014[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 4075
diagnoses_2014[,uniqueN(PTID)] # 1279508
pts_2014 <- 
  diagnoses_2014[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2014)
## 2015
diagnoses_2015 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2015.fst',
           as.data.table = T)
setkey(diagnoses_2015,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2015[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2015[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 5338
diagnoses_2015[,uniqueN(PTID)] # 1372602
pts_2015 <- 
  diagnoses_2015[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2015)
## 2016
diagnoses_2016 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2016.fst',
           as.data.table = T)
setkey(diagnoses_2016,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2016[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2016[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 6749
diagnoses_2016[,uniqueN(PTID)] # 1463761
pts_2016 <- 
  diagnoses_2016[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2016)
## 2017
diagnoses_2017 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2017.fst',
           as.data.table = T)
setkey(diagnoses_2017,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2017[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2017[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 7338
diagnoses_2017[,uniqueN(PTID)] # 1455636
pts_2017 <- 
  diagnoses_2017[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2017)
## 2018
diagnoses_2018 <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_diag_2018.fst',
           as.data.table = T)
setkey(diagnoses_2018,DIAGNOSIS_CD_TYPE,DIAGNOSIS_CD)
diagnoses_2018[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,.N,by=DIAGNOSIS_CD]
diagnoses_2018[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,uniqueN(PTID)] # 6531
diagnoses_2018[,uniqueN(PTID)] # 1307714
pts_2018 <- 
  diagnoses_2018[oud_diag[,list(Type,value_noPeriod)],nomatch=0][,list(PTID=unique(PTID))] 
rm(diagnoses_2018)
## OUD rate is generally increasing from .17% in 2010 to .5% in 2018
pts_all_diag <-
  unique(rbindlist(list(pts_2010,pts_2011,pts_2012,pts_2013,pts_2014,pts_2015,
                        pts_2016,pts_2017,pts_2018)))
## OUD rate in full sample is 0.5% based on diagnosis tables. 

## checking claims tables
claims_diag <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_med_diag.fst',
           as.data.table = T)
setkey(claims_diag,DIAG)
claims_diag[grepl('\\.',DIAG)] ## very few zeros
claims_diag[oud_diag[,list(value_noPeriod),nomatch=0]]
claims_diag[oud_diag[,list(Value)],nomatch=0] ## none with period
claims_diag[oud_diag[,list(value_noPeriod)]][is.na(PTID)] ## a few don't match
pts_all_claims <- 
  unique(claims_diag[oud_diag[,list(value_noPeriod)],nomatch=0][,list(PTID)]) # 7218
unique(rbind(pts_all_claims,pts_all_diag)) # 32244 total unique pts - very few overlap in the datasets? 
rm(claims_diag)

## inpt confinement
claims_inpt <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_inp_conf.fst',
           as.data.table = T)
claims_long <-
  melt(claims_inpt[,list(PTID,ADMIT_DATE,ICD_FLAG,DIAG1,DIAG2,DIAG3,DIAG4,DIAG5)],
       id.vars = c('PTID','ADMIT_DATE','ICD_FLAG'),
       variable.name = 'Position',value.name ='DIAG')
setkey(claims_long,DIAG)
claims_long[grepl('\\.',DIAG)] ## no periods
claims_long[oud_diag[,list(value_noPeriod)],nomatch=0]
claims_long[oud_diag[,list(Value)],nomatch=0] ## none with period
claims_long[oud_diag[,list(value_noPeriod)]][is.na(PTID)] ## a few don't match
pts_all_claims_inpt <- 
  unique(claims_long[oud_diag[,list(value_noPeriod)],nomatch=0][,list(PTID)]) # 1548

## save patient list
pts_all <-
  unique(rbind(pts_all_claims,pts_all_diag,pts_all_claims_inpt)) # 32269
write.fst(pts_all,
        path = paste0('z:/OUD_Bup_Depression/data/source_data/OUD_patient_list_',
                      gsub('-','',Sys.Date()),'.fst'),
        compress = 100)

#################################################################################
##### Subset required files by patientID and save in project folder
#################################################################################

file_path <- 
  'z:/OUD_Bup_Depression/data/source_data/'
  
## Patient file
patient <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_pt.fst',
           as.data.table = T)
setkey(patient,PTID)
write.fst(patient[pts_all,nomatch=0],
          paste0(file_path,
                 'patient_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(patient)
#### Diagnosis file
diag_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('starting loop',i,'\n'))
  diag_temp <- 
    read_fst(paste0('D:/Optum_Data/data/r_data_fst/stl_random_201905_diag_',
                  as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(diag_temp,PTID)
  diag_list[[i]] <- diag_temp[pts_all,nomatch=0]
  rm(diag_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
diag_all <- 
  rbindlist(diag_list)
write.fst(diag_all,
          paste0(file_path,
                 'diag_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(diag_all,diag_list)

## Encounter 
enc_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('starting loop',i,'\n'))
  enc_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_enc_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(enc_temp,PTID)
  enc_list[[i]] <- enc_temp[pts_all,nomatch=0]
  rm(enc_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
enc_all <- 
  rbindlist(enc_list)
write.fst(enc_all,
          paste0(file_path,
                 'enc_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(enc_all,enc_list)

## Visit file
visit_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('starting loop',i,'\n'))
  visit_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_vis_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(visit_temp,PTID)
  visit_list[[i]] <- visit_temp[pts_all,nomatch=0]
  cat(paste(nrow(visit_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(visit_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
visit_all <- 
  rbindlist(visit_list)
write.fst(visit_all,
          paste0(file_path,
                 'visit_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(visit_all,visit_list)
## prescriptions
rx_presc_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('starting loop',i,'\n'))
  rx_presc_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_rx_presc_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(rx_presc_temp,PTID)
  rx_presc_list[[i]] <- rx_presc_temp[pts_all,nomatch=0]
  cat(paste(nrow(rx_presc_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(rx_presc_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
rx_presc_all <- 
  rbindlist(rx_presc_list)
write.fst(rx_presc_all,
          paste0(file_path,
                 'rx_presc_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(rx_presc_all,rx_presc_list)

## patient reported meds
rx_patrep_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('starting loop',i,'\n'))
  rx_patrep_temp <- 
    read_fst(paste0('D:/Optum_Data/data/r_data_fst/stl_random_201905_rx_patrep_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(rx_patrep_temp,PTID)
  rx_patrep_list[[i]] <- rx_patrep_temp[pts_all,nomatch=0]
  cat(paste(nrow(rx_patrep_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(rx_patrep_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
rx_patrep_all <- 
  rbindlist(rx_patrep_list)
write.fst(rx_patrep_all,
          paste0(file_path,
                 'rx_patrep_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(rx_patrep_all,rx_patrep_list)

## med administrations
rx_adm_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  rx_adm_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_rx_adm_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(rx_adm_temp,PTID)
  rx_adm_list[[i]] <- rx_adm_temp[pts_all,nomatch=0]
  cat(paste(nrow(rx_adm_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(rx_adm_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
rx_adm_all <- 
  rbindlist(rx_adm_list)
write.fst(rx_adm_all,
          paste0(file_path,
                 'rx_adm_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(rx_adm_all,rx_adm_list)

## labs
labs_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  labs_temp <- 
    read_fst(paste0('D:/Optum_Data/data/r_data_fst/stl_random_201905_lab_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(labs_temp,PTID)
  labs_list[[i]] <- labs_temp[pts_all,nomatch=0]
  cat(paste(nrow(labs_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(labs_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
labs_all <- 
  rbindlist(labs_list)
write.fst(labs_all,
          paste0(file_path,
                 'labs_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(labs_all,labs_list)

## procedure
proc_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  proc_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_proc_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(proc_temp,PTID)
  proc_list[[i]] <- proc_temp[pts_all,nomatch=0]
  cat(paste(nrow(proc_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(proc_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
proc_all <- 
  rbindlist(proc_list)
write.fst(proc_all,
          paste0(file_path,
                 'proc_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(proc_all,proc_list)

## claims - member (patient)
claims_member <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_mbr_dtl.fst',
           as.data.table = T)
setkey(claims_member,PTID)
write.fst(claims_member[pts_all,nomatch=0],
          paste0(file_path,
                 'claim_member_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(claims_member)

## claims -  rx
claims_rx <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_rx.fst',
           as.data.table = T)
setkey(claims_rx,PTID)
write.fst(claims_rx[pts_all,nomatch=0],
          paste0(file_path,
                 'claim_rx_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(claims_rx)

## claims -  diagnosis
claims_med_diag <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_med_diag.fst',
           as.data.table = T)
setkey(claims_med_diag,PTID)
write.fst(claims_med_diag[pts_all,nomatch=0],
          paste0(file_path,
                 'claim_med_diag',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(claims_med_diag)

## Add claims medical services
claims_med_services <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_med_serv.fst',
           as.data.table = T)
setkey(claims_med_services,PTID)
write.fst(claims_med_services[pts_all,nomatch=0],
          paste0(file_path,
                 'claim_med_serv',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(claims_med_services)

## Add claims inpatient confinement
claims_inpt <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_clm_inp_conf.fst',
           as.data.table = T)
setkey(claims_inpt,PTID)
write.fst(claims_inpt[pts_all,nomatch=0],
          paste0(file_path,
                 'claim_inp_conf',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(claims_inpt)

## insurance
ins_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  ins_temp <- 
    read_fst(paste0('D:/Optum_Data/data/r_data_fst/stl_random_201905_ins_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(ins_temp,PTID)
  ins_list[[i]] <- ins_temp[pts_all,nomatch=0]
  cat(paste(nrow(ins_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(ins_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
ins_all <- 
  rbindlist(ins_list)
write.fst(ins_all,
          paste0(file_path,
                 'ins_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(ins_all,ins_list)

## observations - pain score
obs_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  obs_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_obs_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(obs_temp,PTID)
  obs_list[[i]] <- obs_temp[pts_all,nomatch=0]
  cat(paste(nrow(obs_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(obs_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
obs_all <- 
  rbindlist(obs_list)
write.fst(obs_all,
          paste0(file_path,
                 'obs_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(obs_all,obs_list)

## nlp SDS
nlp_sds_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  nlp_sds_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_nlp_sds_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(nlp_sds_temp,PTID)
  nlp_sds_list[[i]] <- nlp_sds_temp[pts_all,nomatch=0]
  cat(paste(nrow(nlp_sds_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(nlp_sds_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
nlp_sds_all <- 
  rbindlist(nlp_sds_list)
write.fst(nlp_sds_all,
          paste0(file_path,
                 'nlp_sds_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(nlp_sds_all,nlp_sds_list)

## nlp measurement
nlp_measure_list <- list()
for (i in seq_along(as.character(2010:2018))){
  cat(paste('-----------------\n\nstarting loop',i,'\n'))
  nlp_measure_temp <- 
    read_fst(paste0('D:/data/r_data_fst/stl_random_201905_nlp_measure_',
                    as.character(2010:2018)[i],'.fst'),
             as.data.table = T)
  setkey(nlp_measure_temp,PTID)
  nlp_measure_list[[i]] <- nlp_measure_temp[pts_all,nomatch=0]
  cat(paste(nrow(nlp_measure_temp[pts_all,nomatch=0]),'rows found\n'))
  rm(nlp_measure_temp)
  gc()
  cat(paste('finished loop',i,'\n'))
}
nlp_measure_all <- 
  rbindlist(nlp_measure_list)
write.fst(nlp_measure_all,
          paste0(file_path,
                 'nlp_measure_',
                 gsub('-','',Sys.Date()),
                 '.fst'),
          compress = 100)
rm(nlp_measure_all,nlp_measure_list)


################################################################################################
##  Check for PHQ9
################################################################################################

## observations
obs_temp <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_obs_2014.fst',
           as.data.table = T)
t1 <- 
  obs_temp[,.N,by=OBS_TYPE]
# No PHQ9 here
rm(obs_temp,t1)

## NLP Measurement
nlp_temp <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_nlp_measure_2014.fst',
           as.data.table = T)
t1 <- nlp_temp[,.N,by=MEASUREMENT_TYPE]
t1[grepl('phq',MEASUREMENT_TYPE,ignore.case = T)]
# yes PHQ-9 is in NLP Measurement
rm(t1,nlp_temp)

## NLP SDS
nlp_sds <- 
  read_fst('D:/data/r_data_fst/stl_random_201905_nlp_sds_2014.fst',
           as.data.table = T)
t1 <- nlp_sds[,.N,by=SDS_TERM]
t1[grepl('phq',SDS_TERM,ignore.case = T)] # no PHQ
t1[grepl('depre',SDS_TERM,ignore.case = T)] # some depression
rm(nlp_sds,t1)
