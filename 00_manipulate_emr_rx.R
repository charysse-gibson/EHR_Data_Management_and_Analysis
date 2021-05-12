######################################################################
## Title: 00_manipulate_Emr_rx.R                                        
## Author: Charysse Gibson
## Date created: August 1, 2019
######################################################################
# Purpose: Manipulate Rx data from EMR datasets
######################################################################
# Inputs: rx_adm_20190712.fst
#         rx_patrep_20190712.fst
#         rx_presc_20190712.fst
# outputs: 
######################################################################

setwd('A:/')

library(data.table)
library(fst)
library(Hmisc)
library(bit64)

#-----Dataset & Variables Needed-----

# rx_adm (med administrations)
    # PTID
    # NDC
    # ORDER_DATE (ADMIN_DATE mostly missing, but matches ORDER_DATE)
    # QUANTITY_OF_DOSE
    # STRENGTH
    # STRENGTH_UNIT

# rx_patrep (patient reported meds)
    # PTID
    # NDC
    # REPORTED_DATE
    # QUANTITY_OF_DOSE
    # STRENGTH
    # STRENGTH_UNIT

# rx_presc (EMR prescriptions)
    # PTID
    # NDC
    # RXDATE
    # QUANTITY_OF_DOSE
    # STRENGTH
    # STRENGTH_UNIT

#-----Inputs-----

## med administrations
rx_adm <- 
  read_fst('data/source_data/rx_adm_20190712.fst',
           as.data.table = T)
# rx_adm[,list(.N,unique_pats=uniqueN(PTID))]
#          N unique_pats
# 1: 5762763       20536
str(rx_adm)
# describe(rx_adm[,list(ADMIN_DATE)])
# describe(rx_adm[,list(ORDER_DATE)])
rx_adm[,order_date:=as.Date(ORDER_DATE,format='%m-%d-%Y')]

# rx_patrep
rx_patrep <-
  read_fst('data/source_data/rx_patrep_20190712.fst',
           as.data.table = T)
# rx_patrep[,list(.N,unique_pats=uniqueN(PTID))]
#          N unique_pats
# 1: 1297153       20969
str(rx_patrep)
rx_patrep[,rep_date:=as.Date(REPORTED_DATE,format='%m-%d-%Y')]

## rx_presc
rx_presc <-
  read_fst('data/source_data/rx_presc_20190712.fst',
           as.data.table = T)
# rx_presc[,list(.N,unique_pats=uniqueN(PTID))]
#          N unique_pats
# 1: 1139827       22908
str(rx_presc)
rx_presc[,presc_date:=as.Date(RXDATE,format='%m-%d-%Y')]

##-----Data Manipulation-----

## combine all EMR rx events
# identify unique prescriptions by PTID, DATE, NDC
rx_adm[,DATE:=order_date]
rx_patrep[,DATE:=rep_date]
rx_presc[,DATE:=presc_date]
setkey(rx_adm,PTID,DATE,NDC)
setkey(rx_patrep,PTID,DATE,NDC)
setkey(rx_presc,PTID,DATE,NDC)
rxemr = list(rx_adm[,list(PTID,DATE,NDC,STRENGTH,STRENGTH_UNIT)],
             rx_patrep[,list(PTID,DATE,NDC,STRENGTH,STRENGTH_UNIT)],
             rx_patrep[,list(PTID,DATE,NDC,STRENGTH,STRENGTH_UNIT)])
# NOTE: claim_rx data does not give STRENGTH_UNIT, units are already attached

all_emr_rx <-
  unique(rbindlist(rxemr,use.names = TRUE,fill = TRUE,idcol = 'file'),by=c('PTID','DATE','NDC'))

## explore combined dataset
str(all_emr_rx)
describe(all_emr_rx)
    # NDC 
    #       n    missing   distinct       
    # 2827797       7327      18322
# majority of STRENGTH and STRENGTH_UNIT are missing
# no missing PTID or DATE

## combine STRENGTH and STRENGTH UNIT (match claim_rx)
all_emr_rx[STRENGTH!=''&STRENGTH_UNIT!='',
           emr_strength := paste0(toupper(STRENGTH),sep=' ',toupper(STRENGTH_UNIT))]
all_emr_rx[emr_strength!='']

## subset necessary variables
emr_rx <- 
  all_emr_rx[!is.na(NDC),list(PTID,DATE,NDC,emr_strength)]
# emr_rx[,list(.N,unique_pats=uniqueN(PTID))]
    #          N unique_pats
    # 1: 2827797       25611
str(emr_rx)
# convert NDC class from integer64 to character
emr_rx[,NDC:=as.character(NDC)]

#-----Outputs-----

write.fst(emr_rx,
          path = paste0('data/created_data/emr_rx_',
                        gsub('-','',Sys.Date()),'.fst'),
          compress = 100)

