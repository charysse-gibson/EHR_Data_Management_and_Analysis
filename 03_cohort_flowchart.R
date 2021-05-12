######################################################################
## Title: 03_cohort_flowchart.R                                         
## Author: Charysse Gibson
## Date created: August 20, 2019
######################################################################
# Purpose: Create eligibility criteria flowchart
######################################################################
# Inputs: cohort_dataset_20190821.fst
# outputs: cohort_diagram.png
######################################################################

setwd('A:/')

library(data.table)
library(fst)

#-----Inputs-----

cohort_dataset <-
  read_fst('data/created_data/cohort_dataset_20190821.fst',
           as.data.table = T)

#-----Create subsets-----

random_sample <- 5000000
baseline <- cohort_dataset[,uniqueN(PTID)]
OUD_excl <- random_sample-baseline
age_incl <- cohort_dataset[AGE>17&AGE<65,uniqueN(PTID)]
age_excl <- baseline-age_incl
washout_incl <- cohort_dataset[AGE>17&AGE<65&
                                 WASHOUT_2YR==1,uniqueN(PTID)]
washout_excl <- age_incl-washout_incl
prior_bup_incl <- cohort_dataset[AGE>17&AGE<65&
                                   WASHOUT_2YR==1&
                                   is.na(PRIOR_BUP),uniqueN(PTID)]
prior_bup_excl <- washout_incl-prior_bup_incl
follow_up_incl <- cohort_dataset[AGE>17&AGE<65&
                                   WASHOUT_2YR==1&
                                   is.na(PRIOR_BUP)&
                                   FOLLOW_UP==1,uniqueN(PTID)]
follow_up_excl <- prior_bup_incl-follow_up_incl
mtd_incl <- cohort_dataset[AGE>17&AGE<65&
                             WASHOUT_2YR==1&
                             is.na(PRIOR_BUP)&
                             FOLLOW_UP==1&
                             is.na(MTD_TREATMENT),uniqueN(PTID)]
mtd_excl <- follow_up_incl-mtd_incl
post_dep_incl <- cohort_dataset[AGE>17&AGE<65&
                                  WASHOUT_2YR==1&
                                  is.na(PRIOR_BUP)&
                                  FOLLOW_UP==1&
                                  is.na(MTD_TREATMENT)&
                                  is.na(POST_DEP),uniqueN(PTID)]
post_dep_excl <- mtd_incl-post_dep_incl
demog_incl <- cohort_dataset[AGE>17&AGE<65&
                               WASHOUT_2YR==1&
                               is.na(PRIOR_BUP)&
                               FOLLOW_UP==1&
                               is.na(MTD_TREATMENT)&
                               is.na(POST_DEP)&
                               !is.na(RACE)&
                               !is.na(GENDER),uniqueN(PTID)]
demog_excl <- post_dep_incl-demog_incl

#-----Create Flowchart-----

library(DiagrammeR)

# Define data
options(scipen = 999)
data <- c(random_sample, 
          b=OUD_excl, 
          c=baseline, 
          d=age_excl, 
          e=age_incl,
          f=washout_excl,
          g=washout_incl,
          h=prior_bup_excl,
          i=prior_bup_incl,
          j=follow_up_excl,
          k=follow_up_incl,
          l=mtd_excl,
          m=mtd_incl,
          n=post_dep_excl,
          o=post_dep_incl,
          p=demog_excl,
          q=demog_incl)
data <- trimws(format(data,big.mark = ","))

DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = rectangle, fontname=Calibri, fontsize=24, fillcolor = Biege, width=3]
a [label = '@@1']
b [label = '@@2']
c [label = '@@3']
d [label = '@@4']
e [label = '@@5']
f [label = '@@6']
g [label = '@@7']
h [label = '@@8']
i [label = '@@9']
j [label = '@@10']
k [label = '@@11']
l [label = '@@12']
m [label = '@@13']
n [label = '@@14']
o [label = '@@15']
p [label = '@@16']
q [label = '@@17']
blank1[label = '', width = 0.01, height = 0.01]
blank2[label = '', width = 0.01, height = 0.01]
blank3[label = '', width = 0.01, height = 0.01]
blank4[label = '', width = 0.01, height = 0.01]
blank5[label = '', width = 0.01, height = 0.01]
blank6[label = '', width = 0.01, height = 0.01]
blank7[label = '', width = 0.01, height = 0.01]
blank8[label = '', width = 0.01, height = 0.01]

a -> blank1 [arrowhead = none];
blank1 -> c;
c -> blank2 [arrowhead = none];
blank2 -> e;
e -> blank3 [arrowhead = none];
blank3 -> g;
g -> blank4 [arrowhead = none];
blank4 -> i;
i -> blank5 [arrowhead = none];
blank5 -> k;
k -> blank6 [arrowhead = none];
blank6 -> m;
m -> blank7 [arrowhead = none];
blank7 -> o;
o -> blank8 [arrowhead = none];
blank8 -> q;
{rank=same ; blank1 -> b};
{rank=same ; blank2 -> d};
{rank=same ; blank3 -> f};
{rank=same ; blank4 -> h};
{rank=same ; blank5 -> j};
{rank=same ; blank6 -> l};
{rank=same ; blank7 -> n};
{rank=same ; blank8 -> p};
}

[1]: paste0('', data[1], ' Random sample, aged \\n 18-29 years 2010-2018')
[2]: paste0('Exclude ', data[2], ' never diagnosed \\n with Opioid Use Disorder 2010-2018')
[3]: paste0('', data[3], ' With Opioid Use Disorder 2010-2018 ')
[4]: paste0('Exclude ',data[4],' aged 65-89 years')
[5]: paste0('', data[5], ' Patients aged 18-64 years 2010-2018')
[6]: paste0('', data[6], ' No yearly visits 2 years \\n before first OUD diagnosis')
[7]: paste0('', data[7], ' Had yearly visits 2 years \\n before first OUD diagnosis')
[8]: paste0('Exclude ',data[8],' with buprenorphine \\n treatment prior to first OUD')
[9]: paste0('', data[9], ' buprenorphine treatment \\n before first OUD free')
[10]: paste0('',data[10],' Noninformative 2-year \\n follow-up after first OUD')
[11]: paste0('', data[11], ' Informative 2-year \\n follow-up after first OUD')
[12]: paste0('Exclude ',data[12],' treated with methadone between \\n 2 years before first OUD and follow-up')
[13]: paste0('', data[13], ' Methadone treatment free between \\n 2 years before first OUD and follow-up')
[14]: paste0('Exclude ',data[14],' First depression diagnosis \\n after first OUD diagnosis')
[15]: paste0('', data[15], ' First depression diagnosis did \\n not occur after first OUD diagnosis')
[16]: paste0('Exclude ',data[16],' Missing race or sex data')
[17]: paste0('', data[17], ' Not missing covariate data')
")

