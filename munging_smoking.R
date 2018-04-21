##%######################################################%##
#                                                          #
####                  Data munging for                  ####
####                      smoking                       ####
####                       status                       ####
####           ------------------------------           ####
####          -------------------------------           ####
#                                                          #
##%######################################################%##

# Setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(haven)
`%nin%` <- Negate(`%in%`)
if('pkg:stringr' %nin% search()){
  library(stringr)
}
ProjTemplate::reload()
# Original cohort -----------------------------------------------------------------------------

datadir <- file.path(set_datadir(), 'archives','framcohort', 'Datasets')

smoke_orig <- list(
## lex0_7
### MF 330 gives cig/day in exam 5 (1 = yes,  rest = no)
### MF 537 gives cig/day in exam 7 (9999/1999/9997 = NA, 2000 = No, rest = yes)
'exam5' = read_sas(file.path(datadir,'lex0_7.sas7bdat')) %>% select(PID, MF330) %>%
  mutate(smoking = case_when(is.na(MF330) ~ NA_real_,
                             MF330 ==1 ~ 1,
                             TRUE ~ 0)) %>%
  select(PID, smoking),
'exam7' = read_sas(file.path(datadir, 'lex0_7.sas7bdat')) %>% select(PID, MF537) %>%
  mutate(smoking = case_when(is.na(MF537) ~ NA_real_,
                             MF537 == 1999 ~ NA_real_,
                             MF537 == 2000 ~ 0,
                             MF537 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_8
### FA 137 gives smoking status (2000 = no, 1999 = NA, rest = yes)
'exam8' = read_sas(file.path(datadir, 'lex0_8.sas7bdat')) %>% select(PID, FA137) %>%
  mutate(smoking = case_when(FA137 == 1999 ~ NA_real_,
                             FA137 == 2000 ~ 0,
                             is.na(FA137) ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_9
### FB86 gives smoking status (2000 = no, rest = yes)
'exam9' = read_sas(file.path(datadir, 'lex0_9.sas7bdat')) %>% select(PID, FB86) %>%
  mutate(smoking = case_when(FB86 == 2000 ~ 0,
                             is.na(FB86) ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_10
### FC102 gives cigarette smoking status (0 = no, 1 = yes, 9 = NA)
'exam10' = read_sas(file.path(datadir, 'lex0_10.sas7bdat')) %>% select(PID, FC102) %>%
  mutate(smoking = case_when(FC102 == 9 ~ NA_real_,
                             FC102 == 0 ~ 0,
                             is.na(FC102) ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_11
### FD95  gives cigarette smoking status (0 = no, 1 = yes, 9 = NA)
'exam11' = read_sas(file.path(datadir, 'lex0_11.sas7bdat')) %>% select(PID, FD95) %>%
  mutate(smoking = case_when(FD95==9 ~ NA_real_,
                             FD95==0 ~ 0,
                             is.na(FD95) ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_12
### FE108 gives cig status (88 = no, 99 = NA, rest = yes)
'exam12' = read_sas(file.path(datadir, 'lex0_12.sas7bdat')) %>% select(PID, FE108) %>%
  mutate(smoking = case_when(FE108 == 99 ~ NA_real_,
                             is.na(FE108) ~ NA_real_,
                             FE108 == 88 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_13
### FF110 gives cig status (88 = no, . = NA, rest = yes)
'exam13' = read_sas(file.path(datadir, 'lex0_13.sas7bdat')) %>% select(PID, FF110) %>%
  mutate(smoking = case_when(FF110 ==88 ~ 0,
                             is.na(FF110) ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_14
### FG104 gives cig status (88 = no, . = NA, rest = yes)
'exam14' = read_sas(file.path(datadir, 'lex0_14.sas7bdat')) %>% select(PID, FG104) %>%
  mutate(smoking = case_when(is.na(FG104) ~ NA_real_,
                             FG104 == 88 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_15
### FH102 gives cig status (88 = no, . = NA, rest = yes)
'exam15' = read_sas(file.path(datadir, 'lex0_15.sas7bdat')) %>% select(PID, FH102) %>%
  mutate(smoking = case_when(is.na(FH102) ~ NA_real_,
                             FH102 == 88 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_17
### FJ40 gives cig status (88 & 0 = no, . = NA, rest = yes)
'exam17' = read_sas(file.path(datadir, 'lex0_17.sas7bdat')) %>% select(PID, FJ40) %>%
  mutate(smoking = case_when(is.na(FJ40) ~ NA_real_,
                             FJ40 == 88 ~ 0,
                             FJ40 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_18
### FK131 gives cig status (0 = no, . = NA, rest = yes)
'exam18' = read_sas(file.path(datadir, 'lex0_18.sas7bdat')) %>% select(PID, FK131) %>%
  mutate(smoking = case_when(is.na(FK131) ~ NA_real_,
                             FK131 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_19
### FL187 gives cig status (0 = no, . = NA, rest = yes)
'exam19' = read_sas(file.path(datadir, 'lex0_19.sas7bdat')) %>% select(PID, FL187) %>%
  mutate(smoking = case_when(is.na(FL187) ~ NA_real_,
                             FL187 == 0 ~ NA_real_,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_20
### FM233 gives cig status (0 = no, . = NA, rest = yes)
'exam20' = read_sas(file.path(datadir, 'lex0_20.sas7bdat')) %>% select(PID, FM233) %>%
  mutate(smoking = case_when(is.na(FM233) ~ NA_real_,
                             FM233 == 0 ~ 0,
                             TRUE  ~ 1)) %>%
  select(PID, smoking),
## lex0_21
### FN186 gives cig status (0 = no, . = NA, rest = yes)
'exam21' = read_sas(file.path(datadir, 'lex0_21.sas7bdat')) %>% select(PID, FN186) %>%
  mutate(smoking = case_when(is.na(FN186) ~ NA_real_,
                             FN186 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_22
### FO181 gives cig status (0 = no, . = NA, rest = yes)
'exam22' = read_sas(file.path(datadir, 'lex0_22.sas7bdat')) %>% select(PID, FO181) %>%
  mutate(smoking = case_when(is.na(FO181) ~ NA_real_,
                             FO181 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_23
### FP134 gives cig status (0 = no, . = NA, rest = yes)
'exam23' = read_sas(file.path(datadir, 'lex0_23.sas7bdat')) %>% select(PID, FP134) %>%
  mutate(smoking = case_when(is.na(FP134) ~ NA_real_,
                             FP134 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_24
### FQ195 gives cig status (0 = no, . = NA, rest = yes)
'exam24' = read_sas(file.path(datadir, 'lex0_24.sas7bdat')) %>% select(PID, FQ195) %>%
  mutate(smoking = case_when(is.na(FQ195) ~ NA_real_,
                             FQ195 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_25
### FR252 gives cig status (0 = no, 1 = yes, . = NA)
'exam25' = read_sas(file.path(datadir, 'lex0_25.sas7bdat')) %>% select(PID, FR252) %>%
  mutate(smoking = case_when(is.na(FR252) ~ NA_real_,
                             FR252 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## lex0_26
### FS347 gives cig status (0 = no, 1 = yes, . = NA)
'exam26' = read_sas(file.path(datadir, 'lex0_26.sas7bdat')) %>% select(PID, FS347) %>%
  mutate(smoking = case_when(is.na(FS347) ~ NA_real_,
                             FS347 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## e_exam_ex27_0_0075d
### FT349 gives cig status (0 = no, 1 = yes, . = NA)
'exam27' = read_sas(file.path(datadir, 'e_exam_ex27_0_0075d.sas7bdat')) %>% select(PID, FT349) %>%
  mutate(smoking = case_when(is.na(FT349) ~ NA_real_,
                             FT349 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## e_exam_ex28_0_0256d
### FU051 gives cig status (0 = no, . = NA, rest = yes)
'exam28' = read_sas(file.path(datadir, 'e_exam_ex28_0_0256d.sas7bdat')) %>% select(PID, FU051) %>%
  mutate(smoking = case_when(is.na(FU051) ~ NA_real_,
                             FU051==0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## e_exam_ex29_0_0210d; the info here is in xls file
### FV027 gives cig status (0 = no, . = NA, rest = yes)
'exam29' = read_sas(file.path(datadir, 'e_exam_ex29_0_0210d.sas7bdat')) %>%
  set_names(toupper(names(.))) %>%
  select(PID, FV027) %>%
  mutate(smoking = case_when(is.na(FV027) ~ NA_real_,
                             FV027 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking),
## e_exam_ex30_0_0274d; the info here is in xls file
### FW026 gives cig status (0 = no, rest = yes)
'exam30' = read_sas(file.path(datadir, 'e_exam_ex30_0_0274d.sas7bdat')) %>%
  set_names(toupper(names(.))) %>% select(PID, FW026) %>%
  mutate(smoking = case_when(is.na(FW026) ~ NA_real_,
                             FW026 == 0 ~ 0,
                             TRUE ~ 1)) %>%
  select(PID, smoking))
smoke_orig <- smoke_orig %>%
  bind_rows(.id = 'exam') %>%
  mutate(PID = as.character(PID),
         exam = as.numeric(str_remove(exam,'exam')),
         smoking = as.numeric(smoking)) %>%
  arrange(PID, exam) %>%
  group_by(PID) %>%
  tidyr::fill(smoking) %>% # LVCF
  ungroup()

# Offspring cohort -----------------------------------------------------------------------------

## lex1_1
### A99 gives cig status (1 = yes, . = NA, rest = no)
## lex1_2v2
### B86 gives smoking status (0 = no, 1 = yes, . = NA)
## lex1_3
### C68 gives cig status (0 = no, . = NA, rest = yes)
## lex1_4
### D093 gives cig status (0 = no, 1 = yes, . = NA)
## lex1_5
### E319 gives cig status (0 = no, 1 = yes, . = NA)
## lex1_6
### F288 gives cig status (0 = no, 1 = yes, . = NA)
## lex1_7
### G116 gives cig status (0 = no, 1 = yes, . = NA)

