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

datadir <- file.path(set_datadir(), 'archives','framcohort')

## lex0_7
### MF 331 gives cig/day in exam 5 (9999/1999/9997 = NA, 2000 = No, rest = yes)
### MF 537 gives cig/day in exam 7 (9999/1999/9997 = NA, 2000 = No, rest = yes)
## lex0_8
### FA 137 gives smoking status (2000 = no, 1999 = NA, rest = yes)
## lex0_9
### FB86 gives smoking status (2000 = no, rest = yes)
## lex0_10
### FC102 gives cigarette smoking status (0 = no, 1 = yes, 9 = NA)
## lex0_11
### FD95  gives cigarette smoking status (0 = no, 1 = yes, 9 = NA)
## lex0_12
### FE108 gives cig status (88 = no, 99 = NA, rest = yes)
## lex0_13
### FF110 gives cig status (88 = no, . = NA, rest = yes)
## lex0_14
### FG104 gives cig status (88 = no, . = NA, rest = yes)
## lex0_15
### FH102 gives cig status (88 = no, . = NA, rest = yes)
## lex0_17
### FJ40 gives cig status (88 & 0 = no, . = NA, rest = yes)
## lex0_18
### FK131 gives cig status (0 = no, . = NA, rest = yes)
## lex0_19
### FL187 gives cig status (0 = no, . = NA, rest = yes)
## lex0_20
### FM233 gives cig status (0 = no, . = NA, rest = yes)
## lex0_21
### FN186 gives cig status (0 = no, . = NA, rest = yes)
## lex0_22
### FO181 gives cig status (0 = no, . = NA, rest = yes)
## lex0_23
### FP134 gives cig status (0 = no, . = NA, rest = yes)
## lex0_24
### FQ195 gives cig status (0 = no, . = NA, rest = yes)
## lex0_25
### FR252 gives cig status (0 = no, 1 = yes, . = NA)
## lex0_26
### FS347 gives cig status (0 = no, 1 = yes, . = NA)
## e_exam_ex27_0_0075d
### FT349 gives cig status (0 = no, 1 = yes, . = NA)
## e_exam_ex28_0_0256d
### FU051 gives cig status (0 = no, . = NA, rest = yes)
## e_exam_ex29_0_0256d; the info here is in xls file
### FV027 gives cig status (0 = no, . = NA, rest = yes)
## e_exam_ex30_0_0256d; the info here is in xls file
### FW026 gives cig status (0 = no, rest = yes)


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

