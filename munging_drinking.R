##%######################################################%##
#                                                          #
####          Data munging for drinking status          ####
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

datadir <- file.path(set_datadir(), 'archives','framcohort','Datasets')

drink_origin <- list(
## lex0_7
'exam7' = read_sas(file.path(datadir,'lex0_7.sas7bdat')) %>% select(PID, MF543:MF545) %>%
  mutate(beer = case_when(is.na(MF544)~ NA_real_,
                          MF544 %in% c(9999,1999,9997) ~ NA_real_,
                          MF544 == 2000 ~ 0,
                          TRUE ~ MF544),
         wine = case_when(is.na(MF545) ~ NA_real_,
                          MF545 %in% c(9999, 1999,9997) ~ NA_real_,
                          MF545 == 2000 ~ 0,
                          TRUE ~ MF545),
         liquor = case_when(is.na(MF543) ~ NA_real_,
                            MF543 %in% c(9999, 1999, 9997) ~ NA_real_,
                            MF543 == 2000 ~ 0,
                            TRUE ~ MF543)) %>%
  mutate(drink_mo = rowSums(.[c('beer', 'wine', 'liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_mo > 90, 1, 0)) %>% # per month
  select(PID, rf_etoh),
## lex0_8
'exam8' = read_sas(file.path(datadir, 'lex0_8.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_9
'exam9' = read_sas(file.path(datadir, 'lex0_9.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_10
'exam10' = read_sas(file.path(datadir, 'lex0_10.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_11
'exam11' = read_sas(file.path(datadir, 'lex0_11.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_12
'exam12' = read_sas(file.path(datadir, 'lex0_12.sas7bdat')) %>% select(PID, FE127:FE129) %>%
  mutate(beer = case_when(is.na(FE127)~NA_real_,
                          FE127 == 99 ~ NA_real_,
                          TRUE ~ FE127),
         wine = case_when(is.na(FE128) ~ NA_real_,
                          FE128 == 99 ~ NA_real_,
                          TRUE ~ FE128),
         liquor = case_when(is.na(FE129) ~ NA_real_,
                            FE129 == 99 ~ NA_real_,
                            TRUE ~ FE129)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>%  # per week
  select(PID, rf_etoh),
## lex0_13
'exam13' = read_sas(file.path(datadir, 'lex0_13.sas7bdat')) %>% select(PID, FF125:FF127) %>%
  mutate(beer = case_when(is.na(FF125) ~ NA_real_,
                          TRUE ~ FF125),
         wine = case_when(is.na(FF126) ~ NA_real_,
                          TRUE  ~ FF126),
         liquor = case_when(is.na(FF127) ~ NA_real_,
                            TRUE ~ FF127)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_14
'exam14' = read_sas(file.path(datadir, 'lex0_14.sas7bdat')) %>% select(PID, FG118:FG120) %>%
  mutate(beer = case_when(is.na(FG118) ~ NA_real_,
                          TRUE ~ FG118),
         wine = case_when(is.na(FG119) ~ NA_real_,
                          TRUE ~ FG119),
         liquor = case_when(is.na(FG120) ~ NA_real_,
                            TRUE ~ FG120)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_15
'exam15' = read_sas(file.path(datadir, 'lex0_15.sas7bdat')) %>% select(PID, FH115:FH117) %>%
  mutate(beer = case_when(is.na(FH115) ~ NA_real_,
                          TRUE ~ FH115),
         wine = case_when(is.na(FH116) ~ NA_real_,
                          TRUE ~ FH116),
         liquor = case_when(is.na(FH117) ~ NA_real_,
                            TRUE ~ FH117)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_16
'exam16' = read_sas(file.path(datadir, 'lex0_16.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_17
'exam17' = read_sas(file.path(datadir, 'lex0_17.sas7bdat')) %>% select(PID, FJ59:FJ61) %>%
  mutate(beer = case_when(is.na(FJ59) ~ NA_real_,
                          TRUE ~ FJ59),
         wine = case_when(is.na(FJ60) ~ NA_real_,
                          TRUE ~ FJ60),
         liquor = case_when(is.na(FJ61) ~ NA_real_,
                            TRUE ~ FJ61)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_18
'exam18' = read_sas(file.path(datadir, 'lex0_18.sas7bdat')) %>% select(PID, FK141:FK143) %>%
  mutate(beer = case_when(is.na(FK141) ~ NA_real_,
                          TRUE ~ FK141),
         wine = case_when(is.na(FK142) ~ NA_real_,
                          TRUE ~ FK142),
         liquor = case_when(is.na(FK143) ~ NA_real_,
                            TRUE ~ FK143)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_19
'exam19' = read_sas(file.path(datadir, 'lex0_19.sas7bdat')) %>% select(PID, FL202, FL205, FL208) %>%
  mutate(beer = case_when(is.na(FL202) ~ NA_real_,
                          TRUE ~ FL202),
         wine = case_when(is.na(FL205) ~ NA_real_,
                          TRUE ~ FL205),
         liquor = case_when(is.na(FL208) ~ NA_real_,
                            TRUE ~ FL208)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_20
'exam20' = read_sas(file.path(datadir, 'lex0_20.sas7bdat')) %>% select(PID, FM221, FM224, FM227) %>%
  mutate(beer = case_when(is.na(FM221) ~ NA_real_,
                          TRUE ~ FM221),
         wine = case_when(is.na(FM224) ~ NA_real_,
                          TRUE ~ FM224),
         liquor = case_when(is.na(FM227) ~ NA_real_,
                            TRUE ~ FM227)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_21
'exam21' = read_sas(file.path(datadir, 'lex0_21.sas7bdat')) %>% select(PID, FN175, FN178, FN181) %>%
  mutate(beer =  case_when(is.na(FN175) ~ NA_real_,
                          TRUE ~ FN175),
         wine =  case_when(is.na(FN178) ~ NA_real_,
                          TRUE ~ FN178),
         liquor =  case_when(is.na(FN181) ~ NA_real_,
                            TRUE ~ FN181)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_22
'exam22' = read_sas(file.path(datadir, 'lex0_22.sas7bdat')) %>% select(PID, FO171, FO174, FO177) %>%
  mutate(beer =   case_when(is.na(FO171) ~ NA_real_,
                           TRUE ~ FO171),
         wine =   case_when(is.na(FO174) ~ NA_real_,
                           TRUE ~ FO174),
         liquor =   case_when(is.na(FO177) ~ NA_real_,
                             TRUE ~ FO177)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_23
'exam23' = read_sas(file.path(datadir, 'lex0_23.sas7bdat')) %>% select(PID, FP111, FP114, FP117) %>%
  mutate(beer =   case_when(is.na(FP111) ~ NA_real_,
                           TRUE ~ FP111),
         wine =   case_when(is.na(FP114) ~ NA_real_,
                           TRUE ~ FP114),
         liquor =   case_when(is.na(FP117) ~ NA_real_,
                             TRUE ~ FP117)) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## lex0_24
'exam24' = read_sas(file.path(datadir, 'lex0_24.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_25
'exam25' = read_sas(file.path(datadir, 'lex0_25.sas7bdat')) %>% select(PID) %>%
  mutate(rf_etoh = NA_real_),
## lex0_26
'exam26' = read_sas(file.path(datadir, 'lex0_26.sas7bdat')) %>% select(PID, FS329, FS333, FS337, FS341, FS345) %>%
  mutate(beer = FS329, white_wine = FS333, red_wine = FS337, other_wine = FS341, liquor = FS345) %>%
  mutate(drink_no = rowSums(.[c('beer','white_wine','red_wine','other_wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## e_exam_ex27_0_0075d
'exam27' = read_sas(file.path(datadir, 'e_exam_ex27_0_0075d.sas7bdat')) %>%
  select(PID, FT339, FT341, FT343, FT345, FT347) %>%
  mutate(beer = FT339, white_wine = FT341, red_wine = FT343, liquor = FT345, other = FT347) %>%
  mutate(drink_no = rowSums(.[c('beer','white_wine','red_wine','liquor', 'other')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## e_exam_ex28_0_0256d
'exam28' = read_sas(file.path(datadir, 'e_exam_ex28_0_0256d.sas7bdat')) %>%
  select(PID, FU040, FU042, FU044, FU046, FU048) %>%
  mutate(beer = FU040, white_wine=FU042, red_wine = FU044, liquor = FU046, other=FU048) %>%
  mutate(drink_no = rowSums(.[c('beer','white_wine','red_wine','liquor','other')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## e_exam_ex29_0_0210d
'exam29' = read_sas(file.path(datadir, 'e_exam_ex29_0_0210d.sas7bdat')) %>%
  select(PID, fv032, fv034, fv036, fv038) %>%
  mutate(beer = fv032, wine = fv034, liquor = fv036, other = fv038) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor','other')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh),
## e_exam_ex30_0_0274d
'exam30' = read_sas(file.path(datadir, 'e_exam_ex30_0_0274d.sas7bdat')) %>%
  select(PID, fw030, fw032, fw034) %>%
  mutate(beer = fw030, wine = fw032, liquor = fw034) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(PID, rf_etoh))

# Offspring cohort -----------------------------------------------------------------------------

datadir <- file.path(set_datadir(), 'archives','framoffspring','Datasets')
drink_offspring <- list(
## lex1_1
'exam1' = read_sas(file.path(datadir, 'lex1_1.sas7bdat')) %>% select(pid, A111:A113) %>%
  mutate(beer = A111, wine = A112, liquor =A113) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_2v2
'exam2' = read_sas(file.path(datadir, 'lex1_2v2.sas7bdat')) %>% select(pid, B117:B119) %>%
  mutate(beer = B117, wine = B118, liquor = B119) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_3
'exam3' = read_sas(file.path(datadir, 'lex1_3.sas7bdat')) %>% select(pid, C83, C86, C89) %>%
  mutate(beer = C83, wine = C86, liquor = C89) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_4
'exam4' = read_sas(file.path(datadir, 'lex1_4.sas7bdat')) %>% select(pid, D082, D085, D088) %>%
  mutate(beer = D082, wine = D085, liquor = D088) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_5
'exam5' = read_sas(file.path(datadir, 'lex1_5.sas7bdat')) %>% select(pid, E310, E313, E316) %>%
  mutate(beer = E310, wine = E313, liquor = E316) %>%
  mutate(drink_no = rowSums(.[c('beer','wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_6
'exam6' = read_sas(file.path(datadir, 'lex1_6.sas7bdat')) %>% select(pid, F276, F279, F282, F285) %>%
  mutate(beer = F276, white_wine = F279, red_wine = F282, liquor = F285) %>%
  mutate(drink_no = rowSums(.[c('beer','white_wine','red_wine','liquor')], na.rm=T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh),
## lex1_7
'exam7' = read_sas(file.path(datadir, 'lex1_7.sas7bdat')) %>% select(pid, G104, G107, G110, G113) %>%
  mutate(beer = G104, white_wine = G107, red_wine = G110, liquor = G113) %>%
  mutate(drink_no = rowSums(.[c('beer','white_wine','red_wine','liquor')], na.rm = T)) %>%
  mutate(rf_etoh = ifelse(drink_no > 21, 1, 0)) %>% # per week
  select(pid, rf_etoh))
