setwd("P:/FranWork/Hip fracture")
#library(sparklyr)
#sc <- spark_connect(master="local")
library(dplyr)
require(sas7bdat)
require(ggplot2)
library(haven)

#readin framingham and offspring general data
# ex1<-read.sas7bdat("data/sas/vr_dates_2014_a_0912d_yr_fram.sas7bdat")
ex1 <- read_sas('data/sas/vr_dates_2014_a_0912d_yr_fram.sas7bdat')
#write.csv(ex1,file="vr_dates_2014_a_0912d_yr_fram.csv")
# ex2<-read.sas7bdat("data/sas/vr_dates_2014_a_0912d_yr_offspring.sas7bdat")
ex2<-read_sas("data/sas/vr_dates_2014_a_0912d_yr_offspring.sas7bdat")
#0fram 1offspring 2os spouse 3gen3 7omni1 72omni2
tbl_df(ex1);tbl_df(ex2)

#validated data for year of fx
yr1<-read_sas("data/sas/vr_fxrev_2012_0_0746d_yr_fram.sas7bdat")
yr2<-read_sas("data/sas/vr_fxrev_2012_1_0747d_yr_offspring.sas7bdat")




###=================framingham cohort==============================
#save(ex1, ex2, yr1, yr2, file='data/rawdata.rda', compress=T)

library(reshape2)
#bl <- ex1 %>% select(PID, starts_with('examyr'), starts_with('age')) %>%
#  melt(id.vars='PID') #melt wide data to long data

#denominator data
yr_dat = ex1 %>% select(PID, starts_with('examyr')) %>% melt(id.vars = 'PID')#turn data to long form
yrs_participated <- yr_dat %>% group_by(PID) %>% summarise(startyr = min(value, na.rm=T),
                                                           endyr = max(value, na.rm=T)) %>%
  ungroup()#5079 3

#numerator data
first_frac_dat = yr1 %>% select(PID, YrFrac) %>%
  group_by(PID) %>% summarise(firstFracYr = min(YrFrac, na.rm=T)) %>%
  ungroup()
yrs_participated<- left_join(yrs_participated, first_frac_dat) %>%  #5079 4
  mutate(endyr = ifelse(is.na(firstFracYr), endyr, firstFracYr)) #adjusted for yrfrac
#two types of participants: those with firstFracYr,firstFracYr=endyr
#those without firstFracYr(NA), endyr = endyr
#names(yrs_participated)

#calculate age at fracture,NA means no fracture happened
yrs_participated <- left_join(yrs_participated, select(ex1, PID, age1,sex))#from ex1 select pid age1
yrs_participated <- mutate(yrs_participated, ageFrac = age1 + (firstFracYr-startyr))#startyr=examyr1,examyr1-age1=yob



#====================offspring cohort============================
#denominator data
yr_datoff = ex2 %>% select(PID, starts_with('examyr')) %>% melt(id.vars = 'PID')#turn data to long form
yrs_participatedoff <- yr_datoff %>% group_by(PID) %>% summarise(startyr = min(value, na.rm=T),
                                                           endyr = max(value, na.rm=T)) %>%
  ungroup()

#numerator data
first_frac_datoff = yr2 %>% filter(!is.na(of_hipfxtype))%>%
  select(PID, YrFrac) %>%
  group_by(PID) %>% summarise(firstFracYr = min(YrFrac, na.rm=T)) %>%
  ungroup()

#eliminate last exam before beginning of epoch
yrs_participatedoff <- left_join(yrs_participatedoff, first_frac_datoff) %>% #5507 4
  mutate(endyr = ifelse(is.na(firstFracYr), endyr, firstFracYr))
#problem here: firstFrac happened before endyr, participant stop contributing to person year.
#table(yrs_participated[,2]>yrs_participated[,3])
#ex2[ex2$PID %in% c(1830821,8797663),]
#yrs_participated[yrs_participated$PID %in% c(1830821,8797663),]
#yrs_participated<-yrs_participated[!yrs_participated$PID %in% c(1830821,8797663),] #5505 4


#check====two patients endyr 1stfx out of order
table(yrs_participatedoff[,2]>yrs_participatedoff[,3])
ex2[ex2$PID %in% c(1830821,8797663),]
yrs_participatedoff[yrs_participatedoff$PID %in% c(1830821,8797663),]#1stfrac before start,eliminate

yrs_participatedoff<-yrs_participatedoff %>% filter(startyr<=endyr) #5505 4

#calculate age at fracture,NA means no fracture happened
yrs_participatedoff <- left_join(yrs_participatedoff, select(ex2, PID, age1,sex))
yrs_participatedoff <- mutate(yrs_participatedoff, ageFrac = age1 + (firstFracYr-startyr))
#====================offspring================================


#====================fram+offspring============================
framboth<-rbind(yrs_participated,yrs_participatedoff)
#subject_timesboth <- lapply(framboth$PID, function(id){
#  x = filter(framboth, PID==id) #easy way of making PID into a vector
#  out<- tibble(yrs = seq(x$startyr, x$endyr, by=1),#create a tibble with 2 variables,calendar year and corresponding age
#               ages = seq(x$age1, x$age1+(x$endyr-x$startyr),by=1))
#  out<- out %>% mutate(ageFrac = ifelse(ages == x$ageFrac, 1,0)) %>% #use age frac to give a stop
#    mutate(ageFrac = ifelse(is.na(ageFrac), 0, ageFrac))#change NA in agefrac (no frac) to 0
#  return(out)
#})
#names(subject_timesboth) <-framboth$PID





#=============function to calculate incidence rate by age, decade, cohort, gender==========
decadeage<-function(epochstart,epochend,agestart,ageend,cohort,gender){
  epoch_yrs = seq(epochstart,epochend)

  subject_times <- lapply(cohort$PID, function(id){
    x = filter(cohort, PID==id) #easy way of making PID into a vector
    out<- tibble(yrs = seq(x$startyr, x$endyr, by=1),#create a tibble with 2 variables,calendar year and corresponding age
                 ages = seq(x$age1, x$age1+(x$endyr-x$startyr),by=1))
    out<- out %>% mutate(ageFrac = ifelse(ages == x$ageFrac, 1,0)) %>% #use age frac to give a stop
      mutate(ageFrac = ifelse(is.na(ageFrac), 0, ageFrac))#change NA in agefrac (no frac) to 0
    return(out)
  })

  denom_age = sapply(subject_times[cohort$sex==gender], function(d){ # Person-level within epoch and age
    d1 <- d %>% filter(ages %in% seq(agestart,ageend))
    return(length(intersect(epoch_yrs, d1$yrs)))
  })
  denominator_age = sum(denom_age) # Total person-years in age group within epoch

  num_age <- sapply(subject_times[cohort$sex==gender], function(d){ # Person-level within epoch and age
    d1 <- d %>% filter(ages %in% seq(agestart,ageend))
    n = sum(d1$ageFrac[d1$yrs %in% epoch_yrs])
    return(n)
  })
  numerator_age = sum(num_age) # Number of events within age group within epoch
  rate=round(numerator_age/denominator_age,digits=4)
  #result<-paste0(numerator_age,'/',denominator_age,'=',rate)
  print(rate)
}
#decadeage(2000,2010,70,79,framboth,1)
#=============function to calculate incidence rate by age, decade, cohort, gender==========


#write incidence rate into table
options(scipen=999)#disable scientific notation

#set decades and age breakdown
decades<-c(1970,1979,1980,1989,1990,1999,2000,2010)
agegroup<-c(60,69,70,79,80,100)

#male=1
breakdown1<-c(decadeage(1970,1979,60,69,framboth,1),decadeage(1980,1989,60,69,framboth,1),decadeage(1990,1999,60,69,framboth,1),decadeage(2000,2010,60,69,framboth,1),
              decadeage(1970,1979,70,79,framboth,1),decadeage(1980,1989,70,79,framboth,1),decadeage(1990,1999,70,79,framboth,1),decadeage(2000,2010,70,79,framboth,1),
              decadeage(1970,1979,80,89,framboth,1),decadeage(1980,1989,80,89,framboth,1),decadeage(1990,1999,80,89,framboth,1),decadeage(2000,2010,80,89,framboth,1))
table1<-matrix(breakdown1,4,3)

#colnames(table1,do.NULL=F)
rownames(table1)<-c("1970-1979","1980-1989","1990-1999","2000-2010")
colnames(table1)<-c("60-69","70-79","80above")
table1

#female=2
breakdown2<-c(decadeage(1970,1979,60,69,framboth,2),decadeage(1980,1989,60,69,framboth,2),decadeage(1990,1999,60,69,framboth,2),decadeage(2000,2010,60,69,framboth,2),
              decadeage(1970,1979,70,79,framboth,2),decadeage(1980,1989,70,79,framboth,2),decadeage(1990,1999,70,79,framboth,2),decadeage(2000,2010,70,79,framboth,2),
              decadeage(1970,1979,80,89,framboth,2),decadeage(1980,1989,80,89,framboth,2),decadeage(1990,1999,80,89,framboth,2),decadeage(2000,2010,80,89,framboth,2))
table2<-matrix(breakdown2,4,3)
rownames(table2)<-c("1970-1979","1980-1989","1990-1999","2000-2010")
colnames(table2)<-c("60-69","70-79","80above")
table2



#===========================1990 census by age and sex as standard population==============
#https://www2.census.gov/library/publications/decennial/1990/cp-1/cp-1-1.pdf table 15

census1990m<-c(4947047+4532307,3409306+2399768,1366094+857698)#60s 70s 80above
census1990f<-c(5669120+5579428,4585517+3721601,2567645+2222467)

#1990 census male1 weight
weight1<-c(census1990m[1]/sum(census1990m),#60s
           census1990m[2]/sum(census1990m),#70s
           census1990m[3]/sum(census1990m))#80above

#1990 census female2 weight
weight2<-c(census1990f[1]/sum(census1990f),#60s
           census1990f[2]/sum(census1990f),#70s
           census1990f[3]/sum(census1990f))#80above

#calculate age adjusted for male
a1<-c((table1[1,1]*weight1[1]+table1[1,2]*weight1[2]+table1[1,3]*weight1[3])*100000,
      (table1[2,1]*weight1[1]+table1[2,2]*weight1[2]+table1[2,3]*weight1[3])*100000,
      (table1[3,1]*weight1[1]+table1[3,2]*weight1[2]+table1[3,3]*weight1[3])*100000,
      (table1[4,1]*weight1[1]+table1[4,2]*weight1[2]+table1[4,3]*weight1[3])*100000)
b=c(1970,1980,1990,2000)
#plot age standardized rate per 100,000 age>60 in 1970 1980 1990 2000
plot(b,a1,type='l',ylim=c(0,500),
     xlab='decades',ylab='age sex adjusted fx incidence rate per 100,000',
     main='Male 1990 census 60 and above')

#===================use asht espoissonTest to get CI=============================
#male crude rate table
crudenum1<-matrix(c(5,11,14+4,4,8,18+16,1,6,13+20),3,3,byrow=T)
rownames(crudenum1)<-c("60s", "70s", "80above")
colnames(crudenum1)<-c("1980-1989","1990-1999","2000-2010")
crudedeno1<-matrix(c(5527,3571,1125+90,5145,3589,1477+422,6010,3721,1576+696),3,3,byrow=T)#60s 70s 80above
rownames(crudedeno1)<-c("60s", "70s", "80above")
colnames(crudedeno1)<-c("1980-1989","1990-1999","2000-2010")

crude1980m=c(5,11,14+4)#60s 70s 80above
denom1980m=c(5527,3571,1125+90)#crude pooled incidence rate wont change
#standardpopulation1990=c(4947047+4532307,3409306+2399768,1366094+857698)#60s 70s 80above

require(asht)#get CI
#male
ci80m<-wspoissonTest(crudenum1[1,],census1990m/(crudedeno1[1,]*sum(census1990m)),mult=10^5,nullValue = T)
ci90m<-wspoissonTest(crudenum1[2,],census1990m/(crudedeno1[2,]*sum(census1990m)),mult=10^5,nullValue = T)
ci00m<-wspoissonTest(crudenum1[3,],census1990m/(crudedeno1[3,]*sum(census1990m)),mult=10^5,nullValue = T)
#===================use asht espoissonTest to get CI=============================

#manually add CI to plot
segments(1980, ci80m[4]$conf.int[1], 1980, ci80m[4]$conf.int[2],col='red')
segments(1990, ci90m[4]$conf.int[1], 1990, ci90m[4]$conf.int[2],col='red')
segments(2000, ci00m[4]$conf.int[1],2000, ci00m[4]$conf.int[2],col='red')





a2<-c((table2[1,1]*weight2[1]+table2[1,2]*weight2[2]+table2[1,3]*weight2[3])*100000,
      (table2[2,1]*weight2[1]+table2[2,2]*weight2[2]+table2[2,3]*weight2[3])*100000,
      (table2[3,1]*weight2[1]+table2[3,2]*weight2[2]+table2[3,3]*weight2[3])*100000,
      (table2[4,1]*weight2[1]+table2[4,2]*weight2[2]+table2[4,3]*weight2[3])*100000)
plot(b,a2,type='l',ylim=c(400,900),
     xlab='decades',ylab='age sex adjusted fx incidence rate per 100,000',
     main='Female 1990 census 60 and above')

#female crude rate table
crudenum2<-matrix(c(10,29,61+18,6,39,65+16,9,12,45+20),3,3,byrow=T)
rownames(crudenum2)<-c("60s", "70s", "80above")
colnames(crudenum2)<-c("1980-1989","1990-1999","2000-2010")
crudedeno2<-matrix(c(6414,5610,2217+279,5679,5043,2988+422,7238,4705,2575+696),3,3,byrow=T)#60s 70s 80above
rownames(crudedeno2)<-c("60s", "70s", "80above")
colnames(crudedeno2)<-c("1980-1989","1990-1999","2000-2010")
ci80f<-wspoissonTest(crudenum2[1,],census1990f/(crudedeno2[1,]*sum(census1990f)),mult=10^5,nullValue = T)
ci90f<-wspoissonTest(crudenum2[2,],census1990f/(crudedeno2[2,]*sum(census1990f)),mult=10^5,nullValue = T)
ci00f<-wspoissonTest(crudenum2[3,],census1990f/(crudedeno2[3,]*sum(census1990f)),mult=10^5,nullValue = T)

segments(1980, ci80f[4]$conf.int[1], 1980, ci80f[4]$conf.int[2],col='red')
segments(1990, ci90f[4]$conf.int[1], 1990, ci90f[4]$conf.int[2],col='red')
segments(2000, ci00f[4]$conf.int[1],2000, ci00f[4]$conf.int[2],col='red')




#use epitools.ageadjust.direct to get CI
#require(epitools)
#census1990m<-c(4947047+4532307,3409306+2399768,1366094+857698)
#rownames(census1990m)<-c("60s","70s","80above")
#ageadjust.direct(table1[2,],population[,1],stdpop=census1990m)








#mark numbers
text(1981, 656,"650")
text(1981, 759,"759")
text(1981, 547,"547")

text(1991, 575,"574")
text(1991, 490,"490")
text(1991, 674,"674")

text(1999, 400,"394")
text(1999, 324,"324")
text(1999, 473,"473")



#====================================tim data============================
#framrecut<-left_join(yrs_participated,select(ex1,PID,sex,age1,examyr1))%>%
#  mutate(yob=examyr1-age1)%>%
#  mutate(AgeE1=1948-yob)%>%
#  mutate(AgeE2=1977-yob)%>%
#  mutate(AgeE3=1986-yob)%>%
#  mutate(AgeE4=1992-yob)%>%
#  mutate(AgeE5=2004-yob)%>%
#  mutate(endyr1=endyr)%>% #endyr1=original endyr,endyr=original+firstfrac
#  mutate(e1status= ifelse(is.na(firstFracYr),#0=censor 1=present 2=firstfrac
#                          ifelse(endyr1<1948,0,1),#0=censor
#                          ifelse(firstFracYr %in% 1948:1976,2,1)))%>%
#  mutate(e2status= ifelse(is.na(firstFracYr),
#                          ifelse(endyr1<1977,0,1),
#                          ifelse(firstFracYr %in% 1977:1985,2,1)))%>%
#  mutate(e3status= ifelse(is.na(firstFracYr),
#                          ifelse(endyr1<1986,0,1),
#                          ifelse(firstFracYr %in% 1986:1991,2,1)))%>%
#  mutate(e4status= ifelse(is.na(firstFracYr),
#                          ifelse(endyr1<1992,0,1),
#                          ifelse(firstFracYr %in% 1992:2003,2,1)))%>%
#  mutate(e5status= ifelse(is.na(firstFracYr),
#                          ifelse(endyr1<2004,0,1),
#                          ifelse(firstFracYr %in% 2004:2010,2,1)))%>%
#  select(PID,yob,AgeE1,AgeE2,AgeE3,AgeE4,AgeE5,endyr1,firstFracYr,
#         e1status,e2status,e3status,e4status,e5status)
#check
#framrecut[,c('firstFracYr','endyr1','e1status')]
#table(framrecut$e1status)
#table(framrecut$e2status)
#table(framrecut$e3status)
#table(framrecut$e4status)
#table(framrecut$e5status)
#write.csv(framrecut,file=('framrecut.csv'))
#====================================tim data============================





#===============1990 standard million
#https://seer.cancer.gov/stdpopulations/stdpop.19ages.html

#age-adjusted incidence rate=incidence / standard population *10000
#1990 population
#42685+40657+32145+24612+15817+12385=168301
#60s weight
#(42685+40657)/(42685+40657+32145+24612+15817+12385)#0.4951961
#70s weight
#(32145+24612)/(42685+40657+32145+24612+15817+12385)#0.3372351
#80above weight
#(15817+12385)/(42685+40657+32145+24612+15817+12385)#0.1675688

#age adjusted rate 1980:60s+70s+80above
#http://www.statcan.gc.ca/eng/dai/btd/asr
#age adjusted means crude incidence rate multiplied by standard population weight
#(crude incidence as numerator/crude denominator)*(proportional population/total population)
#1980:0.006507588
#0.0013*0.4951961+0.0044*0.3372351+((75+22)/(3342+369))*0.1675688

#1990:0.005741059
#0.0009*0.4951961+0.0054*0.3372351+((83+21)/(4465+551))*0.1675688

#2000:0.003940587
#0.0008*0.4951961+0.0021*0.3372351+((58+28)/(4151+930))*0.1675688



#===============poisson analysis==============================
## Including offset(log(n)) in the right hand side
#model.1 <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)
## Using the offset option
#model.1 <- glm(cases ~ city + age.range, offset = log(n), family = poisson(link = "log"), data = nonmel)

## Results from regular Poisson
#summary(model.1)





















#====================old code=======================
head(yr1);head(yr2)
ex1$age2
#create last exam date
names(ex1) #5079

#denominator data
dat1<-ex1 %>%
  select(idtype,PID,sex,age1,starts_with("examyr"))%>%
  mutate(lastexyr=pmax(examyr1,examyr2,examyr3,examyr4,examyr5,
                       examyr6,examyr7,examyr8,examyr9,examyr10,
                       examyr11,examyr12,examyr13,examyr14,examyr15,
                       examyr16,examyr17,examyr18,examyr19,examyr20,
                       examyr21,examyr22,examyr23,examyr24,examyr25,
                       examyr26,examyr27,examyr28,examyr29,examyr30,
                       na.rm=T))%>%
  mutate(yob=examyr1-age1)

#table(dat1$yob)#1877-1923.
#from 1937 the oldest become 60 yr old. earliest fx is 1936.

#numerator data
#remove duplicates in yr1;names(yr1);dim(yr1)#559;n_distinct(yr1$PID)#490
dat2<-yr1%>% #559
 mutate(firstfx=as.numeric(!duplicated(PID))) #490


#merge ex1+yr1, right_join to keep to yr1 PID
dat3<-right_join(dat1,dat2,by="PID")%>%  #490 rows
  select(IDTYPE,PID,sex,yob,YrFrac,firstfx,lastexyr)%>%
  filter(firstfx==1)%>%
  mutate(agefx=YrFrac-yob)%>%
  mutate(agefxdec=ifelse(agefx>=40 & agefx<50,40,
                         ifelse(agefx>=50 & agefx<60,50,
                         ifelse(agefx>=60 & agefx<70,60,
                         ifelse(agefx>=70 & agefx<80,70,
                         ifelse(agefx>=80 & agefx<90,80,90))))))%>%
  mutate(epoch=ifelse(YrFrac<1977,0,
                       ifelse(YrFrac>=1977 & YrFrac<1986, 1,
                              ifelse(YrFrac>=1986 & YrFrac<1992,2,
                                     ifelse(YrFrac>=1992 & YrFrac<2004,3,4)))))%>%
  select(-firstfx)


hist(dat3$agefx,xlab='age at fx',ylab='fx frequency',main='agefx by fxfrequency')
hist(dat3$agefxdec)
hist(dat3$epoch,ylab='fx frequency',main='epoch by fx',
     xlab='48-76              77-85            86-91              92-03             04-10')

#check agefx agefxdec
table(dat3$agefx)
cbind(dat3$agefx,dat3$agefxdec)#conclusion:three 40s->50s, one 100->90s
dat3[287,]#agefx=100 epoch4
#conclusion:numerator has only 3 ppl difference btw agefx and agefxdec
#check epoch vs age at cut year
#fix and rerun table



###################################calculate numerator each epoch######################
#calculate numerator based on agefxdec
t1<-as.matrix(table(dat3$epoch,dat3$agefxdec));t1
#      40 50 60 70 80 90
#48-76  3 16 24 36 14  0
#77-85  0  1 20 39 68 10
#86-91  0  0  0 22 42 14
#92-03  0  0  0 25 91 31
#04-10  0  0  0  0 18 16




########################write a function to calculate denom each epoch##################
#create new dataset to index for denominator,keep dat1 pid
dat4<-left_join(dat1,dat2,by='PID')%>%  #NA created with nonmatching observations
  select(IDTYPE,PID,sex,yob,YrFrac,firstfx,lastexyr)%>%
  #filter(firstfx==1)%>%
  mutate(agefx=YrFrac-yob)%>%
  mutate(agefxdec=ifelse(agefx>=40 & agefx<50,40,
                         ifelse(agefx>=50 & agefx<60,50,
                                ifelse(agefx>=60 & agefx<70,60,
                                       ifelse(agefx>=70 & agefx<80,70,
                                              ifelse(agefx>=80 & agefx<90,80,90))))))%>%
  #mutate(epoch_fx=ifelse(YrFrac<1977,0,
  #                    ifelse(YrFrac>=1977 & YrFrac<1986, 1,
  #                           ifelse(YrFrac>=1986 & YrFrac<1992,2,
  #                                  ifelse(YrFrac>=1992 & YrFrac<2004,3,4)))))
  mutate(epoch=ifelse(YrFrac<1977,0,
                      ifelse(YrFrac>=1977 & YrFrac<1986, 1,
                             ifelse(YrFrac>=1986 & YrFrac<1992,2,
                                    ifelse(YrFrac>=1992 & YrFrac<2004,3,4)))))


#calculate denominator
myfun1<-function(dat,e){
  epochs=data.frame()
  for (i in e) {
    ages<- i - dat$yob #age at year
    indic1<-dat$lastexyr>=i
    indic2<-dat$YrFrac>=i & dat$firstfx==1
    ages_i<-ages[indic1==T & indic2==T]
    denyr<-table(cut(ages_i, c(40,50,60,70,80,90,100), include.lowest = T, right=F))
    epochs<-rbind(epochs,denyr)
  }
  print(colSums(epochs))
}



ages<- 1977 - dat4$yob
indic1<-dat4$lastexyr<1977
indic2<-dat4$YrFrac<1977 & dat4$firstfx==1
ages_1977<-ages[indic1==T & indic2==T]
ages_1977<-length(dat4$PID)-
  length(dat4$lastexyr[dat4$lastexyr<1977])-
  length(dat4$PID[dat4$YrFrac<1977 & dat4$firstfx==1])


denyr<-table(cut(ages_i, c(40,50,60,70,80,90,100), include.lowest = T, right=F))
epochs<-rbind(epochs,denyr)




e0<-myfun1(dat=dat4,e=1948:1976)
e1<-myfun1(dat=dat4,e=1977:1985)
e2<-myfun1(dat=dat4,e=1986:1991)
e3<-myfun1(dat=dat4,e=1992:2003)
e4<-myfun1(dat=dat4,e=2004:2010)
t2<-as.matrix(rbind(e0,e1,e2,e3,e4))
#           40s   50s    60s  70s    80s   90s
#1948-1976  2901  4198  3142  1411   173     0
#1977-1985     0    81  1107   964   505    21
#1986-1991     0     0   120   650   306    42
#1992-2003     0     0     0   326   517    83
#2004-2010     0     0     0     0    38    32




#==============check denominator 1977-1985
dat5<-dat4
dat5 %>%
  mutate(ages=1977 - yob)%>%
  mutate(indic1=lastexyr>=1977) %>%
  mutate(indic2=YrFrac>=1977) %>%
  mutate(indic3=firstfx>=1977) %>%
  mutate(ages1977<-ages %in% c(indic1==T,indic2==T, indic3==T) )
  #ages1977<-ages[indic1==T | indic2==T | indic3==T ]
####################################calculate rate#####################################
print(t1/t2)
#           40          50          60          70          80          90
# 0.001034126 0.003811339 0.007638447 0.025513820 0.080924855
#1             0.012345679 0.018066847 0.040456432 0.134653465 0.476190476
#2                         0.000000000 0.033846154 0.137254902 0.333333333
#3                                     0.076687117 0.176015474 0.373493976
#4                                                 0.473684211 0.500000000





#check
denepoch<-data.frame()
for (i in 1977:1985) {
  ages<- i - dat1$yob #age at year
  indic<-dat1$lastexyr>=i
  indic1<-dat1$YrFrac>=i
  ages_i<-ages[indic]
  ages_i1<-ages_i[indic1]
  denyr<-table(cut(ages_i, c(40,50,60,70,80,90,100), include.lowest = T, right=F))
  denepoch<-rbind(denepoch,denyr)
  print(x<-colSums(denepoch))
}

#check1
ages<- 1977- dat1$yob #age at 1977
indic<-dat1$lastexyr>=1977
indic1<-length(dat3$YrFrac[dat3$YrFrac<1977])
ages_i<-ages[indic]
ages_i1<-ages_i[indic1]
denyr<-table(cut(ages_i1, c(40,50,60,70,80,90,100), include.lowest = T, right=F))







#==============================offspring==================================
#denominator data
dat1off<-ex2 %>% #ex2 includes pid 1-7,select1,5013
  filter(idtype==1)%>% #idtype1=offspring
  select(idtype,PID,sex,age1,starts_with("examyr"))%>%
  mutate(lastexyr=pmax(examyr1,examyr2,examyr3,examyr4,examyr5,
                       examyr6,examyr7,examyr8,examyr9,
                       na.rm=T))%>%
  mutate(yob=examyr1-age1)


#numerator data
dat2off<-
  yr2%>%
  filter(!is.na(of_hipfxtype))%>%
  mutate(firstfx=as.numeric(!duplicated(PID))) #


#merge ex1+yr1, right_join to keep to yr1 PID
dat3off<-right_join(dat1off,dat2off,by="PID")%>%
  select(IDTYPE,PID,sex,yob,YrFrac,firstfx,lastexyr)%>%
  filter(firstfx==1)%>%
  mutate(agefx=YrFrac-yob)%>%
  mutate(agefxdec=ifelse(agefx>=40 & agefx<50,40,
                         ifelse(agefx>=50 & agefx<60,50,
                         ifelse(agefx>=60 & agefx<70,60,
                                ifelse(agefx>=70 &agefx<80,70,
                                       ifelse(agefx>=80 & agefx<90,80,90))))))%>%
  mutate(epoch=ifelse(YrFrac<1977,0,
                      ifelse(YrFrac>=1977 & YrFrac<1986, 1,
                             ifelse(YrFrac>=1986 & YrFrac<1992,2,
                                    ifelse(YrFrac>=1992 & YrFrac<2004,3,4)))))%>%
  select(-firstfx)

hist(dat3off$agefx)
hist(dat3off$agefxdec)
hist(dat3off$epoch)



t1off<-as.matrix(table(dat3off$epoch,dat3off$agefxdec))
#  50 60 70 80
#0  4  0  0  0
#1  6  2  0  0
#2  1  4  0  0
#3 10 13 21 10
#4  1  5 11 12

dat4off<-left_join(dat1off,dat2off,by='PID')%>%
  select(IDTYPE,PID,sex,yob,YrFrac,firstfx,lastexyr)%>%
  #filter(firstfx==1)%>%
  mutate(agefx=YrFrac-yob)%>%
  mutate(agefxdec=ifelse(agefx>=40 & agefx<50,40,
                         ifelse(agefx>=50 & agefx<60,50,
                                ifelse(agefx>=60 & agefx<70,60,
                                       ifelse(agefx>=70 & agefx<80,70,
                                              ifelse(agefx>=80 & agefx<90,80,90))))))%>%
  mutate(epoch=ifelse(YrFrac<1977,0,
                      ifelse(YrFrac>=1977 & YrFrac<1986, 1,
                             ifelse(YrFrac>=1986 & YrFrac<1992,2,
                                    ifelse(YrFrac>=1992 & YrFrac<2004,3,4)))))%>%
  select(-firstfx)



#######################calculate denominator############
e0<-myfun1(dat=dat4off,e=1948:1976)
e1<-myfun1(dat=dat4off,e=1977:1985)
e2<-myfun1(dat=dat4off,e=1986:1991)
e3<-myfun1(dat=dat4off,e=1992:2003)
e4<-myfun1(dat=dat4off,e=2004:2010)
t2off<-as.matrix(rbind(e0,e1,e2,e3,e4))

###########calculate numerator
#calculate numerator based on agefxdec
print(t1off/t2off)
#           40          50          60          70          80          90
#0 0.001443001 0.003968254 0.000000000                                 Inf
#1 0.000000000 0.011661808 0.006060606 0.000000000                     Inf
#2 0.000000000 0.020618557 0.023923445 0.000000000 0.000000000
#3 0.051282051 0.065217391 0.078651685 0.102803738 0.219512195
#4             0.500000000 0.444444444 0.357142857 0.722222222














#combine original+offspring
yr<-rbind(yr1[,c("IDTYPE","PID","YrFrac")],yr2[,c("IDTYPE", "PID","YrFrac")])
dim(yr)
fracfreq<-as.matrix(table(yr$YrFrac))
dim(fracfreq)

yr[,c("PID","YrFrac")]%>%
  group_by(YrFrac)%>%
  summarize(sum_frac=table(YrFrac))

names(yr)


#year<-as.data.frame(yr.1[,1])
#class(fracfreq)

yr.2<-cbind(fracfreq,year)
plot(yr.2[,2],yr.2[,1],xlab='Year',ylab='Fx',main='validated data set')
hist(yr.2[,1],breaks=20)
barplot(table(yr.2))
summary(lm(yr.2[,1]~yr.2[,2]))
#               Estimate    Std. Error t value  Pr(>|t|)
#(Intercept)   -3276.3730   234.6255  -13.96   <2e-16 ***
#  yr.2[, 2]       1.6831     0.1188   14.16   <2e-16 ***

#change table objects to data.frame
yr.1<-data.frame(rbind(table(yr$YrFrac)))
dim(yr.1)
dim(t(yr.1))
yr.1









#analysis plan:1.take all cohorts as base, subtract those fracture dates
#                before time frame, subtract those droppedout/died, use as
#                denominator over the number of fracture occurred.
#2.: age at the exam stratified analysis
#3.: time variant variable analysis
#4.age adjusted trend
#5.:separate first fracture from other fracture
