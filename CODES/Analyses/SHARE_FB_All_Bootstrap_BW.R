
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: Last update May 2021
# 
# Description: Code to process the data, do the analyses and create the graphs
#   and tables used in the article:
#   "Gil-Clavel, S., Zagheni, E. & Bordone, V. Close Social Networks Among Older
#   Adults: The Online and Offline Perspectives. Popul Res Policy Rev (2021). 
#   https://doi.org/10.1007/s11113-021-09682-3"
# 
# Computer Environment:
#   - Windows 
#   - R - 4.0.2 (2020)
#   - Rstudio (1.3.1056)
#   - Microsoft Windows 10 Enterprise
#
##################################

#### Libraries ####
library(foreach)
library(tidyverse)
library(foreign)
library(survey)
library(gridExtra)
library(ggplot2)
library(ggforce)
library(MASS)

# Cleaning Memory
rm(list = ls())
gc()

#### Root Folder and Dir to Save ####
Direc<-"~/OlderAdultsCloseSocialNetworks/"
Save<-"RESULTS/"

#### Call R Function "Functions_SGC.R" into environment ####

source(paste0(Direc,"CODES/Analyses/Functions_SGC.R"))

#### --- SHARE --- ####

#### Opening Modules ####
DEMOG<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_dn.dta")
IT_DB<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_it.dta")
# USING IMPUTED DATA:
IMPUTATIONS<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_gv_imputations.dta")
IMPUTATIONS<-IMPUTATIONS%>%filter(implicat==1)
MENTAL<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_gv_health.dta")
SOT_NET<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_gv_networks.dta")
WEIGHTS<-read.dta("DataBases/sharew6_rel7-0-0_ALL_datasets_stata/sharew6_rel7-0-0_gv_weights.dta")

#### Merging data ####

DB_merge<-merge(DEMOG[,c("mergeid","country","language","dn003_","dn004_","dn005c","dn010_","dn041_","dn042_","dn503_")],
                IT_DB,by=c("mergeid","country","language"))

DB_merge<-merge(DB_merge,IMPUTATIONS[,c("mergeid","country","language","gender","age","isced","nchild")],
                by=c("mergeid","country","language"))

DB_merge<-merge(DB_merge,SOT_NET[,c("mergeid","country","language",
                                    "sn_person_1","sn_person_2","sn_person_3","sn_person_4","sn_person_5","sn_person_6","sn_person_7","rel_1","rel_2","rel_3","rel_4",
                                    "rel_5","rel_6","rel_7","prx_1","prx_2","prx_3","prx_4","prx_5","prx_6","prx_7","contact_1",
                                    "contact_2","contact_3","contact_4","contact_5","contact_6","contact_7","close_1","close_2","close_3","close_4","close_5",
                                    "close_6","close_7")], #,"sn012_"
                by=c("mergeid","country","language"))

DB_merge<-merge(DB_merge,MENTAL[,c(1,5,6,11,28,33)],by=c("mergeid","country","language"))

DB_merge<-merge(DB_merge,WEIGHTS[,c(1,5:14)],by=c("mergeid","country","language"))

DB_merge<-DB_merge[!is.na(DB_merge$cciw_w6),]

#### Building Database ####

# Variable Definitions 
# dn003_mod Year of birth 
# dn004_mod Born in the country of interview 
# dn005c	Foreign country of birth coding
# dn010_	Highest school degree obtained
#  # ISCED level 0 – Early childhood education
#  # ISCED level 1 – Primary education 
#  # ISCED level 2 – Lower secondary education
#  # ISCED level 3 – Upper secondary education
#  # ISCED level 4 – Post-secondary non-tertiary education
#  # ISCED level 5 – Short-cycle tertiary education
#  # ISCED level 6 – Bachelor’s or equivalent level
#  # ISCED level 7 – Master’s or equivalent level
#  # ISCED level 8 – Doctoral or equivalent level
#  # dn041_	Years education
# dn042_	Male or female
# dn503_	Born a citizen of country of interview
# ch001_ Number of children 
# close_Network_Closeness
#  # 1	Not very close
#  # 2	Somewhat close
#  # 3	Very close
#  # 4	Extremely close
# IT004_UseWWW:
#  # During the past 7 days, have you used the Internet, for e-mailing, searching for information, making purchases, or
#  # for any other purpose at least once?
#  # 1) Yes
#  # 5) No

#### Transforming variables to match with FB database
# Country| Sex| Education| Age| MIGRANT| Parents| Friends

DB_Tranm<-DB_merge[,c("mergeid","psu","ssu","stratum1","stratum2","cciw_w6","country","gender","age","isced","nchild","it004_")] #"dn042_",
colnames(DB_Tranm)<-c("mergeid","psu","ssu","stratum1","stratum2","cciw_w6","COUNTRY","SEX","AGEN","ISCED","NCHILD","INTERNET")

# Age Variable: 50-64 and 65+
DB_Tranm$AGE<-ifelse(is.na(DB_Tranm$AGEN),NA,
                     ifelse(DB_Tranm$AGEN<=64,"50-64","65-65"))

# Migrant
DB_Tranm$MIGRANT<-ifelse(is.na(DB_merge$dn004_),0,
                       ifelse(DB_merge$dn004_=="No",1,0))
sum(DB_Tranm$MIGRANT)

# Parent
DB_Tranm$PARENT<-ifelse(is.na(DB_Tranm$NCHILD),0,
                        ifelse(DB_Tranm$NCHILD>0,1,0))

# Close Friends
CLOSE<-c("Somewhat close","Very close","Extremely close") # 
Closeness<-c("close_1","close_2","close_3","close_4","close_5",
             "close_6","close_7")
UNO<-rep(0,dim(DB_Tranm)[1])
MATRIX_CLOSES<-DB_merge[,Closeness]
for(i in 1:7){
  UNO<-ifelse(is.na(DB_merge[,Closeness[i]]%in%CLOSE),0,
              ifelse(DB_merge[,Closeness[i]]%in%CLOSE,UNO+1,0))
  levels(MATRIX_CLOSES[,i])<-c(0,0,0,0,1,1,1)
}
UNO<-ifelse(UNO>0,1,0)
DB_Tranm$FRIENDS=UNO
sum(DB_Tranm$FRIENDS)

# Education
BELOW<-c("Isced-97 code 1","Isced-97 code 2","Isced-97 code 3","Isced-97 code 4","Isced-97 code 5")
COLLEGE<-c("Isced-97 code 6")
UNSP<-c("Refusal","Don't know","None","Other","Still in school")
DB_Tranm$EDUCATION<-ifelse(DB_Tranm$ISCED%in%BELOW,"Below College",
                           ifelse(DB_Tranm$ISCED%in%COLLEGE,"College or Above","Unspecified"))
DB_Tranm$EDUCATION<-as.factor(DB_Tranm$EDUCATION)
DB_Tranm$EDUCATION<-relevel(DB_Tranm$EDUCATION,ref = "Below College")

# Subregions
# Keeping only EU
DB_Tranm<-DB_Tranm[DB_Tranm$COUNTRY!="Israel",]
DB_Tranm$COUNTRY<-droplevels(DB_Tranm$COUNTRY)

ene<-which(levels(DB_Tranm$COUNTRY)=="Czech Republic")
levels(DB_Tranm$COUNTRY)[ene]<-"Czechia"

CODES<-read.csv("DataBases/CountryByRegion.csv")
CODES<-CODES[CODES$region=="Europe",c(1,8)]
colnames(CODES)<-c("COUNTRY","SUBREGION")

DB_Tranm<-merge(DB_Tranm,CODES,by=c("COUNTRY"))
DB_Tranm$SUBREGION<-droplevels(DB_Tranm$SUBREGION)

#### Transforming DB  ####
DB_Tranm_Gr<-DB_Tranm[!is.na(DB_Tranm$INTERNET)&
                      (DB_Tranm$INTERNET=="No"|DB_Tranm$INTERNET=="Yes"),]

DB_Tranm_Gr$INTERNET<-droplevels(DB_Tranm_Gr$INTERNET)
DB_Tranm_Gr$INTERNET<-relevel(DB_Tranm_Gr$INTERNET,ref = "No")

DB_Tranm_Gr$AGE<-as.factor(DB_Tranm_Gr$AGE)
levels(DB_Tranm_Gr$AGE)<-factor(c("50-64","65-65"))
DB_Tranm_Gr$AGE<-relevel(DB_Tranm_Gr$AGE,ref = "50-64")

DB_Tranm_Gr$MIGRANT<-as.factor(DB_Tranm_Gr$MIGRANT)
levels(DB_Tranm_Gr$MIGRANT)<-factor(c("No","Yes"))
DB_Tranm_Gr$MIGRANT<-relevel(DB_Tranm_Gr$MIGRANT,ref = "No")

DB_Tranm_Gr$PARENT<-as.factor(DB_Tranm_Gr$PARENT)
levels(DB_Tranm_Gr$PARENT)<-factor(c("Parent: No","Parent: Yes"))
DB_Tranm_Gr$PARENT<-relevel(DB_Tranm_Gr$PARENT,ref = "Parent: No")

DB_Tranm_Gr$FRIENDS<-as.factor(DB_Tranm_Gr$FRIENDS)
levels(DB_Tranm_Gr$FRIENDS)<-factor(c("Friend: No","Friend: Yes"))
DB_Tranm_Gr$FRIENDS<-relevel(DB_Tranm_Gr$FRIENDS,ref = "Friend: No")

#### SHARE Internet Model ####

VARnames<-c("Intercept","Female","Age: 65+","College or Above","Unspecified",
            "Parent: Yes","Immigrant: Yes","Friend: Yes")

#### Survey Design ####

Disenio<-svydesign(id=~psu+ssu,strata=~stratum1+stratum2, weights=~cciw_w6,
                   data=DB_Tranm_Gr, nest=TRUE)
options(survey.lonely.psu="adjust")

#### Checking structure of the data - Totals ####

VARIABLES=c("SEX","AGE","EDUCATION","PARENT","MIGRANT","FRIENDS","INTERNET")
ene=2^3 # PARENT(2)*MIGRANT(2)*FRIENDS(2)
SHARE_Total=data.frame(Name=rep("o",ene),
                        Internet=rep(0,ene),
                        Internet_V=rep(0,ene),
                        InternetNo=rep(0,ene),
                        InternetNo_V=rep(0,ene),
                       Total=rep(0,ene),
                       Total_V=rep(0,ene),stringsAsFactors = FALSE)
cont=1
for(i in VARIABLES){
  LEVELS=levels(DB_Tranm_Gr[,i])
  for(j in LEVELS){
    uno<-svytotal(~I((eval(parse(text = i))==j)*(INTERNET=="Yes")),Disenio)
    dos<-svytotal(~I((eval(parse(text = i))==j)*(INTERNET=="No")),Disenio)
    tres<-svytotal(~I((eval(parse(text = i))==j)),Disenio)
    SHARE_Total[cont,1]=paste0(i,j)
    SHARE_Total[cont,2:7]=c(uno[1],SE(uno),dos[1],SE(dos),tres[2],SE(tres)[2])
    cont=cont+1
  }
}

#### Checking structure of the data - Ratios ####
 
SHARE_Ratios=data.frame(Name=rep("o",ene),
                        Internet=rep(0,ene),
                        Internet_V=rep(0,ene),
                        InternetNo=rep(0,ene),
                        InternetNo_V=rep(0,ene),stringsAsFactors = FALSE)
cont=1
for(i in VARIABLES){
  LEVELS=levels(DB_Tranm_Gr[,i])
  for(j in LEVELS){
    uno<-svyratio(~I((eval(parse(text = i))==j)*(INTERNET=="Yes")),
             ~(INTERNET=="Yes"),Disenio)
    
    dos<-svyratio(~I((eval(parse(text = i))==j)*(INTERNET=="No")),
             ~(INTERNET=="No"),Disenio)
    SHARE_Ratios[cont,1]=paste0(i,j)
    SHARE_Ratios[cont,2:5]=c(uno$ratio,SE(uno),dos$ratio,SE(dos))
    cont=cont+1
  }
}

#### Checking structure of the data by Characteristic ####

CHARS=c("MIGRANT","FRIENDS","PARENT")
VARIABLES=c("SEX","AGE","EDUCATION")


ene=36+12 # 3(CHARS)*2(SEX)*2(AGE)*3(EDU) + 1(none)*2(SEX)*2(AGE)*3(EDU)

SHARE_Ratios_Chars=data.frame(Name=rep("o",ene),
                              SEX=rep("o",ene),
                              AGE=rep("o",ene),
                              EDUCATION=rep("o",ene),
                              Internet=rep(0,ene),
                              Internet_V=rep(0,ene),
                              InternetNo=rep(0,ene),
                              InternetNo_V=rep(0,ene),
                              Both=rep(0,ene),
                              Both_V=rep(0,ene),
                              stringsAsFactors = FALSE)
cont=1
for(j in CHARS){
  lj=levels(DB_Tranm_Gr[,j])[2]
  for(sex in c("Male","Female")){
    for(age in c("50-64","65-65")){
      for(edu in c("Below College","College or Above","Unspecified")){
        uno<-svyratio(~I((eval(parse(text = j))==lj)*
                           (eval(parse(text = "SEX"))==sex)*
                           (eval(parse(text = "AGE"))==age)*
                           (eval(parse(text = "EDUCATION"))==edu)*
                           (INTERNET=="Yes")),
                      ~I((eval(parse(text = j))==lj)*(INTERNET=="Yes")),Disenio)
        
        dos<-svyratio(~I((eval(parse(text = j))==lj)*
                           (eval(parse(text = "SEX"))==sex)*
                           (eval(parse(text = "AGE"))==age)*
                           (eval(parse(text = "EDUCATION"))==edu)*
                           (INTERNET=="No")),
                      ~I((eval(parse(text = j))==lj)*(INTERNET=="No")),Disenio)
        
        tres<-svyratio(~I((eval(parse(text = j))==lj)*
                           (eval(parse(text = "SEX"))==sex)*
                           (eval(parse(text = "AGE"))==age)*
                           (eval(parse(text = "EDUCATION"))==edu)),
                      ~(eval(parse(text = j))==lj),Disenio)
        
        SHARE_Ratios_Chars[cont,1]=paste(j,lj,sep="_")
        SHARE_Ratios_Chars[cont,2]=sex
        SHARE_Ratios_Chars[cont,3]=age
        SHARE_Ratios_Chars[cont,4]=edu
        SHARE_Ratios_Chars[cont,5:10]=c(uno$ratio,SE(uno),dos$ratio,SE(dos),
                                       tres$ratio,SE(tres))
        cont=cont+1
      }
    }
  }
}

j1=levels(DB_Tranm_Gr[,CHARS[1]])[1]
j2=levels(DB_Tranm_Gr[,CHARS[2]])[1]
j3=levels(DB_Tranm_Gr[,CHARS[3]])[1]
for(sex in c("Male","Female")){
  for(age in c("50-64","65-65")){
    for(edu in c("Below College","College or Above","Unspecified")){
      uno<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)*
                         (eval(parse(text = "SEX"))==sex)*
                         (eval(parse(text = "AGE"))==age)*
                         (eval(parse(text = "EDUCATION"))==edu)*
                         (INTERNET=="Yes")),
                    ~I((INTERNET=="Yes")*(eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      dos<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)*
                         (eval(parse(text = "SEX"))==sex)*
                         (eval(parse(text = "AGE"))==age)*
                         (eval(parse(text = "EDUCATION"))==edu)*
                         (INTERNET=="No")),
                    ~I((INTERNET=="No")*(eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      tres<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)*
                         (eval(parse(text = "SEX"))==sex)*
                         (eval(parse(text = "AGE"))==age)*
                         (eval(parse(text = "EDUCATION"))==edu)),
                    ~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      SHARE_Ratios_Chars[cont,1]="None"
      SHARE_Ratios_Chars[cont,2]=sex
      SHARE_Ratios_Chars[cont,3]=age
      SHARE_Ratios_Chars[cont,4]=edu
      SHARE_Ratios_Chars[cont,5:10]=c(uno$ratio,SE(uno),dos$ratio,SE(dos),
                                      tres$ratio,SE(tres))
      cont=cont+1
    }
  }
}

#### Without Education #####

VARIABLES=c("SEX","AGE")
CHARS=c("MIGRANT","FRIENDS","PARENT")

ene=12+4 # 3(CHARS)*2(SEX)*2(AGE) + 1(none)*2(SEX)*2(AGE)

SHARE_Ratios_NoEdu=data.frame(Name=rep("o",ene),
                              SEX=rep("o",ene),
                              AGE=rep("o",ene),
                              Internet=rep(0,ene),
                              Internet_V=rep(0,ene),
                              InternetNo=rep(0,ene),
                              InternetNo_V=rep(0,ene),
                              Both=rep(0,ene),
                              Both_V=rep(0,ene),
                              stringsAsFactors = FALSE)
cont=1
for(j in CHARS){
  lj=levels(DB_Tranm_Gr[,j])[2]
  for(sex in c("Male","Female")){
    for(age in c("50-64","65-65")){
        uno<-svyratio(~I((eval(parse(text = j))==lj)*
                           (eval(parse(text = "SEX"))==sex)*
                           (eval(parse(text = "AGE"))==age)*
                           (INTERNET=="Yes")),
                      ~I((eval(parse(text = j))==lj)*(INTERNET=="Yes")),Disenio)
        
        dos<-svyratio(~I((eval(parse(text = j))==lj)*
                           (eval(parse(text = "SEX"))==sex)*
                           (eval(parse(text = "AGE"))==age)*
                           (INTERNET=="No")),
                      ~I((eval(parse(text = j))==lj)*(INTERNET=="No")),Disenio)
        
        tres<-svyratio(~I((eval(parse(text = j))==lj)*
                            (eval(parse(text = "SEX"))==sex)*
                            (eval(parse(text = "AGE"))==age)),
                       ~(eval(parse(text = j))==lj),Disenio)
        
        SHARE_Ratios_NoEdu[cont,1]=paste(j,lj,sep="_")
        SHARE_Ratios_NoEdu[cont,2]=sex
        SHARE_Ratios_NoEdu[cont,3]=age
        SHARE_Ratios_NoEdu[cont,4:9]=c(uno$ratio,SE(uno),dos$ratio,SE(dos),
                                        tres$ratio,SE(tres))
        cont=cont+1
      }
    }
}

j1=levels(DB_Tranm_Gr[,CHARS[1]])[1]
j2=levels(DB_Tranm_Gr[,CHARS[2]])[1]
j3=levels(DB_Tranm_Gr[,CHARS[3]])[1]
for(sex in c("Male","Female")){
  for(age in c("50-64","65-65")){
      uno<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)*
                         (eval(parse(text = "SEX"))==sex)*
                         (eval(parse(text = "AGE"))==age)*
                         (INTERNET=="Yes")),
                    ~I((INTERNET=="Yes")*(eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      dos<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)*
                         (eval(parse(text = "SEX"))==sex)*
                         (eval(parse(text = "AGE"))==age)*
                         (INTERNET=="No")),
                    ~I((INTERNET=="No")*(eval(parse(text = CHARS[1]))==j1)*
                         (eval(parse(text = CHARS[2]))==j2)*
                         (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      tres<-svyratio(~I((eval(parse(text = CHARS[1]))==j1)*
                          (eval(parse(text = CHARS[2]))==j2)*
                          (eval(parse(text = CHARS[3]))==j3)*
                          (eval(parse(text = "SEX"))==sex)*
                          (eval(parse(text = "AGE"))==age)),
                     ~I((eval(parse(text = CHARS[1]))==j1)*
                          (eval(parse(text = CHARS[2]))==j2)*
                          (eval(parse(text = CHARS[3]))==j3)),Disenio)
      
      SHARE_Ratios_NoEdu[cont,1]="None"
      SHARE_Ratios_NoEdu[cont,2]=sex
      SHARE_Ratios_NoEdu[cont,3]=age
      SHARE_Ratios_NoEdu[cont,4:9]=c(uno$ratio,SE(uno),dos$ratio,SE(dos),
                                      tres$ratio,SE(tres))
      cont=cont+1
  }
}

#### SHARE LOGIT MODEL ####

VAROR<-c("(Intercept)","SEXFemale","AGE65-65","PARENTParent: Yes","MIGRANTYes")
VARnames<-c("Intercept","Female","Age: 65+","Parent: Yes","Immigrant: Yes")

# Factor Levels:
VAROR_DF=data.frame(VARnames=VARnames)
row.names(VAROR_DF)=VAROR

#### Having FRIENDS (Internet=1) ####

fitMultI1 <- svyglm(FRIENDS~SEX+AGE+PARENT+MIGRANT,
                  design=Disenio, family=binomial(),
                  subset=I(INTERNET=="Yes"))

# Confidence Interval
CI<-CI_logit_svy(fitMultI1,"BOTH",VAROR_DF)

# Plot
(PshareI1<-ggplot(CI, aes(x=NAMES, y=mean,label=`p-value<`)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.6,position = position_dodge(1)) +
    geom_point(size=3,position = position_dodge(1))+
    coord_flip()+
    scale_x_discrete(labels = VARnames, breaks=VARnames)+
    scale_y_continuous(trans = "log10",
                       expand = c(0,0),
                       limits = c(0.02,15),
                       breaks = c(0.03,0.1,0.3,1.0,3.0,10.0),
                       labels = function(x)ifelse(x<1,format(x,digits = 4),
                                format(x,digits = 0)))+ # 10^log10(c(0.1,0.3,1.0,3.0,10.0))
    guides(shape=guide_legend(title = "Age",reverse = TRUE))+
    geom_hline(yintercept = 1, linetype="dashed")+
    geom_vline(aes(xintercept=0.4),size=1.2)+
    theme(panel.spacing = unit(1, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",text = element_text(size=18),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"), 
          panel.grid.major.x = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"),
          strip.background = element_rect(color = NA,
                                          fill = NA, size = 1),
          axis.line = element_line(color = 'black'),
          plot.caption = element_text(hjust = 0.5,size = 18))+
    geom_text(hjust=0, vjust=0,size=8,position = position_dodge(1)))

# Save Plot info to use later
PshareI1<-PshareI1+labs(caption = "(b) Internet User ")
SHARE_Tab_FriendsI1=CI

#### Having FRIENDS (Internet=0) ####

fitMultI0 <- svyglm(FRIENDS~SEX+AGE+PARENT+MIGRANT,
                  design=Disenio, family=binomial(),
                  subset=I(INTERNET=="No")) 

# Confidence Interval
CI<-CI_logit_svy(fitMultI0,"BOTH",VAROR_DF)

# Plot
(PshareI0<-ggplot(CI, aes(x=NAMES, y=mean,label=`p-value<`)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.6,position = position_dodge(1)) +
    coord_flip()+
    geom_point(size=3,position = position_dodge(1))+
    scale_x_discrete(labels = VARnames, breaks=VARnames)+
    scale_y_continuous(trans = "log10",
                       expand = c(0,0),
                       limits = c(0.02,15),
                       breaks = c(0.03,0.1,0.3,1.0,3.0,10.0),
                       labels = function(x)ifelse(x<1,format(x,digits = 4),
                              format(x,digits = 0)))+ # 10^log10(c(0.1,0.3,1.0,3.0,10.0))
    guides(shape=guide_legend(title = "Age",reverse = TRUE))+
    geom_hline(yintercept=0.01)+
    geom_hline(yintercept = 1, linetype="dashed")+
    geom_vline(aes(xintercept=0.4),size=1.2)+
    theme(panel.spacing = unit(1, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",text = element_text(size=18),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"), 
          panel.grid.major.x = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"),
          strip.background = element_rect(color = NA,
                                          fill = NA, size = 1),
          axis.line = element_line(color = 'black'),
          plot.caption = element_text(hjust = 0.5,size = 18))+
    geom_text(hjust=0, vjust=0,size=8,position = position_dodge(1)))

# Save Plot info to use later
PshareI0<-PshareI0+labs(caption = "(a) Non-Internet User ")
SHARE_Tab_Friends_I0=CI


#### --- Facebook --- ####

### Labels and Functions

Edu_Names<-c("50-64"="50-64","65-65"="65+","DAU"="DAU","DE"="DAU",
             "FEMALE"="Female","MALE"="Male",
             `1`='Below College',`2`='College or Above',`3`='Unspecified',
             "000"="None", "111"="Im.Pa.Fr.", "110"="Im.Pa.","101"="Im.Fr.",
             "011"="Pa.Fr.","100"="Im.", "010"="Pa.","001"="Fr.")

Combine_Names<-c("000"="None",
                 "010"="Parents (Pa.)",
                 "001"="Close Friends (Fr.)",
                 "100"="Immigrant (Im.)",
                 "110"="Im.&Pa.",
                 "101"="Im.&Fr.",
                 "011"="Pa.&Fr.",
                 "111"="Im.&Pa.&Fr.")

### FB users Uploading the data

BASE<-read.csv("DataBases/BASE_BackUp.csv",stringsAsFactors = FALSE)

BASE<-BASE%>%dplyr::select(-FAMEXPAT)%>%
  group_by(CODE,COUNTRY,REGION,SUBREGION,SEX,EDUCATION,AGE,EXPAT,
           PARENTS,FRIENDS,TIME,DATE)%>%
  summarise(estimate_dau=sum(estimate_dau),estimate_mau=sum(estimate_mau))

BASE[BASE$SEX==1,"SEX"]=0 # Men
BASE[BASE$SEX==2,"SEX"]=1 # Women

BASE<-BASE[!BASE$COUNTRY%in%c("Cyprus","Israel"),]

# Just those that are part of SHARE
UNIQUE_SHARE_COUNTRIES<-levels(DB_Tranm_Gr$COUNTRY)

BASE<-BASE[BASE$COUNTRY%in%UNIQUE_SHARE_COUNTRIES,]

unique(BASE$COUNTRY)

# Adding the columns:
# 
# * COMBINE: Combination
# * n: Number of observations per country, sex, age, education and combination.

BASE$COMBINE=paste0(BASE$EXPAT,BASE$PARENTS,BASE$FRIENDS)

#### Adding number of observations ####
BASE<-BASE %>%
  group_by(COUNTRY,SEX,EDUCATION,AGE,COMBINE)%>%
  add_count()

BASE_Summ<-BASE %>%
  group_by(COUNTRY,CODE,SUBREGION,SEX,EDUCATION,AGE,COMBINE)%>%
  summarise(mu_D = mean(estimate_dau), med_D = median(estimate_dau),
            min_D=min(estimate_dau),max_D=max(estimate_dau),
            sd_D=sd(estimate_dau),IQR_D=IQR(estimate_dau),n=n[1]) 


#### Descriptive Heatmap ####

BASE_Summ2<-BASE_Summ%>%
  group_by(SEX,EDUCATION,AGE,COMBINE)%>%
  summarise(mu_D=sum(mu_D))

BASE_Summ2$SEX<-factor(BASE_Summ2$SEX,
                       levels = c(1,0),labels = c("Female","Male")) #,ordered = TRUE
BASE_Summ2$EDUCATION<-factor(BASE_Summ2$EDUCATION,
                             levels = c(3,2,1),
                             labels = c("Unspecified","College or Above","Below College")) #,ordered = TRUE
BASE_Summ2$AGE<-factor(BASE_Summ2$AGE,
                       levels = c("50-64","65-65"),labels = c("50-64","65+")) #,ordered = TRUE

BASE_Summ2$COMBINE<-factor(BASE_Summ2$COMBINE,
                           levels = names(Combine_Names),labels = Combine_Names)

BASE_Summ2%>%
  filter(COMBINE%in%c("None","Parents (Pa.)","Close Friends (Fr.)",
                      "Immigrant (Im.)"))%>%
  group_by(COMBINE)%>%
  summarise(VALUE=sum(mu_D))

#### Heat Map: Big Totals ####

COMB_NONE<-BASE_Summ2%>%
  mutate(FOUND=str_detect(COMBINE,"None"))%>%
  filter(FOUND)%>%
  summarise(NONE=sum(mu_D))

COMB_PARENT<-BASE_Summ2%>%
  mutate(FOUND=str_detect(COMBINE,"Pa."))%>%
  filter(FOUND)%>%
  summarise(PARENT=sum(mu_D))

COMB_FRIEND<-BASE_Summ2%>%
  mutate(FOUND=str_detect(COMBINE,"Fr."))%>%
  filter(FOUND)%>%
  summarise(FRIEND=sum(mu_D))

COMB_MIGRANT<-BASE_Summ2%>%
  mutate(FOUND=str_detect(COMBINE,"Im."))%>%
  filter(FOUND)%>%
  summarise(MIGRANT=sum(mu_D))

ALL<-COMB_NONE%>%
  left_join(COMB_PARENT)%>%
  left_join(COMB_FRIEND)%>%
  left_join(COMB_MIGRANT)%>%
  gather("TYPE","VALUE",NONE,PARENT,FRIEND,MIGRANT)

ALL$TYPE<-factor(ALL$TYPE,
                 levels=c("NONE","PARENT","FRIEND","MIGRANT"),
                 labels = c("None","Parent","Friend","Immigrant"))

ALL$EDUCATION<-droplevels(ALL$EDUCATION)

ALL$EDUCATION<-factor(ALL$EDUCATION,
                      c("College or Above","Below College","Unspecified"))
ALL$TYPE<-factor(ALL$TYPE,
                 levels = rev(c("None","Parent",
                                "Friend","Immigrant")),
                  labels=rev(c("100%\nNone","100%\nParent",
                       "100%\nFriend","100%\nImmigrant"))) 
  
ALL<-ALL%>%
  group_by(TYPE)%>%
  mutate(PROP=100*VALUE/sum(VALUE))

#### heat map SHARE ####

SHARE_Ratios_Chars2<-SHARE_Ratios_Chars

SHARE_Ratios_Chars2$Name<-factor(SHARE_Ratios_Chars2$Name,
                        level=unique(SHARE_Ratios_Chars2$Name),
                        labels = c("100%\nImmigrant","100%\nFriend",
                                   "100%\nParent","100%\nNone"))

colnames(SHARE_Ratios_Chars2)[1]<-"TYPE"

SHARE_Ratios_Chars2$EDUCATION<-factor(SHARE_Ratios_Chars2$EDUCATION,
                        level=c("College or Above","Below College","Unspecified"))

SHARE_Ratios_Chars2$AGE<-factor(SHARE_Ratios_Chars2$AGE,
                              level=c("50-64","65-65"),
                              labels = c("50-64","65+"))

SHARE_Ratios_Chars2[,5:10]<-100*SHARE_Ratios_Chars2[,5:10]

SHARE_Ratios_Chars2%>%group_by(TYPE)%>%summarise(sum(InternetNo))

#### Joining SHARE and Facebook ####

Ratios_SHARE_FB<-SHARE_Ratios_Chars2%>%left_join(ALL)

(CORS1<-ggplot(Ratios_SHARE_FB,aes(PROP,Internet,color=EDUCATION))+
  geom_point())

(CORS1<-ggplot(Ratios_SHARE_FB,aes(PROP,Internet,color=TYPE,shape=SEX,size=AGE))+
    geom_point()+ theme_minimal()+
    geom_abline(intercept = 0, slope = 1,color="black",linetype = "dashed")+
    labs(x="Facebook %",y="SHARE-internet %",color="Characteristic",shape="Sex",size="Age")+
    xlim(0,60)+ylim(0,60)+ guides(color=FALSE)+
    scale_color_manual(values=c("#060606", "#686868", "#b6b6b6", "#e7e7e7"), 
                       labels=c("Immigrant","Friend","Parent","None"))+
    theme(legend.position = "right"))

(RATS1<-Ratios_SHARE_FB%>%
  ggplot() +
  geom_tile(aes(TYPE,EDUCATION,fill= PROP/Internet))+
  geom_text(aes(TYPE,EDUCATION,color=ifelse(PROP/Internet<5,"255","1"),
                label = paste0(round(PROP, 2),"% | ",round(Internet, 2),"%")),
            size=8) +
  scale_color_manual(values = c("white", "black"))+
  facet_grid(AGE+SEX~.)+
  theme_minimal(base_size = 25)+
  theme(axis.text.x = element_text(hjust = 0.5,size = 25),
        axis.title = element_blank(),
        legend.position= "right",
        plot.caption = element_text(hjust =1,size = 25))+
  scale_fill_distiller(palette = "Greys",direction = 1,trans = 'log10')+
  guides(fill=guide_colorbar(title = "Facebook / Internet",
                             title.position = "right",
                             title.hjust = 0.5,
                             title.theme = element_text(angle=-90,size = 25),
                             barwidth = 0.5, barheight = 40,
                             ticks.colour = "black",
                             ticks.linewidth = 10,ticks = TRUE),color=FALSE))

#### Heat Map: Big Totals without education ####
BASE_Summ2_EDU<-BASE_Summ2[,-2]

COMB_NONE<-BASE_Summ2_EDU%>%
  mutate(FOUND=str_detect(COMBINE,"None"))%>%
  filter(FOUND)%>%group_by(SEX,AGE)%>%
  summarise(NONE=sum(mu_D))

COMB_PARENT<-BASE_Summ2_EDU%>%
  mutate(FOUND=str_detect(COMBINE,"Pa."))%>%
  filter(FOUND)%>%group_by(SEX,AGE)%>%
  summarise(PARENT=sum(mu_D))

COMB_FRIEND<-BASE_Summ2_EDU%>%
  mutate(FOUND=str_detect(COMBINE,"Fr."))%>%
  filter(FOUND)%>%group_by(SEX,AGE)%>%
  summarise(FRIEND=sum(mu_D))

COMB_MIGRANT<-BASE_Summ2_EDU%>%
  mutate(FOUND=str_detect(COMBINE,"Im."))%>%
  filter(FOUND)%>%group_by(SEX,AGE)%>%
  summarise(MIGRANT=sum(mu_D))

ALL_EDU<-COMB_NONE%>%
  left_join(COMB_PARENT)%>%
  left_join(COMB_FRIEND)%>%
  left_join(COMB_MIGRANT)%>%
  # left_join(COMB_FAMEX)%>%
  gather("TYPE","VALUE",NONE,PARENT,FRIEND,MIGRANT) #,FAMEX

ALL_EDU$TYPE<-factor(ALL_EDU$TYPE,
                 levels=c("NONE","PARENT","FRIEND","MIGRANT"),
                 labels = c("None","Parent","Friend","Immigrant"))

ALL_EDU$TYPE<-factor(ALL_EDU$TYPE,
                 levels = rev(c("None","Parent",
                                "Friend","Immigrant")),
                 labels=rev(c("100%\nNone","100%\nParent",
                              "100%\nFriend","100%\nImmigrant")))

ALL_EDU<-ALL_EDU%>%
  group_by(TYPE)%>%
  mutate(PROP=100*VALUE/sum(VALUE))

#### heat map SHARE without education ####

SHARE_Ratios_NoEdu2<-SHARE_Ratios_NoEdu

SHARE_Ratios_NoEdu2$Name<-factor(SHARE_Ratios_NoEdu2$Name,
                                level=unique(SHARE_Ratios_NoEdu2$Name),
                                labels = c("100%\nImmigrant","100%\nFriend",
                                           "100%\nParent","100%\nNone"))#"100%\nFam. Away",

colnames(SHARE_Ratios_NoEdu2)[1]<-"TYPE"

SHARE_Ratios_NoEdu2$AGE<-factor(SHARE_Ratios_NoEdu2$AGE,
                               level=c("50-64","65-65"),
                               labels = c("50-64","65+"))

SHARE_Ratios_NoEdu2[,4:9]<-100*SHARE_Ratios_NoEdu2[,4:9]

SHARE_Ratios_NoEdu2%>%group_by(TYPE)%>%summarise(sum(InternetNo))

#### NO EDU: Joining SHARE and Facebook ####
Ratios_SHARE_FB_EDU<-SHARE_Ratios_NoEdu2%>%left_join(ALL_EDU)

(CORS2<-ggplot(Ratios_SHARE_FB_EDU,aes(PROP,Internet,color=TYPE,shape=SEX,size=AGE))+
  geom_point()+ theme_minimal()+
  geom_abline(intercept = 0, slope = 1,color="black",linetype = "dashed")+
  labs(x="Facebook %",y="SHARE-internet %",color="Characteristic",
       shape="Sex",size="Age")+
  xlim(0,60)+ylim(0,60)+ 
  guides(shape=FALSE,size=FALSE,
         color = guide_legend(override.aes = list(size=5)))+
  scale_color_manual(values=c("#060606", "#686868", "#b6b6b6", "#e7e7e7"), #, "#56B4E9", "#D55E00", "#009E73"
                       labels=c("Immigrant","Friend","Parent","None"))+
  theme(legend.position = "top",legend.title = element_text(size=12),
        legend.text = element_text(size=12)))

leg1=get_legend(CORS1)
leg2=get_legend(CORS2)

CORS1_<-CORS1+labs(caption = "a) Broken down by education\n")+
  theme(plot.caption = element_text(hjust = 0.5,vjust = 0.5,size=12),legend.position = "none")

CORS2_<-CORS2+labs(caption = "b) No broken down by education")+
  theme(plot.caption = element_text(hjust = 0.5,vjust = 0.5,size=12),legend.position = "none")

# png(paste0(Save,'Ratio_CorrsALL_BW.png'),units = "cm",width = 15*1.3,
#   height = 15*1.3,res = 200)
grid.arrange(leg2,CORS1_,CORS2_,leg1,
             ncol=2,nrow=3,
             layout_matrix = cbind(c(1,2,3),c(1,4,4)),
             widths=c(5,1),
             heights = c(1,5,5))
# dev.off()


Ratios_SHARE_FB_EDU$SEX<-factor(Ratios_SHARE_FB_EDU$SEX,levels = c("Male","Female"))

(RATS2<-Ratios_SHARE_FB_EDU%>%
  ggplot() +
  geom_tile(aes(TYPE,SEX,fill= PROP/Internet))+
  geom_text(aes(TYPE,SEX,color=ifelse(PROP/Internet<1.5,"255","1"),
                label = paste0(round(PROP, 2),"% | ",round(Internet, 2),"%")),
            size=8) +
  scale_color_manual(values = c("white", "black"))+
  facet_grid(AGE~.)+
  theme_minimal(base_size = 25)+
  theme(axis.text.x = element_text(hjust = 0.5,size = 25),
        axis.title = element_blank(),
        legend.position= "right",
        plot.caption = element_text(hjust =1,size = 25))+
  scale_fill_distiller(palette = "Greys",direction = 1)+ #,trans = 'log10'
  guides(fill=guide_colorbar(title = "Facebook / Internet",
                             title.position = "right",
                             title.hjust = 0.5,
                             title.theme = element_text(angle=-90,size = 25),
                             barwidth = 0.5, barheight = 20,
                             ticks.colour = "black",
                             ticks.linewidth = 10,ticks = TRUE),color=FALSE))

RATS1_<-RATS1+labs(caption = "a) Broken down by education\n")+
  theme(plot.caption = element_text(hjust = 0.5,vjust = 0.5))

RATS2_<-RATS2+labs(caption = "b) No broken down by education")+
  theme(plot.caption = element_text(hjust = 0.5,vjust = 0.5))

# png(paste0(Save,'Ratio_HeatGraphALL_edunoEDU_BW.png'),units = "cm",
#   width = 15*3.2,height = 15*3,res = 200)
grid.arrange(RATS1_,RATS2_,
             ncol=1,nrow=2,
             layout_matrix = cbind(c(1,2)),
             widths=c(6),
             heights = c(4.5,2.5))
# dev.off()


#### Preparing Variables for models ####

#### FB to factors ####
TABLE_FB<-BASE

TABLE_FB$SEX<-as.factor(TABLE_FB$SEX)
levels(TABLE_FB$SEX)<-factor(c("Male","Female"))
TABLE_FB$SEX<-relevel(TABLE_FB$SEX,ref = "Male")

TABLE_FB$AGE<-as.factor(TABLE_FB$AGE)
levels(TABLE_FB$AGE)<-factor(c("50-64","65-65"))
TABLE_FB$AGE<-relevel(TABLE_FB$AGE,ref = "50-64")

colnames(TABLE_FB)[8]<-"MIGRANT"
TABLE_FB$MIGRANT<-as.factor(TABLE_FB$MIGRANT)
levels(TABLE_FB$MIGRANT)<-factor(c("No","Yes"))
TABLE_FB$MIGRANT<-relevel(TABLE_FB$MIGRANT,ref = "No")

colnames(TABLE_FB)[9]<-"PARENT"
TABLE_FB$PARENT<-as.factor(TABLE_FB$PARENT)
levels(TABLE_FB$PARENT)<-factor(c("Parent: No","Parent: Yes"))
TABLE_FB$PARENT<-relevel(TABLE_FB$PARENT,ref = "Parent: No")

TABLE_FB$FRIENDS<-as.factor(TABLE_FB$FRIENDS)
levels(TABLE_FB$FRIENDS)<-factor(c("Friend: No","Friend: Yes"))
TABLE_FB$FRIENDS<-relevel(TABLE_FB$FRIENDS,ref = "Friend: No")

TABLE_FB$EDUCATION<-as.factor(TABLE_FB$EDUCATION)
levels(TABLE_FB$EDUCATION)<-factor(c("Below College","College or Above",
                                   "Unspecified"))
TABLE_FB$EDUCATION<-relevel(TABLE_FB$EDUCATION,ref = "Below College")

TABLE_FB$COMBINE=paste0(TABLE_FB$MIGRANT,TABLE_FB$PARENT,TABLE_FB$FRIENDS)

#### The models ####

VAROR<-c("(Intercept)","SEXFemale","AGE65-65","PARENTParent: Yes","MIGRANTYes")

VARnames<-c("Intercept","Female","Age: 65+","Parent: Yes","Immigrant: Yes")

# Factor Levels:
VAROR_DF=data.frame(VARnames=VARnames)
row.names(VAROR_DF)=VAROR

#### Bootstrap GLM Facebook Coefficients ####

BOOTS<-foreach(j=1:1000,.combine = rbind)%do%{
  SAMPLING_GLM(TABLE_FB,j)
}

SUM<-BOOTS%>%
  group_by(NAMES)%>%
  summarise(mean=mean(VALUES),err=sd(VALUES))%>%
  mutate(lower=mean-1.96*err,upper=mean+1.96*err,
         t_value=mean/err,`p-value`=round(2*pnorm(-abs(mean/err)),digits = 5))%>%
  mutate(`p-value<`=p_val_iks(`p-value`),AGE="BOTH",TYPE="Friends")

SUM<-as.data.frame(SUM)
SUM$NAMES<-VAROR_DF[SUM$NAMES,]
row.names(SUM)<-SUM$NAMES
SUM<-SUM[VAROR_DF$VARnames,]
SUM<-SUM[,c("NAMES","lower","mean","upper","p-value","err","AGE","TYPE","p-value<")]

#### Tables and Graphs - FB ####

All_T<-as.data.frame(SUM) #EFE
All_T[,c(2:4,6)]<-exp(All_T[,c(2:4,6)])

All_T$NAMES=factor(All_T$NAMES,levels = VARnames[length(VARnames):1])
All_T$TYPE=factor(All_T$TYPE,levels = c("Sex","Parents","Friends","Immigrant"))

All_T$lower<-as.numeric(All_T$lower)
All_T$mean<-as.numeric(All_T$mean)
All_T$upper<-as.numeric(All_T$upper)

All_T$`p-value<`<-p_val_iks(All_T$`p-value`)

CI<-rbind(All_T)

(FRIENDS<-ggplot(CI, aes(x=NAMES, y=mean,label=`p-value<`)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.6,position = position_dodge(1)) +
    geom_point(size=3,position = position_dodge(1))+
    coord_flip()+ 
    scale_x_discrete(labels = VARnames, breaks=VARnames,expand = c(0.1,0.1))+
    scale_y_continuous(trans = "log10",
                       expand = c(0,0),
                       limits = c(0.02,15),
                       breaks = c(0.03,0.1,0.3,1.0,3.0,10.0),
                       labels = function(x)ifelse(x<1,format(x,digits = 4),format(x,digits = 0)))+ # 10^log10(c(0.1,0.3,1.0,3.0,10.0))
    guides(shape=guide_legend(title = "Age",reverse = TRUE))+
    geom_hline(yintercept = 1, linetype="dashed")+
    theme(panel.spacing = unit(1, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",text = element_text(size = 18),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"), 
          panel.grid.major.x = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"),
          strip.background = element_rect(color = NA,
                                          fill = NA, size = 1),
          axis.line = element_line(color = 'black'),
          plot.caption = element_text(hjust = 0.5,size = 18))+
    geom_text(hjust=0, vjust=0,size=8,position = position_dodge(1)))

FRIENDS<-FRIENDS+labs(caption = "(b) Facebook")

FACEBOOK_Tab_Friend=CI

UNO<-PshareI0$data
UNO$TYPE="SH_I0"
DOS<-PshareI1$data
DOS$TYPE="SH_I1"
TRES<-(FRIENDS$data)[,c(2,3,4,6,5,1,7,9)]
TRES$TYPE="FB"
names(TRES)<-names(UNO)

ALL=rbind(UNO,DOS)
ALL=rbind(ALL,TRES)

ALL$TYPE<-factor(ALL$TYPE,levels = c("FB","SH_I1","SH_I0"),
              labels = c("Facebook","Internet","Non-Internet"))

(FRIENDS_ALL<-ggplot(ALL, aes(x=NAMES, y=mean,label=`p-value<`,
                          shape=TYPE,color=NAMES)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper), 
                  width=.6,position = position_dodge(1)) +
    geom_point(size=3,position = position_dodge(1))+
    coord_flip()+ 
    scale_x_discrete(labels = VARnames, 
                     breaks=VARnames,expand = c(0.1,0.1))+
    scale_y_continuous(trans = "log10",
                       expand = c(0,0),
                       limits = c(0.03,15),
                       breaks = c(0.1,0.3,1.0,3.0,10.0),
                       labels = function(x)ifelse(x<1,format(x,digits = 4),
                                                  format(x,digits = 0)))+ 
    scale_color_manual(values=c("#060606", "#858585","#060606", "#858585","#060606"), 
                       labels=c("Immigrant","Friend","Parent","None"))+
    guides(color=FALSE,shape= guide_legend(title = "Data",reverse=TRUE))+
    geom_hline(yintercept = 1, linetype="dashed")+
    theme(panel.spacing = unit(1, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",text = element_text(size = 18),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"), 
          panel.grid.major.x = element_line(size = 0.25, 
                                            linetype = 'dashed',
                                            colour = "gray"),
          strip.background = element_rect(color = NA,
                                          fill = NA, size = 1),
          axis.line = element_line(color = 'black'),
          plot.caption = element_text(hjust = 0.5,size = 18))+
    geom_text(hjust=0, vjust=0,size=8,position = position_dodge(1)))

# ggsave(paste0(Save,"Both_Friends_FB_SHARE.png"),width = 22.5,height =20,
#   units = "cm")

#### The Tables ####
VAROR<-c("(Intercept)","SEXFemale","AGE65-65",
         "PARENTParent: Yes","MIGRANTYes","AWAYAway:Yes")
VARnames<-c("Intercept","Female","Age: 65+",
            "Parent: Yes","Immigrant: Yes","Fam.Abroad: Yes")
VAROR_DF2=data.frame(VARnames=VARnames)
row.names(VAROR_DF2)=VAROR

UNO<-CI_DB_logit2(fitMultI0,"BOTH",VAROR_DF2)
UNO$`p-value<`<-p_val_iks(UNO$`p-value`)

DOS<-CI_DB_logit2(fitMultI1,"BOTH",VAROR_DF2)
DOS$`p-value<`<-p_val_iks(DOS$`p-value`)

TRES<-SUM[,c("lower","mean","upper","p-value","err","NAMES","AGE","p-value<")]

All_T<-cbind(UNO[,c(2,4,1,3,5,8)],
             DOS[,c(2,4,1,3,5,8)])
All_T<-cbind(All_T,TRES[,c(2,4,1,3,5,8)])

row.names(All_T)=as.character(VAROR_DF$VARnames)

# 1 - pnorm(2.37) + pnorm(-2.37) = 0.01778809
# 2*pnorm(-2.37) = 0.01778809

All_T2<-All_T[,c(1,5,7,11,13,17)]
names(All_T2)<-NULL
rownames(All_T2)<-NULL
All_T2<-as.matrix(All_T2)
MEANS=matrix(as.double(All_T2[,c(1,3,5)]),ncol=3)
ERR=matrix(as.double(All_T2[,1+c(1,3,5)]),ncol=3)
DIFF1=round(c(MEANS[,1]-MEANS[,2]),digits = 4)
(VAL1=c(MEANS[,1]-MEANS[,2])/(ERR[,1]^2 + ERR[,2]^2)^(1/2))
# Testing Xi^2
# W<-VAL1
# W_chi=pchisq(W^2, df=1, lower.tail=FALSE)
# c("***","**","*","."," ")[findInterval(W_chi, sort(c(1, 0.1, 0.05, 0.01, 0.001, 0)))]
(VAL1<-2*pnorm(-abs(VAL1)))
(VAL1<-p_val_iks(VAL1))
lower1=round(DIFF1-1.96*(ERR[,1]^2 + ERR[,2]^2)^(1/2),digits = 4)
upper1=round(DIFF1+1.96*(ERR[,1]^2 + ERR[,2]^2)^(1/2),digits = 4)

DIFF2=round(c(MEANS[,1]-MEANS[,3]),digits = 4)
(VAL2=c(MEANS[,1]-MEANS[,3])/(ERR[,1]^2 + ERR[,3]^2)^(1/2))
(VAL2<-2*pnorm(-abs(VAL2)))
(VAL2<-p_val_iks(VAL2))
lower2=round(DIFF2-1.96*(ERR[,1]^2 + ERR[,3]^2)^(1/2),digits = 4)
upper2=round(DIFF2+1.96*(ERR[,1]^2 + ERR[,3]^2)^(1/2),digits = 4)

#### Both ###
All_T<-as.matrix(All_T)

NEW_AllT=rbind(c(rownames(All_T)[1],All_T[1,c(1,6,7,12,13,18)],DIFF1[1],VAL1[1],DIFF2[1],VAL2[1]),
               c(rownames(All_T)[1],All_T[1,c(3,4,9,10,15,16)],lower1[1],upper1[1],lower2[1],upper2[1]))

for(i in 2:dim(All_T)[1]){
  NEW_AllT2=rbind(c(rownames(All_T)[i],All_T[i,c(1,6,7,12,13,18)],DIFF1[i],VAL1[i],DIFF2[i],VAL2[i]),
                  c(rownames(All_T)[i],All_T[i,c(3,4,9,10,15,16)],lower1[i],upper1[i],lower2[i],upper2[i]))
  NEW_AllT<-rbind(NEW_AllT,NEW_AllT2)
}

View(NEW_AllT)

TABLE_Fr_BOTH=list(TABLE=NEW_AllT,
                   NAMES=c("FriendxInternet0","FriendxInternet1","FriendxFacebook",
                           "NoInternet:Internet","NoInternet:Facebook"))


#### Checking structure of the data ####

### By Bootstrapping instead of Survey Package

BOOTS2<-foreach(j=1:2500,.combine = rbind)%do%{
  SAMPLING_RATIOS(TABLE_FB,j)
}

apply(as.matrix(unique(paste0(BOOTS2$TYPE,BOOTS2$CATG))),
      MARGIN = 1,
      FUN = function(x){
        hist((BOOTS2%>%filter(paste0(TYPE,CATG)==x))$n,main = x)
        })

FB_TOT_RAT<-BOOTS2%>%
  ungroup()%>%
  mutate(Name=paste0(TYPE,CATG))%>%
  group_by(Name)%>%
  summarise(mu=mean(n),SDn=sd(n),
            mu_r=mean(ratio),SDr=sd(ratio))

#### Ratios ####

RATIOS<-left_join(SHARE_Ratios,FB_TOT_RAT[,c(1,4,5)])
RATIOS<-RATIOS[-c(1,13,14,17,18),]

#### Totals ####

TOTALS<-left_join(SHARE_Total[-c(1,17,18),],FB_TOT_RAT[,c(1,2,3)])
TOTALS<-TOTALS[-c(12,13),]

### Joining Tables ####

names(TOTALS)=paste0(names(TOTALS),"_TOT")
names(RATIOS)=paste0(names(RATIOS),"_RAT")

colnames(TOTALS)[1]
colnames(RATIOS)[1]="Name"

TOTALS<-TOTALS%>%dplyr::select(-Total_TOT,-Total_V_TOT)

NEW_AllT=rbind(round(c(TOTALS[1,2],100*RATIOS[1,2],
                TOTALS[1,4],100*RATIOS[1,4],
                TOTALS[1,6],100*RATIOS[1,6]),digits = 0),
              round(c(TOTALS[1,3],RATIOS[1,3],
                TOTALS[1,5],RATIOS[1,5],
                TOTALS[1,7],RATIOS[1,7]),digits = 10))

for(i in 2:dim(TOTALS)[1]){
  NEW_AllT2=rbind(round(c(TOTALS[i,2],100*RATIOS[i,2],
                          TOTALS[i,4],100*RATIOS[i,4],
                          TOTALS[i,6],100*RATIOS[i,6]),digits = 0),
                  round(c(TOTALS[i,3],RATIOS[i,3],
                          TOTALS[i,5],RATIOS[i,5],
                          TOTALS[i,7],RATIOS[i,7]),digits = 10))
  NEW_AllT<-rbind(NEW_AllT,NEW_AllT2)
}

NEW_AllT<-data.frame(NEW_AllT)

names(NEW_AllT)<-c(names(TOTALS)[2],names(RATIOS)[2],
                   names(TOTALS)[4],names(RATIOS)[4],
                   names(TOTALS)[6],names(RATIOS)[6])

NEW_AllT$names=rep(TOTALS$Name,each=2)

NEW_AllT<-NEW_AllT[,c(7,1:6)]

TABLE_TOT_RAT=NEW_AllT

#### Save Rdata Tables for Rmarkdown ####

# save(TABLE_TOT_RAT,TABLE_Fr_BOTH,
#      file = paste0(Save,"TablesDocument.RData"))

