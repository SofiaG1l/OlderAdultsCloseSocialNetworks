
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: Last update June 2019
# 
# Description: Code to retrieve the data used in the article:
#   "Gil-Clavel, S., Zagheni, E. & Bordone, V. Close Social Networks Among Older
#   Adults: The Online and Offline Perspectives. Popul Res Policy Rev (2021). 
#   https://doi.org/10.1007/s11113-021-09682-3"
# 
# Computer Environment:
#   - Windows 
#   - R - 3.5.1 (2019)
#   - Rstudio (1.1.453)
#   - Microsoft Windows Server 2012 R2 Standard
#
##################################

#### Open Packages ####

library(httr)
library(tidyverse)
library(jsonlite)
library(parallel)
library(combinat)

#### Environment Settings ####
Sys.setenv(LANG="EN")
options(scipen = 999)

#### Cleaning Working Environment ####
rm(list=ls())
gc()

#### Setting up Facebook API parameters ####
# More info on this here:
# https://github.com/SofiaG1l/Using_Facebook_API

TOKEN='<TOKEN>'
ACT='<ACT>'

# The Facebook API 2019 version was 3.2. 
Inicio=paste0('https://graph.facebook.com/v3.2/act_',ACT,'/delivery_estimate?access_token=',TOKEN,'&include_headers=false&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1')

#### Root Folder and Dir to Save ####
Direc<-"~/OlderAdultsCloseSocialNetworks/"
Save<-"DATA/"

#### Call R Function "StringGenerator.R" into environment ####

source(paste0(Direc,"CODES/Retrieving_FB_Data/StringGenerator.R"))

#### Open External Data ####

# Country Codes
code<-read.csv(paste0(Direc,'EXTERNAL_DB/CODES.csv'),stringsAsFactors = FALSE)
code[code$NAME=="Poland",]$CODE1="PL"

# Language Codes
lang<-read.csv(paste0(Direc,'EXTERNAL_DB/Country_Lng_Code.csv'),stringsAsFactors = FALSE)

# Facebook Variables obtained with Facebook API V.3.2 in 2019.
ids<-c("6015559470583","6002714398372","6019684757183")
name<-c("Expats (All)","Parents (All)","Close friends of people with birthdays in a month")
type<-c("behaviors","family_statuses","life_events")

# Data Frame
Demog<-data.frame(ids=ids,name=name,type=type)

# Keeping only the ones with which we are going to work
JustTwo<-rbind(Demog[which(Demog$name=='Expats (All)'),], # Expat is migrant
               Demog[which(Demog$name=='Parents (All)'),],
               Demog[which(Demog$name=='Close friends of people with birthdays in a month'),])

# Saving the number of variables used
ENE=nrow(JustTwo)
# Renaming Rows
row.names(JustTwo)<-1:ENE

#### Age Group ####
EDADG1<-c(50,65)
EDADG2<-c(64,65)

# Behavior list
## The total number of possible combinations where order does not matter
1+ncol(combn(ENE,2))+ncol(combn(ENE,1))+1 
# Checking code manually
combn(ENE,ENE) # All possible combinations of 3 in 3
combn(ENE,2) # All possible combinations of 2 in 3
combn(ENE,1) # All possible combinations of 1 in 3
NULL # No category

#### Creating lists with all possible variable combinations ####

# This list contains the INCLUDED variables
BH=list()
BH[[1]]=NULL
cont=2
for(i in ENE:1){
  Val=combn(ENE,i)
  if(is.null(ncol(Val))){
    BH[[cont]]=JustTwo[Val,]
    cont=cont+1
  }else{
    for(j in 1:ncol(Val)){
      BH[[cont]]=JustTwo[Val[,j],]
      cont=cont+1
    }
  }
}

# This list contains the EXCLUDED categories
# We need to pass this info to the API so that 
# the subsets are mutually exclusive
BHex=list()
CONJ<-paste(1:nrow(JustTwo))
NAMES<-lapply(BH,rownames)
j=1
for(v in NAMES){
  if(is.null(v)){
    BHex[[j]]<-JustTwo
    j=j+1
  }else{
    if(length(v)==nrow(JustTwo)){
      BHex[[j]]<-NULL
      j=j+1
    }else{
      complem<-setdiff(CONJ,v)
      BHex[[j]]=JustTwo[complem,]
      j=j+1
    }
  }
}


#### Education ####
##################################
# EDU LEVELS:
# CODE	NAME
# 1	HIGH_SCHOOL
# 2	UNDERGRAD
# 3	ALUM
# 4	HIGH_SCHOOL_GRAD
# 5	SOME_COLLEGE
# 7	IN_GRAD_SCHOOL
# 8	SOME_GRAD_SCHOOL
# 9	MASTER_DEGREE
# 10	PROFESSIONAL_DEGREE
# 11	DOCTORATE_DEGREE
# 12	UNSPECIFIED
# 13	SOME_HIGH_SCHOOL
##################################
EDU1<-'"education_statuses":["1","2","3","4","13"]' # Low than college degree
EDU2<-'"education_statuses":["5","6","7","8","9","10","11"]' # college or higher
EDU3<-'"education_statuses":["12"]' # UNSPECIFIED
EDU<-list(EDU1=EDU1,EDU2=EDU2,EDU3=EDU3)

#### Infinite Loop – it will stop once the Facebook Token Expires ####
# day
date=1
while(1){
  for(C in code$CODE1){
    
    # Data Frame with the variables
    DF=data.frame(CODE=rep(C,96)) # 2(SEX)*3(EDU)*2(AGE)*2(CATEG)^3 = 96
    DF$COUNTRY=code[code$CODE1==C,'NAME'][1]
    DF$REGION=code[code$CODE1==C,'REGION'][1]
    DF$SUBREGION=code[code$CODE1==C,'SUBREGION'][1]
    DF$SEX=0
    DF$EDUCATION=0
    DF$AGE=0
    DF$EXPAT=0
    DF$PARENTS=0
    DF$FRIENDS=0
    DF$estimate_dau=0
    DF$estimate_mau=0
    DF$estimate_ready=0
    
    i=1 # i is the row in DF
    
    # Retrieving DAU and MAU values of each combination
    for(k in 1:length(BH)){ # Behavior & Demog
      for(g in c(1,2)){ # gender
        for(j in 1:length(EDU)){ # education
          for (e in 1:length(EDADG1)) { # age-group
            
            # Creating the query to pass to the Facebook API
            INTEREST<-StringGenerator(BH=BH[[k]],BHex=BHex[[k]],EDU=EDU[[j]],EduNum=1)
            if(!is.null(INTEREST))
              test <- paste0(Inicio,'&include_headers=false&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={"age_max":',EDADG2[e],',',INTEREST,',"geo_locations":{"countries":["',C,'"],"location_types":["home"]},"facebook_positions":["feed","instant_article","instream_video","marketplace"],"genders":[',g,'],"age_min":',EDADG1[e],',"device_platforms":["mobile","desktop"],"publisher_platforms":["facebook","messenger"],"messenger_positions":["messenger_home"]}')
            else
              test <- paste0(Inicio,'&include_headers=false&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1&targeting_spec={"age_max":',EDADG2[e],',"geo_locations":{"countries":["',C,'"],"location_types":["home"]},"facebook_positions":["feed","instant_article","instream_video","marketplace"],"genders":[',g,'],"age_min":',EDADG1[e],',"device_platforms":["mobile","desktop"],"publisher_platforms":["facebook","messenger"],"messenger_positions":["messenger_home"]}')
            
            # Checking if the query returns an error
            message<-try(bla<-url(test)%>%fromJSON,silent = TRUE)
            
            # If there is an error then:
            while(class(message)=="try-error"|"error"%in%names(message)){
              # - Print the message
              print(message)
              # - Print the system time
              print(format(Sys.time(), "%X"))
              # - Wait for 15 minutes
              Sys.sleep(900)
              # - Try again
              message<-try(bla<-url(test)%>%fromJSON,silent = TRUE)
            }
            
            # Saving all the info in the Database 
            DF$SEX[i]=g
            DF$EDUCATION[i]=ifelse(names(EDU[j])=="EDU1",1,ifelse(names(EDU[j])=="EDU2",2,3))
            DF$AGE[i]=paste0(EDADG1[e],'-',EDADG2[e])
            DF$EXPAT[i]=ifelse(any(row.names(BH[[k]])==1),1,0)
            DF$PARENTS[i]=ifelse(any(row.names(BH[[k]])==2),1,0)
            DF$FRIENDS[i]=ifelse(any(row.names(BH[[k]])==3),1,0)
            DF$estimate_dau[i]=bla$data['estimate_dau'][[1]]
            DF$estimate_mau[i]=bla$data['estimate_mau'][[1]]
            DF$estimate_ready[i]=ifelse(bla$data['estimate_ready']=="TRUE","TRUE","FALSE")
            
            # Waiting 25 seconds before the next query
            Sys.sleep(25)
            
            i=i+1
          }
        }
      }
    }
    
    # Saving the system time 
    DF$TIME=format(Sys.time(), "%X")
    
    # Saving the system date
    DF$DATE=Sys.Date()
    
    # Saving the data into a "csv" file
    if(C==code$CODE1[1]){
      write.table(DF,paste0(Direc,Save,'/BASE_',date,'.csv'),
                row.names = FALSE, sep = ",", col.names = T)
    }else{
      write.table(DF,paste0(Direc,Save,'BASE_',date,'.csv'),
                row.names = FALSE, sep = ",", col.names = F, append = T)
    }
    
    # Deleting DF from memory
    rm(DF) 
    
    # Printing progress
    print(paste(date,code[code$CODE1==C,'NAME'][1],Sys.time()))
  }
  
  # Update date
  date=date+1
}




