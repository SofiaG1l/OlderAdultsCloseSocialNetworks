
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: Last update May 2021
# 
# Description: Functions to process the data, do the analyses and create the 
#   graphs and tables used in the article:
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

#### Functions to create tables and graphs
# For Logit Models
CI_logit_svy<-function(fitMult,AGE,VAROR_DF){
  
  SUM<-summary(fitMult)
  
  Coef<-coef(fitMult)
  
  # Confidence Interval
  conf<-exp(confint(fitMult))
  CI1<-data.frame(lower=round(conf[,1],4),
                  mean=round(exp(Coef),4),
                  upper=round(conf[,2],4))
  
  # SE
  CI1$SE<-SUM$coefficients[,2]
  
  # p-values
  p <- SUM$coefficients[,4]
  CI1$`p-value`=round(p,digits = 4)

  # Names
  CI1$NAMES=factor(VARnames,levels = VARnames[length(VARnames):1])
  CI1$AGE<-"BOTH"
  
  CI1$`p-value<`<-p_val_iks(CI1$`p-value`)
  
  return(CI1)
}


CI_DB_logit2<-function(fitMult,AGE,VAROR_DF){
  
  Coef<-coef(fitMult)
  SUM<-summary(fitMult)
  # Confidence Interval
  # CONFIDENCE INTERVAL: β_k ± z^{α/2}SE(βk) ; α=0.05
  CI<-confint(fitMult)
  CI1<-data.frame(lower=round(CI[,1],4),
                  mean=round(Coef,4),
                  upper=round(CI[,2],4))
  # p-values
  p <- coef(summary(fitMult))[,4]
  CI1$`p-value`=round(p,digits = 4)
  CI1$err<-round(SUM$coefficients[,2],4)
  CI1$NAMES=as.factor(VAROR_DF[names(coefficients(fitMult)),])
  CI1$AGE<-AGE
  
  return(CI1)
}

# p-values codes
p_val_iks<-function(pe){
  c("***","**","*","."," ")[findInterval(pe, sort(c(1, 0.1, 0.05, 0.01, 0.001, 0))) ]
}

#### Bootstrapping ####

# Bootstrap Procedure for the generalized linear model
SAMPLING_GLM<-function(DATA,j) {
  samp1 <- DATA %>%
    group_by(CODE,SEX,EDUCATION,AGE,PARENT,MIGRANT,FRIENDS)%>% #,DATE
    sample_n(size = 1,TRUE)
  
  fitSAMP1 <- glm(FRIENDS~SEX+AGE+PARENT+MIGRANT,
                  data = samp1,weights = estimate_dau,family = binomial())
  
  ALL<-data.frame(NAMES=names(coef(fitSAMP1)),VALUES=coef(fitSAMP1))
  
  ALL$ITER=j
  
  return(ALL)
}

# Bootstrap Procedure for percentages and totals
SAMPLING_RATIOS<-function(DATA,j) {
  # 17*2*3*2*2^4 = 3264 (Sampling categories by Date)
  samp1 <- DATA %>%
    group_by(CODE,SEX,EDUCATION,AGE,
             MIGRANT,PARENT,FRIENDS)%>%
    sample_n(size = 1,TRUE)%>%
    summarise(DATE=DATE,estimate_dau=mean(estimate_dau),.groups="keep")
  # dim(samp1);samp1[1,]
  
  N=sum(samp1$estimate_dau)
  
  SEX=samp1%>%
    group_by(SEX)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="SEX")
  colnames(SEX)[1]<-"CATG"
  
  AGE=samp1%>%
    group_by(AGE)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="AGE")
  colnames(AGE)[1]<-"CATG"
  
  EDUCATION=samp1%>%
    group_by(EDUCATION)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="EDUCATION")
  colnames(EDUCATION)[1]<-"CATG"
  
  PARENT=samp1%>%
    group_by(PARENT)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="PARENT")
  colnames(PARENT)[1]<-"CATG"
  
  FRIENDS=samp1%>%
    group_by(FRIENDS)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="FRIENDS")
  colnames(FRIENDS)[1]<-"CATG"
  
  MIGRANT=samp1%>%
    group_by(MIGRANT)%>%
    summarise(n=sum(estimate_dau))%>%
    mutate(ratio=(n/N),TYPE="MIGRANT")
  colnames(MIGRANT)[1]<-"CATG"
  
  ALL<-rbind(SEX,AGE,EDUCATION,PARENT,FRIENDS,MIGRANT)
  
  ALL$ITER=j
  
  return(ALL)
}

#### External Functions ####

# This function comes from:
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
