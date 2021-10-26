
##################################
# 
# Author: Sofia Gil-Clavel
# 
# Date: Last update June 2019
# 
#   
# Description: Function to write Facebook API queries where categories
#   can be mutually exclusive. 
#   This function was written to retrieve the data of the article:
#   "Gil-Clavel, S., Zagheni, E. & Bordone, V. Close Social Networks Among Older
#   Adults: The Online and Offline Perspectives. Popul Res Policy Rev (2021). 
#   https://doi.org/10.1007/s11113-021-09682-3"
# 
# Parameters:
#   - BH: matrix of categories to include
#   - BHex: matrix of categories to exclude
#   - edu: string like the next one '"education_statuses":["1","2"]'
#   - EduNum: integer
# 
# Output:
#   - Facebook API query (string) or NULL
#
##################################


StringGenerator<-function(BH=NULL,BHex=NULL,EDU=NULL,EduNum=NULL){
  
  #### BehaviourS ####
  if(!is.null(BH)){
    
    #### Paste all ANDS ####
    if(nrow(BH)==1){
      Behaviours<-paste0('{"',BH$type,'":[{"id":"',BH$id,'","name":"',BH$name,'"}]}')
      ANDS<-paste0('"flexible_spec":[',Behaviours,']')
    }else{
      Behaviours=paste0('{"',BH$type[1],'":[{"id":"',BH$id[1],'","name":"',BH$name[1],'"}]}')
      for(i in 2:nrow(BH)){
        CADENA2=paste0('{"',BH$type[i],'":[{"id":"',BH$id[i],'","name":"',BH$name[i],'"}]}')
        Behaviours=paste0(Behaviours,',',CADENA2)
      }
      ANDS<-paste0('"flexible_spec":[',Behaviours,']')
    }
    
    #### Return in case NO EDU and NO EXCS ####
    if(is.null(EDU) & is.null(BHex))
      return(paste0('"flexible_spec":[',Behaviours,']'))
    else
      ANDS<-Behaviours
  }
  
  if(!is.null(BHex)){
    #### Paste all EXCLUDES ####
    if(nrow(BHex)==1){
      Behaviours<-paste0('{"',BHex$type,'":[{"id":"',BHex$id,'","name":"',BHex$name,'"}]}')
      EXCS<-paste0('exclusions:{',Behaviours,'}')
    }else{
      Behaviours=paste0('{"',BHex$type[1],'":[{"id":"',BHex$id[1],'","name":"',BHex$name[1],'"}]')
      for(i in 2:nrow(BHex)){
        CADENA2=paste0('"',BHex$type[i],'":[{"id":"',BHex$id[i],'","name":"',BHex$name[i],'"}]')
        Behaviours=paste0(Behaviours,',',CADENA2)
      }
      Behaviours=paste0(Behaviours,'}')
    }
    
    #### Return in case NO EDU and NO EXCS ####
    if(is.null(EDU) & is.null(BH))
      return(paste0('exclusions:',Behaviours))
    else
      EXCS<-Behaviours
    
  }
  
  #### Education Section ####
  # EduNum={AND=1; OR=2; EXCLUDE=3}
  
  if(is.null(EDU)){# Return in case NO EDU 
    
    return(paste0('exclusions:',EXCS,',"flexible_spec":[',ANDS,']'))
    
  }else{
    
    if(!is.null(BH) & is.null(BHex)){
      return(paste0('"flexible_spec":[{',EDU,'},',ANDS,']'))
    }
    
    if(!is.null(BHex) & is.null(BH)){
      return(paste0('exclusions:',EXCS,',"flexible_spec":[{',EDU,'}]'))
    }
    
    if(!is.null(BHex) & !is.null(BH)){
      return(paste0('exclusions:',EXCS,',"flexible_spec":[{',EDU,'},',ANDS,']'))
    }
    
  }
  
  return(NULL)
  
}

