#### This file takes the subset of data corresponding to the denominator for bedsores 
#### and creates a model/EDA ready data set. This file sources a code from a 
#### generic file for data cleaning, and holds additional code, specific for bedsores.

mistake<-"Bedsore"

load("~/shared/Data/Full_Data/Raw_Data.obj")
#there should now be a huge data set in the global environment called model.data

###########################################################################################
source(file='~/shared/Scripts/Appendices/Appendix_Codes.R')
###########################################################################################

exclude<-rep(0,length=nrow(raw.data)) # make exclusion variable
exclude<-ifelse(raw.data$los<=5,1,exclude) # exclude length of stay less than 5

## Exclude MDC 9 / MDC 14



# exclusion function for appendicies
exclude.test<-function(patient,appendix){
  any(is.element(appendix,patient))
}

# include only medical and surgical discharge
exclude<-ifelse(raw.data$drg %in% AppendixB | raw.data$drg %in% AppendixD | 
                  raw.data$msdrg %in% AppendixC | raw.data$msdrg %in% AppendixE,exclude,1)

#exclude patients in appendix H, Iproc, Idiag
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixH),1,exclude)
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixIproc),1,exclude)
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixIdiag),1,exclude)

# exclude principle cases or secondary cases present on admission
bed.codes<-c("7070","70700","70701","70702","70703","70704","70705","70706","70707","70709")
exclude<-ifelse(raw.data$diag_p %in% bed.codes,1,exclude)
exclude<-ifelse((raw.data$odiag1 %in% bed.codes)&raw.data$opoa1=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag2 %in% bed.codes)&raw.data$opoa2=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag3 %in% bed.codes)&raw.data$opoa3=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag4 %in% bed.codes)&raw.data$opoa4=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag5 %in% bed.codes)&raw.data$opoa5=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag6 %in% bed.codes)&raw.data$opoa6=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag7 %in% bed.codes)&raw.data$opoa7=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag8 %in% bed.codes)&raw.data$opoa8=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag9 %in% bed.codes)&raw.data$opoa9=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag10 %in% bed.codes)&raw.data$opoa10=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag11 %in% bed.codes)&raw.data$opoa11=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag12 %in% bed.codes)&raw.data$opoa12=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag13 %in% bed.codes)&raw.data$opoa13=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag14 %in% bed.codes)&raw.data$opoa14=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag15 %in% bed.codes)&raw.data$opoa15=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag16 %in% bed.codes)&raw.data$opoa16=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag17 %in% bed.codes)&raw.data$opoa17=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag18 %in% bed.codes)&raw.data$opoa18=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag19 %in% bed.codes)&raw.data$opoa19=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag20 %in% bed.codes)&raw.data$opoa20=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag21 %in% bed.codes)&raw.data$opoa21=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag22 %in% bed.codes)&raw.data$opoa22=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag23 %in% bed.codes)&raw.data$opoa23=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag24 %in% bed.codes)&raw.data$opoa24=="Y",1,exclude)





# there should now be a file called Bedsore Exclusion in the ~shared/Data folder.