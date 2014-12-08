#### This file takes the entire raw data set and creates a model-ready subset for catheter 
#### infections. This file sources code from Appendix Codes.R and Generic Clean Data Subset.R 
#### and holds additional exclusion criteria, specific for catheter infections.

mistake<-"Catheter Infection"

load("~/shared/Data/Raw Data")
#there should now be a huge data set in the global environment called raw.data
  
###########################################################################################
source(file='~/shared/Scripts/Appendix Codes.R')
###########################################################################################

exclude<-rep(0,length=nrow(raw.data)) # make exclusion variable
exclude<-ifelse(raw.data$los<2,1,exclude) # exclude length of stay

# exclusion function for appendicies
exclude.test<-function(patient,appendix){
  any(is.element(appendix,patient))
}
#exclude patients in appendix H, Iproc, Idiag
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixH),1,exclude)
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixIproc),1,exclude)
exclude<-ifelse(apply(diag.data,1,exclude.test,appendix=appendixIdiag),1,exclude)

# include only medical and surgical discharge
exclude<-ifelse(raw.data$drg %in% AppendixB | raw.data$drg %in% AppendixD,exclude,1)
exclude<-ifelse(raw.data$msdrg %in% AppendixC | raw.data$drg %in% AppendixE,exclude,1)

#exclude patients with primary procedure as a catheter infection
cath.codes<-c("99662","9993","99931","99932")
exclude<-ifelse(raw.data$diag_p %in% cath.codes,1,exclude)
exclude<-ifelse((raw.data$0diag1 %in% cath.codes)&opoa1=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag2 %in% cath.codes)&opoa2=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag3 %in% cath.codes)&opoa3=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag4 %in% cath.codes)&opoa4=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag5 %in% cath.codes)&opoa5=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag6 %in% cath.codes)&opoa6=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag7 %in% cath.codes)&opoa7=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag8 %in% cath.codes)&opoa8=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag9 %in% cath.codes)&opoa9=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag10 %in% cath.codes)&opoa10=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag11 %in% cath.codes)&opoa11=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag12 %in% cath.codes)&opoa12=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag13 %in% cath.codes)&opoa13=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag14 %in% cath.codes)&opoa14=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag15 %in% cath.codes)&opoa15=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag16 %in% cath.codes)&opoa16=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag17 %in% cath.codes)&opoa17=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag18 %in% cath.codes)&opoa18=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag19 %in% cath.codes)&opoa19=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag20 %in% cath.codes)&opoa20=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag21 %in% cath.codes)&opoa21=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag22 %in% cath.codes)&opoa22=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag23 %in% cath.codes)&opoa23=="Y",1,exclude)
exclude<-ifelse((raw.data$0diag24 %in% cath.codes)&opoa24=="Y",1,exclude)

data.subset<-raw.data[exclude==0,] # create data subset
rm(raw.data) #erase raw data to save memory

save(data.subset,paste('~/shared/Data/'mistake,' Data Subset',sep=""))

# Tests patients for catheter infections
mistake.test<-function(patient){
  codes<-c("99662","9993","99931","99932") # catheter infection codes
  any(is.element(codes,patient))
}

###########################################################################################
source(file='~/shared/Scripts/Generic Clean Data Subset.R')
###########################################################################################
# there should now be a file called Catheter Infection Model Data in the ~shared/Data folder.