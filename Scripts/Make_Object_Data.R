#### This file takes the entire raw data set and creates a model-ready subset for objects left 
#### in patients. This file sources code from Appendix Codes.R and Generic Clean Data Subset.R 
#### and holds additional exclusion criteria, specific for objects left in patients.

mistake<-"Object"

load("~/shared/Data/raw.data2")
#there should now be a huge data set in the global environment called model.data

###########################################################################################
source(file='~/shared/Scripts/Appendix Codes.R')
###########################################################################################

exclude<-rep(0,length=nrow(raw.data)) # make exclusion variable

# exclusion function for appendicies
exclude.test<-function(patient,appendix){
  any(is.element(appendix,patient))
}

# include only medical and surgical discharge
exclude<-ifelse(raw.data$drg %in% AppendixB | raw.data$drg %in% AppendixD | 
                  raw.data$msdrg %in% AppendixC | raw.data$msdrg %in% AppendixE,exclude,1)

#exclude patients with primary procedure as an object left in them
obj.codes<-c("9984","9987","E8710","E8711","E8712","E8713","E8714","E8715","E8716","E8717","E8718","E8719")
exclude<-ifelse(raw.data$diag_p %in% obj.codes,1,exclude)
exclude<-ifelse((raw.data$odiag1 %in% obj.codes)&raw.data$opoa1=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag2 %in% obj.codes)&raw.data$opoa2=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag3 %in% obj.codes)&raw.data$opoa3=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag4 %in% obj.codes)&raw.data$opoa4=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag5 %in% obj.codes)&raw.data$opoa5=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag6 %in% obj.codes)&raw.data$opoa6=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag7 %in% obj.codes)&raw.data$opoa7=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag8 %in% obj.codes)&raw.data$opoa8=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag9 %in% obj.codes)&raw.data$opoa9=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag10 %in% obj.codes)&raw.data$opoa10=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag11 %in% obj.codes)&raw.data$opoa11=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag12 %in% obj.codes)&raw.data$opoa12=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag13 %in% obj.codes)&raw.data$opoa13=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag14 %in% obj.codes)&raw.data$opoa14=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag15 %in% obj.codes)&raw.data$opoa15=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag16 %in% obj.codes)&raw.data$opoa16=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag17 %in% obj.codes)&raw.data$opoa17=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag18 %in% obj.codes)&raw.data$opoa18=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag19 %in% obj.codes)&raw.data$opoa19=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag20 %in% obj.codes)&raw.data$opoa20=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag21 %in% obj.codes)&raw.data$opoa21=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag22 %in% obj.codes)&raw.data$opoa22=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag23 %in% obj.codes)&raw.data$opoa23=="Y",1,exclude)
exclude<-ifelse((raw.data$odiag24 %in% obj.codes)&raw.data$opoa24=="Y",1,exclude)

rm(raw.data) #erase raw data to save memory
rm(diag.data)

save(exclude,file = paste('~/shared/Data/',mistake,'_Exclusion.obj',sep=""))
# there should now be a file called Object_Exclusion.obj in the ~shared/Data folder.

load("~/shared/Data/Full_Data/Clean_Data.obj")
model.data <- model.data[exclude==0,]
save(model.data,file="~/shared/Data/Object_Data.obj")
rm(model.data)