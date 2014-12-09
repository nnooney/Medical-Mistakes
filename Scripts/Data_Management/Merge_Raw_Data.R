## This file reads in data from 2003-2011, and combines it into a large data set.
## The variables "cat_code","epoa1","epoa2","epoa3","epoa4","epoa_p","grouper","sev_code" are excluded from 2008-2011
## The variable "msdrg" from 2008-2011 is combined with "drg" from 2003-2007 to create the variable "drg" in the final dataset.
## The final data set is stored as as an R Object: '~/shared/Data/Raw Data'.
## Two intermediate data sets are stored as '~/shared/Data/Raw Data 08 11' and '~/shared/Data/Raw Data 03 07'.
## This code takes a long time to run.

library(foreign)

year<-rep(as.character(2003:2011),each=3)
extension<-rep(c("_LA_base.dta","_North_base.dta","_South_base.dta"),times=9)
path<-"~/shared/Data/Original_Data/"
files<-paste(path,year,extension,sep="")

# 2003-2007 have the same column names, so we can rbind them
raw.data<-NULL
n<-1
for(name in files[1:15]){
  dta<-read.dta(name)
  dta$year<-rep(year[n],times=nrow(dta))
  raw.data<-rbind(raw.data,dta)
  n<-n+1
}
save(raw.data,file='~/shared/Data/Full_Data/Raw_Data_03_07.obj')
# This file is now saved as Raw Data 03 07

# Same for 2008-2011
raw.data2<-NULL
for(name in files[16:27]){
  dta<-read.dta(name)
  dta$year<-rep(year[n],times=nrow(dta))
  raw.data2<-rbind(raw.data2,dta)
  n<-n+1
}
save(raw.data2,file='~/shared/Data/Full_Data/Raw_Data_08_11.obj')

# now we will take the extra columns out of 2008-2011 and save them
extra2<-c("cat_code","epoa1","epoa2","epoa3","epoa4","epoa_p","grouper","msdrg","sev_code")
extra.data2<-raw.data2$msdrg # quick to run
raw.data2<-raw.data2[,!is.element(colnames(raw.data2),extra2)] # quick to run

# now we will take the extra columns out of 2003-2007 and save them
extra.data1<-raw.data$drg
raw.data<-raw.data[,colnames(raw.data)!="drg"]

# This data frame shows us that the only non-matching variables are the opoa# and the cpoa# variables.
# Since this is just a name swap, we can simply rbind them.
cbind(colnames(raw.data),colnames(raw.data2),colnames(raw.data)==colnames(raw.data2))

colnames(raw.data)<-colnames(raw.data2) # first we make the column names consistent
raw.data<-rbind(raw.data,raw.data2) # then we rbind them
raw.data$drg<-c(extra.data1,extra.data2) # add the drg variable back in

save(raw.data,file='~/shared/Data/Full_Data/Raw_Data.obj')
