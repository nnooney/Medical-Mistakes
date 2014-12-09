#### This script cleans the Raw Data object and creates a model-ready version to be subsetted.
#### The following variables are created:
######### Hospital ID
######### Mistake Indicator/Factor
######### Charleson Score
######### Expected Number of Medical Specialists
######### Sex, Agecat, Ethnicity, Race
######### Pay Category, Pay Type, Length of Stay, Type of Care, and Admissions Type
#### The file is saved as Clean Raw Data in the data folder.

load("~/shared/Data/Full_Data/Raw_Data.obj")

#Hospital ID
oshpd<-raw.data$oshpd_id

################################################# Create mistake indicator
# First Define Diagnosis Columns
diag.cols<-c("diag_p","odiag1","odiag2","odiag3","odiag4","odiag5","odiag6","odiag7","odiag8","odiag9",
             "odiag10","odiag11","odiag12","odiag13","odiag14","odiag15","odiag16","odiag17","odiag18",
             "odiag19","odiag20","odiag21","odiag22","odiag23","odiag24")

# Subset data to include only diagnosis columns
test<-raw.data[,diag.cols]

# Define mistake codes:
obj.codes<-c("9984","9987","E8710","E8711","E8712","E8713","E8714","E8715","E8716","E8717","E8718","E8719")
cath.codes<-c("99662","9993","99931","99932")
bed.codes<-c("7070","70700","70701","70702","70703","70704","70705","70706","70707","70709")

mistake.test<-function(patient,codes){
  any(is.element(codes,patient))
}

# We apply our functions from above to our data to create indicators for medical mistakes.
obj.mistake<-apply(test,1,mistake.test,codes=obj.codes)
cath.mistake<-apply(test,1,mistake.test,codes=cath.codes)
bed.mistake<-apply(test,1,mistake.test,codes=bed.codes)

# Making Factors of these Variables
cath.mistake.indicator<-as.factor(cath.mistake)
levels(cath.mistake.indicator)<-c("Infection","No Infection")

obj.mistake.indicator<-as.factor(obj.mistake)
levels(obj.mistake.indicator)<-c("Object","No Object")

bed.mistake.indicator<-as.factor(bed.mistake)
levels(bed.mistake.indicator)<-c("Bedsore","No Bedsore")

############################################################ Create Charleson Scores
# this function reformats our diagnosis data
make5<-function(alex){
  alex<-as.character(alex)
  alex2<-sapply(alex,nchar)
  alex<-ifelse(alex2==3,paste(alex,'00',sep=""),alex)
  alex<-ifelse(alex2==4,paste(alex,'0',sep=""),alex)
  alex<-ifelse(alex2==0,paste('00000',alex,sep=""),alex)
  alex<-paste(substr(alex,1,3),".",substr(alex,4,5),sep="")
  alex[alex=="V4340"]<-"443.90"
  alex
} # makes icd9 codes into form "999.99"
make5b<-function(alex){
  alex2<-sapply(alex,nchar)
  alex<-ifelse(alex2==3,paste(alex,'.00',sep=""),alex)
  alex<-ifelse(alex2==5,paste(alex,'0',sep=""),alex)
  alex
} # same thing, but for vector of relevant icd9 codes
diag.data<-apply(test,2,make5) #this is the data that we will use to create the charleson scores

### re-defining charleson score function
# this function will count the number of relevant diagnoses for each charleson score category
make.count<-function(set,codes){
  charl.test<-function(patient) {patient %in% codes}
  colSums(apply(set,1,charl.test))
}

# creating vectors of strings for diagnoses to include in the charleson score
mi <- make5b(paste(c(seq(from = 410, to = 410.9, by = 0.01), 412)))
chf <- make5b(paste(seq(from = 428, to = 428.9, by = 0.01)))
pvd <- make5b(paste(c(443.9, 441, 441.9, 785.4)))
cvd <- make5b(seq(from = 430, to = 438, by = 0.01))
dementia<- make5b(paste(seq(from = 290, to = 290.9, by = 0.01)))
copd<- make5b(paste(c(seq(from = 490, to = 496, by = 0.01), seq(from = 500,to = 505, by = 0.01), 506.4)))
rheum <- make5b(paste(c(710, 710.1, 710.4, seq(from = 714, to = 714.2,by = 0.01), 714.81, 725)))
pud<- make5b(paste(seq(from = 531, to = 534.9, by = 0.01)))
mild.liver<- make5b(paste(c(571.2, 571.5, 571.6, seq(from = 571.4,to = 571.49, by = 0.01))))
dm <- make5b(paste(c(seq(from = 250, to = 250.3, by = 0.01), 250.7)))
dm.comp <- make5b(paste(seq(from = 250.4, to = 250.6, by = 0.01)))
plegia <- make5b(paste(c(344.1, seq(from = 342, to = 342.9, by = 0.01))))
renal<- make5b(paste(c(seq(from = 582, to = 582.9, by = 0.01), seq(from = 583,to = 583.7, by = 0.01), 585, 586, seq(from = 588,to = 588.9, by = 0.01))))
malignancy<- make5b(paste(c(seq(from = 140, to = 172.9, by = 0.01),seq(from = 174, to = 195.8, by = 0.01), seq(from = 200,to = 208.9, by = 0.01))))
severe.liver<- make5b(paste(c(seq(from = 572.2, to = 572.8, by = 0.01),seq(from = 456, to = 456.21, by = 0.01))))
mets<- make5b(paste(seq(from = 196, to = 199.1, by = 0.01)))
hiv<- make5b(paste(seq(from = 42, to = 44.93, by = 0.01)))

# making counts for each relevant charleson category
mi.cnt<-make.count(diag.data,mi) #
chf.cnt<-make.count(diag.data,chf) # 
pvd.cnt<-make.count(diag.data,pvd) #
cvd.cnt<-make.count(diag.data,cvd) #
dementia.cnt<-make.count(diag.data,dementia) #
copd.cnt<-make.count(diag.data,copd) #
rheum.cnt<-make.count(diag.data,rheum) #
pud.cnt<-make.count(diag.data,pud) #
mild.liver.cnt<-make.count(diag.data,mild.liver) #
dm.cnt<-make.count(diag.data,dm) #
dm.comp.cnt<-make.count(diag.data,dm.comp) #
plegia.cnt<-make.count(diag.data,plegia) #
renal.cnt<-make.count(diag.data,renal) #
malignancy.cnt<-make.count(diag.data,malignancy) #
severe.liver.cnt<-make.count(diag.data,severe.liver) #
mets.cnt<-make.count(diag.data,mets) #
hiv.cnt<-make.count(diag.data,hiv) #

#create the charleson scores!!! (later)
  
############################################################# Create Expected Number of Specialists
# Read in chronic indicators file
chronic.ind<-read.csv(file='~/shared/Scripts/Appendices/Chronic_Category_Index_No_Header.csv', header=F, stringsAsFactors=FALSE)

# Sort out codes for each chronic category
heart.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==7,1]
endo.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==3,1]
pulm.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==8,1]
ment.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==5,1]
cancer.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==2,1]
blood.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==4,1]
preg.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==11,1]
nerv.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==6,1]
cong.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==14,1]
natal.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==15,1]
other.codes<-chronic.ind[chronic.ind[,3]==1 & chronic.ind[,4]==18,1]

### Function that indicates conditions
chronic.test<-function(patient,codes) any(patient %in% codes)

# Makes chronic indicators for specialist categories
heart.ind<-apply(test,1,chronic.test,codes=heart.codes)
pulm.ind<-apply(test,1,chronic.test,codes=pulm.codes)
ment.ind<-apply(test,1,chronic.test,codes=ment.codes)
endo.ind<-apply(test,1,chronic.test,codes=endo.codes)
cancer.ind<-apply(test,1,chronic.test,codes=cancer.codes)
blood.ind<-apply(test,1,chronic.test,codes=blood.codes)
preg.ind<-apply(test,1,chronic.test,codes=preg.codes)
nerv.ind<-apply(test,1,chronic.test,codes=nerv.codes)
cong.ind<-apply(test,1,chronic.test,codes=cong.codes)
natal.ind<-apply(test,1,chronic.test,codes=natal.codes)
other.ind<-apply(test,1,chronic.test,codes=other.codes)

# Expected number of medical specialists
exp.specialists <- heart.ind + pulm.ind + endo.ind + ment.ind + 
  cancer.ind + blood.ind + preg.ind + nerv.ind + cong.ind + 
  natal.ind + other.ind

################################################################# Make Demographic Variables
# first make factors for all of the demographics
sex1<-as.factor(raw.data$sex)
levels(sex1)<-c('Censored','Male',"Female","Other","Unknown")
race1<-as.factor(raw.data$race)
levels(race1)<-c('Censored','White',"Black","Nat.American","Asian","Other","Unknown")
eth1<-as.factor(raw.data$ethncty)
levels(eth1)<-c('Censored',"Hispanic","Non-Hispanic","Unknown")
age1<-as.factor(raw.data$agecat20)
levels(age1)<-c("Censored","Unknown","Under 1","1-4 yrs","5-9 yrs","10-14 yrs",
                "15-19 yrs","20-24 yrs","25-29 yrs","30-34 yrs", "35-39 yrs",
                "40-44 yrs", "45-49 yrs", "50-54 yrs", "55-59 yrs","60-64 yrs",
                "65-69 yrs", "70-74 yrs", "75-79 yrs","80-84 yrs","85+ yrs")

# female is baseline
#male<-ifelse(sex1=='Male',1,0)
#sex.miss<-ifelse(sex1=='Censored'|sex1=='Other'|sex1=='Unknown',1,0)

# non-hispanic is baseline
#hispanic<-ifelse(eth1=='Hispanic',1,0)
#hispanic.miss<-ifelse(eth1=='Censored'|eth1=='Unknown',1,0)

# white is baseline
#black<-ifelse(race1=='Black',1,0)
#nat.amer<-ifelse(race1=='Nat.American',1,0)
#asian.pi<-ifelse(race1=='Asian',1,0)
#other.race<-ifelse(race1=='Other',1,0)
#race.miss<-ifelse(race1=='Unknown'|race1=='Censored',1,0)

# no baseline
#agecat1<-ifelse(raw.data$agecat20=="01",1,0)
#agecat2<-ifelse(raw.data$agecat20=="02",1,0)
#agecat3<-ifelse(raw.data$agecat20=="03",1,0)
#agecat4<-ifelse(raw.data$agecat20=="04",1,0)
#agecat5<-ifelse(raw.data$agecat20=="05",1,0)
#agecat6<-ifelse(raw.data$agecat20=="06",1,0)
#agecat7<-ifelse(raw.data$agecat20=="07",1,0)
#agecat8<-ifelse(raw.data$agecat20=="08",1,0)
#agecat9<-ifelse(raw.data$agecat20=="09",1,0)
agecat10<-ifelse(raw.data$agecat20=="10",1,0)
agecat11<-ifelse(raw.data$agecat20=="11",1,0)
agecat12<-ifelse(raw.data$agecat20=="12",1,0)
agecat13<-ifelse(raw.data$agecat20=="13",1,0)
agecat14<-ifelse(raw.data$agecat20=="14",1,0)
agecat15<-ifelse(raw.data$agecat20=="15",1,0)
agecat16<-ifelse(raw.data$agecat20=="16",1,0)
agecat17<-ifelse(raw.data$agecat20=="17",1,0)
agecat18<-ifelse(raw.data$agecat20=="18",1,0)
agecat19<-ifelse(raw.data$agecat20=="19",1,0)
#agecat.unknown<-ifelse(raw.data$agecat20=="00"|raw.data$agecat20=="Censored",1,0)

#create the charleson scores!!!
charleson<-mi.cnt + chf.cnt + pvd.cnt + cvd.cnt + dementia.cnt + copd.cnt + rheum.cnt + pud.cnt + mild.liver.cnt + dm.cnt + 
  2 * (dm.comp.cnt + plegia.cnt + malignancy.cnt + renal.cnt) +
  3 * (severe.liver.cnt) +
  6 * (hiv.cnt + mets.cnt) +
  agecat10 + agecat11 +
  2 * (agecat12 + agecat13) +
  3 * (agecat14 + agecat15) +
  4 * (agecat16 + agecat17 + agecat18 + agecat19)

################################################################## Other Hospital Variables
typ_care1<-as.factor(raw.data$typ_care)
levels(typ_care1)<-c("Blank","Acute","Skilled Nursing","Psych Care", "Chem Recovery", "Phys Rehab")
adm_typ1<-as.factor(raw.data$adm_typ)
levels(adm_typ1)<-c("Blank","Scheduled","Unscheduled","Infant","Unknown")
los<-raw.data$los
pay_cat1<-as.factor(raw.data$pay_cat)
levels(pay_cat1)<-c("Blank","Medicare","Medi-Cal","Private","Workers","CIP","Other Gov","Other Indigent",
                               "Self Pay","Other Payer")
pay_type1<-as.factor(raw.data$pay_type)
levels(pay_type1)<-c("NA","Managed Care MCOHS","Managed Care Other","Traditional Coverage")

# acute/blank is baseline
#skill.nurse<-ifelse(typ_care1=="Skilled Nursing",1,0)
#phys.re<-ifelse(typ_care1=="Phys Rehab",1,0)
#psych.care<-ifelse(typ_care1=="Psych Care",1,0)
#chem.re<-ifelse(typ_care1=="Chem Recovery",1,0)

# baseline unscheduled/blank/unknown
#scheduled<-ifelse(adm_typ1=="Scheduled",1,0)
#infant<-ifelse(adm_typ1=="Infant",1,0)

# baseline is private
#medicare<-ifelse(pay_cat1=="Medicare",1,0)
#medi.cal<-ifelse(pay_cat1=="Medi-Cal",1,0)
#workers<-ifelse(pay_cat1=="Workers",1,0)
#cip<-ifelse(pay_cat1=="CIP",1,0)
#other.gov<-ifelse(pay_cat1=="Other Gov",1,0)
#other.ind<-ifelse(pay_cat1=="Other Indigent",1,0)
#self.pay<-ifelse(pay_cat1=="Self Pay",1,0)
#other.payer<-ifelse(pay_cat1=="Other Payer",1,0)

# baseline is managed care
#pay.other<-ifelse(pay_type1=="Managed Care Other",1,0)
#pay.traditional<-ifelse(pay_type1=="Traditional Coverage",1,0)
#pay.na<-ifelse(pay_type1=="NA",1,0)

################################################################### Save File
model.data<-data.frame(oshpd, charleson, exp.specialists, sex1, race1, eth1, age1,
                       obj.mistake, cath.mistake, bed.mistake,
                       typ_care1, adm_typ1, los, pay_cat1, pay_type1)
save(model.data,file = '~/shared/Data/Full_Data/Clean_Data.obj')
rm(list=ls())
