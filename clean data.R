source("load data.R")

data %<>%  data.table()


## CLeaning Anticoag colum and creating a new column to seperate noacs categoircally
data %<>% rename(Anticoagulant=`Oral Anticoagulant`,AdmissionHb=`Heamoglbin`)
table(data$Anticoagulant)
data$Antiplatelet <- "Other"
data$Antiplatelet[grep("Aspirin",data$Anticoagulant)] <- "Aspirin"
data$Antiplatelet[grep("Clopidogril",data$Anticoagulant)] <- "Clopidogrel"
data$Antiplatelet[grep("Clopidogrel",data$Anticoagulant)] <- "Clopidogrel"
data$Antiplatelet[grep("Aspirin \\+ Clopidogrel",data$Anticoagulant)] <- "Dual Antiplatelet"
data$Antiplatelet %<>% as_factor()

#creating no NOAC columns
data %<>% mutate(NOAC="Yes") %>% select(1,2,NOAC,3:10)
data$NOAC[data$Anticoagulant=='Nil'] <- 'No'
data$NOAC[which(str_detect(data$Anticoagulant,"an"))] <- "NOAC"
data$NOAC[which(str_detect(data$Anticoagulant,"Eliquis"))] <- "NOAC"
data$NOAC[which(str_detect(data$Anticoagulant,"Nil"))] <- "No Anticoagulant Prescribed"
data$NOAC[which(str_detect(data$Anticoagulant,"nil"))] <- "No Anticoagulant Prescribed"
data$NOAC[data$Anticoagulant=="Warfarin"] <- "Warfarin"
data$NOAC[data$NOAC=="Yes"] <- "Antiplatelets"

data$NOAC %<>%  as_factor()
data$Anticoagulant %<>% as_factor()
levels(data$NOAC) -> noaclevels

# Fixing Wrong entry where indication and anticoagulant was mixed up
data$NOAC[data$Anticoagulant=="AAA"] <- "Antiplatelets"

## indication
data$Indication[is.na(data$Indication)] <- "Not Indicated"
data$Indication[data$Indication=='Nil'] <- "Not Indicated"
data$Indication[which(str_detect(data$Indication,'No cause'))] <- "No Indication in Records"





# Fixing Wrong entry where indication and anticoagulant was mixed up
data$Indication[data$Indication=="Aspirin"] <-  "AAA"
#Wrong Entry
data$Indication[data$Indication=="SAH"] <- "No Indication in Records"
data$Indication[data$Indication=="Not Indicated"&data$Anticoagulant=="Warfarin"] <-"No Indication in Records"


data$Indication[grep(pattern = "mitral regurg",x=data$Indication,ignore.case = T)] <-  "Mitral Regurgitation"

#IndicationGroup Column created
data$IndicationGroup <- data$Indication
data %<>% select(1:3,Indication,IndicationGroup,5:11)


data$IndicationGroup[grep(pattern = "valve rep",x=data$Indication,ignore.case = T)] <-  "Valve Replacement"

data$IndicationGroup[grep(pattern="Stenosis",x=data$IndicationGroup,ignore.case = T)] <-  "Valvular Disorder"
data$IndicationGroup[grep(pattern="Regurg",x=data$IndicationGroup,ignore.case = T)] <-  "Valvular Disorder"


data$IndicationGroup[grep(pattern="MI",x=data$IndicationGroup,ignore.case = T)] <-  "Ischemic Heart Disease"
data$IndicationGroup[grep(pattern="IHD",x=data$IndicationGroup,ignore.case = T)] <-  "Ischemic Heart Disease"
data$IndicationGroup[grep(pattern="Angina",x=data$IndicationGroup,ignore.case = T)] <-  "Ischemic Heart Disease"

data$IndicationGroup[grep(pattern="AF",x=data$IndicationGroup,ignore.case = T)] <-  "Atrial Fibrilation"
data$IndicationGroup[grep(pattern="Atrial fibrilation",x=data$IndicationGroup,ignore.case = T)] <-  "Atrial Fibrilation"
data$IndicationGroup[grep(pattern="Atrial Flutter",x=data$IndicationGroup,ignore.case = T)] <-  "Atrial Fibrilation"

#Wrong entry fixed by examining records of other entries
data$IndicationGroup[grep(pattern="`",x=data$IndicationGroup,ignore.case = T)] <-  "Not Indicated"





## Admission Hb exploration

data %>%
         ggplot(aes(x=AdmissionHb)) +geom_freqpoly()

data %>%
         ggplot(aes(x=AdmissionHb,
                    fill=fct_collapse(NOAC,
                                      Noanticoagulant=noaclevels[1],
                                      Anticogulated=noaclevels[2:4]))) +
        geom_density(alpha=0.7)

data %>%
         ggplot(aes(x=AdmissionHb,
                    fill=fct_collapse(NOAC,Noanticoagulant=noaclevels[1],
                                      Anticogulated=noaclevels[3:4],
                                      Antiplatelets=noaclevels[2]))) +
        geom_density()
data %>%
         ggplot(aes(x=AdmissionHb,
                    fill=fct_collapse(NOAC,`No Anticoagulant`=noaclevels[1:2],
                                      Warfarin=noaclevels[4],
                                      NOAC=noaclevels[3]))) +
        geom_density(alpha=0.5) +
        facet_wrap(~Gender)+
        labs(fill="Anticoagulation Status")
data %>%
         ggplot(aes(x=AdmissionHb,
                    fill=fct_collapse(NOAC,`No Anticoagulant`=noaclevels[1:2],
                                      Warfarin=noaclevels[4],
                                      NOAC=noaclevels[3]))) +
        geom_density(alpha=0.5) +
        facet_wrap(~IndicationGroup)+
        labs(fill="Anticoagulation Status")



Hbmeans<- data %>% group_by(NOAC) %>% summarise(AdmissionHb= mean(AdmissionHb,na.rm = T),LOS=mean(LOS))
Hbmedians<- data %>% group_by(NOAC) %>% summarise(AdmissionHb= median(AdmissionHb,na.rm = T),LOS=median(LOS))


fitdistrplus::descdist(data$AdmissionHb[!is.na(data$AdmissionHb)])
qplot(data=data,x=AdmissionHb,geom="density")
summary(data$AdmissionHb)






## Gender
data$Gender %<>%  as_factor()
qplot(Gender,AdmissionHb,fill=Gender,geom = "boxplot",data=data)


wilcox.test(data$AdmissionHb~data$Gender)
data %>% group_by(Gender) %>% summarize(median(LOS),median(AdmissionHb,na.rm = T))
data %>% ggplot(aes(AdmissionHb,fill=Gender))+
        geom_density(alpha=0.7)

# Diagnosis
data$Diagnosis[grepl("Heam",data$Diagnosis,ignore.case = T)] <-  "Haemorrhoids"
data$Diagnosis[grepl("Haem",data$Diagnosis,ignore.case = T)] <-  "Haemorrhoids"
data$Diagnosis[grepl("Diverticulosis",data$Diagnosis,ignore.case = T)] <- " Diverticulosis"
data$Diagnosis[grepl("itis",data$Diagnosis,ignore.case = T)] <- "Colitis"
data$Diagnosis[grepl("Confirmed",data$Diagnosis,ignore.case = T)] <- "NAD"


##Diangnosis grouped
data$DiagnosisGrouped   <- data$Diagnosis
data %<>% dplyr::select(1:10,12,11,13)

data$DiagnosisGrouped[grepl("Rectal",data$DiagnosisGrouped,ignore.case = T)] <-  "Rectal Mass"
data$DiagnosisGrouped[grepl("Rectal",data$DiagnosisGrouped,ignore.case = T)] <-  "Rectal Mass"

data$DiagnosisGrouped %<>% as_factor()


qplot(DiagnosisGrouped,AdmissionHb,data=data,geom=c("boxplot"))

# LOS
subset(data,subset = LOS<20) %>% qplot(y=LOS,x=NOAC,data=.,geom="boxplot")

data %>% ggplot(aes(x=AdmissionHb,y=LOS))+geom_point()+geom_smooth()+ facet_wrap(~Gender)
fitdistrplus::descdist(subset(data$AdmissionHb,data$DiagnosisGrouped!="Anal Fissure"&!is.na(data$AdmissionHb)))

summary(data$LOS)



#Age
summary(data$Age)
fitdistrplus::descdist(subset(data$Age,!is.na(data$Age)))
fitdistrplus::descdist(subset(data$Age,!is.na(data$Age)&data$DiagnosisGrouped!="Anal Fissure"))
qplot(data=data, x=Age,geom="bar",facets = "DiagnosisGrouped")


# invesitgation
data$Investigations1ry <- NA
data$Investigations2nd <- NA
data$Investigations1ry <- str_split(string = data$Investigations,pattern = "[+]",n=2,simplify = T)[,1]
data$Investigations2nd <- str_split(string = data$Investigations,pattern = "[+]",n=2,simplify = T)[,2]

data$Investigations1ry %>% table
data$Investigations1ry[grep("scop",x = data$Investigations1ry,ignore.case = T)] <- "LGI Endoscopy"
data$Investigations1ry[grep("sop",x = data$Investigations1ry,ignore.case = T)] <- "LGI Endoscopy"
data$Investigations1ry[grep("cop",x = data$Investigations1ry,ignore.case = T)] <- "LGI Endoscopy"
data$Investigations1ry[grep("flex",x = data$Investigations1ry,ignore.case = T)] <- "LGI Endoscopy"
data$Investigations1ry[grep("nil",x = data$Investigations1ry,ignore.case = T)] <- "Nil"
data$Investigations1ry[grep("devel",x = data$Investigations1ry,ignore.case = T)] <- "Nil"


data$Investigations2nd %>% table
data$Investigations2nd[grep("scop",x=data$Investigations2nd)] <- "CT Colonoscopy"
data$Investigations2nd[-grep("CT",x=data$Investigations2nd)] <- "Primary Only"


data$Investigations1ry %<>% as_factor()
data$Investigations2nd %<>% as_factor()

