

##################### Initilization and Package Installation #######
# Initilization
cat("\014") # Clear the console
rm(list=ls())
graphics.off()

require(pacman)
p_load(dplyr,caret,foreign,
       lubridate,dataPreparation,httr, DT,stringr,AUC,parallel, testit,caretEnsemble,
       C50,
       randomForest,
       kernlab,
       e1071)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/isotonic_paper_functions.R")
# source("/users/PMIU0138/miu0150/isotonic_paper_functions.R")
source("C:/Users/hza0020/OneDrive - Auburn University/Transplant/BUAL-LAB/isotonic_paper_functions.R")



############################ Data Gathering #########################
# first I download the data from github and then merge them into one large file named heart.df
{
  # file1<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_1_9000L.rds?raw=true"))
  # file2<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_9001_18000L.rds?raw=true"))
  # file3<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_18001_27000L.rds?raw=true"))
  # file4<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_27001_36000L.rds?raw=true"))
  # file5<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_36001_45000L.rds?raw=true"))
  # file6<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_45001_54000L.rds?raw=true"))
  # file7<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_54001_63000L.rds?raw=true"))
  # file8<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_63001_72000L.rds?raw=true"))
  # file9<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_72001_81000L.rds?raw=true"))
  # file10<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_81001_90000L.rds?raw=true"))
  # file11<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_90001_99000L.rds?raw=true"))
  # file12<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_99001_108000L.rds?raw=true"))
  # file13<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_108001_117000L.rds?raw=true"))
  # file14<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_117001_126000L.rds?raw=true"))
  # file15<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_126001_135000L.rds?raw=true"))
  # file16<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_135001_144000L.rds?raw=true"))
  # file17<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_144001_153000L.rds?raw=true"))
  # file18<-readRDS(url("https://github.com/hamidahady/transplant/blob/master/data/raw/thoracic_data_153001_159318L.rds?raw=true"))
  
  file1<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_1_9000L.rds"))
  file2<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_9001_18000L.rds"))
  file3<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_18001_27000L.rds"))
  file4<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_27001_36000L.rds"))
  file5<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_36001_45000L.rds"))
  file6<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_45001_54000L.rds"))
  file7<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_54001_63000L.rds"))
  file8<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_63001_72000L.rds"))
  file9<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_72001_81000L.rds"))
  file10<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_81001_90000L.rds"))
  file11<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_90001_99000L.rds"))
  file12<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_99001_108000L.rds"))
  file13<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_108001_117000L.rds"))
  file14<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_117001_126000L.rds"))
  file15<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_126001_135000L.rds"))
  file16<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_135001_144000L.rds"))
  file17<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_144001_153000L.rds"))
  file18<-readRDS(("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/raw/thoracic_data_153001_159318L.rds"))
  
  
  heart.df<-rbind(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10,file11,file12,file13,file14,file15,file16,
                  file17,file18)
  
  rm(list = c("file1","file2","file3","file4","file5","file6","file7","file8","file9","file10","file11","file12","file13",
              "file14","file15","file16","file17","file18"))
}
#####end of importing the data



# heart.form contains the variable definitions of each variable in data
heart.form <- read.csv("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/var_desc.csv")  
form.categories <- table(heart.form$FORM)
knitr::kable(heart.df[1:5,1:5])

# heart.cat explains the categories for each variable in the data
heart.cat <- read.csv("C:/Users/hza0020/Box/Transplant/Thesis/github/transplant/transplant/data/var_desc.csv")
# heart.discard explains which variables should be removed
heart.discard <- heart.cat %>% # We created a column a colum INTERPRETATION_TYPE  
  subset(INTERPRETATION_TYPE=="D", select=c(1))
heart.discard <- heart.discard$VARIABLE.NAME %>% as.character()

# Identifying the variables that did not end & those added before 2000
var.end.dates <- trimws(heart.form$VAR.END.DATE) %>% str_trim()
if.var.did.not.end <- which(var.end.dates=="")
names.of.var.did.not.end <- heart.form[if.var.did.not.end, 1] 

vars.added.dates <- heart.form$VAR.START.DATE %>% as.character.Date()
vars.added.dates[which(vars.added.dates=="01-Oct-87, 01-Oct-90")] <- "01-Oct-90"
vars.added.years<- sapply(vars.added.dates, function(x) str_extract_all(x,"[0-9]{1,2}")[[1]][2]) %>% as.integer()
heart.form$YR_ADDED <- vars.added.years
vars.added.before.2000 <- subset(heart.form,YR_ADDED>=87,
                                 select = c(1))  # available variables added before 2000
vars.added.NA <- subset(heart.form,is.na(YR_ADDED),select = c(1))
vars.added.all <- rbind(vars.added.before.2000,vars.added.NA)
vars.added.all <- vars.added.all[["VARIABLE.NAME"]] %>% 
  as.character()


# Subsetting the data and creating other variables based on literature or to capture time effects
heart.df.cleaned <- subset(heart.df, WL_ORG=="HR") %>% # Heart 
  subset(AGE>=18) %>% # Adults only
  # we excluded too light or too short people
  subset(WGT_KG_DON_CALC >= quantile(WGT_KG_DON_CALC, 0.0001, na.rm = TRUE)) %>% 
  subset(WGT_KG_TCR >= quantile(WGT_KG_TCR, 0.0001, na.rm = TRUE)) %>%
  subset(HGT_CM_DON_CALC >= quantile(HGT_CM_DON_CALC, 0.0001, na.rm = TRUE)) %>% 
  subset(HGT_CM_TCR >= quantile(HGT_CM_TCR, 0.0001, na.rm = TRUE)) %>% 
  
  subset(select=intersect(names.of.var.did.not.end,vars.added.all)) %>% 
  # the next 4 variables are developed based on the Nature paper
  # we did not work with PRAMR since it is added after 2004
  mutate(PVR = (HEMO_PA_MN_TRR - HEMO_PCW_TRR)*79.72/HEMO_CO_TRR) %>% 
  mutate(ISCHTIME = ISCHTIME*60) %>% 
  mutate(ECMO = ifelse(ECMO_TCR + ECMO_TRR == 0, 0, 1)) %>% 
  # mutate(PERCENT_WGT_CHANGE = 100*(WGT_KG_CALC-INIT_WGT_KG_CALC)/INIT_WGT_KG_CALC) %>% 
  # I think having the following variable is useful
  mutate(BMI_CHNG = 100*(BMI_CALC- INIT_BMI_CALC)/INIT_BMI_CALC) %>%
  mutate(WAITING_TIME = TX_DATE - INIT_DATE)  %>%
  mutate(WGT_CHNG = 100*(WGT_KG_CALC - INIT_WGT_KG_CALC)/INIT_WGT_KG_CALC) %>%
  mutate(HGT_CHNG = 100*(HGT_CM_CALC - INIT_HGT_CM_CALC)/INIT_HGT_CM_CALC) %>%
  mutate(AGE_MAT = abs(AGE - AGE_DON)) %>%
  mutate(BMI_MAT = abs(BMI_CALC - BMI_DON_CALC))


# then I drop the following variables so there is no need for them
heart.df.cleaned<-heart.df.cleaned[, !(names(heart.df.cleaned) %in% c("TX_DATE","INIT_DATE"))]

# in the following part, the general definition of the NA cells are identified

# PVR, pulmonary vascular resistance / its calculation is based on the below mentioned links:
# https://en.wikipedia.org/wiki/Vascular_resistance
# http://www.scymed.com/en/smnxph/phkhr013.htm
# https://radiopaedia.org/articles/mean-pulmonary-arterial-pressure  (calculation of Mean Pulmonary Arterial Pressure)
# PVR= (Mean Pulmonary Arterial Pressure (mmHg) - Pulmonary Capillary Wedge Pressure (mmHg)) * 79.72 / Cardiac Output (L/min)
# PVR = (HEMO_PA_MN_TRR - HEMO_PCW_TRR)* 79.72 / HEMO_CO_TRR

# ECMO / merge of (ECMO_TCR, ECMO_TRR)

#Identifying and removing the variables that are post transplant
# Based on observations from the Form and Form Section Descriptors
vars.post.trans.index1 <-  sapply(heart.form$FORM.SECTION,
                                  function(x) str_detect(x,
                                                         "POST TRANSPLANT CLINICAL INFORMATION"))
vars.post.trans.index2 <-  sapply(heart.form$FORM,
                                  function(x) str_detect(x,
                                                         "TRF/TRR|TRR/TRF-CALCULATED|TRR/TRF|TRF"))
vars.post.trans <- heart.form[as.logical(vars.post.trans.index1+vars.post.trans.index2),][,1] %>%
  as.character()
vars.post.trans <- intersect(colnames(heart.df.cleaned),
                             vars.post.trans)
heart.df.cleaned <- select(heart.df.cleaned,-vars.post.trans)
heart.discard <- intersect(heart.discard, colnames(heart.df.cleaned))
heart.df.cleaned <- select(heart.df.cleaned, -heart.discard) 



# the following block is our variable manipulation based on the Nature paper
{
  # refer to a tool provided in the: http://ihtsa.cs.lth.se/ , which is product of this paper:
  # https://www.nature.com/articles/s41598-018-21417-7.pdf
  
  # In this paper they used these variables for recipients:
  # Diagnosis, Age, Gender, Height, Weight, insulin treated diabetes, infection within two weeks, Blood group, previous blood transfusion,
  # previously transplanted, previous cardiac surgery, intensive care unit, mechanical ventilation,
  # ECMO, ventiricular assist device, transplant era, SPP, PVR, creatinine, serum bilirubin,
  # use (mg/dl) instead of (mio mol/l), PRA>10%, HLA-DR 2 mismatch
  # Also, they used these variables for donor:
  # Age, Gender, Height, Weight, Duration of ischemia, Blood group, cause of death
  
  
  # we also used the following variables without any change:
  # Age / AGE, AGE_DON
  # Height / HGT_CM_CALC, HGT_CM_DON_CALC, HGT_CM_TCR, INIT_HGT_CM_CALC, 
  # Gender / GENDER, GENDER_DON
  # Weight / WGT_KG_DON_CALC, WGT_KG_TCR, WGT_KG_CALC, PERCENT_WGT_CHANGE (we made it)
  # infection within two weeks / INFECT_IV_DRUG_TRR
  # previous blood transfusion / TRANSFUSIONS
  # Previously transplanted / PREV_TX
  # Intensive care unit / ICU
  # ventiricular assist device / VAS
  # Mechanical ventilation / VENTILATOR_TRR
  # SPP (mmHG), systolic pulmonary pressure  / HEMO_SYS_TRR
  # Creatinine / CREAT_TRR
  # Serum bilirubin (μmol/l) / TBILI
  # Use (mg/dl) instead of (μmol/l) / TBILI
  # we used mg/dl
  # we did not work with PRAMR since it is added after 2004
  
  # we regroup the Diagnosis as below:
  # Diagnosis / DIAG, TCR_DGN, THORACIC_DGN:
  # Non-ischemic cardiomyopathy: 1201
  # Ischemic: 1007
  # Valv heart disease: 1202
  # Congenital: 1203 1205 1206 1207
  # Other : The rest
  
  
  table(heart.df.cleaned$DIAG)
  heart.df.cleaned$temp<-heart.df.cleaned$DIAG
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$DIAG[i])){
      heart.df.cleaned$DIAG[i]<-"other"
      if(heart.df.cleaned$temp[i]==1201){heart.df.cleaned$DIAG[i]<-"nonisch"}
      if(heart.df.cleaned$temp[i]==1007){heart.df.cleaned$DIAG[i]<-"isch"}
      if(heart.df.cleaned$temp[i]==1202){heart.df.cleaned$DIAG[i]<-"valv"}
      if(heart.df.cleaned$temp[i]==1203){heart.df.cleaned$DIAG[i]<-"cong"}
      if(heart.df.cleaned$temp[i]==1205){heart.df.cleaned$DIAG[i]<-"cong"}
      if(heart.df.cleaned$temp[i]==1206){heart.df.cleaned$DIAG[i]<-"cong"}
      if(heart.df.cleaned$temp[i]==1207){heart.df.cleaned$DIAG[i]<-"cong"}
      
    }
  }
  ###############################################################
  # cause of death / COD_CAD_DON:
  # Head trauma: 3
  # cerebrovascular event: 2
  # other: the rest
  
  heart.df.cleaned$temp<-heart.df.cleaned$COD_CAD_DON
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$COD_CAD_DON[i])){
      heart.df.cleaned$COD_CAD_DON[i]<-"rest"
      if(heart.df.cleaned$temp[i]==3){heart.df.cleaned$COD_CAD_DON[i]<-"head"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$COD_CAD_DON[i]<-"cereb"}
    }
  }
  
  ##############################################################
  # Blood group / ABO, ABO_DON: 
  # A: A1, A2, A
  # B: B
  # AB: A1B, A2B, AB
  # O: O
  
  heart.df.cleaned$temp<-heart.df.cleaned$ABO
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$ABO[i])){
      heart.df.cleaned$ABO[i]<-"AB"
      if(heart.df.cleaned$temp[i]=="A1"){heart.df.cleaned$ABO[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="A2"){heart.df.cleaned$ABO[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="A"){heart.df.cleaned$ABO[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="B"){heart.df.cleaned$ABO[i]<-"B"}
      if(heart.df.cleaned$temp[i]=="O"){heart.df.cleaned$ABO[i]<-"O"}
    }
  }
  
  heart.df.cleaned$temp<-heart.df.cleaned$ABO_DON
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$ABO_DON[i])){
      heart.df.cleaned$ABO_DON[i]<-"AB"
      if(heart.df.cleaned$temp[i]=="A1"){heart.df.cleaned$ABO_DON[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="A2"){heart.df.cleaned$ABO_DON[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="A"){heart.df.cleaned$ABO_DON[i]<-"A"}
      if(heart.df.cleaned$temp[i]=="B"){heart.df.cleaned$ABO_DON[i]<-"B"}
      if(heart.df.cleaned$temp[i]=="O"){heart.df.cleaned$ABO_DON[i]<-"O"}
    }
  }
  
  ##############################################################
  # insulin treated diabetes / DIAB: Refer to this paper, type 1 and type 2 are considered as insulin dependent patient
  # Kaufman, D. B., & Sutherland, D. E. (2011). Simultaneous pancreas-kidney transplants are appropriate in insulin-treated candidates 
  # with uremia regardless of diabetes type. Clinical Journal of the American Society of Nephrology, 6(5), 957-959.
  
  # Yes: 2,3
  # No: 1,4
  
  heart.df.cleaned$temp<-heart.df.cleaned$DIAB
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$DIAB[i])){
      heart.df.cleaned$DIAB[i]<-"N"
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$DIAB[i]<-"Y"}
      if(heart.df.cleaned$temp[i]==3){heart.df.cleaned$DIAB[i]<-"Y"}
    }
  }
  ##############################################################
  
  # previous cardiac surgery /merge of (PRIOR_CARD_SURG_TCR, PRIOR_CARD_SURG_TRR)
  
  heart.df.cleaned$CARD_SURG<-NA
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$PRIOR_CARD_SURG_TCR[i])){
      if(heart.df.cleaned$PRIOR_CARD_SURG_TCR[i]=="Y"){heart.df.cleaned$CARD_SURG[i]<-"Y"}
      
      if(heart.df.cleaned$PRIOR_CARD_SURG_TCR[i]=="N"){
        
        if(!is.na(heart.df.cleaned$PRIOR_CARD_SURG_TRR[i])){
          
          if(heart.df.cleaned$PRIOR_CARD_SURG_TRR[i]=="N"){heart.df.cleaned$CARD_SURG[i]<-"N"}
        }
      }
    }
    if(!is.na(heart.df.cleaned$PRIOR_CARD_SURG_TRR[i])){
      if(heart.df.cleaned$PRIOR_CARD_SURG_TRR[i]=="Y"){heart.df.cleaned$CARD_SURG[i]<-"Y"}}
  } 
  # then I drop the following variables so there is no need for them
  heart.df.cleaned<-heart.df.cleaned[, !(names(heart.df.cleaned) %in% c("PRIOR_CARD_SURG_TCR","PRIOR_CARD_SURG_TRR"))]
  # CARD_SURG: TCR PRIOR CARDIAC SURGERY AT LISTING, TRR CARDIAC SURGERY BETWEEN LISTING AND TRANSPLANT
  ##############################################################
  # HLA-DR 2 mismatch / RDR2:
  # any values: Y, otherwise: 0
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$RDR2[i])){ 
      heart.df.cleaned$RDR2[i]<-"Y"
    }else{heart.df.cleaned$RDR2[i]<-"N"}
  }
  
  ##############################################################
  # Transplant era / TX_YEAR:
  # I focused on after 2000. however, I can do for the rest, together or 4 different modeling
  # -1995
  # 1996-2000
  # 2001-2005
  # 2006-
  heart.df.cleaned$TX_YEAR_CAT<-heart.df.cleaned$TX_YEAR
  
  # categoirizing the transplantation date
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$TX_YEAR[i])){ 
      if(heart.df.cleaned$TX_YEAR[i]<=1995){heart.df.cleaned$TX_YEAR_CAT[i]<-"A"}
      else if(heart.df.cleaned$TX_YEAR[i]>1995 && heart.df.cleaned$TX_YEAR[i]<=2000){heart.df.cleaned$TX_YEAR_CAT[i]<-"B"}
      else if(heart.df.cleaned$TX_YEAR[i]>2000 && heart.df.cleaned$TX_YEAR[i]<=2005){heart.df.cleaned$TX_YEAR_CAT[i]<-"C"}
      else if(heart.df.cleaned$TX_YEAR[i]>2005){heart.df.cleaned$TX_YEAR_CAT[i]<-"D"}
    }
  }
  heart.df.cleaned$TX_YEAR<-NULL
  ##############################################################
  # these are the variables that are derived from the Nature paper
  # Age / AGE, AGE_DON
  # Height / HGT_CM_CALC, HGT_CM_DON_CALC
  # Gender / GENDER, GENDER_DON
  # Weight / WGT_KG_DON_CALC, WGT_KG_TCR
  # Diagnosis / DIAG
  # cause of death / COD_CAD_DON
  # Blood group / ABO, ABO_DON
  # insulin treated diabetes / DIAB
  # infection within two weeks / INFECT_IV_DRUG_TRR
  # previous blood transfusion / TRANSFUSIONS
  # Previously transplanted / PREV_TX
  # previous cardiac surgery / CARD_SURG
  # Intensive care unit / ICU
  # ventiricular assist device / VAS
  # Mechanical ventilation / VENTILATOR_TRR
  # ECMO / ECMO
  # SPP (mmHG), systolic pulmonary pressure  / HEMO_SYS_TRR
  # PVR 
  # Duration of ischemia / ISCHTIME 
  # Creatinine / CREAT_TRR
  # Serum bilirubin (μmol/l) / TBILI
  # PRA > 10 % / PRAMR (we dropped it, because it is made after 2004)
  # HLA-DR 2 mismatch / RDR2
  # Transplant era / TX_YEAR_CAT
  
}

#in the following section I add the other literature variables then clean the data and then look at the data

##############################################################################
##############################################################################
##############################################################################

var_nature<-c("AGE", "AGE_DON","INIT_HGT_CM_CALC", "HGT_CM_DON_CALC","GENDER", "GENDER_DON",
              "WGT_KG_DON_CALC", "INIT_WGT_KG_CALC","DIAG","COD_CAD_DON","ABO", "ABO_DON","DIAB","INFECT_IV_DRUG_TRR",
              "TRANSFUSIONS","PREV_TX","CARD_SURG","ICU","VAS",'VENTILATOR_TRR',"ECMO","HEMO_SYS_TRR","PVR",
              "ISCHTIME" ,"CREAT_TRR","TBILI","RDR2","TX_YEAR_CAT","HEMO_PA_MN_TRR",
              "HEMO_PCW_TRR","HEMO_CO_TRR","ECMO_TRR","ECMO_TCR")

var_lit<-c("LIFE_SUP_TCR","HGT_CM_TCR","DAYS_STAT1","GENDER","GENDER_DON","DEATH_MECH_DON","AGE",
           "HLAMIS","AGE_DON","PROC_TY_HR","EDUCATION","SHARE_TY","IMPL_DEFIBRIL","PULM_INF_DON",
           "TATTOOS","ABO" , "HIST_CIG_DON","DAYS_STAT1A","LIFE_SUP_TRR","MED_COND_TRR",
           "CORONARY_ANGIO","VENT_SUPPORT_AFTER_LIST","ETHCAT_DON","HIST_OTH_DRUG_DON",
           "DIAB","INIT_STAT","DDAVP_DON","END_STAT","DEATH_CIRCUM_DON","PRI_PAYMENT_TCR",
           "ETHCAT","REGION","ABO_DON","DIAG","THORACIC_DGN","FUNC_STAT_TCR",
           "TCR_DGN","FUNC_STAT_TRR","GENDER_MAT","ABO_MAT","AGE_MAT","ETH_MAT")

var_new<-c("WAITING_TIME","BMI_CHNG","WGT_CHNG","HGT_CHNG","BMI_CALC", "INIT_BMI_CALC", "BMI_MAT",
           "WGT_KG_CALC" , "INIT_WGT_KG_CALC","HGT_CM_CALC" , "INIT_HGT_CM_CALC")

var_dep<-c("year0","year1","year2","year3","year4","year5","year6","year7","year8","year9","year10")

var_numeric<-c("AGE", "AGE_DON","HGT_CM_TCR", "HGT_CM_DON_CALC","WGT_KG_DON_CALC", 
               "HEMO_SYS_TRR","PVR", "ISCHTIME" ,"CREAT_TRR","TBILI","DAYS_STAT1",
               "DAYS_STAT1A","AGE_MAT","HEMO_PA_MN_TRR",
               "HEMO_PCW_TRR","HEMO_CO_TRR","WAITING_TIME","BMI_CHNG","WGT_CHNG","HGT_CHNG","BMI_CALC",
               "INIT_BMI_CALC", "BMI_DON_CALC","BMI_MAT",
               "WGT_KG_CALC" , "INIT_WGT_KG_CALC","HGT_CM_CALC" , "INIT_HGT_CM_CALC")
var_char<-c("GENDER","GENDER_DON","DIAG","COD_CAD_DON","ABO","ABO_DON","DIAB","INFECT_IV_DRUG_TRR",
            "TRANSFUSIONS","PREV_TX","CARD_SURG","ICU","VAS","VENTILATOR_TRR","ECMO","RDR2","TX_YEAR_CAT",
            "LIFE_SUP_TCR","DEATH_MECH_DON","HLAMIS","PROC_TY_HR","EDUCATION","SHARE_TY","IMPL_DEFIBRIL",
            "PULM_INF_DON","TATTOOS","HIST_CIG_DON","LIFE_SUP_TRR","MED_COND_TRR","CORONARY_ANGIO",
            "VENT_SUPPORT_AFTER_LIST","ETHCAT_DON","HIST_OTH_DRUG_DON","INIT_STAT","DDAVP_DON",
            "END_STAT","DEATH_CIRCUM_DON","PRI_PAYMENT_TCR","ETHCAT","REGION","THORACIC_DGN",
            "FUNC_STAT_TCR","TCR_DGN","FUNC_STAT_TRR","GENDER_MAT","ABO_MAT","ETH_MAT","ECMO_TRR","ECMO_TCR")

# here save1

# the following block is our variable manipulation based on the other literature as specified in Ali Dag's paper
{
  
  heart.df.cleaned$ETH_MAT<-NA
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$ETHCAT[i])){
      if(!is.na(heart.df.cleaned$ETHCAT_DON[i])){
        if(heart.df.cleaned$ETHCAT_DON[i]==heart.df.cleaned$ETHCAT[i]){
          heart.df.cleaned$ETH_MAT[i]<-"Y"}else{heart.df.cleaned$ETH_MAT[i]<-"N"}
      }
    }
  }
  
  heart.df.cleaned$GENDER_MAT<-NA
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$GENDER[i])){
      if(!is.na(heart.df.cleaned$GENDER_DON[i])){
        if(heart.df.cleaned$GENDER[i]==heart.df.cleaned$GENDER_DON[i]){
          heart.df.cleaned$GENDER_MAT[i]<-"Y"}else{heart.df.cleaned$GENDER_MAT[i]<-"N"}
      }
    }
  }
  
  # according to the several independent levels of DEATH_MECH_DON, I divided it to these groups
  # A:GUNSHOT WOUND, STAB, BLUNT INJURY, Gunshot/stab wound (Pre-OTIS)
  # B: INTRACRANIAL HEMORRHAGE/STROKE, CARDIOVASCULAR
  # Other: DROWNING, SEIZURE, DRUG INTOXICATION, ASPHYXIATION, ELECTRICAL, None of the Above, DEATH FROM NATURAL CAUSES, SIDS
  temp<-heart.df.cleaned
  heart.df.cleaned<-temp
  table(heart.df.cleaned$DEATH_MECH_DON)
  
  heart.df.cleaned$temp<-heart.df.cleaned$DEATH_MECH_DON
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$DEATH_MECH_DON[i])){
      heart.df.cleaned$DEATH_MECH_DON[i]<-"Other"
      if(heart.df.cleaned$temp[i] %in% c(7,8,9,995)){heart.df.cleaned$DEATH_MECH_DON[i]<-"A"}
      if(heart.df.cleaned$temp[i]==11){heart.df.cleaned$DEATH_MECH_DON[i]<-"B"}
    }
  }
  
  # according to the several independent levels of HLAMIS, I divided it to these groups
  # https://bethematch.org/patients-and-families/before-transplant/find-a-donor/hla-matching/
  # according to the refrence, the levels are independent
  # the first 3 levels have fewer members so it makes sense to merge into a group
  table(heart.df.cleaned$HLAMIS)
  heart.df.cleaned$temp<-heart.df.cleaned$HLAMIS
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$HLAMIS[i])){
      heart.df.cleaned$HLAMIS[i]<-"Other"
      if(heart.df.cleaned$temp[i]==4){heart.df.cleaned$HLAMIS[i]<-"A"}
      if(heart.df.cleaned$temp[i]==5){heart.df.cleaned$HLAMIS[i]<-"B"}
      if(heart.df.cleaned$temp[i]==6){heart.df.cleaned$HLAMIS[i]<-"C"}
    }
  }
  
  # PROC_TY_HR, it has 4 and less levels so we keep it. Also it doesn't make sense to merge any
  table(heart.df.cleaned$PROC_TY_HR)
  heart.df.cleaned$temp<-heart.df.cleaned$PROC_TY_HR
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$PROC_TY_HR[i])){
      heart.df.cleaned$PROC_TY_HR[i]<-"Other"
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$PROC_TY_HR[i]<-"A"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$PROC_TY_HR[i]<-"B"}
    }
  }
  
  # SHARE_TY, ALLOCATION TYPE-LOCAL/REGIONAL/NATIONAL - 3=LOCAL/4=REGIONAL/5=NATIONAL/6=FOREIGN
  table(heart.df.cleaned$SHARE_TY)
  heart.df.cleaned$temp<-heart.df.cleaned$SHARE_TY
  heart.df.cleaned$SHARE_TY<- heart.df.cleaned$temp
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$SHARE_TY[i])){
      # heart.df.cleaned$SHARE_TY[i]<-NA
      if(heart.df.cleaned$temp[i]==3){heart.df.cleaned$SHARE_TY[i]<-"A"}
      if(heart.df.cleaned$temp[i]==4){heart.df.cleaned$SHARE_TY[i]<-"B"}
      if(heart.df.cleaned$temp[i]==5 | heart.df.cleaned$temp[i]==6){heart.df.cleaned$SHARE_TY[i]<-"O"}
    }
  }
  
  table(heart.df.cleaned$EDUCATION)
  heart.df.cleaned$temp<-heart.df.cleaned$EDUCATION
  heart.df.cleaned$EDUCATION<-heart.df.cleaned$temp
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$EDUCATION[i])){
      heart.df.cleaned$EDUCATION[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$EDUCATION[i]<-"A"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$EDUCATION[i]<-"A"}
      if(heart.df.cleaned$temp[i]==996){heart.df.cleaned$EDUCATION[i]<-"A"}
      if(heart.df.cleaned$temp[i]==3){heart.df.cleaned$EDUCATION[i]<-"B"}
      if(heart.df.cleaned$temp[i]==4){heart.df.cleaned$EDUCATION[i]<-"B"}
      if(heart.df.cleaned$temp[i]==5){heart.df.cleaned$EDUCATION[i]<-"C"}
      if(heart.df.cleaned$temp[i]==6){heart.df.cleaned$EDUCATION[i]<-"C"}
    }
  }
  
  
  table(heart.df.cleaned$ETHCAT)
  # ETHCAT, ethnicity of recepients
  heart.df.cleaned$temp<-heart.df.cleaned$ETHCAT
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$ETHCAT[i])){
      heart.df.cleaned$ETHCAT[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$ETHCAT[i]<-"W"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$ETHCAT[i]<-"B"}
      if(heart.df.cleaned$temp[i]==4){heart.df.cleaned$ETHCAT[i]<-"H"}
      if(heart.df.cleaned$temp[i]>=5 && heart.df.cleaned$temp[i]<=9){heart.df.cleaned$ETHCAT[i]<-"O"}
      
    }
  } 
  
  table(heart.df.cleaned$ETHCAT_DON)
  # ETHCAT_DON, ethnicity of recepients
  heart.df.cleaned$temp<-heart.df.cleaned$ETHCAT_DON
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$ETHCAT_DON[i])){
      heart.df.cleaned$ETHCAT_DON[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$ETHCAT_DON[i]<-"W"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$ETHCAT_DON[i]<-"B"}
      if(heart.df.cleaned$temp[i]==4){heart.df.cleaned$ETHCAT_DON[i]<-"H"}
      if(heart.df.cleaned$temp[i]>=5 && heart.df.cleaned$temp[i]<=9){heart.df.cleaned$ETHCAT_DON[i]<-"O"}
      
    }
  } 
  
  table(heart.df.cleaned$DEATH_CIRCUM_DON)
  
  heart.df.cleaned$temp<-heart.df.cleaned$DEATH_CIRCUM_DON
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$DEATH_CIRCUM_DON[i])){
      heart.df.cleaned$DEATH_CIRCUM_DON[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$DEATH_CIRCUM_DON[i]<-"A"}
      if(heart.df.cleaned$temp[i]==2){heart.df.cleaned$DEATH_CIRCUM_DON[i]<-"B"}
      if(heart.df.cleaned$temp[i]==6){heart.df.cleaned$DEATH_CIRCUM_DON[i]<-"C"}
      if(heart.df.cleaned$temp[i]>=3 && heart.df.cleaned$temp[i]<=4){heart.df.cleaned$DEATH_CIRCUM_DON[i]<-"O"}
      if(heart.df.cleaned$temp[i]==997){heart.df.cleaned$DEATH_CIRCUM_DON[i]<-"O"}
    }
  }
  
  table(heart.df.cleaned$PRI_PAYMENT_TCR)
  
  heart.df.cleaned$temp<-heart.df.cleaned$PRI_PAYMENT_TCR
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$PRI_PAYMENT_TCR[i])){
      heart.df.cleaned$PRI_PAYMENT_TCR[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$PRI_PAYMENT_TCR[i]<-"A"}
      if(heart.df.cleaned$temp[i]>=2 && heart.df.cleaned$temp[i]<=7){heart.df.cleaned$PRI_PAYMENT_TCR[i]<-"B"}
      if(heart.df.cleaned$temp[i]==13){heart.df.cleaned$PRI_PAYMENT_TCR[i]<-"B"}
      if(heart.df.cleaned$temp[i]>=3 && heart.df.cleaned$temp[i]<=12){heart.df.cleaned$PRI_PAYMENT_TCR[i]<-"O"}
      if(heart.df.cleaned$temp[i]==14){heart.df.cleaned$PRI_PAYMENT_TCR[i]<-"O"}
    }
  }
  # I dropped PRI_PAYMENT_CTRY_TRR because too many NAs
  
  # https://unos.org/transplantation/matching-organs/regions/
  table(heart.df.cleaned$REGION)
  heart.df.cleaned$temp<-heart.df.cleaned$REGION
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$REGION[i])){
      heart.df.cleaned$REGION[i]<-NA
      if(heart.df.cleaned$temp[i]>=1 && heart.df.cleaned$temp[i]<=2){heart.df.cleaned$REGION[i]<-"NE"}
      if(heart.df.cleaned$temp[i]==9){heart.df.cleaned$REGION[i]<-"NE"}
      if(heart.df.cleaned$temp[i]>=3 && heart.df.cleaned$temp[i]<=4){heart.df.cleaned$REGION[i]<-"SE"}
      if(heart.df.cleaned$temp[i]==11){heart.df.cleaned$REGION[i]<-"SE"}
      if(heart.df.cleaned$temp[i]>=7 && heart.df.cleaned$temp[i]<=8){heart.df.cleaned$REGION[i]<-"MW"}
      if(heart.df.cleaned$temp[i]==10){heart.df.cleaned$REGION[i]<-"MW"}
      if(heart.df.cleaned$temp[i]>=5 && heart.df.cleaned$temp[i]<=6){heart.df.cleaned$REGION[i]<-"W"}
      
    }
  }
  
  table(heart.df.cleaned$THORACIC_DGN)
  heart.df.cleaned$temp<-heart.df.cleaned$THORACIC_DGN
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$THORACIC_DGN[i])){
      heart.df.cleaned$THORACIC_DGN[i]<-NA
      if(heart.df.cleaned$temp[i]>=1000 && heart.df.cleaned$temp[i]<=1049){heart.df.cleaned$THORACIC_DGN[i]<-"A"}
      if(heart.df.cleaned$temp[i]>=1050 && heart.df.cleaned$temp[i]<=1099){heart.df.cleaned$THORACIC_DGN[i]<-"B"}
      if(heart.df.cleaned$temp[i]>=1100 && heart.df.cleaned$temp[i]<=1199){heart.df.cleaned$THORACIC_DGN[i]<-"C"}
      if(heart.df.cleaned$temp[i]==1200){heart.df.cleaned$THORACIC_DGN[i]<-"D"}
      if(heart.df.cleaned$temp[i]>=100 && heart.df.cleaned$temp[i]<=999){heart.df.cleaned$THORACIC_DGN[i]<-"O"}
      if(heart.df.cleaned$temp[i]>=1201 && heart.df.cleaned$temp[i]<=1998){heart.df.cleaned$THORACIC_DGN[i]<-"O"}
    }
  }
  
  # here we recategrize FUNC_STAT_TCR, based on their activitiy level and hospitalization status
  #https://www.communitycarenc.org/media/tool-resource-files/what-does-it-take-qualify-personal-care-services-d.pdf
  #http://www.npcrc.org/files/news/karnofsky_performance_scale.pdf
  table(heart.df.cleaned$FUNC_STAT_TCR)
  table(heart.df.cleaned$FUNC_STAT_TRR)
  
  heart.df.cleaned$temp<-heart.df.cleaned$FUNC_STAT_TCR
  heart.df.cleaned$FUNC_STAT_TCR<- heart.df.cleaned$temp
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$FUNC_STAT_TCR[i])){
      heart.df.cleaned$FUNC_STAT_TCR[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$FUNC_STAT_TCR[i]<-"A"}
      if(heart.df.cleaned$temp[i]>=2 && heart.df.cleaned$temp[i]<=3){heart.df.cleaned$FUNC_STAT_TCR[i]<-"B"}
      if(heart.df.cleaned$temp[i]>=2010 && heart.df.cleaned$temp[i]<=2040){heart.df.cleaned$FUNC_STAT_TCR[i]<-"C"}
      if(heart.df.cleaned$temp[i]>=2050 && heart.df.cleaned$temp[i]<=2070){heart.df.cleaned$FUNC_STAT_TCR[i]<-"D"}
      if(heart.df.cleaned$temp[i]>=2080 && heart.df.cleaned$temp[i]<=2100){heart.df.cleaned$FUNC_STAT_TCR[i]<-"E"}
      if(heart.df.cleaned$temp[i]>=4010 ){heart.df.cleaned$FUNC_STAT_TCR[i]<-NA}
      
    }
  }
  
  table(heart.df.cleaned$TCR_DGN)
  heart.df.cleaned$temp<-heart.df.cleaned$TCR_DGN
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$TCR_DGN[i])){
      heart.df.cleaned$TCR_DGN[i]<-NA
      if(heart.df.cleaned$temp[i]>=1000 && heart.df.cleaned$temp[i]<=1049){heart.df.cleaned$TCR_DGN[i]<-"A"}
      if(heart.df.cleaned$temp[i]>=1050 && heart.df.cleaned$temp[i]<=1099){heart.df.cleaned$TCR_DGN[i]<-"B"}
      if(heart.df.cleaned$temp[i]>=1100 && heart.df.cleaned$temp[i]<=1199){heart.df.cleaned$TCR_DGN[i]<-"C"}
      if(heart.df.cleaned$temp[i]==1200 ){heart.df.cleaned$TCR_DGN[i]<-"D"}
      if(heart.df.cleaned$temp[i]==999 ){heart.df.cleaned$TCR_DGN[i]<-"O"}
      if(heart.df.cleaned$temp[i]>=1201){heart.df.cleaned$TCR_DGN[i]<-"O"}
      
    }
  }
  
  
  table(heart.df.cleaned$FUNC_STAT_TRR)
  heart.df.cleaned$temp<-heart.df.cleaned$FUNC_STAT_TRR
  for(i in 1:nrow(heart.df.cleaned)){
    if(!is.na(heart.df.cleaned$FUNC_STAT_TRR[i])){
      heart.df.cleaned$FUNC_STAT_TRR[i]<-NA
      if(heart.df.cleaned$temp[i]==1){heart.df.cleaned$FUNC_STAT_TRR[i]<-"A"}
      if(heart.df.cleaned$temp[i]>=2 && heart.df.cleaned$temp[i]<=3){heart.df.cleaned$FUNC_STAT_TRR[i]<-"B"}
      if(heart.df.cleaned$temp[i]>=2010 && heart.df.cleaned$temp[i]<=2040){heart.df.cleaned$FUNC_STAT_TRR[i]<-"C"}
      if(heart.df.cleaned$temp[i]>=2050 && heart.df.cleaned$temp[i]<=2070){heart.df.cleaned$FUNC_STAT_TRR[i]<-"D"}
      if(heart.df.cleaned$temp[i]>=2080 && heart.df.cleaned$temp[i]<=2100){heart.df.cleaned$FUNC_STAT_TRR[i]<-"E"}
    }
  }
  
  
  
  all_vars<-unique(c(var_nature,var_lit,var_new,var_numeric,var_char))
  
  heart.df.cleaned.lit<-heart.df.cleaned[all_vars]
  
  # Here the NA equivalent characters would change to NA
  {
    NA_cells<-c(""," ","U")
    for(i in 1:length(NA_cells)){
      heart.df.cleaned.lit[heart.df.cleaned.lit == NA_cells[i]] <- NA
      gc()}
  }
  
  #next code is for making sure that type of data is fine
  
  
  heart.df.cleaned.lit$ID<-row.names(heart.df.cleaned.lit)
  
  
}


# here I made 11 consecutive dependent variables, month1, year1, year2, year3, ..., year10
{
  p_unit<-c(1/12,1,2,3,4,5,6,7,8,9,10)
  predict_length<-365
  
  heart.df.cleaned.lit$year0<-NA
  heart.df.cleaned.lit$year1<-NA
  heart.df.cleaned.lit$year2<-NA
  heart.df.cleaned.lit$year3<-NA
  heart.df.cleaned.lit$year4<-NA
  heart.df.cleaned.lit$year5<-NA
  heart.df.cleaned.lit$year6<-NA
  heart.df.cleaned.lit$year7<-NA
  heart.df.cleaned.lit$year8<-NA
  heart.df.cleaned.lit$year9<-NA
  heart.df.cleaned.lit$year10<-NA
  
  heart.df.cleaned.lit$GSTATUS<-heart.df.cleaned$GSTATUS
  heart.df.cleaned.lit$GTIME<-heart.df.cleaned$GTIME
  
  heart.df.cleaned.lit$GSTATUS<-as.character(heart.df.cleaned.lit$GSTATUS)
  
  heart.df.cleaned.lit<-heart.df.cleaned.lit[complete.cases(heart.df.cleaned.lit$GTIME),]
  
  for(i in 1:nrow(heart.df.cleaned.lit)){
    
    heart.df.cleaned.lit$year0[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[1],predict_length)
    heart.df.cleaned.lit$year1[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[2],predict_length)
    heart.df.cleaned.lit$year2[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[3],predict_length)
    heart.df.cleaned.lit$year3[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[4],predict_length)
    heart.df.cleaned.lit$year4[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[5],predict_length)
    heart.df.cleaned.lit$year5[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[6],predict_length)
    heart.df.cleaned.lit$year6[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[7],predict_length)
    heart.df.cleaned.lit$year7[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[8],predict_length)
    heart.df.cleaned.lit$year8[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[9],predict_length)
    heart.df.cleaned.lit$year9[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[10],predict_length)
    heart.df.cleaned.lit$year10[i]<-class_generator_bino(heart.df.cleaned.lit$GSTATUS[i], heart.df.cleaned.lit$GTIME[i] ,p_unit[11],predict_length)
  }
  
  #heart.df.cleaned.lit<-heart.df.cleaned.lit[complete.cases(heart.df.cleaned.lit$year10),]
  heart.df.cleaned.lit$GSTATUS<-NULL
  heart.df.cleaned.lit$GTIME<-NULL
  
}

# I made a new version of data and then drop the numerical NAs and change the NA in categorical data to NO_INFO
heart.df.cleaned.lit_NA<-heart.df.cleaned.lit
# getting rid of NAs in numerical variables
heart.df.cleaned.lit_NA<-heart.df.cleaned.lit_NA[complete.cases(heart.df.cleaned.lit_NA[var_numeric]),]
heart.df.cleaned.lit_NA_temp<-heart.df.cleaned.lit_NA[all_vars]
heart.df.cleaned.lit_NA_temp[is.na(heart.df.cleaned.lit_NA_temp)] <- "UNKOWN"
heart.df.cleaned.lit_NA<-cbind(heart.df.cleaned.lit_NA_temp,heart.df.cleaned.lit_NA[c(var_dep,"ID")])

rm(list=c("heart.df.cleaned.lit_NA_temp","heart.df.cleaned.lit_NA_num","temp"))

# in the following block I make the holdout set which is same in all the years and also train sets
# for each year
{
  keep_NA<-list()
  
  # Here I keep ID 
  keep_NA$ID0<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year0),"ID"]
  keep_NA$ID1<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year1),"ID"]
  keep_NA$ID2<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year2),"ID"]
  keep_NA$ID3<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year3),"ID"]
  keep_NA$ID4<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year4),"ID"]
  keep_NA$ID5<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year5),"ID"]
  keep_NA$ID6<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year6),"ID"]
  keep_NA$ID7<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year7),"ID"]
  keep_NA$ID8<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year8),"ID"]
  keep_NA$ID9<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year9),"ID"]
  keep_NA$ID10<-heart.df.cleaned.lit_NA[!is.na(heart.df.cleaned.lit_NA$year10),"ID"]
  
  # I select the holdout set from the last year since those are available in the previous years
  # Then I exclude those IDs from each year to find IDs for other train sets
  keep_NA$ID_holdout<-sample(keep_NA$ID10,.3*length(keep_NA$ID10))
  keep_NA$ID_train0<-keep_NA$ID0[-which(keep_NA$ID0 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train1<-keep_NA$ID1[-which(keep_NA$ID1 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train2<-keep_NA$ID2[-which(keep_NA$ID2 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train3<-keep_NA$ID3[-which(keep_NA$ID3 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train4<-keep_NA$ID4[-which(keep_NA$ID4 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train5<-keep_NA$ID5[-which(keep_NA$ID5 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train6<-keep_NA$ID6[-which(keep_NA$ID6 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train7<-keep_NA$ID7[-which(keep_NA$ID7 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train8<-keep_NA$ID8[-which(keep_NA$ID8 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train9<-keep_NA$ID9[-which(keep_NA$ID9 %in% keep_NA$ID_holdout)]
  keep_NA$ID_train10<-keep_NA$ID10[-which(keep_NA$ID10 %in% keep_NA$ID_holdout)]
}

#here save2

##############################################################################
##############################################################################
##############################################################################

#then I take out the NAs
# in table_cleaner_simple(Try_data,col2row_emp, ID, kept_col), "Try_data" is the data we use and "col2row_emp" represent 
# our preference of keeping columns in compare with rows, higher means more interest to keep columns than rows, 
# ID is the ID column exclude_vars: is the variables we want to exclude from cleaning
# kept_col, is the columns, that we want to keep in any circumstances
clean_table<-list()

clean_table$heart_max10<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year0","ID")],1,"ID",c("year0"),c("FUNC_STAT_TRR"))
clean_table$heart_max11<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year1","ID")],1,"ID",c("year1"),c("FUNC_STAT_TRR"))
clean_table$heart_max12<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year2","ID")],1,"ID",c("year2"),c("FUNC_STAT_TRR"))
clean_table$heart_max13<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year3","ID")],1,"ID",c("year3"),c("FUNC_STAT_TRR"))
clean_table$heart_max14<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year4","ID")],1,"ID",c("year4"),c("FUNC_STAT_TRR"))
clean_table$heart_max15<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year5","ID")],1,"ID",c("year5"),c("FUNC_STAT_TRR"))
clean_table$heart_max16<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year6","ID")],1,"ID",c("year6"),c("FUNC_STAT_TRR"))
clean_table$heart_max17<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year7","ID")],1,"ID",c("year7"),c("FUNC_STAT_TRR"))
clean_table$heart_max18<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year8","ID")],1,"ID",c("year8"),c("FUNC_STAT_TRR"))
clean_table$heart_max19<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year9","ID")],1,"ID",c("year9"),c("FUNC_STAT_TRR"))
clean_table$heart_max110<-table_cleaner_simple(heart.df.cleaned.lit[c(all_vars,"year10","ID")],1,"ID",c("year10"),c("FUNC_STAT_TRR"))

# Do not run next block it is just  for showing different values as "col2row_emp"
{
  # heart_max5<-table_cleaner_simple(heart.df.cleaned.lit,5,"ID",c(""))
  # heart_max10<-table_cleaner_simple(heart.df.cleaned.lit,10,"ID",c(""))
}
table(train0$year0)
#then I select IDs of 20% of the last year for the holdout set
trimmed<-list()

trimmed$common_IDs<-Reduce(intersect,list(clean_table$heart_max10$row_ID_inc[,],clean_table$heart_max11$row_ID_inc[,],
                                          clean_table$heart_max12$row_ID_inc[,],clean_table$heart_max13$row_ID_inc[,],
                                          clean_table$heart_max14$row_ID_inc[,],clean_table$heart_max15$row_ID_inc[,],
                                          clean_table$heart_max16$row_ID_inc[,],clean_table$heart_max17$row_ID_inc[,],
                                          clean_table$heart_max18$row_ID_inc[,],clean_table$heart_max19$row_ID_inc[,],
                                          clean_table$heart_max110$row_ID_inc[,]))

trimmed$ID_holdout<-sample(trimmed$common_IDs,.20*length(trimmed$common_IDs))
trimmed$ID_train0<-clean_table$heart_max10$row_ID_inc[,][-which(clean_table$heart_max10$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train1<-clean_table$heart_max11$row_ID_inc[,][-which(clean_table$heart_max11$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train2<-clean_table$heart_max12$row_ID_inc[,][-which(clean_table$heart_max12$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train3<-clean_table$heart_max13$row_ID_inc[,][-which(clean_table$heart_max13$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train4<-clean_table$heart_max14$row_ID_inc[,][-which(clean_table$heart_max14$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train5<-clean_table$heart_max15$row_ID_inc[,][-which(clean_table$heart_max15$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train6<-clean_table$heart_max16$row_ID_inc[,][-which(clean_table$heart_max16$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train7<-clean_table$heart_max17$row_ID_inc[,][-which(clean_table$heart_max17$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train8<-clean_table$heart_max18$row_ID_inc[,][-which(clean_table$heart_max18$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train9<-clean_table$heart_max19$row_ID_inc[,][-which(clean_table$heart_max19$row_ID_inc[,] %in% trimmed$ID_holdout)]
trimmed$ID_train10<-clean_table$heart_max110$row_ID_inc[,][-which(clean_table$heart_max110$row_ID_inc[,] %in% trimmed$ID_holdout)]

#################
sum((clean_table$heart_max10$row_ID_inc[,] %in% trimmed$ID_holdout))
sum((clean_table$heart_max15$row_ID_inc[,] %in% trimmed$ID_holdout))
sum((clean_table$heart_max110$row_ID_inc[,] %in% trimmed$ID_holdout))

#here save3

# in the next part I merge all the categories that just resemble less than 5% of my data into other "oth" category
# however in the dummy maker section, that level is shooted out because of in the general rule of dummy making one level should
# taken away  

i<-0
#here is make formula for different years
str(temp)
results<-list()
for(i in 0:10){
  # in dummay_maker2 function, I do not care about correlation of the created dummy variables, and I do not 
  # drop any dummy variable representing a level of the categorical variables
  temp<-dummy_maker(type_fixer(heart.df.cleaned.lit[,
                                                     eval(parse(text = paste("clean_table$heart_max1",i,"$col_names_inc",sep = "")))],
                                var_numeric,var_char,var_dep,c(var_dep,"ID")),var_char)
  
  train<-temp[trimmed[[paste("ID_train",i,sep="")]],]
  hold_out<-temp[trimmed$ID_holdout,]
  
  
  # train<-dummy_maker(type_fixer(heart.df.cleaned.lit[trimmed[[paste("ID_train",i,sep="")]],
  #             eval(parse(text = paste("clean_table$heart_max1",i,"$col_names_inc",sep = "")))],
  #                var_numeric,var_char,var_dep,c(var_dep,"ID")),var_char)
  # 
  # hold_out<-dummy_maker(type_fixer(heart.df.cleaned.lit[trimmed$ID_holdout,
  #             eval(parse(text = paste("clean_table$heart_max1",i,"$col_names_inc",sep = "")))],
  #                var_numeric,var_char,var_dep,c(var_dep,"ID")),var_char)
  
  
  
  vars<-colnames(train)[!colnames(train) %in% c(var_dep,"ID")]
  
  assign("formula_format",paste("year",i," ~ ",paste(vars, collapse="+"),sep = ""))
  assign(paste("formul_year",i,sep=""),as.formula(formula_format))
  
  
  results[[paste("resul_year",i,"_RUS_clean",sep = "")]]<-pred_func(train,hold_out,
                                                       eval(parse(text = paste("formul_year",i,sep = ""))),
                                                       "RUS",log=1,svm=1,nnet=1,rf_bag=1,
                                                       rf_bag_stack=1,c5_boost=1, gbm_boost=1,
                                                       cart_bag=1,repeat_no=1, fold_no=5)
}
# now I do preparation and prediction on the data without dropping the NAs
# I considered NAs as UNKOWN


# then I do the prediction for the ones that instead of dropping rows and columns I just subsituted NAs with "UNKOWN"
for(i in 0:10){
  vars<-unique(c(var_numeric,var_char,paste("year",i,sep=""),"ID"))
  
  temp<-dummy_maker2(type_fixer(heart.df.cleaned.lit_NA[keep_NA[[paste("ID",i,sep="")]],vars],
                               var_numeric,var_char,var_dep,c(var_dep,"ID")),var_char)
  
  train<-temp[keep_NA[[paste("ID_train",i,sep="")]],]
  hold_out<-temp[keep_NA$ID_holdout,]
  
  
  vars<-colnames(train)[!colnames(train) %in% c(var_dep,"ID")]
  
  assign("formula_format",paste("year",i," ~ ",paste(vars, collapse="+"),sep = ""))
  assign(paste("formul_year_NA",i,sep=""),as.formula(formula_format))
  
  
  results[[paste("resul_year",i,"_RUS_NA",sep = "")]]<-pred_func(train,hold_out,
                                                          eval(parse(text = paste("formul_year_NA",i,sep = ""))),
                                                          "RUS",log=1,svm=1,nnet=1,rf_bag=1,
                                                          rf_bag_stack=1,c5_boost=1, gbm_boost=1,
                                                          cart_bag=1,repeat_no=1, fold_no=5)
  
  
  
}
# now I do preparation and prediction on the data without dropping the NAs


#Here is how you can find the performance for the two versions clean "after dropping rows and column" and NA "without dropping"

results$resul_year0_RUS_clean$resul_perf
results$resul_year1_RUS_clean$resul_perf
results$resul_year2_RUS_clean$resul_perf
results$resul_year3_RUS_clean$resul_perf
results$resul_year4_RUS_clean$resul_perf
results$resul_year5_RUS_clean$resul_perf
results$resul_year6_RUS_clean$resul_perf
results$resul_year7_RUS_clean$resul_perf
results$resul_year8_RUS_clean$resul_perf
results$resul_year9_RUS_clean$resul_perf
results$resul_year10_RUS_clean$resul_perf

results$resul_year0_RUS_NA$resul_perf
results$resul_year1_RUS_NA$resul_perf
results$resul_year2_RUS_NA$resul_perf
results$resul_year3_RUS_NA$resul_perf
results$resul_year4_RUS_NA$resul_perf
results$resul_year5_RUS_NA$resul_perf
results$resul_year6_RUS_NA$resul_perf
results$resul_year7_RUS_NA$resul_perf
results$resul_year8_RUS_NA$resul_perf
results$resul_year9_RUS_NA$resul_perf
results$resul_year10_RUS_NA$resul_perf

#Here Save5

knitr::kable(table(heart.df.cleaned$SHARE_TY))





