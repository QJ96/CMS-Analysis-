
#-------Load libraries---------------------------------


library(lubridate)
library(scales)
library(summarytools)
library(expss)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(flextable)
library(haven)
library(tidyverse)
library(janitor)
library(stringr)
library(english)
library(knitr)
#library(kableExtra)
library(lubridate)
library(DT)
library(haven)
library(htmlwidgets)
library(gtools)



#--------------------- Load files----------------------------

Medicare_Part_D_2020_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2020.csv"
AS_CMS_PA_2020<- read.csv(Medicare_Part_D_2020_PA_file)

Medicare_Part_D_2021_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2021.csv"
AS_CMS_PA_2021<- read.csv(Medicare_Part_D_2021_PA_file) 

Medicare_Part_D_2022_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/Medicare_Part_D_Prescribers_by_Provider_2022.csv"
AS_CMS_PA_2022<- read.csv(Medicare_Part_D_2022_PA_file)

PA_zipcode_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/PA_zipcode.xlsx"
PA_zipcode <- read_excel(PA_zipcode_file) 


#----------Data cleaning-------------------------------------

# PA zipcode package (N= 2213)

duplicates_zipcode_package_1 <- PA_zipcode %>% #0
  mutate(zipcode = as.numeric(zipcode)) %>% 
  group_by(zipcode) %>% 
  filter(n()>1)

duplicates_county_package_2 <- PA_zipcode %>% #2213 
  mutate(county = as.numeric(county)) %>% 
  group_by(county) %>% 
  filter(n()>1)

missing_zipcodepack <- sum(is.na(PA_zipcode$zipcode)) #0
missing_countypack <- sum(is.na(PA_zipcode$county)) # 3 (for zips : 15266, 15273, 15288)

range_zippack <- range(PA_zipcode$zipcode) #(min 15001-- max 19640)

unique_zippack <- PA_zipcode %>% #(n=2213)
  distinct(zipcode)

#merge

# zipcode file check
PA_zipcode_merge <- PA_zipcode %>% 
  mutate(zipcode = as.numeric(zipcode)) %>% 
  select(Prscrbr_zip5=zipcode, County= county)


#------------------------------------------------------------------------------------

#CMS 2020

prscbr_summary_2020 <- AS_CMS_PA_2020 %>% #(n= 60,229--- n= 32,199)
  select(PRSCRBR_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_zip5,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),#due to presence of commas after converting to numeric those values are replaced with NAs
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # (n= 32428) excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) 

#B.No. of Specialties; variable: Prscrbr_Type 

blank_count_spcl_2020 <- sum(prscbr_summary_2020$Prscrbr_Type == "") # no blanks
missing_spcls_2020 <- sum(is.na(prscbr_summary_2020$Prscrbr_Type)) # 0 missing

tot_specialties_1_2020 <- prscbr_summary_2020 %>% #(n=80) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type))

# collapsing specialties
prscbr_summary_2020 <- prscbr_summary_2020 %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      Prscrbr_Type %in% c("Pediatric Medicine",
                                                          "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                                          "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type))

tot_specialties_2_2020 <- prscbr_summary_2020 %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=67) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_2020 <- prscbr_summary_2020 %>% #(n=23952)
  select(PRSCRBR_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_zip5,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type_new,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant")))

CMS_2020_zip_pack <- specialt_name_2020 %>% 
  mutate(Prscrbr_zip5 = as.numeric(Prscrbr_zip5))

#leftjoin
CMS_zipcountypack_2020_merge <- CMS_2020_zip_pack %>% #(n=23952)
  left_join(PA_zipcode_merge, by="Prscrbr_zip5")

missing_zipcodepack_merge <- sum(is.na(CMS_zipcountypack_2020_merge$zipcode)) #0
missing_countypack_merge <- sum(is.na(CMS_zipcountypack_2020_merge$County)) # 11 (for zips : 15266, 15273, 15288)

CMS_zip_rate <- CMS_zipcountypack_2020_merge %>% #(n=23952)
  mutate(AB_rate_2020 =(Antbtc_Tot_Clms/Tot_Benes)* 1000,
        County = gsub(" County", "", County)
         ) %>% 
  filter(!(is.na(County))) #(n=23941)


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")


top_5_settings_2020 <-CMS_zip_rate %>% 
  filter(Prscrbr_Type_new %in% c("Dentistry","Infectious Disease","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology")) %>%
  left_join(counties_settings, by = c("County" = "County Name")) %>% 
  group_by(Prscrbr_Type_new,settings = `Rural/ Urban`) %>%
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (Antbtc_Tot_Clms ) /(Tot_Benes) * 1000,
    AB_Prescp_rate=round(AB_Prescp_rate)) %>% 
  #providers = n() 
  arrange(desc(AB_Prescp_rate)) %>%
  pivot_wider(
    names_from = settings,
    values_from = c(Tot_Benes, Antbtc_Tot_Clms, AB_Prescp_rate)
    
  ) %>% 
  rename(Specialty = Prscrbr_Type_new,
         `Number of antibiotic claims rural` = Antbtc_Tot_Clms_Rural,
         `Number of total beneficiaries rural` = Tot_Benes_Rural,
         `Antibiotic prescriptions per 1,000 beneficiaries,Rate rural` = AB_Prescp_rate_Rural,
         #`Number of Specialists_Rural` = providers_Rural,
         `Number of antibiotic claims urban` = Antbtc_Tot_Clms_Urban,
         `Number of total beneficiaries urban` = Tot_Benes_Urban,
         `Antibiotic prescriptions per 1,000 beneficiaries,Rate urban` = AB_Prescp_rate_Urban,
         #`Number of Specialists_Urban` = providers_Urban
  ) %>% 
  select(1,4,2,6,5,3,7)


#------ CMS 2021--------------------------------------------------------------------------

prscbr_summary_2021 <- AS_CMS_PA_2021 %>% #(n= 61409 --- 32839)
  select(PRSCRBR_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,
         Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_St1,Prscrbr_St2,Prscrbr_St2,Prscrbr_City,Prscrbr_State_FIPS,Prscrbr_zip5) %>% 
  mutate(Prscrbr_First_Name= case_when(Prscrbr_First_Name == ""~ NA_character_,
                                       T~Prscrbr_First_Name),
         Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~ 5, T~ as.numeric(Tot_Benes)),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms%in%c(0,1))) %>%  # n= 33010 excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes%in% 5)) # n= 32839 excluded values < 11 which are assumed to be suppressed values

blank_count_spcl_2021 <- sum(AS_CMS_PA_2021$Prscrbr_Type == "") # no blanks
missing_count_spcls_2021 <- sum(is.na(AS_CMS_PA_2021$Prscrbr_Type)) # 0 missing

tot_specialties_1_2021 <- prscbr_summary_2021 %>% #(n=81) before collapsing specialties
  summarize(Total_Unique_Specialties = n_distinct(Prscrbr_Type))

prscbr_summary_2021 <- prscbr_summary_2021 %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      # Prscrbr_Type %in% c("Pediatric Medicine",
                                      #                     "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      # Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                      #                     "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type)) 


tot_specialties_2_2021 <- prscbr_summary_2021 %>% #(n=67)
  summarize(Specialties = n_distinct(Prscrbr_Type_new))  # after collapsing specialties and without excluding nurse practioners and Physician Assistant

specialt_name_2021 <- prscbr_summary_2021 %>% #(32839---- 24164)
  select(PRSCRBR_NPI,Prscrbr_Type,Prscrbr_zip5,Prscrbr_State_Abrvtn, Tot_Benes,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 

CMS_2021_zip_pack <- specialt_name_2021 %>% #(n= 24164)
  mutate(Prscrbr_zip5 = as.numeric(Prscrbr_zip5))

#leftjoin
CMS_zipcountypack_2021_merge <- CMS_2021_zip_pack %>% #(n=24164)
  left_join(PA_zipcode_merge, by="Prscrbr_zip5")

missing_zipcodepack_merge_2021 <- sum(is.na(CMS_zipcountypack_2021_merge$zipcode)) #0
missing_countypack_merge_2021 <- sum(is.na(CMS_zipcountypack_2021_merge$County)) # 14 

CMS_zip_rate_2021 <- CMS_zipcountypack_2021_merge %>% #(n=24164)
  mutate(AB_rate_2021 =(Antbtc_Tot_Clms/Tot_Benes)* 1000,
         County = gsub(" County", "", County)
  ) %>% 
  filter(!(is.na(County))) #(n=24150)



counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")


top_5_settings_2021 <-CMS_zip_rate_2021 %>% 
  filter(Prscrbr_Type_new %in% c("Infectious Disease","Dentistry","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology")) %>%
  left_join(counties_settings, by = c("County" = "County Name")) %>% 
  group_by(Prscrbr_Type_new,settings = `Rural/ Urban`) %>%
  group_by(Prscrbr_Type_new,settings) %>%
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (sum(Antbtc_Tot_Clms ) /sum(Tot_Benes)) * 1000,
    AB_Prescp_rate=round(AB_Prescp_rate),
    #providers = n()
  ) %>%
  arrange(desc(AB_Prescp_rate)) %>%
  ungroup %>% 
  pivot_wider(
    names_from = settings,
    values_from = c(Tot_Benes, Antbtc_Tot_Clms, AB_Prescp_rate)#,providers)
  ) %>% 
  rename(SPECIALTY = Prscrbr_Type_new,
         `NUMBER OF ANTIBIOTIC CLAIMS RURAL` = Antbtc_Tot_Clms_Rural,
         `NUMBER OF TOTAL BENEFICIARIES RURAL` = Tot_Benes_Rural,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE RURAL` = AB_Prescp_rate_Rural,
         `NUMBER OF ANTIBIOTIC CLAIMS URBAN` = Antbtc_Tot_Clms_Urban,
         `NUMBER OF TOTAL BENEFICIARIES URBAN` = Tot_Benes_Urban,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE URBAN` = AB_Prescp_rate_Urban) %>% 
  select(1,4,2,6,5,3,7)


#------------------ CMS 2022----------------------------------------------------------------------------------------------------------------------------

prscbr_summary_2022 <- AS_CMS_PA_2022 %>% #(n= 62,959--33824)
  select(Prscrbr_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_zip5,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_City) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes),  #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) # excluded values < 11 which are assumed to be suppressed values


blank_count_spcl_2022 <- sum(prscbr_summary_2022$Prscrbr_Type == "") # no blanks
missing_spcls_2022 <- sum(is.na(prscbr_summary_2022$Prscrbr_Type)) # 0 missing

tot_specialties_1_2022 <- prscbr_summary_2022 %>% #(n=83) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type)) 


prscbr_summary_2022 <- prscbr_summary_2022 %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      Prscrbr_Type %in% c("Pediatric Medicine",
                                                          "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                                          "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type))

tot_specialties_2_2022 <- prscbr_summary_2022 %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=68) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_2022 <- prscbr_summary_2022 %>% #(n=24420)
  select(Prscrbr_NPI,Prscrbr_Type,Prscrbr_zip5,Prscrbr_State_Abrvtn, Tot_Benes,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 


CMS_2022_zip_pack <- specialt_name_2022 %>% #(n= 24420)
  mutate(Prscrbr_zip5 = as.numeric(Prscrbr_zip5))

#leftjoin
CMS_zipcountypack_2022_merge <- CMS_2022_zip_pack %>% #(n=24420)
  left_join(PA_zipcode_merge, by="Prscrbr_zip5")

missing_zipcodepack_merge_2022 <- sum(is.na(CMS_zipcountypack_2022_merge$zipcode)) #0
missing_countypack_merge_2022 <- sum(is.na(CMS_zipcountypack_2022_merge$County)) # 13 

CMS_zip_rate_2022 <- CMS_zipcountypack_2022_merge %>% # (n=24420)
  mutate(AB_rate_2022 =(Antbtc_Tot_Clms/Tot_Benes)* 1000,
         County = gsub(" County", "", County)) %>% 
  filter(!(is.na(County))) #(n=24407)


top_5_settings_2022 <-CMS_zip_rate_2022 %>% 
  filter(Prscrbr_Type_new %in% c("Infectious Disease","Dentistry","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology")) %>%
  left_join(counties_settings, by = c("County" = "County Name")) %>% 
  group_by(Prscrbr_Type_new,settings = `Rural/ Urban`) %>%
  group_by(Prscrbr_Type_new,settings) %>%
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (Antbtc_Tot_Clms ) /(Tot_Benes) * 1000,
    AB_Prescp_rate=round(AB_Prescp_rate)) %>% 
  #providers = n() 
  arrange(desc(AB_Prescp_rate)) %>%
  pivot_wider(
    names_from = settings,
    values_from = c(Tot_Benes, Antbtc_Tot_Clms, AB_Prescp_rate)) %>% 
  rename(specialty = Prscrbr_Type_new,
         `number of antibiotic claims rural` = Antbtc_Tot_Clms_Rural,
         `number of total beneficiaries rural` = Tot_Benes_Rural,
         `antibiotic prescriptions per 1,000 beneficiaries,rate rural` = AB_Prescp_rate_Rural,
         #`Number of Specialists_Rural` = providers_Rural,
         `number of antibiotic claims urban` = Antbtc_Tot_Clms_Urban,
         `number of total beneficiaries urban` = Tot_Benes_Urban,
         `antibiotic prescriptions per 1,000 beneficiaries,rate urban` = AB_Prescp_rate_Urban,
         #`Number of Specialists_Urban` = providers_Urban
  ) %>% 
  select(1,4,2,6,5,3,7)



#---------------------------------Maps--------------------------------------------------


Rates_by_county <- CMS_zip_rate %>% 
  group_by(County) %>% 
  summarise(Ab_rate_2020 = mean(AB_rate_2020)) %>% 
  mutate(Ab_rate_2020 = round(Ab_rate_2020, 1))

# with number of prescribers
Rates_by_county <- CMS_zip_rate %>% 
  group_by(County) %>% 
  summarise(
    Ab_rate_2020 = mean(AB_rate_2020),
    num_prescribers = n_distinct(PRSCRBR_NPI)  # Count unique prescribers
  ) %>% 
  mutate(Ab_rate_2020 = round(Ab_rate_2020, 1)) 
# %>% adorn_totals()

# install.packages("writexl")
# library(writexl)
# write_xlsx(CMS_zip_rate, "Merged_CMS_county_2020.xlsx")

#-------------------------- Counties: geometry/ lat, long ---------------------------

library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(writexl)

# Load Pennsylvania county shapefile
pa_counties <- counties(state = "PA", cb = TRUE)

# Left join shape file with medicare data
map_data <- pa_counties%>% #(23941----n=23942 (additional forest county from counties included)--included all 67 counties)
  left_join(CMS_zip_rate , by = c("NAME"="County"))


# NOTES:
# PA counties :67
# CMS data has 66/67 counties, doesn't have data for Forest county
# We excluded 11 zipcode observations from CMS 21 which are not matched with any counties
# ignore:  left_join(CMS_zip_rate, by = c("NAMELSAD" = "County"))

average_AB_rate_overall_2020 <- map_data %>%
  group_by(NAME) %>%
  summarise(avg_AB_rate_2020 = mean(AB_rate_2020, na.rm = TRUE),
            num_prescribers = n_distinct(PRSCRBR_NPI)  # Count unique prescribers
  ) %>% 
  mutate(avg_AB_rate_2020 = round(avg_AB_rate_2020, 1)) %>% 
  ungroup() 


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

#source : https://www.rural.pa.gov/data/rural-urban-definitions  --- list of counties data ----- The Center for Rural Pennsylvania's definition of rural and urban is based on population density.


average_AB_rate_2020 <- average_AB_rate_overall_2020 %>% 
  left_join(counties_settings, by = c("NAME"= "County Name")) %>% 
  #filter(!(NAME == "Forest")) %>% 
  rename(setting = `Rural/ Urban`)

average_AB_rate_2020$rate_category <- cut(average_AB_rate_2020$avg_AB_rate_2020,breaks = c(-Inf, 500, 600, 700, Inf),                        
                                          labels = c("< 500", "500-600", "600-700", "> 700"))


average_AB_rate_2020 <- average_AB_rate_2020 %>% 
  mutate(setting_symbol = ifelse(setting== "Rural", "+", "-"),
         label = paste0(NAME, " ", setting_symbol))

########################################################################################################################


#--------- CMS 2021--------------------------------------------------------------------------

prscbr_summary_2021 <- AS_CMS_PA_2021 %>% #(n= 61409 --- 32839)
  select(PRSCRBR_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,
         Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_St1,Prscrbr_St2,Prscrbr_St2,Prscrbr_City,Prscrbr_State_FIPS,Prscrbr_zip5) %>% 
  mutate(Prscrbr_First_Name= case_when(Prscrbr_First_Name == ""~ NA_character_,
                                       T~Prscrbr_First_Name),
         Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~ 5, T~ as.numeric(Tot_Benes)),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms%in%c(0,1))) %>%  # n= 33010 excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes%in% 5)) # n= 32839 excluded values < 11 which are assumed to be suppressed values

blank_count_spcl_2021 <- sum(AS_CMS_PA_2021$Prscrbr_Type == "") # no blanks
missing_count_spcls_2021 <- sum(is.na(AS_CMS_PA_2021$Prscrbr_Type)) # 0 missing

tot_specialties_1_2021 <- prscbr_summary_2021 %>% #(n=81) before collapsing specialties
  summarize(Total_Unique_Specialties = n_distinct(Prscrbr_Type))

prscbr_summary_2021 <- prscbr_summary_2021 %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      # Prscrbr_Type %in% c("Pediatric Medicine",
                                      #                     "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      # Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                      #                     "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type)) 


tot_specialties_2_2021 <- prscbr_summary_2021 %>% #(n=67)
  summarize(Specialties = n_distinct(Prscrbr_Type_new))  # after collapsing specialties and without excluding nurse practioners and Physician Assistant

specialt_name_2021 <- prscbr_summary_2021 %>% #(32839---- 24164)
  select(PRSCRBR_NPI,Prscrbr_Type,Prscrbr_zip5, Tot_Benes,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 

CMS_2021_zip_pack <- specialt_name_2021 %>% #(n= 24164)
  mutate(Prscrbr_zip5 = as.numeric(Prscrbr_zip5))

#leftjoin
CMS_zipcountypack_2021_merge <- CMS_2021_zip_pack %>% #(n=24164)
  left_join(PA_zipcode_merge, by="Prscrbr_zip5")

missing_zipcodepack_merge_2021 <- sum(is.na(CMS_zipcountypack_2021_merge$zipcode)) #0
missing_countypack_merge_2021 <- sum(is.na(CMS_zipcountypack_2021_merge$County)) # 14 

CMS_zip_rate_2021 <- CMS_zipcountypack_2021_merge %>% #(n=24164)
  mutate(AB_rate_2021 =(Antbtc_Tot_Clms/Tot_Benes)* 1000,
         County = gsub(" County", "", County)
  ) %>% 
  filter(!(is.na(County))) #(n=24150)


Rates_by_county_2021 <- CMS_zip_rate_2021 %>% 
  group_by(County) %>% 
  summarise(Ab_rate_2021 = mean(AB_rate_2021),
            num_prescribers_21 = n_distinct(PRSCRBR_NPI)) %>% 
  mutate(Ab_rate_2021 = round(Ab_rate_2021, 1))

# install.packages("writexl")
# library(writexl)
# write_xlsx(CMS_zip_rate_2021, "Merged_CMS_county_2021.xlsx")


#-------------------------- Counties: geometry/ lat, long ---------------------------

library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(writexl)

# Load Pennsylvania county shapefile
pa_counties <- counties(state = "PA", cb = TRUE)

# Left join shape file with medicare data
map_data_2021 <- pa_counties%>% #(24150----n=24151 (additional forest county from counties included)--included all 67 counties)
  left_join(CMS_zip_rate_2021 , by = c("NAME"="County"))
 
# NOTES:
# PA counties :67
# CMS data has 66/67 counties, doesn't have data for Forest county
# We excluded 14 zipcode observations from CMS 21 which are not matched with any counties
# ignore:  left_join(CMS_zip_rate, by = c("NAMELSAD" = "County"))

average_AB_rate_overall_2021 <- map_data_2021 %>%
  group_by(NAME) %>%
  summarise(avg_AB_rate_2021 = mean(AB_rate_2021, na.rm = TRUE),
            num_prescribers_21 = n_distinct(PRSCRBR_NPI) ) %>% 
  mutate(avg_AB_rate_2021 = round(avg_AB_rate_2021, 1)) %>% 
  ungroup()


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

average_AB_rate_2021 <- average_AB_rate_overall_2021 %>% 
  left_join(counties_settings, by = c("NAME"= "County Name")) %>% 
 #filter(!(NAME == "Forest")) %>% 
  rename(setting = `Rural/ Urban`)

average_AB_rate_2021$rate_category <- cut(average_AB_rate_2021$avg_AB_rate_2021,breaks = c(-Inf, 500, 600, 700, Inf),                        
                        labels = c("< 500", "500-600", "600-700", "> 700"))


average_AB_rate_2021 <- average_AB_rate_2021 %>% 
  mutate(setting_symbol = ifelse(setting== "Rural", "+", "-"),
         label = paste0(NAME, " ", setting_symbol))

########################################################################################################################

#------------------ CMS 2022----------------------------

prscbr_summary_2022 <- AS_CMS_PA_2022 %>% #(n= 62,959--33824)
  select(Prscrbr_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_zip5,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_City) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes),  #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) # excluded values < 11 which are assumed to be suppressed values


blank_count_spcl_2022 <- sum(prscbr_summary_2022$Prscrbr_Type == "") # no blanks
missing_spcls_2022 <- sum(is.na(prscbr_summary_2022$Prscrbr_Type)) # 0 missing

tot_specialties_1_2022 <- prscbr_summary_2022 %>% #(n=83) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type)) 


prscbr_summary_2022 <- prscbr_summary_2022 %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      Prscrbr_Type %in% c("Pediatric Medicine",
                                                          "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                                          "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type))


tot_specialties_2_2022 <- prscbr_summary_2022 %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=68) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_2022 <- prscbr_summary_2022 %>% #(n=24420)
  select(PRSCRBR_NPI=Prscrbr_NPI,Prscrbr_Type,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new,Prscrbr_City) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 


CMS_2022_zip_pack <- specialt_name_2022 %>% #(n= 24420)
  mutate(Prscrbr_zip5 = as.numeric(Prscrbr_zip5))

#leftjoin
CMS_zipcountypack_2022_merge <- CMS_2022_zip_pack %>% #(n=24420)
  left_join(PA_zipcode_merge, by="Prscrbr_zip5")

missing_zipcodepack_merge_2022 <- sum(is.na(CMS_zipcountypack_2022_merge$zipcode)) #0
missing_countypack_merge_2022 <- sum(is.na(CMS_zipcountypack_2022_merge$County)) # 13 

CMS_zip_rate_2022 <- CMS_zipcountypack_2022_merge %>% # (n=24420)
  mutate(AB_rate_2022 =(Antbtc_Tot_Clms/Tot_Benes)* 1000,
         County = gsub(" County", "", County)) %>% 
  filter(!(is.na(County))) #(n=24407)

Rates_by_county_2022 <- CMS_zip_rate_2022 %>% 
  group_by(County) %>% 
  summarise(Ab_rate_2022 = mean(AB_rate_2022),
            num_prescribers_22 = n_distinct(PRSCRBR_NPI)) %>% 
  mutate(Ab_rate_2022 = round(Ab_rate_2022, 1))


# install.packages("writexl")
# library(writexl)
# write_xlsx(CMS_zip_rate_2022, "Merged_CMS_county_2022.xlsx")


#-------------------------- Counties: geometry/ lat, long ---------------------------

library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(writexl)

# Load Pennsylvania county shapefile
pa_counties <- counties(state = "PA", cb = TRUE)

# Left join shape file with medicare data
map_data_2022 <- pa_counties%>% #(24407----n=24408 (additional forest county from counties included)--included all 67 counties)
  left_join(CMS_zip_rate_2022 , by = c("NAME"="County"))


# NOTES:
# PA counties :67
# CMS data has 66/67 counties, doesn't have data for Forest county
# We excluded 13 zipcode observations from CMS 21 which are not matched with any counties
# ignore:  left_join(CMS_zip_rate, by = c("NAMELSAD" = "County"))

average_AB_rate_overall_2022 <- map_data_2022 %>%
  group_by(NAME) %>%
  summarise(avg_AB_rate_2022 = mean(AB_rate_2022, na.rm = TRUE),
            num_prescribers_22 = n_distinct(PRSCRBR_NPI)) %>% 
  mutate(avg_AB_rate_2022 = round(avg_AB_rate_2022, 1)) %>% 
  ungroup()


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

average_AB_rate_2022 <- average_AB_rate_overall_2022 %>% 
  left_join(counties_settings, by = c("NAME"= "County Name")) %>% 
  #filter(!(NAME == "Forest")) %>% 
  rename(setting = `Rural/ Urban`)

average_AB_rate_2022$rate_category <- cut(average_AB_rate_2022$avg_AB_rate_2022,breaks = c(-Inf, 500, 600, 700, Inf),                        
                                          labels = c("< 500", "500-600", "600-700", "> 700"))

average_AB_rate_2022 <- average_AB_rate_2022 %>% 
  mutate(setting_symbol = ifelse(setting== "Rural", "+", "-"),
         label = paste0(NAME, " ", setting_symbol))

###########################################################################################################################3

# All Years: 2020 - 2022


Rates_by_county_20_22 <- Rates_by_county %>% 
  full_join(Rates_by_county_2021, by = "County") %>% 
  full_join(Rates_by_county_2022, by = "County")

Mean_rates_by_county_20_22 <- Rates_by_county_20_22 %>% 
  mutate(Mean_ABrate_20_22 = round(rowMeans(across(c(Ab_rate_2020, 
                                                     Ab_rate_2021,
                                                     Ab_rate_2022)), 
                                                  na.rm = TRUE), 1)) 
  
#-------------------------- Counties: geometry/ lat, long ---------------------------

library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(writexl)

# Load Pennsylvania county shapefile
pa_counties_20_22 <- counties(state = "PA", cb = TRUE)

# Left join shape file with medicare data
map_data_20_22 <- pa_counties_20_22%>% #(24407----n=24408 (additional forest county from counties included)--included all 67 counties)
  left_join(Mean_rates_by_county_20_22 , by = c("NAME"="County"))


# NOTES:
# PA counties :67
# CMS data has 66/67 counties, doesn't have data for Forest county
# We excluded 13 zipcode observations from CMS 21 which are not matched with any counties
# ignore:  left_join(CMS_zip_rate, by = c("NAMELSAD" = "County"))

# average_AB_rate_overall_20_22 <- map_data_20_22 %>%
#   group_by(NAME) %>%
#   summarise(avg_AB_rate_2022 = mean(AB_rate_2022, na.rm = TRUE)) %>% 
#   mutate(avg_AB_rate_2022 = round(avg_AB_rate_2022, 1)) %>% 
#   ungroup()


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

average_AB_rate_20_22 <- map_data_20_22 %>% 
  left_join(counties_settings, by = c("NAME"= "County Name")) %>% 
  #filter(!(NAME == "Forest")) %>% 
  rename(setting = `Rural/ Urban`)

average_AB_rate_20_22 <- average_AB_rate_20_22 %>%
  mutate(
    num_prescribers = as.numeric(num_prescribers),
    num_prescribers_21 = as.numeric(num_prescribers_21),
    num_prescribers_22 = as.numeric(num_prescribers_22))

average_AB_rate_20_22 <- average_AB_rate_20_22 %>%
  rowwise() %>%
  mutate(mean_prescribers_3_years = mean(c(num_prescribers, num_prescribers_21, num_prescribers_22), na.rm = TRUE),
         fill_color = ifelse(mean_prescribers_3_years < 5, NA, Mean_ABrate_20_22)) %>%
  ungroup()

average_AB_rate_20_22$rate_category <- cut(average_AB_rate_20_22$Mean_ABrate_20_22,breaks = c(-Inf, 500, 600, 700, Inf),                        
                                          labels = c("< 500", "500-600", "600-700", "> 700"))

average_AB_rate_20_22 <- average_AB_rate_20_22 %>% 
  mutate(setting_symbol = ifelse(setting== "Rural", "+", "-"),
         label = paste0(NAME, " ", setting_symbol))


install.packages("RColorBrewer")
library(RColorBrewer)

palette <- brewer.pal(n = 4, name = "Blues")

## with grey colored counties (2020-2022)

pacman::p_load(ggrepel)


average_AB_rate_20_22 <- st_as_sf(average_AB_rate_20_22)

# Calculate centroids for the label positions
average_AB_rate_20_22$centroid <- st_centroid(average_AB_rate_20_22$geometry)

# Extract the x and y coordinates of the centroids
average_AB_rate_20_22$x <- st_coordinates(average_AB_rate_20_22$centroid)[,1]
average_AB_rate_20_22$y <- st_coordinates(average_AB_rate_20_22$centroid)[,2]


# geom_sf_text
ggplot(data = average_AB_rate_20_22) +
  geom_sf(aes(fill = fill_color), color = "black") + 
  # geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
  #              size = 3.5, color = "black",fontface = "bold" ) +
  geom_sf_text(aes( label = NAME), 
               size = 3.3, color = "black", fontface = "bold") +  
  # Add setting_symbol with larger size (e.g., size 5)
  geom_text(aes(x = x, y = y, label = setting_symbol), 
            size = 6, color = "black", fontface = "bold", vjust = 1.5)+
  scale_fill_gradientn(
    colors = palette, 
    name = "Prescribing Rate", 
    na.value = "grey",  
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    plot.margin = margin(0, 0, 2, 0, "cm") 
  ) +
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 2, size = 4, color = "black") +
  coord_sf(clip = 'off')


# without grey colored counties

palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

  ggplot(data = average_AB_rate_20_22) +
  geom_sf(aes(fill = Mean_ABrate_20_22), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 3, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 2, size = 4, color = "black")+
  coord_sf(clip = 'off')

ggsave("rurality_map_21.png",rurality_map_21,width =10, height = 8, dpi = 300)

  
  Mean_rate_by_county_rev_2020_21 <- prescribing_rate_by_county_rev_2020_22 %>% 
  select(1,2,4,5) %>% 
  mutate(Mean_rate_vul_cat_20_21 = round(rowMeans(across(c(mean_AB_Prescp_rate, 
                                                           mean_AB_Prescp_rate_21)), 
                                                  na.rm = TRUE), 1))

average_AB_rate_20_22 <- average_AB_rate_2020 %>% 
  full_join(average_AB_rate_2021, by)


###################################################### Map ################################################################3

install.packages("RColorBrewer")
library(RColorBrewer)
palette <- brewer.pal(n = 4, name = "Blues")

ggplot(data = average_AB_rate_2021) +
  geom_sf(aes(fill = rate_category), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 3, color = "black", fontface = "bold")+
  scale_fill_manual(
    values = palette,
    name = "Outpatient antibiotic prescriptions
            per 1,000 persons",
    guide = guide_legend(
      title.position = "left",
      title.hjust = 0.5,
      label.position = "right",
      keywidth = unit(1, "cm"),  # Width of each legend key (rectangle)
      keyheight = unit(0.1, "cm"),  # Height of each legend key
      ncol  = 1,  # Display keys in a single row
      #byrow = TRUE
    )) + 
  theme_minimal() + 
  labs(title = "Antibiotic Prescribing Rates by Counties, Pennsylvania, 2020",
       theme(
         legend.position = "bottom",
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.ticks = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()
       ))+
  annotate("text", x = Inf, y = Inf, label = " Rural (+)\nUrban (-)", 
                   hjust = -0.5, vjust = 11, size = 4, color = "black")+
  coord_sf(clip = 'off')








#option 2
# 2020

# Update palette to include grey for "NA"
palette <- c(brewer.pal(n = 4, name = "Blues"), "grey")

# Ensure labels include "NA"
labels <- c("< 500", "500-600", "600-700", "> 700", "Data not available")
labels <-  as.data.frame(labels) %>% 
  mutate(labels = as.numeric(labels))
# Create the main plot

ggplot(data = average_AB_rate_2020) +
  geom_sf(aes(fill = rate_category), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(
    values = setNames(palette, labels),
    name = "Outpatient antibiotic prescriptions\nper 1,000 persons"
  ) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Place legend at the bottom
    legend.box.spacing = unit(0.5, "cm"),  # Adjust space between legend items
    legend.key.width = unit(1.3, "cm"),  # Width of the legend keys
    legend.key.height = unit(0.09, "cm"),  # Height of the legend keys
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 5, size = 4, color = "black")+
  coord_sf(clip = 'off')

colnames(average_AB_rate_2020)

#-------------------------------

# average_AB_rate_2020 <- average_AB_rate_2020 %>% 
#   mutate(average_AB_rate_2020_new= case_when(NAME =="Forest"~ NA,
#                                          T~average_AB_rate_2020))
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

rurality_map_20 <-
  
  ggplot(data = average_AB_rate_2020) +
  geom_sf(aes(fill = avg_AB_rate_2020), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 4, color = "black") +
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend)+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 2, size = 4, color = "black")+
  coord_sf(clip = 'off')

ggsave("rurality_map_20.png",rurality_map_20,width =10, height = 8, dpi = 300)


###########

svi_map_22 <- ggplot(data = pa_county_data_22) +
  geom_sf(aes(fill = mean_AB_Prescp_rate)) + 
  geom_sf_text(aes(label = NAME), size = 3, color = "black", fontface= "bold") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "")+
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 9)) 


ggsave("2022 SVI map rev.png",svi_map_22,width =10, height = 8, dpi = 300)


# 2021

# Update palette to include grey for "NA"
palette <- c(brewer.pal(n = 4, name = "Blues"), "grey")

# Ensure labels include "NA"
labels <- c("< 500", "500-600", "600-700", "> 700", "Data not available")

# Create the main plot

ggplot(data = average_AB_rate_2021) +
  geom_sf(aes(fill = rate_category), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(
    values = setNames(palette, labels),
    name = "Outpatient antibiotic prescriptions\nper 1,000 persons"
  ) +
  theme_minimal() + 
  labs(title = "Antibiotic Prescribing Rates by Counties, Pennsylvania, 2021") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Place legend at the bottom
    legend.box.spacing = unit(0.5, "cm"),  # Adjust space between legend items
    legend.key.width = unit(1.3, "cm"),  # Width of the legend keys
    legend.key.height = unit(0.09, "cm"),  # Height of the legend keys
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 5, size = 4, color = "black")+
  coord_sf(clip = 'off')


#--------------
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

rurality_map_21 <- ggplot(data = average_AB_rate_2021) +
  geom_sf(aes(fill = avg_AB_rate_2021), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 3, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 2, size = 4, color = "black")+
  coord_sf(clip = 'off')

ggsave("rurality_map_21.png",rurality_map_21,width =10, height = 8, dpi = 300)


#2022

# Update palette to include grey for "NA"
palette <- c(brewer.pal(n = 4, name = "Blues"), "grey")

# Ensure labels include "NA"
labels <- c("< 500", "500-600", "600-700", "> 700", "Data not available")

# Create the main plot

ggplot(data = average_AB_rate_2022) +
  geom_sf(aes(fill = rate_category), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 2, color = "black", fontface = "bold") +
  scale_fill_manual(
    values = setNames(palette, labels),
    name = "Outpatient antibiotic prescriptions\nper 1,000 persons"
  ) +
  theme_minimal() + 
  labs(title = "Antibiotic Prescribing Rates by Counties, Pennsylvania, 2022") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",  # Place legend at the bottom
    legend.box.spacing = unit(0.5, "cm"),  # Adjust space between legend items
    legend.key.width = unit(1.3, "cm"),  # Width of the legend keys
    legend.key.height = unit(0.09, "cm"),  # Height of the legend keys
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 5, size = 4, color = "black")+
  coord_sf(clip = 'off')

#------ 2022 --------------
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

rurality_map_22 <- ggplot(data = average_AB_rate_2022) +
  geom_sf(aes(fill = avg_AB_rate_2022), color = "black") + 
  geom_sf_text(aes(label = paste(NAME, setting_symbol, sep = "\n")),
               size = 3, color = "black", fontface = "bold") +
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  theme_minimal() + 
  labs(title = "") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9),
    plot.margin = margin(0, 0, 2, 0, "cm")  # Add margin for the legend
  )+
  annotate("text", x = Inf, y = Inf, label =  "  Setting\n Rural (+)\n Urban (-)", 
           hjust = -0.5, vjust = 2, size = 4, color = "black")+
  coord_sf(clip = 'off')

ggsave("rurality_map_22.png",rurality_map_22,width =10, height = 8, dpi = 300)





###############################################################################################################################################################3

#----- Revised Statistical analysis T test (ABrate vs settings)


counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")


#merge 2020-2022

CMS_zip_rate <- CMS_zip_rate %>% 
  mutate(year = "2020") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2020,year)

CMS_zip_rate_2021 <- CMS_zip_rate_2021 %>% 
  mutate(year = "2021") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2021, year)

CMS_zip_rate_2022 <- CMS_zip_rate_2022 %>% 
  mutate(year = "2022") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2022, year)


CMS_zip_rate_20_22 <- rbind(CMS_zip_rate,CMS_zip_rate_2021,CMS_zip_rate_2022) %>% 
  left_join(counties_settings, by = c("County" = "County Name")) 


##### Line level

# Dentistry

Dentistry_map_ttest <- CMS_zip_rate_20_22 %>% 
  filter(Prscrbr_Type_new == "Dentistry") %>% 
  #select(1,10:15) %>% 
  #left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate(AB_rate = round(AB_rate)) %>% 
  rename(Setting = `Rural/ Urban`)

# t test

Den_value <- t.test(AB_rate ~ Setting, data = Dentistry_map_ttest)

Den_pvalue <- data.frame(specialty = "Dentistry",P_value =Den_value$p.value)


# Infectious Disease

ID_map_ttest <- CMS_zip_rate_20_22 %>% 
  filter(Prscrbr_Type_new == "Infectious Disease") %>% 
  #select(1,10:15) %>% 
  #left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate(AB_rate = round(AB_rate)) %>% 
  rename(Setting = `Rural/ Urban`)

# t test

ID_value <- t.test(AB_rate ~ Setting, data = ID_map_ttest)

ID_pvalue <- data.frame(specialty = "Infectious Disease",P_value = sprintf("%.2f",ID_value$p.value))

# Oral and maxillofacial surgery

OM_map_ttest <- CMS_zip_rate_20_22 %>% 
  filter(Prscrbr_Type_new == "Oral and Maxillofacial Surgery") %>% 
  #select(1,10:15) %>% 
  #left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate(AB_rate = round(AB_rate)) %>% 
  rename(Setting = `Rural/ Urban`)

# t test


OM_value <- t.test(AB_rate ~ Setting, data = OM_map_ttest)

OM_pvalue <- data.frame(specialty = "Oral and Maxillofacial Surgery",P_value =sprintf("%.2f",OM_value$p.value))

#Plastic and reconstructive surgery


PR_map_ttest <- CMS_zip_rate_20_22 %>% 
  filter(Prscrbr_Type_new == "Plastic and Reconstructive Surgery") %>% 
  # select(1,10:15) %>% 
  # left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate(AB_rate = round(AB_rate)) %>% 
  rename(Setting = `Rural/ Urban`)

# t test

PR_value <- t.test(AB_rate ~ Setting, data = PR_map_ttest)

PR_pvalue <- data.frame(specialty = "Plastic and Reconstructive Surgery",P_value =sprintf("%.2f",PR_value$p.value))

# Urology


Uro_map_ttest <- CMS_zip_rate_20_22 %>% 
  filter(Prscrbr_Type_new == "Urology") %>% 
  # select(1,10:15) %>% 
  # left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate(AB_rate = round(AB_rate)) %>% 
  rename(Setting = `Rural/ Urban`)

# t test

Uro_value <- t.test(AB_rate ~ Setting, data = Uro_map_ttest)


Uro_pvalue <- data.frame(specialty = "Urology",P_value =sprintf("%.2f",Uro_value$p.value))


CMS_20_22_pvalues <- rbind(Den_pvalue,ID_pvalue,OM_pvalue,PR_pvalue,Uro_pvalue)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#### overall line level, 12/23 #####



#merge 2020-2022

CMS_zip_rate_2020_b <- CMS_zip_rate %>% 
  mutate(year = "2020") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2020,year)

CMS_zip_rate_2021_b <- CMS_zip_rate_2021 %>% 
  mutate(year = "2021") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2021, year)

CMS_zip_rate_2022_b <- CMS_zip_rate_2022 %>% 
  mutate(year = "2022") %>% 
  select(Prscrbr_Type_new,Prscrbr_Gndr,Prscrbr_zip5,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,County,AB_rate=AB_rate_2022, year)

CMS_zip_rate_20_22_b <- rbind(CMS_zip_rate_2020_b,CMS_zip_rate_2021_b,CMS_zip_rate_2022_b) %>% 
  left_join(counties_settings, by = c("County" = "County Name")) %>% 
  mutate (AB_rate = round(AB_rate,2)) %>% 
  rename(setting = `Rural/ Urban`)


# T test

CMS_zip_rate_20_22_b$setting <- as.factor(CMS_zip_rate_20_22_b$setting)

Ttest_result <-t.test(AB_rate ~ setting, data = CMS_zip_rate_20_22_b)

print(Ttest_result)

# Levene's Test

install.packages("car")
library(car)

leveneTest(AB_rate ~ setting, data = CMS_zip_rate_20_22_b)


# Since the p-value (0.007862) is less than 0.05,
# Null hypothesis is rejected and concluded that the variancesbetween the groups are significantly different.


# The assumption of equal variances is violated  Welch’s t-test performed, which adjusts for unequal variances.

# Welch’s t-test

Ttest_result <- t.test(AB_rate ~ setting, data = CMS_zip_rate_20_22_b, var.equal = FALSE)

print(Ttest_result)

###############################################################################



### Gender 

specialt_name_2020 <- specialt_name_2020 %>% 
  mutate(year = 2020) %>% 
  select(-Prscrbr_Last_Org_Name, -Prscrbr_First_Name , - Prscrbr_Ent_Cd,-Prscrbr_State_Abrvtn)

specialt_name_2021 <- specialt_name_2021 %>%  
mutate(year = 2021) %>% 
  select(-Prscrbr_Type)

specialt_name_2022 <- specialt_name_2022 %>% 
  mutate(year = 2022) %>% 
  select(-Prscrbr_Type)


Gender_2020_22 <- rbind(specialt_name_2020,specialt_name_2021,specialt_name_2022) %>% 
  mutate(AB_Prescp_rate_20_22 = (Antbtc_Tot_Clms ) /(Tot_Benes) * 1000,
         AB_Prescp_rate_20_22 = round(AB_Prescp_rate_20_22,2))
  
 

# T test

Gender_2020_22$Prscrbr_Gndr <- as.factor(Gender_2020_22$Prscrbr_Gndr)

Ttest_gender <-t.test(AB_Prescp_rate_20_22 ~ Prscrbr_Gndr, data = Gender_2020_22)

print(Ttest_gender)


# Levene's Test

install.packages("car")
library(car)

leveneTest(AB_Prescp_rate_20_22 ~ Prscrbr_Gndr, data = Gender_2020_22)

# Since the p-value (0.007862) is less than 0.05,
# Null hypothesis is rejected and concluded that the variancesbetween the groups are significantly different.


# The assumption of equal variances is violated  Welch’s t-test performed, which adjusts for unequal variances.

# Welch’s t-test

Ttest_result_gender <- t.test(AB_Prescp_rate_20_22 ~ Prscrbr_Gndr, data = Gender_2020_22, var.equal = FALSE)

print(Ttest_result_gender)



















