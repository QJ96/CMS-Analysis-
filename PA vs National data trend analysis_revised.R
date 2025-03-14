


# Title: Top 5 specialties Antibiotic Prescription Rates (2020-2022): Pennsylvania (state) vs USA (National) 
# Data: Medicare Part D - by Provider state & National
# Display : Bar charts


#Loading Packages

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

# Main Analysis: #### USA only #####
# Note: This script has only US data, run PA (medicare part d by provider) data before.################

AS_CMS_2020_US <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2020_US.csv"
AS_CMS_2020_US <- read.csv(AS_CMS_2020_US)

#1255175

# Data Cleaning and transformation (missing values,blanks,duplicates,mutations,pivoting,binding, summarize, grouping) and
# calculating counts required for tables

Prescribers_F_2020_US <- sum(is.na(AS_CMS_2020_US$Prscrbr_First_Name))       # 0
Prescribers_F_B_2020_US <- sum((AS_CMS_2020_US$Prscrbr_First_Name == ""))    # 51 blank
Prescribers_L_2020_US <- sum(is.na(AS_CMS_2020_US$Prscrbr_Last_Org_Name)) # 0
Prescribers_L_B_2020_US <- sum((AS_CMS_2020_US$Prscrbr_Last_Org_Name == "")) # 0
duplicates_NPI_2020_US <- AS_CMS_2020_US$Prscrbr_NPI[duplicated(AS_CMS_2020_US$Prscrbr_NPI)] # empty

# Calculating total No.of Prescribers (n= 1255175); variables :Prscrbr_First_Name, Prscrbr_Last_Org_Name

Total_prescribers_2020_US <- AS_CMS_2020_US %>% 
  summarise(Total_Prescribers_2020 = sum(!is.na(Prscrbr_First_Name)))

#num_rows <- dim(Total_prescribers_2020)[1]

Total_prescribers_new_2020 <- Total_prescribers_2020_US %>% 
  mutate(Total_prescribers_2020_US = format(Total_prescribers_2020_US, big.mark=","))# formatted to include comma 


prscbr_summary_2020_US <- AS_CMS_2020_US %>% # (n= 1255175--- n= 680414)
  select(Prscrbr_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),#due to presence of commas after converting to numeric those values are replaced with NAs
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # (n= 685713) excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) # (n= 680414) excluded values < 11 which are assumed to be suppressed values
#----------------------------------------------------------------------------------------------------------------
#A. No.of Prescribers; variable:Prscrbr_First_Name

Prescribers_2020_US<- prscbr_summary_2020_US%>% 
  summarise(Prescribers = sum(!is.na(Prscrbr_First_Name)))

Prescribers_new_2020_US <- Prescribers_2020_US %>% 
  mutate(Prescribers = format(Prescribers, big.mark=","))# formatted to include common in numbers wherever required

#----------------------------------------------------------------------------------------------------------------
#B.No. of Specialties; variable: Prscrbr_Type 

blank_count_spcl_2020_US <- sum(prscbr_summary_2020_US$Prscrbr_Type == "") # no blanks
missing_spcls_2020_US <- sum(is.na(prscbr_summary_2020_US$Prscrbr_Type)) # 0 missing

tot_specialties_1_2020_US <- prscbr_summary_2020_US %>% #(n=148) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type))

# collapsing specialties
prscbr_summary_2020_US <- prscbr_summary_2020_US %>% 
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


tot_specialties_2_2020_US <- prscbr_summary_2020_US %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=128) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

tot_specialties_names_2020_US <- prscbr_summary_2020_US %>%
  distinct(Prscrbr_Type_new)
#----------------------------------------------------------------------------------------------------------------
#C. No. of Beneficiaries (n=6626763) ; variable :Tot_Benes, Counts fewer than 11 are suppressed and are indicated by a blank.(inconsitencies- commas, blanks)

blank_count_bene_2020_US <- sum(AS_CMS_2020_US$Tot_Benes == "") # 142965 blanks = suppressed
missing_count_bene_2020_US<- sum(is.na(AS_CMS_2020_US$Tot_Benes)) # 0 missing

Total_benefi_2020_US <- prscbr_summary_2020_US %>% # 134889718
  summarize(Beneficiaries = sum(Tot_Benes))

Total_benefi_new_2020_US <- Total_benefi_2020_US %>% #  134,889,718
  mutate(Beneficiaries = format(Beneficiaries, big.mark=","))

#Notes by Analyst (QJ):: 7030 blanks(suppressed) were in raw dataset been converted to missing and later to numeric 5 as per requirements,and are excluded from study(N= 229(no.of blanks in benes after excl 0,1); n= 32428( after excl 0,1)-229 = 32199)
#----------------------------------------------------------------------------------------------------------------

#D. No. of total antibiotic claims(n=2423316); variable :Antbtc_Tot_Clms

#The Antbtc_Tot_Clms is suppressed when Antbtc_Tot_Clms is between 1 and 10.
#A blank indicates the value is suppressed.

blank_count_Anbtcclms_2020_US <- sum(AS_CMS_2020_US$Antbtc_Tot_Clms == "") #373592
missing_count_Anbtcclms_2020_US <- sum(is.na(AS_CMS_2020_US$Antbtc_Tot_Clms)) # 0

Total_Anbtcclms_2020_US <- prscbr_summary_2020_US %>%
  summarize(`Antibiotic Claims` = sum(Antbtc_Tot_Clms,na.rm = TRUE))

Total_Anbtcclms_new_2020_US <- Total_Anbtcclms_2020_US %>% # 52,747,454
  mutate(`Antibiotic Claims` = format(`Antibiotic Claims`, big.mark=","))# formatted to include common in numbers wherever required

#F.Overall Antibiotic Prescription rate for PA (after excluding suppressed values )
#calculation: (Total_Anbtcclms_2020 / Total_benefi_2020)* 1000 (No. of outpatient antibiotic prescriptions per 1,000 
#beneficiaries)

antibiotic_prescription_rate_2020_US <- (Total_Anbtcclms_2020_US / Total_benefi_2020_US)* 1000 

antibiotic_prescription_rate_2020_US <- antibiotic_prescription_rate_2020_US %>% 
  rename(`Antibiotic Prescription Rate 2020 US`= `Antibiotic Claims`)  

antibiotic_prescription_rate_2020_US <-antibiotic_prescription_rate_2020_US %>% #(value=391.04-- 391)
  mutate(`Antibiotic Prescription Rate 2020 US`=round(`Antibiotic Prescription Rate 2020 US`,1))


#SECTION 2

# Table 2. Top 5 Specialties; variables:(Prscrbr_Type,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc)

# excluding Nurse Practitioner and Physician Assistant

specialt_name_2020_US <- prscbr_summary_2020_US %>% #(n=505300)
  select(Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 

 dental_subgroups <- specialt_name_2020_US %>% 
   filter(!(Prscrbr_Type %in% c("Dentist", 	
                                "Oral Surgery (Dentist only)",
                                "Student in an Organized Health Care Education/Training Program",
                                "Independent Medical Examiner")))

# total distinct specialties and their counts

specialty_count_1_2020_US <- specialt_name_2020_US %>% #(n=126)
  count(Prscrbr_Type_new, name = "Specialty_Counts") #(nurse and physician assistant were excluded)

specialty_count_1a_2020_US <-specialty_count_1_2020_US  %>% #(n=505300)
  summarize(total_spcl_count = sum(Specialty_Counts,na.rm = TRUE))


Top_5_specl_2020_US <- specialt_name_2020_US %>%
  group_by(Prscrbr_Type_new) %>%
  left_join(specialty_count_1_2020_US ,by= "Prscrbr_Type_new") %>% 
  filter(Specialty_Counts >= 100) %>%
  filter(Prscrbr_Type_new %in% c("Dentistry","Infectious Disease","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology"))%>% 
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000) %>%
  #arrange(desc(AB_Prescp_rate)) %>%
  #top_n(5) %>% 
  left_join(specialty_count_1_2020_US ,by= "Prscrbr_Type_new") %>% 
  relocate(Specialty_Counts,.after = Prscrbr_Type_new) %>% 
  relocate(Tot_Benes, .after = Antbtc_Tot_Clms) %>% 
  mutate(AB_Prescp_rate=round(AB_Prescp_rate)) %>% 
  rename(`Specialty US 20` =Prscrbr_Type_new,
         `Number of specialists US 20`= Specialty_Counts,
         `Number of antibiotic claims US 20`=Antbtc_Tot_Clms,
         `Number of total beneficiaries US 20`=Tot_Benes,
         `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 20`= AB_Prescp_rate)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------


#2021

AS_CMS_2021_US<- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2021_US.csv"
AS_CMS_2021_US<- read.csv(AS_CMS_2021_US)

#1287454

# Data Cleaning and transformation (missing values,blanks,duplicates,mutations,pivoting,binding, summarize, grouping) and
# calculating counts required for tables

Prescribers_F_2021_US<- sum(is.na(AS_CMS_2021_US$Prscrbr_First_Name))       # 0
Prescribers_F_B_2021_US<- sum((AS_CMS_2021_US$Prscrbr_First_Name == ""))    # 2 blanks
Prescribers_L_2021_US<- sum(is.na(AS_CMS_2021_US$Prscrbr_Last_Org_Name)) # 0
Prescribers_L_B_2021_US<- sum((AS_CMS_2021_US$Prscrbr_Last_Org_Name == "")) # 0
duplicates_NPI_2021_US<- AS_CMS_2021_US$Prscrbr_NPI[duplicated(AS_CMS_2021_US$Prscrbr_NPI)] # empty

# Calculating total No.of Prescribers (n= 1287454); variables :Prscrbr_First_Name, Prscrbr_Last_Org_Name

Total_prescribers_2021_US<- AS_CMS_2021_US%>% 
  summarise(Total_Prescribers_2021 = sum(!is.na(Prscrbr_First_Name)))

#num_rows <- dim(Total_prescribers_2021)[1]

Total_prescribers_new_2021_US<- Total_prescribers_2021_US%>% #n= 1,287,454
  mutate(Total_prescribers_2021_US= format(Total_prescribers_2021_US, big.mark=","))# formatted to include comma 


prscbr_summary_2021_US<- AS_CMS_2021_US%>% # (n= 1287454--- n= 701070)
  select(Prscrbr_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),#due to presence of commas after converting to numeric those values are replaced with NAs
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # (n= 581730) excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) # (n= 4654) excluded values < 11 which are assumed to be suppressed values
#----------------------------------------------------------------------------------------------------------------
#A. No.of Prescribers; variable:Prscrbr_First_Name (n=701,070)

Prescribers_2021_US<- prscbr_summary_2021_US%>% 
  summarise(Prescribers = sum(!is.na(Prscrbr_First_Name)))

Prescribers_new_2021_US<- Prescribers_2021_US%>% 
  mutate(Prescribers = format(Prescribers, big.mark=","))# formatted to include common in numbers wherever required

#----------------------------------------------------------------------------------------------------------------
#B.No. of Specialties; variable: Prscrbr_Type 

blank_count_spcl_2021_US<- sum(prscbr_summary_2021_US$Prscrbr_Type == "") # 2 blanks
missing_spcls_2021_US<- sum(is.na(prscbr_summary_2021_US$Prscrbr_Type)) # 0 missing

tot_specialties_1_2021_US<- prscbr_summary_2021_US%>% #(n=150) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type))

# collapsing specialties
prscbr_summary_2021_US<- prscbr_summary_2021_US%>% 
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


tot_specialties_2_2021_US<- prscbr_summary_2021_US%>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=129) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

tot_specialties_names_2021_US<- prscbr_summary_2021_US%>%
  distinct(Prscrbr_Type_new)
#----------------------------------------------------------------------------------------------------------------
#C. No. of Beneficiaries (n=6626763) ; variable :Tot_Benes, Counts fewer than 11 are suppressed and are indicated by a blank.(inconsitencies- commas, blanks)

blank_count_bene_2021_US<- sum(AS_CMS_2021_US$Tot_Benes == "") # 140711 blanks = suppressed
missing_count_bene_2021_US<- sum(is.na(AS_CMS_2021_US$Tot_Benes)) # 0 missing

Total_benefi_2021_US<- prscbr_summary_2021_US%>% # 143105510
  summarize(Beneficiaries = sum(Tot_Benes))

Total_benefi_new_2021_US<- Total_benefi_2021_US%>% #  143,105,510
  mutate(Beneficiaries = format(Beneficiaries, big.mark=","))

#Notes by Analyst (QJ):: 7030 blanks(suppressed) were in raw dataset been converted to missing and later to numeric 5 as per requirements,and are excluded from study(N= 229(no.of blanks in benes after excl 0,1); n= 32428( after excl 0,1)-229 = 32199)
#----------------------------------------------------------------------------------------------------------------

#D. No. of total antibiotic claims(n=2423316); variable :Antbtc_Tot_Clms

#The Antbtc_Tot_Clms is suppressed when Antbtc_Tot_Clms is between 1 and 10.
#A blank indicates the value is suppressed.

blank_count_Anbtcclms_2021_US<- sum(AS_CMS_2021_US$Antbtc_Tot_Clms == "") #377130
missing_count_Anbtcclms_2021_US<- sum(is.na(AS_CMS_2021_US$Antbtc_Tot_Clms)) # 0

Total_Anbtcclms_2021_US<- prscbr_summary_2021_US%>% #(53847375)
  summarize(`Antibiotic Claims` = sum(Antbtc_Tot_Clms,na.rm = TRUE))

Total_Anbtcclms_new_2021_US<- Total_Anbtcclms_2021_US%>% # 53,847,375
  mutate(`Antibiotic Claims` = format(`Antibiotic Claims`, big.mark=","))# formatted to include common in numbers wherever required



#F.Overall Antibiotic Prescription rate for PA (after excluding suppressed values )
#calculation: (Total_Anbtcclms_2021 / Total_benefi_2021)* 1000 (No. of outpatient antibiotic prescriptions per 1,000 
#beneficiaries)

antibiotic_prescription_rate_2021_US<- (Total_Anbtcclms_2021_US/ Total_benefi_2021_US)* 1000 

antibiotic_prescription_rate_2021_US<- antibiotic_prescription_rate_2021_US%>% 
  rename(`Antibiotic Prescription Rate 2021 US`= `Antibiotic Claims`)  

antibiotic_prescription_rate_2021_US<-antibiotic_prescription_rate_2021_US%>% #(value=376.3)
  mutate(`Antibiotic Prescription Rate 2021 US`=round(`Antibiotic Prescription Rate 2021 US`,1))


#SECTION 2

# Table 2. Top 5 Specialties; variables:(Prscrbr_Type,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc)

# excluding Nurse Practitioner and Physician Assistant

specialt_name_2021_US <- prscbr_summary_2021_US %>% #(n=513650)
  select(Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 

# total distinct specialties and their counts

specialty_count_1_2021_US <- specialt_name_2021_US %>% #(n=127)
  count(Prscrbr_Type_new, name = "Specialty_Counts") #(nurse and physician assistant were excluded)

specialty_count_1a_2021_US <-specialty_count_1_2021_US  %>% #(n=513650)
  summarize(total_spcl_count = sum(Specialty_Counts,na.rm = TRUE))


Top_5_specl_2021_US <- specialt_name_2021_US %>%
  group_by(Prscrbr_Type_new) %>%
  left_join(specialty_count_1_2021_US ,by= "Prscrbr_Type_new") %>% 
  filter(Specialty_Counts >= 100) %>%
  filter(Prscrbr_Type_new %in% c("Dentistry","Infectious Disease","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology"))%>% 
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000
  ) %>%
 # arrange(desc(AB_Prescp_rate)) %>%
 # top_n(5) %>% 
  left_join(specialty_count_1_2021_US ,by= "Prscrbr_Type_new") %>% 
  relocate(Specialty_Counts,.after = Prscrbr_Type_new) %>% 
  relocate(Tot_Benes, .after = Antbtc_Tot_Clms) %>% 
  mutate(AB_Prescp_rate=round(AB_Prescp_rate)) %>% 
  rename(`Specialty US 21` =Prscrbr_Type_new,
         `Number of specialists US 21`= Specialty_Counts,
         `Number of antibiotic claims US 21`=Antbtc_Tot_Clms,
         `Number of total beneficiaries US 21 `=Tot_Benes,
         `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 21`= AB_Prescp_rate)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

#2022


AS_CMS_2022_US <-"C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2022_US.csv"
AS_CMS_2022_US <- read.csv(AS_CMS_2022_US)

#1332309

# Data Cleaning and transformation (missing values,blanks,duplicates,mutations,pivoting,binding, summarize, grouping) and
# calculating counts required for tables

Prescribers_F_2022_US <- sum(is.na(AS_CMS_2022_US$Prscrbr_First_Name))       # 0
Prescribers_F_B_2022_US <- sum((AS_CMS_2022_US$Prscrbr_First_Name == ""))    # 1 blanks
Prescribers_L_2022_US <- sum(is.na(AS_CMS_2022_US$Prscrbr_Last_Org_Name)) # 0
Prescribers_L_B_2022_US <- sum((AS_CMS_2022_US$Prscrbr_Last_Org_Name == "")) # 0
duplicates_NPI_2022_US <- AS_CMS_2022_US$Prscrbr_NPI[duplicated(AS_CMS_2022_US$Prscrbr_NPI)] # empty

# Calculating total No.of Prescribers (n= 1332309); variables :Prscrbr_First_Name, Prscrbr_Last_Org_Name

Total_prescribers_2022_US <- AS_CMS_2022_US %>% 
  summarise(Total_Prescribers_2022 = sum(!is.na(Prscrbr_First_Name)))

#num_rows <- dim(Total_prescribers_2022)[1]

Total_prescribers_new_2022_US <- Total_prescribers_2022_US %>% #n= 1,332,309
  mutate(Total_prescribers_2022_US = format(Total_prescribers_2022_US, big.mark=","))# formatted to include comma 


prscbr_summary_2022_US <- AS_CMS_2022_US %>% # (n= 1332309--- n= 725763)
  select(Prscrbr_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms) %>% 
  mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),#due to presence of commas after converting to numeric those values are replaced with NAs
         Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
                                     T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # (n= 601549) excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) # (n= 4997) excluded values < 11 which are assumed to be suppressed values
#----------------------------------------------------------------------------------------------------------------
#A. No.of Prescribers; variable:Prscrbr_First_Name (n=725,763)

Prescribers_2022_US <- prscbr_summary_2022_US %>% 
  summarise(Prescribers = sum(!is.na(Prscrbr_First_Name)))

Prescribers_new_2022_US <- Prescribers_2022_US %>% 
  mutate(Prescribers = format(Prescribers, big.mark=","))# formatted to include common in numbers wherever required

# This calculation is ignored for revised report: calculating percentage of prescribers from total prescribers after excluding the ones with <11 antibiotic claims

# percentage_of_prescribers_2022_US <- (Prescribers_2022_US / Total_prescribers_2022) * 100
# 
# percent_of_prescribers_2022_US <-percentage_of_prescribers_2022_US%>% 
#   mutate(percent_prescribers=paste0(round(Prescribers), "%"))
#----------------------------------------------------------------------------------------------------------------
#B.No. of Specialties; variable: Prscrbr_Type 

blank_count_spcl_2022_US <- sum(prscbr_summary_2022_US$Prscrbr_Type == "") # 1 blanks
missing_spcls_2022_US <- sum(is.na(prscbr_summary_2022_US$Prscrbr_Type)) # 0 missing

tot_specialties_1_2022_US <- prscbr_summary_2022_US %>% #(n=144) # before collapsing specialties
  summarize(Specialties = n_distinct(Prscrbr_Type))

# collapsing specialties
prscbr_summary_2022_US <- prscbr_summary_2022_US %>% 
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


tot_specialties_2_2022_US <- prscbr_summary_2022_US %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=124) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

tot_specialties_names_2022_US <- prscbr_summary_2022_US %>%
  distinct(Prscrbr_Type_new)
#----------------------------------------------------------------------------------------------------------------
#C. No. of Beneficiaries (n=6626763) ; variable :Tot_Benes, Counts fewer than 11 are suppressed and are indicated by a blank.(inconsitencies- commas, blanks)

blank_count_bene_2022_US <- sum(AS_CMS_2022_US$Tot_Benes == "") # 143176 blanks = suppressed
missing_count_bene_2022_US<- sum(is.na(AS_CMS_2022_US$Tot_Benes)) # 0 missing

Total_benefi_2022_US <- prscbr_summary_2022_US %>% # 155352840
  summarize(Beneficiaries = sum(Tot_Benes))

Total_benefi_new_2022_US <- Total_benefi_2022_US %>% # 155,352,840
  mutate(Beneficiaries = format(Beneficiaries, big.mark=","))

#Notes by Analyst (QJ):: 7030 blanks(suppressed) were in raw dataset been converted to missing and later to numeric 5 as per requirements,and are excluded from study(N= 229(no.of blanks in benes after excl 0,1); n= 32428( after excl 0,1)-229 = 32199)
#----------------------------------------------------------------------------------------------------------------

#D. No. of total antibiotic claims(n=2423316); variable :Antbtc_Tot_Clms

#The Antbtc_Tot_Clms is suppressed when Antbtc_Tot_Clms is between 1 and 10.
#A blank indicates the value is suppressed.

blank_count_Anbtcclms_2022_US <- sum(AS_CMS_2022_US$Antbtc_Tot_Clms == "") #388739
missing_count_Anbtcclms_2022_US <- sum(is.na(AS_CMS_2022_US$Antbtc_Tot_Clms)) # 0

Total_Anbtcclms_2022_US <- prscbr_summary_2022_US %>% #(59511221)
  summarize(`Antibiotic Claims` = sum(Antbtc_Tot_Clms,na.rm = TRUE))

Total_Anbtcclms_new_2022_US <- Total_Anbtcclms_2022_US %>% # 59,511,221
  mutate(`Antibiotic Claims` = format(`Antibiotic Claims`, big.mark=","))# formatted to include common in numbers wherever required

#F.Overall Antibiotic Prescription rate for PA (after excluding suppressed values )
#calculation: (Total_Anbtcclms_2022 / Total_benefi_2022)* 1000 (No. of outpatient antibiotic prescriptions per 1,000 
#beneficiaries)

antibiotic_prescription_rate_2022_US <- (Total_Anbtcclms_2022_US / Total_benefi_2022_US)* 1000 

antibiotic_prescription_rate_2022_US <- antibiotic_prescription_rate_2022_US %>% 
  rename(`Antibiotic Prescription Rate 2022 US`= `Antibiotic Claims`)  

antibiotic_prescription_rate_2022_US <-antibiotic_prescription_rate_2022_US %>% #(value=383.07-- 383.1)
  mutate(`Antibiotic Prescription Rate 2022 US`=round(`Antibiotic Prescription Rate 2022 US`,1))


#SECTION 2

# Table 2. Top 5 Specialties; variables:(Prscrbr_Type,Tot_Clms,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc)

# excluding Nurse Practitioner and Physician Assistant

specialt_name_2022_US <- prscbr_summary_2022_US %>% #(n=521047)
  select(Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms,Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Gndr,Prscrbr_Type_new) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant"))) 

# total distinct specialties and their counts

specialty_count_1_2022_US <- specialt_name_2022_US %>% #(n=122)
  count(Prscrbr_Type_new, name = "Specialty_Counts") #(nurse and physician assistant were excluded)

specialty_count_1a_2022_US <-specialty_count_1_2022_US  %>% #(n=521047)
  summarize(total_spcl_count = sum(Specialty_Counts,na.rm = TRUE))


Top_5_specl_2022_US <- specialt_name_2022_US %>%
  group_by(Prscrbr_Type_new) %>%
  left_join(specialty_count_1_2022_US ,by= "Prscrbr_Type_new") %>% 
  filter(Specialty_Counts >= 100) %>%
  filter(Prscrbr_Type_new %in% c("Dentistry","Infectious Disease","Oral and Maxillofacial Surgery","Plastic and Reconstructive Surgery","Urology"))%>% 
  summarise(
    Tot_Benes = sum(Tot_Benes),
    Antbtc_Tot_Clms = sum(Antbtc_Tot_Clms),
    AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000
  ) %>%
  #arrange(desc(AB_Prescp_rate)) %>%
  #top_n(5) %>% 
  left_join(specialty_count_1_2022_US ,by= "Prscrbr_Type_new") %>% 
  relocate(Specialty_Counts,.after = Prscrbr_Type_new) %>% 
  relocate(Tot_Benes, .after = Antbtc_Tot_Clms) %>% 
  mutate(AB_Prescp_rate=round(AB_Prescp_rate)) %>% 
  rename(`Specialty US 22` =Prscrbr_Type_new,
         `Number of specialists US 22`= Specialty_Counts,
         `Number of antibiotic claims US 22`=Antbtc_Tot_Clms,
         `Number of total beneficiaries US 22 `=Tot_Benes,
         `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 22`= AB_Prescp_rate)


#------------------------------------------------------------------------------------------------------------------------------------------

### Data preparation for plots

##### USA #######

# merging --- selecting req cols only----

Top_5_specl_2020_2022_new_US <- cbind(Top_5_specl_2020_US,Top_5_specl_2021_US,Top_5_specl_2022_US) %>%
  rename(`SPECIALTY US (2020)` = `Specialty US 20`,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2020)` = `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 20` ,
         `SPECIALTY US (2021)` = `Specialty US 21`,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2021)`=  `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 21`,
         `SPECIALTY US (2022)` = `Specialty US 22`,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2022)` = `Antibiotic prescriptions per 1,000 beneficiaries,Rate US 22`) 

trend_top_5_US <- Top_5_specl_2020_2022_new_US %>% 
  select(Specialties=`SPECIALTY US (2020)`,5,10,15) # removed col 1

trend_rate_all_US <- trend_top_5_US  %>% 
  pivot_longer(cols= `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2020)`: `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2022)`,
               names_to = "Prescription_year",values_to = "Rate") %>% 
  mutate(Year = c(2020,2021,2022,2020,2021,2022,2020,2021,2022,2020,2021,2022,2020,2021,2022),
         Rate = round(Rate, 0))

# Specialties = c("Dentistry","Dentistry","Dentistry","Infectious Disease","Infectious Disease","Infectious Disease",
#                          "Oral and Maxillofacial Surgery","Oral and Maxillofacial Surgery","Oral and Maxillofacial Surgery",
#                          "Plastic and Reconstructive Surgery","Plastic and Reconstructive Surgery","Plastic and Reconstructive Surgery",
#                          "Urology","Urology","Urology"))


trend_rate_all_US$Group <-   "United States"


################ combined analysis across 3 years ##########################

###### calculating Mean for all three years ####
mean_ab_rate_20_22_US <- trend_top_5_US %>% 
  rowwise() %>% 
  mutate(Mean_AB_rate_2020_22_US = mean(c_across(c(`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2020)`,`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2021)`,`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE US (2022)`))),
         Mean_AB_rate_2020_22_US = round(Mean_AB_rate_2020_22_US))


trend_top_5_state_20_22_US <- mean_ab_rate_20_22_US

trend_rate_all_state_20_22_US <- trend_top_5_state_20_22_US  %>% 
  pivot_longer(cols= Mean_AB_rate_2020_22_US,
               names_to = "Mean_2020_22",values_to = "Rate") %>% 
  mutate(Rate = round(Rate, 0)) %>% 
  select(1,5,6)

trend_rate_all_state_20_22_US$Group <-   "United States"







#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

####### STATE #####

#### Note: Make sure the state data is already loaded in the environment #########

# `SPECIALTY (2022)` = specialty,
# Top_5_specl_2022 <- Top_5_specl_2022 %>% 
#   select(`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2022)`= `antibiotic prescriptions per 1,000 beneficiaries,Rate`)

Top_5_specl_2020_2022_state <- cbind(Top_5_specl_2020,Top_5_specl_2021, Top_5_specl_2022) %>%
  rename(`Specialties` = specialty,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2020)` = `Antibiotic prescriptions per 1,000 beneficiaries,Rate` ,
         `SPECIALTY (2021)` = SPECIALTY,
         `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2021)`=  `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE`) %>% 
  select(1,5,10,12)



trend_top_5_state <- Top_5_specl_2020_2022_state

trend_rate_all_state <- trend_top_5_state  %>% 
  pivot_longer(cols= `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2020)`: `ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2022)`,
               names_to = "Prescription_year",values_to = "Rate") %>% 
  mutate(Year = c(2020,2021,2022,2020,2021,2022,2020,2021,2022,2020,2021,2022,2020,2021,2022),
         Rate = round(Rate, 0))

trend_rate_all_state$Group <-   "Pennsylvania"

################ combined analysis across 3 years ##########################

###### calculating Mean for all three years ####

mean_ab_rate_20_22 <- Top_5_specl_2020_2022_state %>% 
  rowwise() %>% 
  mutate(Mean_AB_rate_2020_22 = mean(c_across(c(`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2020)`,`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2021)`,`ANTIBIOTIC PRESCRIPTIONS per 1,000 BENEFICIARIES,RATE (2022)`))),
         Mean_AB_rate_2020_22 = round(Mean_AB_rate_2020_22))


trend_top_5_state_20_22 <- mean_ab_rate_20_22

trend_rate_all_state_20_22 <- trend_top_5_state_20_22  %>% 
  pivot_longer(cols= Mean_AB_rate_2020_22,
               names_to = "Mean_2020_22",values_to = "Rate") %>% 
  mutate(Rate = round(Rate, 0)) %>% 
  select(1,5,6)

trend_rate_all_state_20_22$Group <-   "Pennsylvania"
  # Customize colors as needed

# ---------------------------------------------------------------------------------------------------------------------------------------------------
##### STATE & USA ######

# individual data for each year
rate_trend_PA_US<- rbind(trend_rate_all_state,trend_rate_all_US)

rate_trend_PA_US_2020 <- rate_trend_PA_US %>% 
  filter(Year == 2020)

rate_trend_PA_US_2021 <- rate_trend_PA_US %>% 
  filter(Year == 2021)

rate_trend_PA_US_2022 <-  rate_trend_PA_US %>% 
  filter(Year == 2022)

rate_trend_PA_US$Specialties <- str_wrap(rate_trend_PA_US$Specialties, width= 5)




## combined for all 3 years  , please running below 
rate_trend_PA_US_20_22<- rbind(trend_rate_all_state_20_22,trend_rate_all_state_20_22_US)


#------------------------------------------------------------------------------------------------------------------------------------------------------

######### PLOTS 2020, 2021, 2022 ###########


######### Combined: Mean antibiotic Prescribing rates (2020-2022) ############

ggplot(rate_trend_PA_US_20_22, aes(x = Specialties, y = Rate, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       #title = " Top 5 specialties: PA rate vs US rate, 2020",
       x = "Specialties",
       y = "Mean Antibiotic Prescription Rate per 1,000 Beneficiaries") +
  #facet_grid(~ Year)+
  scale_y_continuous(label = comma, breaks = seq(0, 2400, by = 200), limits = c(0, 2400)) +
  scale_x_discrete(labels = c("Dentistry",
                              "Infectious Disease",
                              "Oral and
      Maxillofacial 
   Surgery", 
   "Plastic and
      Reconstructive 
   Surgery",
   "Urology"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, size = 14,color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust= 3,size = 14),
        legend.text = element_text(size = 14, color = "black"),
        #strip.text = element_text(face = "bold", size= 12),
        panel.grid = element_blank())+
  geom_text(aes(label = comma(Rate)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 5)+
  geom_point(aes(y= 50, shape = Group, fill = Group),
             position = position_dodge(width = 0.9), 
             size = 4, 
             color = "black", stroke = 1.2) +
  scale_shape_manual(values = c("United States" = 21, "Pennsylvania" = 22),
                     name = "Legend") +
  # scale_fill_manual(values = c("National" = "#1f77b4", "Pennsylvania" = "#ffbf00"),
  #                   name = "Geographic Region") +
  scale_fill_manual(values = c(  "#d0e1f2", "#4a90e2"))+
  guides(
    fill = guide_legend(override.aes = list(shape = c(22, 21), size = 4)),  
    shape = "none",
   # color = guide_legend(override.aes = list(shape = c(8, 15), size = 6)),  
    linetype = guide_legend(override.aes = list(color = c("black", "darkgrey"), size = 1)))

# ALT TEXT:
# Bar chart comparing mean antibiotic prescription rates in Pennsylvania and 
# the United States for 2020-2022 across five specialties: Dentistry, Infectious Disease,
# Oral and Maxillofacial Surgery, Plastic and Reconstructive Surgery, and Urology. 
# Pennsylvania rates were higher than national rates in Dentistry and Oral and Maxillofacial Surgery.

# Facet (all three years)

ggplot(rate_trend_PA_US, aes(x = Specialties, y = Rate, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       #title = " Top 5 specialties: PA rate vs US rate, 2020",
       x = "Specialties",
       y = "Antibiotic Prescription Rate") +
  facet_grid(~ Year)+
  scale_y_continuous(breaks = seq(0, 2500, by = 100)) +
  scale_x_discrete(labels = c("Dentistry",
                              "Infectious Disease",
                              "Oral and
      Maxillofacial 
   Surgery", 
   "Plastic and
      Reconstructive 
   Surgery",
   "Urology"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size= 12),
        panel.grid = element_blank())+
  geom_text(aes(label = Rate), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 8)+
  scale_fill_manual(values = c("#d0e1f2", "#4a90e2"))  # Customize colors as needed




















# Individual

#2020

ggplot(rate_trend_PA_US_2020, aes(x = Specialties, y = Rate, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       title = " Top 5 specialties: PA rate vs US rate, 2020",
       x = "Specialties",
       y = "Antibiotic Prescription Rate") +
  #geom_line(data = rate_trend_new, aes(x = Year, y = Rate, color=Group, group = Group))+
  #geom_line(aes(color=Group, group = Group),size = 0.8)+
  #geom_point(data = rate_trend_new,aes(color= Group))+
  scale_y_continuous(breaks = seq(0, 2500, by = 100)) +
  scale_x_discrete(labels = c("Dentistry",
                              "Infectious Disease",
                              "Oral and Maxillofacial Surgery", 
                              "Plastic and Reconstructive Surgery",
                              "Urology"))+
  #facet_wrap(vars(Setting))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_fill_viridis_d()+
  geom_text(aes(label = Rate), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 3)


#2021

ggplot(rate_trend_PA_US_2021, aes(x = Specialties, y = Rate, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       title = "Top 5 specialties: PA rate vs US rate, 2021",
       
       x = "Specialties",
       y = "Antibiotic Prescription Rate") +
  #geom_line(data = rate_trend_new, aes(x = Year, y = Rate, color=Group, group = Group))+
  #geom_line(aes(color=Group, group = Group),size = 0.8)+
  #geom_point(data = rate_trend_new,aes(color= Group))+
  scale_y_continuous(breaks = seq(0, 2500, by = 100)) +
  scale_x_discrete(labels = c("Dentistry",
                              "Infectious Disease",
                              "Oral and Maxillofacial Surgery", 
                              "Plastic and Reconstructive Surgery",
                              "Urology"))+
  #facet_wrap(vars(Setting))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_fill_viridis_d()+
  geom_text(aes(label = Rate), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 3)


#2022

ggplot(rate_trend_PA_US_2022, aes(x = Specialties, y = Rate, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       title = "Top 5 specialties: PA rate vs US rate, 2022",
       
       x = "Specialties",
       y = "Antibiotic Prescription Rate") +
  #geom_line(data = rate_trend_new, aes(x = Year, y = Rate, color=Group, group = Group))+
  #geom_line(aes(color=Group, group = Group),size = 0.8)+
  #geom_point(data = rate_trend_new,aes(color= Group))+
  scale_y_continuous(breaks = seq(0, 2500, by = 100)) +
  scale_x_discrete(labels = c("Dentistry",
                              "Infectious Disease",
                              "Oral and Maxillofacial Surgery", 
                              "Plastic and Reconstructive Surgery",
                              "Urology"))+
  #facet_wrap(vars(Setting))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_fill_viridis_d()+
  geom_text(aes(label = Rate), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 3)




