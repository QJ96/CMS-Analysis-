
# purpose: Develop a table that describes most frequently prescribed antbiotics by dentists in PA state compared to the USA, 2020-2022


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

---------------------

# Import raw CSV data
  
# state 2020(PA) #( 1244056 ~ 1M records)

Medicare_Part_D_DrugP_2020_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/Drug-provider/Medicare_Part_D_Prescribers_by_Provider_and_Drug_2020.csv"

AS_CMS_DrugP_PA_2020<- read.csv(Medicare_Part_D_DrugP_2020_PA_file)

Medicare_Part_D_DrugP_2021_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/Drug-provider/Medicare_Part_D_Prescribers_by_Provider_and_Drug_2021.csv"

AS_CMS_DrugP_PA_2021<- read.csv(Medicare_Part_D_DrugP_2021_PA_file)

Medicare_Part_D_DrugP_2022_PA_file <-"C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/Drug-provider/Medicare_Part_D_Prescribers_by_Provider_and_Drug_2022.2.csv"

AS_CMS_DrugP_PA_2022<- read.csv(Medicare_Part_D_DrugP_2022_PA_file)


# Clindamycin  -- No other formulations for dentist

clindamycin_1 <- AS_CMS_DrugP_PA_2020 %>%
  filter(str_starts(Gnrc_Name, "Clindamycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

clindamycin_2 <- AS_CMS_DrugP_PA_2021 %>%
  filter(str_starts(Gnrc_Name, "Clindamycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

clindamycin_3 <- AS_CMS_DrugP_PA_2022 %>%
  filter(str_starts(Gnrc_Name, "Clindamycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)


# Penicillin -- No other formulations for dentist

Penicillin_1 <- AS_CMS_DrugP_PA_2020 %>%
  filter(str_starts(Gnrc_Name, "Penicillin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Penicillin_2 <- AS_CMS_DrugP_PA_2021 %>%
  filter(str_starts(Gnrc_Name, "Penicillin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Penicillin_3 <- AS_CMS_DrugP_PA_2022 %>%
  filter(str_starts(Gnrc_Name, "Penicillin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)



# Azithromycin ---- No other formulations for Dentist

Azithromycin_1 <- AS_CMS_DrugP_PA_2020 %>%
  filter(str_starts(Gnrc_Name, "Azithromycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Azithromycin_2 <- AS_CMS_DrugP_PA_2021 %>%
  filter(str_starts(Gnrc_Name, "Azithromycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Azithromycin_3 <- AS_CMS_DrugP_PA_2022 %>%
  filter(str_starts(Gnrc_Name, "Azithromycin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)


# Cephalexin --- No other formulations for Dentist

Cephalexin_1 <- AS_CMS_DrugP_PA_2020 %>%
  filter(str_starts(Gnrc_Name, "Cephalexin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Cephalexin_2 <- AS_CMS_DrugP_PA_2021 %>%
  filter(str_starts(Gnrc_Name, "Cephalexin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)

Cephalexin_3 <- AS_CMS_DrugP_PA_2022 %>%
  filter(str_starts(Gnrc_Name, "Cephalexin")) %>%
  distinct(Prscrbr_Type,Gnrc_Name)




# National (US) # ( 25209729 ~ 25M records)

Medicare_Part_D_DrugP_2020_US_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/Drug-provider/Medicare_Part_D_Prescribers_by_Provider_and_Drug_2020_US.csv"
AS_CMS_DrugP_US_2020<- read.csv(Medicare_Part_D_DrugP_2020_US_file) # RUNNING TIME : 9:08 - 9:19 (11 minutes)

Medicare_Part_D_DrugP_2021_US_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/New folder/Medicare_Part_D_Prescribers_by_Provider_and_Drug_US_2021.csv"
AS_CMS_DrugP_US_2021<- read.csv(Medicare_Part_D_DrugP_2021_US_file) # Running time : 9:20 - 9:31 (11 minutes)

Medicare_Part_D_DrugP_2022_US_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/New folder/Medicare_Part_D_Prescribers_by_Provider_and_Drug_US_2022.csv"
AS_CMS_DrugP_US_2022<- read.csv(Medicare_Part_D_DrugP_2022_US_file) # run time : 9:40 - 10:00 (20 minutes)

# # providers only
# 
# Medicare_Part_D_2020_PA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/Medicare_Part_D_Prescribers_by_Provider_2020.csv"
# 
# AS_CMS_PA_2020<- read.csv(Medicare_Part_D_2020_PA_file)

#---------------------------------------------------------------------------------------------------------------------------------------------


 # STATE (PA)
 # 2020

merge_drug_provider_20 <- AS_CMS_DrugP_PA_2020 %>% #(n= 7181)
  filter(Prscrbr_Type == "Dentist") %>% 
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  #filter(Brnd_Name == "Amoxicillin-Clavulanate Potass") %>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen" ,
                            "Acetaminophen With Codeine","Hydrocodone/Acetaminophen")))

clindamycin <- merge_drug_provider_20 %>%
  filter(str_starts(Gnrc_Name, "Clindamycin")) %>%
  distinct(Gnrc_Name)


# running above lines produces the all amoxicillin counts(dentists who wrote both amoxi & combn) in the data
# Amoxicillin - n=4145 ( can see in gnrc_names_list_20)


#---------------------------------------------------------------------------------------------------
# The below statements are to verify the excluded amoxi counts (amoxicllin)

# filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>%
# mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
#                             T~Gnrc_Name)) %>% 
# select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>%
#---------------------------------------------------------------------------------------------------

# run after including above filter statements

# DUPES_2020 <- merge_drug_provider_20 %>%
# group_by(Prscrbr_NPI) %>%
# summarise(n()>1) # only TRUE n=108 (4145(crct count)-- 108(were ex)+4037 (group_by(Antibiotic = Gnrc_Name) ,
                                                                            #n_distinct(Prscrbr_NPI)) == 4145)

#--------------------------------------------------------------------------------------------------


gnrc_names_list_20 <- tabyl(merge_drug_provider_20$Gnrc_Name) %>% #(n= 106)
  adorn_totals() # total : 7181

# ex: 1013087188 NPI prescribed amoxicillin comb (brand name) and amoxicillin 

# QJ : finally decided to take total for all medications (not just top 5 100%)
# Preferred output : percent prescribing 90.6%


  penicillin_2020<- merge_drug_provider_20 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
    mutate(`Mean Duration (Days) PA 20`= (Tot_Day_Suply/Tot_Clms),
           Mean_2_20 = (`Mean Duration (Days) PA 20`/n())) %>%
    adorn_totals()

Top_5_Antibiotics_dentists_PA_2020 <- merge_drug_provider_20 %>% 
  group_by(Antibiotic = Gnrc_Name) %>% 
  summarize(`Providers Prescribing (n) PA 20` = n_distinct(paste(Prscrbr_NPI,Brnd_Name)), # to include all amoxici's
             `Mean Duration (Days) PA 20`= round(mean(Tot_Day_Suply/Tot_Clms),1)) %>%
  arrange(desc(`Providers Prescribing (n) PA 20`)) %>% # calculates percent for all 94 antibiotics not top 5
  mutate(
    `Percent Prescribing PA 20` = round((`Providers Prescribing (n) PA 20` / sum(`Providers Prescribing (n) PA 20`)) * 100,1),
    Antibiotic = case_when(Antibiotic == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotic)) %>%
  #adorn_totals() %>% # Dentists = 7058
  top_n(5, wt = `Providers Prescribing (n) PA 20` ) %>% # without wt takes last column, messed order
  select(1,2,4,3)  
  
# convert Antibiotic column to factor and then reorder the values

  Top_5_Antibiotics_dentists_PA_2020 <- Top_5_Antibiotics_dentists_PA_2020 %>%
    mutate(Antibiotic = factor(Antibiotic, levels = c("Amoxicillin","Clindamycin","Penicillin V Potassium","Azithromycin","Cephalexin"))) %>% 
    arrange(Antibiotic)
    
    
                                                    
# %>%
# adorn_totals()


# if goal:percent prescribing - 100%
# approach: group_by, summarize, arrange, get top 5 , then calculate percentage


Top_5_Antibiotics_dentists_PA_2020_QJ <- merge_drug_provider_20 %>%
  group_by(Antibiotic = Gnrc_Name) %>%
  summarize(
    `Providers Prescribing (n) PA 20` = n_distinct(Prscrbr_NPI),
    `Mean Duration (Days) PA` = round(mean(Tot_Day_Suply / Tot_Clms), 1)) %>%
  arrange(desc(`Providers Prescribing (n) PA 20`)) %>%
  top_n(5, wt = `Providers Prescribing (n) PA 20`) %>%
  mutate(
    `Percent Prescribing PA 20` = round(`Providers Prescribing (n) PA 20` / sum(`Providers Prescribing (n) PA 20`) * 100,1)
  )%>%
  select(1,2,4,3) %>% 
  adorn_totals()
  
print(Top_5_Antibiotics_dentists_PA_2020)

#-------2021-------

merge_drug_provider_21 <- AS_CMS_DrugP_PA_2021 %>% #(n= 8142)
  filter(Prscrbr_Type == "Dentist") %>% 
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  #filter(Brnd_Name == "Amoxicillin-Clavulanate Potass") %>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen"))) 
 

# run upto mutate -- run gnrc_names_list -- open and desc  count (n)-- look for interuupting medications to antibiotics-- then filter out

#---------------------------------------------------------------------------------------------------
# filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>%
# mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
#                             T~Gnrc_Name))
# select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>%
#---------------------------------------------------------------------------------------------------

# DUPES_2021 <- merge_drug_provider_21 %>%
# group_by(Prscrbr_NPI) %>%
# summarise(n()>1) # only TRUE n=129(4371(crct count)-- 129(were ex)+4242 == 4371)

#--------------------------------------------------------------------------------------------------


gnrc_names_list_21 <- tabyl(merge_drug_provider_21$Gnrc_Name) %>%  #(n= 94)
adorn_totals() #8142

penicillin_2021<- merge_drug_provider_21 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
  mutate(`Mean Duration (Days) PA 21`= (Tot_Day_Suply/Tot_Clms),
         Mean_2_21 = (`Mean Duration (Days) PA 21`/n())) %>% 
  adorn_totals()


Top_5_Antibiotics_dentists_PA_2021 <- merge_drug_provider_21 %>%
  group_by(Antibiotic = Gnrc_Name) %>%
  summarize(
    `Providers Prescribing (n) PA 21` = n_distinct(paste(Prscrbr_NPI,Brnd_Name)),
    `Mean Duration (Days) PA 21` = round(mean(Tot_Day_Suply / Tot_Clms), 1)
  ) %>%
  arrange(desc(`Providers Prescribing (n) PA 21`)) %>%
  #adorn_totals() #Dentists = 7977
  mutate(
    `Percent Prescribing PA 21` = round(`Providers Prescribing (n) PA 21` / sum(`Providers Prescribing (n) PA 21`) * 100,1),
    Antibiotic = case_when(Antibiotic == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotic)
  )%>%
  top_n(5, wt = `Providers Prescribing (n) PA 21`) %>%  # get top 5 antibiotics or slice_head(n = 5)
  select(1,2,4,3)
 # %>%
 # adorn_totals()


print(Top_5_Antibiotics_dentists_PA_2021)

#---PA:2022---------

  merge_drug_provider_22 <- AS_CMS_DrugP_PA_2022 %>% #(n= 7960)
  filter(Prscrbr_Type == "Dentist") %>% 
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  #filter(Brnd_Name == "Amoxicillin-Clavulanate Potass") %>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen", "Fluoride (Sodium)","Hydrocodone/Acetaminophen","Acetaminophen With Codeine"))) 
 

# run upto mutate -- run gnrc_names_list -- open and desc n-- look for interuupting medications to antibiotics-- then filter out


#---------------------------------------------------------------------------------------------------
 # filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>% 
 # mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
 #                             T~Gnrc_Name))
 #select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>% 
#---------------------------------------------------------------------------------------------------

  # DUPES_2022 <- merge_drug_provider_22 %>% 
  # group_by(Prscrbr_NPI) %>% 
  # summarise(n()>1) # only TRUE n=176 (4426(crct count)-- 176(were ex)+4250 == 4426)

#--------------------------------------------------------------------------------------------------


gnrc_names_list_22 <- tabyl(merge_drug_provider_22$Gnrc_Name) %>%  #(n= 96)
  adorn_totals() #7960


penicillin_2022<- merge_drug_provider_22 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
  mutate(`Mean Duration (Days) PA 22`= (Tot_Day_Suply/Tot_Clms),
         Mean_2_22 = (`Mean Duration (Days) PA 22`/n())) %>% 
  adorn_totals()



Top_5_Antibiotics_dentists_PA_2022 <- merge_drug_provider_22 %>%
  group_by(Antibiotic = Gnrc_Name) %>%
  summarize(
    `Providers Prescribing (n) PA 22` = n_distinct(paste(Prscrbr_NPI, Brnd_Name)),
    `Mean Duration (Days) PA 22` = round(mean(Tot_Day_Suply / Tot_Clms), 1)
  ) %>%
  arrange(desc(`Providers Prescribing (n) PA 22`)) %>%
  #adorn_totals() #Dentists = 7782
  mutate(
    `Percent Prescribing PA 22` = round(`Providers Prescribing (n) PA 22` / sum(`Providers Prescribing (n) PA 22`) * 100,1),
    Antibiotic = case_when(Antibiotic == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotic))%>%
  top_n(5, wt = `Providers Prescribing (n) PA 22`) %>%  # get top 5 antibiotics or slice_head(n = 5)
  select(1,2,4,3)

# %>%
# adorn_totals()


print(Top_5_Antibiotics_dentists_PA_2021)


#Notes:

#n_distinct(Prscrbr_NPI) ----modified to----- n_distinct(paste(Prscrbr_NPI, Brnd_Name)):
 
# As same prescribers prescribed amoxicillin which have different brand name (Amoxicillin-Clavulanate Potass,Amoxicillin) but same generic name
# when grouped by generic and summarized distinct NPI -- resulted in excluding second occurence of amoxicillin (n=193)-- resolved by 
# n_distinct(paste(Prscrbr_NPI, Brnd_Name))

#(4426(crct count)-- 176(were ex)+4250 == 4426)                                                                          


#-------------- code checks -------------------------------------------------------------------------


# prscbr_summary_2020_new <- AS_CMS_PA_2020 %>% #(n=4751)
#   select(Prscrbr_NPI =PRSCRBR_NPI,Prscrbr_Last_Org_Name,Prscrbr_First_Name,Prscrbr_Gndr,Prscrbr_Ent_Cd,Prscrbr_State_Abrvtn,
#          Prscrbr_RUCA,Prscrbr_RUCA_Desc,Prscrbr_Type,Tot_Clms,Tot_Benes,Antbtc_Tot_Clms) %>% 
#   mutate(Tot_Clms = as.numeric(str_remove_all(Tot_Clms, ",")),
#          # due to presence of commas after converting to numeric those values are replaced with NAs
#          Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes),  #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
#          Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
#          Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
#          Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
#          Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
#          Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,
#                                      T~Antbtc_Tot_Clms)) %>% 
#   # Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry", T~ Prscrbr_Type))%>% 
#   filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% 
#   filter(Prscrbr_Type == "Dentist") %>% 
#   select()

#  filter(Gnrc_Name == "Amoxicillin") %>% 
# mutate(mean_duration= mean(Tot_Day_Suply, na.rm = TRUE))

# mutate(Drug_count = n()) %>% 
# filter(Gnrc_Name == "Amoxicillin")
# 
# duplicates <- merge_drug_provider_20$Prscrbr_NPI[duplicated(merge_drug_provider_20$Prscrbr_NPI)]
# 
# group_by(Gnrc_Name) %>% 
# summarize(Providers Prescribing =) 
# 
# # if merged
# not_antibiotics <- merge_drug_provider_20 %>% # n= 91
#   filter((is.na(Antbtc_Tot_Clms)))
# 
# gnrc_names_list <- tabyl(no_antibiotics$Gnrc_Name)
#   
# gnrc_names_list_2 <- tabyl(merge_drug_provider_20$Gnrc_Name)

#------------------------------------------------------------------------


#NATIONAL DATA- US

US_drug_provider_20 <- AS_CMS_DrugP_US_2020 %>% #(n= 149699)
  filter(Prscrbr_Type == "Dentist") %>% 
  # left_join(prscbr_summary_2020_new, by = "Prscrbr_NPI") %>%
  # filter(!(is.na(Antbtc_Tot_Clms))) %>% #(n=91 ex --- 9218)
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen","Hydrocodone/Acetaminophen","Acetaminophen With Codeine")))  

  
  #filter(Gnrc_Name %in% c("Amoxicillin", "Clindamycin Hcl" ,"Penicillin V Potassium","Cephalexin","Azithromycin"))

#---------------------------------------------------------------------------------------------------
# filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>% 
# mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
#                             T~Gnrc_Name))
#select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>% 
#---------------------------------------------------------------------------------------------------

  # DUPES_US_2020 <- US_drug_provider_20 %>%
  # group_by(Prscrbr_NPI) %>%
  # summarise(n()>1) # only TRUE n=2694 (92744(crct count)-- 2694(were ex)+90050 == 92744)

#--------------------------------------------------------------------------------------------------



penicillin_US_2020<- US_drug_provider_20 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
  mutate(`Mean Duration (Days) US 20`= (Tot_Day_Suply/Tot_Clms),
          Mean_2_20 = (`Mean Duration (Days) US 20`/n())) %>% 
  adorn_totals()


gnrc_names_list_US_20 <- tabyl(US_drug_provider_20$Gnrc_Name) %>%  #(n= 375)
adorn_totals() # n= 149699 (this list provides total amoxicillin count)

Top_5_Antibiotics_dentists_US_2020 <- US_drug_provider_20 %>% 
  group_by(Antibiotics = Gnrc_Name) %>% 
  summarize( `Providers Prescribing (n) US 20` = n_distinct(paste(Prscrbr_NPI,Brnd_Name)),
             `Mean Duration (Days) US`= round(mean(Tot_Day_Suply/Tot_Clms),1)) %>% 
  arrange(desc(`Providers Prescribing (n) US 20`)) %>%
  #adorn_totals() #Dentists = 146572
  mutate(`Percent Prescribing US 20` = round(`Providers Prescribing (n) US 20`/sum(`Providers Prescribing (n) US 20`)*100,1),
         Antibiotics = case_when(Antibiotics == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotics)) %>% 
  top_n(5, wt = `Providers Prescribing (n) US 20`) %>% 
  select(1,2,4,3)  
  #adorn_totals()


# convert Antibiotic column to factor and then reorder the values

Top_5_Antibiotics_dentists_US_2020 <- Top_5_Antibiotics_dentists_US_2020 %>%
  mutate(Antibiotics = factor(Antibiotics, levels = c("Amoxicillin","Clindamycin","Penicillin V Potassium","Azithromycin","Cephalexin"))) %>% 
  arrange(Antibiotics)

# PA & US

PA_US_2020 <-  cbind(Top_5_Antibiotics_dentists_PA_2020,Top_5_Antibiotics_dentists_US_2020) %>% 
  select(1,2,3,4,6,7,8)

  # %>% 
  # adorn_totals()

PA_US_2020_b<- PA_US_2020 %>% 
  select(1,3,6)

PA_US_2020_plot <- PA_US_2020_b  %>% 
  pivot_longer(cols= `Percent Prescribing PA 20`:`Percent Prescribing US 20`,
               names_to = "level",values_to = "Prescribing_Percentage") %>% 
  mutate(Percent_2020 = paste0(round(Prescribing_Percentage, 2),"%"))

colnames(PA_US_2020)

# q:line 354 rounding?


#2021

US_drug_provider_21 <- AS_CMS_DrugP_US_2021 %>% #(n= 164587)
  filter(Prscrbr_Type == "Dentist") %>% 
  # left_join(prscbr_summary_2020_new, by = "Prscrbr_NPI") %>%
  # filter(!(is.na(Antbtc_Tot_Clms))) %>% #(n=91 ex --- 9218)
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen","Hydrocodone/Acetaminophen","Acetaminophen With Codeine"))) 
  
  
#filter(Gnrc_Name %in% c("Amoxicillin", "Clindamycin Hcl" ,"Penicillin V Potassium","Cephalexin","Azithromycin"))


#---------------------------------------------------------------------------------------------------
# filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>% 
# mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
#                             T~Gnrc_Name))
# select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>% 
#---------------------------------------------------------------------------------------------------

# DUPES_US_2021 <- US_drug_provider_21 %>%
# group_by(Prscrbr_NPI) %>%
# summarise(n()>1) # only TRUE n=3518 (101518(crct count)-- 3518(were ex)+98000 == 101518)

#--------------------------------------------------------------------------------------------------



gnrc_names_list_US_21 <- tabyl(US_drug_provider_21$Gnrc_Name) %>%  #(n= 348)
adorn_totals() # 164587  (amoxi count - 101518)



penicillin_US_2021<- US_drug_provider_21 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
  mutate(`Mean Duration (Days) US 21`= (Tot_Day_Suply/Tot_Clms),
         Mean_2_21 = (`Mean Duration (Days) US 21`/n())) %>% 
  adorn_totals()




Top_5_Antibiotics_dentists_US_2021 <- US_drug_provider_21 %>% 
  group_by(Antibiotics = Gnrc_Name) %>% 
  summarize( `Providers Prescribing (n) US 21` = n_distinct(paste(Prscrbr_NPI,Brnd_Name)),
             `Mean Duration (Days) US 21`= round(mean(Tot_Day_Suply/Tot_Clms),1)) %>% 
  arrange(desc(`Providers Prescribing (n) US 21`)) %>%
 # adorn_totals() %>% # 160427
  mutate(`Percent Prescribing US 21` = round((`Providers Prescribing (n) US 21`/sum(`Providers Prescribing (n) US 21`)*100),1),
         Antibiotics = case_when(Antibiotics == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotics)) %>% 
  top_n(5, wt = `Providers Prescribing (n) US 21`) %>% 
  select(1,2,4,3)  
# %>% 
# adorn_totals() 

# PA & US

PA_US_2021 <-  cbind(Top_5_Antibiotics_dentists_PA_2021,Top_5_Antibiotics_dentists_US_2021) %>% 
  select(1,2,3,4,6,7,8)

PA_US_2021_b<- PA_US_2021 %>% 
  select(1,3,6)

PA_US_2021_plot <- PA_US_2021_b  %>% 
  pivot_longer(cols= `Percent Prescribing PA 21`:`Percent Prescribing US 21`,
               names_to = "level_21",values_to = "Prescribing_Percentage_21") %>% 
  mutate(Percent_2021 = paste0(round(Prescribing_Percentage_21, 2),"%"))

colnames(PA_US_2021)



#2022


US_drug_provider_22 <- AS_CMS_DrugP_US_2022 %>% #(n= 164587)
  filter(Prscrbr_Type == "Dentist") %>% 
  # left_join(prscbr_summary_2020_new, by = "Prscrbr_NPI") %>%
  # filter(!(is.na(Antbtc_Tot_Clms))) %>% #(n=91 ex --- 9218)
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>% 
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen","Hydrocodone/Acetaminophen","Acetaminophen With Codeine"))) 
  
  

#filter(Gnrc_Name %in% c("Amoxicillin", "Clindamycin Hcl" ,"Penicillin V Potassium","Cephalexin","Azithromycin"))



#---------------------------------------------------------------------------------------------------
# filter(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass","Amoxicillin")) %>% 
# mutate(Gnrc_Name =case_when(Brnd_Name %in% c("Amoxicillin-Clavulanate Potass")~ "amoxicillin",
#                             T~Gnrc_Name))
# select(Prscrbr_NPI,Brnd_Name,Gnrc_Name,Prscrbr_Type,Tot_Clms,Tot_Benes) %>% 
#---------------------------------------------------------------------------------------------------
# 
# DUPES_US_2022 <- US_drug_provider_22 %>%
#   group_by(Prscrbr_NPI) %>%
#   summarise(n()>1) # only TRUE n=4152 (103384(crct count)-- 4152(were ex)+99232 == 103384)


#--------------------------------------------------------------------------------------------------


gnrc_names_list_US_22 <- tabyl(US_drug_provider_22$Gnrc_Name) %>%  #(n= 341)
  adorn_totals() # 236262  (amoxi count - 103384)


penicillin_US_2022<- US_drug_provider_22 %>% 
  filter(Gnrc_Name=="Penicillin V Potassium") %>% 
  select(Gnrc_Name,Tot_Day_Suply,Tot_Clms) %>% 
  mutate(`Mean Duration (Days) US 22`= (Tot_Day_Suply/Tot_Clms),
         Mean_2_22 = (`Mean Duration (Days) US 22`/n())) %>% 
  adorn_totals()




Top_5_Antibiotics_dentists_US_2022 <- US_drug_provider_22 %>% 
  group_by(Antibiotics = Gnrc_Name) %>% 
  summarize( `Providers Prescribing (n) US 22` = n_distinct(paste(Prscrbr_NPI, Brnd_Name)),
             `Mean Duration (Days) US 22`= round(mean(Tot_Day_Suply/Tot_Clms),1)) %>% 
  arrange(desc(`Providers Prescribing (n) US 22`)) %>%
  # adorn_totals() %>% # 160427
  mutate(`Percent Prescribing US 22` = round((`Providers Prescribing (n) US 22`/sum(`Providers Prescribing (n) US 22`)*100),1),
         Antibiotics = case_when(Antibiotics == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotics)) %>% 
  top_n(5, wt = `Providers Prescribing (n) US 22`) %>% 
  select(1,2,4,3)  
# %>% 
# adorn_totals() 

# PA & US

PA_US_2022 <-  cbind(Top_5_Antibiotics_dentists_PA_2022,Top_5_Antibiotics_dentists_US_2022) %>% 
  select(1,2,3,4,6,7,8)

PA_US_2022_b<- PA_US_2022 %>% 
  select(1,3,6)

PA_US_2022_plot <- PA_US_2022_b  %>% 
  pivot_longer(cols= `Percent Prescribing PA 22`:`Percent Prescribing US 22`,
               names_to = "level_22",values_to = "Prescribing_Percentage_22") %>% 
  mutate(Percent_2022 = paste0(round(Prescribing_Percentage_22, 0),"%"))

colnames(PA_US_2022)



##########################  Combined Mean duration days across 3 years #####################################

# DOES NOT WORK
# Mean_state_nat_20_22_new <- Mean_state_nat_20_22 %>% 
# mutate(Mean_duration_PA_20_22 = round(mean(c_across(c(`Mean Duration (Days) PA 20`,`Mean Duration (Days) PA 21`,`Mean Duration (Days) PA 22`))),1),
#        Mean_duration_US_20_22 = round(mean(c_across(c(`Mean Duration (Days) US`,`Mean Duration (Days) US 21`,`Mean Duration (Days) US 22`))),1))

# WORKS

Mean_state_nat_20_22 <- cbind(PA_US_2020,PA_US_2021,PA_US_2022) %>% 
  select(1,4,11,18,7,14,21) 

Mean_state_nat_20_22_new <- Mean_state_nat_20_22 %>%
  mutate(
    Mean_duration_PA_20_22 = round(rowMeans(across(c(`Mean Duration (Days) PA 20`, 
                                                     `Mean Duration (Days) PA 21`, 
                                                     `Mean Duration (Days) PA 22`)), 
                                            na.rm = TRUE), 1),
    Mean_duration_US_20_22 = round(rowMeans(across(c(`Mean Duration (Days) US`, 
                                                     `Mean Duration (Days) US 21`, 
                                                     `Mean Duration (Days) US 22`)), 
                                            na.rm = TRUE), 1))

Mean_duration_PA_US_2020_2022 <- Mean_state_nat_20_22_new  %>% 
  pivot_longer(cols= c(Mean_duration_PA_20_22,Mean_duration_US_20_22),
               names_to = "Mean_duration_2020_22",values_to = "days") %>% 
  mutate(
         Group = c("Pennsylvania","USA","Pennsylvania","USA","Pennsylvania","USA","Pennsylvania","USA","Pennsylvania","USA"))
         # Antibiotic= case_when(Antibiotic == "Clindamycin Hcl" ~ "Clindamycin", T ~ Antibiotic)) 


ggplot(Mean_duration_PA_US_2020_2022, aes(x = Antibiotic, y = days, fill = interaction(Group))) +
  geom_bar(stat= "identity", width = 0.8)+
  geom_text(aes(label = sprintf("%.1f", days)), 
            position = position_stack(vjust = 0.5), 
            #color = "black",
            size = 5)+
  labs(#title = " Top 5 Antibiotics by Dentists: State (PA) vs National (US) , 2020 - 2022",
    y = "Mean duration (days)") +
  # scale_x_discrete(breaks = c('PA 2020','PA 2021','PA 2022'),
  #                  labels = c('2020','2021','2022'))+
  scale_y_continuous(breaks = seq(0,10, by = 2.5), limits = c(0,10)) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("#d0e1f2", "#4a90e2"))  # Customize colors as needed


#### individual -- in the report ######################

ggplot(Mean_duration_PA_US_2020_2022, aes(x = Antibiotic, y = days, fill = interaction(Group))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill= NULL,
       #title = " Top 5 specialties: PA rate vs US rate, 2020",
       x = "Antibiotics",
       y = "Mean duration (days)") +
  #facet_grid(~ Year)+
  scale_y_continuous(breaks = seq(0,9, by =1.5), limits = c(0,9)) +
  # scale_x_discrete(labels = c("Dentistry",
  #                             "Infectious Disease",
  #                             "Oral and
  #     Maxillofacial 
  #  Surgery", 
  #  "Plastic and
  #     Reconstructive 
  #  Surgery",
  #  "Urology"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, size = 14,color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 16, vjust = -1.0),
        axis.title.y = element_text(vjust= 3,size = 16),
        legend.text = element_text(size = 14, color = "black"),
        #strip.text = element_text(face = "bold", size= 12),
        panel.grid = element_blank())+
  geom_text(aes(label = sprintf("%.1f", days)),  # Ensure one decimal place
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 5)+
  scale_fill_manual(values = c("#d0e1f2", "#4a90e2"))  # Customize colors as needed



######################################################################################

state_nat_20_22 <- cbind(PA_US_2020,PA_US_2021,PA_US_2022) %>% 
  select(1,3,6,10,13,17,20)

state_nat_20_22_plot <- state_nat_20_22  %>%
  pivot_longer(cols= c(`Percent Prescribing PA 20`:`Percent Prescribing US 22`),
               names_to = "level",values_to = "Prescribing_Percentage")



# PLOTS


# Revised -- Finalized
state_nat_20_22_plot_a <- state_nat_20_22_plot %>% 
  mutate(level = case_when(
    level == "Percent Prescribing PA 20" ~ "PA 2020",
    level == "Percent Prescribing PA 21" ~ "PA 2021",
    level == "Percent Prescribing PA 22" ~ "PA 2022",
    level == "Percent Prescribing US 20" ~ "US 2020",
    level == "Percent Prescribing US 21" ~ "US 2021",
    level == "Percent Prescribing US 22" ~ "US 2022",
    TRUE ~ level  
  ))



# Ensure 'level' is a factor with the correct order
state_nat_20_22_plot_a$level <- factor(state_nat_20_22_plot_a$level, 
                                       levels = c("PA 2020", "US 2020", "PA 2021", "US 2021", "PA 2022", "US 2022"))

# state_nat_20_22_plot_a <- state_nat_20_22_plot %>%
#   mutate(level = factor(level, levels = c("PA 2020","US 2020","PA 2021","US 2021","PA 2022","US 2022"))) %>% 
#   arrange(level)
# 
# state_nat_20_22_plot_a$Year <- factor(state_nat_20_22_plot_a$Year)



#1

state_nat_20_22_plot_a <- state_nat_20_22_plot_a %>% 
  mutate(x_pos = rep(c(1,1.1,2, 2.1, 3,3.1), times = 5)) # these are overlpapping

#2 changed to--

state_nat_20_22_plot_a <- state_nat_20_22_plot_a %>% 
  mutate(x_pos = rep(c(1,1.8,3,3.8,5,5.8), times = 5))

state_nat_20_22_plot_a <- state_nat_20_22_plot_a %>% 
  group_by(x_pos) %>% 
  mutate(total_percent = sum(Prescribing_Percentage, na.rm = T))

# final output code

ggplot(state_nat_20_22_plot_a, aes(x = x_pos, y = Prescribing_Percentage, fill = Antibiotic)) +
  geom_bar(stat = "identity", width = 0.8, color= "black", size= 0.5) +
  geom_text(aes(label = sprintf("%.1f", Prescribing_Percentage)), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 2.7, vjust = 51, hjust = 7.6) + 
  annotate("text", x = Inf, y = 100, label = "National", size = 2.7, vjust = 51, hjust = 10.4) + 
  annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 2.7, vjust = 51, hjust = 5.0) + 
  annotate("text", x = Inf, y = 100, label = "National", size = 2.7, vjust = 51, hjust = 6) + 
  annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 2.7, vjust = 51, hjust = 2.4) +
  annotate("text", x = Inf, y = 100, label = "National", size = 2.7, vjust = 51, hjust = 1.7) + 
  labs(#title = "Top 5 Antibiotics by Dentists: State (PA) vs National (US), 2020 - 2022",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_x_continuous(breaks = c(1.025, 3.025, 5.025),
                     labels = c('2020', '2021', '2022')) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1.8, hjust =  -1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=10 ),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1)

# dis adv annotate : hjust & vjust are not constant , changes with plot pane drag and release


############# Mean duration days ####################

Mean_duration_20_22_plot_a <- Mean_duration_PA_US_2020_2022 %>% 
  mutate(x_pos = rep(c(1,1.8), times = 5))


ggplot(Mean_duration_20_22_plot_a, aes(x = x_pos, y = days, fill = Antibiotic)) +
  geom_bar(stat= "identity", width = 0.8,color= "black", size= 0.5)+
  geom_text(aes(label = sprintf("%.1f", days)), 
            position = position_stack(vjust = 0.5), 
            #color = "black",
            size = 5)+
  annotate("text", x = Inf, y = 30, label = "Pennsylvania", size = 3, vjust = 45, hjust = 5) + 
  annotate("text", x = Inf, y = 30, label = "National", size = 3, vjust = 45, hjust = 3) + 
  labs(#title = " Top 5 Antibiotics by Dentists: State (PA) vs National (US) , 2020 - 2022",
    y = "Mean duration (days)") +
  # scale_x_discrete(breaks = c('PA 2020','PA 2021','PA 2022'),
  #                  labels = c('2020','2021','2022'))+
  scale_x_continuous(breaks = c(1.025, 3.025, 5.025))+
  scale_y_continuous(breaks = seq(0,35, by = 5), limits = c(0,35)) +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "Blues",direction = -1)






#-------------------------------------------------- Tables -----------------------------------------------------


PA_US_2020_T <- PA_US_2020 %>% 
  select(1,4,7)

PA_US_2021_T <- PA_US_2021 %>% 
  select(4,7)

PA_US_2022_T <- PA_US_2022 %>% 
  select(4,7)


PA_US_20_22_T <-  cbind(PA_US_2020_T,PA_US_2021_T, PA_US_2022_T) %>% 
  select(1,`Mean Duration (Days) 2020`=2,`Mean Duration (Days) 2021`=4,`Mean Duration (Days) 2022`=6,`Mean Duration (Days) 20`=3,
         `Mean Duration (Days) 21`=5,`Mean Duration (Days) 22`=7)  
  

PA_US_20_22_T%>%
  flextable() %>%
  set_caption( as_paragraph(
    as_chunk("Table 5. Most Frequently Prescribed Antibiotics by Dentists in Pennsylvania State compared to the United States, 2020-2022",props = fp_text_default(font.family = "Cambria(Body)",font.size = 11))),
    style = "Table Caption") %>%
  width(., j = c(1:7), width = 1.0, unit = "in") %>%
  width(., j = 1, width = 1.2, unit = "in") %>%
  width(., j = 7, width = 0.8, unit = "in") %>% 
add_header_row(., values = c("Pennsylvania","United States"), colwidths = c(4,3)) %>% 
align(., i=1, part="header", align="center")
#add_footer_lines(., " ")

set_flextable_defaults(
  font.size = 9, theme_fun = theme_box,
  padding = 6,
  background.color = "#EFEFEF")

print(PA_US_20_22_T)


# Editable flextable in word

library(officer)
library(flextable)


my_flextable <- flextable(PA_US_20_22_T)


my_flextable <- my_flextable %>% 
  set_caption( as_paragraph(
    as_chunk("Table 5. Most Frequently Prescribed Antibiotics by Dentists in Pennsylvania State compared to the United States, 2020-2022",props = fp_text_default(font.family = "Cambria(Body)",font.size = 11))),
    style = "Table Caption") %>%
  width(., j = c(1:7), width = 1.0, unit = "in") %>%
  width(., j = 1, width = 1.2, unit = "in") %>%
  width(., j = 7, width = 0.8, unit = "in") %>% 
  add_header_row(., values = c("Pennsylvania","United States"), colwidths = c(4,3)) %>% 
  align(., i=1, part="header", align="center") 


doc <- read_docx() %>% 
  body_add_flextable(my_flextable)

print(doc, target ="editable_table.docx") -- # exporting to word


  
  ##### END ############################################################
  
  
  
  
  
  
  

##### ignore running below code #################################################################################################
#-------------------- Previous code------------------------------------------------------------------------------------------------ 
state_nat_20_22_plot_a <- state_nat_20_22_plot_a %>%
  mutate(level = factor(level, levels = c('PA 2020', 'US 2020', 'PA 2021', 'US 2021', 'PA 2022', 'US 2022')))

ggplot(subset(state_nat_20_22_plot_a, Year == 2020), aes(x = level, y = Prescribing_Percentage, fill = Antibiotic)) +
  geom_bar(stat = "identity", width = 0.9 ) +  # Stacked bar plot
  geom_text(aes(label = Prescribing_Percentage), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 3) +
  annotate("text", x = 1, y = 100, label = "Pennsylvania", size = 3, vjust = 2, hjust = 0.5) + 
  annotate("text", x = 2, y = 100, label = "National", size = 3, vjust = 2, hjust = 0.5) + 
  labs(title = "Top 5 Antibiotics by Dentists: State (PA) vs National (US), 2020 - 2022",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_x_discrete(breaks = c('PA 2020'),
                   labels = c('2020'))+    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1)


# geom_text(data = state_nat_20_22_plot_ab, aes(x= level, y=Prescribing_Percentage,label = ifelse(grepl("PA", level), "PA", "US")),
#           position= position_stack(vjust = 0.5),
#           hjust = 5,
#           vjust = 1,
#           color = "black",
#           size = 3)+

annotate(text,x= state_nat_20_22_plot_ab$level, y= state_nat_20_22_plot_ab$Prescribing_Percentage[state_nat_20_22_plot_ab$level == "PA 2020"]+1, label = "PA", size =4)+
  annotate(text,x="PA 2021", y= state_nat_20_22_plot_ab$Prescribing_Percentage[state_nat_20_22_plot_ab$level == "US 2020"]+1, label = "US", size =4)



scale_x_discrete(labels = function(x){gsub("PA 2020", "2020",
                                           gsub("PA 2021", "2021",
                                                gsub("PA 2022","2022", x)))})+  
  
  
  geom_text(aes(label = ifelse(grepl("PA", level), "PA", "US")),
            position= position_stack(vjust = 0.5), 
            hjust = 5,
            vjust = 1,
            color = "black",
            size = 3)+


# only bars PA + US
ggplot(state_nat_20_22_plot, aes(x = Antibiotic, y=Prescribing_Percentage, fill = interaction(level))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  labs(
       title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
       x = "Antibiotics",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 360),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "Blues")+
  geom_text(aes(label = Prescribing_Percentage), 
                position = position_dodge(width = 0.9), 
                vjust = -0.2, 
                size = 3)

#-----------------------------------------------------------------------------------------------------------------
ggplot(state_nat_20_22_plot, aes(x = Antibiotic, y=Prescribing_Percentage, fill = interaction(level))) +
  geom_bar(data = state_nat_20_22_plot %>% 
           filter(level %in% c("Percent Prescribing PA 20",	
                               "Percent Prescribing PA 21",
                               "Percent Prescribing PA 22")),
                  stat = "identity",
                  position = "dodge") +
  geom_line(data = state_nat_20_22_plot %>% 
              filter(level %in% c("Percent Prescribing US 20",	
                                  "Percent Prescribing US 21",
                                  "Percent Prescribing US 22")),
            aes(group=1))+
  labs(
    title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
    x = "Antibiotics",
    y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin"))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_fill_viridis_d()

ggplot(state_nat_20_22_plot, aes(x = Antibiotic)) +
# Bars for PA data
  geom_bar(data = state_nat_20_22_plot %>% 
             filter(level %in% c("Percent Prescribing PA 20",	
                                 "Percent Prescribing PA 21",
                                 "Percent Prescribing PA 22")),
           aes(y = Prescribing_Percentage, fill = interaction(level)),
           stat = "identity",
           position = "dodge") 

  # Line for US data
  geom_line(data = state_nat_20_22_plot %>% 
              filter(level %in% c("Percent Prescribing US 20",	
                                  "Percent Prescribing US 21",
                                  "Percent Prescribing US 22")),
            aes(y = Prescribing_Percentage, group = level, color = level), 
            size = 1) +
  # Points for US data
  geom_point(data = state_nat_20_22_plot %>% 
               filter(level %in% c("Percent Prescribing US 20",	
                                   "Percent Prescribing US 21",
                                   "Percent Prescribing US 22")),
             aes(y = Prescribing_Percentage, color = level), size = 2) +  
  labs(
    title = "Top 5 Antibiotics by Dentists: PA vs US, 2020 - 2022",
    x = "Antibiotics",
    y = "Antibiotic Prescribing Percentage (%)",
    fill = NULL,
    color = "Year"  # Label for color legend
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin")) +
  theme(axis.text.x = element_text(angle = 360)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()



+
  geom_text(aes(label = Prescribing_Percentage), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 3)



# only lines


#1 # only lines
ggplot(state_nat_20_22_plot, aes(x = level, y=Prescribing_Percentage, group = Antibiotic)) +
  geom_line(aes(color= Antibiotic), size = 1.0)+
  geom_point(aes(color=Antibiotic), size = 2.0)+
  labs(
    title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
    y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  scale_fill_viridis_d()

#2 bar and line


ggplot(state_nat_20_22_plot, aes(x = Antibiotic, y=Prescribing_Percentage, group = level)) +
  geom_bar(data = state_nat_20_22_plot %>% 
           filter(grepl("PA", level)),
               aes(fill = level), stat = "identity", position = position_dodge(width = 0.6))+
  geom_line(data = state_nat_20_22_plot %>% 
            filter(grepl("US", level)),
            aes(color = level), size = 1.0)+
  geom_point(data = state_nat_20_22_plot %>% 
             filter(grepl("US", level)),
             aes(color = level), size = 2.0)+
  labs(
    title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
    y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Blues")+
  scale_color_manual(values = c("#ff7f00","#4daf4a", "#a65628"))


#3 Stacked bar plot - finalized plot


# Preliminary -- underwent revisions

  ggplot(state_nat_20_22_plot_a, aes(x = level, y=Prescribing_Percentage, fill = Antibiotic)) +
  geom_bar(stat= "identity", width = 0.8)+
  geom_text(aes(label = Prescribing_Percentage), 
            position = position_stack(vjust = 0.5), 
            color = "black",
            size = 3)+
   annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 3, vjust = 2, hjust = 7.6) + 
   annotate("text", x = Inf, y = 100, label = "National", size = 3, vjust = 2, hjust = 10.6) + 
   annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 2.8, vjust = 2, hjust = 5.1) + 
   annotate("text", x = Inf, y = 100, label = "National", size = 3, vjust = 2, hjust = 6) + 
   annotate("text", x = Inf, y = 100, label = "Pennsylvania", size = 3, vjust = 2, hjust = 2.6) +
   annotate("text", x = Inf, y = 100, label = "National", size = 3, vjust = 2, hjust = 1.7) + 
   labs(title = " Top 5 Antibiotics by Dentists: State (PA) vs National (US) , 2020 - 2022",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_x_discrete(breaks = c('PA 2020','PA 2021','PA 2022'),
                    labels = c('2020','2021','2022'))+
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust =-1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "Blues", direction = -1 )
  
  
#-----------------------------------------------------------------------------------------------------------------------------------  
  

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#4. x axis just 3 years - Bars --PA ; line -- US


# Facet wraps---

state_20_22_plot_fac <- state_nat_20_22_plot %>% 
  mutate(Year = as.numeric(sub(".*(\\d{4})$", "\\1", level)),
         level = case_when (level == "PA 2020" ~ "Pennsylvania",
                            level == "US 2020" ~ "National",
                            level == "PA 2021" ~ "Pennsylvania",
                            level == "US 2021" ~ "National",
                            level == "PA 2022" ~ "Pennsylvania",
                            level == "US 2022" ~ "National", T ~ level))
  

state_20_22_plot_fac <- state_20_22_plot_fac %>%
  mutate(level = factor(level, levels = c("Pennsylvania","National"))) %>% 
  arrange(level)

dodge <- position_dodge(width = 0.9)

plot20 <- ggplot(subset(state_20_22_plot_fac, Year == 2020), aes(x = level, y = Prescribing_Percentage)) +
  geom_bar(aes(fill = Antibiotic), stat = "identity", position = "dodge", show.legend = TRUE) +
  # geom_text(aes(label = Prescribing_Percentage), 
  #           position = dodge,
  #           hjust = -0.05,
  #           vjust =0)+
  facet_wrap(~ Year) +  # Facet by Year
  
  labs(
    y = "Antibiotic Prescribing Percentage (%)",
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) 



plot21 <- ggplot(subset(state_20_22_plot_fac, Year == 2021), aes(x = level, y = Prescribing_Percentage)) +
  geom_bar(aes(fill = Antibiotic), stat = "identity", position = "dodge", show.legend = TRUE) +
  facet_wrap(~ Year) +  # Facet by Year
  labs(y = "Antibiotic Prescribing Percentage (%)",
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) 



plot22 <- ggplot(subset(state_20_22_plot_fac, Year == 2022), aes(x = level, y = Prescribing_Percentage)) +
  geom_bar(aes(fill = Antibiotic), stat = "identity", position = "dodge", show.legend = TRUE) +
  facet_wrap(~ Year) +  # Facet by Year
  labs(y = "Antibiotic Prescribing Percentage (%)",
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Blues", direction = -1) 


install.packages(patchwork)
library(patchwork)

combined_20_22 <- (plot20 + plot21 + plot22)+
  plot_layout(guides = 'collect')+
  plot_annotation(title =  "Top 5 Antibiotics by Dentists: State (PA) vs National (US), 2020 - 2022") &
  theme(plot.title = element_text(size= 15))

print(combined_20_22)




ggplot(state_nat_20_22_plot, aes(x = Year, y=Prescribing_Percentage)) +
  geom_bar(data = state_nat_20_22_plot %>% 
             filter(grepl("PA", level)),
           aes(fill = Antibiotic), stat = "identity", position = "dodge", show.legend = T)+
  geom_line(data = state_nat_20_22_plot %>% 
              filter(grepl("US", level)),
            aes(group = Antibiotic, color = Antibiotic), size = 1.0, show.legend = T)+
  geom_point(data = state_nat_20_22_plot %>% 
               filter(grepl("US", level)),
             aes(color = Antibiotic), size= 2.0, show.legend = F)+
  labs(
    title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
    y = "Antibiotic Prescribing Percentage (%)") +
  scale_x_continuous(breaks = c(2020, 2021, 2022)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "Blues")+
  scale_color_manual(values = c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf"))






str(state_nat_20_22_plot_a)
#--------------------------
PA_20_22_only <- cbind(PA_US_2020,PA_US_2021,PA_US_2022) %>% 
  select(1,3,10,17)


PA_20_22_plot <- PA_20_22_only  %>%
  pivot_longer(cols= c(`Percent Prescribing PA 20`:`Percent Prescribing PA 22`),
               names_to = "level_PA",values_to = "Prescribing_Percentage_PA")
  #mutate(Percent = paste0(Prescribing_Percentage,"%"))



US_20_22_only <- cbind(PA_US_2020,PA_US_2021,PA_US_2022) %>% 
  select(1,6,13,20)

US_20_22_plot <- US_20_22_only %>% 
  pivot_longer(cols= c(`Percent Prescribing US 20`:`Percent Prescribing US 22`),
               names_to = "level_US",values_to = "Prescribing_Percentage_US")





PA_US_20_22_plot <- cbind(PA_20_22_plot, US_20_22_plot) %>% 
  select(1,2,3,5,6)




ggplot(PA_US_20_22_plot, aes(x = Antibiotic)) +
  geom_bar(aes(y= Prescribing_Percentage_PA,fill = interaction(level_PA)),
           stat = "identity",
           position = "dodge") +
  geom_point(aes(y= Prescribing_Percentage_US))+
  labs(fill= NULL,
       title = " Top 5 Antibiotics by Dentists: PA  vs US , 2020 - 2022",
       x = "Antibiotics",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin"))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_fill_viridis_d()+
  geom_text(aes(label = c(Prescribing_Percentage_PA,Prescribing_Percentage_US), 
            position = position_dodge(width = 0.9), 
            vjust = -0.2, 
            size = 3))






PA_US_20_22_plot




ggplot(PA_US_20_22_plot, aes(x = Antibiotic)) +
  geom_bar(aes(y = Prescribing_Percentage_PA, fill = interaction(level_PA)),
           stat = "identity",
           position = "dodge") +
  geom_line(aes(y = Prescribing_Percentage_US, group = Antibiotic, color = "US"), 
            position = position_dodge(width = 0.9), size = 1) +  # Trend line for US data
  labs(fill = NULL,
       title = "Top 5 Antibiotics by Dentists: PA vs US, 2020 - 2022",
       x = "Antibiotics",
       y = "Antibiotic Prescribing Percentage (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin")) +
  theme(axis.text.x = element_text(angle = 360))  +
  scale_fill_viridis_d() +
  scale_color_manual(values = "black") +  # Color for US line and points
  facet_wrap(~ Year) 






ggplot(PA_US_20_22_plot, aes(x = Antibiotic)) +
  # Bars for PA data
  geom_bar(data = PA_US_20_22_plot %>% 
             filter(level_PA %in% c("Percent Prescribing PA 20",	
                                 "Percent Prescribing PA 21",
                                 "Percent Prescribing PA 22")),
           aes(y = Prescribing_Percentage_PA, fill = interaction(level_PA)),
           stat = "identity",
           position = "dodge") +
  # Line for US data (single line across years)
  geom_line(data = PA_US_20_22_plot %>% 
              filter(level_US %in% c("Percent Prescribing US 20",	
                                  "Percent Prescribing US 21",
                                  "Percent Prescribing US 22")),
            aes(y = Prescribing_Percentage_US, group = Antibiotic, color = "level_US"), 
            size = 1) +
  # Points for US data
  geom_point(data = PA_US_20_22_plot %>% 
               filter(level_US %in% c("Percent Prescribing US 20",	
                                   "Percent Prescribing US 21",
                                   "Percent Prescribing US 22")),
             aes(y = Prescribing_Percentage_US, color = "level_US"), size = 2) +  
  labs(
    title = "Top 5 Antibiotics by Dentists: PA vs US, 2020 - 2022",
    x = "Antibiotics",
    y = "Antibiotic Prescribing Percentage (%)",
    fill = NULL,
    color = "Data Source"  # Label for color legend
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  scale_x_discrete(labels = c("Amoxicillin",
                              "Clindamycin Hcl",
                              "Penicillin V Potassium",
                              "Azithromycin",
                              "Cephalexin")) +
  theme(axis.text.x = element_text(angle = 360)) +
  scale_fill_viridis_d() +
  scale_color_manual(values = "black")





#----------------------------------------------------------------------------

# Washington (WC)



Medicare_Part_D_DrugP_2021_WA_file <- "C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/Drug-provider/Medicare_Part_D_Prescribers_by_Provider_and_Drug_2021_WA.csv"
AS_CMS_DrugP_WA_2021<- read.csv(Medicare_Part_D_DrugP_2021_WA_file)

WA_drug_provider_21 <- AS_CMS_DrugP_WA_2021 %>% #(n= 6769)
  filter(Prscrbr_Type == "Dentist") %>% 
  filter(Prscrbr_State_Abrvtn=="WA") %>%
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>%
  filter(!(Gnrc_Name %in% c("Chlorhexidine Gluconate", "Ibuprofen" ,
                            "Hydrocodone/Acetaminophen", "Fluoride (Sodium)"))) # we are not considering dentists count forthese drugs
 # distinct(Prscrbr_NPI) # dupes so ignore


gnrc_names_list_WA <- tabyl(WA_drug_provider_21$Gnrc_Name) %>% 
  adorn_totals()#(n= 94)



WA_drug_provider_21_count <- AS_CMS_DrugP_WA_2021 %>% #(n= 6769)
  filter(Prscrbr_Type == "Dentist") %>% 
  filter(Prscrbr_State_Abrvtn=="WA") %>%
  mutate(Gnrc_Name = case_when(Gnrc_Name== "Amoxicillin/Potassium Clav" ~ "Amoxicillin", T ~ Gnrc_Name))%>%
  filter(Gnrc_Name == "Amoxicillin") %>% 
  distinct(Prscrbr_NPI) %>% 
  
  # the above is other way to get no of providers for top 5 antibiotics
  
  
 # Example: Amoxicillin:
  
 # we need no. of dentist from WA for year 2021 who prescribed amoxicillin
 #the parent data set has duplicates in NPI because same NPi prescribed different antibiotics
# we need distinct dentist who prescribed amoxicillin
  
  
Top_5_Antibiotics_dentists_WA_2021 <- WA_drug_provider_21 %>% 
  group_by(Antibiotic = Gnrc_Name) %>% 
  summarize( `Providers Prescribing (n)` = n_distinct(Prscrbr_NPI),
             `Mean Duration (Days)`= round(mean(Tot_Day_Suply/Tot_Clms),1)) %>% 
  arrange(desc(`Providers Prescribing (n)`)) %>% 
 adorn_totals() %>% 
  mutate(`Percent Prescribing` = round((`Providers Prescribing (n)`/sum(`Providers Prescribing (n)`)*100),1)) %>% 
  top_n(5, wt= `Providers Prescribing (n)`) %>% 
  select(1,2,4,3) %>% 
  adorn_totals()




initial_count <- WA_drug_provider_21 %>%
  filter(Gnrc_Name == "Amoxicillin") %>%
  tally()
print(initial_count)

unique_providers_amoxicillin <- WA_drug_provider_21 %>%
  filter(Gnrc_Name == "Amoxicillin") %>%
  distinct(Prscrbr_NPI) %>%
  tally()
print(unique_providers_amoxicillin)
