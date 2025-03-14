
#------------------------------------------- CMS-SVI (2020-2022) --------------------------------------------------------------------------------


#purpose: 

#1.To have a CMS dataset with SVI scores & visualize PA counties with Antibiotic prescribing patterns & SVI scores
#2.Merging : CMS- by provider (2020-2022) (zipcode) --- PA Zipcode file (zipcode, county)--- SVI- CDC/ATSDR 2020-2022 (county, SVI scores for each county)
#3.classify counties into SVI categories (low, high, low-medium, medium-high)
#4.Calculate AB prescribing rates & analyze rates  among counties & SVI categories

#detach("package:MASS", unload = T)

# load libraries

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
library(lubridate)
library(DT)
library(haven)
library(htmlwidgets)
library(gtools)
library(writexl)
library(zipcodeR)
library(RColorBrewer) 


# Read files

CMS_2020 <- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/Medicare_Part_D_Prescribers_by_Provider_2020.csv")
CMS_2020 <- rename (CMS_2020, zipcode = Prscrbr_zip5)

zipcode <- search_state('PA')

SVI_2020<- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/svi_interactive_map 2020.csv") 


# Data cleaning & validation 

# select(COUNTY_NAME,RPL_THEMES)
# range(SVI_2020$RPL_THEMES) # 0-1
# colnames(SVI_2020)
# distinct(zipcode)
# missing_zipcodes <- anti_join(CMS_2020,zipcode, by = "zipcode")


# Main analysis

#1. Merging CMS 2020 with PA zipcode file to add county column

CMS_with_county_2020 <- merge(CMS_2020, zipcode, by = "zipcode") %>%
  rename(COUNTY_NAME = county) %>% 
  mutate(COUNTY_NAME = case_when(COUNTY_NAME== "McKean County" ~ "Mckean County", T ~ COUNTY_NAME))

# Mckean County -- SVI
# McKean County -- Zipcode
# select(PRSCRBR_NPI,Prscrbr_Type, zipcode,COUNTY_NAME, Prscrbr_St1, Antbtc_Tot_Clms, Tot_Benes)
# distinct(county, .keep_all = T) # n= 67 counties

#2. Merging CMS_county 2020 with SVI 2020 to add SVI data to CMS

CMS_SVI_2020_rev <- left_join(CMS_with_county_2020,SVI_2020, by = "COUNTY_NAME")

# Validation  -- ignore running below code ----------------------------------------------

# CMS_SVI_2020_rev <-  CMS_SVI_2020_rev %>% #(n= 60207)
#   select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,lat,lng,Antbtc_Tot_Clms,Tot_Benes,RPL_THEME1,RPL_THEME2,RPL_THEME3,
#          RPL_THEME4,RPL_THEMES) 
# 
# 
# colnames(CMS_SVI_2020_rev)
# range(CMS_SVI_2020_rev$RPL_THEMES)
# 
# # Exporting as XLSX
# 
# write_xlsx(CMS_SVI_2020_rev,"N:/CMS 2020-2022/CMS_SVI_2020.xlsx")

#----------------------------------------------------------------------------------

# Calculating AB presctiption rate
# Parent dataset

CMS_SVI_2020_rev <- CMS_SVI_2020_rev%>%
  mutate(Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) %>%
  mutate(AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000) %>% 
  dplyr::select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) 

# collapsing specialties
CMS_SVI_2020_rev <- CMS_SVI_2020_rev %>% 
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


tot_specialties_2__SVI_2020 <- CMS_SVI_2020_rev %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=67) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_SVI_2020 <- CMS_SVI_2020_rev %>% #(n=23952)
  select(PRSCRBR_NPI,Prscrbr_Type_new,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant")))


# Creating SV categories

# CMS_SVI_2020_rev_2 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))
# 
# # 2 levels only 
# CMS_SVI_2020_rev_3 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.5500 ~ "low",
#     RPL_THEMES > 0.5500 & RPL_THEMES <= 1.0 ~"high",
#     TRUE ~ NA_character_))



# 3 levels 
CMS_SVI_2020_rev_4 <- specialt_name_SVI_2020 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "High",
    TRUE ~ NA_character_))

# # check
# moderate <- CMS_SVI_2020_rev_4 %>% 
#   filter(Vul_Cat=="moderate")
# range(moderate$RPL_THEMES)


# Calculating Mean Antibiotic prescribing rate for each SV category

mean_prescribing_rate_by_vul_cat_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))


# County by SV categories

county_vul_category_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(Vul_Cat = first(Vul_Cat)) 

# Calculate the mean prescribing rate by county and get vulnerability category
prescribing_rate_by_county_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            num_prescribers = n_distinct(PRSCRBR_NPI),
            Vul_Cat = first(Vul_Cat))

#Mapping
library(sf)
library(tigris)

pa_counties <- counties(state = "PA", year = 2020, class = "sf") %>% 
  mutate(NAMELSAD = case_when(NAMELSAD == "McKean County" ~ "Mckean County", T ~ NAMELSAD))


#Merge the prescribing rate data with the Pennsylvania county 

pa_county_data <- pa_counties %>%
  left_join(prescribing_rate_by_county_rev, by = c("NAMELSAD" = "COUNTY_NAME")) %>% 
  mutate(NAME = str_replace(NAMELSAD, "County", ""))


#Color palette for the mean prescribing rate
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)   #not color blind

# palette <- c(brewer.pal(n = 9, name = "Blues"))
# 
# palette <- c("darkgrey", colorRampPalette(brewer.pal(9, "Blues"))(100))
# 

#Symbols for vulnerability categories
vul_cat_symbols <- c("High" = 24,        
                     "Moderate" = 22,  
                     "Low" = 21)          
# coordinates
pa_county_data <- pa_county_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_coordinates(.x)[1]),
         lat = map_dbl(centroid, ~st_coordinates(.x)[2]))

symbol_offset <- 0.075
pa_county_data <- pa_county_data %>%
  mutate(lon_offset = lon + symbol_offset,
         lat_offset = lat - symbol_offset) 



pa_county_data <- pa_county_data %>%
  mutate(
         Vul_Cat = factor(Vul_Cat, levels = c("High", "Moderate", "Low")))

pa_county_data <- pa_county_data %>% 
  mutate(Vul_Cat = case_when(is.na(Vul_Cat)~ "",
                             T ~Vul_Cat))


# shape_color_palette <- brewer.pal(n = 4, name = "Set1")
#library(RColorBrewer)

library(viridis)

# CMS County- SVI Map 2020
svi_map_20 <- 
  ggplot(data = pa_county_data) +
  geom_sf(aes(fill = mean_AB_Prescp_rate)) + 
  geom_sf_text(aes(label = NAME), size = 3, fontface = "bold",color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
  scale_color_viridis(discrete = TRUE, option = "D", name = "Vulnerability Category")  +
  scale_fill_gradientn(colors = palette,  name = "Prescribing Rate",
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "")+
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 9)) 


ggsave("2020 SVI map rev.png",svi_map_20,width =10, height = 8, dpi = 300)
 
#---------------------------------------------------------------------------------------------------------

# RURALITY 2020:

counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

# 
# # 4 levels
# 
# CMS_SVI_2020_rev_2 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))
# 
# CMS_SVI_2020_rev_setting_4 <- CMS_SVI_2020_rev_2 %>% 
#   mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                T~ COUNTY_NAME),
#          `County Name`= str_replace(COUNTY_NAME, " County", ""),
#          AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#   left_join(counties_settings, by = "County Name") %>% 
#   rename(Setting = "Rural/ Urban" )
# 
# 
# CMS_SVI_Rurality_2020_urbanco_4 <-CMS_SVI_2020_rev_setting_4 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise(Count= n(),
#             mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#             mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#    )
#   #adorn_totals()
# 
# 
# CMS_SVI_Rurality_2020_counco_4 <-CMS_SVI_2020_rev_setting_4 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise(Count= n_distinct(`County Name`),
#             mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#             mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#             
#   ) 
#   #mutate(total_distinct= sum(Count)) %>% 
#  # adorn_totals()
# 
# # 2 levels
# 
# CMS_SVI_2020_rev_3 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.5500 ~ "low",
#     RPL_THEMES > 0.5500 & RPL_THEMES <= 1.0 ~"high",
#     TRUE ~ NA_character_))
# 
# CMS_SVI_2020_rev_setting_2 <- CMS_SVI_2020_rev_3 %>% 
#   mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                T~ COUNTY_NAME),
#          `County Name`= str_replace(COUNTY_NAME, " County", ""),
#          AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#   left_join(counties_settings, by = "County Name") %>% 
#   rename(Setting = "Rural/ Urban" )
# 
# # cms_low_medium_4<- CMS_SVI_2020_rev_setting_2 %>% 
# #   filter(Vul_Cat == "low-medium")
# 
# CMS_SVI_Rurality_2020_urbanco_2 <-CMS_SVI_2020_rev_setting_2 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise(Count= n(),
#             mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#             mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#             
#   ) 
#  # adorn_totals()
# 
# 
# CMS_SVI_Rurality_counco_2 <-CMS_SVI_2020_rev_setting_2 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise(Count= n_distinct(`County Name`),
#             mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#             mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#             
# ) 
  #mutate(total_distinct= sum(Count)) %>% 
 # adorn_totals()


#3levels

# 3 levels

#Recategorising

CMS_SVI_2020_rev_4 <- specialt_name_SVI_2020 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "High",
    TRUE ~ NA_character_))


# check
# moderate <- CMS_SVI_2020_rev_4 %>% 
#   filter(Vul_Cat=="moderate")
# range(moderate$RPL_THEMES)


CMS_SVI_2020_rev_setting_3 <- CMS_SVI_2020_rev_4 %>% 
  mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
                               T~ COUNTY_NAME),
         `County Name`= str_replace(COUNTY_NAME, " County", ""),
         AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
  left_join(counties_settings, by = "County Name") %>% 
  rename(Setting = "Rural/ Urban" )

# urban-rural counts
CMS_SVI_Rurality_2020_urbanco_3 <-CMS_SVI_2020_rev_setting_3 %>% 
  group_by(Setting,Vul_Cat) %>% 
  summarise(Providers= n(),
            mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            mean_AB_Prescp_rate = round(mean_AB_Prescp_rate),
            Prescriptions = sum(Antbtc_Tot_Clms)
            
  ) %>% 
  select(1,2,5,3,4)
 # adorn_totals()

# test
#low 
# 
# low <- CMS_SVI_2020_rev_setting_3 %>% 
#   filter(Vul_Cat== "LOW" & Setting== "Urban") %>% 
#   adorn_totals()

# county counts
CMS_SVI_Rurality_counco_3 <-CMS_SVI_2020_rev_setting_3 %>% 
  group_by(Setting,Vul_Cat) %>% 
  summarise(Count= n_distinct(`County Name`),
            mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
            
  ) 
  #mutate(total_distinct= sum(Count)) %>% 
  #adorn_totals()


###########################################
mean_prescribing_rate_by_vul_cat_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(COUNTY_NAME,Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))

# final code

CMS_SVI_Rurality_2020 <- CMS_SVI_Rurality_2020_urbanco_3 %>% 
  mutate(Vul_Cat = factor(Vul_Cat, levels = c("Low",
                                              "Moderate",
                                              "High"))) %>% 
  arrange(Vul_Cat) 
  
########################## ignore ##########################################
# Statistical Analysis

# Two independent sample t test

#low
# low_t_2020 <- CMS_SVI_Rurality_2020 %>% 
#   filter(Vul_Cat == "low") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_lowt_2020 <- data.frame(low = round(low_t_2020$p.value,2)) # pvalue = 0.03
# 
# 
# # low_medium
# 
# low_m_t_2020 <- CMS_SVI_Rurality_2020 %>% 
#   filter(Vul_Cat == "low-medium") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_low_mt_2020 <- data.frame(`low-medium` = round(low_m_t_2020$p.value,2)) # pvalue = 0.01
# 
# 
# #medium-high
# 
# medium_high_t_2020 <- CMS_SVI_Rurality_2020 %>% 
#   filter(Vul_Cat == "medium-high") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_med_h_2020 <- data.frame(`medium-high` = round(medium_high_t_2020$p.value,2)) # pvalue = 0.01
# 
# 
# #high
# 
# high_t_2020 <- CMS_SVI_Rurality_2020 %>% 
#   filter(Vul_Cat == "high") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_high_2020 <- data.frame(high = round(high_t_2020$p.value,2)) # pvalue = 0.05
# 
# 
# SVI_rurality_2020 <-  cbind(t_p_value_lowt_2020,t_p_value_low_mt_2020,t_p_value_med_h_2020,t_p_value_high_2020) %>% 
#   pivot_longer(
#     cols = c(low, low.medium, medium.high, high),
#     names_to = "Vul_Cat",
#     values_to = "p_values") %>% 
#   mutate(Vul_Cat =case_when(Vul_Cat ==  "low.medium" ~ "low-medium",
#                             Vul_Cat ==  "medium.high" ~ "medium-high", T~ Vul_Cat))
# 
# colnames(SVI_rurality_2020)
# 
# # horizontal bars
# 
# 
# CMS_SVI_Rurality_2020_a <- CMS_SVI_Rurality_2020 %>% 
#   left_join(SVI_rurality_2020, by = "Vul_Cat") %>% 
#   # mutate(p_values = as.character(p_values),
#   #        p_values = case_when(Vul_Cat == "low" & Setting == "Urban" ~ "",
#   #                             Vul_Cat == "low-medium" & Setting == "Urban" ~ "",
#   #                             Vul_Cat == "medium-high" & Setting == "Urban" ~ "",
#   #                             Vul_Cat == "high" & Setting == "Urban" ~ "",
#   #                             T~ p_values),
#   #        p_values = ifelse(p_values != "", paste("p-value =", p_values), "")) %>% 
#   mutate(Vul_Cat = factor(Vul_Cat, levels = c("high",
#                                               "medium-high",
#                                               "low-medium",
#                                               "low"))) 
# 
# 
# 
# CMS_SVI_Rurality_2020_a <- CMS_SVI_Rurality_2020_a %>% 
#  mutate(Vul_Cat = recode(Vul_Cat ,"high"="HIGH","medium-high"= "MEDIUM-HIGH","low-medium"= "LOW-MEDIUM","low"="LOW"))
# 
# 
# ggplot(data = CMS_SVI_Rurality_2020_a, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
#             position = position_dodge(width = 0.9), 
#             hjust = -0.1, size = 4.0) + 
#   geom_text(aes(label = p_values),
#             position = position_dodge(width =0.9), 
#             hjust = ifelse(CMS_SVI_Rurality_2020_a$Vul_Cat =="MEDIUM-HIGH",-1.8,
#                            ifelse(CMS_SVI_Rurality_2020_a$p_values =="p-value = 0.05",-0.6,
#                                   ifelse(CMS_SVI_Rurality_2020_a$p_values =="p-value = 0.01",-1.4,-0.4))),
#             vjust = -1.0,size = 4.0)+# Adjust hjust for horizontal bars
#   labs(fill = NULL,
#        title = "",
#        x = "Antibiotic Prescribing Rate",
#        y = "SVI Levels") +
#   scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
#   theme_minimal() +
#   theme(axis.title.y = element_text(size = 12, vjust = -0.5, hjust =0.5),
#         axis.text.y = element_text(size = 12, hjust = 1.1),
#         axis.text.x = element_text(size = 12, hjust = 0),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12, hjust = 0),
#         plot.title = element_text(size = 12, hjust = 0),
#         legend.position = "top",
#         panel.grid = element_blank())  +
#   
#   scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
# coord_flip()

############################################## resume #######################################################################

#3 levels
ggplot(data = CMS_SVI_Rurality_2020, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 4.0) + 
  # geom_text(aes(label = p_values),
  #           position = position_dodge(width =0.9), 
  #           # hjust = ifelse(CMS_SVI_Rurality_2020_a$Vul_Cat =="MEDIUM-HIGH",-1.8,
  #           #                ifelse(CMS_SVI_Rurality_2020_a$p_values =="p-value = 0.05",-0.6,
  #           #                       ifelse(CMS_SVI_Rurality_2020_a$p_values =="p-value = 0.01",-1.4,-0.4))),
  #           vjust = -1.0,size = 4.0)+# Adjust hjust for horizontal bars
  labs(fill = NULL,
       title = "",
       x = "Antibiotic Prescribing Rate",
       y = "SVI Levels") +
  scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 12, vjust = -0.5, hjust =0.5),
        axis.text.y = element_text(size = 12, hjust = 1.1),
        axis.text.x = element_text(size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, hjust = 0),
        plot.title = element_text(size = 12, hjust = 0),
        legend.position = "top",
        panel.grid = element_blank())  +
  
  scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
coord_flip()



##### end 2020 ######


############################################################################## ignore ########################################################3
# excel files send to kranthi for modelling  IGNORE RUNNING

# SVI_themes <-  read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/SVI_themes.xlsx")
# 
# SVI_Settings <- specialt_name_SVI_2020 %>% 
#   mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                T~ COUNTY_NAME),
#          `County Name`= str_replace(COUNTY_NAME, " County", ""),
#          AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#   left_join(counties_settings, by = "County Name") %>% 
#   rename(Setting = "Rural/ Urban" ) %>% 
#   select(1:2,7:16)
# 
# 
# write_xlsx(SVI_Settings,"N:/CMS 2020-2022/SVI_Settings.xlsx")
#                                                 
# counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")
# 
# 
# #Individual Themes Quartiles
# 
# #Socio Economic status:
# 
# CMS_SVI_2020_rev_2_SE <- CMS_SVI_2020_rev_2 %>% 
#   mutate(SE_quart = ntile(RPL_THEME1,4))
# 
# 
# CMS_SVI_2020_rev_2_SE <- CMS_SVI_2020_rev_2_SE %>% 
#   group_by(SE_quart) %>% 
#   summarize(mean_AB_rate = mean(AB_Prescp_rate))
# 
# # Household characteristics
# 
# CMS_SVI_2020_rev_2_HE <- CMS_SVI_2020_rev_2 %>% 
#   mutate(HE_quart = ntile(RPL_THEME2,4))
# 
# 
# CMS_SVI_2020_rev_2_HE <- CMS_SVI_2020_rev_2_HE %>% 
#   group_by(HE_quart) %>% 
#   summarize(mean_AB_rate = mean(AB_Prescp_rate))
# 
# 
# # Racial & Ethnic 
# 
# CMS_SVI_2020_rev_2_RE <- CMS_SVI_2020_rev_2 %>% 
#   mutate(RE_quart = ntile(RPL_THEME3,4))
# 
# 
# CMS_SVI_2020_rev_2_RE <- CMS_SVI_2020_rev_2_RE %>% 
#   group_by(RE_quart) %>% 
#   summarize(mean_AB_rate = mean(AB_Prescp_rate))
# 
# # Housing & Transportation
# 
# CMS_SVI_2020_rev_2_HT <- CMS_SVI_2020_rev_2 %>% 
#   mutate(HT_quart = ntile(RPL_THEME4,4))
# 
# 
# CMS_SVI_2020_rev_2_HT <- CMS_SVI_2020_rev_2_HT %>% 
#   group_by(HT_quart) %>% 
#   summarize(mean_AB_rate = mean(AB_Prescp_rate))


#-------------------------------------------------------------------------------------------------


############################################################# RESUME #############################################


#2021

# Read files


CMS_2021 <- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/Medicare_Part_D_Prescribers_by_Provider_2021.csv")
CMS_2021 <- rename (CMS_2021, zipcode = Prscrbr_zip5)


zipcode <- search_state('PA')

SVI_2021<- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/svi_interactive_map 2020.csv") 


# CMS_2021 <- read.csv("N:/CMS 2020-2022/Medicare_Part_D_Prescribers_by_Provider_2021.csv")
# CMS_2021 <- rename (CMS_2021, zipcode = Prscrbr_zip5)
# 
# zipcode <- search_state('PA')
#  
# SVI_2021<- read.csv("N:/CMS 2020-2022/svi_interactive_map 2020.csv") 


# Data cleaning & validation 

# select(COUNTY_NAME,RPL_THEMES)
# range(SVI_2021$RPL_THEMES) # 0-1
# colnames(SVI_2021)
# distinct(zipcode)
# missing_zipcodes <- anti_join(CMS_2021,zipcode, by = "zipcode")


# Main analysis

#1. Merging CMS 2021 with PA zipcode file

CMS_with_county_2021 <- merge(CMS_2021, zipcode, by = "zipcode") %>% #(n= 61387)
  rename(COUNTY_NAME = county) %>% 
  mutate(COUNTY_NAME = case_when(COUNTY_NAME== "McKean County" ~ "Mckean County", T ~ COUNTY_NAME))

# Mckean County -- SVI
# McKean County -- Zipcode
# select(PRSCRBR_NPI,Prscrbr_Type, zipcode,COUNTY_NAME, Prscrbr_St1, Antbtc_Tot_Clms, Tot_Benes)
# distinct(county, .keep_all = T) # n= 67 counties

#2. Merging CMS_county 2021 with SVI 2021

CMS_SVI_2021_rev <- left_join(CMS_with_county_2021,SVI_2021, by = "COUNTY_NAME")

# # Validation  -- ignore running 
# 
# CMS_SVI_2021_val <-  CMS_SVI_2021 %>% #(n= 61387)
#   select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,RPL_THEMES) 
# 
# range(CMS_SVI_2021$RPL_THEMES) # 0 1
# 
# # Exporting as XLSX
# 
# write_xlsx(CMS_SVI_2021_rev,"N:/CMS 2020-2022/CMS_SVI_2021.xlsx")
#-----------------------------------------------------------------------------------------------------------

# Calculating AB presctiption rate
# Parent dataset

CMS_SVI_2021_rev <- CMS_SVI_2021_rev%>%  #(n= 32825)
  mutate(Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) %>%
  mutate(AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000) %>% 
  select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4,RPL_THEMES) 

# collapsing specialties
CMS_SVI_2021_rev <- CMS_SVI_2021_rev %>% 
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


tot_specialties_2__SVI_2021 <- CMS_SVI_2021_rev %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=67) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_SVI_2021 <- CMS_SVI_2021_rev %>% #(n=23952)
  select(PRSCRBR_NPI,Prscrbr_Type_new,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant")))


# Creating SV categories
# 
# CMS_SVI_2021_rev_2<- specialt_name_SVI_2021%>% #(n=24150)
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))

CMS_SVI_2021_rev_4 <- specialt_name_SVI_2021 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "High",
    TRUE ~ NA_character_))

# Calculating Mean Antibiotic prescribing rate for each SV category

mean_prescribing_rate_by_vul_cat_21 <- CMS_SVI_2021_rev_4 %>%
  group_by(Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))


# County by SV categories

county_vul_category_rev_21 <- CMS_SVI_2021_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(Vul_Cat = first(Vul_Cat)) 

# Calculate the mean prescribing rate by county and get vulnerability category
prescribing_rate_by_county_rev_21 <- CMS_SVI_2021_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            num_prescribers_21 = n_distinct(PRSCRBR_NPI),
            Vul_Cat = first(Vul_Cat)) %>% 
  rename(mean_AB_Prescp_rate_21=mean_AB_Prescp_rate)



#Mapping
library(sf)
library(tigris)

pa_counties_2021 <- counties(state = "PA", year = 2021, class = "sf") %>% 
  mutate(NAMELSAD = case_when(NAMELSAD == "McKean County" ~ "Mckean County", T ~ NAMELSAD))


#Merge the prescribing rate data with the Pennsylvania county 

pa_county_data_21 <- pa_counties_2021 %>%
  left_join(prescribing_rate_by_county_rev_21, by = c("NAMELSAD" = "COUNTY_NAME")) %>% 
  mutate(NAME = str_remove(NAMELSAD, " County"))


#Color palette for the mean prescribing rate
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Symbols for vulnerability categories
vul_cat_symbols <- c("Low" = 21,  "Moderate" = 22, "High" = 24)

pa_county_data_21 <- pa_county_data_21 %>%
  mutate(
    Vul_Cat = factor(Vul_Cat, levels = c("High", "Moderate", "Low")))

pa_county_data_21 <- pa_county_data_21 %>% 
  mutate(Vul_Cat = case_when(is.na(Vul_Cat)~ "",
                             T ~Vul_Cat))


pa_county_data_21 <- pa_county_data_21 %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_coordinates(.x)[1]),
         lat = map_dbl(centroid, ~st_coordinates(.x)[2]))

symbol_offset <- 0.075
pa_county_data_21 <- pa_county_data_21 %>%
  mutate(lon_offset = lon + symbol_offset,
         lat_offset = lat - symbol_offset) 

# CMS County- SVI Map 2020
svi_map_21 <- 
  ggplot(data = pa_county_data_21) +
  geom_sf(aes(fill = mean_AB_Prescp_rate_21)) + 
  geom_sf_text(aes(label = NAME), size = 3, fontface= "bold", color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "")+
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 9)) 

ggsave("2021 SVI map rev.png",svi_map_21,width =10, height = 8, dpi = 300)
#-------------------------------------------------------------------------------------------------

# RURALITY 2021:

#3levels

# 3 levels

#Recategorising

counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

CMS_SVI_2021_rev_4 <- specialt_name_SVI_2021 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "HIGH",
    TRUE ~ NA_character_))


# # check
# moderate <- CMS_SVI_2020_rev_4 %>% 
#   filter(Vul_Cat=="moderate")
# range(moderate$RPL_THEMES)
# 

CMS_SVI_2021_rev_setting_3 <- CMS_SVI_2021_rev_4 %>% 
  mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
                               T~ COUNTY_NAME),
         `County Name`= str_replace(COUNTY_NAME, " County", ""),
         AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
  left_join(counties_settings, by = "County Name") %>% 
  rename(Setting = "Rural/ Urban" )

# urban-rural counts
CMS_SVI_Rurality_2021_urbanco_3 <-CMS_SVI_2021_rev_setting_3 %>% 
  group_by(Setting,Vul_Cat) %>% 
  summarise(Providers= n(),
            mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            mean_AB_Prescp_rate = round(mean_AB_Prescp_rate),
            Prescriptions = sum(Antbtc_Tot_Clms)
            
  ) %>% 
  select(1,2,5,3,4)



# 
# CMS_SVI_2021_rev_settings <- CMS_SVI_2021_rev_2 %>% 
#   mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                T~ COUNTY_NAME),
#          `County Name`= str_replace(COUNTY_NAME, " County", ""),
#          AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#   left_join(counties_settings, by = "County Name") %>% 
#   rename(Setting = "Rural/ Urban" )
# 


mean_prescribing_rate_by_vul_cat_rev_21 <- CMS_SVI_2021_rev_4 %>%
  group_by(Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))

# final code

CMS_SVI_Rurality_2021 <- CMS_SVI_Rurality_2021_urbanco_3 %>% 
  mutate(Vul_Cat = factor(Vul_Cat, levels = c("Low",
                                              "Moderate",
                                              "High"))) %>% 
  arrange(Vul_Cat)


############################################# ignore#####################################################################################
# # final code
# 
# CMS_SVI_Rurality_2021 <- CMS_SVI_Rurality_2021 %>% 
#   mutate(Vul_Cat = factor(Vul_Cat, levels = c("low",
#                                               "low-medium",
#                                               "medium-high",
#                                               "high"))) %>% 
#   arrange(Vul_Cat)


# Statistical Analysis

# Two independent sample t test

#low
# low_t_2021 <- CMS_SVI_Rurality_2021 %>% 
#   filter(Vul_Cat == "low") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_lowt_2021 <- data.frame(low = round(low_t_2021$p.value,2)) # pvalue = 0.03
# 
# 
# # low_medium
# 
# low_m_t_2021 <- CMS_SVI_Rurality_2021 %>% 
#   filter(Vul_Cat == "low-medium") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_low_mt_2021 <- data.frame(`low-medium` = round(low_m_t_2021$p.value,2)) # pvalue = 0.02
# 
# 
# #medium-high
# 
# medium_high_t_2021 <- CMS_SVI_Rurality_2021 %>% 
#   filter(Vul_Cat == "medium-high") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_med_h_2021 <- data.frame(`medium-high` = round(medium_high_t_2021$p.value,2)) # pvalue = 0.01
# 
# 
# #high
# 
# high_t_2021 <- CMS_SVI_Rurality_2021 %>% 
#   filter(Vul_Cat == "high") %>% 
#   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
#   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
#   select(2,3) %>% 
#   t.test()
# 
# t_p_value_high_2021 <- data.frame(high = round(high_t_2021$p.value,2)) # pvalue = 0.04
# 
# 
# SVI_rurality_2021 <-  cbind(t_p_value_lowt_2021,t_p_value_low_mt_2021,t_p_value_med_h_2021,t_p_value_high_2021) %>% 
#   pivot_longer(
#     cols = c(low, low.medium, medium.high, high),
#     names_to = "Vul_Cat",
#     values_to = "p_values") %>% 
#   mutate(Vul_Cat =case_when(Vul_Cat ==  "low.medium" ~ "low-medium",
#                             Vul_Cat ==  "medium.high" ~ "medium-high", T~ Vul_Cat))
# 
# # colnames(SVI_rurality)

# horizontal bars


# CMS_SVI_Rurality_2021_a <- CMS_SVI_Rurality_2021 %>% 
#   left_join(SVI_rurality_2021, by = "Vul_Cat") %>% 
#   mutate(p_values = as.character(p_values),
#          p_values = case_when(Vul_Cat == "low" & Setting == "Urban" ~ "",
#                               Vul_Cat == "low-medium" & Setting == "Urban" ~ "",
#                               Vul_Cat == "medium-high" & Setting == "Urban" ~ "",
#                               Vul_Cat == "high" & Setting == "Urban" ~ "",
#                               T~ p_values),
#          p_values = ifelse(p_values != "", paste("p-value =", p_values), "")) %>% 
#   mutate(Vul_Cat = factor(Vul_Cat, levels = c("high",
#                                               "medium-high",
#                                               "low-medium",
#                                               "low"))) 
# 
# 
# 
# ggplot(data = CMS_SVI_Rurality_2021_a, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
#             position = position_dodge(width = 0.9), 
#             hjust = -0.1, size = 4.0) + 
#   geom_text(aes(label = p_values),
#             position = position_dodge(width =0.9), 
#             hjust = ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.02",-1.55,
#                            ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.01",-2.2,
#                                   ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.04",-0.95,-0.6))),
#             vjust = -1.0, size = 4.0)+# Adjust hjust for horizontal bars
#   labs(fill = NULL,
#        title = "",
#        x = "Antibiotic Prescribing Rate",
#        y = "SVI levels") +
#   scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
#   theme_minimal() +
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_text(size = 12, hjust = 0),
#         axis.text.x = element_text(size = 12, hjust = 0),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12, hjust = 0),
#         legend.position = "top",
#         plot.title = element_text(size = 12, hjust = 0),
#         panel.grid = element_blank()) +
#   
#   scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
# coord_flip()


####################################### resume #####################################################################################


ggplot(data = CMS_SVI_Rurality_2021, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 4.0) + 
  # geom_text(aes(label = p_values),
  #           position = position_dodge(width =0.9), 
  #           hjust = ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.02",-1.55,
  # #                          ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.01",-2.2,
  # #                                 ifelse(CMS_SVI_Rurality_2021_a$p_values =="p-value = 0.04",-0.95,-0.6))),
  #           vjust = -1.0, size = 4.0)+# Adjust hjust for horizontal bars
  labs(fill = NULL,
       title = "",
       x = "Antibiotic Prescribing Rate",
       y = "SVI levels") +
  scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 12, vjust = 1),
        axis.text.y = element_text(size = 12, hjust = 0),
        axis.text.x = element_text(size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, hjust = 0),
        legend.position = "top",
        plot.title = element_text(size = 12, hjust = 0),
        panel.grid = element_blank()) +
  
  scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
coord_flip()


################ end 2021 ###############################

# 2022

 
CMS_2022 <- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/Medicare_Part_D_Prescribers_by_Provider_2022.csv")
CMS_2022 <- rename (CMS_2022, zipcode = Prscrbr_zip5)

zipcode <- search_state('PA')

SVI_2022<- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/svi_interactive_map 2022.csv") 

# CMS_2022 <- read.csv("N:/CMS 2020-2022/Medicare_Part_D_Prescribers_by_Provider_2022.csv") # (n= 62959)
# CMS_2022 <- rename (CMS_2022, zipcode = Prscrbr_zip5)
# zipcode <- search_state('PA')
# SVI_2022<- read.csv("N:/CMS 2020-2022/svi_interactive_map 2022.csv") 


# Data cleaning & validation 

# select(COUNTY_NAME,RPL_THEMES)
# range(SVI_2022$RPL_THEMES) # 0-1
# colnames(SVI_2022)
# distinct(zipcode)
# missing_zipcodes <- anti_join(CMS_2022,zipcode, by = "zipcode")


# Main analysis

#1. Merging CMS 2022 with PA zipcode file

CMS_with_county_2022<- merge(CMS_2022, zipcode, by = "zipcode") %>% #(n= 62938)
  rename(COUNTY_NAME = county) %>% 
  mutate(COUNTY_NAME = case_when(COUNTY_NAME== "McKean County" ~ "Mckean County", T ~ COUNTY_NAME))

# Mckean County -- SVI
# McKean County -- Zipcode
# select(PRSCRBR_NPI,Prscrbr_Type, zipcode,COUNTY_NAME, Prscrbr_St1, Antbtc_Tot_Clms, Tot_Benes)
# distinct(county, .keep_all = T) # n= 67 counties

#2. Merging CMS_county 2022 with SVI 2022

CMS_SVI_2022_rev<- left_join(CMS_with_county_2022,SVI_2022, by = "COUNTY_NAME") # (n= 62959)

# Validation  -- ignore running 

# CMS_SVI_2022_val <-  CMS_SVI_2022 %>% #(n= 61387)
#   select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,RPL_THEMES) 
# 
# range(CMS_SVI_2022$RPL_THEMES) # 0 1
# 
# # Exporting as XLSX
# 
# write_xlsx(CMS_SVI_2022_rev,"N:/CMS 2020-2022/CMS_SVI_2022.xlsx")
#-------------------------------------------------------------------------------------------

# Calculating AB presctiption rate
# Parent dataset

CMS_SVI_2022_rev <- CMS_SVI_2022_rev%>%  #(n= 32825)
  mutate(Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) %>%
  mutate(AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000) %>% 
  select(Prscrbr_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) 


# collapsing specialties
CMS_SVI_2022_rev <- CMS_SVI_2022_rev %>% 
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


tot_specialties_2__SVI_2022 <- CMS_SVI_2022_rev %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=67) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_SVI_2022 <- CMS_SVI_2022_rev %>% #(n=23952)
  select(PRSCRBR_NPI =Prscrbr_NPI,Prscrbr_Type_new,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant")))


# Creating SV categories
# 
# CMS_SVI_2022_rev_2<- specialt_name_SVI_2022%>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))

CMS_SVI_2022_rev_4 <- specialt_name_SVI_2022 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "High",
    TRUE ~ NA_character_))

# Calculating Mean Antibiotic prescribing rate for each SV category

mean_prescribing_rate_by_vul_cat_22 <- CMS_SVI_2022_rev_4 %>%
  group_by(Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))


# Maps:
# Creating SV categories

# County by SV categories

county_vul_category_rev_22 <- CMS_SVI_2022_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(Vul_Cat = first(Vul_Cat)) 

# Calculate the mean prescribing rate by county and get vulnerability category
prescribing_rate_by_county_rev_22 <- CMS_SVI_2022_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            Vul_Cat = first(Vul_Cat),
            num_prescriber_22 = n_distinct(PRSCRBR_NPI)) %>% 
  rename(mean_AB_Prescp_rate_22=mean_AB_Prescp_rate)


# RURALITY 2022:
# overall , not filtered prescribers , should I??

counties_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/AS CMS 2022/List of Counties settings 2022.xlsx")

# # 4 levels
# 
# CMS_SVI_2022_rev_2<- specialt_name_SVI_2022%>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))
# 
# 
# CMS_SVI_2022_rev_setting_4 <- CMS_SVI_2022_rev_2 %>% 
#   mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                T~ COUNTY_NAME),
#          `County Name`= str_replace(COUNTY_NAME, " County", ""),
#          AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#   left_join(counties_settings, by = "County Name") %>% 
#   rename(Setting = "Rural/ Urban" )
# 
# 
# CMS_SVI_Rurality_2022 <-CMS_SVI_2022_rev_setting_4 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))
# 
# 
# # final code
# 
# CMS_SVI_Rurality_2022 <- CMS_SVI_Rurality_2022 %>% 
#   mutate(Vul_Cat = factor(Vul_Cat, levels = c("low",
#                                               "low-medium",
#                                               "medium-high",
#                                               "high"))) %>% 
#   arrange(Vul_Cat)
# 
# 
# CMS_SVI_Rurality_2022_urbanco_4 <-CMS_SVI_2022_rev_setting_4 %>% 
#   group_by(Setting,Vul_Cat) %>% 
#   summarise((Providers= n(),
#              mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#              mean_AB_Prescp_rate = round(mean_AB_Prescp_rate),
#              Prescriptions = sum(Antbtc_Tot_Clms)
#   ) %>% 
#     select(1,SVI_Category =2,5,3,Mean_Antibiotic_Prescription_Rate=4)
#   
#   
#   #adorn_totals()
#   
#   
#   
#   
#   
#   CMS_SVI_Rurality_2022_counco_4 <-CMS_SVI_2022_rev_setting_4 %>% 
#     group_by(Setting,Vul_Cat) %>% 
#     summarise(Count= n_distinct(`County Name`),
#               mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#               mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#               
#     ) 
#   #mutate(total_distinct= sum(Count)) %>% 
#   # adorn_totals()
#   
#   
#   # 2 levels
#   
#   CMS_SVI_2022_rev_3 <- specialt_name_SVI_2022 %>%
#     mutate(Vul_Cat = case_when(
#       RPL_THEMES >= 0 & RPL_THEMES <= 0.5500 ~ "low",
#       RPL_THEMES > 0.5500 & RPL_THEMES <= 1.0 ~"high",
#       TRUE ~ NA_character_))
#   
#   CMS_SVI_2022_rev_setting_2 <- CMS_SVI_2022_rev_3 %>% 
#     mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
#                                  T~ COUNTY_NAME),
#            `County Name`= str_replace(COUNTY_NAME, " County", ""),
#            AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
#     left_join(counties_settings, by = "County Name") %>% 
#     rename(Setting = "Rural/ Urban" )
#   
#   
#   
#   CMS_SVI_Rurality_2022_urbanco_2 <-CMS_SVI_2022_rev_setting_2 %>% 
#     group_by(Setting,Vul_Cat) %>% 
#     summarise(Count= n(),
#               mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#               mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#               
#     ) 
#   # adorn_totals()
#   
#   CMS_SVI_Rurality_2022_counco_2 <- CMS_SVI_2022_rev_setting_2 %>% 
#     group_by(Setting,Vul_Cat) %>% 
#     summarise(Count= n_distinct(`County Name`),
#               mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
#               mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
#               
#     ) 
#   #mutate(total_distinct= sum(Count)) %>% 
#   # adorn_totals()
#   
#   
  #3levels
  
  # 3 levels
  
  #Recategorising
  
  CMS_SVI_2022_rev_4 <- specialt_name_SVI_2022 %>%
    mutate(Vul_Cat = case_when(
      RPL_THEMES <= 0.33 ~ "Low",
      RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
      RPL_THEMES > 0.67 ~ "High",
      TRUE ~ NA_character_))
  
  
  CMS_SVI_2022_rev_setting_3 <- CMS_SVI_2022_rev_4 %>% 
    mutate(COUNTY_NAME=case_when(COUNTY_NAME == "Mckean County" ~ "McKean County",
                                 T~ COUNTY_NAME),
           `County Name`= str_replace(COUNTY_NAME, " County", ""),
           AB_Prescp_rate = round(AB_Prescp_rate,1)) %>% 
    left_join(counties_settings, by = "County Name") %>% 
    rename(Setting = "Rural/ Urban")
  
  # urban-rural counts
  CMS_SVI_Rurality_2022_urbanco_3 <-CMS_SVI_2022_rev_setting_3 %>% 
    group_by(Setting,Vul_Cat) %>% 
    summarise(Providers= n(),
              mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
              mean_AB_Prescp_rate = round(mean_AB_Prescp_rate),
              Prescriptions = sum(Antbtc_Tot_Clms)
              
    )%>% 
    select(1,2,5,3,4)
  
  # adorn_totals()
  
  # county counts
  CMS_SVI_Rurality_2022_counco_3 <-CMS_SVI_2022_rev_setting_3 %>% 
    group_by(Setting,Vul_Cat) %>% 
    summarise(Count= n_distinct(`County Name`),
              mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
              mean_AB_Prescp_rate = round(mean_AB_Prescp_rate,1)
              
    ) 
  #mutate(total_distinct= sum(Count)) %>% 
  #adorn_totals()
  
  
  ###########################################
  mean_prescribing_rate_by_vul_cat_rev_22 <- CMS_SVI_2022_rev_4 %>%
    group_by(Vul_Cat) %>%
    summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))
  
  # final code
  
  CMS_SVI_Rurality_2022 <- CMS_SVI_Rurality_2022_urbanco_3 %>% 
    mutate(Vul_Cat = factor(Vul_Cat, levels = c("Low",
                                                "Moderate",
                                                "High"))) %>% 
    arrange(Vul_Cat)
  
 ######################################################### ignore ################################################## 
  # Statistical Analysis
  
  # Two independent sample t test
  
  #low
  # low_t_2022 <- CMS_SVI_Rurality_2022 %>% 
  #   filter(Vul_Cat == "low") %>% 
  #   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
  #   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
  #   select(2,3) %>% 
  #   t.test()
  # 
  # t_p_value_lowt_2022 <- data.frame(low = round(low_t_2022$p.value,2)) # pvalue = 0.03
  # 
  # 
  # # low_medium
  # 
  # low_m_t_2022 <- CMS_SVI_Rurality_2022 %>% 
  #   filter(Vul_Cat == "low-medium") %>% 
  #   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
  #   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
  #   select(2,3) %>% 
  #   t.test()
  # 
  # t_p_value_low_mt_2022 <- data.frame(`low-medium` = round(low_m_t_2022$p.value,2)) # pvalue = 0.01
  # 
  # 
  # #medium-high
  # 
  # medium_high_t_2022 <- CMS_SVI_Rurality_2022 %>% 
  #   filter(Vul_Cat == "medium-high") %>% 
  #   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
  #   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
  #   select(2,3) %>% 
  #   t.test()
  # 
  # t_p_value_med_h_2022 <- data.frame(`medium-high` = round(medium_high_t_2022$p.value,2)) # pvalue = 0.01
  # 
  # 
  # #high
  # 
  # high_t_2022 <- CMS_SVI_Rurality_2022 %>% 
  #   filter(Vul_Cat == "high") %>% 
  #   mutate(mean_AB_Prescp_rate = round(mean_AB_Prescp_rate)) %>% 
  #   pivot_wider(names_from = Setting, values_from = mean_AB_Prescp_rate) %>% 
  #   select(2,3) %>% 
  #   t.test()
  # 
  # t_p_value_high_2022 <- data.frame(high = round(high_t_2022$p.value,2)) # pvalue = 0.04
  # 
  # 
  # SVI_rurality_2022 <-  cbind(t_p_value_lowt_2022,t_p_value_low_mt_2022,t_p_value_med_h_2022,t_p_value_high_2022) %>% 
  #   pivot_longer(
  #     cols = c(low, low.medium, medium.high, high),
  #     names_to = "Vul_Cat",
  #     values_to = "p_values") %>% 
  #   mutate(Vul_Cat =case_when(Vul_Cat ==  "low.medium" ~ "low-medium",
  #                             Vul_Cat ==  "medium.high" ~ "medium-high", T~ Vul_Cat))
  # 
  # colnames(SVI_rurality)
  # 
  # # horizontal bars
  # 
  # 
  # CMS_SVI_Rurality_2022_a <- CMS_SVI_Rurality_2022 %>% 
  #   left_join(SVI_rurality_2022, by = "Vul_Cat") %>% 
  #   mutate(p_values = as.character(p_values),
  #          p_values = case_when(Vul_Cat == "low" & Setting == "Urban" ~ "",
  #                               Vul_Cat == "low-medium" & Setting == "Urban" ~ "",
  #                               Vul_Cat == "medium-high" & Setting == "Urban" ~ "",
  #                               Vul_Cat == "high" & Setting == "Urban" ~ "",
  #                               T~ p_values),
  #          p_values = ifelse(p_values != "", paste("p-value =", p_values), "")) %>% 
  #   mutate(Vul_Cat = factor(Vul_Cat, levels = c("high",
  #                                               "medium-high",
  #                                               "low-medium",
  #                                               "low"))) 
  # 
  # 
  # 
  # ggplot(data = CMS_SVI_Rurality_2022_a, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
  #   geom_bar(stat = "identity", position = "dodge") +
  #   geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
  #             position = position_dodge(width = 0.9), 
  #             hjust = -0.1, size = 4.0) + 
  #   geom_text(aes(label = p_values),
  #             position = position_dodge(width =0.9), 
  #             hjust = ifelse(CMS_SVI_Rurality_2022_a$Vul_Cat =="medium-high",-2.55,
  #                            ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.04",-1.1,
  #                                   ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.01",-1.85,-0.6))),
  #             vjust = -1.0,size = 4.0)+# Adjust hjust for horizontal bars
  #   labs(fill = NULL,
  #        title = "",
  #        x = "Antibiotic Prescribing Rate",
  #        y = "SVI levels") +
  #   scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
  #   theme_minimal() +
  #   theme(axis.title.y = element_blank(),
  #         axis.text.y = element_text(size = 12, hjust = 0),
  #         axis.text.x = element_text(size = 12, hjust = 0),
  #         legend.title = element_blank(),
  #         legend.text = element_text(size = 12, hjust = 0),
  #         plot.title = element_text(size = 12, hjust = 0),
  #         legend.position = "top",
  #         panel.grid = element_blank()) +
  #   
  #   scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
  # coord_flip()
  # 
  # 
  
  
 ######################################### resume #################################################################### 
  
  
  ggplot(data = CMS_SVI_Rurality_2022, aes(x = mean_AB_Prescp_rate, y = Vul_Cat, fill = Setting)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(mean_AB_Prescp_rate)),  # Round the values here
              position = position_dodge(width = 0.9), 
              hjust = -0.1, size = 4.0) + 
    # geom_text(aes(label = p_values),
    #           position = position_dodge(width =0.9), 
    #           hjust = ifelse(CMS_SVI_Rurality_2022_a$Vul_Cat =="medium-high",-2.55,
    #                          ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.04",-1.1,
    #                                 ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.01",-1.85,-0.6))),
    #           vjust = -1.0,size = 4.0)+# Adjust hjust for horizontal bars
    labs(fill = NULL,
         title = "",
         x = "Antibiotic Prescribing Rate",
         y = "SVI levels") +
    scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
    theme_minimal() +
    theme(axis.title.y = element_text(size = 12, vjust = 1),
          axis.text.y = element_text(size = 12, hjust = 0),
          axis.text.x = element_text(size = 12, hjust = 0),
          legend.title = element_blank(),
          legend.text = element_text(size = 12, hjust = 0),
          plot.title = element_text(size = 12, hjust = 0),
          legend.position = "top",
          panel.grid = element_blank()) +
    
    scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
  coord_flip()
  
  

########################### Combined ##########################

##### Mean of all three years

# 1. combine all 3 SVI categories mean prescribing rates across 3 years

#2020  
mean_prescribing_rate_by_vul_cat_rev <- mean_prescribing_rate_by_vul_cat_rev %>% 
    rename(mean_AB_Prescp_rate_20 = mean_AB_Prescp_rate)

#2021
mean_prescribing_rate_by_vul_cat_21 <- mean_prescribing_rate_by_vul_cat_21 %>% 
    rename(mean_AB_Prescp_rate_21 = mean_AB_Prescp_rate)

#2022  
mean_prescribing_rate_by_vul_cat_22 <- mean_prescribing_rate_by_vul_cat_22 %>% 
  #rename(mean_AB_Prescp_rate = mean_AB_Prescp_rate)
    rename(mean_AB_Prescp_rate_22 = mean_AB_Prescp_rate)

# combine
SVI_2020_22<-   cbind(mean_prescribing_rate_by_vul_cat_rev ,mean_prescribing_rate_by_vul_cat_21, mean_prescribing_rate_by_vul_cat_22)%>%
  select(1,2,4,6) %>% 
    mutate(Mean_ab_rate_SVI_20_22 = round(rowMeans(across(c(mean_AB_Prescp_rate_20,
                                                                 mean_AB_Prescp_rate_21, 
                                                                 mean_AB_Prescp_rate_22)), 
                                                        na.rm = TRUE))) %>% 
  select(1,5)
  

# Overall ANOVA not suitable with less observations

SVI_2020_22$Vul_Cat <- as.factor(SVI_2020_22$Vul_Cat)
  
anova_result <- aov(Mean_ab_rate_SVI_20_22 ~ Vul_Cat, data = SVI_2020_22)

summary(anova_result)

p_value <- Anova_summary[[1]][["Pr(>F)"]][1]

p_value <- data.frame(p_value)



###############3 Linelevel updated 12/20/2024 ########################


##########3 Statistical analysis in report ####################

## only SVI

CMS_SVI_2020_rev_4$Year <- "2020"
CMS_SVI_2021_rev_4$Year <- "2021"
CMS_SVI_2022_rev_4$Year <- "2022"

CMS_SVI_2022_rev_4 <- CMS_SVI_2022_rev_4 %>% 
  rename(PRSCRBR_NPI=Prscrbr_NPI)


CMS_SVI_2020_22_rev_4 <- rbind(CMS_SVI_2020_rev_4,CMS_SVI_2021_rev_4,CMS_SVI_2022_rev_4) %>% 
  mutate(AB_Prescp_rate = round(AB_Prescp_rate,2))


CMS_SVI_2020_22_rev_4$Vul_Cat <- as.factor(CMS_SVI_2020_22_rev_4$Vul_Cat)

anova_result <- aov(AB_Prescp_rate ~ Vul_Cat, data = CMS_SVI_2020_22_rev_4)

summary(anova_result)

tukey_result <- TukeyHSD(anova_result)

summary(tukey_result)

print(tukey_result)


##############################################################################################################################################3
  
CMS_SVI_Rurality_2020 <- CMS_SVI_Rurality_2020 %>% 
  rename(mean_AB_Prescp_rate_20 = mean_AB_Prescp_rate) %>% 
  select(1,2,5)


CMS_SVI_Rurality_2021<- CMS_SVI_Rurality_2021 %>% 
  rename(mean_AB_Prescp_rate_21 = mean_AB_Prescp_rate) %>% 
  select(5)

CMS_SVI_Rurality_2022_ab <- CMS_SVI_Rurality_2022 %>% 
  rename(mean_AB_Prescp_rate_22 = mean_AB_Prescp_rate) %>% 
  select(5)


CMS_SVI_Rurality_20_22 <- cbind(CMS_SVI_Rurality_2020,CMS_SVI_Rurality_2021,CMS_SVI_Rurality_2022_ab) %>% 
  select(Setting =1,2,3,5,7) %>% 
  #mutate(Mean_ab_rate_rurality_20_22 = mean(c_across(c(mean_AB_Prescp_rate_20,mean_AB_Prescp_rate_21,mean_AB_Prescp_rate_22))))
  mutate(Mean_ab_rate_rurality_20_22 = round(rowMeans(across(c(mean_AB_Prescp_rate_20,
                                                               mean_AB_Prescp_rate_21, 
                                                               mean_AB_Prescp_rate_22)), 
                                                      na.rm = TRUE)))

# Rurality

CMS_SVI_2020_rev_setting_3 <- CMS_SVI_2020_rev_setting_3 %>% 
  mutate(Year = 2021)

CMS_SVI_2021_rev_setting_3 <- CMS_SVI_2021_rev_setting_3 %>% 
  mutate(Year = 2021)

CMS_SVI_2022_rev_setting_3 <- CMS_SVI_2022_rev_setting_3 %>% 
  mutate(Year = 2022) %>% 
  rename(PRSCRBR_NPI=Prscrbr_NPI)

CMS_SVI_2020_22_rev_4_rurality <- rbind(CMS_SVI_2020_rev_setting_3,CMS_SVI_2021_rev_setting_3,CMS_SVI_2022_rev_setting_3) %>% 
  mutate(AB_Prescp_rate = round(AB_Prescp_rate,2))
  

# T Test
CMS_SVI_2020_22_rev_4_rurality$Setting <- as.factor(CMS_SVI_2020_22_rev_4_rurality$Setting)

Ttest_rurality_SVI <-t.test(AB_Prescp_rate ~ Setting, data = CMS_SVI_2020_22_rev_4_rurality)

print(Ttest_rurality_SVI)


#############################################################################################################33


ggplot(data = CMS_SVI_Rurality_20_22, aes(x = Mean_ab_rate_rurality_20_22, y = Vul_Cat, fill = Setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean_ab_rate_rurality_20_22)),  # Round the values here
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 5) + 
  # geom_text(aes(label = p_values),
  #           position = position_dodge(width =0.9), 
  #           hjust = ifelse(CMS_SVI_Rurality_2022_a$Vul_Cat =="medium-high",-2.55,
  #                          ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.04",-1.1,
  #                                 ifelse(CMS_SVI_Rurality_2022_a$p_values =="p-value = 0.01",-1.85,-0.6))),
  #           vjust = -1.0,size = 4.0)+# Adjust hjust for horizontal bars
  labs(fill = NULL,
       title = "",
       x = "Antibiotic Prescribing Rate",
       y = "SVI levels") +
  scale_x_continuous(breaks = seq(0, 700, by = 200), limits = c(0, 700)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 14, vjust = 2),
        axis.title.x = element_text(size = 14, vjust = 1),
        axis.text.y = element_text(size = 14, vjust = 0.5,hjust = 1.3, color = "black"),
        axis.text.x = element_text(size = 14,vjust = 1, hjust = 0, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, hjust = 0),
        plot.title = element_text(size = 14, hjust = 0),
        legend.position = "top",
        panel.grid = element_blank()) +
  
  scale_fill_manual(values = c("#4a90e2","#d0e1f2", "#d0e1f2","#4a90e2", "#d0e1f2","#4a90e2","#d0e1f2","#4a90e2"))  # Customize colors as needed
coord_flip()


###### combined for maps across all three years #######

# 2020 -2021 combined 
# 2022 represented separately

######################### 11/29 full join 2020-2022 ############################
prescribing_rate_by_county_rev_2020_22 <- prescribing_rate_by_county_rev %>% 
  full_join(prescribing_rate_by_county_rev_21, by = c("COUNTY_NAME", "Vul_Cat")) %>% 
  full_join(prescribing_rate_by_county_rev_22, by = c("COUNTY_NAME")) %>% 
  rename(Vul_cat_20_21 = Vul_Cat.x,Vul_cat_22 = Vul_Cat.y) 
  #select(1,3,6,2,4,5)

#Cameron County,	Jefferson County,Schuylkill County,Sullivan County  these counties in 2022 are have different VUL_cat categories
# SVI_2020 , SVI_2022

### 2020-2021

prescribing_rate_by_county_rev_2020_22 <- prescribing_rate_by_county_rev_2020_22 %>%
  mutate(
    num_prescribers = as.numeric(num_prescribers),
    num_prescribers_21 = as.numeric(num_prescribers_21),
    num_prescribers_22 = as.numeric(num_prescriber_22)) %>% 
  mutate(Mean_rate_vul_cat_20_21 = round(rowMeans(across(c(mean_AB_Prescp_rate, 
                                                           mean_AB_Prescp_rate_21)), 
                                                  na.rm = TRUE), 1))

Mean_rate_by_county_rev_2020_21 <- prescribing_rate_by_county_rev_2020_22 %>%
  rowwise() %>%
  mutate(mean_prescribers_3_years = mean(c(num_prescribers, num_prescribers_21, num_prescribers_22), na.rm = TRUE),
         fill_color = ifelse(mean_prescribers_3_years < 5, NA, Mean_rate_vul_cat_20_21)) %>%
  ungroup()

# Mean_rate_by_county_rev_2020_21 <- prescribing_rate_by_county_rev_2020_22 %>% 
#   select(1,2,4,5) %>% 
#   mutate(Mean_rate_vul_cat_20_21 = round(rowMeans(across(c(mean_AB_Prescp_rate, 
#                                                            mean_AB_Prescp_rate_21)), 
#                                           na.rm = TRUE), 1)
         
         
         
         
#Mapping
library(sf)
library(tigris)

pa_counties_20_21 <- counties(state = "PA", year = 2020, class = "sf") %>% 
  mutate(NAMELSAD = case_when(NAMELSAD == "McKean County" ~ "Mckean County", T ~ NAMELSAD))


#Merge the prescribing rate data with the Pennsylvania county 

pa_county_data_20_21 <- pa_counties_20_21 %>%
  left_join(Mean_rate_by_county_rev_2020_21, by = c("NAMELSAD" = "COUNTY_NAME")) %>% 
  mutate(NAME = str_remove(NAMELSAD, " County"))


#Color palette for the mean prescribing rate
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Symbols for vulnerability categories
vul_cat_symbols <- c("Low" = 16,  "Moderate" = 15, "High" = 8)

pa_county_data_20_21 <- pa_county_data_20_21 %>%
  mutate(
    Vul_cat_20_21 = factor(Vul_cat_20_21, levels = c("High", "Moderate", "Low")))


pa_county_data_20_21 <- pa_county_data_20_21 %>% 
  mutate(Vul_cat_20_21 = case_when(is.na(Vul_cat_20_21)~ "",
                             T ~Vul_cat_20_21))


pa_county_data_20_21 <- pa_county_data_20_21 %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_coordinates(.x)[1]),
         lat = map_dbl(centroid, ~st_coordinates(.x)[2]))

symbol_offset <- 0.075
pa_county_data_20_21 <- pa_county_data_20_21%>%
  mutate(lon_offset = lon + symbol_offset,
         lat_offset = lat - symbol_offset) 

pa_county_data_20_21_filtered <- pa_county_data_20_21 %>%
  filter(!is.na(lon_offset) & !is.na(lat_offset))

#### 2020 -2021


ggplot(data = pa_county_data_20_21) +
  geom_sf(aes(fill = fill_color), color = "black") +  
  geom_sf_text(aes(label = NAME), size = 4.5, color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_cat_20_21), size = 3.5, color = "red") + 
  scale_fill_gradientn(
    colors = c("#deebf7", palette), 
    name = "Prescribing Rate",
    na.value = "grey", 
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),       # Remove axis titles
    axis.text = element_blank(),        # Remove axis numbers
    axis.ticks = element_blank(),       # Remove axis ticks
    panel.grid = element_blank(),       # Remove grid lines
    panel.background = element_blank(), # Remove background
    legend.position = "right",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12))



ggplot(data = pa_county_data_20_21) +
  geom_sf(aes(fill = Mean_rate_vul_cat_20_21)) + 
  geom_sf_text(aes(label = NAME), size = 3, color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_cat_20_21), size = 2, color = "red") + 
  scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "")+
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 9)) 


ggplot(data = pa_county_data_20_21) +
  geom_sf(aes(fill = fill_color), color = "black") +  
  geom_sf_text(aes(label = NAME), size = 3, color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_cat_20_21), size = 2, color = "red") + 
  scale_fill_gradientn(
    colors = c("grey", palette),  
    name = "Prescribing Rate",
    na.value = "grey", 
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )





#################################################################################################

#Mapping
library(sf)
library(tigris)

pa_counties_22 <- counties(state = "PA", year = 2022, class = "sf") %>% 
  mutate(NAMELSAD = case_when(NAMELSAD == "McKean County" ~ "Mckean County", T ~ NAMELSAD))


#Merge the prescribing rate data with the Pennsylvania county 

pa_county_data_22 <- pa_counties_22 %>%
  left_join(prescribing_rate_by_county_rev_22, by = c("NAMELSAD" = "COUNTY_NAME")) %>% 
  mutate(NAME = str_remove(NAMELSAD, " County"),
         num_prescribers_22 = as.numeric(num_prescriber_22))


pa_county_data_22 <- pa_county_data_22 %>%
  rowwise() %>%
  mutate(
         fill_color = ifelse(num_prescribers_22 < 5, NA, mean_AB_Prescp_rate_22))



#Color palette for the mean prescribing rate
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Symbols for vulnerability categories
vul_cat_symbols <- c("Low" = 16,  "Moderate" = 15, "High" = 8)

pa_county_data_22 <- pa_county_data_22 %>%
  mutate(
    Vul_Cat = factor(Vul_Cat, levels = c("High", "Moderate", "Low")))


pa_county_data_22 <- pa_county_data_22 %>% 
  mutate(Vul_Cat = case_when(is.na(Vul_Cat)~ "",
                             T ~Vul_Cat))

pa_county_data_22 <- pa_county_data_22 %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_coordinates(.x)[1]),
         lat = map_dbl(centroid, ~st_coordinates(.x)[2]))

symbol_offset <- 0.075
pa_county_data_22 <- pa_county_data_22%>%
  mutate(lon_offset = lon + symbol_offset,
         lat_offset = lat - symbol_offset) 




ggplot(data = pa_county_data_22) +
  geom_sf(aes(fill = fill_color), color = "black") +
  geom_sf_text(aes(label = NAME), size = 4.5, color = "black") +
  geom_point(aes(x = lon_offset,
                 y =lat_offset,
                 shape = Vul_Cat),
             size = 3.5, color = "red") + 
  scale_fill_gradientn(
    colors = c("#deebf7", palette),  
    na.value = "grey",  
    name = "Prescribing Rate",
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis numbers
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank(), # Remove background
    legend.position = "right",
    legend.text = element_text(size = 11),  
    legend.title = element_text(size = 12))























svi_map_22 <- ggplot(data = svi_map_22 <- 
                       
                       ggplot(data = pa_county_data_22) +
  geom_sf(aes(fill = mean_AB_Prescp_rate_22)) + 
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

  
  
  ### less than 5 prescribers
  
  ggplot(data = pa_county_data_22) +
    geom_sf(aes(fill = fill_color), color = "black") +
    geom_sf_text(aes(label = NAME), size = 3, color = "black", fontface = "bold") +
    geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
    scale_fill_gradientn(
      colors = c("grey", palette),  
      na.value = "grey",  
      guide = guide_colorbar(barwidth = 0.5, barheight = 10)
    ) +  
    scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
    labs(title = "") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),  
      legend.title = element_text(size = 9)
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

ggsave("2022 SVI map rev.png",svi_map_22,width =10, height = 8, dpi = 300)) +
  geom_sf(aes(fill = mean_AB_Prescp_rate_22)) + 
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



#### Kranthi test - Montour county and axis removed ####

ggplot(data = pa_county_data_22) +
  geom_sf(aes(fill = fill_color), color = "black") +
  geom_sf_text(aes(label = NAME), size = 3, color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
  scale_fill_gradientn(
    colors = c("#deebf7", palette),  
    na.value = "grey",  
    name = "Prescribing Rate",
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis numbers
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_blank(), # Remove background
    legend.position = "right",
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 9))


ggplot(data = pa_county_data_20_21) +
  geom_sf(aes(fill = fill_color), color = "black") +  
  geom_sf_text(aes(label = NAME), size = 3, color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_cat_20_21), size = 2, color = "red") + 
  scale_fill_gradientn(
    colors = c("#deebf7", palette), 
    name = "Prescribing Rate",
    na.value = "grey", 
    guide = guide_colorbar(barwidth = 0.5, barheight = 10)
  ) +
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),       # Remove axis titles
    axis.text = element_blank(),        # Remove axis numbers
    axis.ticks = element_blank(),       # Remove axis ticks
    panel.grid = element_blank(),       # Remove grid lines
    panel.background = element_blank(), # Remove background
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9))

subset_22 <- pa_county_data_22[, c("Vul_Cat", "fill_color", "NAMELSAD")]

sorted_22 <- subset_22[order(subset_22$fill_color), ]
lowest_22 <- head(sorted_22, 2)
highest_22 <- tail(sorted_22, 2)
table(subset_22$Vul_Cat)


subset_2021 <- pa_county_data_20_21[, c("Vul_cat_20_21", "fill_color", "NAMELSAD")]

sorted_2021 <- subset_2021[order(subset_2021$fill_color), ]
lowest_2021 <- head(sorted_2021, 2)
highest_2021 <- tail(sorted_2021, 2)
table(subset_2021$Vul_cat_20_21)










ggsave("2022 SVI map rev.png",svi_map_22,width =10, height = 8, dpi = 300)) +
  geom_sf(aes(fill = mean_AB_Prescp_rate_22)) + 
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





















# 
# ################################################# bind all years  -- Ignore ###############
# 
# pa_county_data_20_21 <- st_transform(pa_county_data_21, st_crs(pa_county_data))
# 
# pa_county_data_20_22 <-pa_county_data %>% 
#   st_join(pa_county_data_21 )
# 
# pa_county_data_21 <- st_transform(pa_county_data_21, st_crs(pa_county_data))
# pa_county_data_22 <- st_transform(pa_county_data_22, st_crs(pa_county_data))
# 
# # First, join 2021 data to 2020 data by spatial geometry
# merged_data_20_21 <- st_join(pa_county_data, pa_county_data_21)
# 
# merged_data_F <- merged_data_20_21 %>%
#   distinct(NAME.x, NAME.y, .keep_all = TRUE) %>% 
#   select(NAME.x, NAME.y, Vul_Cat.x,Vul_Cat.y,mean_AB_Prescp_rate,mean_AB_Prescp_rate_21)
# 
# 
# merged_data_F <- merged_data_20_21 %>%
#   # Group by NAME.x and NAME.y
#   group_by(NAME.x, NAME.y) %>%
#   # Summarize the necessary columns
#   summarize(
#     Vul_Cat.x = first(Vul_Cat.x),  # Take the first value of Vul_Cat.x for each group
#     Vul_Cat.y = first(Vul_Cat.y),  # Take the first value of Vul_Cat.y for each group
#     mean_AB_Prescp_rate = mean(mean_AB_Prescp_rate, na.rm = TRUE),  # Calculate mean for the rate
#     mean_AB_Prescp_rate_21 = mean(mean_AB_Prescp_rate_21, na.rm = TRUE),  # Calculate mean for 2021 rate
#     .groups = 'drop'  # Drop the grouping after summarizing
#   ) %>%
#   select(NAME.x, NAME.y, Vul_Cat.x, Vul_Cat.y, mean_AB_Prescp_rate, mean_AB_Prescp_rate_21)
# 
# 
# 
# 
# # Check CRS for both datasets
# st_crs(pa_county_data)
# st_crs(pa_county_data_21)
# 
# # If they are different, align the CRS by transforming one to the other
# pa_county_data_21 <- st_transform(pa_county_data_21, st_crs(pa_county_data))
# 
# pa_county_data_20_aligned <- st_intersection(pa_county_data, pa_county_data_21)
# pa_county_data_21_aligned <- st_intersection(pa_county_data_21, pa_county_data)
# 
# pa_county_data_20_21 <- st_join(pa_county_data, pa_county_data_21)
# 
# 
# pa_county_data_20 <- st_make_valid(pa_county_data)
# pa_county_data_21 <- st_make_valid(pa_county_data_21)
# 
# merged_data <- st_join(pa_county_data, pa_county_data_21, join = st_within) %>% 
#   select(NAME.x, NAME.y, Vul_Cat.x,Vul_Cat.y,mean_AB_Prescp_rate,mean_AB_Prescp_rate_21)
# 
# merged_data_2 <- st_join(pa_county_data, pa_county_data_21, join = st_contains) %>% 
# select(NAME.x, NAME.y, Vul_Cat.x,Vul_Cat.y,mean_AB_Prescp_rate,mean_AB_Prescp_rate_21)
# 
# intersections <- st_intersects(pa_county_data, pa_county_data_21)
# table(lengths(intersections)) 
# 
# # Then, join 2022 data to the result from the first join
# merged_data_20_22 <- st_join(merged_data_20_21, pa_county_data_22))
# 
# 
# pa_county_data_20_22 <- pa_county_data_20_22 %>% 
#   st_join(pa_county_data_22)
# ###########################################################################################3
# 
# # CMS County- SVI Map 2020
# svi_map_22 <- ggplot(data = pa_county_data_20_22) +
#   geom_sf(aes(fill = mean_AB_Prescp_rate_22)) + 
#   geom_sf_text(aes(label = NAME), size = 3, color = "black", fontface= "bold") +
#   geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
#   scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
#                        guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
#   scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
#   labs(title = "")+
#   theme_minimal() +
#   theme(legend.position = "right",
#         legend.text = element_text(size = 8),  
#         legend.title = element_text(size = 9)) 
# 
# 
# ggsave("2022 SVI map rev.png",svi_map_22,width =10, height = 8, dpi = 300)
# 
# 
# #-------------------------------------------------------------------------------
# pa_county_data <- pa_county_data %>% 
#   mutate(Year = 2020)
# 
# pa_county_data_21 <-pa_county_data_21 %>% 
#   mutate(Year = 2021)
# 
# 
# pa_county_data_22 <- pa_county_data_22%>% 
#   mutate(Year = 2022)
# 
# 
# 
# pa_count_20_22 <- rbind(pa_county_data,pa_county_data_21,pa_county_data_22)
# 
# svi_map_22 <-  ggplot(data = pa_count_20_22) +
#   geom_sf(aes(fill = mean_AB_Prescp_rate)) + 
#   geom_sf_text(aes(label = NAME), size = 3, color = "black") +
#   geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
#   scale_fill_gradientn(colors = palette, name = "Prescribing Rate", 
#                        guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
#   scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
#   facet_wrap(~Year, ncol=1)+
#   labs(title = "")+
#   theme_minimal() +
#   theme(legend.position = "right",
#         legend.text = element_text(size = 8),  
#         legend.title = element_text(size = 9)) 
# 
# 
# ggsave("2022 SVI map rev.png",svi_map_22,width =10, height = 8, dpi = 300)
# 
# 

#----------------------------
# NOTES:
# 2020 low SV - high AB rate
# 2021 low SV - high AB rate (reason?)
# used 2020 SVI for both CMS 2020 & 2021 (nebraska)
# missing values coz of non matching zipcodes in PA & supression values
#  McKean county
# explore more literature


############################################################################# END ###############################################################################################


  
  
  

# Logistic regression (Kranthi)



install.packages("stats")
library(stats)

CMS_SVI_settings_settings <- read_excel("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/SVI_Settings.xlsx")

# glm() with Poisson distribution
model <- glm(AB_Prescp_rate ~ RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4+ offset(log(Tot_Benes)),
             family = poisson(link="log"), data = CMS_SVI_settings)

dispersion_ratio <- summary(model)$deviance/summary(model)$df.residual
dispersion_ratio

#348.85 >1 -- overdispersed

summary(model)

# Quasi-Poisson model
quasi_model <- glm(AB_Prescp_rate ~ RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4 + offset(log(Tot_Benes)), 
                   family = quasi(link = "log", variance = "mu"), data = CMS_SVI_settings)
summary(quasi_model)


#Negative Binomial model

install.packages("MASS")
library(MASS)

# Fit the negative binomial model
nb_model <- glm.nb(AB_Prescp_rate ~ (RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4)* Setting+ offset(log(Tot_Benes)), data = CMS_SVI_settings)
summary(nb_model)

#Negative Binomial Model Interpretation

# Intercept: The estimated log rate of antibiotic prescriptions, holding all other variables at zero, was 2.27423, highly statistically significant with a p-value less than 2e-16. This finding suggests a substantial baseline rate of antibiotic prescriptions when no socio-economic or demographic factors are considered.
# 
# RPL_THEME1 (Socioeconomic Status): The coefficient for socioeconomic status was -0.02063, indicating a non-significant decrease in the rate of antibiotic prescriptions as socioeconomic status increases (p = 0.654094). This result suggests that socioeconomic status, as measured in this study, does not significantly impact the rate of antibiotic prescriptions.
# 
# RPL_THEME2 (Household Characteristics): The model estimated a negative coefficient of -0.02750 for household characteristics. However, this effect was not statistically significant (p = 0.563787), indicating that the prescription rates do not significantly decrease with worsening household characteristics within the study's context.

# RPL_THEME3 (Racial & Ethnic Minority Status): A significant positive association was observed with RPL_THEME3, where the coefficient of 0.40989 (p < 2e-16) indicates that higher ranks in racial and ethnic minority status are significantly associated with increased rates of antibiotic prescriptions. This relationship underscores the impact of racial and ethnic factors on healthcare practices, particularly in prescribing behaviors.
# 
# RPL_THEME4 (Housing Type & Transportation): The analysis showed a significant negative coefficient of -0.15282 (p = 0.000495), suggesting that poorer housing and transportation conditions are associated with lower antibiotic prescription rates. This finding highlights the role of environmental and logistical factors in influencing healthcare access and subsequent prescription behaviors.
# 
# Setting (Interaction term) (Rural & Urban)--- (effect of an RPL them on rates differs by setting) ---
# 
# RPL_THEME1:Setting Urban -- log -- -0.18 exp(-0.18) = 0.84 (1-0.84 =0.16= 16%) : Negative coefficient - one -unit increase in the themes value is associated with a decrease in prescription rate
# ~ rate ratio 0.84 indicates for one unit increase in RPL theme in urban areas, the rate is predicted to decrease by about 16% , yet not statistically significant
# 
# 
# RPL_THEME2:SettingUrban : rate ratio 0.31284 --- exp(0.31284) = 1.37 (1.37-1= 0.37 = 37% increase in rates per beneficiaries with one unit increase in theme 2 in urban areas) -- and this is statistically significant


# Stepwise selection:(both backward & forward)
# 
nb_model <- glm.nb(AB_Prescp_rate ~ RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4+ Setting+ offset(log(Tot_Benes)), data = CMS_SVI_settings)
summary(nb_model)
# 
# 
# stepwise_model <- stepAIC(nb_model, direction = "both")
# summary(stepwise_model)
# 
# Final model = goodness of fit: RPL_THEME 3 & 4  significant predictors.


#Forward only:

#intercept only  no preictors
nb_model_full <- glm.nb(AB_Prescp_rate ~ RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4+ Setting+ offset(log(Tot_Benes)), data = CMS_SVI_settings)

nb_model_inter <- glm.nb(AB_Prescp_rate ~ 1+ offset(log(Tot_Benes)), data = CMS_SVI_settings)
forward_selection <-  stepAIC(nb_model_inter, direction = "forward", scope = nb_model_full)
summary(forward_selection)






# Kranthi:


library(stats)
library(read)
library(MASS)

CMS_SVI <- read_excel("N:/Kranthi Koonisetty/SVI_Settings.xlsx")


#####Negative Binomial + Forward Selection + Individual themes########

# Define the outcome variable
outcome <- "AB_Prescp_rate"

# Define the predictors
predictors <- c("RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "Setting")

null_model <- glm.nb(as.formula(paste("AB_Prescp_rate ~ offset(log(Tot_Benes))")), data = CMS_SVI)
full_formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + "), "+ offset(log(Tot_Benes))"))
full_model <- glm.nb(full_formula, data = CMS_SVI)

# Perform forward selection
step_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), 
                      direction = "forward", trace = TRUE)
summary(step_model)


# Set 'Urban' as the baseline for 'Setting'
CMS_SVI$Setting <- relevel(as.factor(CMS_SVI$Setting), ref = "Urban")

# Fit the negative binomial model with the updated baseline
step_model <- glm.nb(AB_Prescp_rate ~ Setting + RPL_THEME4 + RPL_THEME3 + offset(log(Tot_Benes)), 
                     data = CMS_SVI, link = log)

# Display the summary of the model
summary(step_model)

# Set 'Urban' as the baseline for 'Setting'
CMS_SVI$Setting <- relevel(as.factor(CMS_SVI$Setting), ref = "Urban")

# Fit the negative binomial model with the updated baseline
step_model <- glm.nb(AB_Prescp_rate ~ Setting + RPL_THEME4 + RPL_THEME3 + offset(log(Tot_Benes)), 
                     data = CMS_SVI, link = log)

# Display the summary of the model
summary(step_model)


#####Negative Binomial + Forward Selection + Overall theme########

# Outcome variable
outcome <- "AB_Prescp_rate"

# Define the predictors (only RPL_THEMES and Setting)
predictors2 <- c("RPL_THEMES", "Setting")

# Modify the dataframe name to CMS_SVI
# Ensure the data frame CMS_SVI exists with required columns

# Define the null and full models with only the specified predictors
null_model_2 <- glm.nb(as.formula(paste("AB_Prescp_rate ~ offset(log(Tot_Benes))")), data = CMS_SVI)
full_formula_2 <- as.formula(paste("AB_Prescp_rate ~", paste(predictors2, collapse = " + "), "+ offset(log(Tot_Benes))"))
full_model_2 <- glm.nb(full_formula_2, data = CMS_SVI)

# Perform forward selection
step_model_2 <- stepAIC(null_model_2, scope = list(lower = null_model_2, upper = full_model_2), 
                        direction = "forward", trace = TRUE)

# Display the summary of the final selected model
summary(step_model_2)

########################Code to run model with one variable at a time#########################

# Define the outcome and predictors
outcome <- "AB_Prescp_rate"
predictors <- c("RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "Setting")

# Loop through each predictor and fit a negative binomial model
for (predictor in predictors) {
  # Define the formula for the current predictor with offset
  formula <- as.formula(paste(outcome, "~", predictor, "+ offset(log(Tot_Benes))"))
  
  # Fit the negative binomial model
  model <- glm.nb(formula, data = CMS_SVI)
  
  # Display the summary of the model
  cat("\n\nModel with predictor:", predictor, "\n")
  print(summary(model))
}

















