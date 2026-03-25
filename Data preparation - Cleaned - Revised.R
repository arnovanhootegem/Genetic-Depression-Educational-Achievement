##################################################################################
########################### SCRIPT FOR ##########################################

#### Genetic Risk for Depression and Educational Achievement in Adolescence #####

#### By Arno Van Hootegem, Tina Baier, Jan Paul Heisig and Torkild Lyngstad ######
###################################################################################


library(here)
library(tidyverse)
library(dplyr)
library(data.table)
library(haven)
library(estimatr)
library(fixest)
library(sandwich)
library(lmtest)
library(lavaan)
library(estimatr)
library(gtsummary)

# Loading in population and person year data ---------------------------------------------

# Population data

basic <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_FASTE_OPPLYSNINGER.csv")
basic$foedselsaar <- as.numeric(substr(basic$foedsels_aar_mnd, 1, 4))
basic <- filter(basic, foedselsaar > 2001 & foedselsaar < 2010)

faste <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/POPULATION_SOCIAL/csv/POPULATION_FASTE_OPPLYSNINGER.csv")
mor <- select(faste, w19_0634_lnr, foedsels_aar_mnd, fodeland)
mor$mother_fodeland <- mor$fodeland
mor$mother_w19_0634_lnr <- mor$w19_0634_lnr
mor$lopenr_mor <- mor$w19_0634_lnr
mor$mother_cohort <- as.numeric(substr(mor$foedsels_aar_mnd, 1, 4))
basic <- left_join(basic, select(mor, mother_fodeland, mother_cohort, mother_w19_0634_lnr, lopenr_mor))

far <- select(faste, w19_0634_lnr, foedsels_aar_mnd, fodeland)
far$father_fodeland <- far$fodeland
far$father_w19_0634_lnr <- far$w19_0634_lnr
far$lopenr_far <- far$w19_0634_lnr
far$father_cohort <- as.numeric(substr(far$foedsels_aar_mnd, 1, 4))
basic <- left_join(basic, select(far, father_fodeland, father_cohort, father_w19_0634_lnr, lopenr_far))


# Person year data

personyear <- fread("N:/durable/projects/openflux/projects/tina/tina.csv")

personyear <- select(personyear, year, child_female, 
                     child_cohort, child_fodeland,
                     child_PC1:child_PC10,
                     child_Depression_Howard_2019_PGS, child_EA4_excl23andMe_mobaref_PGS,
                     child_w19_0634_lnr,
                     father_Depression_Howard_2019_PGS, father_EA4_excl23andMe_mobaref_PGS,
                     mother_Depression_Howard_2019_PGS, mother_EA4_excl23andMe_mobaref_PGS)

personyear$w19_0634_lnr <- personyear$child_w19_0634_lnr
personyear <- left_join(personyear, select(basic, w19_0634_lnr, mother_fodeland, mother_cohort, mother_w19_0634_lnr, father_fodeland, father_cohort, father_w19_0634_lnr))

personyear <- distinct(personyear)
stable <- personyear %>% select(-year) %>% distinct()

# Add extra person years until 2022

quadruple_rows <- function(df) {
  df[rep(1:nrow(df), each = 4), ]
}

extra_data <- quadruple_rows(stable)

extra_data <- extra_data %>% 
  group_by(child_w19_0634_lnr) %>% 
  mutate(year = row_number()) %>% 
  ungroup() %>% 
  mutate(year = case_when(
    year == 1 ~ 2019,
    year == 2 ~ 2020,
    year == 3 ~ 2021,
    year == 4 ~ 2022))

personyear <- rbind(personyear, extra_data)

rm(stable, extra_data)

# Selecting European ancestors in population file

basic <- filter(basic, (mother_fodeland == 0 | (mother_fodeland > 100 & mother_fodeland < 162)))
basic <- filter(basic, (father_fodeland == 0 | (father_fodeland > 100 & father_fodeland < 162)))


# Standardizing test scores and matching to person year data-----------------------------------------------

nasjprov <- fread("N:/durable/data/registers/SSB/01_data/data_v6.1/EDUCATION_VGS_GRS/csv/EDUCATION_NASJONALE_PROVER.csv") 

nprover <- select(nasjprov, w19_0634_lnr, provekode, utd_skoleaar_start, poeng, deltattstatus) %>% 
  filter(provekode %in% c("NPENG08","NPREG08","NPLES08","NPENG05","NPREG09","NPLES05","NPREG05","NPLES09")) %>% 
  filter(deltattstatus=="D", poeng>0) %>% 
  group_by(w19_0634_lnr, provekode) %>% 
  arrange(desc(utd_skoleaar_start)) %>% 
  slice(1) %>% 
  group_by(provekode, utd_skoleaar_start) %>% 
  mutate(zresult = scale.default(poeng)) %>% 
  ungroup %>% 
  gather(info, value, -w19_0634_lnr, -provekode) %>% 
  mutate(info = if_else(info=="zresult", "",info)) %>% 
  unite(col = varname, provekode, info, sep="", remove=T) %>% 
  spread(varname, value)

rm(nasjprov)

nprover <- select(nprover, -NPENG05deltattstatus, -NPENG08deltattstatus, -NPLES05deltattstatus,
                  -NPLES08deltattstatus, -NPLES09deltattstatus, 
                  -NPREG05deltattstatus, -NPREG08deltattstatus, -NPREG09deltattstatus)

personyear <- left_join(personyear, nprover, by = "w19_0634_lnr")

personyear$NPENG05AARGANG <- substr(personyear$NPENG05utd_skoleaar_start, 1, 4)
personyear$NPENG05AARGANG <- as.numeric(personyear$NPENG05AARGANG)
personyear$NPENG08AARGANG <- substr(personyear$NPENG08utd_skoleaar_start, 1, 4)
personyear$NPENG08AARGANG <- as.numeric(personyear$NPENG08AARGANG)
personyear$NPLES05AARGANG <- substr(personyear$NPLES05utd_skoleaar_start, 1, 4)
personyear$NPLES05AARGANG <- as.numeric(personyear$NPLES05AARGANG)
personyear$NPLES08AARGANG <- substr(personyear$NPLES08utd_skoleaar_start, 1, 4)
personyear$NPLES08AARGANG <- as.numeric(personyear$NPLES08AARGANG)
personyear$NPLES09AARGANG <- substr(personyear$NPLES09utd_skoleaar_start, 1, 4)
personyear$NPLES09AARGANG <- as.numeric(personyear$NPLES09AARGANG)
personyear$NPREG05AARGANG <- substr(personyear$NPREG05utd_skoleaar_start, 1, 4)
personyear$NPREG05AARGANG <- as.numeric(personyear$NPREG05AARGANG)
personyear$NPREG08AARGANG <- substr(personyear$NPREG08utd_skoleaar_start, 1, 4)
personyear$NPREG08AARGANG <- as.numeric(personyear$NPREG08AARGANG)
personyear$NPREG09AARGANG <- substr(personyear$NPREG09utd_skoleaar_start, 1, 4)
personyear$NPREG09AARGANG <- as.numeric(personyear$NPREG09AARGANG)

personyear <- personyear %>%
  group_by(child_w19_0634_lnr) %>%
  mutate(NPENG05 = ifelse(NPENG05AARGANG == year, NPENG05, NA)) %>%
  mutate(NPENG05POENG = ifelse(NPENG05AARGANG == year, NPENG05poeng, NA)) %>%
  mutate(NPENG08 = ifelse(NPENG08AARGANG == year, NPENG08, NA)) %>%
  mutate(NPENG08POENG = ifelse(NPENG08AARGANG == year, NPENG08poeng, NA)) %>%
  mutate(NPLES05 = ifelse(NPLES05AARGANG == year, NPLES05, NA)) %>%
  mutate(NPLES05POENG = ifelse(NPLES05AARGANG == year, NPLES05poeng, NA)) %>%
  mutate(NPLES08 = ifelse(NPLES08AARGANG == year, NPLES08, NA)) %>%
  mutate(NPLES08POENG = ifelse(NPLES08AARGANG == year, NPLES08poeng, NA)) %>%
  mutate(NPLES09 = ifelse(NPLES09AARGANG == year, NPLES09, NA)) %>%
  mutate(NPLES09POENG = ifelse(NPLES09AARGANG == year, NPLES09poeng, NA)) %>%
  mutate(NPREG05 = ifelse(NPREG05AARGANG == year, NPREG05, NA)) %>%
  mutate(NPREG05POENG = ifelse(NPREG05AARGANG == year, NPREG05poeng, NA)) %>%
  mutate(NPREG08 = ifelse(NPREG08AARGANG == year, NPREG08, NA)) %>%
  mutate(NPREG08POENG = ifelse(NPREG08AARGANG == year, NPREG08poeng, NA)) %>%
  mutate(NPREG09 = ifelse(NPREG09AARGANG == year, NPREG09, NA)) %>%
  mutate(NPREG09POENG = ifelse(NPREG09AARGANG == year, NPREG09poeng, NA)) %>%
  ungroup()

rm(nprover)

basic <- left_join(basic, select(nprover, w19_0634_lnr, NPLES09, NPREG09, NPREG09utd_skoleaar_start))
basic$NPREG09AARGANG <- as.numeric(substr(basic$NPREG09utd_skoleaar_start, 1, 4))
basic$ageat09 <- basic$NPREG09AARGANG - basic$foedselsaar
basic$LES09 <- as.numeric(basic$NPLES09)
basic$REG09 <- as.numeric(basic$NPREG09)
basic$grades09 <- rowMeans(basic[ , c("LES09", "REG09")], na.rm = TRUE)


# Include GPA -------------------------------------------------------------

GPA <- fread("N:/durable/data/registers/SSB/01_data/data_v4.0/EDUCATION_VGS_GRS/csv/EDUCATION_TAB_KAR_GRS.csv")
GPA <- select(GPA, w19_0634_lnr, STP, SKR, MUN, AVGDATO)
GPA$year <- as.numeric(substr(GPA$AVGDATO, 1, 4))

GPA$STP <- as.numeric(GPA$STP)
GPA$STP <- na_if(GPA$STP, 7)

GPA$SKR <- as.numeric(GPA$SKR)
GPA$SKR <- na_if(GPA$SKR, 0)

GPA$MUN <- as.numeric(GPA$MUN)
GPA$MUN <- na_if(GPA$MUN, 0)

summary_df <- GPA %>%
  group_by(w19_0634_lnr, year) %>%
  summarize(STPmean = mean(STP, na.rm = TRUE), 
            SKRmean = mean(SKR, na.rm = TRUE),
            MUNmean = mean(MUN, na.rm = TRUE),
            STPcount = sum(!is.na(STP)), 
            SKRcount = sum(!is.na(SKR)), 
            MUNcount = sum(!is.na(MUN))) %>% 
  ungroup()

summary_df$GPA <- rowMeans(summary_df[ , c("STPmean", "SKRmean", "MUNmean")], na.rm = TRUE)
summary_df <- summary_df %>% group_by(year) %>% mutate(GPA = scale.default(GPA)) %>% ungroup()

personyear <- left_join(personyear, select(summary_df, w19_0634_lnr, year, GPA))

rm(GPA, summary_df)

saveRDS(personyear, "N:/durable/users/arnovh/Mental health - educ/01 - Data/cleaned - personyeargenes2.rds")

# Income of parents ----------------------------------

income <- fread("N:/durable/data/registers/SSB/01_data/data_v4.0/INCOME/csv/INCOME_INNTEKT.csv", select = c("w19_0634_lnr","aargang","saminnt")) %>% 
  setNames(nm=c("w19_0634_lnr","year","income"))

income <- filter(income, year > 2001)

income <- income %>% 
  group_by(year, w19_0634_lnr) %>% 
  summarise(incsum = sum(income)) %>% 
  ungroup()

morinc <- select(income, w19_0634_lnr, year, incsum)
morinc <- distinct(morinc)
morinc <- rename(morinc, "mother_w19_0634_lnr" = "w19_0634_lnr")
morinc <- rename(morinc, "mother_incsum" = "incsum")

farinc <- select(income, w19_0634_lnr, year, incsum)
farinc <- distinct(farinc)
farinc <- rename(farinc, "father_w19_0634_lnr" = "w19_0634_lnr")
farinc <- rename(farinc, "father_incsum" = "incsum")

personyear <- left_join(personyear, morinc)
personyear <- left_join(personyear, farinc)

personyear <- mutate(personyear, mother_incsum = ifelse(mother_incsum < 0, NA, mother_incsum))
personyear <- mutate(personyear, mother_incsum = ifelse(year > child_cohort + 15, NA, mother_incsum))
personyear <- mutate(personyear, father_incsum = ifelse(father_incsum < 0, NA, father_incsum))
personyear <- mutate(personyear, father_incsum = ifelse(year > child_cohort + 15, NA, father_incsum))

morincs <- morinc %>% 
  mutate(mother_incsum = ifelse(mother_incsum < 0, NA, mother_incsum)) %>% 
  rename("lopenr_mor" = "mother_w19_0634_lnr")

farincs <- farinc %>% 
  mutate(father_incsum = ifelse(father_incsum < 0, NA, father_incsum)) %>% 
  rename("lopenr_far" = "father_w19_0634_lnr")

basic <- left_join(basic, morincs)
basic <- left_join(basic, farincs)

basic <- filter(basic, (year >= foedselsaar) & (year < foedselsaar + 16))

basic <- basic %>% 
  group_by(w19_0634_lnr) %>% 
  mutate(father_inclong = mean(father_incsum, na.rm = TRUE)) %>% 
  mutate(mother_inclong = mean(mother_incsum, na.rm = TRUE)) %>% 
  ungroup()
  
basic$father_inclong <- basic$father_inclong / 100000
basic <- mutate(basic, father_inclong = ifelse(father_inclong > 30, NA, father_inclong))
basic$mother_inclong <- basic$mother_inclong / 100000
basic <- mutate(basic, mother_inclong = ifelse(mother_inclong > 30, NA, mother_inclong))

rm(income, farinc, morinc, morincs, farincs)


# Education of parents at age of testing --------------------------------------------------------

education <- fread("N:/durable/data/registers/SSB/01_data/data_v4.0/EDUCATION_ADVANCED/csv/EDUCATION_BU_UTD.csv") 

education <- distinct(education)
education <- select(education, w19_0634_lnr, BU_2002, BU_2003, BU_2004,
                    BU_2005, BU_2006, BU_2007, BU_2008, BU_2009,
                    BU_2010, BU_2011, BU_2012, BU_2013, BU_2014, 
                    BU_2015, BU_2016, BU_2017, BU_2018, BU_2019,
                    BU_2020, BU_2021, BU_2022)
educ_long <- gather(education, year, BU, BU_2002:BU_2022, factor_key=TRUE)

educ_long$year <- substr(educ_long$year, 4, 7)
educ_long$year <- as.numeric(educ_long$year)
educ_long$BU <- substr(educ_long$BU, 1, 1)
educ_long$BU <- as.numeric(educ_long$BU)
table(educ_long$BU)
educ_long$BU <- na_if(educ_long$BU, 9)

rm(education)

moreduc <- select(educ_long, w19_0634_lnr, year, BU)
moreduc <- rename(moreduc, "mother_w19_0634_lnr" = "w19_0634_lnr")
moreduc <- rename(moreduc, "mother_BU" = "BU")

fareduc <- select(educ_long, w19_0634_lnr, year, BU)
fareduc <- rename(fareduc, "father_w19_0634_lnr" = "w19_0634_lnr")
fareduc <- rename(fareduc, "father_BU" = "BU")

personyear <- left_join(personyear, moreduc, by = c("mother_w19_0634_lnr", "year"))
personyear <- left_join(personyear, fareduc, by = c("father_w19_0634_lnr", "year"))

personyear <- personyear %>% 
  mutate(eduyrs_mor = case_when(mother_BU==1 ~ 6.0,
                                mother_BU==2 ~ 9.0,
                                mother_BU==3 ~ 10.0,
                                mother_BU==4 ~ 12.0,
                                mother_BU==5 ~ 14.0,
                                mother_BU==6 ~ 16.0,
                                mother_BU==7 ~ 18.0,
                                mother_BU==8 ~ 21.0,
                                TRUE ~ mother_BU)) %>% 
  mutate(eduyrs_mor = ifelse(year > child_cohort + 15, NA, eduyrs_mor)) %>% 
  mutate(eduyrs_far = case_when(father_BU==1 ~ 6.0,
                                father_BU==2 ~ 9.0,
                                father_BU==3 ~ 10.0,
                                father_BU==4 ~ 12.0,
                                father_BU==5 ~ 14.0,
                                father_BU==6 ~ 16.0,
                                father_BU==7 ~ 18.0,
                                father_BU==8 ~ 21.0,
                                TRUE ~ father_BU)) %>% 
  mutate(eduyrs_far = ifelse(year > child_cohort + 15, NA, eduyrs_far)) 
  

personyear <- personyear %>% 
  group_by(year) %>% 
  mutate(eduyears_z_mor = scale.default(eduyrs_mor)) %>% 
  mutate(eduyears_z_far = scale.default(eduyrs_far)) %>% 
  ungroup()

saveRDS(personyear, "N:/durable/users/arnovh/Mental health - educ/01 - Data/cleaned - personyeargeneswithincedu2.rds")

basic <- left_join(basic, moreduc)
basic <- left_join(basic, fareduc)

basic <- basic %>% 
  group_by(w19_0634_lnr) %>% 
  arrange(w19_0634_lnr, desc(mother_BU)) %>% 
  mutate(eduyrs_mor = first(mother_BU)) %>% 
  mutate(eduyrs_mor = case_when(eduyrs_mor==1 ~ 6.0,
                                eduyrs_mor==2 ~ 9.0,
                                eduyrs_mor==3 ~ 10.0,
                                eduyrs_mor==4 ~ 12.0,
                                eduyrs_mor==5 ~ 14.0,
                                eduyrs_mor==6 ~ 16.0,
                                eduyrs_mor==7 ~ 18.0,
                                eduyrs_mor==8 ~ 21.0,
                                TRUE ~ eduyrs_mor)) %>% 
  ungroup()

basic <- basic %>% 
  group_by(w19_0634_lnr) %>% 
  arrange(w19_0634_lnr, desc(father_BU)) %>% 
  mutate(eduyrs_far = first(father_BU)) %>% 
  mutate(eduyrs_far = case_when(eduyrs_far==1 ~ 6.0,
                                eduyrs_far==2 ~ 9.0,
                                eduyrs_far==3 ~ 10.0,
                                eduyrs_far==4 ~ 12.0,
                                eduyrs_far==5 ~ 14.0,
                                eduyrs_far==6 ~ 16.0,
                                eduyrs_far==7 ~ 18.0,
                                eduyrs_far==8 ~ 21.0,
                                TRUE ~ eduyrs_far)) %>% 
  ungroup()

rm(educ_long, moreduc, fareduc)


# Adding survey data ------------------------------------------------------

survey <- read_dta("N:/durable/data/moba/prepared/Full MoBa wide/PDB2601_all_questionnaires_v12.dta")

survey <- mutate(survey, AA1550 = ifelse((AA1550 < 1 | AA1550 > 4), NA, AA1550))
survey <- mutate(survey, AA1551 = ifelse((AA1551 < 1 | AA1551 > 4), NA, AA1551))
survey <- mutate(survey, AA1552 = ifelse((AA1552 < 1 | AA1552 > 4), NA, AA1552))
survey <- mutate(survey, FF253 = ifelse((FF253 < 1 | FF253 > 4), NA, FF253))
survey <- mutate(survey, FF254 = ifelse((FF254 < 1 | FF254 > 4), NA, FF254))
survey <- mutate(survey, FF255 = ifelse((FF255 < 1 | FF255 > 4), NA, FF255))

link <- read_dta("N:/durable/data/moba/prepared/linkage/PDB2601_kobling_SSB_v12_wide.dta")
link$child_w19_0634_lnr <- link$w19_0634_lnrSU2PT_CHILD
link <- select(link, child_w19_0634_lnr, PREG_ID_2601, BARN_NR)
survey <- left_join(survey, link, by = c("PREG_ID_2601", "BARN_NR"))
survey <- select(survey, child_w19_0634_lnr, AA1550, AA1551, AA1552, FF253, FF254, FF255)
survey <- distinct(survey)
survey <- filter(survey, !is.na(child_w19_0634_lnr))
survey <- survey %>% group_by(child_w19_0634_lnr) %>% arrange(child_w19_0634_lnr, desc(AA1550)) %>% slice(1) %>% ungroup()

personyear <- left_join(personyear, survey, by = "child_w19_0634_lnr")

saveRDS(personyear, "N:/durable/projects/36290998_Arno_GenRiskDeprEdu/01 - Data/cleaned - personyear-extra3-revised.rds")


# Preparing stable dataset ------------------------------------------------

personyear$age <- personyear$year - personyear$child_cohort

personyear$NPENG05 <- as.numeric(personyear$NPENG05)
personyear$NPENG08 <- as.numeric(personyear$NPENG08)
personyear$NPLES05 <- as.numeric(personyear$NPLES05)
personyear$NPLES08 <- as.numeric(personyear$NPLES08)
personyear$NPLES09 <- as.numeric(personyear$NPLES09)
personyear$NPREG05 <- as.numeric(personyear$NPREG05)
personyear$NPREG08 <- as.numeric(personyear$NPREG08)
personyear$NPREG09 <- as.numeric(personyear$NPREG09)

personyear <- personyear %>% 
  group_by(w19_0634_lnr) %>%
  mutate(age05 = ifelse((year == NPENG05AARGANG | year == NPLES05AARGANG | year == NPREG05AARGANG), age, NA)) %>%
  mutate(age08 = ifelse((year == NPENG08AARGANG | year == NPLES08AARGANG | year == NPREG08AARGANG), age, NA)) %>%
  mutate(age09 = ifelse((year == NPLES09AARGANG | year == NPREG09AARGANG), age, NA)) %>%
  ungroup()

personyear <- personyear %>% 
  group_by(w19_0634_lnr) %>% 
  arrange(w19_0634_lnr, desc(GPA)) %>% 
  mutate(GPA = first(GPA)) %>% 
  mutate(ageatGPA = first(age)) %>% 
  ungroup()

personyear$ageatGPA <- na_if(personyear$ageatGPA, 0)

personyear <- personyear %>% 
  group_by(w19_0634_lnr) %>%
  mutate(ageat05 = mean(age05, na.rm = TRUE)) %>% 
  mutate(ageat08 = mean(age08, na.rm = TRUE)) %>% 
  mutate(ageat09 = mean(age09, na.rm = TRUE)) %>% 
  mutate(ENG05 = mean(NPENG05, na.rm = TRUE)) %>% 
  mutate(ENG08 = mean(NPENG08, na.rm = TRUE)) %>% 
  mutate(LES05 = mean(NPLES05, na.rm = TRUE)) %>% 
  mutate(LES08 = mean(NPLES08, na.rm = TRUE)) %>% 
  mutate(LES09 = mean(NPLES09, na.rm = TRUE)) %>% 
  mutate(REG05 = mean(NPREG05, na.rm = TRUE)) %>% 
  mutate(REG08 = mean(NPREG08, na.rm = TRUE)) %>% 
  mutate(REG09 = mean(NPREG09, na.rm = TRUE)) %>% 
  ungroup()

personyear <- personyear %>% 
  filter(is.na(NPLES09AARGANG) | is.na(NPREG09AARGANG) | (!(year > NPLES09AARGANG) & !(year > NPREG09AARGANG))) %>% 
  group_by(w19_0634_lnr) %>% 
  arrange(w19_0634_lnr, desc(eduyrs_mor)) %>% 
  mutate(mother_eduyrs = first(eduyrs_mor)) %>% 
  ungroup()

personyear <- personyear %>% 
  group_by(w19_0634_lnr) %>% 
  arrange(w19_0634_lnr, desc(eduyrs_far)) %>% 
  mutate(father_eduyrs = first(eduyrs_far)) %>% 
  ungroup()

personyear <- personyear %>% 
  group_by(w19_0634_lnr) %>% 
  mutate(mother_inclong = mean(mother_incsum, na.rm = TRUE)) %>% 
  mutate(father_inclong = mean(father_incsum, na.rm = TRUE)) %>% 
  ungroup()


# Select relevant stable variables and remove duplicates ------------------

stable <- select(personyear, w19_0634_lnr, child_w19_0634_lnr, father_w19_0634_lnr, mother_w19_0634_lnr,
                 child_female, child_cohort, mother_cohort, father_cohort, 
                 child_fodeland, father_fodeland, mother_fodeland, 
                 child_w19_0634_lnr, child_PC1:child_PC10,
                 child_Depression_Howard_2019_PGS, child_EA4_excl23andMe_mobaref_PGS,
                 child_w19_0634_lnr, father_w19_0634_lnr, mother_w19_0634_lnr,
                 father_Depression_Howard_2019_PGS, father_EA4_excl23andMe_mobaref_PGS,
                 mother_Depression_Howard_2019_PGS, mother_EA4_excl23andMe_mobaref_PGS,
                 mother_inclong, father_inclong, 
                 mother_eduyrs, father_eduyrs, 
                 ENG05, ENG08, LES05, LES08, LES09, REG05, REG08, REG09, ageat05,
                 ageat08, ageat09, AA1550, AA1551, AA1552,
                 FF253, FF254, FF255, GPA, ageatGPA)

stable <- distinct(stable)

stable$grades05 <- rowMeans(stable[ , c("ENG05", "LES05", "REG05")], na.rm = TRUE)
stable$grades08 <- rowMeans(stable[ , c("ENG08", "LES08", "REG08")], na.rm = TRUE)
stable$grades09 <- rowMeans(stable[ , c("LES09", "REG09")], na.rm = TRUE)

stable$father_inclong <- stable$father_inclong / 100000
stable <- mutate(stable, father_inclong = ifelse(father_inclong > 30, NA, father_inclong))
stable$mother_inclong <- stable$mother_inclong / 100000
stable <- mutate(stable, mother_inclong = ifelse(mother_inclong > 30, NA, mother_inclong))

stable <- stable %>% 
  group_by(mother_w19_0634_lnr, father_w19_0634_lnr) %>% 
  mutate(family_id = cur_group_id()) %>% 
  ungroup()


# Remove non-consenting individuals 

a <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs_20250317.csv")
a <- filter(a, ROLE == "Child")
stable <- left_join(stable, a)
stable <- filter(stable, !is.na(PREG_ID_2601))

# Restrict sample to European ancestors

stable <- filter(stable, (mother_fodeland == 0 | (mother_fodeland > 100 & mother_fodeland < 162)))
stable <- filter(stable, (father_fodeland == 0 | (father_fodeland > 100 & father_fodeland < 162)))


# Descriptive statistics --------------------------------------------------

stable$depression_mor <- rowMeans(stable[ , c("AA1550", "AA1551", "AA1552")], na.rm = TRUE)
stable$depression_far <- rowMeans(stable[ , c("FF253", "FF254", "FF255")], na.rm = TRUE)

mplus <- filter(stable, !is.na(child_Depression_Howard_2019_PGS), !is.na(child_EA4_excl23andMe_mobaref_PGS), !is.na(mother_EA4_excl23andMe_mobaref_PGS), !is.na(father_EA4_excl23andMe_mobaref_PGS), 
                !is.na(child_female), !is.na(ageat09), !is.na(mother_Depression_Howard_2019_PGS),
                !is.na(father_Depression_Howard_2019_PGS))

basic <- basic %>% mutate(child_female = ifelse(kjoenn == 2, 1, 0))
basic <- basic %>% select(w19_0634_lnr, ageat09, child_female, grades09, foedselsaar, mother_cohort, eduyrs_mor, mother_inclong, 
                                   father_cohort, eduyrs_far, father_inclong) %>% distinct()

saveRDS(basic, "N:/durable/projects/36290998_Arno_GenRiskDeprEdu/01 - Data/population.rds")


basic %>% tbl_summary(include = c("grades09", "ageat09", "child_female",  "foedselsaar", "mother_cohort", "father_cohort", "eduyrs_mor", "eduyrs_far", "mother_inclong", 
                                  "father_inclong"), 
                       statistic = list(all_continuous() ~ "{mean} ({sd})") , type = list(where(is.numeric) ~ "continuous2"), digits = ~ 3)


stable %>% tbl_summary(include = c("grades09", "ageat09", "child_female", "child_cohort", "mother_cohort", "father_cohort", "mother_eduyrs", "father_eduyrs", "mother_inclong", 
                                   "father_inclong", "child_Depression_Howard_2019_PGS", "child_EA4_excl23andMe_mobaref_PGS", 
                                   "mother_Depression_Howard_2019_PGS", "father_Depression_Howard_2019_PGS","mother_EA4_excl23andMe_mobaref_PGS", "father_EA4_excl23andMe_mobaref_PGS", "depression_mor",
                                    "depression_far"), 
                       statistic = list(all_continuous() ~ "{mean} ({sd})") , type = list(where(is.numeric) ~ "continuous2"), digits = ~ 3)

mplus %>% tbl_summary(include = c("grades09", "ageat09", "child_female", "child_cohort", "mother_cohort", "father_cohort", "mother_eduyrs", "father_eduyrs", "mother_inclong", 
                                  "father_inclong", "child_Depression_Howard_2019_PGS", "child_EA4_excl23andMe_mobaref_PGS", 
                                  "mother_Depression_Howard_2019_PGS", "father_Depression_Howard_2019_PGS","mother_EA4_excl23andMe_mobaref_PGS", "father_EA4_excl23andMe_mobaref_PGS", "depression_mor",
                                  "depression_far"), 
                       statistic = list(all_continuous() ~ "{mean} ({sd})") , type = list(where(is.numeric) ~ "continuous2"), digits = ~ 3)


vars <- c("grades09", "ageat09", "child_cohort", "mother_cohort", "father_cohort", "mother_eduyrs", "father_eduyrs", "mother_inclong", 
          "father_inclong", "child_Depression_Howard_2019_PGS", "child_EA4_excl23andMe_mobaref_PGS", 
          "mother_Depression_Howard_2019_PGS", "father_Depression_Howard_2019_PGS","mother_EA4_excl23andMe_mobaref_PGS", "father_EA4_excl23andMe_mobaref_PGS", "depression_mor",
          "depression_far")

cor_mat <- cor(mplus[vars], use = "pairwise.complete.obs")
cor_mat <- as.data.frame(cor_mat)

write_xlsx(cor_mat, "N:/durable/projects/36290998_Arno_GenRiskDeprEdu/02 - Results/correlation_table.xlsx")


# Save for Mplus ----------------------------------------------------------


mplus <- mplus %>%
  mutate_all(~ifelse(is.na(.), -999, .))
write.table(mplus, "N:/durable/projects/36290998_Arno_GenRiskDeprEdu/01 - Data/cleaned - mplus - revised.dat", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

