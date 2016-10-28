getwd()
setwd("/Users/Tulsigompo/Desktop/R_project_new/data_raw/yearly_fars_data")
list.files()
library(foreign)
drunk_d_1999<-read.dbf("PERSON_1999.dbf")
drunk_d_2000<-read.dbf("PERSON_2000.dbf")
drunk_d_2001<-read.dbf("PERSON_2001.dbf")
drunk_d_2002<-read.dbf("PERSON_2002.dbf")
drunk_d_2003<-read.dbf("PERSON_2003.dbf")
drunk_d_2004<-read.dbf("PERSON_2004.dbf")
drunk_d_2005<-read.dbf("PERSON_2005.dbf")
drunk_d_2006<-read.dbf("PERSON_2006.dbf")
drunk_d_2007<-read.dbf("PERSON_2007.dbf")
drunk_d_2008<-read.dbf("PERSON_2008.dbf")
drunk_d_2009<-read.dbf("PERSON_2009.dbf")
drunk_d_2010<-read.dbf("PERSON_2010.dbf")
library(dplyr)
library(tidyr)
clean_d_1999<-drunk_d_1999 
colnames(clean_d_1999)<-tolower(colnames(clean_d_1999))
clean_d_1999<-clean_d_1999 %>%
mutate(year=1999) %>%
  filter(inj_sev==4 & per_typ==1) %>%
unite(unique_id,st_case, veh_no, per_no, year, remove = FALSE) %>%
 mutate(sex=ifelse(sex %in% c(8,9),NA,sex),
       sex=factor(sex,levels= c(1,2),
                   labels=c("Male","female"))) %>%
mutate(Alcohol= ifelse(alc_res %in% c(1:94),"TRUE",NA),
Alcohol= ifelse(alc_res==00,"FALSE",Alcohol)) %>%
select(-alc_res) 
if(year<=2008){
clean_d_1999<-clean_d_1999 %>%
mutate(lag_hrs = ifelse(lag_hrs %in% c(99,999),NA,lag_hrs)) 
 } else { clean_d_1999<-clean_d_1999 %>% 
  mutate(lag_hrs=ifelse(lag_hrs==999,NA,lag_hrs)) } 
clean_d_1999<-clean_d_1999 %>%
filter((lag_hrs < 1) | (lag_hrs == 1 & lag_mins == 0)) %>%
mutate(agecat = cut(age, breaks= c(0, 25, 44, 64, 98), labels= c("<25 years", "25 to 44 years", "45--64 years", "NA"))) %>%
  select(-age) 

gathered_df<-clean_d_1999 %>%
  gather(drug_number,drug_type_raw,contains("drugres")) %>%
  mutate(drug_type=ifelse(drug_type_raw %in% 100:295,"Narcotic",NA ),
         drug_type=ifelse(drug_type_raw %in%  300:395,"Depressant",drug_type),
         drug_type=ifelse(drug_type_raw %in%  400:495,"Stimulant", drug_type),
         drug_type=ifelse(drug_type_raw %in% 600:695,"Cannabinoid",drug_type),
         drug_type=ifelse(drug_type_raw %in% c(500:595,700:996),"Other",drug_type),
         drug_type=ifelse(drug_type_raw==1,"None",drug_type),
         drug_type = factor(drug_type)) %>% 
  select(-drug_type_raw, -drug_number) %>%
  dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))

non_missing_drugs <- gathered_df %>%
  filter(!is.na(drug_type)) %>%
  group_by(unique_id, drug_type) %>%
  summarize(has_drug = TRUE) %>%
  ungroup()  %>% 
  mutate(row_num = 1:n()) %>%
  spread(drug_type, has_drug, fill = FALSE) %>%
  select(-row_num) %>%
  group_by(unique_id) %>%
  summarize(Cannabinoid = any(Cannabinoid),
            Depressant = any(Depressant),
            Narcotic = any(Narcotic),
            Other = any(Other),
            Stimulant = any(Stimulant)) %>%
  ungroup()

clean_d_1999 <- clean_d_1999 %>%
  dplyr::select(-contains("drugres")) %>%
  dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
  tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                Depressant, Narcotic, Other, Stimulant) %>%
  dplyr::mutate(drug_type = factor(drug_type)) %>%
  dplyr::mutate(positive_for_drug=as.logical(positive_for_drug)) %>%
 select(unique_id,sex,year,agecat,drug_type,positive_for_drug)

clean_yearly_person_file<-function(year){
drunk_d_1999<-foreign::read.dbf(paste0("PERSON_",year,".dbf"))
clean_d_1999<-drunk_d_1999 
colnames(clean_d_1999)<-tolower(colnames(clean_d_1999))
clean_d_1999<-clean_d_1999 %>%
  filter(state %in% c(6,15,17,33,44,54)) %>%
  mutate(year=year) %>%
  filter(inj_sev==4 & per_typ==1) %>%
  unite(unique_id,st_case, veh_no, per_no, year, remove = FALSE) %>%
  mutate(sex=ifelse(sex %in% c(8,9),NA,sex),
         sex=factor(sex,levels= c(1,2),
                    labels=c("Male","female"))) %>%
  mutate(Alcohol= ifelse(alc_res %in% c(1:94),"TRUE",NA),
         Alcohol= ifelse(alc_res==00,"FALSE",Alcohol)) %>%
  select(-alc_res) 
if(year<=2008){
  clean_d_1999<-clean_d_1999 %>%
    mutate(lag_hrs = ifelse(lag_hrs %in% c(99,999),NA,lag_hrs)) 
} else { clean_d_1999<-clean_d_1999 %>% 
  mutate(lag_hrs=ifelse(lag_hrs==999,NA,lag_hrs)) } 
clean_d_1999<-clean_d_1999 %>%
  filter((lag_hrs < 1) | (lag_hrs == 1 & lag_mins == 0)) %>%
  mutate(agecat = cut(age, breaks= c(0, 25, 44, 64, 98), labels= c("<25 years", "25 to 44 years", "45--64 years", "NA"))) %>%
  select(-age) 

gathered_df<-clean_d_1999 %>%
  gather(drug_number,drug_type_raw,contains("drugres")) %>%
  mutate(drug_type=ifelse(drug_type_raw %in% 100:295,"Narcotic",NA ),
         drug_type=ifelse(drug_type_raw %in%  300:395,"Depressant",drug_type),
         drug_type=ifelse(drug_type_raw %in%  400:495,"Stimulant", drug_type),
         drug_type=ifelse(drug_type_raw %in% 600:695,"Cannabinoid",drug_type),
         drug_type=ifelse(drug_type_raw %in% c(500:595,700:996),"Other",drug_type),
         drug_type=ifelse(drug_type_raw==1,"None",drug_type),
         drug_type = factor(drug_type)) %>% 
  select(-drug_type_raw, -drug_number) %>%
  dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))

non_missing_drugs <- gathered_df %>%
  filter(!is.na(drug_type)) %>%
  group_by(unique_id, drug_type) %>%
  summarize(has_drug = TRUE) %>%
  ungroup()  %>% 
  mutate(row_num = 1:n()) %>%
  spread(drug_type, has_drug, fill = FALSE) %>%
  select(-row_num) %>%
  group_by(unique_id) %>%
  summarize(Cannabinoid = any(Cannabinoid),
            Depressant = any(Depressant),
            Narcotic = any(Narcotic),
            Other = any(Other),
            Stimulant = any(Stimulant)) %>%
  ungroup()

clean_d_1999 <- clean_d_1999 %>%
  dplyr::select(-contains("drugres")) %>%
  dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
  tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                Depressant, Narcotic, Other, Stimulant) %>%
  dplyr::mutate(drug_type = factor(drug_type)) %>%
  dplyr::mutate(positive_for_drug=as.logical(positive_for_drug)) %>%
  select(unique_id,sex,year,agecat,drug_type,positive_for_drug) %>%
 filter(!is.na(positive_for_drug))
return(clean_d_1999)
}

data_1999 <- clean_yearly_person_file(1999)
data_1999 %>%
  group_by(drug_type) %>%
  slice(1:3)
for(study_year in 1999:2010){
  df <- clean_yearly_person_file(study_year)
  
  if(study_year == 1999){
    clean_fars <- df
  } else {
    clean_fars <- rbind(clean_fars, df)
  }
}
save(clean_fars, file = "/Users/Tulsigompo/Desktop/R_project_new/data/clean_fars.R")

load("/Users/Tulsigompo/Desktop/R_project_new/data/clean_fars.R")
dim(clean_fars)
length(unique(clean_fars$unique_id))
summary(clean_fars)

clean_fars %>%
  mutate(year_cat = cut(year, breaks = c(1999, 2002, 2006, 2010),
                        labels = c("1999-2002", "2003-2006",
                                   "2007-2010"),
                        include.lowest = TRUE, right = TRUE)) %>%
  filter(!is.na(sex)) %>%
  group_by(drug_type, sex, year_cat) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
  select(drug_type, sex, year_cat, perc_positive) %>%
  unite(sex_year_cat, sex, year_cat) %>%
  spread(sex_year_cat, perc_positive) %>%
  knitr::kable(col.names = c("Drug type", "F 1999-2002",
                             "F 2003-2006", "F 2007-2010",
                             "M 1999-2002", "M 2003-2006",
                             "M 2007-2010"))