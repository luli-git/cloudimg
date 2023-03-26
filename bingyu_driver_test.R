
####
#### Get number of negative tests before cohort_entry_date / [-18 months, -179 days)
compute_negative_tests<-function(pos_cohort = rslt$pos,
                                 observation_derivation_recover=cdm_tbl("observation_derivation_recover"),
                                 require_covid_inpatient_ed=FALSE, max_date="2022-06-05"){
  
  obs_der_tbl<-observation_derivation_recover %>% filter(observation_date<max_date)
  
  # identify test records
  pcr_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001530L, value_as_concept_id %in% c(9189L))
  antigen_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001529L, value_as_concept_id %in% c(9189L))
  serology_negative<-obs_der_tbl %>%
    filter(observation_concept_id==2000001528L, value_as_concept_id %in% c(9189L)) %>%
    mutate(observation_date=observation_date-weeks(4))
  
  negatives<-pcr_negative %>% 
    dplyr::union(antigen_negative) %>%
    dplyr::union(serology_negative) %>% 
    filter(observation_date>"2020-03-01")
  
  negative_records <- pos_cohort %>% 
    left_join(negatives %>% select(person_id, observation_date), by = 'person_id') %>%  # 384824, 129700
    filter(observation_date<cohort_entry_date - days(179), 
           observation_date>=cohort_entry_date - months(18)) # 90760, 30691
  
  negative_tests_num <- negative_records %>%
    group_by(site,person_id) %>%
    summarise(total_negative_tests=n_distinct(observation_date)) %>%
    ungroup()
  negative_tests_num_total <- pos_cohort %>%
    left_join(negative_tests_num %>% select(-site), by = 'person_id') %>% 
    mutate(total_negative_tests = case_when(is.na(total_negative_tests) ~ 0L, 
                                            TRUE ~ total_negative_tests))
  
  return(negative_tests_num_total)
}

### Add to driver.R
max_date = "2022-06-05"

rslt <- list()
rslt$pos <- results_tbl('dis_Q2-5_NO_ADI_COI_bz_new2')
rslt$pos_negative_tests <- compute_negative_tests(rslt$pos, max_date = max_date)
# Check colnames and counts
colnames(rslt$pos_negative_tests)
rslt$pos_negative_tests %>% count()

# Output table
rslt$pos_negative_tests %>% output_tbl('dis_Q2-5_NO_ADI_COI_bz_new3', index='person_id')

test <- rslt$pos_negative_tests %>% collect_new()

unique(test$total_negative_tests)
#  [1]  2  4  1  5  3  6  7  9  8 19 12 11 24 32 15 33 10 13 14 17 35 23 18 21 22 16 31 30 29 40 27 20 36 44 45 26 39 59 65 57 28 38  0
rm(test)
